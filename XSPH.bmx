' Simple fluid Simulation
' Original code by "Noobody" 
' http://www.blitzbasic.com/Community/posts.php?topic=87491
'
' Patched by Laurent "ker2x" Laborde (http://www.keru.org)
'
' Licenced under the ZLIB licence
' Copyright (c) 2009 Benedikt 'Noobody' Bitterli
'
' This software is provided 'as-is', without any express or implied
' warranty. In no event will the authors be held liable for any damages
' arising from the use of this software.
'
' Permission is granted to anyone to use this software for any purpose,
' including commercial applications, and to alter it and redistribute it
' freely, subject to the following restrictions:
'
'    1. The origin of this software must not be misrepresented; you must not
'    claim that you wrote the original software. If you use this software
'    in a product, an acknowledgment in the product documentation would be
'    appreciated but is not required.
'
'    2. Altered source versions must be plainly marked as such, and must not be
'    misrepresented as being the original software.
'
'    3. This notice may not be removed or altered from any source
'    distribution.



SuperStrict

Framework BRL.Basic

Import BRL.Timer
Import BRL.Random
Import BRL.PNGLoader
Import BRL.StandardIO
Import BRL.GLGraphics
Import Pub.OpenGL
Import pub.Glew

Include "TParticle.bmx"
Include "TSPH.bmx"

Incbin "Metaball.png"


'Import bah.opencl 'OpenCL stuff, Using the SVN version at : http://code.google.com/p/maxmods/

' Initialize OpenCL Device
'Local platform:TCLPlatform = TCLPlatform.InitDevice(CL_DEVICE_TYPE_ALL)
'Local program:TCLProgram = platform.LoadProgram(LoadString("fluid.cl"))
'Local kernelGridCount:TCLKernel = program.LoadKernel("gridCount")




InitGL()
Function InitGL()
	GLGraphics GWIDTH, GHEIGHT 'Basic OGL initialization
	
	glewInit()

	glMatrixMode( GL_PROJECTION )
	glLoadIdentity()

	glOrtho( 0, GWIDTH, GHEIGHT, 0, -1, 1 )
	glMatrixMode( GL_MODELVIEW )
	glLoadIdentity()
	
	glEnable(GL_BLEND) 'Oh mighty OGL, please doth some nice alpha blending in thy rendering pipeline
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
End Function


Local SPH:TSPH = InitScene()
Local UpdateCounter:Int, RenderCounter:Int, CountingFPS:Int, FPS:Int, FPSTimer:Int = MilliSecs()

'MAIN LOOP
While Not ( KeyHit( KEY_ESCAPE ) Or AppTerminate() )
 
	'User input and misc stuff
	glClear(GL_COLOR_BUFFER_BIT) 'clear screen
	UserInput(SPH)  'handle user input
	glColor4f(1.0, 1.0, 1.0, 1.0)
	GLDrawText "Updatetime: " + String( UpdateCounter )[ .. 3 ] + "ms  Rendertime: " + String( RenderCounter )[ .. 4 ] + "ms  FPS: " + String( FPS )[ .. 4 ] + " Particles: " + TParticle.Count, 0, 0
	
	'Simulation 
	UpdateCounter = MilliSecs()
	If Not SPH.Paused   'Press P To Pause simulation
		SPH.Update()
	End If
	UpdateCounter = MilliSecs() - UpdateCounter
	
	'Rendering
	RenderCounter = MilliSecs()
	SPH.Render()
	Flip 0
	RenderCounter = MilliSecs() - Rendercounter
	
	'Time counter
	CountingFPS :+ 1
	If MilliSecs() - FPSTimer > 500 Then
		FPS = CountingFPS*2
		CountingFPS = 0
		FPSTimer = MilliSecs()
	EndIf
Wend
End
'END OF MAIN LOOP

Function UserInput( SPH:TSPH )
	
	If KeyHit(KEY_P) Then 'P : Pause the simulation
		SPH.Paused = ~ SPH.Paused
	End If
	
	Local MX:Int = MouseX(), MY:Int = MouseY()
	
	If MouseHit(1) Then 'Left click : Add some water
		SPH.FreeRect(MX - 41, MY - 41, MX + 41, MY + 41)
		
		For Local X:Float = Max(MX - 40, 0) To Min(MX + 40, GWIDTH) Step 30 * SPH.WORLD_SCALE
			For Local Y:Float = Max(MY - 40, 0) To Min(MY + 40, GHEIGHT) Step 30 * SPH.WORLD_SCALE
				SPH.AddParticle( X + Rnd( -0.5, 0.5 ), Y + Rnd( -0.5, 0.5 ) )
			Next
		Next
	EndIf
	
	If MouseHit(2) Then 'Right click : Place a wall
		SPH.FreeRectKeepBoundary( MX - 11, MY - 11, MX + 11, MY + 11 )
		
		For Local X:Float = Max( MX - 10, 0 ) To Min( MX + 10, GWIDTH ) Step 30*SPH.WORLD_SCALE
			For Local Y:Float = Max( MY - 10, 0 ) To Min( MY + 10, GHEIGHT ) Step 30*SPH.WORLD_SCALE
				SPH.AddBoundaryParticle( X, Y )
			Next
		Next
	EndIf
End Function



Function InitScene:TSPH() 'Fills parts of the initial area with water and places a few boundary particles
	Local SPH:TSPH = New TSPH
	
	For Local Y:Float = 50 To 450 Step SPH.PARTICLE_SPACING
		For Local X:Float = 100 To 260 Step SPH.PARTICLE_SPACING
			SPH.AddParticle( X + Rnd( -0.5, 0.5 ), Y + Rnd( -0.5, 0.5 ) )
		Next
	
	Next
	
	For Local Y:Float = 500 To 515 Step SPH.BOUNDARY_SPACING
		For Local X:Float = 50 To 550 Step SPH.BOUNDARY_SPACING
			SPH.AddBoundaryParticle(X, Y)
		Next
	Next
	
	For Local Y:Float = 50 To 450 Step SPH.BOUNDARY_SPACING
		For Local X:Float = 0 To 15 Step SPH.BOUNDARY_SPACING
			SPH.AddBoundaryParticle( X + 80, Y )
			SPH.AddBoundaryParticle( X + 270, Y )
		Next
	Next
	
	'For Local Y:Float = 450 To 515 Step SPH.BOUNDARY_SPACING
	'	For Local X:Float = 35 To 50 Step SPH.BOUNDARY_SPACING
	'		SPH.AddBoundaryParticle(X, Y)
	'	Next
	'Next
	
	For Local Y:Float = 570 To 598 Step SPH.PARTICLE_SPACING
		For Local X:Float = 5 To 795 Step SPH.PARTICLE_SPACING
			SPH.AddParticle( X + Rnd( -0.5, 0.5 ), Y + Rnd( -0.5, 0.5 ) )
		Next
	Next
	
	Return SPH
End Function


