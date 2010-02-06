' Simple fluid Simulation
' Original code by Thomas "Tachyon" Riegsecker (http://basiliskgames.com/)
'
' Patched by Laurent "ker2x" Laborde (http://www.keru.org)
'
' Licenced under some kind of OpenSource License ... details comming soon



SuperStrict

Framework BRL.Basic

Import BRL.Timer
Import BRL.Random
Import BRL.PNGLoader
Import BRL.StandardIO
Import BRL.GLGraphics
Import Pub.OpenGL
Import pub.Glew

'Import bah.opencl 'OpenCL stuff, Using the SVN version at : http://code.google.com/p/maxmods/

Incbin "Metaball.png"

' Initialize OpenCL Device
'Local platform:TCLPlatform = TCLPlatform.InitDevice(CL_DEVICE_TYPE_ALL)
'Local program:TCLProgram = platform.LoadProgram(LoadString("fluid.cl"))
'Local kernelGridCount:TCLKernel = program.LoadKernel("gridCount")


Const GWIDTH:Int = 800  'Screen Width
Const GHEIGHT:Int = 600 'Screen Height

InitGL()

Local SPH:TSPH = InitScene()

Local UpdateCounter:Int, RenderCounter:Int, CountingFPS:Int, FPS:Int, FPSTimer:Int = MilliSecs()

While Not ( KeyHit( KEY_ESCAPE ) Or AppTerminate() ) 'I guess I don't have to explain the mainloop - just input, output and FPS calculation
	glClear( GL_COLOR_BUFFER_BIT )
	
	UserInput( SPH )
	
	glColor4f( 1.0, 1.0, 1.0, 1.0 )
	GLDrawText "Updatetime: " + String( UpdateCounter )[ .. 3 ] + "ms  Rendertime: " + String( RenderCounter )[ .. 4 ] + "ms  FPS: " + String( FPS )[ .. 4 ] + " Particles: " + TParticle.Count, 0, 0
	
	UpdateCounter = MilliSecs()
	If Not SPH.Paused   'Press P To Pause simulation
		SPH.Update()
	End If
	UpdateCounter = MilliSecs() - UpdateCounter
	
	RenderCounter = MilliSecs()
	SPH.Render()
	
	Flip 0
	RenderCounter = MilliSecs() - Rendercounter
	
	CountingFPS :+ 1
	
	If MilliSecs() - FPSTimer > 500 Then
		FPS = CountingFPS*2
		CountingFPS = 0
		FPSTimer = MilliSecs()
	EndIf
Wend
End

Function UserInput( SPH:TSPH )
	SPH.NiceRender = SPH.NiceRender ~ KeyHit( KEY_SPACE ) 'A fancy way of doing "If KeyHit( KEY_SPACE ) Then SPH.NiceRender = Not SPH.NiceRender"
	
	If KeyHit(KEY_P) Then 'Pause the simulation
		SPH.Paused = ~ SPH.Paused
	End If
	
	Local MX:Int = MouseX(), MY:Int = MouseY()
	
	If MouseHit( 1 ) Then 'Add some water
		SPH.FreeRect( MX - 41, MY - 41, MX + 41, MY + 41 )
		
		For Local X:Float = Max( MX - 40, 0 ) To Min( MX + 40, GWIDTH ) Step 30*SPH.WORLD_SCALE
			For Local Y:Float = Max( MY - 40, 0 ) To Min( MY + 40, GHEIGHT ) Step 30*SPH.WORLD_SCALE
				SPH.AddParticle( X + Rnd( -0.5, 0.5 ), Y + Rnd( -0.5, 0.5 ) )
			Next
		Next
	EndIf
	
	If MouseHit( 2 ) Then 'Place a wall
		SPH.FreeRect( MX - 9, MY - 9, MX + 9, MY + 9 )
		
		For Local X:Float = Max( MX - 10, 0 ) To Min( MX + 10, GWIDTH ) Step 30*SPH.WORLD_SCALE
			For Local Y:Float = Max( MY - 10, 0 ) To Min( MY + 10, GHEIGHT ) Step 30*SPH.WORLD_SCALE
				SPH.AddBoundaryParticle( X, Y )
			Next
		Next
	EndIf
End Function

Function InitGL()
	GLGraphics GWIDTH, GHEIGHT 'Basic OGL initialization
	
	glewInit()
	
	glMatrixMode( GL_PROJECTION )
	glLoadIdentity()
	
	glOrtho( 0, GWIDTH, GHEIGHT, 0, -1, 1 )
	
	glMatrixMode( GL_MODELVIEW )
	glLoadIdentity()
	
	glEnable( GL_BLEND ) 'Oh mighty OGL, please doth some nice alpha blending in thy rendering pipeline
	glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA )
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
			SPH.AddBoundaryParticle( X, Y )
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
	'		SPH.AddBoundaryParticle( X, Y )
	'	Next
	'Next
	
	For Local Y:Float = 580 To 599 Step SPH.PARTICLE_SPACING
		For Local X:Float = 1 To 799 Step SPH.PARTICLE_SPACING
			SPH.AddParticle( X + Rnd( -0.5, 0.5 ), Y + Rnd( -0.5, 0.5 ) )
		Next
	Next
	
	Return SPH
End Function

Type TSPH
	Const DAMPING:Float  = 0.0015 'Damping used in the integration step. Feels a bit hacky, but looks nicer
	
	Const HEAT_RATIO:Float = 1.0 'The ratio of heat transmission
	
	Const V_ALPHA:Float = 5.0 'Two constants used for the shear-and-bulk viscosity. Should not be altered at the moment, since this might induce cancer
	Const V_BETA:Float  = 10.0
	
	Const TIMESTEP:Float = 0.4 'Timestepping constant. The simulation will run faster if the timestep is set higher, but the accuracy will decrease
	Const TIMESTEP_SQ:Float = TIMESTEP * TIMESTEP
	Const INV_TIMESTEP:Float    = 1.0/TIMESTEP
	
	Const UNIT_SCALE:Float = 0.07 'The unit length used in the equations. You might choose this to be smaller, but don't forget to adjust the rest density
	Const INV_UNIT_SCALE:Float = 1.0/UNIT_SCALE
	
	Const WORLD_SCALE:Float = 0.5 * 0.35 'Scale constant used to map between pixel space and world space. You can set this smaller if you want to fit more water on the screen
	Const INV_WORLD_SCALE:Float = 1.0/WORLD_SCALE
	
	Const CONTAINER_WIDTH:Float  = GWIDTH *UNIT_SCALE*INV_WORLD_SCALE 'Width and height of the container (=window) in world space
	Const CONTAINER_HEIGHT:Float = GHEIGHT*UNIT_SCALE*INV_WORLD_SCALE
	
	Const REPULSIVE_DISTANCE:Float = 30.0 * UNIT_SCALE 'If a fluid particle approaches a boundary particle, this distance is used to determine, whether it will feel repulsive force
	Const INV_REPULSIVE_DISTANCE:Float = 1.0/REPULSIVE_DISTANCE
	Const REPULSIVE_DIST_SQ:Float = REPULSIVE_DISTANCE * REPULSIVE_DISTANCE
	
	Const REPULSIVE_D:Float = 0.5 'Force scale for the repulsive force between fluid and boundary particles
	
	Const SMOOTHING_LENGTH:Float = 55 * UNIT_SCALE 'The radius in which particles will interact with each other
	Const SMOOTHING_LENGTH_SQ:Float	= SMOOTHING_LENGTH*SMOOTHING_LENGTH
	Const INV_SMOOTHING_LENGTH:Float	= 1.0/SMOOTHING_LENGTH
	
	'Different kernel normalization factors and their deritatives used for smoothing attributes between particles
	Const D_KERNEL_FACTOR:Float = 315.0/( 64.0*Pi*SMOOTHING_LENGTH^9 )	'Poly6
	Const P_KERNEL_FACTOR:Float = -45.0/( Pi*SMOOTHING_LENGTH^6 )		'Spiky (gradient)
	
	Const D_KERNEL_FACTOR_GRADIENT:Float = -6.0*D_KERNEL_FACTOR 'Poly6 deritative (I'm not sure if this one is right, since I had to calculate it on my own)
	
	Const PARTICLE_AREA:Float = ( SMOOTHING_LENGTH*0.3 )^2*Pi 'Volume of a particle used to calculate the particle mass
	
	Const XSPH_EPSILON:Float = 0.5 'The epsilon factor used in the XSPH variant to provide more orderly high-speed flows (makes the simulation more stable too)
	
	Const GRAVITY_X:Float = 0.0*UNIT_SCALE 'Gravity
	Const GRAVITY_Y:Float = 0.5*UNIT_SCALE
	
	Const REST_DENSITY:Float	= 0.35 'The density the fluid would have if it was at rest, used to initialize particles and calculate the pressure
	Const INV_REST_DENSITY:Float	= 1.0/REST_DENSITY
	
	Const P_B:Float = GRAVITY_Y*CONTAINER_HEIGHT/7.0*3 'Scaling constant used for the pressure and speed-of-sound calculation
	
	Const SAMPLE_RATE:Float = 5.0 'Pixels per sample, used for the nice-render-routine
	Const MAX_WHITE:Float   = 0.8 'Brah, just color stuff
	
	Const PARTICLE_SPACING:Float = 30*TSPH.WORLD_SCALE 'Minimal distance that water particles should have
	Const BOUNDARY_SPACING:Float = 30 * TSPH.WORLD_SCALE 'Same for boundary particles
	
	Field ParticleTex:Int    'Metaball texture
	
	Field NiceRender:Int = False
	
	Field GridWidth:Int
	Field GridHeight:Int
	
	Field FluidGrid:TParticle[,]
	Field BoundaryGrid:TParticle[,]
	
	Field Particles:TParticle[]
	Field BoundaryParticles:TParticle[]
	
	Field Paused:Int = False
	
	Method New()
		GridWidth = Ceil(CONTAINER_WIDTH * INV_SMOOTHING_LENGTH)    'Simulation Grid Width
		GridHeight = Ceil(CONTAINER_HEIGHT * INV_SMOOTHING_LENGTH)  'Simulation Grid Height
		
		FluidGrid    = New TParticle[ GridWidth, GridHeight ]
		BoundaryGrid = New TParticle[GridWidth, GridHeight]
		
		ParticleTex = GLTexFromPixmap(LoadPixmapPNG("incbin::Metaball.png"))
	End Method
	
	Method Update()
		RedistributeGrid()  'Slowest Method
		UpdateDensity()
		CalcForces()
		Integrate()
	End Method
	
	' Slowest Method
	Method RedistributeGrid() 'Fills the particles in a spatial grid whose cells contain linked lists of particles
		For Local X:Int = 0 Until GridWidth
			For Local Y:Int = 0 Until GridHeight
				FluidGrid[X, Y] = Null
				BoundaryGrid[X, Y] = Null
			Next
		Next
		
		' Slow loop
		For Local I:Int = 0 Until Particles.Length
			Local P:TParticle = Particles[I]
			
			Local GridX:Int = Floor(P.PositionX * TSPH.INV_SMOOTHING_LENGTH)
			Local GridY:Int = Floor(P.PositionY * TSPH.INV_SMOOTHING_LENGTH)
			
			Local Successor:TParticle = FluidGrid[GridX, GridY]
			
			FluidGrid[GridX, GridY] = P
			
			P.Succ = Successor
		Next
		
		For Local I:Int = 0 Until BoundaryParticles.Length
			Local P:TParticle = BoundaryParticles[I]
			
			Local GridX:Int = Floor(P.PositionX * TSPH.INV_SMOOTHING_LENGTH)
			Local GridY:Int = Floor(P.PositionY * TSPH.INV_SMOOTHING_LENGTH)
			
			Local Successor:TParticle = BoundaryGrid[GridX, GridY]
			
			BoundaryGrid[GridX, GridY] = P
			
			P.Succ = Successor
		Next
		
		GCCollect()
	End Method
	
	Method UpdateDensity() 'Calculates the new density, pressure, mass and sound speed values.
		'I won't go into the details of the calculations themselves. I posted the articles where I got the formulas from.
		'I guess the articles explain them much better than I ever could :)
		
		Local DirX:Int[] = [ -1, -1,  1, 1, -1, 1,  0, 0, 0 ]
		Local DirY:Int[] = [ -1,  1, -1, 1,  0, 0, -1, 1, 0 ]
		
		For Local I:Int = 0 Until Particles.Length
			Local P1:TParticle = Particles[ I ]
			
			Local IntX:Int = Int( P1.PositionX*INV_SMOOTHING_LENGTH )
			Local IntY:Int = Int( P1.PositionY*INV_SMOOTHING_LENGTH )
			
			For Local I:Int = 0 To 8 'This loop iterates over the cell the particle is in and all 8 neighbouring cells
				Local GridX:Int = IntX + DirX[ I ]
				Local GridY:Int = IntY + DirY[ I ]
				
				If GridX < 0 Or GridY < 0 Or GridX >= GridWidth Or GridY >= GridHeight Then Continue
				
				Local P2:TParticle = FluidGrid[ GridX, GridY ]
				
				While P2 <> Null 'Iterate through the linked list
					If P1.ID > P2.ID Then 'Do each pair just once
						Local DX:Float = ( P1.PositionX - P2.PositionX )
						Local DY:Float = ( P1.PositionY - P2.PositionY )
						
						Local DSQ:Float = DX*DX + DY*DY
						
						If DSQ < SMOOTHING_LENGTH_SQ Then
							Local D:Float = Sqr( DSQ )
							
							Local R:Float = SMOOTHING_LENGTH_SQ - DSQ
							
							Local DensityFactor:Float = ( ( P1.VelocityX - P2.VelocityX )*DX + ( P1.VelocityY - P2.VelocityY )*DY )*R*R/D
							
							P1.DeltaDensity :+ P2.Mass*DensityFactor
							P2.DeltaDensity :+ P1.Mass*DensityFactor
						EndIf
					EndIf
					
					P2 = P2.Succ
				Wend
			Next
		Next
		
		For Local I:Int = 0 Until Particles.Length
			Local P:TParticle = Particles[ I ]
			
			P.Density :+ P.DeltaDensity*D_KERNEL_FACTOR_GRADIENT
			
			If P.Density < 0.3 Or P.Density > 0.4 Then P.Density = REST_DENSITY*1.1 'CANCER. ELIMINATE.
			
			P.InvDensity   = 1.0/P.Density
			P.InvDensitySq = P.InvDensity*P.InvDensity
			
			P.Mass = P.Density*PARTICLE_AREA
			
			P.Pressure = P_B*P.Density*( ( P.Density*INV_REST_DENSITY )^7.0 - 1.0 )
			
			P.SoundSpeed = Sqr(Abs(HEAT_RATIO * P.Pressure * P.InvDensity))
		Next
	End Method
	
	Method CalcForces()
		Local DirX:Int[] = [ -1, -1,  1, 1, -1, 1,  0, 0, 0 ]
		Local DirY:Int[] = [ -1,  1, -1, 1,  0, 0, -1, 1, 0 ]
		
		For Local I:Int = 0 Until Particles.Length 'Reset the second order deritatives of the position and the first deritative of the density
			Local P:TParticle = Particles[ I ]
			
			P.ForceX = GRAVITY_X*P.Density
			P.ForceY = GRAVITY_Y*P.Density
			P.DeltaVelocityX = 0.0
			P.DeltaVelocityY = 0.0
			P.DeltaDensity   = 0.0
		Next
		
		For Local I:Int = 0 Until Particles.Length 'See the UpdateDensity method for more information about the grid iteration
			Local P1:TParticle = Particles[ I ]
			
			Local IntX:Int = Int( P1.PositionX*INV_SMOOTHING_LENGTH )
			Local IntY:Int = Int( P1.PositionY*INV_SMOOTHING_LENGTH )
			
			For Local I:Int = 0 To 8
				Local GridX:Int = IntX + DirX[ I ]
				Local GridY:Int = IntY + DirY[ I ]
				
				If GridX < 0 Or GridY < 0 Or GridX >= GridWidth Or GridY >= GridHeight Then Continue
				
				Local P2:TParticle = FluidGrid[ GridX, GridY ]
				
				While P2 <> Null
					If P1.ID > P2.ID Then
						Local DX:Float = ( P1.PositionX - P2.PositionX )
						Local DY:Float = ( P1.PositionY - P2.PositionY )
						
						Local DSQ:Float = DX*DX + DY*DY
						
						If DSQ < SMOOTHING_LENGTH_SQ Then
							Local D:Float = Sqr( DSQ )
							
							Local R:Float  = SMOOTHING_LENGTH - D
							
							Local VDX:Float = P2.VelocityX - P1.VelocityX
							Local VDY:Float = P2.VelocityY - P1.VelocityY
							
							Local PressureForce:Float = P1.Pressure*P1.InvDensitySq + P2.Pressure*P2.InvDensitySq 'Pressure force
							
							Local InvDensityAverage:Float = 2.0/( P1.Density + P2.Density )
							
							Local DotP:Float = -DX*VDX - DY*VDY
							
							If DotP < 0.0 Then 'Shear-bulk-viscosity
								Local M:Float = SMOOTHING_LENGTH*DotP/( DSQ + 20.0 )
								
								PressureForce :+ ( -V_ALPHA*M*( P1.SoundSpeed + P2.SoundSpeed )*0.5 + V_BETA*M*M )*InvDensityAverage
							EndIf
							
							PressureForce :* -P2.Mass*P_KERNEL_FACTOR*R*R/D
							
							DX :* PressureForce
							DY :* PressureForce
							
							P1.ForceX :+ DX
							P1.ForceY :+ DY
							P2.ForceX :- DX
							P2.ForceY :- DY
							
							
							Local R2:Float = SMOOTHING_LENGTH_SQ - DSQ
							
							'The XSPH variant introduced by Monaghan
							Local KernelFactor:Float = XSPH_EPSILON*P2.Mass*R2*R2*R2*D_KERNEL_FACTOR*InvDensityAverage
							
							VDX :* KernelFactor
							VDY :* KernelFactor
							
							P1.DeltaVelocityX :+ VDX
							P1.DeltaVelocityY :+ VDY
							P2.DeltaVelocityX :- VDX
							P2.DeltaVelocityY :- VDY
						EndIf
					EndIf
					
					P2 = P2.Succ
				Wend
				
				P2 = BoundaryGrid[ GridX, GridY ]
				
				While P2 <> Null 'Basic repulsive force calculation for boundary/fluid particle collision
					Local DX:Float = ( P1.PositionX - P2.PositionX )
					Local DY:Float = ( P1.PositionY - P2.PositionY )
					
					Local DSQ:Float = DX*DX + DY*DY
					
					If DSQ < REPULSIVE_DIST_SQ Then
						Local InvDSQ:Float   = 1.0/DSQ
						
						Local RSQ:Float = REPULSIVE_DIST_SQ*InvDSQ
						
						Local RepulsiveFactor:Float = REPULSIVE_D*( RSQ*RSQ - RSQ )*InvDSQ
						
						P1.ForceX :+ DX*RepulsiveFactor
						P1.ForceY :+ DY*RepulsiveFactor
					EndIf
					
					P2 = P2.Succ
				Wend
			Next
			
			Local Dist1:Float = Max( CONTAINER_WIDTH  - P1.PositionX, 0.0 ) 'Repulsive force for the screen boundaries
			Local Dist2:Float = Max( CONTAINER_HEIGHT - P1.PositionY, 0.0 )
			Local Dist3:Float = Max(                    P1.PositionX, 0.0 )
			Local Dist4:Float = Max(                    P1.PositionY, 0.0 )
			
			If Dist1 < REPULSIVE_DISTANCE Then P1.ForceX :- 1.0*( 1.0 - Sqr( Dist1*INV_REPULSIVE_DISTANCE ) )*P1.Density
			If Dist2 < REPULSIVE_DISTANCE Then P1.ForceY :- 1.0*( 1.0 - Sqr( Dist2*INV_REPULSIVE_DISTANCE ) )*P1.Density
			If Dist3 < REPULSIVE_DISTANCE Then P1.ForceX :+ 1.0*( 1.0 - Sqr( Dist3*INV_REPULSIVE_DISTANCE ) )*P1.Density
			If Dist4 < REPULSIVE_DISTANCE Then P1.ForceY :+ 1.0*( 1.0 - Sqr( Dist4*INV_REPULSIVE_DISTANCE ) )*P1.Density
		Next
	End Method
	
	Method Integrate()
		For Local I:Int = 0 Until Particles.Length
			Local P:TParticle = Particles[ I ]
			
			'Basic position verlet for timestepping
			Local OldX:Float = P.PositionX
			Local OldY:Float = P.PositionY
			
			P.PositionX :+ ( 1.0 - DAMPING )*( P.PositionX - P.OldX ) + P.DeltaVelocityX*TIMESTEP + TIMESTEP_SQ*P.ForceX
			P.PositionY :+ ( 1.0 - DAMPING )*( P.PositionY - P.OldY ) + P.DeltaVelocityY*TIMESTEP + TIMESTEP_SQ*P.ForceY
			
			P.PositionX = Max( Min( P.PositionX, CONTAINER_WIDTH  ), 0.0 )
			P.PositionY = Max( Min( P.PositionY, CONTAINER_HEIGHT ), 0.0 )
			
			P.OldX = OldX
			P.OldY = OldY
			
			P.VelocityX = ( P.PositionX - OldX )*INV_TIMESTEP
			P.VelocityY = ( P.PositionY - OldY )*INV_TIMESTEP
			
			Local DSQ:Float = P.VelocityX*P.VelocityX + P.VelocityY*P.VelocityY
			
			If DSQ > 4.0 Then 'Limit the velocity to restrain the cancer in spreading. Otherwise, the simulation goes *woof* and *boom*
				Local Factor:Float = Sqr( 4.0/DSQ )
				
				P.PositionX = OldX
				P.PositionY = OldY
								
				P.VelocityX :* Factor
				P.VelocityY :* Factor
				
				P.PositionX :+ P.VelocityX*TIMESTEP
				P.PositionY :+ P.VelocityY*TIMESTEP
			EndIf
			
			P.ScreenX = P.PositionX*INV_UNIT_SCALE*WORLD_SCALE 'Calculate the position in pixel space for rendering
			P.ScreenY = P.PositionY*INV_UNIT_SCALE*WORLD_SCALE
		Next
	End Method
	
	Method Render()
		If NiceRender Then
			Local SampleWidth:Int  = Ceil( GWIDTH /SAMPLE_RATE )
			Local SampleHeight:Int = Ceil( GHEIGHT/SAMPLE_RATE )
			
			Local Densities:Float[ SampleWidth, SampleHeight ]
			Local Ratios:Float   [ SampleWidth, SampleHeight ]
			Local Alphas:Float   [ SampleWidth, SampleHeight ]
			
			Local ScaleX:Float = SAMPLE_RATE*UNIT_SCALE*INV_WORLD_SCALE
			Local ScaleY:Float = SAMPLE_RATE*UNIT_SCALE*INV_WORLD_SCALE
			
			Local DirX:Int[] = [ -1, -1,  1, 1, -1, 1,  0, 0, 0 ]
			Local DirY:Int[] = [- 1, 1, -1, 1, 0, 0, -1, 1, 0]
			
			Local WorldX:Float, WorldY:Float
			Local IntX:Int, IntY:Int
			Local GridX:Int, GridY:Int
			Local DSQ:Float
			
			Local DensityFactor:Float = 1.0 / 13000.0
			
			For Local SampleY:Float = 0 Until SampleHeight 'Step through the screen and sample the density at certain points
				For Local SampleX:Float = 0 Until SampleWidth
				
					WorldX = SampleX * ScaleX	'World position according to the Sampler Number
					WorldY = SampleY * ScaleY
					
					IntX = Int(WorldX * INV_SMOOTHING_LENGTH)
					IntY = Int(WorldY * INV_SMOOTHING_LENGTH)
					
					'60% of rendering time lost in this loop
					For Local I:Int = 0 To 8
						GridX = IntX + DirX[I]
						GridY = IntY + DirY[I]
						
						If GridX < 0 Or GridY < 0 Or GridX >= GridWidth Or GridY >= GridHeight Then Continue

						' Count the particule inside the grid cell
						' 50% of time lost here						
						Local Iterator:TParticle = FluidGrid[GridX, GridY]
						'DebugStop
						While Iterator
							DSQ = (Iterator.PositionX - WorldX) * (Iterator.PositionX - WorldX) + (Iterator.PositionY - WorldY) * (Iterator.PositionY - WorldY)
							
							If DSQ < SMOOTHING_LENGTH_SQ Then
								Local R:Float = SMOOTHING_LENGTH_SQ - DSQ
								Densities[SampleX, SampleY]:+R * R * R
							EndIf
							
							Iterator = Iterator.Succ
						Wend
					Next
					
					Densities[ SampleX, SampleY ] :* DensityFactor
					
					Alphas[SampleX, SampleY] = 2.0 * Densities[SampleX, SampleY]
					Ratios[SampleX, SampleY] = 1.0 - Densities[SampleX, SampleY]
					
					If Ratios[SampleX, SampleY] > MAX_WHITE Then
						Alphas[SampleX, SampleY]:*MAX_WHITE / Ratios[SampleX, SampleY]
						Ratios[SampleX, SampleY] = MAX_WHITE
					EndIf
				Next
			Next
			
			glEnable(GL_BLEND)
			glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA )
			
			Local ScreenX:Float, ScreenY:Float 'Draw the previously calculated density samples
			For Local SampleY:Float = 0 Until SampleHeight - 1
				Local DoSpacing:Int
				
				glBegin( GL_TRIANGLE_STRIP )
					For Local SampleX:Float = 0 Until SampleWidth
						Local Density1:Float = Densities[ SampleX, SampleY ]
						Local Density2:Float = Densities[ SampleX, SampleY + 1 ]
						
						If Density1 + Density2 = 0.0 Then
							If Not DoSpacing Then
								glColor4f( 0.0, 0.0, 0.0, 0.0 )
								
								glVertex2f( ScreenX, ScreenY )
								glVertex2f( ScreenX, ScreenY + SAMPLE_RATE )
								
								DoSpacing = True
							EndIf
						Else
							If DoSpacing Then
								DoSpacing = False
								
								glVertex2f( ScreenX, ScreenY )
								glVertex2f( ScreenX, ScreenY + SAMPLE_RATE )
							EndIf
							
							Local Ratio:Float = Ratios[ SampleX, SampleY ]
							
							glColor4f( Ratio, Ratio, 1.0, Alphas[ SampleX, SampleY ] )
							glVertex2f( ScreenX, ScreenY )
							
							
							Ratio = Ratios[ SampleX, SampleY + 1 ]
							
							glColor4f( Ratio, Ratio, 1.0, Alphas[ SampleX, SampleY + 1 ] )
							glVertex2f( ScreenX, ScreenY + SAMPLE_RATE )
						EndIf
						
						ScreenX :+ SAMPLE_RATE
					Next
					
					ScreenX  = 0.0
					ScreenY :+ SAMPLE_RATE
				glEnd()
			Next
		Else  'if not "NiceRender"
			glEnable(GL_TEXTURE_2D)
			glBindTexture( GL_TEXTURE_2D, ParticleTex )
			
			glEnable( GL_BLEND )
			glBlendFunc(GL_ONE, GL_ONE) 'Additive blending
			
			glBegin(GL_QUADS) 'Just a few rectangles
				Local S:Float = 8.0
				
				For Local P:TParticle = EachIn Particles
					'Local Ratio:Float = Min( Max( ( P.Density + REST_DENSITY*0.0 )/( 2*REST_DENSITY ), 0.0 ), 1.0 )
					'glColor3f(1.0 - Ratio, 1.0 - Ratio, 1.0)
					
					Local col:Float = (0.7 + 0.2 * P.Pressure) * P.Density
					glColor4f(col ^ 3, col ^ 1.5, col, 1.0)
					
					glTexCoord2f(0.0, 0.0) ; glVertex2f(P.ScreenX - S, P.ScreenY - S)
					glTexCoord2f( 1.0, 0.0 ); glVertex2f( P.ScreenX + S, P.ScreenY - S )
					glTexCoord2f( 1.0, 1.0 ); glVertex2f( P.ScreenX + S, P.ScreenY + S )
					glTexCoord2f( 0.0, 1.0 ); glVertex2f( P.ScreenX - S, P.ScreenY + S )
				Next
			glEnd()
			
			glDisable(GL_TEXTURE_2D)
			
			glBlendFunc(GL_ZERO, GL_DST_COLOR) 'I don't really know how that one works - I just tried a few flag combinations and this one seemed to work
			
			glBegin( GL_QUADS )
				For Local I:Int = 0 To 1
					glVertex2f( 0.0, 0.0 )
					glVertex2f( GWIDTH, 0.0 )
					glVertex2f( GWIDTH, GHEIGHT )
					glVertex2f( 0.0, GHEIGHT )
				Next
			glEnd()
			
			glDisable( GL_BLEND )
			
		EndIf
		
		glColor4f( 1.0, 0.5, 0.5, 1.0 )
		
		glBegin( GL_QUADS ) 'Brown boundaries
			For Local P:TParticle = EachIn BoundaryParticles
				glVertex2f( P.ScreenX - 3.0, P.ScreenY - 3.0 )
				glVertex2f( P.ScreenX + 3.0, P.ScreenY - 3.0 )
				glVertex2f( P.ScreenX + 3.0, P.ScreenY + 3.0 )
				glVertex2f( P.ScreenX - 3.0, P.ScreenY + 3.0 )
			Next
		glEnd()
	End Method
	
	Method FreeRect( StartX:Float, StartY:Float, EndX:Float, EndY:Float ) 'Removes all fluid and boundary particles that are inside the specified rectangle
		Local ArraySize:Int = Particles.Length
		
		For Local I:Int = 0 Until Particles.Length
			If I >= ArraySize Then Exit
			
			Local P:TParticle = Particles[ I ]
			
			If P.ScreenX >= StartX And P.ScreenX <= EndX And P.ScreenY >= StartY And P.ScreenY <= EndY Then
				Particles = Particles[ .. I ] + Particles[ I + 1 .. ]
				
				TParticle.Count :- 1
				ArraySize :- 1
				I :- 1
			EndIf
		Next
		
		ArraySize = BoundaryParticles.Length
		
		For Local I:Int = 0 Until BoundaryParticles.Length
			If I >= ArraySize Then Exit
			
			Local P:TParticle = BoundaryParticles[ I ]
			
			If P.ScreenX >= StartX And P.ScreenX <= EndX And P.ScreenY >= StartY And P.ScreenY <= EndY Then
				BoundaryParticles = BoundaryParticles[ .. I ] + BoundaryParticles[ I + 1 .. ]
				
				TParticle.Count :- 1
				ArraySize :- 1
				I :- 1
			EndIf
		Next
		
		GCCollect()
	End Method
	
	Method AddParticle(X:Float, Y:Float)
		If X < 0 Then X = 0.0;
		If Y < 0 Then Y = 0.0;
		Particles :+ [ TParticle.Create( X*UNIT_SCALE*INV_WORLD_SCALE, Y*UNIT_SCALE*INV_WORLD_SCALE ) ]
	End Method
	
	Method AddBoundaryParticle( X:Float, Y:Float )
		BoundaryParticles :+ [ TParticle.Create( X*UNIT_SCALE*INV_WORLD_SCALE, Y*UNIT_SCALE*INV_WORLD_SCALE ) ]
	End Method
End Type

Type TParticle 'Well, I guess the variable names say pretty much everything
	Global Count:Int
	
	Field ScreenX:Float
	Field ScreenY:Float
	
	Field PositionX:Float
	Field PositionY:Float
	
	Field OldX:Float
	Field OldY:Float
	
	Field VelocityX:Float
	Field VelocityY:Float
	
	Field DeltaVelocityX:Float
	Field DeltaVelocityY:Float
	
	Field ForceX:Float
	Field ForceY:Float
	
	Field Density:Float
	Field DeltaDensity:Float
	Field InvDensity:Float
	Field InvDensitySq:Float
	
	Field Mass:Float
	
	Field Pressure:Float
	Field SoundSpeed:Float
	
	Field Succ:TParticle
	Field ID:Int
	
	Function Create:TParticle( X:Float, Y:Float )
		Local P:TParticle = New TParticle
			P.PositionX = X
			P.PositionY = Y
			P.OldX = X
			P.OldY = Y
			P.ScreenX = X*TSPH.INV_UNIT_SCALE*TSPH.WORLD_SCALE
			P.ScreenY = Y*TSPH.INV_UNIT_SCALE*TSPH.WORLD_SCALE
			P.Density = TSPH.REST_DENSITY 'Initialize the density and mass
			P.Mass = TSPH.PARTICLE_AREA*TSPH.REST_DENSITY
			P.ID = Count
		
		Count :+ 1
		
		Return P
	End Function
End Type