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

'Thoses 2 const shouldn't be here :(
Const GWIDTH:Int = 800  'Screen Width
Const GHEIGHT:Int = 600 'Screen Height

Type TSPH
	Const DAMPING:Float = 0.0015 'Damping used in the integration step. Feels a bit hacky, but looks nicer
	
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
		
		FluidGrid = New TParticle[GridWidth, GridHeight]
		BoundaryGrid = New TParticle[GridWidth, GridHeight]
		
		ParticleTex = GLTexFromPixmap(LoadPixmapPNG("incbin::Metaball.png"))
	End Method
	
	Method Update()
		RedistributeGrid()  'Slowest Method
		UpdateDensity()
		CalcForces()  '2nd slowest
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
		
		Local DirX:Int[] = [- 1, -1, 1, 1, -1, 1, 0, 0, 0]
		Local DirY:Int[] = [- 1, 1, -1, 1, 0, 0, -1, 1, 0]
		
		For Local J:Int = 0 Until Particles.Length
			Local P1:TParticle = Particles[J]
			
			Local IntX:Int = Int( P1.PositionX*INV_SMOOTHING_LENGTH )
			Local IntY:Int = Int( P1.PositionY*INV_SMOOTHING_LENGTH )
			
			For Local I:Int = 0 To 8 'This loop iterates over the cell the particle is in and all 8 neighbouring cells
				Local GridX:Int = IntX + DirX[ I ]
				Local GridY:Int = IntY + DirY[ I ]
				
				If GridX < 0 Or GridY < 0 Or GridX >= GridWidth Or GridY >= GridHeight Then Continue
				
				Local P2:TParticle = FluidGrid[GridX, GridY]
				
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
	
	' 2nd slowest method in simulation
	Method CalcForces()
		Local DirX:Int[] = [- 1, -1, 1, 1, -1, 1, 0, 0, 0]
		Local DirY:Int[] = [- 1, 1, -1, 1, 0, 0, -1, 1, 0]
		
		'Reset the second order deritatives of the position and the first deritative of the density
		For Local I:Int = 0 Until Particles.Length
			Particles[I].ForceX = GRAVITY_X * Particles[I].Density
			Particles[I].ForceY = GRAVITY_Y * Particles[I].Density
			Particles[I].DeltaVelocityX = 0.0
			Particles[I].DeltaVelocityY = 0.0
			Particles[I].DeltaDensity = 0.0
		Next
		
		'See the UpdateDensity method for more information about the grid iteration
		For Local J:Int = 0 Until Particles.Length
					
			Local P1:TParticle = Particles[J]
			
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
				
				P2 = BoundaryGrid[GridX, GridY]
				
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
			
			If Dist1 < REPULSIVE_DISTANCE Then P1.ForceX:-1.0 * (1.0 - Sqr(Dist1 * INV_REPULSIVE_DISTANCE)) * P1.Density
			If Dist2 < REPULSIVE_DISTANCE Then P1.ForceY:-1.0 * (1.0 - Sqr(Dist2 * INV_REPULSIVE_DISTANCE)) * P1.Density
			If Dist3 < REPULSIVE_DISTANCE Then P1.ForceX:+1.0 * (1.0 - Sqr(Dist3 * INV_REPULSIVE_DISTANCE)) * P1.Density
			If Dist4 < REPULSIVE_DISTANCE Then P1.ForceY:+1.0 * (1.0 - Sqr(Dist4 * INV_REPULSIVE_DISTANCE)) * P1.Density
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
			
			P.PositionX = Max(Min(P.PositionX, CONTAINER_WIDTH), 0.0)
			P.PositionY = Max(Min(P.PositionY, CONTAINER_HEIGHT), 0.0)
			
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
		glEnable(GL_TEXTURE_2D)
		glBindTexture(GL_TEXTURE_2D, ParticleTex)
		
		glEnable(GL_BLEND)
		glBlendFunc(GL_ONE, GL_ONE) 'Additive blending
		
		glBegin(GL_QUADS) 'Just a few rectangles
			Local S:Float = 14.0
			
			For Local P:TParticle = EachIn Particles
				Local col:Float = (0.4 + 0.2 * P.Pressure) * P.Density
				glColor4f(col ^ 1.5, col ^ 1.5, col, 1.0)
				
				glTexCoord2f(0.0, 0.0) ; glVertex2f(P.ScreenX - S, P.ScreenY - S)
				glTexCoord2f(1.0, 0.0) ; glVertex2f(P.ScreenX + S, P.ScreenY - S)
				glTexCoord2f( 1.0, 1.0 ); glVertex2f( P.ScreenX + S, P.ScreenY + S )
				glTexCoord2f(0.0, 1.0) ; glVertex2f(P.ScreenX - S, P.ScreenY + S)
			Next
		glEnd()
		
		glDisable(GL_TEXTURE_2D)
		
		glBlendFunc(GL_ZERO, GL_DST_COLOR) 'I don't really know how that one works - I just tried a few flag combinations and this one seemed to work
		
		glBegin(GL_QUADS)
			For Local I:Int = 0 To 1
				glVertex2f(0.0, 0.0)
				glVertex2f(GWIDTH, 0.0)
				glVertex2f(GWIDTH, GHEIGHT)
				glVertex2f(0.0, GHEIGHT)
			Next
		glEnd()
		
		glDisable(GL_BLEND)
		
	
		'Draw boundaries (brownish wall)		
		glColor4f(1.0, 0.5, 0.5, 1.0)
		glBegin(GL_QUADS) 'Brown boundaries
		For Local P:TParticle = EachIn BoundaryParticles
			glVertex2f(P.ScreenX - 3.0, P.ScreenY - 3.0)
			glVertex2f( P.ScreenX + 3.0, P.ScreenY - 3.0 )
			glVertex2f(P.ScreenX + 3.0, P.ScreenY + 3.0)
			glVertex2f(P.ScreenX - 3.0, P.ScreenY + 3.0)
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
