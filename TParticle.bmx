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

Type TParticle

	Global Count:Int      'Global particle counter

	Field ID:Int          'Particle ID
	
	Field ScreenX:Float   '?
	Field ScreenY:Float   '?
	
	Field PositionX:Float 'Particle X position
	Field PositionY:Float 'Particle Y position
	
	Field OldX:Float      'Old X position ?
	Field OldY:Float      'Old Y position ?
	
	Field VelocityX:Float 'X velocity
	Field VelocityY:Float 'Y velocity
	
	Field DeltaVelocityX:Float 'bleh
	Field DeltaVelocityY:Float 'Mmmm ?
	
	Field ForceX:Float    'X Force
	Field ForceY:Float    'Y Force
	
	Field Density:Float      'doh !
	Field DeltaDensity:Float 'wazup ?
	'Field InvDensity:Float   'This is... 1/Density \o/
	'Field InvDensitySq:Float 'and (1/Density) * (1/Density) , heh
	
	Field Mass:Float         'Particle Mass (bleh ?)
	
	Field Pressure:Float     'Particle Pressure, probably
	'Field SoundSpeed:Float   'Lesser than LightSpeed
	
	Field Succ:TParticle     'Next particle because we're in a linked list, isn't it ?
	
	'The particle constructor, tehehehe
	Function Create:TParticle(X:Float, Y:Float)
		Local P:TParticle = New TParticle
			P.PositionX = X
			P.PositionY = Y
			P.OldX = X
			P.OldY = Y
			P.ScreenX = X * TSPH.INV_UNIT_SCALE * TSPH.WORLD_SCALE 'TParticle shouldn't depend of TSPH :(
			P.ScreenY = Y * TSPH.INV_UNIT_SCALE * TSPH.WORLD_SCALE 'TParticle shouldn't depend of TSPH :(
			P.Density = TSPH.REST_DENSITY                          'TParticle shouldn't depend of TSPH :(
			P.Mass = TSPH.PARTICLE_AREA * TSPH.REST_DENSITY        'TParticle shouldn't depend of TSPH :(
			P.ID = Count
		
		Count :+ 1
		
		Return P
	End Function
End Type