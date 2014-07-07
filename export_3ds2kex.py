#!BPY
# coding: utf-8
# 3ds Exporter for KEX

# This script Exports a 3ds file primarily for KEX translation into Real Flight G3.5

# The KEX version of this script is based on the 3ds exporter available with blender. (v0.90a)
# Automatically creates KEX SUP file.
# Optionally calls 3DS2KEX.exe utility from Knife Edge software to create KEX file.

# Exporting is based on 3ds loader from www.gametutorials.com(Thanks DigiBen) and using information
# from the lib3ds project (http://lib3ds.sourceforge.net/) sourcecode.
#


# ***** BEGIN GPL LICENSE BLOCK *****
#
# Script copyright (C) Bob Holcomb
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
# ***** END GPL LICENCE BLOCK *****
# --------------------------------------------------------------------------

######################################################
# Add-on definition
######################################################

bl_info = {
    "name": "Export to KEX (RealFlight) file format",
    "author": ["Campbell Barton", "Bob Holcomb", "Richard Lärkäng", "Damien McGinnes", "Mark Stijnman","Alfonso Garcia"],
    "version": (1, 0),
    "blender": (2, 7, 0),
    "location": "File -> Export -> Export to KEX file",
    "description": "Exports selection in a format readable for the RealFlight program",
    "warning": "",
    "wiki_url": "",
    "tracker_url": "",
    "category": "Import-Export"}


######################################################
# Importing modules
######################################################

import math
import mathutils
import time
import bpy
import struct
import sys
import os

from bpy.types import Operator, AddonPreferences
from bpy.props import StringProperty, BoolProperty

# Properties controls
CS_GLOBALOPTIONS = {}
CS_GLOBALOPTIONS_OBJ = None

class OBJECT_OT_kex_export(Operator,AddonPreferences):
    """KEX export class"""
    bl_idname = 'object.kex_export'
    bl_label = '3DS to KEX export'
    bl_options = {'REGISTER','UNDO'}

    kexfile = StringProperty(
        name='Location of 3DS2KEX.exe utility',
		subtype='FILE_PATH',
    	)
    enablekex = BoolProperty(
		name='Enable creation of KEX by calling 3DS2KEX utility',
		default=False,
		)
    enablesup = BoolProperty(
		name='Enable creation of custom SUP file for pivot and NUP parameters.',
		default=False,
		)
    enablebeta = BoolProperty(
		name='Enable beta 3ds2kex utility features (Dec 2007 Beta release from Knife Edge)',
		default=False,
		)

    def draw(self, context):
        layout = self.layout
        layout.label(text="Preferences for KEX file export")
        layout.prop(self, "kexfile")
        layout.prop(self, "enablekex")
        layout.prop(self, "enablesup")
        layout.prop(self, "enablebeta")

    filepath = StringProperty(subtype="FILE_PATH")

    def invoke(self,context,event):
        context.window_manager.fileselect_add(self)
        return {'RUNNING_MODAL'}    

    def execute(self, context):
        bpy.context.window.cursor_set('WAIT')
        save_3ds(self, self.filepath)
        bpy.context.window.cursor_set('DEFAULT')
        return {'FINISHED'}

def menu_func(self, context):
	self.layout.operator_context = 'INVOKE_DEFAULT'
	self.layout.operator(OBJECT_OT_kex_export.bl_idname, text="Export to KEX file")
		
def register():
	bpy.utils.register_class(OBJECT_OT_kex_export)
	bpy.types.INFO_MT_file_export.append(menu_func)

def unregister():		
    bpy.types.INFO_MT_file_export.remove(menu_func)
    bpy.utils.unregister_class(OBJECT_OT_kex_export)

# So 3ds max can open files, limit names to 12 in length
# this is verry annoying for filenames!
name_unique = {}
name_mapping = {}
def sane_name(name, nameBucket, isFilename):
	if nameBucket not in name_mapping:
		name_mapping[nameBucket] = {}
		name_unique[nameBucket] = []

	name_fixed = name_mapping[nameBucket].get(name)
	if name_fixed != None:
		return name_fixed

	if (isFilename) and (len(name) > 12):
		print('Warning: Filenames must be less than 12 characters (\'' + name + '\')')
		new_name = name[:12]
	else:
		new_name = name

	i = 0

	while new_name in name_unique[nameBucket]:
		if (isFilename) or (new_name != name):
			new_name = new_name[:-4] + '.%.3d' % i
		else:
			new_name = new_name + '.%.3d' % i
		i+=1

	name_unique[nameBucket].append(new_name)
	name_mapping[nameBucket][name] = new_name
	return new_name

######################################################
# Data Structures
######################################################

#Some of the chunks that we will export
#----- Primary Chunk, at the beginning of each file
PRIMARY = int("0x4D4D",16)

#------ Main Chunks
OBJECTINFO   =      int("0x3D3D",16);      #This gives the version of the mesh and is found right before the material and object information
VERSION      =      int("0x0002",16);      #This gives the version of the .3ds file
KFDATA       =      int("0xB000",16);      #This is the header for all of the key frame info

#------ sub defines of OBJECTINFO
MATERIAL     =      45055		#0xAFFF				// This stored the texture info
OBJECT       =      16384		#0x4000				// This stores the faces, vertices, etc...

#>------ sub defines of MATERIAL
MATNAME      =      int("0xA000",16);      # This holds the material name
MATAMBIENT   =      int("0xA010",16);      # Ambient color of the object/material
MATDIFFUSE   =      int("0xA020",16);      # This holds the color of the object/material
MATSPECULAR  =      int("0xA030",16);      # SPecular color of the object/material
MATSHINESS   =      int("0xA040",16);      # Shininess of the object/material (percent)
MATSHIN2     =      int("0xA041",16);      # Specularity of the object/material (percent)
MATMAP       =      int("0xA200",16);      # This is a header for a new material
MATMAPFILE   =      int("0xA300",16);      # This holds the file name of the texture
MATTRANS     =      int("0xA050",16);      # Transparency value (i.e. =100-OpacityValue) (percent)

RGB1         =      int("0x0011",16)
RGB2         =      int("0x0012",16)
PCT          =      int("0x0030",16)
MASTERSCALE  =      int("0x0100",16)

#>------ sub defines of OBJECT
OBJECT_MESH  =      int("0x4100",16);      # This lets us know that we are reading a new object
OBJECT_LIGHT =      int("0x4600",16);      # This lets un know we are reading a light object
OBJECT_CAMERA=      int("0x4700",16);      # This lets un know we are reading a camera object

#>------ sub defines of CAMERA
OBJECT_CAM_RANGES=  int("0x4720",16);      # The camera range values

#>------ sub defines of OBJECT_MESH
OBJECT_VERTICES =   int("0x4110",16);      # The objects vertices
OBJECT_FACES    =   int("0x4120",16);      # The objects faces
OBJECT_MATERIAL =   int("0x4130",16);      # This is found if the object has a material, either texture map or color
OBJECT_UV       =   int("0x4140",16);      # The UV texture coordinates
OBJECT_TRANS_MATRIX=int("0x4160",16); # The Object Matrix

#>------ sub defines of KFDATA
KFDATA_KFHDR    =   int("0xB00A",16);
KFDATA_KFSEG    =   int("0xB008",16);
KFDATA_KFCURTIME=   int("0xB009",16);
KFDATA_OBJECT_NODE_TAG=int("0xB002",16);

#>------ sub defines of OBJECT_NODE_TAG
OBJECT_NODE_ID  =   int("0xB030",16);
OBJECT_NODE_HDR =   int("0xB010",16);
OBJECT_PIVOT    =   int("0xB013",16);
OBJECT_INSTANCE_NAME=int("0xB011",16);
POS_TRACK_TAG   =   int("0xB020",16);
ROT_TRACK_TAG   =   int("0xB021",16);
SCL_TRACK_TAG   =   int("0xB022",16);
BOUNDBOX        =   int("0xB014",16);

def uv_key(uv):
	return round(uv.x, 6), round(uv.y, 6)

# size defines:
SZ_SHORT = 2
SZ_INT   = 4
SZ_FLOAT = 4

class _3ds_short(object):
	'''Class representing a short (2-byte integer) for a 3ds file.
	*** This looks like an unsigned short H is unsigned from the struct docs - Cam***'''
	__slots__ = 'value'
	def __init__(self, val=0):
		self.value=val

	def get_size(self):
		return SZ_SHORT

	def write(self,file):
		self.value = int(self.value)
		if (self.value>32767):
			file.write(struct.pack("<H", self.value))
		else:
			file.write(struct.pack("<h", self.value))

	def __str__(self):
		return str(self.value)

class _3ds_int(object):
	'''Class representing an int (4-byte integer) for a 3ds file.'''
	__slots__ = 'value'
	def __init__(self, val=0):
		self.value=val

	def get_size(self):
		return SZ_INT

	def write(self,file):
		file.write(struct.pack("<i", self.value))

	def __str__(self):
		return str(self.value)

class _3ds_float(object):
	'''Class representing a 4-byte IEEE floating point number for a 3ds file.'''
	__slots__ = 'value'
	def __init__(self, val=0.0):
		self.value=val

	def get_size(self):
		return SZ_FLOAT

	def write(self,file):
		file.write(struct.pack("<f", self.value))

	def __str__(self):
		return str(self.value)


class _3ds_string(object):
	'''Class representing a zero-terminated string for a 3ds file.'''
	__slots__ = ('value',)
	def __init__(self, val=""):
		self.value=val

	def get_size(self):
		return (len(self.value)+1)

	def write(self,file):
		binary_format = "<%ds" % (len(self.value)+1)
		file.write(struct.pack(binary_format, bytearray(self.value,'ascii')))

	def __str__(self):
		return self.value

class _3ds_point_3d(object):
	'''Class representing a three-dimensional point for a 3ds file.'''
	__slots__ = 'x','y','z'
	def __init__(self, point=(0.0,0.0,0.0)):
		self.x, self.y, self.z = point

	def get_size(self):
		return 3*SZ_FLOAT

	def write(self,file):
		file.write(struct.pack('<3f', self.x, self.y, self.z))

	def __str__(self):
		return '(%f, %f, %f)' % (self.x, self.y, self.z)

# Used for writing a track
class _3ds_point_4d(object):
	'''Class representing a four-dimensional point for a 3ds file, for instance a quaternion.'''
	__slots__ = 'x','y','z','w'
	def __init__(self, point=(0.0,0.0,0.0,0.0)):
		self.x, self.y, self.z, self.w = point

	def get_size(self):
		return 4*SZ_FLOAT

	def write(self,file):
		data=struct.pack('<4f', self.x, self.y, self.z, self.w)
		file.write(data)

	def __str__(self):
		return '(%f, %f, %f, %f)' % (self.x, self.y, self.z, self.w)

class _3ds_point_uv(object):
	'''Class representing a UV-coordinate for a 3ds file.'''
	__slots__ = 'uv'
	def __init__(self, point=(0.0,0.0)):
		self.uv = point

	def __cmp__(self, other):
		return cmp(self.uv,other.uv)

	def get_size(self):
		return 2*SZ_FLOAT

	def write(self,file):
		data=struct.pack('<2f', self.uv[0], self.uv[1])
		file.write(data)

	def __str__(self):
		return '(%g, %g)' % self.uv

class _3ds_rgb_color(object):
	'''Class representing a (24-bit) rgb color for a 3ds file.'''
	__slots__ = 'r','g','b'
	def __init__(self, col=(0,0,0)):
		self.r, self.g, self.b = col

	def get_size(self):
		return 3

	def write(self,file):
		br =int(255*self.r).to_bytes(1,'little')
		bg =int(255*self.g).to_bytes(1,'little')
		bb =int(255*self.b).to_bytes(1,'little')
		file.write( struct.pack('<3c', br, bg, bb ) )

	def __str__(self):
		return '{%f, %f, %f}' % (self.r, self.g, self.b)

class _3ds_face(object):
	'''Class representing a face for a 3ds file.'''
	__slots__ = 'vindex'
	def __init__(self, vindex):
		self.vindex = vindex

	def get_size(self):
		return 4*SZ_SHORT

	def write(self,file):
		# The last zero is only used by 3d studio
		file.write(struct.pack("<4h", self.vindex[0],self.vindex[1], self.vindex[2], 0))

	def __str__(self):
		return '[%d %d %d]' % (self.vindex[0],self.vindex[1], self.vindex[2])

class _3ds_array(object):
	'''Class representing an array of variables for a 3ds file.

	Consists of a _3ds_short to indicate the number of items, followed by the items themselves.
	'''
	__slots__ = 'values', 'size'
	def __init__(self):
		self.values=[]
		self.size=SZ_SHORT

	# add an item:
	def add(self,item):
		self.values.append(item)
		self.size+=item.get_size()

	def get_size(self):
		return self.size

	def write(self,file):
		_3ds_short(len(self.values)).write(file)
		#_3ds_int(len(self.values)).write(file)
		for value in self.values:
			value.write(file)

	# To not overwhelm the output in a dump, a _3ds_array only
	# outputs the number of items, not all of the actual items.
	def __str__(self):
		return '(%d items)' % len(self.values)

class _3ds_named_variable(object):
	'''Convenience class for named variables.'''

	__slots__ = 'value', 'name'
	def __init__(self, name, val=None):
		self.name=name
		self.value=val

	def get_size(self):
		if (self.value==None):
			return 0
		else:
			return self.value.get_size()

	def write(self, file):
		if (self.value!=None):
			self.value.write(file)

	def dump(self,indent):
		if (self.value!=None):
			spaces=""
			for i in range(indent):
				spaces+="  ";
			if (self.name!=""):
				print( spaces, self.name, " = ", self.value )
			else:
				print( spaces, "[unnamed]", " = ", self.value )


#the chunk class
class _3ds_chunk(object):
	'''Class representing a chunk in a 3ds file.

	Chunks contain zero or more variables, followed by zero or more subchunks.
	'''
	__slots__ = 'ID', 'size', 'variables', 'subchunks'
	def __init__(self, id=0):
		self.ID=_3ds_short(id)
		self.size=_3ds_int(0)
		self.variables=[]
		self.subchunks=[]

	def set_ID(id):
		self.ID=_3ds_short(id)

	def add_variable(self, name, var):
		'''Add a named variable.

		The name is mostly for debugging purposes.'''
		self.variables.append(_3ds_named_variable(name,var))

	def add_subchunk(self, chunk):
		'''Add a subchunk.'''
		self.subchunks.append(chunk)

	def get_size(self):
		'''Calculate the size of the chunk and return it.

		The sizes of the variables and subchunks are used to determine this chunk\'s size.'''
		tmpsize=self.ID.get_size()+self.size.get_size()
		for variable in self.variables:
			tmpsize+=variable.get_size()
		for subchunk in self.subchunks:
			tmpsize+=subchunk.get_size()
		self.size.value=tmpsize
		return self.size.value

	def write(self, file):
		'''Write the chunk to a file.

		Uses the write function of the variables and the subchunks to do the actual work.'''
		#write header
		self.ID.write(file)
		self.size.write(file)
		for variable in self.variables:
			variable.write(file)
		for subchunk in self.subchunks:
			subchunk.write(file)


	def dump(self, indent=0):
		'''Write the chunk to a file.

		Dump is used for debugging purposes, to dump the contents of a chunk to the standard output.
		Uses the dump function of the named variables and the subchunks to do the actual work.'''
		spaces=""
		for i in range(indent):
			spaces+="  ";
		print( spaces, "ID=", hex(self.ID.value), "size=", self.get_size() )
		for variable in self.variables:
			variable.dump(indent+1)
		for subchunk in self.subchunks:
			subchunk.dump(indent+1)



######################################################
# EXPORT
######################################################

def get_material_images(material):
	# blender utility func.
	images = []
	if material:
		for mtex in material.getTextures():
			if mtex and mtex.tex.type == Blender.Texture.Types.IMAGE:
				image = mtex.tex.image
				if image:
					images.append(image) # maye want to include info like diffuse, spec here.
	return images

def make_material_subchunk(id, color):
	'''Make a material subchunk.

	Used for color subchunks, such as diffuse color or ambient color subchunks.'''
	mat_sub = _3ds_chunk(id)
	col1 = _3ds_chunk(RGB1)
	col1.add_variable("color1", _3ds_rgb_color(color));
	mat_sub.add_subchunk(col1)
# optional:
	col2 = _3ds_chunk(RGB2)
	col2.add_variable("color2", _3ds_rgb_color(color));
	mat_sub.add_subchunk(col2)
	return mat_sub

def make_percent_subchunk(id, percentval):
	# Make a percentage based subchunk
	pct_sub = _3ds_chunk(id)
	pct1 = _3ds_chunk(PCT)
	pct1.add_variable("percent", _3ds_short(round(percentval*100,0)))
	pct_sub.add_subchunk(pct1)
	return pct_sub

def make_material_texture_chunk(id, images):
	""" Make Material Map texture chunk """
	# 4KEX: Add texture percentage value (100 = 1.0)
	mat_sub = make_percent_subchunk(id, 1)

	def add_image(img):
		#filename = img.filepath.split('\\')[-1].split('/')[-1]
		filename = img.name
		mat_sub_file = _3ds_chunk(MATMAPFILE)
		mat_sub_file.add_variable("mapfile", _3ds_string(sane_name(filename, "imgfile", True)))
		mat_sub.add_subchunk(mat_sub_file)

	for image in images:
		add_image(image)

	return mat_sub

def make_material_chunk(material, image):
	'''Make a material chunk out of a blender material.'''
	material_chunk = _3ds_chunk(MATERIAL)
	name = _3ds_chunk(MATNAME)

	if material:
		name_str = material.name
	else:
		name_str = 'None'
	# 4KEX: Removed image name adding to material name
	if image:
		name_str += " " + image.name

	name.add_variable("name", _3ds_string(sane_name(name_str, "material", False)))
	material_chunk.add_subchunk(name)

	if not material:
		material_chunk.add_subchunk(make_material_subchunk(MATAMBIENT, (0,0,0) ))
		material_chunk.add_subchunk(make_material_subchunk(MATDIFFUSE, (.8, .8, .8) ))
		material_chunk.add_subchunk(make_material_subchunk(MATSPECULAR, (1,1,1) ))
		material_chunk.add_subchunk(make_percent_subchunk(MATSHINESS, .2))
		material_chunk.add_subchunk(make_percent_subchunk(MATSHIN2, 1))
		material_chunk.add_subchunk(make_percent_subchunk(MATTRANS, 0))

	else:
		material_chunk.add_subchunk(make_material_subchunk(MATAMBIENT, [a*material.ambient for a in material.diffuse_color] ))
		material_chunk.add_subchunk(make_material_subchunk(MATDIFFUSE, material.diffuse_color))
		material_chunk.add_subchunk(make_material_subchunk(MATSPECULAR, material.specular_color))
		material_chunk.add_subchunk(make_percent_subchunk(MATSHINESS, material.roughness))
		material_chunk.add_subchunk(make_percent_subchunk(MATSHIN2, material.specular_intensity))
		material_chunk.add_subchunk(make_percent_subchunk(MATTRANS, 1-material.alpha))

		images = []
		if image:
			images.append(image)
			material_chunk.add_subchunk(make_material_texture_chunk(MATMAP, images))
			
	return material_chunk

class tri_wrapper(object):
	'''Class representing a triangle.

	Used when converting faces to triangles'''

	__slots__ = 'vertex_index', 'mat', 'image', 'faceuvs', 'offset'
	def __init__(self, vindex=(0,0,0), mat=None, image=None, faceuvs=None):
		self.vertex_index= vindex
		self.mat= mat
		self.image= image
		self.faceuvs= faceuvs
		self.offset= [0, 0, 0] # offset indicies

def extract_triangles(mesh):
	"""Extract triangles from a mesh.

 	If the mesh contains quads, they will be split into triangles.
	Assumes caller has already calculated tessfaces on the mesh"""
	tri_list = []

	img = None
	do_uv = bool(mesh.tessface_uv_textures)
	for i, face in enumerate(mesh.tessfaces):
		f_v = face.vertices

		uf = mesh.tessface_uv_textures.active.data[i] if do_uv else None

		if do_uv:
			f_uv = uf.uv
			img = uf.image if uf else None
			if img is not None:
				img = img.name

        # if f_v[3] == 0:
		if len(f_v) == 3:
			new_tri = tri_wrapper((f_v[0], f_v[1], f_v[2]), face.material_index, img)
			if (do_uv):
				new_tri.faceuvs = uv_key(f_uv[0].data.uv1), uv_key(f_uv[1].data.uv1), uv_key(f_uv[2].data.uv1)
			tri_list.append(new_tri)

		else:  # it's a quad
			new_tri = tri_wrapper((f_v[0], f_v[1], f_v[2]), face.material_index, img)
			new_tri_2 = tri_wrapper((f_v[0], f_v[2], f_v[3]), face.material_index, img)

			if (do_uv):
				new_tri.faceuvs = uv_key(f_uv[0].data.uv1), uv_key(f_uv[1].data.uv1), uv_key(f_uv[2].data.uv1)
				new_tri_2.faceuvs = uv_key(f_uv[0].data.uv1), uv_key(f_uv[2].data.uv1), uv_key(f_uv[3].data.uv1)

			tri_list.append(new_tri)
			tri_list.append(new_tri_2)

	return tri_list


def remove_face_uv(verts, tri_list):
	"""Remove face UV coordinates from a list of triangles.

    Since 3ds files only support one pair of uv coordinates for each vertex, face uv coordinates
    need to be converted to vertex uv coordinates. That means that vertices need to be duplicated when
    there are multiple uv coordinates per vertex."""

    # initialize a list of UniqueLists, one per vertex:
    #uv_list = [UniqueList() for i in range(len(verts))]
	unique_uvs = [{} for i in range(len(verts))]

    # for each face uv coordinate, add it to the UniqueList of the vertex
	for tri in tri_list:
		for i in range(3):
            # store the index into the UniqueList for future reference:
            # offset.append(uv_list[tri.vertex_index[i]].add(_3ds_point_uv(tri.faceuvs[i])))

			context_uv_vert = unique_uvs[tri.vertex_index[i]]
			uvkey = tri.faceuvs[i]

			offset_index__uv_3ds = context_uv_vert.get(uvkey)

			if not offset_index__uv_3ds:
				offset_index__uv_3ds = context_uv_vert[uvkey] = len(context_uv_vert), _3ds_point_uv(uvkey)

			tri.offset[i] = offset_index__uv_3ds[0]

    # At this point, each vertex has a UniqueList containing every uv coordinate that is associated with it
    # only once.

    # Now we need to duplicate every vertex as many times as it has uv coordinates and make sure the
    # faces refer to the new face indices:
	vert_index = 0
	vert_array = _3ds_array()
	uv_array = _3ds_array()
	index_list = []
	for i, vert in enumerate(verts):
		index_list.append(vert_index)

		pt = _3ds_point_3d(vert.co)  # reuse, should be ok
		uvmap = [None] * len(unique_uvs[i])
		for ii, uv_3ds in unique_uvs[i].values():
			# add a vertex duplicate to the vertex_array for every uv associated with this vertex:
			vert_array.add(pt)
            # add the uv coordinate to the uv array:
            # This for loop does not give uv's ordered by ii, so we create a new map
            # and add the uv's later
            # uv_array.add(uv_3ds)
			uvmap[ii] = uv_3ds

        # Add the uv's in the correct order
		for uv_3ds in uvmap:
            # add the uv coordinate to the uv array:
			uv_array.add(uv_3ds)

		vert_index += len(unique_uvs[i])

    # Make sure the triangle vertex indices now refer to the new vertex list:
	for tri in tri_list:
		for i in range(3):
			tri.offset[i] += index_list[tri.vertex_index[i]]
		tri.vertex_index = tri.offset

	return vert_array, uv_array, tri_list

def make_faces_chunk(tri_list, mesh, materialDict):
	"""Make a chunk for the faces.

	Also adds subchunks assigning materials to all faces."""

	materials = mesh.materials
	if not materials:
		mat = None

	face_chunk = _3ds_chunk(OBJECT_FACES)
	face_list = _3ds_array()

	if mesh.tessface_uv_textures:
        # Gather materials used in this mesh - mat/image pairs
		unique_mats = {}
		for i, tri in enumerate(tri_list):

			face_list.add(_3ds_face(tri.vertex_index))

			if materials:
				mat = materials[tri.mat]
				if mat:
					mat = mat.name

			img = tri.image

			try:
				context_mat_face_array = unique_mats[mat_name, img][1]
			except:
				if mat:
					name_str = mat
				else:	
					name_str = 'None'
				if img: 
					name_str += " " + img
				
				context_mat_face_array = _3ds_array()
				unique_mats[mat, img] = _3ds_string(sane_name(name_str, "material", False)), context_mat_face_array
			materialDict.setdefault( (mat,img) ,(materials[tri.mat],bpy.data.images[img] if img else None))
			context_mat_face_array.add(_3ds_short(i))

		face_chunk.add_variable("faces", face_list)
		for mat_name, mat_faces in unique_mats.values():
			obj_material_chunk = _3ds_chunk(OBJECT_MATERIAL)
			obj_material_chunk.add_variable("name", mat_name)
			obj_material_chunk.add_variable("face_list", mat_faces)
			face_chunk.add_subchunk(obj_material_chunk)

	else:

		obj_material_faces = []
		obj_material_names = []
		for m in materials:
			if m:
				obj_material_names.append(_3ds_string(sane_name(m.name,"material",False)))
				obj_material_faces.append(_3ds_array())
		n_materials = len(obj_material_names)

		for i, tri in enumerate(tri_list):
			face_list.add(_3ds_face(tri.vertex_index))
			if (tri.mat < n_materials):
				obj_material_faces[tri.mat].add(_3ds_short(i))

		face_chunk.add_variable("faces", face_list)
		for i in range(n_materials):
			obj_material_chunk = _3ds_chunk(OBJECT_MATERIAL)
			obj_material_chunk.add_variable("name", obj_material_names[i])
			obj_material_chunk.add_variable("face_list", obj_material_faces[i])
			face_chunk.add_subchunk(obj_material_chunk)

	return face_chunk

def make_vert_chunk(vert_array):
	'''Make a vertex chunk out of an array of vertices.'''
	vert_chunk = _3ds_chunk(OBJECT_VERTICES)
	vert_chunk.add_variable("vertices",vert_array)
	return vert_chunk

def make_uv_chunk(uv_array):
	'''Make a UV chunk out of an array of UVs.'''
	uv_chunk = _3ds_chunk(OBJECT_UV)
	uv_chunk.add_variable("uv coords", uv_array)
	return uv_chunk

def strMatrix(matrix):
	r = ''
	for v in matrix:
		for c in v:
			r += ("%f" % c) + ' '
		r += '\n'
	return r

def make_mesh_chunk(mesh, materialDict,ob, name_to_id):
	"""Make a chunk out of a Blender mesh."""

    # Extract the triangles from the mesh:
	tri_list = extract_triangles(mesh)
	if mesh.tessface_uv_textures:
        # Remove the face UVs and convert it to vertex UV:
		vert_array, uv_array, tri_list = remove_face_uv(mesh.vertices, tri_list)
	else:
        # Add the vertices to the vertex array:
		vert_array = _3ds_array()
		for vert in mesh.vertices:
			vert_array.add(_3ds_point_3d(vert.co))
        # no UV at all:
		uv_array = None

    # create the chunk:
	mesh_chunk = _3ds_chunk(OBJECT_MESH)

    # add vertex chunk:
	mesh_chunk.add_subchunk(make_vert_chunk(vert_array))
    # add faces chunk:

	mesh_chunk.add_subchunk(make_faces_chunk(tri_list, mesh, materialDict))

	mesh1 = _3ds_chunk(OBJECT_TRANS_MATRIX)

	scale_vector = ob.matrix_world.to_scale()
	offset_vector = ob.matrix_local.to_translation()
	
	ob_matrix = mathutils.Matrix().Identity(4)
	
	ob_matrix[0][0] = 1/scale_vector[0]
	ob_matrix[1][1] = 1/scale_vector[1]
	ob_matrix[2][2] = 1/scale_vector[2]

	ob_matrix[3][0] = -offset_vector[0]/scale_vector[0]
	ob_matrix[3][1] = -offset_vector[1]/scale_vector[1]
	ob_matrix[3][2] = -offset_vector[2]/scale_vector[2]

	print("Matrix for " + ob.name + " that has " + ("no parent" if ob.parent == None else ob.parent.name + " as parent"))
	print( strMatrix(ob_matrix) )

	mesh1.add_variable("w1", _3ds_float(ob_matrix[0][0]))
	mesh1.add_variable("w2", _3ds_float(ob_matrix[0][1]))
	mesh1.add_variable("w3", _3ds_float(ob_matrix[0][2]))
	mesh1.add_variable("x1", _3ds_float(ob_matrix[1][0]))
	mesh1.add_variable("x2", _3ds_float(ob_matrix[1][1]))
	mesh1.add_variable("x3", _3ds_float(ob_matrix[1][2]))
	mesh1.add_variable("y1", _3ds_float(ob_matrix[2][0]))
	mesh1.add_variable("y2", _3ds_float(ob_matrix[2][1]))
	mesh1.add_variable("y3", _3ds_float(ob_matrix[2][2]))
	mesh1.add_variable("z1", _3ds_float(ob_matrix[3][0]))
	mesh1.add_variable("z2", _3ds_float(ob_matrix[3][1]))
	mesh1.add_variable("z3", _3ds_float(ob_matrix[3][2]))

	mesh_chunk.add_subchunk(mesh1)

	# if available, add uv chunk:
	if uv_array:
		mesh_chunk.add_subchunk(make_uv_chunk(uv_array))

	return mesh_chunk

 # COMMENTED OUT FOR 2.42 RELEASE!! CRASHES 3DS MAX
def make_kfdata(start=0, stop=0, curtime=0):
	'''Make the basic keyframe data chunk'''
	kfdata = _3ds_chunk(KFDATA)

	kfhdr = _3ds_chunk(KFDATA_KFHDR)
	kfhdr.add_variable("revision", _3ds_short(1))
	# Not really sure what filename is used for, but it seems it is usually used
	# to identify the program that generated the .3ds:
	# 4KEX: Based on observations some sample 3DS files typically used start stop of 100 with curtime = 0
	kfhdr.add_variable("filename", _3ds_string("Blender"))
	kfhdr.add_variable("animlen", _3ds_int(100))

	kfseg = _3ds_chunk(KFDATA_KFSEG)
	kfseg.add_variable("start", _3ds_int(0))
	kfseg.add_variable("stop", _3ds_int(100))

	kfcurtime = _3ds_chunk(KFDATA_KFCURTIME)
	kfcurtime.add_variable("curtime", _3ds_int(0))

	kfdata.add_subchunk(kfhdr)
	kfdata.add_subchunk(kfseg)
	kfdata.add_subchunk(kfcurtime)
	return kfdata


def make_track_chunk(obj, ID, var_name, var_value):
	'''Make a chunk for track data.

	Depending on the ID, this will construct a position, rotation or scale track.'''
	track_chunk = _3ds_chunk(ID)
	track_chunk.add_variable("track_flags", _3ds_short())
	track_chunk.add_variable("unknown", _3ds_int())
	track_chunk.add_variable("unknown", _3ds_int())
	track_chunk.add_variable("nkeys", _3ds_int(1))
	# Next section should be repeated for every keyframe, but for now, animation is not actually supported.
	track_chunk.add_variable("tcb_frame", _3ds_int(0))
	track_chunk.add_variable("tcb_flags", _3ds_short())

	track_chunk.add_variable(var_name,var_value)

	return track_chunk

def make_kf_obj_node(obj, name_to_id):
	'''Make a node chunk for a Blender object.

	Takes the Blender object as a parameter. Object id's are taken from the dictionary name_to_id.
	Blender Empty objects are converted to dummy nodes.'''

	name = obj.name
	# main object node chunk:
	kf_obj_node = _3ds_chunk(KFDATA_OBJECT_NODE_TAG)
	# chunk for the object id:
	obj_id_chunk = _3ds_chunk(OBJECT_NODE_ID)
	# object id is from the name_to_id dictionary:
	obj_id_chunk.add_variable("node_id", _3ds_short(name_to_id[name]))

	# object node header:
	obj_node_header_chunk = _3ds_chunk(OBJECT_NODE_HDR)
	# object name:
	if obj.type == 'EMPTY' and False:	#Forcing to use the real name for empties 4KEX
		# Empties are called "$$$DUMMY" and use the OBJECT_INSTANCE_NAME chunk
		# for their 3name (see below):
		obj_node_header_chunk.add_variable("name", _3ds_string("$$$DUMMY"))
	else:
		# Add the name:
		obj_node_header_chunk.add_variable("name", _3ds_string(sane_name(name, "object", False)))
	# Add Flag variables (not sure what they do):
	# 4KEX: Based on observation flags1 is usually 0x0040
	obj_node_header_chunk.add_variable("flags1", _3ds_short(0x0040))
	obj_node_header_chunk.add_variable("flags2", _3ds_short(0))

	# Check parent-child relationships:
	parent = obj.parent
	if (parent == None) or (parent.name not in name_to_id):
		# If no parent, or the parents name is not in the name_to_id dictionary,
		# parent id becomes -1:
		obj_node_header_chunk.add_variable("parent", _3ds_short(-1))
	else:
		# Get the parent's id from the name_to_id dictionary:
		obj_node_header_chunk.add_variable("parent", _3ds_short(name_to_id[parent.name]))

	# add subchunks for object id and node header:
	kf_obj_node.add_subchunk(obj_id_chunk)
	kf_obj_node.add_subchunk(obj_node_header_chunk)

	# 4KEX: Add a pivot point at the object centre
	pivot_pos = obj.matrix_local.to_translation()
		
	obj_pivot_chunk = _3ds_chunk(OBJECT_PIVOT)
	obj_pivot_chunk.add_variable("pivot", _3ds_point_3d(pivot_pos))
	kf_obj_node.add_subchunk(obj_pivot_chunk)

	# Empty objects need to have an extra chunk for the instance name:
	if obj.type == 'EMPTY' and False:	#Will use a real object name for empties for now 4KEX
		obj_instance_name_chunk = _3ds_chunk(OBJECT_INSTANCE_NAME)
		obj_instance_name_chunk.add_variable("name", _3ds_string(sane_name(name, "object", False)))
		kf_obj_node.add_subchunk(obj_instance_name_chunk)

	# Add track chunks for position, rotation and scale:
	# 4KEX: Compute the position and rotation of the object centre
	# 4KEX: The mesh has already been positioned around the object centre and scaled appropriately
	obj_pos = obj.matrix_local.to_translation()
	obj_rot = obj.matrix_local.to_euler().to_quaternion().inverted()
	obj_scale = mathutils.Vector((1.0,1.0,1.0)) 
	if obj.parent != None:
		a = obj
		while a.parent != None:
			obj_rot.rotate( a.parent.matrix_parent_inverse.to_euler().to_quaternion().inverted())
			a = a.parent
		acc_pos = obj.parent.matrix_parent_inverse
		obj_pos = acc_pos * obj.matrix_world.to_translation()

	kf_obj_node.add_subchunk(make_track_chunk(obj, SCL_TRACK_TAG, "scale",_3ds_point_3d(obj_scale)))
	kf_obj_node.add_subchunk(make_track_chunk(obj, ROT_TRACK_TAG, "rotation",_3ds_point_4d((obj_rot.angle, obj_rot.axis[0], obj_rot.axis[1], obj_rot.axis[2]))))
	kf_obj_node.add_subchunk(make_track_chunk(obj, POS_TRACK_TAG, "position",_3ds_point_3d(obj_pos)))

	return kf_obj_node


def read_globaloptions():
	global CS_GLOBALOPTIONS_OBJ
	global CS_GLOBALOPTIONS

	if CS_GLOBALOPTIONS_OBJ != None:
		props = CS_GLOBALOPTIONS_OBJ.getAllProperties()

		for prop in props:
			prop_name = prop.name.lower()
			prop_case = prop.name

			# Determine the data type for this value and the value itself
			prop_type = 'STRING:'
			prop_data = str(prop.data)
			if prop.type.lower() == 'bool':
				prop_type = 'BOOL:'
				if prop.data:
					prop_data = 'Yes'
				else:
					prop_data = 'No'
			elif prop.type.lower() == 'int':
				prop_type = 'INT:'
			elif prop.type.lower() == 'float':
				prop_type = 'FLOAT:'

			CS_GLOBALOPTIONS[prop_name] = [ True, prop_case, prop_data, prop_type ]

def write_sup( file, ob, isEmpty ):
	global CS_GLOBALOPTIONS

	# Build properties from default values and CS_GLOBALOPTIONS
	this_props = {}

	# Properties are set using a list of:
	#	{ BOOL, STRING, STRING, STRING }
	# where,
	#	BOOl = Indicate if the property is handled in a special way (Name, Pivot, IsPivot, Parent)
	#		or if the property is handled 'blindly'
	#	STRING = Property name for output (proper case, where the dictionary index is LOWER case)
	#	STRING = Property value
	#	STRING = Property value prefix (for beta utility)
	this_props["pivot"] = [ False, "Pivot", "", "STRING:" ]
	this_props["ispivot"] = [ False, "IsPivot", "No", "BOOL:" ]
	this_props["name"] = [ False, "Name", sane_name(ob.name, "object", False), "STRING:" ]
	this_props["parent"] = [ False, "Parent", "RootFrame", "STRING:" ]

	# Override RootFrame parent, if required
	if ob.parent != None:
		this_props["parent"][2] = sane_name(ob.parent.name, "object", False)

	for prop in CS_GLOBALOPTIONS:
		this_props[prop] = CS_GLOBALOPTIONS[prop]

	props = ob.getAllProperties()

	for prop in props:
		prop_name = prop.name.lower()

		# Pick up existing blind status and proper case for the name
		prop_blind = True
		prop_case = prop.name
		if prop_name in this_props:
			prop_blind = this_props[prop_name][0]
			prop_case = this_props[prop_name][1]

		# Determine the data type for this value and the value itself
		prop_type = 'STRING:'
		prop_data = str(prop.data)
		if prop.type.lower() == 'bool':
			prop_type = 'BOOL:'
			if prop.data:
				prop_data = 'Yes'
			else:
				prop_data = 'No'
		elif prop.type.lower() == 'int':
			prop_type = 'INT:'
		elif prop.type.lower() == 'float':
			prop_type = 'FLOAT:'

		this_props[prop_name] = [ prop_blind, prop_case, prop_data, prop_type ]

	# Write SUP object header
	if isEmpty and False:	#Forced to skip using the $$$DUMMY name for now will use real name for empties
		file.write( '[' + '$$$DUMMY' + ']\r\n' )
	else:
		file.write( '[' + sane_name(ob.name, "object", False) + ']\r\n' )

	if ENABLE_BETA:
		file.write( this_props['ispivot'][1] + '=' + this_props['ispivot'][3] + this_props['ispivot'][2] + '\r\n' )
	else:
		file.write( this_props['ispivot'][1] + '=' + this_props['ispivot'][2] + '\r\n' )

	for prop in this_props:
		if this_props[prop][0]:
			if ENABLE_BETA:
				file.write( this_props[prop][1] + '=' + this_props[prop][3] + this_props[prop][2] + '\r\n' )
			else:
				file.write( this_props[prop][1] + '=' + this_props[prop][2] + '\r\n' )

	if ENABLE_BETA:
		file.write( this_props['name'][1] + '=' + this_props['name'][3] + this_props['name'][2] + '\r\n' )
	else:
		file.write( this_props['name'][1] + '=' + this_props['name'][2] + '\r\n' )

	if ENABLE_BETA:
		file.write( this_props['parent'][1] + '=' + this_props['parent'][3] + this_props['parent'][2] + '\r\n' )
	else:
		file.write( this_props['parent'][1] + '=' + this_props['parent'][2] + '\r\n' )

	if ENABLE_BETA:
		file.write( this_props['pivot'][1] + '=' + this_props['pivot'][3] + this_props['pivot'][2] + '\r\n' )
	else:
		file.write( this_props['pivot'][1] + '=' + this_props['pivot'][2] + '\r\n' )

	file.write( '\r\n' )

def append_to_name_to(objects,name_to_id):
	for ob,something in objects:
		name_to_id[ob.name]= len(name_to_id)

def save_3ds(exportOptions, filename):
	global CS_GLOBALOPTIONS_OBJ
	'''Save the Blender scene to a 3ds file.'''
	# Time the export

	if not filename.lower().endswith('.3ds'):
		filename += '.3ds'

	time1= time.time()

	sce = bpy.context.scene

	# Initialize the main chunk (primary):
	primary = _3ds_chunk(PRIMARY)
	# Add version chunk:
	version_chunk = _3ds_chunk(VERSION)
	version_chunk.add_variable("version", _3ds_int(3))
	primary.add_subchunk(version_chunk)

	# init main object info chunk:
	object_info = _3ds_chunk(OBJECTINFO)

	''' # COMMENTED OUT FOR 2.42 RELEASE!! CRASHES 3DS MAX
	''' # 4KEX: Enabled kfdata with changes. Hopefully will not crash 3DS MAX (not tested)
	# init main key frame data chunk:
	kfdata = make_kfdata()

	# Get all the supported objects selected in this scene:
	ob_sel= [ob for ob in sce.objects if ob.type == 'MESH' or ob.type == 'CURVE' ]
	for ob in ob_sel:
		if ob.name.upper() == '~CS_GLOBALOPTIONS':
			ob_sel.remove(ob)
			CS_GLOBALOPTIONS_OBJ = ob
			break

	#mesh_objects = [ (ob, me) for ob in ob_sel for me in (ob.to_mesh(sce,True,'PREVIEW',False),) if me ]
	mesh_objects = [ (ob, ob.to_mesh(sce,True,'RENDER',True,False)) for ob in ob_sel  ]
	empty_objects = [ (ob,None) for ob in ob_sel if ob.type == 'EMPTY' ]

	# Make a list of all materials used in the selected meshes (use a dictionary,
	# each material is added once):
	materialDict = dict()
	
	# The original code iterated over all objects, inspecting them
	# and depending if they were mesheszs building the material dictionary
	# Now we iterate directly over meshes and their materials
	for  mat in bpy.data.materials:
		for tex in mat.texture_slots:
			img = None
			if tex and hasattr(tex.texture,'image'):
				img = tex.texture.image
			if img:
				materialDict.setdefault( (mat.name,tex.texture.image.name) ,(mat,tex.texture.image ))
			else:
				materialDict.setdefault( (mat.name,None) ,(mat, None ))

	# 4KEX: Added MASTERSCALE element
	mscale = _3ds_chunk(MASTERSCALE)
	mscale.add_variable("scale", _3ds_float(1))
	object_info.add_subchunk(mscale)

	# Give all objects a unique ID and build a dictionary from object name to object id:
	name_to_id = {}
	append_to_name_to(mesh_objects,name_to_id)
	append_to_name_to(empty_objects,name_to_id)

	# Create object chunks for all meshes:
	i = 0
	for ob, blender_mesh in mesh_objects:
		# create a new object chunk
		object_chunk = _3ds_chunk(OBJECT)

		# set the object name
		object_chunk.add_variable("name", _3ds_string(sane_name(ob.name, "object", False)))

		# make a mesh chunk out of the mesh:
		mesh_chunk = make_mesh_chunk(blender_mesh, materialDict, ob, name_to_id)
		object_chunk.add_subchunk(mesh_chunk)
		object_info.add_subchunk(object_chunk)

		# 4KEX: export kfdata node
		kfdata.add_subchunk(make_kf_obj_node(ob, name_to_id))
		i+=i

	# Create chunks for all empties:
	# 4KEX: Re-enabled kfdata. Empty objects not tested yet.
	for ob in empty_objects:
		# Empties only require a kf object node:
		kfdata.add_subchunk(make_kf_obj_node(ob, name_to_id))

	# Add main object info chunk to primary chunk:
	primary.add_subchunk(object_info)

	# Make material chunks for all materials used in the meshes:
	for mat_and_image in materialDict.values():
		material_chunk = make_material_chunk(mat_and_image[0], mat_and_image[1])
		object_info.add_subchunk(material_chunk)

	# 4KEX: Export kfdata
	primary.add_subchunk(kfdata)

	# At this point, the chunk hierarchy is completely built.

	# Check the size:
	primary.get_size()
	# Open the file for writing:
	file = open( filename, 'wb' )

	# Recursively write the chunks to file:
	primary.write(file)

	# Close the file:
	file.close()

	# Check if SUP file is requested
	if exportOptions.enablesup:
		# Open the SUP file
		file = open( filename[:-4]+'.sup', 'w' )

		# Write the RootFrame object
		if exportOptions.enablebeta:
			file.write( '[RootFrame]\r\nIsPivot=BOOL:No\r\nName=STRING:RootFrame\r\nParent=STRING:\r\nPivot=STRING:\r\n\r\n' )
		else:
			file.write( '[RootFrame]\r\nIsPivot=No\r\nName=RootFrame\r\nParent=\r\nPivot=\r\n\r\n' )

		read_globaloptions()

		for ob, blender_mesh in mesh_objects:
			write_sup( file, ob, False )

		for ob in empty_objects:
			write_sup( file, ob, True )

		# Close the SUP file
		file.close()

	if exportOptions.enablekex:
		cmd_line = '""' + exportOptions.kexfile + '" -s -v3 \"' + filename + '\""'
		os.system(cmd_line)

	# Debugging only: report the exporting time:
	print( "3ds export time: %.2f" % (time.time() - time1) )

	# Debugging only: dump the chunk hierarchy:
	#primary.dump()

if __name__=='__main__':
    try:
        unregister()
    except:
        pass    
    register()
    OBJECT_OT_kex_export.filepath = 'E:\\RF\\test'
    OBJECT_OT_kex_export.kexfile = 'E:\\RF\\3ds2kex.exe'
    OBJECT_OT_kex_export.enablekex = True
    
    OBJECT_OT_kex_export.enablesup = False
    OBJECT_OT_kex_export.enablebeta = False
    save_3ds(OBJECT_OT_kex_export, OBJECT_OT_kex_export.filepath )
# save_3ds( '/test_b.3ds' )