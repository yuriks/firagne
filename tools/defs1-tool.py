#!/usr/bin/env python

import sys
import struct
from mmap import mmap

fs_header = '<6sxx8si'
table_entry = '<8siB3B'

def new_fs(fname, bootfn):
	with open(fname, 'wb') as f:
		# Sector 1 - Bootsector
		with open(bootfn, 'rb') as boot:
			f.write(boot.read())

		# Sector 2 - FS header
		header = struct.pack(fs_header, *('CBFS01', '', 2879))
		f.write(header)
		f.write('\xff'*(512 - len(header)))

		# Sector 3 - Root file table
		f.write('\x00'*(512*1)) # empty

		# Sector 4-6 - Bitmap
		f.write('\x1f') # first 5 bits set
		f.write('\x00'*(512*3 -1))

def assemble_bytes(bytes):
	val = 0

	if sys.byteorder == 'little':
		for i in xrange(len(bytes)):
			val |= bytes[i] << (i * 8)
	elif sys.byteorder == 'big':
		for i in xrange(len(bytes)-1, -1, -1):
			val |= bytes[i] << (i * 8)

	return

def unpack_dir_entry(str):
	(fnamer, type, flags, sz1, sz2, sz3) = struct.unpack(table_entry, str)
	sz = assemble_bytes((sz1, sz2, sz3))

	fname = fnamer.split('\x00', 1)[0]

	return (fname, type, flags, sz)
	

def print_dir_entry(str):
	(fname, type, flags, sz) = unpack_dir_entry(str)
	
	if len(fname) != 0:
		print "%8s {%08x} %02x %d" % (fname, type, flags, sz)

def list_dir(fm, dir_block):
	fm.seek(dir_block * 512)
	for i in xrange(0, 512, 16):
		print_dir_entry(fm.read(16))

def prog_menu(fm):
	cur_dir = 3

	print "--- ls, cd, put, exit"
	while True:
		cmd = raw_input()
		if cmd == 'ls':
			list_dir(fm, cur_dir)
		elif cmd == 'cd':
			pass
		elif cmd == 'put':
			pass
		elif cmd == 'exit':
			break
		

def prog_main(args):
	try:
		f = open(args[1], 'r+b')
	except IOError:
		if len(args) > 2:
			new_fs(args[1], args[2])
			f = open(fname, 'r+b')
		else:
			print "Couldn't find %s." % (args[1],)

	fm = mmap(f.fileno(), 0)
	prog_menu(fm)
	fm.close()
		

if __name__ == '__main__':
	prog_main(sys.argv)
