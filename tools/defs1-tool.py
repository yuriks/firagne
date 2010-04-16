#!/usr/bin/env python

import sys
import struct

fs_header = '<6sxx8si'
table_entry = '<8siB3B'

def new_fs(fname, bootfn):
	f = file(fname, 'wb')
	
	# Sector 1 - Bootsector
	boot = file(bootfn, 'rb')
	f.write(boot.read())
	boot.close()

	# Sector 2 - FS header
	header = struct.pack(fs_header, *('CBFS01', '', 2879))
	f.write(header)
	f.write('\xff'*(512 - len(header)))

	# Sector 3 and 4 - Root file table
	f.write('\x00'*(512*2)) # empty

	# Sector 5 and 6 - Bitmap
	f.write('\x1f') # first 5 bits set
	f.write('\x00'*(512*2 -1))

	f.close()

if __name__ == '__main__':
	new_fs(sys.argv[1], sys.argv[2])
