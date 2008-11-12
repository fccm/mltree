mltree is freesoftware released under the GPL license,
see the file COPYING.txt for details.

mltree is similar to the unix command line utility 'tree'.
The main difference is that currently tree dumps all the
content of a directory mixing files and directories, so
when a directory is recursively dumped before some files
of the current directory it makes it difficult to read.
So mltree first dumps all the regular files of the current
directory before to dump recursively the directories.

If you run mltree without any arguments it will assume to
dump the current directory.

You can give several directories as arguments.

To get help about command line arguments, run the command:
mltree --help
