#!/bin/sh

ADDRESS="unix!./.pipe_socket"

9umount dir 2>/dev/null
killall -s TERM Pipe
rm ./.pipe_socket
ghc Pipe.hs
./Pipe -a "$ADDRESS" &
sleep 1
9mount -i "$ADDRESS" dir
dmesg >> dir/input
cat dir/output

