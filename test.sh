#!/bin/sh

count=$1

if [ -z $1 ]; then
    echo $$
    count=0
fi

if [ "$count" -lt 4 ]; then
    $0 `expr $count + 1`
else
    sleep 600
fi
