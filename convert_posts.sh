#!/bin/bash

for file in *.html
do
  filename=$(basename "$file")
  extension="${filename##*.}"
  filename="${filename%.*}"
  pandoc -f html -t markdown "$filename.html" -o "$filename.markdown"
done
