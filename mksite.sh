ghc --make site.hs && ./site rebuild
cp -r _site/ ../5outh.github.io
./site watch