#/bin/bash
ghc --make zimmerbingo || exit 1
strip zimmerbingo
echo "Uploading to $@"
rsync zimmerbingo "$@"
