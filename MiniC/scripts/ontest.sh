echo $1
TOPDIR=`pwd`
if [ -d MiniC/Parser/tst/base/AST_sinhyunScript ] 
then
    mkdir -p $TOPDIR/Parser/tst/base/AST_sinhyunScript
fi
java -jar $TOPDIR/../build/libs/MiniC-AstGen.jar -t $TOPDIR/Parser/tst/base/AST_sinhyunScript/$1.mc.u.u $TOPDIR/Parser/tst/base/AST_solutions_unparsed/$1.mc.u

diff -u $TOPDIR/Parser/tst/base/AST_sinhyunScript/$1.mc.u.u $TOPDIR/Parser/tst/base/AST_solutions_trees/$1.mc.ast