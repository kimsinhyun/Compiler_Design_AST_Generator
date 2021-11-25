#!/usr/bin/env python3

import sys
import os

YEAR = 2021
CON_MD_DIR = './_md/'

src = [
    'c1',
    'c2',
    'c3',
    'c4',
    'c5',
    'c6',
    'c7',
    'c8',
    'c9',
    'c10',
    'c11',
    'c12',
    'c13',
    'c14',
    'c15',
    'c16',
    'c17',
    'c18',
    'c19',
    'c20',
    'c21',
    'c22',
    'c23',
    'c24',
    'c25',
    'c26',
    'c27',
    'c28',
    'c29',
    'c30',
    'c31',
    'c32',
    'c33',
    'c34',
    'c35',
    'c36',
    'c37',
    'c38',
    'c39',
    'c40',
    'c41',
    'c42']

def convertFile(name, ifname, ofname, jekyllname):
    fi = open(ifname, 'r')
    fo = open(ofname, 'w')
    # frontmatter:
    fo.write('---\n')
    fo.write('layout: course_page\n')
    fo.write('title: CSI4104-01 Compiler Design\n')
    fo.write('permalink: /courses/csi4104/assignment_3/' + jekyllname + '\n')
    fo.write('year: ' + str(YEAR) + '\n')
    fo.write('---\n')
    # content:
    fo.write('\n##### Testcase ' + name + ' of Assignment 3:\n\n')
    # code block:
    fo.write('{% highlight c %}\n')
    for line in fi:
        fo.write(line)
    fo.write('{% endhighlight %}\n')
    # link to AST image:
    fo.write('![' + name + ']({% link _courses/csi4104/assignment_3/' + name + '.png %})\n') 
    fo.close()
    fi.close()


#######
# Main:
#######

#
# Check/create conversion directories:
#
if not os.path.isdir(CON_MD_DIR):
    os.mkdir(CON_MD_DIR)


for ex in src:
    name = ex 
    print('converting', name)
    in_name = name + '.mc'
    jekyll_name = name + '.html'
    out_name = CON_MD_DIR + name + '.md'
    convertFile(name, in_name, out_name, jekyll_name)
