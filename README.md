# ggVenn
Venn diagram in ggplot format.
ggVenn is a wrapper of VennDiagram.
ggVenn can be used two to five items as same as VennDiagram.
You can get the overlapped list using a function venn.overlaplist. Although VennDiagram::calculate.overlap(x) returns names in the number in venn diagram, venn.overlaplist(x) returns names in overlap.


Example:

#two items

ggVenn(list(A = 1:150, B = 121:170))

x <- venn.overlaplist(list(A = 1:150, B = 121:170))

#three items

ggVenn(list(A = 1:150, B = 121:170, C = 101:200))

x <- venn.overlaplist(list(A = 1:150, B = 121:170, C = 101:200))

#four items

ggVenn(list(A = 1:150, B = 121:170, C = 101:200, D = 150:250))

x <- venn.overlaplist(list(A = 1:150, B = 121:170, C = 101:200, D = 150:250))

#five items

ggVenn(list(A = 1:150, B = 121:170, C = 101:200, D = 150:250, E = 200:300))


