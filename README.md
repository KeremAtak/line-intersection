# line segment intersection

This program checks whether two line segments intersect. First it checks whether they can intersect (they're parallel or not), and if they can intersect it also checks if the intersection happens at the line segments. At successful intersections the coordinate where they intersect is returned.

Call function `line-segment-intersection` with repl and pass the line segments in format `["(0,-2)","(4,10)","(2,-2)","(0,4)"]` in which first and second points form the first line segment, and the rest form the second line segment.

Generated with Leiningen.