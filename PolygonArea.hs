module PolygonArea where


--area function
computeArea :: [(Double,Double)] -> Double
computeArea [] = error "List is Empty!"
computeArea [_] = error "not valid Must contain at least two points"
computeArea [(x,y),(z,w)] = 0.5 * (det (x, y) (z, w) + det (z,w) (x, y))
computeArea ((x,y):(z,w):xs) = 0.5 * (det (x, y) (z, w) + computeArea ((z,w):xs) + det (x,y) (last xs))

--determinant function
det :: (Double,Double) -> (Double,Double) -> Double
det (x1, y1) (x2, y2) = (x1*y2 - x2*y1)





