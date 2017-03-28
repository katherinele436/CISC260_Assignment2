-- Solution for Assignment 2
-- CISC 260
-- Winter 2017
-- M. Lamb
module Assignment2 where

-- sublistSums list sum returns a list of the sublists of list that add up to sum
sublistSums :: [Int] -> Int -> [[Int]]
sublistSums [] 0 = [[]]
sublistSums [] _ = []
sublistSums (x:xs) sum = [x:tailList | tailList <- sublistSums xs (sum-x)] ++
                         sublistSums xs sum
                         
-- matching list1 list2 returns a list of all positions that have the same values in list1 
-- and list2
-- example: matching [10,71,83,9,24,5,2] [9,71,26,9] = [1,3]
-- matching "abcde fghijkl" "apple pie" = [0,4,5]
-- matching "hello" "goodbye" = []
matching list1 list2 = matchHelper list1 list2 0
matchHelper [] _ _ = []
matchHelper _ [] _ = []
matchHelper (x:xs) (y:ys) index
    | x == y = index : (matchHelper xs ys (index+1))
    | otherwise = matchHelper xs ys (index+1)
    
-- A Group represents a pair of students who did an assignment together.
-- The first two elements of a Group tuple are the names of the students.
-- Through this assignment we assume those two names will not be the same.
-- The third element of the tuple will be the numerical mark the pair of
-- students got on the assignment.
type Project = (String, String, Int)
type CourseData = [Project]

-- A sample group list as an illustration.  You may use this for testing,
-- but make up group lists of your own too.
groups :: CourseData
groups = [("Harry","Hermoine",80), ("Ron","Ginny",72), 
          ("Hermoine","Ron",95), ("Neville","Ginny",84)]
          
legalCourse :: CourseData -> Bool
legalCourse course = legal1 course && legal2 course
    where
    -- legal1 course means the course does not contain any projects like ("David","David",90)
    legal1 [] = True
    legal1 ((name1,name2,_):moreGroups) = name1 /= name2 && legal1 moreGroups
    -- hasPair course name1 name2 means the course has a project done by name1 and name2
    -- (in either order)
    hasPair [] _ _ = False
    hasPair ((a,b,_):moreGroups) name1 name2 =
        (a == name1 && b == name2) || (a == name2 && b == name1) || hasPair moreGroups name1 name2
    -- legal2 course means the course does not contain two projects done by the same
    -- pair of students (regardless of order)
    legal2 [] = True
    legal2 [_] = True
    legal2 ((name1,name2,_):moreProjects) = 
        not (hasPair moreProjects name1 name2) && legal2 moreProjects
        
studentCount :: String -> CourseData -> Int
studentCount _ [] = 0
studentCount name ((name1,name2,_):moreProjects)
    | name == name1 = 1 + studentCount name moreProjects
    | name == name2 = 1 + studentCount name moreProjects
    | otherwise = studentCount name moreProjects

partners :: String -> CourseData -> [String]
partners _ [] = []
partners name ((student1,student2,_):moreGroups) 
    | student1 == name  = student2 : morePartners
    | student2 == name = student1 : morePartners
    | otherwise = morePartners
    where morePartners = partners name moreGroups 
    
    
courseAvg :: CourseData -> Float
courseAvg [] = 0
courseAvg cdata = courseSum cdata / fromIntegral (length cdata)
    where
    -- the sum of all of the marks
    courseSum [(_,_,mark)] = fromIntegral mark
    courseSum ((_,_,mark):moreProjects) = fromIntegral mark + courseSum moreProjects
    
studentAvg :: String -> CourseData -> Float
studentAvg name cdata = courseAvg (matchStudent name cdata)
    where
    -- matchStudent s cdata = a list of just the projects that student s worked on
    matchStudent _ [] = []
    matchStudent name ((name1,name2,mark):moreProjects) 
        | name == name1 = (name1,name2,mark):(matchStudent name moreProjects)
        | name == name2 = (name1,name2,mark):(matchStudent name moreProjects)
        | otherwise = matchStudent name moreProjects
    

students :: CourseData -> [String]
students [] = []
students ((name1,name2,_):moreProjects) = 
    addNoDups name1 (addNoDups name2 (students moreProjects))
    where
    addNoDups name nameList
        | elem name nameList = nameList
        | otherwise = name:nameList
   
