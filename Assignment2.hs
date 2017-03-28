--Assignment2
--Katherine Le
--10188031
--CISC 260

module Assignment2 where
import Data.List

--Question1
--Take a list and return all the possible sublist of that list.
--Helper function
sublist :: [a] -> [[a]]
sublist [] = []
sublist (x:xs) = let combos = sublist xs
                   in [x] : map (x:) combos ++ combos

--Take a list of Ints and a single Int.
--Return a list of all of the sub-lists of the first parameter whose sum is equal to the second parameter.
--Can replace sublist by subsquence but I already wrote sublist so I used it anyways.
sublistSums :: [Int] -> Int -> [[Int]]
sublistSums list givenSum = filter (\x -> sum x == givenSum) $ sublist list


--Question2
--Take two lists as parameters and return a list of the positions in which list1 and list2 have the same value
matching l1 l2 = [i | (i, x, y) <- zip3 [0..] l1 l2, x == y]


--Question3
--Declare type
type Project = (String, String, Int)
type CourseData = [Project]


--Take a CourseData as a parameter and returns a Bool. Return True if the course data is “legal”.
legalCourse :: CourseData -> Bool
legalCourse [] = True
legalCourse ((name1,name2,mark):rest)
  = (name1/="" && name2/="" && name1/=name2 && mark >= 0 && mark <= 100) && not(checkDup1 (name1,name2) rest) && (legalCourse rest)

--Helper function for legalCourse to check if there are duplicated student names in courseData
checkDup1 (p,q) lst = foldl (\acc (x,y,_) -> if (x == p) && (y==q) || (x == q) && (y==p) then True || acc  else False || acc) False lst



--Takes a student name (String) and a CourseData as parameters and returns an Int which should be the number of projects the named student has worked on.
studentCount:: String -> CourseData -> Int
studentCount s [] = 0
studentCount s ((x,y,z):rest)
  | s == x || s == y = 1+(studentCount s rest)
  | otherwise = studentCount s rest


--Takes a student name and a CourseData as parameters and returns a list of student names (Strings)
--which should be a list of all of the other students the named student has worked with.
partners :: String -> CourseData -> [String]
partners s cd = [y | (x,y,_) <- cd, s == x] ++ [x | (x,y,_) <- cd, s == y]


--Takes a CourseData as a parameter and returns a Float, the average of all the project marks for the course. If the CourseData is empty (zero projects)
--this function should return zero.
courseAvg :: CourseData -> Float
courseAvg [] = 0.0
courseAvg cd = fromIntegral (courseAvgHelp cd) / fromIntegral (length(cd))

--Helper function which takes CourseData and return the sum of all the marks
courseAvgHelp :: CourseData -> Int
courseAvgHelp [] = 0
courseAvgHelp ((_,_,z):rest) = z + (courseAvgHelp rest)


--Takes a student name and a CourseData as parameters and returns a Float
--the average of all the projects that the named student worked on. 
studentAvg :: String -> CourseData -> Float
studentAvg s [] = 0
studentAvg s cd 
  | studentCount s cd == 0 = 0.0
  | otherwise = fromIntegral (sum (getMarks s cd)) /  fromIntegral(studentCount s cd)
  --sum of marks from all the project student has woked on divided by the number of project

--Take a student name and CourseData and return a list of mark from all the project student has woked on.
--Helper for studentAvg
getMarks :: String -> CourseData -> [Int]
getMarks s cd = [z | (x,y,z) <- cd, s == x] ++ [z | (x,y,z) <- cd, s == y]


--Take courseData and return a list of student names
--Helper for students function
nameList :: CourseData -> [String]
nameList ((x,y,z):xs) = x : y : nameList (xs) 
nameList _ = []

--Take list of student names and remove all duplicated names
--Helper for students function
removeDup :: Eq a => [a] -> [a]
removeDup [] = []
removeDup (x:xs) = x : removeDup (filter (/=x) xs)

--Takes a CourseData as a parameter and returns a list of student names (Strings)
--The result should be a list of all the students who worked on at least one project in the course.
students :: CourseData -> [String]
students [] = []
students ls = removeDup (nameList ls)