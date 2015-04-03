import System.IO
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Ord as Ord
import qualified Data.Maybe as Maybe
import qualified Data.Char as Char
import qualified Debug.Trace as Trace

main = do
    putStrLn "Begin"
    let sentence = words "chancellor walks the dog"
    let sentences = ["chancellor walks the dog", "the dog walks", "i make dinner", "the dog makes the dinner for us"]

    handle <- openFile "train.txt" ReadMode
    fileContents <- hGetContents handle

    let trainData = map (\(x:y:z:_) -> (x, y, z)) $ filter (\x -> x/=[]) $ map words $ init $ lines $ map Char.toLower fileContents
    let strictedTrainData = map (\(x, y, _) -> (x, y)) trainData
    let estrictedTrainData = foldr (\x acc -> if (snd x == ".") then ("", "*"):("", "*"):acc else x:acc) [] strictedTrainData
    let restrictedTrainData = ("", "*"):("", "*"):(foldr (\x acc -> if (wrongTag $ snd x) then acc else x:acc) [] estrictedTrainData)

    let threeTagCount = elementCounts $ map (\[x, y, z] -> (x, y, z)) $ gather 3 $ map (\(_, y) -> y) restrictedTrainData
    let twoTagCount = elementCounts $ map (\[x, y] -> (x, y)) $ gather 2 $ map (\(_, y) -> y) restrictedTrainData
    let oneTagCount = elementCounts $ map (\[x] -> x) $ gather 1 $ map (\(_, y) -> y) restrictedTrainData

    let tagWordCount = elementCounts $ restrictedTrainData

    let ef = e (Map.fromList oneTagCount) (Map.fromList tagWordCount)
    let qf = q (length restrictedTrainData) (Map.fromList threeTagCount) (Map.fromList twoTagCount) (Map.fromList oneTagCount)

    let tags = map (\(x, _) -> x) oneTagCount

    let tagger = dpbp tags ef qf

    -- single sentence
    putStrLn . show $ tagger sentence
    -- multiple sentences
    mapM (putStrLn . show) [ (s, tagger $ words s) | s <- sentences]

    hClose handle
    putStrLn "End"

-- BEGIN VIRTEBI ALGORITHM --
dp :: [String] -> (String -> String -> Double) -> (String -> String -> String -> Double) -> [String] -> Double
dp tags ef qf sentence = maximum [calc (n, u, v) | u <- tags, v <- tags]
    where
        n = length sentence + 1
        calc (0, "*", "*") = 1
        calc (0, u, v) = 0
        calc (k, u, v) = maximum $ [(Maybe.fromMaybe 0 $ Map.lookup (k-1, w, u) states) * (qf w u v) * (if (k >= 2) then (ef (sentence !! (k-2)) v) else 1) | w <- tags]
        bp (k, u, v) = fst $ List.maximumBy (Ord.comparing snd) $ [(w, (Maybe.fromMaybe 0 $ Map.lookup (k-1, w, u) states) * (qf w u v) * (if (k >= 2) then (ef (sentence !! (k-2)) v) else 1)) | w <- tags]
        states = Map.fromList [((k, u, v), calc (k, u, v)) | k <- [0..n], u <- tags, v <- tags]

dpbp :: [String] -> (String -> String -> Double) -> (String -> String -> String -> Double) -> [String] -> [String]
dpbp tags ef qf sentence = reverse $ reconstruct (n, fst bestEnd, snd bestEnd)
    where
        reconstruct (1, u, v) = []
        reconstruct (k, u, v) = v : reconstruct (k-1, bp (k, u, v), u)
        bestEnd = fst calculated
        calculated = List.maximumBy (Ord.comparing snd) $ [((u, v), calc (n, u, v)) | u <- tags, v <- tags]
        n = length sentence + 1
        calc (0, "*", "*") = 1
        calc (0, u, v) = 0
        calc (k, u, v) = maximum $ [(Maybe.fromMaybe 0 $ Map.lookup (k-1, w, u) states) * (qf w u v) * (if (k >= 2) then (ef (sentence !! (k-2)) v) else 1) | w <- tags]
        bp (k, u, v) = fst $ List.maximumBy (Ord.comparing snd) $ [(w, (Maybe.fromMaybe 0 $ Map.lookup (k-1, w, u) states) * (qf w u v) * (if (k >= 2) then (ef (sentence !! (k-2)) v) else 1)) | w <- tags]
        states = Map.fromList [((k, u, v), calc (k, u, v)) | k <- [0..n], u <- tags, v <- tags]
-- END VIRTEBI ALGORITHM --

-- BEGIN NAIVE ALGORITHM --
-- returns top match
tag :: [String] -> (String -> String -> Double) -> (String -> String -> String -> Double) -> [String] -> [String]
tag tags ef qf sentence = argmax pf (fixedSets (length sentence) tags)
    where
        pf = p ef qf sentence

argmax :: (Ord b) => (a -> b) -> [a] -> a
argmax f s = fst $ List.maximumBy (Ord.comparing snd) $ map (\x -> (x, f x)) s

-- returns 10 top matches
toptag :: [String] -> (String -> String -> Double) -> (String -> String -> String -> Double) -> [String] -> [([String], Double)]
toptag tags ef qf sentence = topargmax pf (fixedSets (length sentence) tags)
    where
        pf = p ef qf sentence

topargmax :: (Ord b) => (a -> b) -> [a] -> [(a, b)]
topargmax f s = take 10 $ List.reverse $ List.sortBy (Ord.comparing snd) $ map (\x -> (x, f x)) s
-- END NAIVE ALGORITHM --

p :: (String -> String -> Double) -> (String -> String -> String -> Double) -> [String] -> [String] -> Double
p ef qf sentence tags = product (map (\[x, y, z] -> qf x y z) $ gather 3 ("*":"*":tags)) * (product (zipWith ef sentence tags))

-- e (word, tag) = Count (tag | word) / Count (tag)
e :: Map.Map String Int -> Map.Map (String, String) Int -> String -> String -> Double
e tags wordTags word tag = (fromIntegral $ Maybe.fromMaybe 0 $ Map.lookup (word, tag) wordTags) / (fromIntegral $ Maybe.fromMaybe 1 $ Map.lookup tag tags)

-- q (X, Y, Z) = lambda * Count (XYZ) / Count (XY) + lambda * Count (YZ) / Count (Y) + lambda * Count (Z) / Count ()
q :: Int -> Map.Map (String, String, String) Int -> Map.Map (String, String) Int -> Map.Map String Int -> String -> String -> String -> Double
q n three two one first second third = (lambda * threegram) + (lambda * twogram) + (lambda * onegram)
    where
        lambda = 1 / 3
        threegram = (fromIntegral $ Maybe.fromMaybe 0 $ Map.lookup (first, second, third) three) / (fromIntegral $ Maybe.fromMaybe 1 $ Map.lookup (first, second) two)
        twogram = (fromIntegral $ Maybe.fromMaybe 0 $ Map.lookup (second, third) two) / (fromIntegral $ Maybe.fromMaybe 1 $ Map.lookup second one)
        onegram = (fromIntegral $ Maybe.fromMaybe 0 $ Map.lookup third one) / (fromIntegral $ n)

-- discarded tags
wrongTag :: String -> Bool
wrongTag t = elem t ["#","$","''","(",")",",",":","``"]

-- generates all sets of length n with elements from a list
fixedSets :: Int -> [a] -> [[a]]
fixedSets 0 _ = [[]]
fixedSets n xs = [x:g | x <- xs, g <- (fixedSets (n - 1) xs)]

-- counts the elements of a list [a] and returns a list [(a, Int)] with a count for every element
elementCounts :: (Ord a) => [a] -> [(a, Int)]
elementCounts xs =  map (\z@(x:_) -> (x, length z)) $ List.group $ List.sort xs

-- gathers n items from a list
gather :: Int -> [a] -> [[a]]
gather n = foldr (zipWith (:)) (repeat []) . take n . List.tails
