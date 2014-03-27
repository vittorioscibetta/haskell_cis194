-- CIS 194 Homework 2
-- LogAnalysis.hs
{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1

toInt :: String -> Int
toInt a = read a

toTimeStamp :: String -> TimeStamp
toTimeStamp a = read a

parseMessage :: String -> LogMessage
parseMessage message = case words message of 
                         ("E" : i : t : xs) -> LogMessage (Error (toInt i)) (toTimeStamp t) (unwords xs)
                         ("I" : t : xs) -> LogMessage Info (toTimeStamp t) (unwords xs)
                         ("W" : t : xs) -> LogMessage Warning (toTimeStamp t) (unwords xs)
                         xs -> Unknown (unwords xs)
                         


parse :: String -> [LogMessage]
parse  content  = map parseMessage (lines content) 

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _ ) tree = tree
insert message Leaf = Node Leaf message Leaf
insert l@(LogMessage _ stamp _) (Node treeLeft m@(LogMessage _ stampmessage _) treeRight) = 
			if stamp < stampmessage 
		          then Node (insert l treeLeft) m treeRight 
			  else Node treeLeft m (insert l treeRight)
