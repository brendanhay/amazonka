{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.AWS.S3
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.S3.Encryption.Body
    ( bodyTests
    ) where

import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Network.AWS.Prelude
import           Network.AWS.S3.Encryption.Body
import           Test.AWS.Prelude
import           Test.Tasty
import           Test.Tasty.HUnit


bodyTests :: [TestTree]
bodyTests =
    [ testCase "forceChunkSize empty" testForceChunkSizeEmpty
    , testCase "forceChunkSize one empty chunk" testForceChunkSizeOneEmptyChunk
    , testCase "forceChunkSize collapse empty chunks" testForceChunkSizeCollapseEmptyChunks
    , testCase "forceChunkSize one small chunk" testForceChunkSizeOneSmallChunk
    , testCase "forceChunkSize misaligned chunks" testForceChunkSizeMisalignedChunks
    , testCase "forceChunkSize already aligned chunks" testForceChunkSizeAlreadyAlignedChunks
    , testCase "forceChunkSize leftover" testForceChunkSizeLeftover
    ]


testForceChunkSizeEmpty :: Assertion
testForceChunkSizeEmpty = do
    res <- runConduit (return () =$= forceChunkSize 123  $$ CL.consume)
    res @?= []


testForceChunkSizeOneEmptyChunk :: Assertion
testForceChunkSizeOneEmptyChunk = do
    res <- runConduit (CL.sourceList [""] =$= forceChunkSize 16 $$ CL.consume)
    res @?= [""]


testForceChunkSizeCollapseEmptyChunks :: Assertion
testForceChunkSizeCollapseEmptyChunks = do
    res <- runConduit (CL.sourceList ["", "", ""] =$= forceChunkSize 16 $$ CL.consume)
    res @?= [""]


testForceChunkSizeOneSmallChunk :: Assertion
testForceChunkSizeOneSmallChunk = do
    res <- runConduit (CL.sourceList ["foo"] =$= forceChunkSize 16 $$ CL.consume)
    res @?= ["foo"]


testForceChunkSizeMisalignedChunks :: Assertion
testForceChunkSizeMisalignedChunks = do
    let input = ["a", "aab", "bbccc", "dd", "deeeff", "f"]
    res <- runConduit (CL.sourceList input =$= forceChunkSize 3 $$ CL.consume)
    res @?= ["aaa", "bbb", "ccc", "ddd", "eee", "fff"]


testForceChunkSizeAlreadyAlignedChunks :: Assertion
testForceChunkSizeAlreadyAlignedChunks = do
    let input = ["aaa", "bbb", "ccc"]
    res <- runConduit (CL.sourceList input =$= forceChunkSize 3 $$ CL.consume)
    res @?= ["aaa", "bbb", "ccc"]


testForceChunkSizeLeftover :: Assertion
testForceChunkSizeLeftover = do
    let input = ["a", "aab"]
    res <- runConduit (CL.sourceList input =$= forceChunkSize 3 $$ CL.consume)
    res @?= ["aaa", "b"]
