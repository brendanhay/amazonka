{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudTrail
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CloudTrail where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CloudTrail
import Test.AWS.CloudTrail.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDescribeTrails $
--             describeTrails
--
--         , testLookupEvents $
--             lookupEvents
--
--         , testStopLogging $
--             stopLogging
--
--         , testDeleteTrail $
--             deleteTrail
--
--         , testUpdateTrail $
--             updateTrail
--
--         , testCreateTrail $
--             createTrail
--
--         , testGetTrailStatus $
--             getTrailStatus
--
--         , testStartLogging $
--             startLogging
--
--           ]

--     , testGroup "response"
--         [ testDescribeTrailsResponse $
--             describeTrailsResponse
--
--         , testLookupEventsResponse $
--             lookupEventsResponse
--
--         , testStopLoggingResponse $
--             stopLoggingResponse
--
--         , testDeleteTrailResponse $
--             deleteTrailResponse
--
--         , testUpdateTrailResponse $
--             updateTrailResponse
--
--         , testCreateTrailResponse $
--             createTrailResponse
--
--         , testGetTrailStatusResponse $
--             getTrailStatusResponse
--
--         , testStartLoggingResponse $
--             startLoggingResponse
--
--           ]
--     ]

-- Requests

testDescribeTrails :: DescribeTrails -> TestTree
testDescribeTrails = req
    "DescribeTrails"
    "fixture/DescribeTrails"

testLookupEvents :: LookupEvents -> TestTree
testLookupEvents = req
    "LookupEvents"
    "fixture/LookupEvents"

testStopLogging :: StopLogging -> TestTree
testStopLogging = req
    "StopLogging"
    "fixture/StopLogging"

testDeleteTrail :: DeleteTrail -> TestTree
testDeleteTrail = req
    "DeleteTrail"
    "fixture/DeleteTrail"

testUpdateTrail :: UpdateTrail -> TestTree
testUpdateTrail = req
    "UpdateTrail"
    "fixture/UpdateTrail"

testCreateTrail :: CreateTrail -> TestTree
testCreateTrail = req
    "CreateTrail"
    "fixture/CreateTrail"

testGetTrailStatus :: GetTrailStatus -> TestTree
testGetTrailStatus = req
    "GetTrailStatus"
    "fixture/GetTrailStatus"

testStartLogging :: StartLogging -> TestTree
testStartLogging = req
    "StartLogging"
    "fixture/StartLogging"

-- Responses

testDescribeTrailsResponse :: DescribeTrailsResponse -> TestTree
testDescribeTrailsResponse = res
    "DescribeTrailsResponse"
    "fixture/DescribeTrailsResponse"
    (Proxy :: Proxy DescribeTrails)

testLookupEventsResponse :: LookupEventsResponse -> TestTree
testLookupEventsResponse = res
    "LookupEventsResponse"
    "fixture/LookupEventsResponse"
    (Proxy :: Proxy LookupEvents)

testStopLoggingResponse :: StopLoggingResponse -> TestTree
testStopLoggingResponse = res
    "StopLoggingResponse"
    "fixture/StopLoggingResponse"
    (Proxy :: Proxy StopLogging)

testDeleteTrailResponse :: DeleteTrailResponse -> TestTree
testDeleteTrailResponse = res
    "DeleteTrailResponse"
    "fixture/DeleteTrailResponse"
    (Proxy :: Proxy DeleteTrail)

testUpdateTrailResponse :: UpdateTrailResponse -> TestTree
testUpdateTrailResponse = res
    "UpdateTrailResponse"
    "fixture/UpdateTrailResponse"
    (Proxy :: Proxy UpdateTrail)

testCreateTrailResponse :: CreateTrailResponse -> TestTree
testCreateTrailResponse = res
    "CreateTrailResponse"
    "fixture/CreateTrailResponse"
    (Proxy :: Proxy CreateTrail)

testGetTrailStatusResponse :: GetTrailStatusResponse -> TestTree
testGetTrailStatusResponse = res
    "GetTrailStatusResponse"
    "fixture/GetTrailStatusResponse"
    (Proxy :: Proxy GetTrailStatus)

testStartLoggingResponse :: StartLoggingResponse -> TestTree
testStartLoggingResponse = res
    "StartLoggingResponse"
    "fixture/StartLoggingResponse"
    (Proxy :: Proxy StartLogging)
