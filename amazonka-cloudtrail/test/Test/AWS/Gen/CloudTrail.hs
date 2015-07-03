-- Module      : Test.AWS.Gen.CloudTrail
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.CloudTrail where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.CloudTrail

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
testDescribeTrails = undefined

testLookupEvents :: LookupEvents -> TestTree
testLookupEvents = undefined

testStopLogging :: StopLogging -> TestTree
testStopLogging = undefined

testDeleteTrail :: DeleteTrail -> TestTree
testDeleteTrail = undefined

testUpdateTrail :: UpdateTrail -> TestTree
testUpdateTrail = undefined

testCreateTrail :: CreateTrail -> TestTree
testCreateTrail = undefined

testGetTrailStatus :: GetTrailStatus -> TestTree
testGetTrailStatus = undefined

testStartLogging :: StartLogging -> TestTree
testStartLogging = undefined

-- Responses

testDescribeTrailsResponse :: DescribeTrailsResponse -> TestTree
testDescribeTrailsResponse = resp
    "DescribeTrailsResponse"
    "fixture/DescribeTrailsResponse"
    (Proxy :: Proxy DescribeTrails)

testLookupEventsResponse :: LookupEventsResponse -> TestTree
testLookupEventsResponse = resp
    "LookupEventsResponse"
    "fixture/LookupEventsResponse"
    (Proxy :: Proxy LookupEvents)

testStopLoggingResponse :: StopLoggingResponse -> TestTree
testStopLoggingResponse = resp
    "StopLoggingResponse"
    "fixture/StopLoggingResponse"
    (Proxy :: Proxy StopLogging)

testDeleteTrailResponse :: DeleteTrailResponse -> TestTree
testDeleteTrailResponse = resp
    "DeleteTrailResponse"
    "fixture/DeleteTrailResponse"
    (Proxy :: Proxy DeleteTrail)

testUpdateTrailResponse :: UpdateTrailResponse -> TestTree
testUpdateTrailResponse = resp
    "UpdateTrailResponse"
    "fixture/UpdateTrailResponse"
    (Proxy :: Proxy UpdateTrail)

testCreateTrailResponse :: CreateTrailResponse -> TestTree
testCreateTrailResponse = resp
    "CreateTrailResponse"
    "fixture/CreateTrailResponse"
    (Proxy :: Proxy CreateTrail)

testGetTrailStatusResponse :: GetTrailStatusResponse -> TestTree
testGetTrailStatusResponse = resp
    "GetTrailStatusResponse"
    "fixture/GetTrailStatusResponse"
    (Proxy :: Proxy GetTrailStatus)

testStartLoggingResponse :: StartLoggingResponse -> TestTree
testStartLoggingResponse = resp
    "StartLoggingResponse"
    "fixture/StartLoggingResponse"
    (Proxy :: Proxy StartLogging)
