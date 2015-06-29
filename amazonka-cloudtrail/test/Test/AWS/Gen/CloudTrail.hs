-- Module      : Test.AWS.Gen.CloudTrail
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ describeTrailsTest $
--             describeTrails
--
--         , lookupEventsTest $
--             lookupEvents
--
--         , stopLoggingTest $
--             stopLogging
--
--         , deleteTrailTest $
--             deleteTrail
--
--         , updateTrailTest $
--             updateTrail
--
--         , createTrailTest $
--             createTrail
--
--         , getTrailStatusTest $
--             getTrailStatus
--
--         , startLoggingTest $
--             startLogging
--
--           ]

--     , testGroup "response"
--         [ describeTrailsResponseTest $
--             describeTrailsResponse
--
--         , lookupEventsResponseTest $
--             lookupEventsResponse
--
--         , stopLoggingResponseTest $
--             stopLoggingResponse
--
--         , deleteTrailResponseTest $
--             deleteTrailResponse
--
--         , updateTrailResponseTest $
--             updateTrailResponse
--
--         , createTrailResponseTest $
--             createTrailResponse
--
--         , getTrailStatusResponseTest $
--             getTrailStatusResponse
--
--         , startLoggingResponseTest $
--             startLoggingResponse
--
--           ]
--     ]

-- Requests

describeTrailsTest :: DescribeTrails -> TestTree
describeTrailsTest = undefined

lookupEventsTest :: LookupEvents -> TestTree
lookupEventsTest = undefined

stopLoggingTest :: StopLogging -> TestTree
stopLoggingTest = undefined

deleteTrailTest :: DeleteTrail -> TestTree
deleteTrailTest = undefined

updateTrailTest :: UpdateTrail -> TestTree
updateTrailTest = undefined

createTrailTest :: CreateTrail -> TestTree
createTrailTest = undefined

getTrailStatusTest :: GetTrailStatus -> TestTree
getTrailStatusTest = undefined

startLoggingTest :: StartLogging -> TestTree
startLoggingTest = undefined

-- Responses

describeTrailsResponseTest :: DescribeTrailsResponse -> TestTree
describeTrailsResponseTest = resp
    "DescribeTrailsResponse"
    "fixture/DescribeTrailsResponse"
    (Proxy :: Proxy DescribeTrails)

lookupEventsResponseTest :: LookupEventsResponse -> TestTree
lookupEventsResponseTest = resp
    "LookupEventsResponse"
    "fixture/LookupEventsResponse"
    (Proxy :: Proxy LookupEvents)

stopLoggingResponseTest :: StopLoggingResponse -> TestTree
stopLoggingResponseTest = resp
    "StopLoggingResponse"
    "fixture/StopLoggingResponse"
    (Proxy :: Proxy StopLogging)

deleteTrailResponseTest :: DeleteTrailResponse -> TestTree
deleteTrailResponseTest = resp
    "DeleteTrailResponse"
    "fixture/DeleteTrailResponse"
    (Proxy :: Proxy DeleteTrail)

updateTrailResponseTest :: UpdateTrailResponse -> TestTree
updateTrailResponseTest = resp
    "UpdateTrailResponse"
    "fixture/UpdateTrailResponse"
    (Proxy :: Proxy UpdateTrail)

createTrailResponseTest :: CreateTrailResponse -> TestTree
createTrailResponseTest = resp
    "CreateTrailResponse"
    "fixture/CreateTrailResponse"
    (Proxy :: Proxy CreateTrail)

getTrailStatusResponseTest :: GetTrailStatusResponse -> TestTree
getTrailStatusResponseTest = resp
    "GetTrailStatusResponse"
    "fixture/GetTrailStatusResponse"
    (Proxy :: Proxy GetTrailStatus)

startLoggingResponseTest :: StartLoggingResponse -> TestTree
startLoggingResponseTest = resp
    "StartLoggingResponse"
    "fixture/StartLoggingResponse"
    (Proxy :: Proxy StartLogging)
