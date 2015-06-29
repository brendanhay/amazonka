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
--         [ createTrailTest $
--             createTrail
--
--         , deleteTrailTest $
--             deleteTrail
--
--         , describeTrailsTest $
--             describeTrails
--
--         , getTrailStatusTest $
--             getTrailStatus
--
--         , lookupEventsTest $
--             lookupEvents
--
--         , startLoggingTest $
--             startLogging
--
--         , stopLoggingTest $
--             stopLogging
--
--         , updateTrailTest $
--             updateTrail
--
--           ]

--     , testGroup "response"
--         [ createTrailResponseTest $
--             createTrailResponse
--
--         , deleteTrailResponseTest $
--             deleteTrailResponse
--
--         , describeTrailsResponseTest $
--             describeTrailsResponse
--
--         , getTrailStatusResponseTest $
--             getTrailStatusResponse
--
--         , lookupEventsResponseTest $
--             lookupEventsResponse
--
--         , startLoggingResponseTest $
--             startLoggingResponse
--
--         , stopLoggingResponseTest $
--             stopLoggingResponse
--
--         , updateTrailResponseTest $
--             updateTrailResponse
--
--           ]
--     ]

-- Requests

createTrailTest :: CreateTrail -> TestTree
createTrailTest = undefined

deleteTrailTest :: DeleteTrail -> TestTree
deleteTrailTest = undefined

describeTrailsTest :: DescribeTrails -> TestTree
describeTrailsTest = undefined

getTrailStatusTest :: GetTrailStatus -> TestTree
getTrailStatusTest = undefined

lookupEventsTest :: LookupEvents -> TestTree
lookupEventsTest = undefined

startLoggingTest :: StartLogging -> TestTree
startLoggingTest = undefined

stopLoggingTest :: StopLogging -> TestTree
stopLoggingTest = undefined

updateTrailTest :: UpdateTrail -> TestTree
updateTrailTest = undefined

-- Responses

createTrailResponseTest :: CreateTrailResponse -> TestTree
createTrailResponseTest = resp
    "createTrailResponse"
    "fixture/CreateTrailResponse"
    (Proxy :: Proxy CreateTrail)

deleteTrailResponseTest :: DeleteTrailResponse -> TestTree
deleteTrailResponseTest = resp
    "deleteTrailResponse"
    "fixture/DeleteTrailResponse"
    (Proxy :: Proxy DeleteTrail)

describeTrailsResponseTest :: DescribeTrailsResponse -> TestTree
describeTrailsResponseTest = resp
    "describeTrailsResponse"
    "fixture/DescribeTrailsResponse"
    (Proxy :: Proxy DescribeTrails)

getTrailStatusResponseTest :: GetTrailStatusResponse -> TestTree
getTrailStatusResponseTest = resp
    "getTrailStatusResponse"
    "fixture/GetTrailStatusResponse"
    (Proxy :: Proxy GetTrailStatus)

lookupEventsResponseTest :: LookupEventsResponse -> TestTree
lookupEventsResponseTest = resp
    "lookupEventsResponse"
    "fixture/LookupEventsResponse"
    (Proxy :: Proxy LookupEvents)

startLoggingResponseTest :: StartLoggingResponse -> TestTree
startLoggingResponseTest = resp
    "startLoggingResponse"
    "fixture/StartLoggingResponse"
    (Proxy :: Proxy StartLogging)

stopLoggingResponseTest :: StopLoggingResponse -> TestTree
stopLoggingResponseTest = resp
    "stopLoggingResponse"
    "fixture/StopLoggingResponse"
    (Proxy :: Proxy StopLogging)

updateTrailResponseTest :: UpdateTrailResponse -> TestTree
updateTrailResponseTest = resp
    "updateTrailResponse"
    "fixture/UpdateTrailResponse"
    (Proxy :: Proxy UpdateTrail)
