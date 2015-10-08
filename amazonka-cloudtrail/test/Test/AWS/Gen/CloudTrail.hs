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
--         , testListPublicKeys $
--             listPublicKeys
--
--         , testRemoveTags $
--             removeTags
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
--         , testAddTags $
--             addTags
--
--         , testListTags $
--             listTags
--
--         , testStartLogging $
--             startLogging
--
--           ]

--     , testGroup "response"
--         [ testDescribeTrailsResponse $
--             describeTrailsResponse
--
--         , testListPublicKeysResponse $
--             listPublicKeysResponse
--
--         , testRemoveTagsResponse $
--             removeTagsResponse
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
--         , testAddTagsResponse $
--             addTagsResponse
--
--         , testListTagsResponse $
--             listTagsResponse
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
    "fixture/DescribeTrails.yaml"

testListPublicKeys :: ListPublicKeys -> TestTree
testListPublicKeys = req
    "ListPublicKeys"
    "fixture/ListPublicKeys.yaml"

testRemoveTags :: RemoveTags -> TestTree
testRemoveTags = req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

testLookupEvents :: LookupEvents -> TestTree
testLookupEvents = req
    "LookupEvents"
    "fixture/LookupEvents.yaml"

testStopLogging :: StopLogging -> TestTree
testStopLogging = req
    "StopLogging"
    "fixture/StopLogging.yaml"

testDeleteTrail :: DeleteTrail -> TestTree
testDeleteTrail = req
    "DeleteTrail"
    "fixture/DeleteTrail.yaml"

testUpdateTrail :: UpdateTrail -> TestTree
testUpdateTrail = req
    "UpdateTrail"
    "fixture/UpdateTrail.yaml"

testCreateTrail :: CreateTrail -> TestTree
testCreateTrail = req
    "CreateTrail"
    "fixture/CreateTrail.yaml"

testGetTrailStatus :: GetTrailStatus -> TestTree
testGetTrailStatus = req
    "GetTrailStatus"
    "fixture/GetTrailStatus.yaml"

testAddTags :: AddTags -> TestTree
testAddTags = req
    "AddTags"
    "fixture/AddTags.yaml"

testListTags :: ListTags -> TestTree
testListTags = req
    "ListTags"
    "fixture/ListTags.yaml"

testStartLogging :: StartLogging -> TestTree
testStartLogging = req
    "StartLogging"
    "fixture/StartLogging.yaml"

-- Responses

testDescribeTrailsResponse :: DescribeTrailsResponse -> TestTree
testDescribeTrailsResponse = res
    "DescribeTrailsResponse"
    "fixture/DescribeTrailsResponse.proto"
    cloudTrail
    (Proxy :: Proxy DescribeTrails)

testListPublicKeysResponse :: ListPublicKeysResponse -> TestTree
testListPublicKeysResponse = res
    "ListPublicKeysResponse"
    "fixture/ListPublicKeysResponse.proto"
    cloudTrail
    (Proxy :: Proxy ListPublicKeys)

testRemoveTagsResponse :: RemoveTagsResponse -> TestTree
testRemoveTagsResponse = res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    cloudTrail
    (Proxy :: Proxy RemoveTags)

testLookupEventsResponse :: LookupEventsResponse -> TestTree
testLookupEventsResponse = res
    "LookupEventsResponse"
    "fixture/LookupEventsResponse.proto"
    cloudTrail
    (Proxy :: Proxy LookupEvents)

testStopLoggingResponse :: StopLoggingResponse -> TestTree
testStopLoggingResponse = res
    "StopLoggingResponse"
    "fixture/StopLoggingResponse.proto"
    cloudTrail
    (Proxy :: Proxy StopLogging)

testDeleteTrailResponse :: DeleteTrailResponse -> TestTree
testDeleteTrailResponse = res
    "DeleteTrailResponse"
    "fixture/DeleteTrailResponse.proto"
    cloudTrail
    (Proxy :: Proxy DeleteTrail)

testUpdateTrailResponse :: UpdateTrailResponse -> TestTree
testUpdateTrailResponse = res
    "UpdateTrailResponse"
    "fixture/UpdateTrailResponse.proto"
    cloudTrail
    (Proxy :: Proxy UpdateTrail)

testCreateTrailResponse :: CreateTrailResponse -> TestTree
testCreateTrailResponse = res
    "CreateTrailResponse"
    "fixture/CreateTrailResponse.proto"
    cloudTrail
    (Proxy :: Proxy CreateTrail)

testGetTrailStatusResponse :: GetTrailStatusResponse -> TestTree
testGetTrailStatusResponse = res
    "GetTrailStatusResponse"
    "fixture/GetTrailStatusResponse.proto"
    cloudTrail
    (Proxy :: Proxy GetTrailStatus)

testAddTagsResponse :: AddTagsResponse -> TestTree
testAddTagsResponse = res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    cloudTrail
    (Proxy :: Proxy AddTags)

testListTagsResponse :: ListTagsResponse -> TestTree
testListTagsResponse = res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    cloudTrail
    (Proxy :: Proxy ListTags)

testStartLoggingResponse :: StartLoggingResponse -> TestTree
testStartLoggingResponse = res
    "StartLoggingResponse"
    "fixture/StartLoggingResponse.proto"
    cloudTrail
    (Proxy :: Proxy StartLogging)
