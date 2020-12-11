{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudTrail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CloudTrail where

import Data.Proxy
import Network.AWS.CloudTrail
import Test.AWS.CloudTrail.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeTrails $
--             mkDescribeTrails
--
--         , requestListPublicKeys $
--             mkListPublicKeys
--
--         , requestRemoveTags $
--             mkRemoveTags
--
--         , requestLookupEvents $
--             mkLookupEvents
--
--         , requestStopLogging $
--             mkStopLogging
--
--         , requestDeleteTrail $
--             mkDeleteTrail
--
--         , requestUpdateTrail $
--             mkUpdateTrail
--
--         , requestCreateTrail $
--             mkCreateTrail
--
--         , requestPutInsightSelectors $
--             mkPutInsightSelectors
--
--         , requestGetEventSelectors $
--             mkGetEventSelectors
--
--         , requestGetTrail $
--             mkGetTrail
--
--         , requestGetTrailStatus $
--             mkGetTrailStatus
--
--         , requestAddTags $
--             mkAddTags
--
--         , requestListTags $
--             mkListTags
--
--         , requestPutEventSelectors $
--             mkPutEventSelectors
--
--         , requestStartLogging $
--             mkStartLogging
--
--         , requestListTrails $
--             mkListTrails
--
--         , requestGetInsightSelectors $
--             mkGetInsightSelectors
--
--           ]

--     , testGroup "response"
--         [ responseDescribeTrails $
--             mkDescribeTrailsResponse
--
--         , responseListPublicKeys $
--             mkListPublicKeysResponse
--
--         , responseRemoveTags $
--             mkRemoveTagsResponse
--
--         , responseLookupEvents $
--             mkLookupEventsResponse
--
--         , responseStopLogging $
--             mkStopLoggingResponse
--
--         , responseDeleteTrail $
--             mkDeleteTrailResponse
--
--         , responseUpdateTrail $
--             mkUpdateTrailResponse
--
--         , responseCreateTrail $
--             mkCreateTrailResponse
--
--         , responsePutInsightSelectors $
--             mkPutInsightSelectorsResponse
--
--         , responseGetEventSelectors $
--             mkGetEventSelectorsResponse
--
--         , responseGetTrail $
--             mkGetTrailResponse
--
--         , responseGetTrailStatus $
--             mkGetTrailStatusResponse
--
--         , responseAddTags $
--             mkAddTagsResponse
--
--         , responseListTags $
--             mkListTagsResponse
--
--         , responsePutEventSelectors $
--             mkPutEventSelectorsResponse
--
--         , responseStartLogging $
--             mkStartLoggingResponse
--
--         , responseListTrails $
--             mkListTrailsResponse
--
--         , responseGetInsightSelectors $
--             mkGetInsightSelectorsResponse
--
--           ]
--     ]

-- Requests

requestDescribeTrails :: DescribeTrails -> TestTree
requestDescribeTrails =
  req
    "DescribeTrails"
    "fixture/DescribeTrails.yaml"

requestListPublicKeys :: ListPublicKeys -> TestTree
requestListPublicKeys =
  req
    "ListPublicKeys"
    "fixture/ListPublicKeys.yaml"

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags =
  req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestLookupEvents :: LookupEvents -> TestTree
requestLookupEvents =
  req
    "LookupEvents"
    "fixture/LookupEvents.yaml"

requestStopLogging :: StopLogging -> TestTree
requestStopLogging =
  req
    "StopLogging"
    "fixture/StopLogging.yaml"

requestDeleteTrail :: DeleteTrail -> TestTree
requestDeleteTrail =
  req
    "DeleteTrail"
    "fixture/DeleteTrail.yaml"

requestUpdateTrail :: UpdateTrail -> TestTree
requestUpdateTrail =
  req
    "UpdateTrail"
    "fixture/UpdateTrail.yaml"

requestCreateTrail :: CreateTrail -> TestTree
requestCreateTrail =
  req
    "CreateTrail"
    "fixture/CreateTrail.yaml"

requestPutInsightSelectors :: PutInsightSelectors -> TestTree
requestPutInsightSelectors =
  req
    "PutInsightSelectors"
    "fixture/PutInsightSelectors.yaml"

requestGetEventSelectors :: GetEventSelectors -> TestTree
requestGetEventSelectors =
  req
    "GetEventSelectors"
    "fixture/GetEventSelectors.yaml"

requestGetTrail :: GetTrail -> TestTree
requestGetTrail =
  req
    "GetTrail"
    "fixture/GetTrail.yaml"

requestGetTrailStatus :: GetTrailStatus -> TestTree
requestGetTrailStatus =
  req
    "GetTrailStatus"
    "fixture/GetTrailStatus.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestPutEventSelectors :: PutEventSelectors -> TestTree
requestPutEventSelectors =
  req
    "PutEventSelectors"
    "fixture/PutEventSelectors.yaml"

requestStartLogging :: StartLogging -> TestTree
requestStartLogging =
  req
    "StartLogging"
    "fixture/StartLogging.yaml"

requestListTrails :: ListTrails -> TestTree
requestListTrails =
  req
    "ListTrails"
    "fixture/ListTrails.yaml"

requestGetInsightSelectors :: GetInsightSelectors -> TestTree
requestGetInsightSelectors =
  req
    "GetInsightSelectors"
    "fixture/GetInsightSelectors.yaml"

-- Responses

responseDescribeTrails :: DescribeTrailsResponse -> TestTree
responseDescribeTrails =
  res
    "DescribeTrailsResponse"
    "fixture/DescribeTrailsResponse.proto"
    cloudTrailService
    (Proxy :: Proxy DescribeTrails)

responseListPublicKeys :: ListPublicKeysResponse -> TestTree
responseListPublicKeys =
  res
    "ListPublicKeysResponse"
    "fixture/ListPublicKeysResponse.proto"
    cloudTrailService
    (Proxy :: Proxy ListPublicKeys)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    cloudTrailService
    (Proxy :: Proxy RemoveTags)

responseLookupEvents :: LookupEventsResponse -> TestTree
responseLookupEvents =
  res
    "LookupEventsResponse"
    "fixture/LookupEventsResponse.proto"
    cloudTrailService
    (Proxy :: Proxy LookupEvents)

responseStopLogging :: StopLoggingResponse -> TestTree
responseStopLogging =
  res
    "StopLoggingResponse"
    "fixture/StopLoggingResponse.proto"
    cloudTrailService
    (Proxy :: Proxy StopLogging)

responseDeleteTrail :: DeleteTrailResponse -> TestTree
responseDeleteTrail =
  res
    "DeleteTrailResponse"
    "fixture/DeleteTrailResponse.proto"
    cloudTrailService
    (Proxy :: Proxy DeleteTrail)

responseUpdateTrail :: UpdateTrailResponse -> TestTree
responseUpdateTrail =
  res
    "UpdateTrailResponse"
    "fixture/UpdateTrailResponse.proto"
    cloudTrailService
    (Proxy :: Proxy UpdateTrail)

responseCreateTrail :: CreateTrailResponse -> TestTree
responseCreateTrail =
  res
    "CreateTrailResponse"
    "fixture/CreateTrailResponse.proto"
    cloudTrailService
    (Proxy :: Proxy CreateTrail)

responsePutInsightSelectors :: PutInsightSelectorsResponse -> TestTree
responsePutInsightSelectors =
  res
    "PutInsightSelectorsResponse"
    "fixture/PutInsightSelectorsResponse.proto"
    cloudTrailService
    (Proxy :: Proxy PutInsightSelectors)

responseGetEventSelectors :: GetEventSelectorsResponse -> TestTree
responseGetEventSelectors =
  res
    "GetEventSelectorsResponse"
    "fixture/GetEventSelectorsResponse.proto"
    cloudTrailService
    (Proxy :: Proxy GetEventSelectors)

responseGetTrail :: GetTrailResponse -> TestTree
responseGetTrail =
  res
    "GetTrailResponse"
    "fixture/GetTrailResponse.proto"
    cloudTrailService
    (Proxy :: Proxy GetTrail)

responseGetTrailStatus :: GetTrailStatusResponse -> TestTree
responseGetTrailStatus =
  res
    "GetTrailStatusResponse"
    "fixture/GetTrailStatusResponse.proto"
    cloudTrailService
    (Proxy :: Proxy GetTrailStatus)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    cloudTrailService
    (Proxy :: Proxy AddTags)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    cloudTrailService
    (Proxy :: Proxy ListTags)

responsePutEventSelectors :: PutEventSelectorsResponse -> TestTree
responsePutEventSelectors =
  res
    "PutEventSelectorsResponse"
    "fixture/PutEventSelectorsResponse.proto"
    cloudTrailService
    (Proxy :: Proxy PutEventSelectors)

responseStartLogging :: StartLoggingResponse -> TestTree
responseStartLogging =
  res
    "StartLoggingResponse"
    "fixture/StartLoggingResponse.proto"
    cloudTrailService
    (Proxy :: Proxy StartLogging)

responseListTrails :: ListTrailsResponse -> TestTree
responseListTrails =
  res
    "ListTrailsResponse"
    "fixture/ListTrailsResponse.proto"
    cloudTrailService
    (Proxy :: Proxy ListTrails)

responseGetInsightSelectors :: GetInsightSelectorsResponse -> TestTree
responseGetInsightSelectors =
  res
    "GetInsightSelectorsResponse"
    "fixture/GetInsightSelectorsResponse.proto"
    cloudTrailService
    (Proxy :: Proxy GetInsightSelectors)
