{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudTrail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CloudTrail where

import qualified Data.Proxy as Proxy
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
--             newDescribeTrails
--
--         , requestListPublicKeys $
--             newListPublicKeys
--
--         , requestRemoveTags $
--             newRemoveTags
--
--         , requestLookupEvents $
--             newLookupEvents
--
--         , requestStopLogging $
--             newStopLogging
--
--         , requestDeleteTrail $
--             newDeleteTrail
--
--         , requestUpdateTrail $
--             newUpdateTrail
--
--         , requestCreateTrail $
--             newCreateTrail
--
--         , requestPutInsightSelectors $
--             newPutInsightSelectors
--
--         , requestGetEventSelectors $
--             newGetEventSelectors
--
--         , requestGetTrail $
--             newGetTrail
--
--         , requestGetTrailStatus $
--             newGetTrailStatus
--
--         , requestAddTags $
--             newAddTags
--
--         , requestListTags $
--             newListTags
--
--         , requestPutEventSelectors $
--             newPutEventSelectors
--
--         , requestStartLogging $
--             newStartLogging
--
--         , requestListTrails $
--             newListTrails
--
--         , requestGetInsightSelectors $
--             newGetInsightSelectors
--
--           ]

--     , testGroup "response"
--         [ responseDescribeTrails $
--             newDescribeTrailsResponse
--
--         , responseListPublicKeys $
--             newListPublicKeysResponse
--
--         , responseRemoveTags $
--             newRemoveTagsResponse
--
--         , responseLookupEvents $
--             newLookupEventsResponse
--
--         , responseStopLogging $
--             newStopLoggingResponse
--
--         , responseDeleteTrail $
--             newDeleteTrailResponse
--
--         , responseUpdateTrail $
--             newUpdateTrailResponse
--
--         , responseCreateTrail $
--             newCreateTrailResponse
--
--         , responsePutInsightSelectors $
--             newPutInsightSelectorsResponse
--
--         , responseGetEventSelectors $
--             newGetEventSelectorsResponse
--
--         , responseGetTrail $
--             newGetTrailResponse
--
--         , responseGetTrailStatus $
--             newGetTrailStatusResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responsePutEventSelectors $
--             newPutEventSelectorsResponse
--
--         , responseStartLogging $
--             newStartLoggingResponse
--
--         , responseListTrails $
--             newListTrailsResponse
--
--         , responseGetInsightSelectors $
--             newGetInsightSelectorsResponse
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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrails)

responseListPublicKeys :: ListPublicKeysResponse -> TestTree
responseListPublicKeys =
  res
    "ListPublicKeysResponse"
    "fixture/ListPublicKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPublicKeys)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTags)

responseLookupEvents :: LookupEventsResponse -> TestTree
responseLookupEvents =
  res
    "LookupEventsResponse"
    "fixture/LookupEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy LookupEvents)

responseStopLogging :: StopLoggingResponse -> TestTree
responseStopLogging =
  res
    "StopLoggingResponse"
    "fixture/StopLoggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopLogging)

responseDeleteTrail :: DeleteTrailResponse -> TestTree
responseDeleteTrail =
  res
    "DeleteTrailResponse"
    "fixture/DeleteTrailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrail)

responseUpdateTrail :: UpdateTrailResponse -> TestTree
responseUpdateTrail =
  res
    "UpdateTrailResponse"
    "fixture/UpdateTrailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTrail)

responseCreateTrail :: CreateTrailResponse -> TestTree
responseCreateTrail =
  res
    "CreateTrailResponse"
    "fixture/CreateTrailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrail)

responsePutInsightSelectors :: PutInsightSelectorsResponse -> TestTree
responsePutInsightSelectors =
  res
    "PutInsightSelectorsResponse"
    "fixture/PutInsightSelectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutInsightSelectors)

responseGetEventSelectors :: GetEventSelectorsResponse -> TestTree
responseGetEventSelectors =
  res
    "GetEventSelectorsResponse"
    "fixture/GetEventSelectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEventSelectors)

responseGetTrail :: GetTrailResponse -> TestTree
responseGetTrail =
  res
    "GetTrailResponse"
    "fixture/GetTrailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTrail)

responseGetTrailStatus :: GetTrailStatusResponse -> TestTree
responseGetTrailStatus =
  res
    "GetTrailStatusResponse"
    "fixture/GetTrailStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTrailStatus)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTags)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responsePutEventSelectors :: PutEventSelectorsResponse -> TestTree
responsePutEventSelectors =
  res
    "PutEventSelectorsResponse"
    "fixture/PutEventSelectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEventSelectors)

responseStartLogging :: StartLoggingResponse -> TestTree
responseStartLogging =
  res
    "StartLoggingResponse"
    "fixture/StartLoggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartLogging)

responseListTrails :: ListTrailsResponse -> TestTree
responseListTrails =
  res
    "ListTrailsResponse"
    "fixture/ListTrailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrails)

responseGetInsightSelectors :: GetInsightSelectorsResponse -> TestTree
responseGetInsightSelectors =
  res
    "GetInsightSelectorsResponse"
    "fixture/GetInsightSelectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInsightSelectors)
