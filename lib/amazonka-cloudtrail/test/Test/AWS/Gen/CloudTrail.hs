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
--             describeTrails
--
--         , requestListPublicKeys $
--             listPublicKeys
--
--         , requestRemoveTags $
--             removeTags
--
--         , requestLookupEvents $
--             lookupEvents
--
--         , requestStopLogging $
--             stopLogging
--
--         , requestDeleteTrail $
--             deleteTrail
--
--         , requestUpdateTrail $
--             updateTrail
--
--         , requestCreateTrail $
--             createTrail
--
--         , requestPutInsightSelectors $
--             putInsightSelectors
--
--         , requestGetEventSelectors $
--             getEventSelectors
--
--         , requestGetTrail $
--             getTrail
--
--         , requestGetTrailStatus $
--             getTrailStatus
--
--         , requestAddTags $
--             addTags
--
--         , requestListTags $
--             listTags
--
--         , requestPutEventSelectors $
--             putEventSelectors
--
--         , requestStartLogging $
--             startLogging
--
--         , requestListTrails $
--             listTrails
--
--         , requestGetInsightSelectors $
--             getInsightSelectors
--
--           ]

--     , testGroup "response"
--         [ responseDescribeTrails $
--             describeTrailsResponse
--
--         , responseListPublicKeys $
--             listPublicKeysResponse
--
--         , responseRemoveTags $
--             removeTagsResponse
--
--         , responseLookupEvents $
--             lookupEventsResponse
--
--         , responseStopLogging $
--             stopLoggingResponse
--
--         , responseDeleteTrail $
--             deleteTrailResponse
--
--         , responseUpdateTrail $
--             updateTrailResponse
--
--         , responseCreateTrail $
--             createTrailResponse
--
--         , responsePutInsightSelectors $
--             putInsightSelectorsResponse
--
--         , responseGetEventSelectors $
--             getEventSelectorsResponse
--
--         , responseGetTrail $
--             getTrailResponse
--
--         , responseGetTrailStatus $
--             getTrailStatusResponse
--
--         , responseAddTags $
--             addTagsResponse
--
--         , responseListTags $
--             listTagsResponse
--
--         , responsePutEventSelectors $
--             putEventSelectorsResponse
--
--         , responseStartLogging $
--             startLoggingResponse
--
--         , responseListTrails $
--             listTrailsResponse
--
--         , responseGetInsightSelectors $
--             getInsightSelectorsResponse
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
    cloudTrail
    (Proxy :: Proxy DescribeTrails)

responseListPublicKeys :: ListPublicKeysResponse -> TestTree
responseListPublicKeys =
  res
    "ListPublicKeysResponse"
    "fixture/ListPublicKeysResponse.proto"
    cloudTrail
    (Proxy :: Proxy ListPublicKeys)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    cloudTrail
    (Proxy :: Proxy RemoveTags)

responseLookupEvents :: LookupEventsResponse -> TestTree
responseLookupEvents =
  res
    "LookupEventsResponse"
    "fixture/LookupEventsResponse.proto"
    cloudTrail
    (Proxy :: Proxy LookupEvents)

responseStopLogging :: StopLoggingResponse -> TestTree
responseStopLogging =
  res
    "StopLoggingResponse"
    "fixture/StopLoggingResponse.proto"
    cloudTrail
    (Proxy :: Proxy StopLogging)

responseDeleteTrail :: DeleteTrailResponse -> TestTree
responseDeleteTrail =
  res
    "DeleteTrailResponse"
    "fixture/DeleteTrailResponse.proto"
    cloudTrail
    (Proxy :: Proxy DeleteTrail)

responseUpdateTrail :: UpdateTrailResponse -> TestTree
responseUpdateTrail =
  res
    "UpdateTrailResponse"
    "fixture/UpdateTrailResponse.proto"
    cloudTrail
    (Proxy :: Proxy UpdateTrail)

responseCreateTrail :: CreateTrailResponse -> TestTree
responseCreateTrail =
  res
    "CreateTrailResponse"
    "fixture/CreateTrailResponse.proto"
    cloudTrail
    (Proxy :: Proxy CreateTrail)

responsePutInsightSelectors :: PutInsightSelectorsResponse -> TestTree
responsePutInsightSelectors =
  res
    "PutInsightSelectorsResponse"
    "fixture/PutInsightSelectorsResponse.proto"
    cloudTrail
    (Proxy :: Proxy PutInsightSelectors)

responseGetEventSelectors :: GetEventSelectorsResponse -> TestTree
responseGetEventSelectors =
  res
    "GetEventSelectorsResponse"
    "fixture/GetEventSelectorsResponse.proto"
    cloudTrail
    (Proxy :: Proxy GetEventSelectors)

responseGetTrail :: GetTrailResponse -> TestTree
responseGetTrail =
  res
    "GetTrailResponse"
    "fixture/GetTrailResponse.proto"
    cloudTrail
    (Proxy :: Proxy GetTrail)

responseGetTrailStatus :: GetTrailStatusResponse -> TestTree
responseGetTrailStatus =
  res
    "GetTrailStatusResponse"
    "fixture/GetTrailStatusResponse.proto"
    cloudTrail
    (Proxy :: Proxy GetTrailStatus)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    cloudTrail
    (Proxy :: Proxy AddTags)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    cloudTrail
    (Proxy :: Proxy ListTags)

responsePutEventSelectors :: PutEventSelectorsResponse -> TestTree
responsePutEventSelectors =
  res
    "PutEventSelectorsResponse"
    "fixture/PutEventSelectorsResponse.proto"
    cloudTrail
    (Proxy :: Proxy PutEventSelectors)

responseStartLogging :: StartLoggingResponse -> TestTree
responseStartLogging =
  res
    "StartLoggingResponse"
    "fixture/StartLoggingResponse.proto"
    cloudTrail
    (Proxy :: Proxy StartLogging)

responseListTrails :: ListTrailsResponse -> TestTree
responseListTrails =
  res
    "ListTrailsResponse"
    "fixture/ListTrailsResponse.proto"
    cloudTrail
    (Proxy :: Proxy ListTrails)

responseGetInsightSelectors :: GetInsightSelectorsResponse -> TestTree
responseGetInsightSelectors =
  res
    "GetInsightSelectorsResponse"
    "fixture/GetInsightSelectorsResponse.proto"
    cloudTrail
    (Proxy :: Proxy GetInsightSelectors)
