{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IVSRealtime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.IVSRealtime where

import Amazonka.IVSRealtime
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.IVSRealtime.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateParticipantToken $
--             newCreateParticipantToken
--
--         , requestCreateStage $
--             newCreateStage
--
--         , requestDeleteStage $
--             newDeleteStage
--
--         , requestDisconnectParticipant $
--             newDisconnectParticipant
--
--         , requestGetParticipant $
--             newGetParticipant
--
--         , requestGetStage $
--             newGetStage
--
--         , requestGetStageSession $
--             newGetStageSession
--
--         , requestListParticipantEvents $
--             newListParticipantEvents
--
--         , requestListParticipants $
--             newListParticipants
--
--         , requestListStageSessions $
--             newListStageSessions
--
--         , requestListStages $
--             newListStages
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateStage $
--             newUpdateStage
--
--           ]

--     , testGroup "response"
--         [ responseCreateParticipantToken $
--             newCreateParticipantTokenResponse
--
--         , responseCreateStage $
--             newCreateStageResponse
--
--         , responseDeleteStage $
--             newDeleteStageResponse
--
--         , responseDisconnectParticipant $
--             newDisconnectParticipantResponse
--
--         , responseGetParticipant $
--             newGetParticipantResponse
--
--         , responseGetStage $
--             newGetStageResponse
--
--         , responseGetStageSession $
--             newGetStageSessionResponse
--
--         , responseListParticipantEvents $
--             newListParticipantEventsResponse
--
--         , responseListParticipants $
--             newListParticipantsResponse
--
--         , responseListStageSessions $
--             newListStageSessionsResponse
--
--         , responseListStages $
--             newListStagesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateStage $
--             newUpdateStageResponse
--
--           ]
--     ]

-- Requests

requestCreateParticipantToken :: CreateParticipantToken -> TestTree
requestCreateParticipantToken =
  req
    "CreateParticipantToken"
    "fixture/CreateParticipantToken.yaml"

requestCreateStage :: CreateStage -> TestTree
requestCreateStage =
  req
    "CreateStage"
    "fixture/CreateStage.yaml"

requestDeleteStage :: DeleteStage -> TestTree
requestDeleteStage =
  req
    "DeleteStage"
    "fixture/DeleteStage.yaml"

requestDisconnectParticipant :: DisconnectParticipant -> TestTree
requestDisconnectParticipant =
  req
    "DisconnectParticipant"
    "fixture/DisconnectParticipant.yaml"

requestGetParticipant :: GetParticipant -> TestTree
requestGetParticipant =
  req
    "GetParticipant"
    "fixture/GetParticipant.yaml"

requestGetStage :: GetStage -> TestTree
requestGetStage =
  req
    "GetStage"
    "fixture/GetStage.yaml"

requestGetStageSession :: GetStageSession -> TestTree
requestGetStageSession =
  req
    "GetStageSession"
    "fixture/GetStageSession.yaml"

requestListParticipantEvents :: ListParticipantEvents -> TestTree
requestListParticipantEvents =
  req
    "ListParticipantEvents"
    "fixture/ListParticipantEvents.yaml"

requestListParticipants :: ListParticipants -> TestTree
requestListParticipants =
  req
    "ListParticipants"
    "fixture/ListParticipants.yaml"

requestListStageSessions :: ListStageSessions -> TestTree
requestListStageSessions =
  req
    "ListStageSessions"
    "fixture/ListStageSessions.yaml"

requestListStages :: ListStages -> TestTree
requestListStages =
  req
    "ListStages"
    "fixture/ListStages.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateStage :: UpdateStage -> TestTree
requestUpdateStage =
  req
    "UpdateStage"
    "fixture/UpdateStage.yaml"

-- Responses

responseCreateParticipantToken :: CreateParticipantTokenResponse -> TestTree
responseCreateParticipantToken =
  res
    "CreateParticipantTokenResponse"
    "fixture/CreateParticipantTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateParticipantToken)

responseCreateStage :: CreateStageResponse -> TestTree
responseCreateStage =
  res
    "CreateStageResponse"
    "fixture/CreateStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStage)

responseDeleteStage :: DeleteStageResponse -> TestTree
responseDeleteStage =
  res
    "DeleteStageResponse"
    "fixture/DeleteStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStage)

responseDisconnectParticipant :: DisconnectParticipantResponse -> TestTree
responseDisconnectParticipant =
  res
    "DisconnectParticipantResponse"
    "fixture/DisconnectParticipantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisconnectParticipant)

responseGetParticipant :: GetParticipantResponse -> TestTree
responseGetParticipant =
  res
    "GetParticipantResponse"
    "fixture/GetParticipantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetParticipant)

responseGetStage :: GetStageResponse -> TestTree
responseGetStage =
  res
    "GetStageResponse"
    "fixture/GetStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStage)

responseGetStageSession :: GetStageSessionResponse -> TestTree
responseGetStageSession =
  res
    "GetStageSessionResponse"
    "fixture/GetStageSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStageSession)

responseListParticipantEvents :: ListParticipantEventsResponse -> TestTree
responseListParticipantEvents =
  res
    "ListParticipantEventsResponse"
    "fixture/ListParticipantEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListParticipantEvents)

responseListParticipants :: ListParticipantsResponse -> TestTree
responseListParticipants =
  res
    "ListParticipantsResponse"
    "fixture/ListParticipantsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListParticipants)

responseListStageSessions :: ListStageSessionsResponse -> TestTree
responseListStageSessions =
  res
    "ListStageSessionsResponse"
    "fixture/ListStageSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStageSessions)

responseListStages :: ListStagesResponse -> TestTree
responseListStages =
  res
    "ListStagesResponse"
    "fixture/ListStagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStages)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateStage :: UpdateStageResponse -> TestTree
responseUpdateStage =
  res
    "UpdateStageResponse"
    "fixture/UpdateStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStage)
