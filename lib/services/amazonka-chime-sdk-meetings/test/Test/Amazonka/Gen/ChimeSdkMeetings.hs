{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ChimeSdkMeetings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ChimeSdkMeetings where

import Amazonka.ChimeSdkMeetings
import qualified Data.Proxy as Proxy
import Test.Amazonka.ChimeSdkMeetings.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchCreateAttendee $
--             newBatchCreateAttendee
--
--         , requestBatchUpdateAttendeeCapabilitiesExcept $
--             newBatchUpdateAttendeeCapabilitiesExcept
--
--         , requestCreateAttendee $
--             newCreateAttendee
--
--         , requestCreateMeeting $
--             newCreateMeeting
--
--         , requestCreateMeetingWithAttendees $
--             newCreateMeetingWithAttendees
--
--         , requestDeleteAttendee $
--             newDeleteAttendee
--
--         , requestDeleteMeeting $
--             newDeleteMeeting
--
--         , requestGetAttendee $
--             newGetAttendee
--
--         , requestGetMeeting $
--             newGetMeeting
--
--         , requestListAttendees $
--             newListAttendees
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartMeetingTranscription $
--             newStartMeetingTranscription
--
--         , requestStopMeetingTranscription $
--             newStopMeetingTranscription
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAttendeeCapabilities $
--             newUpdateAttendeeCapabilities
--
--           ]

--     , testGroup "response"
--         [ responseBatchCreateAttendee $
--             newBatchCreateAttendeeResponse
--
--         , responseBatchUpdateAttendeeCapabilitiesExcept $
--             newBatchUpdateAttendeeCapabilitiesExceptResponse
--
--         , responseCreateAttendee $
--             newCreateAttendeeResponse
--
--         , responseCreateMeeting $
--             newCreateMeetingResponse
--
--         , responseCreateMeetingWithAttendees $
--             newCreateMeetingWithAttendeesResponse
--
--         , responseDeleteAttendee $
--             newDeleteAttendeeResponse
--
--         , responseDeleteMeeting $
--             newDeleteMeetingResponse
--
--         , responseGetAttendee $
--             newGetAttendeeResponse
--
--         , responseGetMeeting $
--             newGetMeetingResponse
--
--         , responseListAttendees $
--             newListAttendeesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartMeetingTranscription $
--             newStartMeetingTranscriptionResponse
--
--         , responseStopMeetingTranscription $
--             newStopMeetingTranscriptionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAttendeeCapabilities $
--             newUpdateAttendeeCapabilitiesResponse
--
--           ]
--     ]

-- Requests

requestBatchCreateAttendee :: BatchCreateAttendee -> TestTree
requestBatchCreateAttendee =
  req
    "BatchCreateAttendee"
    "fixture/BatchCreateAttendee.yaml"

requestBatchUpdateAttendeeCapabilitiesExcept :: BatchUpdateAttendeeCapabilitiesExcept -> TestTree
requestBatchUpdateAttendeeCapabilitiesExcept =
  req
    "BatchUpdateAttendeeCapabilitiesExcept"
    "fixture/BatchUpdateAttendeeCapabilitiesExcept.yaml"

requestCreateAttendee :: CreateAttendee -> TestTree
requestCreateAttendee =
  req
    "CreateAttendee"
    "fixture/CreateAttendee.yaml"

requestCreateMeeting :: CreateMeeting -> TestTree
requestCreateMeeting =
  req
    "CreateMeeting"
    "fixture/CreateMeeting.yaml"

requestCreateMeetingWithAttendees :: CreateMeetingWithAttendees -> TestTree
requestCreateMeetingWithAttendees =
  req
    "CreateMeetingWithAttendees"
    "fixture/CreateMeetingWithAttendees.yaml"

requestDeleteAttendee :: DeleteAttendee -> TestTree
requestDeleteAttendee =
  req
    "DeleteAttendee"
    "fixture/DeleteAttendee.yaml"

requestDeleteMeeting :: DeleteMeeting -> TestTree
requestDeleteMeeting =
  req
    "DeleteMeeting"
    "fixture/DeleteMeeting.yaml"

requestGetAttendee :: GetAttendee -> TestTree
requestGetAttendee =
  req
    "GetAttendee"
    "fixture/GetAttendee.yaml"

requestGetMeeting :: GetMeeting -> TestTree
requestGetMeeting =
  req
    "GetMeeting"
    "fixture/GetMeeting.yaml"

requestListAttendees :: ListAttendees -> TestTree
requestListAttendees =
  req
    "ListAttendees"
    "fixture/ListAttendees.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartMeetingTranscription :: StartMeetingTranscription -> TestTree
requestStartMeetingTranscription =
  req
    "StartMeetingTranscription"
    "fixture/StartMeetingTranscription.yaml"

requestStopMeetingTranscription :: StopMeetingTranscription -> TestTree
requestStopMeetingTranscription =
  req
    "StopMeetingTranscription"
    "fixture/StopMeetingTranscription.yaml"

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

requestUpdateAttendeeCapabilities :: UpdateAttendeeCapabilities -> TestTree
requestUpdateAttendeeCapabilities =
  req
    "UpdateAttendeeCapabilities"
    "fixture/UpdateAttendeeCapabilities.yaml"

-- Responses

responseBatchCreateAttendee :: BatchCreateAttendeeResponse -> TestTree
responseBatchCreateAttendee =
  res
    "BatchCreateAttendeeResponse"
    "fixture/BatchCreateAttendeeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCreateAttendee)

responseBatchUpdateAttendeeCapabilitiesExcept :: BatchUpdateAttendeeCapabilitiesExceptResponse -> TestTree
responseBatchUpdateAttendeeCapabilitiesExcept =
  res
    "BatchUpdateAttendeeCapabilitiesExceptResponse"
    "fixture/BatchUpdateAttendeeCapabilitiesExceptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdateAttendeeCapabilitiesExcept)

responseCreateAttendee :: CreateAttendeeResponse -> TestTree
responseCreateAttendee =
  res
    "CreateAttendeeResponse"
    "fixture/CreateAttendeeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAttendee)

responseCreateMeeting :: CreateMeetingResponse -> TestTree
responseCreateMeeting =
  res
    "CreateMeetingResponse"
    "fixture/CreateMeetingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMeeting)

responseCreateMeetingWithAttendees :: CreateMeetingWithAttendeesResponse -> TestTree
responseCreateMeetingWithAttendees =
  res
    "CreateMeetingWithAttendeesResponse"
    "fixture/CreateMeetingWithAttendeesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMeetingWithAttendees)

responseDeleteAttendee :: DeleteAttendeeResponse -> TestTree
responseDeleteAttendee =
  res
    "DeleteAttendeeResponse"
    "fixture/DeleteAttendeeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAttendee)

responseDeleteMeeting :: DeleteMeetingResponse -> TestTree
responseDeleteMeeting =
  res
    "DeleteMeetingResponse"
    "fixture/DeleteMeetingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMeeting)

responseGetAttendee :: GetAttendeeResponse -> TestTree
responseGetAttendee =
  res
    "GetAttendeeResponse"
    "fixture/GetAttendeeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAttendee)

responseGetMeeting :: GetMeetingResponse -> TestTree
responseGetMeeting =
  res
    "GetMeetingResponse"
    "fixture/GetMeetingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMeeting)

responseListAttendees :: ListAttendeesResponse -> TestTree
responseListAttendees =
  res
    "ListAttendeesResponse"
    "fixture/ListAttendeesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttendees)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartMeetingTranscription :: StartMeetingTranscriptionResponse -> TestTree
responseStartMeetingTranscription =
  res
    "StartMeetingTranscriptionResponse"
    "fixture/StartMeetingTranscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMeetingTranscription)

responseStopMeetingTranscription :: StopMeetingTranscriptionResponse -> TestTree
responseStopMeetingTranscription =
  res
    "StopMeetingTranscriptionResponse"
    "fixture/StopMeetingTranscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopMeetingTranscription)

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

responseUpdateAttendeeCapabilities :: UpdateAttendeeCapabilitiesResponse -> TestTree
responseUpdateAttendeeCapabilities =
  res
    "UpdateAttendeeCapabilitiesResponse"
    "fixture/UpdateAttendeeCapabilitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAttendeeCapabilities)
