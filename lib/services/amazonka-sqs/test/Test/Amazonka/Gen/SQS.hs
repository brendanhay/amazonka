{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SQS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SQS where

import Amazonka.SQS
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SQS.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddPermission $
--             newAddPermission
--
--         , requestChangeMessageVisibility $
--             newChangeMessageVisibility
--
--         , requestChangeMessageVisibilityBatch $
--             newChangeMessageVisibilityBatch
--
--         , requestCreateQueue $
--             newCreateQueue
--
--         , requestDeleteMessage $
--             newDeleteMessage
--
--         , requestDeleteMessageBatch $
--             newDeleteMessageBatch
--
--         , requestDeleteQueue $
--             newDeleteQueue
--
--         , requestGetQueueAttributes $
--             newGetQueueAttributes
--
--         , requestGetQueueUrl $
--             newGetQueueUrl
--
--         , requestListDeadLetterSourceQueues $
--             newListDeadLetterSourceQueues
--
--         , requestListQueueTags $
--             newListQueueTags
--
--         , requestListQueues $
--             newListQueues
--
--         , requestPurgeQueue $
--             newPurgeQueue
--
--         , requestReceiveMessage $
--             newReceiveMessage
--
--         , requestRemovePermission $
--             newRemovePermission
--
--         , requestSendMessage $
--             newSendMessage
--
--         , requestSendMessageBatch $
--             newSendMessageBatch
--
--         , requestSetQueueAttributes $
--             newSetQueueAttributes
--
--         , requestTagQueue $
--             newTagQueue
--
--         , requestUntagQueue $
--             newUntagQueue
--
--           ]

--     , testGroup "response"
--         [ responseAddPermission $
--             newAddPermissionResponse
--
--         , responseChangeMessageVisibility $
--             newChangeMessageVisibilityResponse
--
--         , responseChangeMessageVisibilityBatch $
--             newChangeMessageVisibilityBatchResponse
--
--         , responseCreateQueue $
--             newCreateQueueResponse
--
--         , responseDeleteMessage $
--             newDeleteMessageResponse
--
--         , responseDeleteMessageBatch $
--             newDeleteMessageBatchResponse
--
--         , responseDeleteQueue $
--             newDeleteQueueResponse
--
--         , responseGetQueueAttributes $
--             newGetQueueAttributesResponse
--
--         , responseGetQueueUrl $
--             newGetQueueUrlResponse
--
--         , responseListDeadLetterSourceQueues $
--             newListDeadLetterSourceQueuesResponse
--
--         , responseListQueueTags $
--             newListQueueTagsResponse
--
--         , responseListQueues $
--             newListQueuesResponse
--
--         , responsePurgeQueue $
--             newPurgeQueueResponse
--
--         , responseReceiveMessage $
--             newReceiveMessageResponse
--
--         , responseRemovePermission $
--             newRemovePermissionResponse
--
--         , responseSendMessage $
--             newSendMessageResponse
--
--         , responseSendMessageBatch $
--             newSendMessageBatchResponse
--
--         , responseSetQueueAttributes $
--             newSetQueueAttributesResponse
--
--         , responseTagQueue $
--             newTagQueueResponse
--
--         , responseUntagQueue $
--             newUntagQueueResponse
--
--           ]
--     ]

-- Requests

requestAddPermission :: AddPermission -> TestTree
requestAddPermission =
  req
    "AddPermission"
    "fixture/AddPermission.yaml"

requestChangeMessageVisibility :: ChangeMessageVisibility -> TestTree
requestChangeMessageVisibility =
  req
    "ChangeMessageVisibility"
    "fixture/ChangeMessageVisibility.yaml"

requestChangeMessageVisibilityBatch :: ChangeMessageVisibilityBatch -> TestTree
requestChangeMessageVisibilityBatch =
  req
    "ChangeMessageVisibilityBatch"
    "fixture/ChangeMessageVisibilityBatch.yaml"

requestCreateQueue :: CreateQueue -> TestTree
requestCreateQueue =
  req
    "CreateQueue"
    "fixture/CreateQueue.yaml"

requestDeleteMessage :: DeleteMessage -> TestTree
requestDeleteMessage =
  req
    "DeleteMessage"
    "fixture/DeleteMessage.yaml"

requestDeleteMessageBatch :: DeleteMessageBatch -> TestTree
requestDeleteMessageBatch =
  req
    "DeleteMessageBatch"
    "fixture/DeleteMessageBatch.yaml"

requestDeleteQueue :: DeleteQueue -> TestTree
requestDeleteQueue =
  req
    "DeleteQueue"
    "fixture/DeleteQueue.yaml"

requestGetQueueAttributes :: GetQueueAttributes -> TestTree
requestGetQueueAttributes =
  req
    "GetQueueAttributes"
    "fixture/GetQueueAttributes.yaml"

requestGetQueueUrl :: GetQueueUrl -> TestTree
requestGetQueueUrl =
  req
    "GetQueueUrl"
    "fixture/GetQueueUrl.yaml"

requestListDeadLetterSourceQueues :: ListDeadLetterSourceQueues -> TestTree
requestListDeadLetterSourceQueues =
  req
    "ListDeadLetterSourceQueues"
    "fixture/ListDeadLetterSourceQueues.yaml"

requestListQueueTags :: ListQueueTags -> TestTree
requestListQueueTags =
  req
    "ListQueueTags"
    "fixture/ListQueueTags.yaml"

requestListQueues :: ListQueues -> TestTree
requestListQueues =
  req
    "ListQueues"
    "fixture/ListQueues.yaml"

requestPurgeQueue :: PurgeQueue -> TestTree
requestPurgeQueue =
  req
    "PurgeQueue"
    "fixture/PurgeQueue.yaml"

requestReceiveMessage :: ReceiveMessage -> TestTree
requestReceiveMessage =
  req
    "ReceiveMessage"
    "fixture/ReceiveMessage.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission =
  req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

requestSendMessage :: SendMessage -> TestTree
requestSendMessage =
  req
    "SendMessage"
    "fixture/SendMessage.yaml"

requestSendMessageBatch :: SendMessageBatch -> TestTree
requestSendMessageBatch =
  req
    "SendMessageBatch"
    "fixture/SendMessageBatch.yaml"

requestSetQueueAttributes :: SetQueueAttributes -> TestTree
requestSetQueueAttributes =
  req
    "SetQueueAttributes"
    "fixture/SetQueueAttributes.yaml"

requestTagQueue :: TagQueue -> TestTree
requestTagQueue =
  req
    "TagQueue"
    "fixture/TagQueue.yaml"

requestUntagQueue :: UntagQueue -> TestTree
requestUntagQueue =
  req
    "UntagQueue"
    "fixture/UntagQueue.yaml"

-- Responses

responseAddPermission :: AddPermissionResponse -> TestTree
responseAddPermission =
  res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddPermission)

responseChangeMessageVisibility :: ChangeMessageVisibilityResponse -> TestTree
responseChangeMessageVisibility =
  res
    "ChangeMessageVisibilityResponse"
    "fixture/ChangeMessageVisibilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ChangeMessageVisibility)

responseChangeMessageVisibilityBatch :: ChangeMessageVisibilityBatchResponse -> TestTree
responseChangeMessageVisibilityBatch =
  res
    "ChangeMessageVisibilityBatchResponse"
    "fixture/ChangeMessageVisibilityBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ChangeMessageVisibilityBatch)

responseCreateQueue :: CreateQueueResponse -> TestTree
responseCreateQueue =
  res
    "CreateQueueResponse"
    "fixture/CreateQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateQueue)

responseDeleteMessage :: DeleteMessageResponse -> TestTree
responseDeleteMessage =
  res
    "DeleteMessageResponse"
    "fixture/DeleteMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMessage)

responseDeleteMessageBatch :: DeleteMessageBatchResponse -> TestTree
responseDeleteMessageBatch =
  res
    "DeleteMessageBatchResponse"
    "fixture/DeleteMessageBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMessageBatch)

responseDeleteQueue :: DeleteQueueResponse -> TestTree
responseDeleteQueue =
  res
    "DeleteQueueResponse"
    "fixture/DeleteQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQueue)

responseGetQueueAttributes :: GetQueueAttributesResponse -> TestTree
responseGetQueueAttributes =
  res
    "GetQueueAttributesResponse"
    "fixture/GetQueueAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQueueAttributes)

responseGetQueueUrl :: GetQueueUrlResponse -> TestTree
responseGetQueueUrl =
  res
    "GetQueueUrlResponse"
    "fixture/GetQueueUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQueueUrl)

responseListDeadLetterSourceQueues :: ListDeadLetterSourceQueuesResponse -> TestTree
responseListDeadLetterSourceQueues =
  res
    "ListDeadLetterSourceQueuesResponse"
    "fixture/ListDeadLetterSourceQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeadLetterSourceQueues)

responseListQueueTags :: ListQueueTagsResponse -> TestTree
responseListQueueTags =
  res
    "ListQueueTagsResponse"
    "fixture/ListQueueTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQueueTags)

responseListQueues :: ListQueuesResponse -> TestTree
responseListQueues =
  res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQueues)

responsePurgeQueue :: PurgeQueueResponse -> TestTree
responsePurgeQueue =
  res
    "PurgeQueueResponse"
    "fixture/PurgeQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurgeQueue)

responseReceiveMessage :: ReceiveMessageResponse -> TestTree
responseReceiveMessage =
  res
    "ReceiveMessageResponse"
    "fixture/ReceiveMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReceiveMessage)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemovePermission)

responseSendMessage :: SendMessageResponse -> TestTree
responseSendMessage =
  res
    "SendMessageResponse"
    "fixture/SendMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendMessage)

responseSendMessageBatch :: SendMessageBatchResponse -> TestTree
responseSendMessageBatch =
  res
    "SendMessageBatchResponse"
    "fixture/SendMessageBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendMessageBatch)

responseSetQueueAttributes :: SetQueueAttributesResponse -> TestTree
responseSetQueueAttributes =
  res
    "SetQueueAttributesResponse"
    "fixture/SetQueueAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetQueueAttributes)

responseTagQueue :: TagQueueResponse -> TestTree
responseTagQueue =
  res
    "TagQueueResponse"
    "fixture/TagQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagQueue)

responseUntagQueue :: UntagQueueResponse -> TestTree
responseUntagQueue =
  res
    "UntagQueueResponse"
    "fixture/UntagQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagQueue)
