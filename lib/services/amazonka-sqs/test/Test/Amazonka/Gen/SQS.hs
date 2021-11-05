{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SQS
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestGetQueueUrl $
--             newGetQueueUrl
--
--         , requestPurgeQueue $
--             newPurgeQueue
--
--         , requestChangeMessageVisibilityBatch $
--             newChangeMessageVisibilityBatch
--
--         , requestSendMessage $
--             newSendMessage
--
--         , requestRemovePermission $
--             newRemovePermission
--
--         , requestGetQueueAttributes $
--             newGetQueueAttributes
--
--         , requestListQueues $
--             newListQueues
--
--         , requestReceiveMessage $
--             newReceiveMessage
--
--         , requestDeleteQueue $
--             newDeleteQueue
--
--         , requestTagQueue $
--             newTagQueue
--
--         , requestDeleteMessageBatch $
--             newDeleteMessageBatch
--
--         , requestSetQueueAttributes $
--             newSetQueueAttributes
--
--         , requestListDeadLetterSourceQueues $
--             newListDeadLetterSourceQueues
--
--         , requestAddPermission $
--             newAddPermission
--
--         , requestDeleteMessage $
--             newDeleteMessage
--
--         , requestListQueueTags $
--             newListQueueTags
--
--         , requestCreateQueue $
--             newCreateQueue
--
--         , requestUntagQueue $
--             newUntagQueue
--
--         , requestSendMessageBatch $
--             newSendMessageBatch
--
--         , requestChangeMessageVisibility $
--             newChangeMessageVisibility
--
--           ]

--     , testGroup "response"
--         [ responseGetQueueUrl $
--             newGetQueueUrlResponse
--
--         , responsePurgeQueue $
--             newPurgeQueueResponse
--
--         , responseChangeMessageVisibilityBatch $
--             newChangeMessageVisibilityBatchResponse
--
--         , responseSendMessage $
--             newSendMessageResponse
--
--         , responseRemovePermission $
--             newRemovePermissionResponse
--
--         , responseGetQueueAttributes $
--             newGetQueueAttributesResponse
--
--         , responseListQueues $
--             newListQueuesResponse
--
--         , responseReceiveMessage $
--             newReceiveMessageResponse
--
--         , responseDeleteQueue $
--             newDeleteQueueResponse
--
--         , responseTagQueue $
--             newTagQueueResponse
--
--         , responseDeleteMessageBatch $
--             newDeleteMessageBatchResponse
--
--         , responseSetQueueAttributes $
--             newSetQueueAttributesResponse
--
--         , responseListDeadLetterSourceQueues $
--             newListDeadLetterSourceQueuesResponse
--
--         , responseAddPermission $
--             newAddPermissionResponse
--
--         , responseDeleteMessage $
--             newDeleteMessageResponse
--
--         , responseListQueueTags $
--             newListQueueTagsResponse
--
--         , responseCreateQueue $
--             newCreateQueueResponse
--
--         , responseUntagQueue $
--             newUntagQueueResponse
--
--         , responseSendMessageBatch $
--             newSendMessageBatchResponse
--
--         , responseChangeMessageVisibility $
--             newChangeMessageVisibilityResponse
--
--           ]
--     ]

-- Requests

requestGetQueueUrl :: GetQueueUrl -> TestTree
requestGetQueueUrl =
  req
    "GetQueueUrl"
    "fixture/GetQueueUrl.yaml"

requestPurgeQueue :: PurgeQueue -> TestTree
requestPurgeQueue =
  req
    "PurgeQueue"
    "fixture/PurgeQueue.yaml"

requestChangeMessageVisibilityBatch :: ChangeMessageVisibilityBatch -> TestTree
requestChangeMessageVisibilityBatch =
  req
    "ChangeMessageVisibilityBatch"
    "fixture/ChangeMessageVisibilityBatch.yaml"

requestSendMessage :: SendMessage -> TestTree
requestSendMessage =
  req
    "SendMessage"
    "fixture/SendMessage.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission =
  req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

requestGetQueueAttributes :: GetQueueAttributes -> TestTree
requestGetQueueAttributes =
  req
    "GetQueueAttributes"
    "fixture/GetQueueAttributes.yaml"

requestListQueues :: ListQueues -> TestTree
requestListQueues =
  req
    "ListQueues"
    "fixture/ListQueues.yaml"

requestReceiveMessage :: ReceiveMessage -> TestTree
requestReceiveMessage =
  req
    "ReceiveMessage"
    "fixture/ReceiveMessage.yaml"

requestDeleteQueue :: DeleteQueue -> TestTree
requestDeleteQueue =
  req
    "DeleteQueue"
    "fixture/DeleteQueue.yaml"

requestTagQueue :: TagQueue -> TestTree
requestTagQueue =
  req
    "TagQueue"
    "fixture/TagQueue.yaml"

requestDeleteMessageBatch :: DeleteMessageBatch -> TestTree
requestDeleteMessageBatch =
  req
    "DeleteMessageBatch"
    "fixture/DeleteMessageBatch.yaml"

requestSetQueueAttributes :: SetQueueAttributes -> TestTree
requestSetQueueAttributes =
  req
    "SetQueueAttributes"
    "fixture/SetQueueAttributes.yaml"

requestListDeadLetterSourceQueues :: ListDeadLetterSourceQueues -> TestTree
requestListDeadLetterSourceQueues =
  req
    "ListDeadLetterSourceQueues"
    "fixture/ListDeadLetterSourceQueues.yaml"

requestAddPermission :: AddPermission -> TestTree
requestAddPermission =
  req
    "AddPermission"
    "fixture/AddPermission.yaml"

requestDeleteMessage :: DeleteMessage -> TestTree
requestDeleteMessage =
  req
    "DeleteMessage"
    "fixture/DeleteMessage.yaml"

requestListQueueTags :: ListQueueTags -> TestTree
requestListQueueTags =
  req
    "ListQueueTags"
    "fixture/ListQueueTags.yaml"

requestCreateQueue :: CreateQueue -> TestTree
requestCreateQueue =
  req
    "CreateQueue"
    "fixture/CreateQueue.yaml"

requestUntagQueue :: UntagQueue -> TestTree
requestUntagQueue =
  req
    "UntagQueue"
    "fixture/UntagQueue.yaml"

requestSendMessageBatch :: SendMessageBatch -> TestTree
requestSendMessageBatch =
  req
    "SendMessageBatch"
    "fixture/SendMessageBatch.yaml"

requestChangeMessageVisibility :: ChangeMessageVisibility -> TestTree
requestChangeMessageVisibility =
  req
    "ChangeMessageVisibility"
    "fixture/ChangeMessageVisibility.yaml"

-- Responses

responseGetQueueUrl :: GetQueueUrlResponse -> TestTree
responseGetQueueUrl =
  res
    "GetQueueUrlResponse"
    "fixture/GetQueueUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQueueUrl)

responsePurgeQueue :: PurgeQueueResponse -> TestTree
responsePurgeQueue =
  res
    "PurgeQueueResponse"
    "fixture/PurgeQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurgeQueue)

responseChangeMessageVisibilityBatch :: ChangeMessageVisibilityBatchResponse -> TestTree
responseChangeMessageVisibilityBatch =
  res
    "ChangeMessageVisibilityBatchResponse"
    "fixture/ChangeMessageVisibilityBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ChangeMessageVisibilityBatch)

responseSendMessage :: SendMessageResponse -> TestTree
responseSendMessage =
  res
    "SendMessageResponse"
    "fixture/SendMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendMessage)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemovePermission)

responseGetQueueAttributes :: GetQueueAttributesResponse -> TestTree
responseGetQueueAttributes =
  res
    "GetQueueAttributesResponse"
    "fixture/GetQueueAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQueueAttributes)

responseListQueues :: ListQueuesResponse -> TestTree
responseListQueues =
  res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQueues)

responseReceiveMessage :: ReceiveMessageResponse -> TestTree
responseReceiveMessage =
  res
    "ReceiveMessageResponse"
    "fixture/ReceiveMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReceiveMessage)

responseDeleteQueue :: DeleteQueueResponse -> TestTree
responseDeleteQueue =
  res
    "DeleteQueueResponse"
    "fixture/DeleteQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQueue)

responseTagQueue :: TagQueueResponse -> TestTree
responseTagQueue =
  res
    "TagQueueResponse"
    "fixture/TagQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagQueue)

responseDeleteMessageBatch :: DeleteMessageBatchResponse -> TestTree
responseDeleteMessageBatch =
  res
    "DeleteMessageBatchResponse"
    "fixture/DeleteMessageBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMessageBatch)

responseSetQueueAttributes :: SetQueueAttributesResponse -> TestTree
responseSetQueueAttributes =
  res
    "SetQueueAttributesResponse"
    "fixture/SetQueueAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetQueueAttributes)

responseListDeadLetterSourceQueues :: ListDeadLetterSourceQueuesResponse -> TestTree
responseListDeadLetterSourceQueues =
  res
    "ListDeadLetterSourceQueuesResponse"
    "fixture/ListDeadLetterSourceQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeadLetterSourceQueues)

responseAddPermission :: AddPermissionResponse -> TestTree
responseAddPermission =
  res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddPermission)

responseDeleteMessage :: DeleteMessageResponse -> TestTree
responseDeleteMessage =
  res
    "DeleteMessageResponse"
    "fixture/DeleteMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMessage)

responseListQueueTags :: ListQueueTagsResponse -> TestTree
responseListQueueTags =
  res
    "ListQueueTagsResponse"
    "fixture/ListQueueTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQueueTags)

responseCreateQueue :: CreateQueueResponse -> TestTree
responseCreateQueue =
  res
    "CreateQueueResponse"
    "fixture/CreateQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateQueue)

responseUntagQueue :: UntagQueueResponse -> TestTree
responseUntagQueue =
  res
    "UntagQueueResponse"
    "fixture/UntagQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagQueue)

responseSendMessageBatch :: SendMessageBatchResponse -> TestTree
responseSendMessageBatch =
  res
    "SendMessageBatchResponse"
    "fixture/SendMessageBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendMessageBatch)

responseChangeMessageVisibility :: ChangeMessageVisibilityResponse -> TestTree
responseChangeMessageVisibility =
  res
    "ChangeMessageVisibilityResponse"
    "fixture/ChangeMessageVisibilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ChangeMessageVisibility)
