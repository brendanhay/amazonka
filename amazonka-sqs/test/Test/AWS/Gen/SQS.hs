{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SQS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.SQS where

import Data.Proxy
import Network.AWS.SQS
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SQS.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestChangeMessageVisibilityBatch $
--             newChangeMessageVisibilityBatch
--
--         , requestPurgeQueue $
--             newPurgeQueue
--
--         , requestChangeMessageVisibility $
--             newChangeMessageVisibility
--
--         , requestTagQueue $
--             newTagQueue
--
--         , requestListQueues $
--             newListQueues
--
--         , requestReceiveMessage $
--             newReceiveMessage
--
--         , requestGetQueueAttributes $
--             newGetQueueAttributes
--
--         , requestDeleteMessage $
--             newDeleteMessage
--
--         , requestAddPermission $
--             newAddPermission
--
--         , requestListQueueTags $
--             newListQueueTags
--
--         , requestSendMessage $
--             newSendMessage
--
--         , requestListDeadLetterSourceQueues $
--             newListDeadLetterSourceQueues
--
--         , requestGetQueueUrl $
--             newGetQueueUrl
--
--         , requestSetQueueAttributes $
--             newSetQueueAttributes
--
--         , requestDeleteMessageBatch $
--             newDeleteMessageBatch
--
--         , requestSendMessageBatch $
--             newSendMessageBatch
--
--         , requestUntagQueue $
--             newUntagQueue
--
--         , requestDeleteQueue $
--             newDeleteQueue
--
--         , requestCreateQueue $
--             newCreateQueue
--
--         , requestRemovePermission $
--             newRemovePermission
--
--           ]

--     , testGroup "response"
--         [ responseChangeMessageVisibilityBatch $
--             newChangeMessageVisibilityBatchResponse
--
--         , responsePurgeQueue $
--             newPurgeQueueResponse
--
--         , responseChangeMessageVisibility $
--             newChangeMessageVisibilityResponse
--
--         , responseTagQueue $
--             newTagQueueResponse
--
--         , responseListQueues $
--             newListQueuesResponse
--
--         , responseReceiveMessage $
--             newReceiveMessageResponse
--
--         , responseGetQueueAttributes $
--             newGetQueueAttributesResponse
--
--         , responseDeleteMessage $
--             newDeleteMessageResponse
--
--         , responseAddPermission $
--             newAddPermissionResponse
--
--         , responseListQueueTags $
--             newListQueueTagsResponse
--
--         , responseSendMessage $
--             newSendMessageResponse
--
--         , responseListDeadLetterSourceQueues $
--             newListDeadLetterSourceQueuesResponse
--
--         , responseGetQueueUrl $
--             newGetQueueUrlResponse
--
--         , responseSetQueueAttributes $
--             newSetQueueAttributesResponse
--
--         , responseDeleteMessageBatch $
--             newDeleteMessageBatchResponse
--
--         , responseSendMessageBatch $
--             newSendMessageBatchResponse
--
--         , responseUntagQueue $
--             newUntagQueueResponse
--
--         , responseDeleteQueue $
--             newDeleteQueueResponse
--
--         , responseCreateQueue $
--             newCreateQueueResponse
--
--         , responseRemovePermission $
--             newRemovePermissionResponse
--
--           ]
--     ]

-- Requests

requestChangeMessageVisibilityBatch :: ChangeMessageVisibilityBatch -> TestTree
requestChangeMessageVisibilityBatch =
  req
    "ChangeMessageVisibilityBatch"
    "fixture/ChangeMessageVisibilityBatch.yaml"

requestPurgeQueue :: PurgeQueue -> TestTree
requestPurgeQueue =
  req
    "PurgeQueue"
    "fixture/PurgeQueue.yaml"

requestChangeMessageVisibility :: ChangeMessageVisibility -> TestTree
requestChangeMessageVisibility =
  req
    "ChangeMessageVisibility"
    "fixture/ChangeMessageVisibility.yaml"

requestTagQueue :: TagQueue -> TestTree
requestTagQueue =
  req
    "TagQueue"
    "fixture/TagQueue.yaml"

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

requestGetQueueAttributes :: GetQueueAttributes -> TestTree
requestGetQueueAttributes =
  req
    "GetQueueAttributes"
    "fixture/GetQueueAttributes.yaml"

requestDeleteMessage :: DeleteMessage -> TestTree
requestDeleteMessage =
  req
    "DeleteMessage"
    "fixture/DeleteMessage.yaml"

requestAddPermission :: AddPermission -> TestTree
requestAddPermission =
  req
    "AddPermission"
    "fixture/AddPermission.yaml"

requestListQueueTags :: ListQueueTags -> TestTree
requestListQueueTags =
  req
    "ListQueueTags"
    "fixture/ListQueueTags.yaml"

requestSendMessage :: SendMessage -> TestTree
requestSendMessage =
  req
    "SendMessage"
    "fixture/SendMessage.yaml"

requestListDeadLetterSourceQueues :: ListDeadLetterSourceQueues -> TestTree
requestListDeadLetterSourceQueues =
  req
    "ListDeadLetterSourceQueues"
    "fixture/ListDeadLetterSourceQueues.yaml"

requestGetQueueUrl :: GetQueueUrl -> TestTree
requestGetQueueUrl =
  req
    "GetQueueUrl"
    "fixture/GetQueueUrl.yaml"

requestSetQueueAttributes :: SetQueueAttributes -> TestTree
requestSetQueueAttributes =
  req
    "SetQueueAttributes"
    "fixture/SetQueueAttributes.yaml"

requestDeleteMessageBatch :: DeleteMessageBatch -> TestTree
requestDeleteMessageBatch =
  req
    "DeleteMessageBatch"
    "fixture/DeleteMessageBatch.yaml"

requestSendMessageBatch :: SendMessageBatch -> TestTree
requestSendMessageBatch =
  req
    "SendMessageBatch"
    "fixture/SendMessageBatch.yaml"

requestUntagQueue :: UntagQueue -> TestTree
requestUntagQueue =
  req
    "UntagQueue"
    "fixture/UntagQueue.yaml"

requestDeleteQueue :: DeleteQueue -> TestTree
requestDeleteQueue =
  req
    "DeleteQueue"
    "fixture/DeleteQueue.yaml"

requestCreateQueue :: CreateQueue -> TestTree
requestCreateQueue =
  req
    "CreateQueue"
    "fixture/CreateQueue.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission =
  req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

-- Responses

responseChangeMessageVisibilityBatch :: ChangeMessageVisibilityBatchResponse -> TestTree
responseChangeMessageVisibilityBatch =
  res
    "ChangeMessageVisibilityBatchResponse"
    "fixture/ChangeMessageVisibilityBatchResponse.proto"
    defaultService
    (Proxy :: Proxy ChangeMessageVisibilityBatch)

responsePurgeQueue :: PurgeQueueResponse -> TestTree
responsePurgeQueue =
  res
    "PurgeQueueResponse"
    "fixture/PurgeQueueResponse.proto"
    defaultService
    (Proxy :: Proxy PurgeQueue)

responseChangeMessageVisibility :: ChangeMessageVisibilityResponse -> TestTree
responseChangeMessageVisibility =
  res
    "ChangeMessageVisibilityResponse"
    "fixture/ChangeMessageVisibilityResponse.proto"
    defaultService
    (Proxy :: Proxy ChangeMessageVisibility)

responseTagQueue :: TagQueueResponse -> TestTree
responseTagQueue =
  res
    "TagQueueResponse"
    "fixture/TagQueueResponse.proto"
    defaultService
    (Proxy :: Proxy TagQueue)

responseListQueues :: ListQueuesResponse -> TestTree
responseListQueues =
  res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse.proto"
    defaultService
    (Proxy :: Proxy ListQueues)

responseReceiveMessage :: ReceiveMessageResponse -> TestTree
responseReceiveMessage =
  res
    "ReceiveMessageResponse"
    "fixture/ReceiveMessageResponse.proto"
    defaultService
    (Proxy :: Proxy ReceiveMessage)

responseGetQueueAttributes :: GetQueueAttributesResponse -> TestTree
responseGetQueueAttributes =
  res
    "GetQueueAttributesResponse"
    "fixture/GetQueueAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetQueueAttributes)

responseDeleteMessage :: DeleteMessageResponse -> TestTree
responseDeleteMessage =
  res
    "DeleteMessageResponse"
    "fixture/DeleteMessageResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMessage)

responseAddPermission :: AddPermissionResponse -> TestTree
responseAddPermission =
  res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    defaultService
    (Proxy :: Proxy AddPermission)

responseListQueueTags :: ListQueueTagsResponse -> TestTree
responseListQueueTags =
  res
    "ListQueueTagsResponse"
    "fixture/ListQueueTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListQueueTags)

responseSendMessage :: SendMessageResponse -> TestTree
responseSendMessage =
  res
    "SendMessageResponse"
    "fixture/SendMessageResponse.proto"
    defaultService
    (Proxy :: Proxy SendMessage)

responseListDeadLetterSourceQueues :: ListDeadLetterSourceQueuesResponse -> TestTree
responseListDeadLetterSourceQueues =
  res
    "ListDeadLetterSourceQueuesResponse"
    "fixture/ListDeadLetterSourceQueuesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeadLetterSourceQueues)

responseGetQueueUrl :: GetQueueUrlResponse -> TestTree
responseGetQueueUrl =
  res
    "GetQueueUrlResponse"
    "fixture/GetQueueUrlResponse.proto"
    defaultService
    (Proxy :: Proxy GetQueueUrl)

responseSetQueueAttributes :: SetQueueAttributesResponse -> TestTree
responseSetQueueAttributes =
  res
    "SetQueueAttributesResponse"
    "fixture/SetQueueAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy SetQueueAttributes)

responseDeleteMessageBatch :: DeleteMessageBatchResponse -> TestTree
responseDeleteMessageBatch =
  res
    "DeleteMessageBatchResponse"
    "fixture/DeleteMessageBatchResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMessageBatch)

responseSendMessageBatch :: SendMessageBatchResponse -> TestTree
responseSendMessageBatch =
  res
    "SendMessageBatchResponse"
    "fixture/SendMessageBatchResponse.proto"
    defaultService
    (Proxy :: Proxy SendMessageBatch)

responseUntagQueue :: UntagQueueResponse -> TestTree
responseUntagQueue =
  res
    "UntagQueueResponse"
    "fixture/UntagQueueResponse.proto"
    defaultService
    (Proxy :: Proxy UntagQueue)

responseDeleteQueue :: DeleteQueueResponse -> TestTree
responseDeleteQueue =
  res
    "DeleteQueueResponse"
    "fixture/DeleteQueueResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteQueue)

responseCreateQueue :: CreateQueueResponse -> TestTree
responseCreateQueue =
  res
    "CreateQueueResponse"
    "fixture/CreateQueueResponse.proto"
    defaultService
    (Proxy :: Proxy CreateQueue)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    defaultService
    (Proxy :: Proxy RemovePermission)
