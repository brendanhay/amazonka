{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SQS
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestGetQueueUrl $
--             mkGetQueueUrl
--
--         , requestPurgeQueue $
--             mkPurgeQueue
--
--         , requestChangeMessageVisibilityBatch $
--             mkChangeMessageVisibilityBatch
--
--         , requestSendMessage $
--             mkSendMessage
--
--         , requestRemovePermission $
--             mkRemovePermission
--
--         , requestGetQueueAttributes $
--             mkGetQueueAttributes
--
--         , requestListQueues $
--             mkListQueues
--
--         , requestReceiveMessage $
--             mkReceiveMessage
--
--         , requestDeleteQueue $
--             mkDeleteQueue
--
--         , requestTagQueue $
--             mkTagQueue
--
--         , requestDeleteMessageBatch $
--             mkDeleteMessageBatch
--
--         , requestSetQueueAttributes $
--             mkSetQueueAttributes
--
--         , requestListDeadLetterSourceQueues $
--             mkListDeadLetterSourceQueues
--
--         , requestAddPermission $
--             mkAddPermission
--
--         , requestDeleteMessage $
--             mkDeleteMessage
--
--         , requestListQueueTags $
--             mkListQueueTags
--
--         , requestCreateQueue $
--             mkCreateQueue
--
--         , requestUntagQueue $
--             mkUntagQueue
--
--         , requestSendMessageBatch $
--             mkSendMessageBatch
--
--         , requestChangeMessageVisibility $
--             mkChangeMessageVisibility
--
--           ]

--     , testGroup "response"
--         [ responseGetQueueUrl $
--             mkGetQueueUrlResponse
--
--         , responsePurgeQueue $
--             mkPurgeQueueResponse
--
--         , responseChangeMessageVisibilityBatch $
--             mkChangeMessageVisibilityBatchResponse
--
--         , responseSendMessage $
--             mkSendMessageResponse
--
--         , responseRemovePermission $
--             mkRemovePermissionResponse
--
--         , responseGetQueueAttributes $
--             mkGetQueueAttributesResponse
--
--         , responseListQueues $
--             mkListQueuesResponse
--
--         , responseReceiveMessage $
--             mkReceiveMessageResponse
--
--         , responseDeleteQueue $
--             mkDeleteQueueResponse
--
--         , responseTagQueue $
--             mkTagQueueResponse
--
--         , responseDeleteMessageBatch $
--             mkDeleteMessageBatchResponse
--
--         , responseSetQueueAttributes $
--             mkSetQueueAttributesResponse
--
--         , responseListDeadLetterSourceQueues $
--             mkListDeadLetterSourceQueuesResponse
--
--         , responseAddPermission $
--             mkAddPermissionResponse
--
--         , responseDeleteMessage $
--             mkDeleteMessageResponse
--
--         , responseListQueueTags $
--             mkListQueueTagsResponse
--
--         , responseCreateQueue $
--             mkCreateQueueResponse
--
--         , responseUntagQueue $
--             mkUntagQueueResponse
--
--         , responseSendMessageBatch $
--             mkSendMessageBatchResponse
--
--         , responseChangeMessageVisibility $
--             mkChangeMessageVisibilityResponse
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
    mkServiceConfig
    (Proxy :: Proxy GetQueueUrl)

responsePurgeQueue :: PurgeQueueResponse -> TestTree
responsePurgeQueue =
  res
    "PurgeQueueResponse"
    "fixture/PurgeQueueResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PurgeQueue)

responseChangeMessageVisibilityBatch :: ChangeMessageVisibilityBatchResponse -> TestTree
responseChangeMessageVisibilityBatch =
  res
    "ChangeMessageVisibilityBatchResponse"
    "fixture/ChangeMessageVisibilityBatchResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ChangeMessageVisibilityBatch)

responseSendMessage :: SendMessageResponse -> TestTree
responseSendMessage =
  res
    "SendMessageResponse"
    "fixture/SendMessageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SendMessage)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemovePermission)

responseGetQueueAttributes :: GetQueueAttributesResponse -> TestTree
responseGetQueueAttributes =
  res
    "GetQueueAttributesResponse"
    "fixture/GetQueueAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetQueueAttributes)

responseListQueues :: ListQueuesResponse -> TestTree
responseListQueues =
  res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListQueues)

responseReceiveMessage :: ReceiveMessageResponse -> TestTree
responseReceiveMessage =
  res
    "ReceiveMessageResponse"
    "fixture/ReceiveMessageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ReceiveMessage)

responseDeleteQueue :: DeleteQueueResponse -> TestTree
responseDeleteQueue =
  res
    "DeleteQueueResponse"
    "fixture/DeleteQueueResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteQueue)

responseTagQueue :: TagQueueResponse -> TestTree
responseTagQueue =
  res
    "TagQueueResponse"
    "fixture/TagQueueResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagQueue)

responseDeleteMessageBatch :: DeleteMessageBatchResponse -> TestTree
responseDeleteMessageBatch =
  res
    "DeleteMessageBatchResponse"
    "fixture/DeleteMessageBatchResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteMessageBatch)

responseSetQueueAttributes :: SetQueueAttributesResponse -> TestTree
responseSetQueueAttributes =
  res
    "SetQueueAttributesResponse"
    "fixture/SetQueueAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetQueueAttributes)

responseListDeadLetterSourceQueues :: ListDeadLetterSourceQueuesResponse -> TestTree
responseListDeadLetterSourceQueues =
  res
    "ListDeadLetterSourceQueuesResponse"
    "fixture/ListDeadLetterSourceQueuesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDeadLetterSourceQueues)

responseAddPermission :: AddPermissionResponse -> TestTree
responseAddPermission =
  res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddPermission)

responseDeleteMessage :: DeleteMessageResponse -> TestTree
responseDeleteMessage =
  res
    "DeleteMessageResponse"
    "fixture/DeleteMessageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteMessage)

responseListQueueTags :: ListQueueTagsResponse -> TestTree
responseListQueueTags =
  res
    "ListQueueTagsResponse"
    "fixture/ListQueueTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListQueueTags)

responseCreateQueue :: CreateQueueResponse -> TestTree
responseCreateQueue =
  res
    "CreateQueueResponse"
    "fixture/CreateQueueResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateQueue)

responseUntagQueue :: UntagQueueResponse -> TestTree
responseUntagQueue =
  res
    "UntagQueueResponse"
    "fixture/UntagQueueResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagQueue)

responseSendMessageBatch :: SendMessageBatchResponse -> TestTree
responseSendMessageBatch =
  res
    "SendMessageBatchResponse"
    "fixture/SendMessageBatchResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SendMessageBatch)

responseChangeMessageVisibility :: ChangeMessageVisibilityResponse -> TestTree
responseChangeMessageVisibility =
  res
    "ChangeMessageVisibilityResponse"
    "fixture/ChangeMessageVisibilityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ChangeMessageVisibility)
