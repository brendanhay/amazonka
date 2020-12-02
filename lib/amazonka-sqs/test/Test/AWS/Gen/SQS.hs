{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SQS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestGetQueueURL $
--             getQueueURL
--
--         , requestPurgeQueue $
--             purgeQueue
--
--         , requestChangeMessageVisibilityBatch $
--             changeMessageVisibilityBatch
--
--         , requestSendMessage $
--             sendMessage
--
--         , requestRemovePermission $
--             removePermission
--
--         , requestGetQueueAttributes $
--             getQueueAttributes
--
--         , requestListQueues $
--             listQueues
--
--         , requestReceiveMessage $
--             receiveMessage
--
--         , requestDeleteQueue $
--             deleteQueue
--
--         , requestTagQueue $
--             tagQueue
--
--         , requestDeleteMessageBatch $
--             deleteMessageBatch
--
--         , requestSetQueueAttributes $
--             setQueueAttributes
--
--         , requestListDeadLetterSourceQueues $
--             listDeadLetterSourceQueues
--
--         , requestAddPermission $
--             addPermission
--
--         , requestDeleteMessage $
--             deleteMessage
--
--         , requestListQueueTags $
--             listQueueTags
--
--         , requestCreateQueue $
--             createQueue
--
--         , requestUntagQueue $
--             untagQueue
--
--         , requestSendMessageBatch $
--             sendMessageBatch
--
--         , requestChangeMessageVisibility $
--             changeMessageVisibility
--
--           ]

--     , testGroup "response"
--         [ responseGetQueueURL $
--             getQueueURLResponse
--
--         , responsePurgeQueue $
--             purgeQueueResponse
--
--         , responseChangeMessageVisibilityBatch $
--             changeMessageVisibilityBatchResponse
--
--         , responseSendMessage $
--             sendMessageResponse
--
--         , responseRemovePermission $
--             removePermissionResponse
--
--         , responseGetQueueAttributes $
--             getQueueAttributesResponse
--
--         , responseListQueues $
--             listQueuesResponse
--
--         , responseReceiveMessage $
--             receiveMessageResponse
--
--         , responseDeleteQueue $
--             deleteQueueResponse
--
--         , responseTagQueue $
--             tagQueueResponse
--
--         , responseDeleteMessageBatch $
--             deleteMessageBatchResponse
--
--         , responseSetQueueAttributes $
--             setQueueAttributesResponse
--
--         , responseListDeadLetterSourceQueues $
--             listDeadLetterSourceQueuesResponse
--
--         , responseAddPermission $
--             addPermissionResponse
--
--         , responseDeleteMessage $
--             deleteMessageResponse
--
--         , responseListQueueTags $
--             listQueueTagsResponse
--
--         , responseCreateQueue $
--             createQueueResponse
--
--         , responseUntagQueue $
--             untagQueueResponse
--
--         , responseSendMessageBatch $
--             sendMessageBatchResponse
--
--         , responseChangeMessageVisibility $
--             changeMessageVisibilityResponse
--
--           ]
--     ]

-- Requests

requestGetQueueURL :: GetQueueURL -> TestTree
requestGetQueueURL = req
    "GetQueueURL"
    "fixture/GetQueueURL.yaml"

requestPurgeQueue :: PurgeQueue -> TestTree
requestPurgeQueue = req
    "PurgeQueue"
    "fixture/PurgeQueue.yaml"

requestChangeMessageVisibilityBatch :: ChangeMessageVisibilityBatch -> TestTree
requestChangeMessageVisibilityBatch = req
    "ChangeMessageVisibilityBatch"
    "fixture/ChangeMessageVisibilityBatch.yaml"

requestSendMessage :: SendMessage -> TestTree
requestSendMessage = req
    "SendMessage"
    "fixture/SendMessage.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission = req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

requestGetQueueAttributes :: GetQueueAttributes -> TestTree
requestGetQueueAttributes = req
    "GetQueueAttributes"
    "fixture/GetQueueAttributes.yaml"

requestListQueues :: ListQueues -> TestTree
requestListQueues = req
    "ListQueues"
    "fixture/ListQueues.yaml"

requestReceiveMessage :: ReceiveMessage -> TestTree
requestReceiveMessage = req
    "ReceiveMessage"
    "fixture/ReceiveMessage.yaml"

requestDeleteQueue :: DeleteQueue -> TestTree
requestDeleteQueue = req
    "DeleteQueue"
    "fixture/DeleteQueue.yaml"

requestTagQueue :: TagQueue -> TestTree
requestTagQueue = req
    "TagQueue"
    "fixture/TagQueue.yaml"

requestDeleteMessageBatch :: DeleteMessageBatch -> TestTree
requestDeleteMessageBatch = req
    "DeleteMessageBatch"
    "fixture/DeleteMessageBatch.yaml"

requestSetQueueAttributes :: SetQueueAttributes -> TestTree
requestSetQueueAttributes = req
    "SetQueueAttributes"
    "fixture/SetQueueAttributes.yaml"

requestListDeadLetterSourceQueues :: ListDeadLetterSourceQueues -> TestTree
requestListDeadLetterSourceQueues = req
    "ListDeadLetterSourceQueues"
    "fixture/ListDeadLetterSourceQueues.yaml"

requestAddPermission :: AddPermission -> TestTree
requestAddPermission = req
    "AddPermission"
    "fixture/AddPermission.yaml"

requestDeleteMessage :: DeleteMessage -> TestTree
requestDeleteMessage = req
    "DeleteMessage"
    "fixture/DeleteMessage.yaml"

requestListQueueTags :: ListQueueTags -> TestTree
requestListQueueTags = req
    "ListQueueTags"
    "fixture/ListQueueTags.yaml"

requestCreateQueue :: CreateQueue -> TestTree
requestCreateQueue = req
    "CreateQueue"
    "fixture/CreateQueue.yaml"

requestUntagQueue :: UntagQueue -> TestTree
requestUntagQueue = req
    "UntagQueue"
    "fixture/UntagQueue.yaml"

requestSendMessageBatch :: SendMessageBatch -> TestTree
requestSendMessageBatch = req
    "SendMessageBatch"
    "fixture/SendMessageBatch.yaml"

requestChangeMessageVisibility :: ChangeMessageVisibility -> TestTree
requestChangeMessageVisibility = req
    "ChangeMessageVisibility"
    "fixture/ChangeMessageVisibility.yaml"

-- Responses

responseGetQueueURL :: GetQueueURLResponse -> TestTree
responseGetQueueURL = res
    "GetQueueURLResponse"
    "fixture/GetQueueURLResponse.proto"
    sqs
    (Proxy :: Proxy GetQueueURL)

responsePurgeQueue :: PurgeQueueResponse -> TestTree
responsePurgeQueue = res
    "PurgeQueueResponse"
    "fixture/PurgeQueueResponse.proto"
    sqs
    (Proxy :: Proxy PurgeQueue)

responseChangeMessageVisibilityBatch :: ChangeMessageVisibilityBatchResponse -> TestTree
responseChangeMessageVisibilityBatch = res
    "ChangeMessageVisibilityBatchResponse"
    "fixture/ChangeMessageVisibilityBatchResponse.proto"
    sqs
    (Proxy :: Proxy ChangeMessageVisibilityBatch)

responseSendMessage :: SendMessageResponse -> TestTree
responseSendMessage = res
    "SendMessageResponse"
    "fixture/SendMessageResponse.proto"
    sqs
    (Proxy :: Proxy SendMessage)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission = res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    sqs
    (Proxy :: Proxy RemovePermission)

responseGetQueueAttributes :: GetQueueAttributesResponse -> TestTree
responseGetQueueAttributes = res
    "GetQueueAttributesResponse"
    "fixture/GetQueueAttributesResponse.proto"
    sqs
    (Proxy :: Proxy GetQueueAttributes)

responseListQueues :: ListQueuesResponse -> TestTree
responseListQueues = res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse.proto"
    sqs
    (Proxy :: Proxy ListQueues)

responseReceiveMessage :: ReceiveMessageResponse -> TestTree
responseReceiveMessage = res
    "ReceiveMessageResponse"
    "fixture/ReceiveMessageResponse.proto"
    sqs
    (Proxy :: Proxy ReceiveMessage)

responseDeleteQueue :: DeleteQueueResponse -> TestTree
responseDeleteQueue = res
    "DeleteQueueResponse"
    "fixture/DeleteQueueResponse.proto"
    sqs
    (Proxy :: Proxy DeleteQueue)

responseTagQueue :: TagQueueResponse -> TestTree
responseTagQueue = res
    "TagQueueResponse"
    "fixture/TagQueueResponse.proto"
    sqs
    (Proxy :: Proxy TagQueue)

responseDeleteMessageBatch :: DeleteMessageBatchResponse -> TestTree
responseDeleteMessageBatch = res
    "DeleteMessageBatchResponse"
    "fixture/DeleteMessageBatchResponse.proto"
    sqs
    (Proxy :: Proxy DeleteMessageBatch)

responseSetQueueAttributes :: SetQueueAttributesResponse -> TestTree
responseSetQueueAttributes = res
    "SetQueueAttributesResponse"
    "fixture/SetQueueAttributesResponse.proto"
    sqs
    (Proxy :: Proxy SetQueueAttributes)

responseListDeadLetterSourceQueues :: ListDeadLetterSourceQueuesResponse -> TestTree
responseListDeadLetterSourceQueues = res
    "ListDeadLetterSourceQueuesResponse"
    "fixture/ListDeadLetterSourceQueuesResponse.proto"
    sqs
    (Proxy :: Proxy ListDeadLetterSourceQueues)

responseAddPermission :: AddPermissionResponse -> TestTree
responseAddPermission = res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    sqs
    (Proxy :: Proxy AddPermission)

responseDeleteMessage :: DeleteMessageResponse -> TestTree
responseDeleteMessage = res
    "DeleteMessageResponse"
    "fixture/DeleteMessageResponse.proto"
    sqs
    (Proxy :: Proxy DeleteMessage)

responseListQueueTags :: ListQueueTagsResponse -> TestTree
responseListQueueTags = res
    "ListQueueTagsResponse"
    "fixture/ListQueueTagsResponse.proto"
    sqs
    (Proxy :: Proxy ListQueueTags)

responseCreateQueue :: CreateQueueResponse -> TestTree
responseCreateQueue = res
    "CreateQueueResponse"
    "fixture/CreateQueueResponse.proto"
    sqs
    (Proxy :: Proxy CreateQueue)

responseUntagQueue :: UntagQueueResponse -> TestTree
responseUntagQueue = res
    "UntagQueueResponse"
    "fixture/UntagQueueResponse.proto"
    sqs
    (Proxy :: Proxy UntagQueue)

responseSendMessageBatch :: SendMessageBatchResponse -> TestTree
responseSendMessageBatch = res
    "SendMessageBatchResponse"
    "fixture/SendMessageBatchResponse.proto"
    sqs
    (Proxy :: Proxy SendMessageBatch)

responseChangeMessageVisibility :: ChangeMessageVisibilityResponse -> TestTree
responseChangeMessageVisibility = res
    "ChangeMessageVisibilityResponse"
    "fixture/ChangeMessageVisibilityResponse.proto"
    sqs
    (Proxy :: Proxy ChangeMessageVisibility)
