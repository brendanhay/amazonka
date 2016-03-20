{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SQS
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.SQS where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.SQS
import Test.AWS.SQS.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testGetQueueURL $
--             getQueueURL
--
--         , testPurgeQueue $
--             purgeQueue
--
--         , testChangeMessageVisibilityBatch $
--             changeMessageVisibilityBatch
--
--         , testSendMessage $
--             sendMessage
--
--         , testRemovePermission $
--             removePermission
--
--         , testGetQueueAttributes $
--             getQueueAttributes
--
--         , testListQueues $
--             listQueues
--
--         , testReceiveMessage $
--             receiveMessage
--
--         , testDeleteQueue $
--             deleteQueue
--
--         , testDeleteMessageBatch $
--             deleteMessageBatch
--
--         , testSetQueueAttributes $
--             setQueueAttributes
--
--         , testListDeadLetterSourceQueues $
--             listDeadLetterSourceQueues
--
--         , testAddPermission $
--             addPermission
--
--         , testDeleteMessage $
--             deleteMessage
--
--         , testCreateQueue $
--             createQueue
--
--         , testSendMessageBatch $
--             sendMessageBatch
--
--         , testChangeMessageVisibility $
--             changeMessageVisibility
--
--           ]

--     , testGroup "response"
--         [ testGetQueueURLResponse $
--             getQueueURLResponse
--
--         , testPurgeQueueResponse $
--             purgeQueueResponse
--
--         , testChangeMessageVisibilityBatchResponse $
--             changeMessageVisibilityBatchResponse
--
--         , testSendMessageResponse $
--             sendMessageResponse
--
--         , testRemovePermissionResponse $
--             removePermissionResponse
--
--         , testGetQueueAttributesResponse $
--             getQueueAttributesResponse
--
--         , testListQueuesResponse $
--             listQueuesResponse
--
--         , testReceiveMessageResponse $
--             receiveMessageResponse
--
--         , testDeleteQueueResponse $
--             deleteQueueResponse
--
--         , testDeleteMessageBatchResponse $
--             deleteMessageBatchResponse
--
--         , testSetQueueAttributesResponse $
--             setQueueAttributesResponse
--
--         , testListDeadLetterSourceQueuesResponse $
--             listDeadLetterSourceQueuesResponse
--
--         , testAddPermissionResponse $
--             addPermissionResponse
--
--         , testDeleteMessageResponse $
--             deleteMessageResponse
--
--         , testCreateQueueResponse $
--             createQueueResponse
--
--         , testSendMessageBatchResponse $
--             sendMessageBatchResponse
--
--         , testChangeMessageVisibilityResponse $
--             changeMessageVisibilityResponse
--
--           ]
--     ]

-- Requests

testGetQueueURL :: GetQueueURL -> TestTree
testGetQueueURL = req
    "GetQueueURL"
    "fixture/GetQueueURL.yaml"

testPurgeQueue :: PurgeQueue -> TestTree
testPurgeQueue = req
    "PurgeQueue"
    "fixture/PurgeQueue.yaml"

testChangeMessageVisibilityBatch :: ChangeMessageVisibilityBatch -> TestTree
testChangeMessageVisibilityBatch = req
    "ChangeMessageVisibilityBatch"
    "fixture/ChangeMessageVisibilityBatch.yaml"

testSendMessage :: SendMessage -> TestTree
testSendMessage = req
    "SendMessage"
    "fixture/SendMessage.yaml"

testRemovePermission :: RemovePermission -> TestTree
testRemovePermission = req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

testGetQueueAttributes :: GetQueueAttributes -> TestTree
testGetQueueAttributes = req
    "GetQueueAttributes"
    "fixture/GetQueueAttributes.yaml"

testListQueues :: ListQueues -> TestTree
testListQueues = req
    "ListQueues"
    "fixture/ListQueues.yaml"

testReceiveMessage :: ReceiveMessage -> TestTree
testReceiveMessage = req
    "ReceiveMessage"
    "fixture/ReceiveMessage.yaml"

testDeleteQueue :: DeleteQueue -> TestTree
testDeleteQueue = req
    "DeleteQueue"
    "fixture/DeleteQueue.yaml"

testDeleteMessageBatch :: DeleteMessageBatch -> TestTree
testDeleteMessageBatch = req
    "DeleteMessageBatch"
    "fixture/DeleteMessageBatch.yaml"

testSetQueueAttributes :: SetQueueAttributes -> TestTree
testSetQueueAttributes = req
    "SetQueueAttributes"
    "fixture/SetQueueAttributes.yaml"

testListDeadLetterSourceQueues :: ListDeadLetterSourceQueues -> TestTree
testListDeadLetterSourceQueues = req
    "ListDeadLetterSourceQueues"
    "fixture/ListDeadLetterSourceQueues.yaml"

testAddPermission :: AddPermission -> TestTree
testAddPermission = req
    "AddPermission"
    "fixture/AddPermission.yaml"

testDeleteMessage :: DeleteMessage -> TestTree
testDeleteMessage = req
    "DeleteMessage"
    "fixture/DeleteMessage.yaml"

testCreateQueue :: CreateQueue -> TestTree
testCreateQueue = req
    "CreateQueue"
    "fixture/CreateQueue.yaml"

testSendMessageBatch :: SendMessageBatch -> TestTree
testSendMessageBatch = req
    "SendMessageBatch"
    "fixture/SendMessageBatch.yaml"

testChangeMessageVisibility :: ChangeMessageVisibility -> TestTree
testChangeMessageVisibility = req
    "ChangeMessageVisibility"
    "fixture/ChangeMessageVisibility.yaml"

-- Responses

testGetQueueURLResponse :: GetQueueURLResponse -> TestTree
testGetQueueURLResponse = res
    "GetQueueURLResponse"
    "fixture/GetQueueURLResponse.proto"
    sQS
    (Proxy :: Proxy GetQueueURL)

testPurgeQueueResponse :: PurgeQueueResponse -> TestTree
testPurgeQueueResponse = res
    "PurgeQueueResponse"
    "fixture/PurgeQueueResponse.proto"
    sQS
    (Proxy :: Proxy PurgeQueue)

testChangeMessageVisibilityBatchResponse :: ChangeMessageVisibilityBatchResponse -> TestTree
testChangeMessageVisibilityBatchResponse = res
    "ChangeMessageVisibilityBatchResponse"
    "fixture/ChangeMessageVisibilityBatchResponse.proto"
    sQS
    (Proxy :: Proxy ChangeMessageVisibilityBatch)

testSendMessageResponse :: SendMessageResponse -> TestTree
testSendMessageResponse = res
    "SendMessageResponse"
    "fixture/SendMessageResponse.proto"
    sQS
    (Proxy :: Proxy SendMessage)

testRemovePermissionResponse :: RemovePermissionResponse -> TestTree
testRemovePermissionResponse = res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    sQS
    (Proxy :: Proxy RemovePermission)

testGetQueueAttributesResponse :: GetQueueAttributesResponse -> TestTree
testGetQueueAttributesResponse = res
    "GetQueueAttributesResponse"
    "fixture/GetQueueAttributesResponse.proto"
    sQS
    (Proxy :: Proxy GetQueueAttributes)

testListQueuesResponse :: ListQueuesResponse -> TestTree
testListQueuesResponse = res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse.proto"
    sQS
    (Proxy :: Proxy ListQueues)

testReceiveMessageResponse :: ReceiveMessageResponse -> TestTree
testReceiveMessageResponse = res
    "ReceiveMessageResponse"
    "fixture/ReceiveMessageResponse.proto"
    sQS
    (Proxy :: Proxy ReceiveMessage)

testDeleteQueueResponse :: DeleteQueueResponse -> TestTree
testDeleteQueueResponse = res
    "DeleteQueueResponse"
    "fixture/DeleteQueueResponse.proto"
    sQS
    (Proxy :: Proxy DeleteQueue)

testDeleteMessageBatchResponse :: DeleteMessageBatchResponse -> TestTree
testDeleteMessageBatchResponse = res
    "DeleteMessageBatchResponse"
    "fixture/DeleteMessageBatchResponse.proto"
    sQS
    (Proxy :: Proxy DeleteMessageBatch)

testSetQueueAttributesResponse :: SetQueueAttributesResponse -> TestTree
testSetQueueAttributesResponse = res
    "SetQueueAttributesResponse"
    "fixture/SetQueueAttributesResponse.proto"
    sQS
    (Proxy :: Proxy SetQueueAttributes)

testListDeadLetterSourceQueuesResponse :: ListDeadLetterSourceQueuesResponse -> TestTree
testListDeadLetterSourceQueuesResponse = res
    "ListDeadLetterSourceQueuesResponse"
    "fixture/ListDeadLetterSourceQueuesResponse.proto"
    sQS
    (Proxy :: Proxy ListDeadLetterSourceQueues)

testAddPermissionResponse :: AddPermissionResponse -> TestTree
testAddPermissionResponse = res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    sQS
    (Proxy :: Proxy AddPermission)

testDeleteMessageResponse :: DeleteMessageResponse -> TestTree
testDeleteMessageResponse = res
    "DeleteMessageResponse"
    "fixture/DeleteMessageResponse.proto"
    sQS
    (Proxy :: Proxy DeleteMessage)

testCreateQueueResponse :: CreateQueueResponse -> TestTree
testCreateQueueResponse = res
    "CreateQueueResponse"
    "fixture/CreateQueueResponse.proto"
    sQS
    (Proxy :: Proxy CreateQueue)

testSendMessageBatchResponse :: SendMessageBatchResponse -> TestTree
testSendMessageBatchResponse = res
    "SendMessageBatchResponse"
    "fixture/SendMessageBatchResponse.proto"
    sQS
    (Proxy :: Proxy SendMessageBatch)

testChangeMessageVisibilityResponse :: ChangeMessageVisibilityResponse -> TestTree
testChangeMessageVisibilityResponse = res
    "ChangeMessageVisibilityResponse"
    "fixture/ChangeMessageVisibilityResponse.proto"
    sQS
    (Proxy :: Proxy ChangeMessageVisibility)
