{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SQS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
--         , testSendMessage $
--             sendMessage
--
--         , testChangeMessageVisibilityBatch $
--             changeMessageVisibilityBatch
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
--         , testChangeMessageVisibility $
--             changeMessageVisibility
--
--         , testSendMessageBatch $
--             sendMessageBatch
--
--           ]

--     , testGroup "response"
--         [ testGetQueueURLResponse $
--             getQueueURLResponse
--
--         , testPurgeQueueResponse $
--             purgeQueueResponse
--
--         , testSendMessageResponse $
--             sendMessageResponse
--
--         , testChangeMessageVisibilityBatchResponse $
--             changeMessageVisibilityBatchResponse
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
--         , testChangeMessageVisibilityResponse $
--             changeMessageVisibilityResponse
--
--         , testSendMessageBatchResponse $
--             sendMessageBatchResponse
--
--           ]
--     ]

-- Requests

testGetQueueURL :: GetQueueURL -> TestTree
testGetQueueURL = req
    "GetQueueURL"
    "fixture/GetQueueURL"

testPurgeQueue :: PurgeQueue -> TestTree
testPurgeQueue = req
    "PurgeQueue"
    "fixture/PurgeQueue"

testSendMessage :: SendMessage -> TestTree
testSendMessage = req
    "SendMessage"
    "fixture/SendMessage"

testChangeMessageVisibilityBatch :: ChangeMessageVisibilityBatch -> TestTree
testChangeMessageVisibilityBatch = req
    "ChangeMessageVisibilityBatch"
    "fixture/ChangeMessageVisibilityBatch"

testRemovePermission :: RemovePermission -> TestTree
testRemovePermission = req
    "RemovePermission"
    "fixture/RemovePermission"

testGetQueueAttributes :: GetQueueAttributes -> TestTree
testGetQueueAttributes = req
    "GetQueueAttributes"
    "fixture/GetQueueAttributes"

testListQueues :: ListQueues -> TestTree
testListQueues = req
    "ListQueues"
    "fixture/ListQueues"

testReceiveMessage :: ReceiveMessage -> TestTree
testReceiveMessage = req
    "ReceiveMessage"
    "fixture/ReceiveMessage"

testDeleteQueue :: DeleteQueue -> TestTree
testDeleteQueue = req
    "DeleteQueue"
    "fixture/DeleteQueue"

testDeleteMessageBatch :: DeleteMessageBatch -> TestTree
testDeleteMessageBatch = req
    "DeleteMessageBatch"
    "fixture/DeleteMessageBatch"

testSetQueueAttributes :: SetQueueAttributes -> TestTree
testSetQueueAttributes = req
    "SetQueueAttributes"
    "fixture/SetQueueAttributes"

testListDeadLetterSourceQueues :: ListDeadLetterSourceQueues -> TestTree
testListDeadLetterSourceQueues = req
    "ListDeadLetterSourceQueues"
    "fixture/ListDeadLetterSourceQueues"

testAddPermission :: AddPermission -> TestTree
testAddPermission = req
    "AddPermission"
    "fixture/AddPermission"

testDeleteMessage :: DeleteMessage -> TestTree
testDeleteMessage = req
    "DeleteMessage"
    "fixture/DeleteMessage"

testCreateQueue :: CreateQueue -> TestTree
testCreateQueue = req
    "CreateQueue"
    "fixture/CreateQueue"

testChangeMessageVisibility :: ChangeMessageVisibility -> TestTree
testChangeMessageVisibility = req
    "ChangeMessageVisibility"
    "fixture/ChangeMessageVisibility"

testSendMessageBatch :: SendMessageBatch -> TestTree
testSendMessageBatch = req
    "SendMessageBatch"
    "fixture/SendMessageBatch"

-- Responses

testGetQueueURLResponse :: GetQueueURLResponse -> TestTree
testGetQueueURLResponse = res
    "GetQueueURLResponse"
    "fixture/GetQueueURLResponse"
    (Proxy :: Proxy GetQueueURL)

testPurgeQueueResponse :: PurgeQueueResponse -> TestTree
testPurgeQueueResponse = res
    "PurgeQueueResponse"
    "fixture/PurgeQueueResponse"
    (Proxy :: Proxy PurgeQueue)

testSendMessageResponse :: SendMessageResponse -> TestTree
testSendMessageResponse = res
    "SendMessageResponse"
    "fixture/SendMessageResponse"
    (Proxy :: Proxy SendMessage)

testChangeMessageVisibilityBatchResponse :: ChangeMessageVisibilityBatchResponse -> TestTree
testChangeMessageVisibilityBatchResponse = res
    "ChangeMessageVisibilityBatchResponse"
    "fixture/ChangeMessageVisibilityBatchResponse"
    (Proxy :: Proxy ChangeMessageVisibilityBatch)

testRemovePermissionResponse :: RemovePermissionResponse -> TestTree
testRemovePermissionResponse = res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse"
    (Proxy :: Proxy RemovePermission)

testGetQueueAttributesResponse :: GetQueueAttributesResponse -> TestTree
testGetQueueAttributesResponse = res
    "GetQueueAttributesResponse"
    "fixture/GetQueueAttributesResponse"
    (Proxy :: Proxy GetQueueAttributes)

testListQueuesResponse :: ListQueuesResponse -> TestTree
testListQueuesResponse = res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse"
    (Proxy :: Proxy ListQueues)

testReceiveMessageResponse :: ReceiveMessageResponse -> TestTree
testReceiveMessageResponse = res
    "ReceiveMessageResponse"
    "fixture/ReceiveMessageResponse"
    (Proxy :: Proxy ReceiveMessage)

testDeleteQueueResponse :: DeleteQueueResponse -> TestTree
testDeleteQueueResponse = res
    "DeleteQueueResponse"
    "fixture/DeleteQueueResponse"
    (Proxy :: Proxy DeleteQueue)

testDeleteMessageBatchResponse :: DeleteMessageBatchResponse -> TestTree
testDeleteMessageBatchResponse = res
    "DeleteMessageBatchResponse"
    "fixture/DeleteMessageBatchResponse"
    (Proxy :: Proxy DeleteMessageBatch)

testSetQueueAttributesResponse :: SetQueueAttributesResponse -> TestTree
testSetQueueAttributesResponse = res
    "SetQueueAttributesResponse"
    "fixture/SetQueueAttributesResponse"
    (Proxy :: Proxy SetQueueAttributes)

testListDeadLetterSourceQueuesResponse :: ListDeadLetterSourceQueuesResponse -> TestTree
testListDeadLetterSourceQueuesResponse = res
    "ListDeadLetterSourceQueuesResponse"
    "fixture/ListDeadLetterSourceQueuesResponse"
    (Proxy :: Proxy ListDeadLetterSourceQueues)

testAddPermissionResponse :: AddPermissionResponse -> TestTree
testAddPermissionResponse = res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse"
    (Proxy :: Proxy AddPermission)

testDeleteMessageResponse :: DeleteMessageResponse -> TestTree
testDeleteMessageResponse = res
    "DeleteMessageResponse"
    "fixture/DeleteMessageResponse"
    (Proxy :: Proxy DeleteMessage)

testCreateQueueResponse :: CreateQueueResponse -> TestTree
testCreateQueueResponse = res
    "CreateQueueResponse"
    "fixture/CreateQueueResponse"
    (Proxy :: Proxy CreateQueue)

testChangeMessageVisibilityResponse :: ChangeMessageVisibilityResponse -> TestTree
testChangeMessageVisibilityResponse = res
    "ChangeMessageVisibilityResponse"
    "fixture/ChangeMessageVisibilityResponse"
    (Proxy :: Proxy ChangeMessageVisibility)

testSendMessageBatchResponse :: SendMessageBatchResponse -> TestTree
testSendMessageBatchResponse = res
    "SendMessageBatchResponse"
    "fixture/SendMessageBatchResponse"
    (Proxy :: Proxy SendMessageBatch)

instance Out AddPermission
instance Out AddPermissionResponse
instance Out BatchResultErrorEntry
instance Out ChangeMessageVisibility
instance Out ChangeMessageVisibilityBatch
instance Out ChangeMessageVisibilityBatchRequestEntry
instance Out ChangeMessageVisibilityBatchResponse
instance Out ChangeMessageVisibilityBatchResultEntry
instance Out ChangeMessageVisibilityResponse
instance Out CreateQueue
instance Out CreateQueueResponse
instance Out DeleteMessage
instance Out DeleteMessageBatch
instance Out DeleteMessageBatchRequestEntry
instance Out DeleteMessageBatchResponse
instance Out DeleteMessageBatchResultEntry
instance Out DeleteMessageResponse
instance Out DeleteQueue
instance Out DeleteQueueResponse
instance Out GetQueueAttributes
instance Out GetQueueAttributesResponse
instance Out GetQueueURL
instance Out GetQueueURLResponse
instance Out ListDeadLetterSourceQueues
instance Out ListDeadLetterSourceQueuesResponse
instance Out ListQueues
instance Out ListQueuesResponse
instance Out Message
instance Out MessageAttributeValue
instance Out PurgeQueue
instance Out PurgeQueueResponse
instance Out QueueAttributeName
instance Out ReceiveMessage
instance Out ReceiveMessageResponse
instance Out RemovePermission
instance Out RemovePermissionResponse
instance Out SendMessage
instance Out SendMessageBatch
instance Out SendMessageBatchRequestEntry
instance Out SendMessageBatchResponse
instance Out SendMessageBatchResultEntry
instance Out SendMessageResponse
instance Out SetQueueAttributes
instance Out SetQueueAttributesResponse
