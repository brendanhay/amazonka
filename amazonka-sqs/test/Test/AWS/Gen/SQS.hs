{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.AWS.Gen.SQS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.SQS where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.SQS

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
testGetQueueURL = undefined

testPurgeQueue :: PurgeQueue -> TestTree
testPurgeQueue = undefined

testSendMessage :: SendMessage -> TestTree
testSendMessage = undefined

testChangeMessageVisibilityBatch :: ChangeMessageVisibilityBatch -> TestTree
testChangeMessageVisibilityBatch = undefined

testRemovePermission :: RemovePermission -> TestTree
testRemovePermission = undefined

testGetQueueAttributes :: GetQueueAttributes -> TestTree
testGetQueueAttributes = undefined

testListQueues :: ListQueues -> TestTree
testListQueues = undefined

testReceiveMessage :: ReceiveMessage -> TestTree
testReceiveMessage = undefined

testDeleteQueue :: DeleteQueue -> TestTree
testDeleteQueue = undefined

testDeleteMessageBatch :: DeleteMessageBatch -> TestTree
testDeleteMessageBatch = undefined

testSetQueueAttributes :: SetQueueAttributes -> TestTree
testSetQueueAttributes = undefined

testListDeadLetterSourceQueues :: ListDeadLetterSourceQueues -> TestTree
testListDeadLetterSourceQueues = undefined

testAddPermission :: AddPermission -> TestTree
testAddPermission = undefined

testDeleteMessage :: DeleteMessage -> TestTree
testDeleteMessage = undefined

testCreateQueue :: CreateQueue -> TestTree
testCreateQueue = undefined

testChangeMessageVisibility :: ChangeMessageVisibility -> TestTree
testChangeMessageVisibility = undefined

testSendMessageBatch :: SendMessageBatch -> TestTree
testSendMessageBatch = undefined

-- Responses

testGetQueueURLResponse :: GetQueueURLResponse -> TestTree
testGetQueueURLResponse = resp
    "GetQueueURLResponse"
    "fixture/GetQueueURLResponse"
    (Proxy :: Proxy GetQueueURL)

testPurgeQueueResponse :: PurgeQueueResponse -> TestTree
testPurgeQueueResponse = resp
    "PurgeQueueResponse"
    "fixture/PurgeQueueResponse"
    (Proxy :: Proxy PurgeQueue)

testSendMessageResponse :: SendMessageResponse -> TestTree
testSendMessageResponse = resp
    "SendMessageResponse"
    "fixture/SendMessageResponse"
    (Proxy :: Proxy SendMessage)

testChangeMessageVisibilityBatchResponse :: ChangeMessageVisibilityBatchResponse -> TestTree
testChangeMessageVisibilityBatchResponse = resp
    "ChangeMessageVisibilityBatchResponse"
    "fixture/ChangeMessageVisibilityBatchResponse"
    (Proxy :: Proxy ChangeMessageVisibilityBatch)

testRemovePermissionResponse :: RemovePermissionResponse -> TestTree
testRemovePermissionResponse = resp
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse"
    (Proxy :: Proxy RemovePermission)

testGetQueueAttributesResponse :: GetQueueAttributesResponse -> TestTree
testGetQueueAttributesResponse = resp
    "GetQueueAttributesResponse"
    "fixture/GetQueueAttributesResponse"
    (Proxy :: Proxy GetQueueAttributes)

testListQueuesResponse :: ListQueuesResponse -> TestTree
testListQueuesResponse = resp
    "ListQueuesResponse"
    "fixture/ListQueuesResponse"
    (Proxy :: Proxy ListQueues)

testReceiveMessageResponse :: ReceiveMessageResponse -> TestTree
testReceiveMessageResponse = resp
    "ReceiveMessageResponse"
    "fixture/ReceiveMessageResponse"
    (Proxy :: Proxy ReceiveMessage)

testDeleteQueueResponse :: DeleteQueueResponse -> TestTree
testDeleteQueueResponse = resp
    "DeleteQueueResponse"
    "fixture/DeleteQueueResponse"
    (Proxy :: Proxy DeleteQueue)

testDeleteMessageBatchResponse :: DeleteMessageBatchResponse -> TestTree
testDeleteMessageBatchResponse = resp
    "DeleteMessageBatchResponse"
    "fixture/DeleteMessageBatchResponse"
    (Proxy :: Proxy DeleteMessageBatch)

testSetQueueAttributesResponse :: SetQueueAttributesResponse -> TestTree
testSetQueueAttributesResponse = resp
    "SetQueueAttributesResponse"
    "fixture/SetQueueAttributesResponse"
    (Proxy :: Proxy SetQueueAttributes)

testListDeadLetterSourceQueuesResponse :: ListDeadLetterSourceQueuesResponse -> TestTree
testListDeadLetterSourceQueuesResponse = resp
    "ListDeadLetterSourceQueuesResponse"
    "fixture/ListDeadLetterSourceQueuesResponse"
    (Proxy :: Proxy ListDeadLetterSourceQueues)

testAddPermissionResponse :: AddPermissionResponse -> TestTree
testAddPermissionResponse = resp
    "AddPermissionResponse"
    "fixture/AddPermissionResponse"
    (Proxy :: Proxy AddPermission)

testDeleteMessageResponse :: DeleteMessageResponse -> TestTree
testDeleteMessageResponse = resp
    "DeleteMessageResponse"
    "fixture/DeleteMessageResponse"
    (Proxy :: Proxy DeleteMessage)

testCreateQueueResponse :: CreateQueueResponse -> TestTree
testCreateQueueResponse = resp
    "CreateQueueResponse"
    "fixture/CreateQueueResponse"
    (Proxy :: Proxy CreateQueue)

testChangeMessageVisibilityResponse :: ChangeMessageVisibilityResponse -> TestTree
testChangeMessageVisibilityResponse = resp
    "ChangeMessageVisibilityResponse"
    "fixture/ChangeMessageVisibilityResponse"
    (Proxy :: Proxy ChangeMessageVisibility)

testSendMessageBatchResponse :: SendMessageBatchResponse -> TestTree
testSendMessageBatchResponse = resp
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
