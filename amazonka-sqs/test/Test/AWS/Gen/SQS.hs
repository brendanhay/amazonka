{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SQS
-- Copyright   : (c) 2013-2015 Brendan Hay
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
    sQS
    (Proxy :: Proxy GetQueueURL)

testPurgeQueueResponse :: PurgeQueueResponse -> TestTree
testPurgeQueueResponse = res
    "PurgeQueueResponse"
    "fixture/PurgeQueueResponse"
    sQS
    (Proxy :: Proxy PurgeQueue)

testSendMessageResponse :: SendMessageResponse -> TestTree
testSendMessageResponse = res
    "SendMessageResponse"
    "fixture/SendMessageResponse"
    sQS
    (Proxy :: Proxy SendMessage)

testChangeMessageVisibilityBatchResponse :: ChangeMessageVisibilityBatchResponse -> TestTree
testChangeMessageVisibilityBatchResponse = res
    "ChangeMessageVisibilityBatchResponse"
    "fixture/ChangeMessageVisibilityBatchResponse"
    sQS
    (Proxy :: Proxy ChangeMessageVisibilityBatch)

testRemovePermissionResponse :: RemovePermissionResponse -> TestTree
testRemovePermissionResponse = res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse"
    sQS
    (Proxy :: Proxy RemovePermission)

testGetQueueAttributesResponse :: GetQueueAttributesResponse -> TestTree
testGetQueueAttributesResponse = res
    "GetQueueAttributesResponse"
    "fixture/GetQueueAttributesResponse"
    sQS
    (Proxy :: Proxy GetQueueAttributes)

testListQueuesResponse :: ListQueuesResponse -> TestTree
testListQueuesResponse = res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse"
    sQS
    (Proxy :: Proxy ListQueues)

testReceiveMessageResponse :: ReceiveMessageResponse -> TestTree
testReceiveMessageResponse = res
    "ReceiveMessageResponse"
    "fixture/ReceiveMessageResponse"
    sQS
    (Proxy :: Proxy ReceiveMessage)

testDeleteQueueResponse :: DeleteQueueResponse -> TestTree
testDeleteQueueResponse = res
    "DeleteQueueResponse"
    "fixture/DeleteQueueResponse"
    sQS
    (Proxy :: Proxy DeleteQueue)

testDeleteMessageBatchResponse :: DeleteMessageBatchResponse -> TestTree
testDeleteMessageBatchResponse = res
    "DeleteMessageBatchResponse"
    "fixture/DeleteMessageBatchResponse"
    sQS
    (Proxy :: Proxy DeleteMessageBatch)

testSetQueueAttributesResponse :: SetQueueAttributesResponse -> TestTree
testSetQueueAttributesResponse = res
    "SetQueueAttributesResponse"
    "fixture/SetQueueAttributesResponse"
    sQS
    (Proxy :: Proxy SetQueueAttributes)

testListDeadLetterSourceQueuesResponse :: ListDeadLetterSourceQueuesResponse -> TestTree
testListDeadLetterSourceQueuesResponse = res
    "ListDeadLetterSourceQueuesResponse"
    "fixture/ListDeadLetterSourceQueuesResponse"
    sQS
    (Proxy :: Proxy ListDeadLetterSourceQueues)

testAddPermissionResponse :: AddPermissionResponse -> TestTree
testAddPermissionResponse = res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse"
    sQS
    (Proxy :: Proxy AddPermission)

testDeleteMessageResponse :: DeleteMessageResponse -> TestTree
testDeleteMessageResponse = res
    "DeleteMessageResponse"
    "fixture/DeleteMessageResponse"
    sQS
    (Proxy :: Proxy DeleteMessage)

testCreateQueueResponse :: CreateQueueResponse -> TestTree
testCreateQueueResponse = res
    "CreateQueueResponse"
    "fixture/CreateQueueResponse"
    sQS
    (Proxy :: Proxy CreateQueue)

testChangeMessageVisibilityResponse :: ChangeMessageVisibilityResponse -> TestTree
testChangeMessageVisibilityResponse = res
    "ChangeMessageVisibilityResponse"
    "fixture/ChangeMessageVisibilityResponse"
    sQS
    (Proxy :: Proxy ChangeMessageVisibility)

testSendMessageBatchResponse :: SendMessageBatchResponse -> TestTree
testSendMessageBatchResponse = res
    "SendMessageBatchResponse"
    "fixture/SendMessageBatchResponse"
    sQS
    (Proxy :: Proxy SendMessageBatch)
