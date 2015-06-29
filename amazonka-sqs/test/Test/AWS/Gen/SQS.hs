-- Module      : Test.AWS.Gen.SQS
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ getQueueURLTest $
--             getQueueURL
--
--         , purgeQueueTest $
--             purgeQueue
--
--         , sendMessageTest $
--             sendMessage
--
--         , changeMessageVisibilityBatchTest $
--             changeMessageVisibilityBatch
--
--         , removePermissionTest $
--             removePermission
--
--         , getQueueAttributesTest $
--             getQueueAttributes
--
--         , listQueuesTest $
--             listQueues
--
--         , receiveMessageTest $
--             receiveMessage
--
--         , deleteQueueTest $
--             deleteQueue
--
--         , deleteMessageBatchTest $
--             deleteMessageBatch
--
--         , setQueueAttributesTest $
--             setQueueAttributes
--
--         , listDeadLetterSourceQueuesTest $
--             listDeadLetterSourceQueues
--
--         , addPermissionTest $
--             addPermission
--
--         , deleteMessageTest $
--             deleteMessage
--
--         , createQueueTest $
--             createQueue
--
--         , changeMessageVisibilityTest $
--             changeMessageVisibility
--
--         , sendMessageBatchTest $
--             sendMessageBatch
--
--           ]

--     , testGroup "response"
--         [ getQueueURLResponseTest $
--             getQueueURLResponse
--
--         , purgeQueueResponseTest $
--             purgeQueueResponse
--
--         , sendMessageResponseTest $
--             sendMessageResponse
--
--         , changeMessageVisibilityBatchResponseTest $
--             changeMessageVisibilityBatchResponse
--
--         , removePermissionResponseTest $
--             removePermissionResponse
--
--         , getQueueAttributesResponseTest $
--             getQueueAttributesResponse
--
--         , listQueuesResponseTest $
--             listQueuesResponse
--
--         , receiveMessageResponseTest $
--             receiveMessageResponse
--
--         , deleteQueueResponseTest $
--             deleteQueueResponse
--
--         , deleteMessageBatchResponseTest $
--             deleteMessageBatchResponse
--
--         , setQueueAttributesResponseTest $
--             setQueueAttributesResponse
--
--         , listDeadLetterSourceQueuesResponseTest $
--             listDeadLetterSourceQueuesResponse
--
--         , addPermissionResponseTest $
--             addPermissionResponse
--
--         , deleteMessageResponseTest $
--             deleteMessageResponse
--
--         , createQueueResponseTest $
--             createQueueResponse
--
--         , changeMessageVisibilityResponseTest $
--             changeMessageVisibilityResponse
--
--         , sendMessageBatchResponseTest $
--             sendMessageBatchResponse
--
--           ]
--     ]

-- Requests

getQueueURLTest :: GetQueueURL -> TestTree
getQueueURLTest = undefined

purgeQueueTest :: PurgeQueue -> TestTree
purgeQueueTest = undefined

sendMessageTest :: SendMessage -> TestTree
sendMessageTest = undefined

changeMessageVisibilityBatchTest :: ChangeMessageVisibilityBatch -> TestTree
changeMessageVisibilityBatchTest = undefined

removePermissionTest :: RemovePermission -> TestTree
removePermissionTest = undefined

getQueueAttributesTest :: GetQueueAttributes -> TestTree
getQueueAttributesTest = undefined

listQueuesTest :: ListQueues -> TestTree
listQueuesTest = undefined

receiveMessageTest :: ReceiveMessage -> TestTree
receiveMessageTest = undefined

deleteQueueTest :: DeleteQueue -> TestTree
deleteQueueTest = undefined

deleteMessageBatchTest :: DeleteMessageBatch -> TestTree
deleteMessageBatchTest = undefined

setQueueAttributesTest :: SetQueueAttributes -> TestTree
setQueueAttributesTest = undefined

listDeadLetterSourceQueuesTest :: ListDeadLetterSourceQueues -> TestTree
listDeadLetterSourceQueuesTest = undefined

addPermissionTest :: AddPermission -> TestTree
addPermissionTest = undefined

deleteMessageTest :: DeleteMessage -> TestTree
deleteMessageTest = undefined

createQueueTest :: CreateQueue -> TestTree
createQueueTest = undefined

changeMessageVisibilityTest :: ChangeMessageVisibility -> TestTree
changeMessageVisibilityTest = undefined

sendMessageBatchTest :: SendMessageBatch -> TestTree
sendMessageBatchTest = undefined

-- Responses

getQueueURLResponseTest :: GetQueueURLResponse -> TestTree
getQueueURLResponseTest = resp
    "GetQueueURLResponse"
    "fixture/GetQueueURLResponse"
    (Proxy :: Proxy GetQueueURL)

purgeQueueResponseTest :: PurgeQueueResponse -> TestTree
purgeQueueResponseTest = resp
    "PurgeQueueResponse"
    "fixture/PurgeQueueResponse"
    (Proxy :: Proxy PurgeQueue)

sendMessageResponseTest :: SendMessageResponse -> TestTree
sendMessageResponseTest = resp
    "SendMessageResponse"
    "fixture/SendMessageResponse"
    (Proxy :: Proxy SendMessage)

changeMessageVisibilityBatchResponseTest :: ChangeMessageVisibilityBatchResponse -> TestTree
changeMessageVisibilityBatchResponseTest = resp
    "ChangeMessageVisibilityBatchResponse"
    "fixture/ChangeMessageVisibilityBatchResponse"
    (Proxy :: Proxy ChangeMessageVisibilityBatch)

removePermissionResponseTest :: RemovePermissionResponse -> TestTree
removePermissionResponseTest = resp
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse"
    (Proxy :: Proxy RemovePermission)

getQueueAttributesResponseTest :: GetQueueAttributesResponse -> TestTree
getQueueAttributesResponseTest = resp
    "GetQueueAttributesResponse"
    "fixture/GetQueueAttributesResponse"
    (Proxy :: Proxy GetQueueAttributes)

listQueuesResponseTest :: ListQueuesResponse -> TestTree
listQueuesResponseTest = resp
    "ListQueuesResponse"
    "fixture/ListQueuesResponse"
    (Proxy :: Proxy ListQueues)

receiveMessageResponseTest :: ReceiveMessageResponse -> TestTree
receiveMessageResponseTest = resp
    "ReceiveMessageResponse"
    "fixture/ReceiveMessageResponse"
    (Proxy :: Proxy ReceiveMessage)

deleteQueueResponseTest :: DeleteQueueResponse -> TestTree
deleteQueueResponseTest = resp
    "DeleteQueueResponse"
    "fixture/DeleteQueueResponse"
    (Proxy :: Proxy DeleteQueue)

deleteMessageBatchResponseTest :: DeleteMessageBatchResponse -> TestTree
deleteMessageBatchResponseTest = resp
    "DeleteMessageBatchResponse"
    "fixture/DeleteMessageBatchResponse"
    (Proxy :: Proxy DeleteMessageBatch)

setQueueAttributesResponseTest :: SetQueueAttributesResponse -> TestTree
setQueueAttributesResponseTest = resp
    "SetQueueAttributesResponse"
    "fixture/SetQueueAttributesResponse"
    (Proxy :: Proxy SetQueueAttributes)

listDeadLetterSourceQueuesResponseTest :: ListDeadLetterSourceQueuesResponse -> TestTree
listDeadLetterSourceQueuesResponseTest = resp
    "ListDeadLetterSourceQueuesResponse"
    "fixture/ListDeadLetterSourceQueuesResponse"
    (Proxy :: Proxy ListDeadLetterSourceQueues)

addPermissionResponseTest :: AddPermissionResponse -> TestTree
addPermissionResponseTest = resp
    "AddPermissionResponse"
    "fixture/AddPermissionResponse"
    (Proxy :: Proxy AddPermission)

deleteMessageResponseTest :: DeleteMessageResponse -> TestTree
deleteMessageResponseTest = resp
    "DeleteMessageResponse"
    "fixture/DeleteMessageResponse"
    (Proxy :: Proxy DeleteMessage)

createQueueResponseTest :: CreateQueueResponse -> TestTree
createQueueResponseTest = resp
    "CreateQueueResponse"
    "fixture/CreateQueueResponse"
    (Proxy :: Proxy CreateQueue)

changeMessageVisibilityResponseTest :: ChangeMessageVisibilityResponse -> TestTree
changeMessageVisibilityResponseTest = resp
    "ChangeMessageVisibilityResponse"
    "fixture/ChangeMessageVisibilityResponse"
    (Proxy :: Proxy ChangeMessageVisibility)

sendMessageBatchResponseTest :: SendMessageBatchResponse -> TestTree
sendMessageBatchResponseTest = resp
    "SendMessageBatchResponse"
    "fixture/SendMessageBatchResponse"
    (Proxy :: Proxy SendMessageBatch)
