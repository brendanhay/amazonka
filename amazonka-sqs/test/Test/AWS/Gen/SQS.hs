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
--         [ addPermissionTest $
--             addPermission
--
--         , changeMessageVisibilityTest $
--             changeMessageVisibility
--
--         , changeMessageVisibilityBatchTest $
--             changeMessageVisibilityBatch
--
--         , createQueueTest $
--             createQueue
--
--         , deleteMessageTest $
--             deleteMessage
--
--         , deleteMessageBatchTest $
--             deleteMessageBatch
--
--         , deleteQueueTest $
--             deleteQueue
--
--         , getQueueAttributesTest $
--             getQueueAttributes
--
--         , getQueueURLTest $
--             getQueueURL
--
--         , listDeadLetterSourceQueuesTest $
--             listDeadLetterSourceQueues
--
--         , listQueuesTest $
--             listQueues
--
--         , purgeQueueTest $
--             purgeQueue
--
--         , receiveMessageTest $
--             receiveMessage
--
--         , removePermissionTest $
--             removePermission
--
--         , sendMessageTest $
--             sendMessage
--
--         , sendMessageBatchTest $
--             sendMessageBatch
--
--         , setQueueAttributesTest $
--             setQueueAttributes
--
--           ]

--     , testGroup "response"
--         [ addPermissionResponseTest $
--             addPermissionResponse
--
--         , changeMessageVisibilityResponseTest $
--             changeMessageVisibilityResponse
--
--         , changeMessageVisibilityBatchResponseTest $
--             changeMessageVisibilityBatchResponse
--
--         , createQueueResponseTest $
--             createQueueResponse
--
--         , deleteMessageResponseTest $
--             deleteMessageResponse
--
--         , deleteMessageBatchResponseTest $
--             deleteMessageBatchResponse
--
--         , deleteQueueResponseTest $
--             deleteQueueResponse
--
--         , getQueueAttributesResponseTest $
--             getQueueAttributesResponse
--
--         , getQueueURLResponseTest $
--             getQueueURLResponse
--
--         , listDeadLetterSourceQueuesResponseTest $
--             listDeadLetterSourceQueuesResponse
--
--         , listQueuesResponseTest $
--             listQueuesResponse
--
--         , purgeQueueResponseTest $
--             purgeQueueResponse
--
--         , receiveMessageResponseTest $
--             receiveMessageResponse
--
--         , removePermissionResponseTest $
--             removePermissionResponse
--
--         , sendMessageResponseTest $
--             sendMessageResponse
--
--         , sendMessageBatchResponseTest $
--             sendMessageBatchResponse
--
--         , setQueueAttributesResponseTest $
--             setQueueAttributesResponse
--
--           ]
--     ]

-- Requests

addPermissionTest :: AddPermission -> TestTree
addPermissionTest = undefined

changeMessageVisibilityTest :: ChangeMessageVisibility -> TestTree
changeMessageVisibilityTest = undefined

changeMessageVisibilityBatchTest :: ChangeMessageVisibilityBatch -> TestTree
changeMessageVisibilityBatchTest = undefined

createQueueTest :: CreateQueue -> TestTree
createQueueTest = undefined

deleteMessageTest :: DeleteMessage -> TestTree
deleteMessageTest = undefined

deleteMessageBatchTest :: DeleteMessageBatch -> TestTree
deleteMessageBatchTest = undefined

deleteQueueTest :: DeleteQueue -> TestTree
deleteQueueTest = undefined

getQueueAttributesTest :: GetQueueAttributes -> TestTree
getQueueAttributesTest = undefined

getQueueURLTest :: GetQueueURL -> TestTree
getQueueURLTest = undefined

listDeadLetterSourceQueuesTest :: ListDeadLetterSourceQueues -> TestTree
listDeadLetterSourceQueuesTest = undefined

listQueuesTest :: ListQueues -> TestTree
listQueuesTest = undefined

purgeQueueTest :: PurgeQueue -> TestTree
purgeQueueTest = undefined

receiveMessageTest :: ReceiveMessage -> TestTree
receiveMessageTest = undefined

removePermissionTest :: RemovePermission -> TestTree
removePermissionTest = undefined

sendMessageTest :: SendMessage -> TestTree
sendMessageTest = undefined

sendMessageBatchTest :: SendMessageBatch -> TestTree
sendMessageBatchTest = undefined

setQueueAttributesTest :: SetQueueAttributes -> TestTree
setQueueAttributesTest = undefined

-- Responses

addPermissionResponseTest :: AddPermissionResponse -> TestTree
addPermissionResponseTest = resp
    "addPermissionResponse"
    "fixture/AddPermissionResponse"
    (Proxy :: Proxy AddPermission)

changeMessageVisibilityResponseTest :: ChangeMessageVisibilityResponse -> TestTree
changeMessageVisibilityResponseTest = resp
    "changeMessageVisibilityResponse"
    "fixture/ChangeMessageVisibilityResponse"
    (Proxy :: Proxy ChangeMessageVisibility)

changeMessageVisibilityBatchResponseTest :: ChangeMessageVisibilityBatchResponse -> TestTree
changeMessageVisibilityBatchResponseTest = resp
    "changeMessageVisibilityBatchResponse"
    "fixture/ChangeMessageVisibilityBatchResponse"
    (Proxy :: Proxy ChangeMessageVisibilityBatch)

createQueueResponseTest :: CreateQueueResponse -> TestTree
createQueueResponseTest = resp
    "createQueueResponse"
    "fixture/CreateQueueResponse"
    (Proxy :: Proxy CreateQueue)

deleteMessageResponseTest :: DeleteMessageResponse -> TestTree
deleteMessageResponseTest = resp
    "deleteMessageResponse"
    "fixture/DeleteMessageResponse"
    (Proxy :: Proxy DeleteMessage)

deleteMessageBatchResponseTest :: DeleteMessageBatchResponse -> TestTree
deleteMessageBatchResponseTest = resp
    "deleteMessageBatchResponse"
    "fixture/DeleteMessageBatchResponse"
    (Proxy :: Proxy DeleteMessageBatch)

deleteQueueResponseTest :: DeleteQueueResponse -> TestTree
deleteQueueResponseTest = resp
    "deleteQueueResponse"
    "fixture/DeleteQueueResponse"
    (Proxy :: Proxy DeleteQueue)

getQueueAttributesResponseTest :: GetQueueAttributesResponse -> TestTree
getQueueAttributesResponseTest = resp
    "getQueueAttributesResponse"
    "fixture/GetQueueAttributesResponse"
    (Proxy :: Proxy GetQueueAttributes)

getQueueURLResponseTest :: GetQueueURLResponse -> TestTree
getQueueURLResponseTest = resp
    "getQueueURLResponse"
    "fixture/GetQueueURLResponse"
    (Proxy :: Proxy GetQueueURL)

listDeadLetterSourceQueuesResponseTest :: ListDeadLetterSourceQueuesResponse -> TestTree
listDeadLetterSourceQueuesResponseTest = resp
    "listDeadLetterSourceQueuesResponse"
    "fixture/ListDeadLetterSourceQueuesResponse"
    (Proxy :: Proxy ListDeadLetterSourceQueues)

listQueuesResponseTest :: ListQueuesResponse -> TestTree
listQueuesResponseTest = resp
    "listQueuesResponse"
    "fixture/ListQueuesResponse"
    (Proxy :: Proxy ListQueues)

purgeQueueResponseTest :: PurgeQueueResponse -> TestTree
purgeQueueResponseTest = resp
    "purgeQueueResponse"
    "fixture/PurgeQueueResponse"
    (Proxy :: Proxy PurgeQueue)

receiveMessageResponseTest :: ReceiveMessageResponse -> TestTree
receiveMessageResponseTest = resp
    "receiveMessageResponse"
    "fixture/ReceiveMessageResponse"
    (Proxy :: Proxy ReceiveMessage)

removePermissionResponseTest :: RemovePermissionResponse -> TestTree
removePermissionResponseTest = resp
    "removePermissionResponse"
    "fixture/RemovePermissionResponse"
    (Proxy :: Proxy RemovePermission)

sendMessageResponseTest :: SendMessageResponse -> TestTree
sendMessageResponseTest = resp
    "sendMessageResponse"
    "fixture/SendMessageResponse"
    (Proxy :: Proxy SendMessage)

sendMessageBatchResponseTest :: SendMessageBatchResponse -> TestTree
sendMessageBatchResponseTest = resp
    "sendMessageBatchResponse"
    "fixture/SendMessageBatchResponse"
    (Proxy :: Proxy SendMessageBatch)

setQueueAttributesResponseTest :: SetQueueAttributesResponse -> TestTree
setQueueAttributesResponseTest = resp
    "setQueueAttributesResponse"
    "fixture/SetQueueAttributesResponse"
    (Proxy :: Proxy SetQueueAttributes)
