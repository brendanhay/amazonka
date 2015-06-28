-- Module      : Test.SQS
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

module Test.SQS where

import Data.Proxy
import Test.AWS.Fixture
import Network.AWS.Prelude
import Network.AWS.SQS

tests :: TestTree
tests = testGroup "SQS"
    [ testGroup "response deserialisation"
        [ resp "GetQueueURL"
            "SQS/GetQueueURLResponse.gen"
            (Proxy :: Proxy GetQueueURL)
            (getQueueURLResponse undefined)

        , resp "PurgeQueue"
            "SQS/PurgeQueueResponse.gen"
            (Proxy :: Proxy PurgeQueue)
            (purgeQueueResponse undefined)

        , resp "SendMessage"
            "SQS/SendMessageResponse.gen"
            (Proxy :: Proxy SendMessage)
            (sendMessageResponse undefined)

        , resp "ChangeMessageVisibilityBatch"
            "SQS/ChangeMessageVisibilityBatchResponse.gen"
            (Proxy :: Proxy ChangeMessageVisibilityBatch)
            (changeMessageVisibilityBatchResponse undefined)

        , resp "RemovePermission"
            "SQS/RemovePermissionResponse.gen"
            (Proxy :: Proxy RemovePermission)
            (removePermissionResponse undefined)

        , resp "GetQueueAttributes"
            "SQS/GetQueueAttributesResponse.gen"
            (Proxy :: Proxy GetQueueAttributes)
            (getQueueAttributesResponse undefined)

        , resp "ListQueues"
            "SQS/ListQueuesResponse.gen"
            (Proxy :: Proxy ListQueues)
            (listQueuesResponse undefined)

        , resp "ReceiveMessage"
            "SQS/ReceiveMessageResponse.gen"
            (Proxy :: Proxy ReceiveMessage)
            (receiveMessageResponse undefined)

        , resp "DeleteQueue"
            "SQS/DeleteQueueResponse.gen"
            (Proxy :: Proxy DeleteQueue)
            (deleteQueueResponse undefined)

        , resp "DeleteMessageBatch"
            "SQS/DeleteMessageBatchResponse.gen"
            (Proxy :: Proxy DeleteMessageBatch)
            (deleteMessageBatchResponse undefined)

        , resp "SetQueueAttributes"
            "SQS/SetQueueAttributesResponse.gen"
            (Proxy :: Proxy SetQueueAttributes)
            (setQueueAttributesResponse undefined)

        , resp "ListDeadLetterSourceQueues"
            "SQS/ListDeadLetterSourceQueuesResponse.gen"
            (Proxy :: Proxy ListDeadLetterSourceQueues)
            (listDeadLetterSourceQueuesResponse undefined)

        , resp "AddPermission"
            "SQS/AddPermissionResponse.gen"
            (Proxy :: Proxy AddPermission)
            (addPermissionResponse undefined)

        , resp "DeleteMessage"
            "SQS/DeleteMessageResponse.gen"
            (Proxy :: Proxy DeleteMessage)
            (deleteMessageResponse undefined)

        , resp "CreateQueue"
            "SQS/CreateQueueResponse.gen"
            (Proxy :: Proxy CreateQueue)
            (createQueueResponse undefined)

        , resp "ChangeMessageVisibility"
            "SQS/ChangeMessageVisibilityResponse.gen"
            (Proxy :: Proxy ChangeMessageVisibility)
            (changeMessageVisibilityResponse undefined)

        , resp "SendMessageBatch"
            "SQS/SendMessageBatchResponse.gen"
            (Proxy :: Proxy SendMessageBatch)
            (sendMessageBatchResponse undefined)

        ]
    ]
