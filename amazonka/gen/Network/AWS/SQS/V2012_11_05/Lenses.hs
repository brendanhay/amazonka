{-# LANGUAGE TemplateHaskell             #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.V2012_11_05.Lenses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.SQS.V2012_11_05.Lenses where

import Control.Lens.TH
import Network.AWS.SQS.V2012_11_05.Types
import Network.AWS.SQS.V2012_11_05.GetQueueUrl
import Network.AWS.SQS.V2012_11_05.ChangeMessageVisibilityBatch
import Network.AWS.SQS.V2012_11_05.SendMessage
import Network.AWS.SQS.V2012_11_05.RemovePermission
import Network.AWS.SQS.V2012_11_05.GetQueueAttributes
import Network.AWS.SQS.V2012_11_05.ListQueues
import Network.AWS.SQS.V2012_11_05.ReceiveMessage
import Network.AWS.SQS.V2012_11_05.DeleteQueue
import Network.AWS.SQS.V2012_11_05.DeleteMessageBatch
import Network.AWS.SQS.V2012_11_05.SetQueueAttributes
import Network.AWS.SQS.V2012_11_05.ListDeadLetterSourceQueues
import Network.AWS.SQS.V2012_11_05.AddPermission
import Network.AWS.SQS.V2012_11_05.DeleteMessage
import Network.AWS.SQS.V2012_11_05.CreateQueue
import Network.AWS.SQS.V2012_11_05.SendMessageBatch
import Network.AWS.SQS.V2012_11_05.ChangeMessageVisibility

-- Newtypes
makeIso ''ChangeMessageVisibilityBatchResultEntry
makeIso ''DeleteMessageBatchResultEntry

-- Products
makeLenses ''BatchResultErrorEntry
makeLenses ''ChangeMessageVisibilityBatchRequestEntry
makeLenses ''DeleteMessageBatchRequestEntry
makeLenses ''Message
makeLenses ''MessageAttributeValue
makeLenses ''SendMessageBatchRequestEntry
makeLenses ''SendMessageBatchResultEntry

-- Requests
makeLenses ''GetQueueUrl
makeLenses ''ChangeMessageVisibilityBatch
makeLenses ''SendMessage
makeLenses ''RemovePermission
makeLenses ''GetQueueAttributes
makeLenses ''ListQueues
makeLenses ''ReceiveMessage
makeLenses ''DeleteQueue
makeLenses ''DeleteMessageBatch
makeLenses ''SetQueueAttributes
makeLenses ''ListDeadLetterSourceQueues
makeLenses ''AddPermission
makeLenses ''DeleteMessage
makeLenses ''CreateQueue
makeLenses ''SendMessageBatch
makeLenses ''ChangeMessageVisibility

-- Responses
makeLenses ''GetQueueUrlResponse
makeLenses ''ChangeMessageVisibilityBatchResponse
makeLenses ''SendMessageResponse
makeLenses ''RemovePermissionResponse
makeLenses ''GetQueueAttributesResponse
makeLenses ''ListQueuesResponse
makeLenses ''ReceiveMessageResponse
makeLenses ''DeleteQueueResponse
makeLenses ''DeleteMessageBatchResponse
makeLenses ''SetQueueAttributesResponse
makeLenses ''ListDeadLetterSourceQueuesResponse
makeLenses ''AddPermissionResponse
makeLenses ''DeleteMessageResponse
makeLenses ''CreateQueueResponse
makeLenses ''SendMessageBatchResponse
makeLenses ''ChangeMessageVisibilityResponse
