-- Module      : Network.AWS.SQS
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

-- | Amazon Simple Queue Service (SQS) is a fast, reliable, scalable, fully
-- managed message queuing service. SQS makes it simple and cost-effective to
-- decouple the components of a cloud application. You can use SQS to transmit
-- any volume of data, at any level of throughput, without losing messages or
-- requiring other services to be always available. With SQS, you can offload
-- the administrative burden of operating and scaling a highly available
-- messaging cluster, while paying a low price for only what you use.
module Network.AWS.SQS
    ( module Network.AWS.SQS.AddPermission
    , module Network.AWS.SQS.ChangeMessageVisibility
    , module Network.AWS.SQS.ChangeMessageVisibilityBatch
    , module Network.AWS.SQS.CreateQueue
    , module Network.AWS.SQS.DeleteMessage
    , module Network.AWS.SQS.DeleteMessageBatch
    , module Network.AWS.SQS.DeleteQueue
    , module Network.AWS.SQS.GetQueueAttributes
    , module Network.AWS.SQS.GetQueueUrl
    , module Network.AWS.SQS.ListDeadLetterSourceQueues
    , module Network.AWS.SQS.ListQueues
    , module Network.AWS.SQS.PurgeQueue
    , module Network.AWS.SQS.ReceiveMessage
    , module Network.AWS.SQS.RemovePermission
    , module Network.AWS.SQS.SendMessage
    , module Network.AWS.SQS.SendMessageBatch
    , module Network.AWS.SQS.SetQueueAttributes
    , module Network.AWS.SQS.Types
    ) where

import Network.AWS.SQS.AddPermission
import Network.AWS.SQS.ChangeMessageVisibility
import Network.AWS.SQS.ChangeMessageVisibilityBatch
import Network.AWS.SQS.CreateQueue
import Network.AWS.SQS.DeleteMessage
import Network.AWS.SQS.DeleteMessageBatch
import Network.AWS.SQS.DeleteQueue
import Network.AWS.SQS.GetQueueAttributes
import Network.AWS.SQS.GetQueueUrl
import Network.AWS.SQS.ListDeadLetterSourceQueues
import Network.AWS.SQS.ListQueues
import Network.AWS.SQS.PurgeQueue
import Network.AWS.SQS.ReceiveMessage
import Network.AWS.SQS.RemovePermission
import Network.AWS.SQS.SendMessage
import Network.AWS.SQS.SendMessageBatch
import Network.AWS.SQS.SetQueueAttributes
import Network.AWS.SQS.Types
