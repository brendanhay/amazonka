{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.V2012_11_05
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Simple Queue Service (SQS) is a fast, reliable, scalable, fully
-- managed message queuing service. SQS makes it simple and cost-effective to
-- decouple the components of a cloud application. You can use SQS to transmit
-- any volume of data, at any level of throughput, without losing messages or
-- requiring other services to be always available. With SQS, you can offload
-- the administrative burden of operating and scaling a highly available
-- messaging cluster, while paying a low price for only what you use.
module Network.AWS.SQS.V2012_11_05
    ( module Network.AWS.SQS.V2012_11_05.AddPermission
    , module Network.AWS.SQS.V2012_11_05.ChangeMessageVisibility
    , module Network.AWS.SQS.V2012_11_05.ChangeMessageVisibilityBatch
    , module Network.AWS.SQS.V2012_11_05.CreateQueue
    , module Network.AWS.SQS.V2012_11_05.DeleteMessage
    , module Network.AWS.SQS.V2012_11_05.DeleteMessageBatch
    , module Network.AWS.SQS.V2012_11_05.DeleteQueue
    , module Network.AWS.SQS.V2012_11_05.GetQueueAttributes
    , module Network.AWS.SQS.V2012_11_05.GetQueueUrl
    , module Network.AWS.SQS.V2012_11_05.Lenses
    , module Network.AWS.SQS.V2012_11_05.ListDeadLetterSourceQueues
    , module Network.AWS.SQS.V2012_11_05.ListQueues
    , module Network.AWS.SQS.V2012_11_05.ReceiveMessage
    , module Network.AWS.SQS.V2012_11_05.RemovePermission
    , module Network.AWS.SQS.V2012_11_05.SendMessage
    , module Network.AWS.SQS.V2012_11_05.SendMessageBatch
    , module Network.AWS.SQS.V2012_11_05.SetQueueAttributes
    , module Network.AWS.SQS.V2012_11_05.Types
    ) where

import Network.AWS.SQS.V2012_11_05.AddPermission
import Network.AWS.SQS.V2012_11_05.ChangeMessageVisibility
import Network.AWS.SQS.V2012_11_05.ChangeMessageVisibilityBatch
import Network.AWS.SQS.V2012_11_05.CreateQueue
import Network.AWS.SQS.V2012_11_05.DeleteMessage
import Network.AWS.SQS.V2012_11_05.DeleteMessageBatch
import Network.AWS.SQS.V2012_11_05.DeleteQueue
import Network.AWS.SQS.V2012_11_05.GetQueueAttributes
import Network.AWS.SQS.V2012_11_05.GetQueueUrl
import Network.AWS.SQS.V2012_11_05.Lenses
import Network.AWS.SQS.V2012_11_05.ListDeadLetterSourceQueues
import Network.AWS.SQS.V2012_11_05.ListQueues
import Network.AWS.SQS.V2012_11_05.ReceiveMessage
import Network.AWS.SQS.V2012_11_05.RemovePermission
import Network.AWS.SQS.V2012_11_05.SendMessage
import Network.AWS.SQS.V2012_11_05.SendMessageBatch
import Network.AWS.SQS.V2012_11_05.SetQueueAttributes
import Network.AWS.SQS.V2012_11_05.Types
