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
module Network.AWS.SQS.V2012_11_05 (module Export) where

import Network.AWS.SQS.V2012_11_05.AddPermission as Export
import Network.AWS.SQS.V2012_11_05.ChangeMessageVisibility as Export
import Network.AWS.SQS.V2012_11_05.ChangeMessageVisibilityBatch as Export
import Network.AWS.SQS.V2012_11_05.CreateQueue as Export
import Network.AWS.SQS.V2012_11_05.DeleteMessage as Export
import Network.AWS.SQS.V2012_11_05.DeleteMessageBatch as Export
import Network.AWS.SQS.V2012_11_05.DeleteQueue as Export
import Network.AWS.SQS.V2012_11_05.GetQueueAttributes as Export
import Network.AWS.SQS.V2012_11_05.GetQueueUrl as Export
import Network.AWS.SQS.V2012_11_05.ListDeadLetterSourceQueues as Export
import Network.AWS.SQS.V2012_11_05.ListQueues as Export
import Network.AWS.SQS.V2012_11_05.ReceiveMessage as Export
import Network.AWS.SQS.V2012_11_05.RemovePermission as Export
import Network.AWS.SQS.V2012_11_05.SendMessage as Export
import Network.AWS.SQS.V2012_11_05.SendMessageBatch as Export
import Network.AWS.SQS.V2012_11_05.SetQueueAttributes as Export
import Network.AWS.SQS.V2012_11_05.Types as Export
