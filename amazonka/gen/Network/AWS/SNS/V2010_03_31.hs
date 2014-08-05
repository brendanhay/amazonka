-- Module      : Network.AWS.SNS.V2010_03_31
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Simple Notification Service (Amazon SNS) is a fast, flexible, fully
-- managed push messaging service. Amazon SNS makes it simple and
-- cost-effective to push notifications to Apple, Google, Fire OS, and Windows
-- devices, as well as Android devices in China with Baidu Cloud Push. You can
-- also use SNS to push notifications to internet connected smart devices, as
-- well as other distributed services. Besides pushing cloud notifications
-- directly to mobile devices, Amazon SNS can also deliver notifications by
-- SMS text message or email, to Amazon Simple Queue Service (SQS) queues, or
-- to any HTTP endpoint. To prevent messages from being lost, all messages
-- published to Amazon SNS are stored redundantly across multiple availability
-- zones.
module Network.AWS.SNS.V2010_03_31 (module Export) where

import Network.AWS.SNS.V2010_03_31.AddPermission as Export
import Network.AWS.SNS.V2010_03_31.ConfirmSubscription as Export
import Network.AWS.SNS.V2010_03_31.CreatePlatformApplication as Export
import Network.AWS.SNS.V2010_03_31.CreatePlatformEndpoint as Export
import Network.AWS.SNS.V2010_03_31.CreateTopic as Export
import Network.AWS.SNS.V2010_03_31.DeleteEndpoint as Export
import Network.AWS.SNS.V2010_03_31.DeletePlatformApplication as Export
import Network.AWS.SNS.V2010_03_31.DeleteTopic as Export
import Network.AWS.SNS.V2010_03_31.GetEndpointAttributes as Export
import Network.AWS.SNS.V2010_03_31.GetPlatformApplicationAttributes as Export
import Network.AWS.SNS.V2010_03_31.GetSubscriptionAttributes as Export
import Network.AWS.SNS.V2010_03_31.GetTopicAttributes as Export
import Network.AWS.SNS.V2010_03_31.ListEndpointsByPlatformApplication as Export
import Network.AWS.SNS.V2010_03_31.ListPlatformApplications as Export
import Network.AWS.SNS.V2010_03_31.ListSubscriptions as Export
import Network.AWS.SNS.V2010_03_31.ListSubscriptionsByTopic as Export
import Network.AWS.SNS.V2010_03_31.ListTopics as Export
import Network.AWS.SNS.V2010_03_31.Publish as Export
import Network.AWS.SNS.V2010_03_31.RemovePermission as Export
import Network.AWS.SNS.V2010_03_31.SetEndpointAttributes as Export
import Network.AWS.SNS.V2010_03_31.SetPlatformApplicationAttributes as Export
import Network.AWS.SNS.V2010_03_31.SetSubscriptionAttributes as Export
import Network.AWS.SNS.V2010_03_31.SetTopicAttributes as Export
import Network.AWS.SNS.V2010_03_31.Subscribe as Export
import Network.AWS.SNS.V2010_03_31.Types as Export
import Network.AWS.SNS.V2010_03_31.Unsubscribe as Export
