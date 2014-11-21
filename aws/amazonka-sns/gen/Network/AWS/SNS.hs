-- Module      : Network.AWS.SNS
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
module Network.AWS.SNS
    ( module Network.AWS.SNS.AddPermission
    , module Network.AWS.SNS.ConfirmSubscription
    , module Network.AWS.SNS.CreatePlatformApplication
    , module Network.AWS.SNS.CreatePlatformEndpoint
    , module Network.AWS.SNS.CreateTopic
    , module Network.AWS.SNS.DeleteEndpoint
    , module Network.AWS.SNS.DeletePlatformApplication
    , module Network.AWS.SNS.DeleteTopic
    , module Network.AWS.SNS.GetEndpointAttributes
    , module Network.AWS.SNS.GetPlatformApplicationAttributes
    , module Network.AWS.SNS.GetSubscriptionAttributes
    , module Network.AWS.SNS.GetTopicAttributes
    , module Network.AWS.SNS.ListEndpointsByPlatformApplication
    , module Network.AWS.SNS.ListPlatformApplications
    , module Network.AWS.SNS.ListSubscriptions
    , module Network.AWS.SNS.ListSubscriptionsByTopic
    , module Network.AWS.SNS.ListTopics
    , module Network.AWS.SNS.Publish
    , module Network.AWS.SNS.RemovePermission
    , module Network.AWS.SNS.SetEndpointAttributes
    , module Network.AWS.SNS.SetPlatformApplicationAttributes
    , module Network.AWS.SNS.SetSubscriptionAttributes
    , module Network.AWS.SNS.SetTopicAttributes
    , module Network.AWS.SNS.Subscribe
    , module Network.AWS.SNS.Types
    , module Network.AWS.SNS.Unsubscribe
    ) where

import Network.AWS.SNS.AddPermission
import Network.AWS.SNS.ConfirmSubscription
import Network.AWS.SNS.CreatePlatformApplication
import Network.AWS.SNS.CreatePlatformEndpoint
import Network.AWS.SNS.CreateTopic
import Network.AWS.SNS.DeleteEndpoint
import Network.AWS.SNS.DeletePlatformApplication
import Network.AWS.SNS.DeleteTopic
import Network.AWS.SNS.GetEndpointAttributes
import Network.AWS.SNS.GetPlatformApplicationAttributes
import Network.AWS.SNS.GetSubscriptionAttributes
import Network.AWS.SNS.GetTopicAttributes
import Network.AWS.SNS.ListEndpointsByPlatformApplication
import Network.AWS.SNS.ListPlatformApplications
import Network.AWS.SNS.ListSubscriptions
import Network.AWS.SNS.ListSubscriptionsByTopic
import Network.AWS.SNS.ListTopics
import Network.AWS.SNS.Publish
import Network.AWS.SNS.RemovePermission
import Network.AWS.SNS.SetEndpointAttributes
import Network.AWS.SNS.SetPlatformApplicationAttributes
import Network.AWS.SNS.SetSubscriptionAttributes
import Network.AWS.SNS.SetTopicAttributes
import Network.AWS.SNS.Subscribe
import Network.AWS.SNS.Types
import Network.AWS.SNS.Unsubscribe
