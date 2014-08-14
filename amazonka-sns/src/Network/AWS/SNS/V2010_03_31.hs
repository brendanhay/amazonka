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
module Network.AWS.SNS.V2010_03_31
    ( module Network.AWS.SNS.V2010_03_31.AddPermission
    , module Network.AWS.SNS.V2010_03_31.ConfirmSubscription
    , module Network.AWS.SNS.V2010_03_31.CreatePlatformApplication
    , module Network.AWS.SNS.V2010_03_31.CreatePlatformEndpoint
    , module Network.AWS.SNS.V2010_03_31.CreateTopic
    , module Network.AWS.SNS.V2010_03_31.DeleteEndpoint
    , module Network.AWS.SNS.V2010_03_31.DeletePlatformApplication
    , module Network.AWS.SNS.V2010_03_31.DeleteTopic
    , module Network.AWS.SNS.V2010_03_31.GetEndpointAttributes
    , module Network.AWS.SNS.V2010_03_31.GetPlatformApplicationAttributes
    , module Network.AWS.SNS.V2010_03_31.GetSubscriptionAttributes
    , module Network.AWS.SNS.V2010_03_31.GetTopicAttributes
    , module Network.AWS.SNS.V2010_03_31.ListEndpointsByPlatformApplication
    , module Network.AWS.SNS.V2010_03_31.ListPlatformApplications
    , module Network.AWS.SNS.V2010_03_31.ListSubscriptions
    , module Network.AWS.SNS.V2010_03_31.ListSubscriptionsByTopic
    , module Network.AWS.SNS.V2010_03_31.ListTopics
    , module Network.AWS.SNS.V2010_03_31.Publish
    , module Network.AWS.SNS.V2010_03_31.RemovePermission
    , module Network.AWS.SNS.V2010_03_31.SetEndpointAttributes
    , module Network.AWS.SNS.V2010_03_31.SetPlatformApplicationAttributes
    , module Network.AWS.SNS.V2010_03_31.SetSubscriptionAttributes
    , module Network.AWS.SNS.V2010_03_31.SetTopicAttributes
    , module Network.AWS.SNS.V2010_03_31.Subscribe
    , module Network.AWS.SNS.V2010_03_31.Types
    , module Network.AWS.SNS.V2010_03_31.Unsubscribe
    ) where

import Network.AWS.SNS.V2010_03_31.AddPermission
import Network.AWS.SNS.V2010_03_31.ConfirmSubscription
import Network.AWS.SNS.V2010_03_31.CreatePlatformApplication
import Network.AWS.SNS.V2010_03_31.CreatePlatformEndpoint
import Network.AWS.SNS.V2010_03_31.CreateTopic
import Network.AWS.SNS.V2010_03_31.DeleteEndpoint
import Network.AWS.SNS.V2010_03_31.DeletePlatformApplication
import Network.AWS.SNS.V2010_03_31.DeleteTopic
import Network.AWS.SNS.V2010_03_31.GetEndpointAttributes
import Network.AWS.SNS.V2010_03_31.GetPlatformApplicationAttributes
import Network.AWS.SNS.V2010_03_31.GetSubscriptionAttributes
import Network.AWS.SNS.V2010_03_31.GetTopicAttributes
import Network.AWS.SNS.V2010_03_31.ListEndpointsByPlatformApplication
import Network.AWS.SNS.V2010_03_31.ListPlatformApplications
import Network.AWS.SNS.V2010_03_31.ListSubscriptions
import Network.AWS.SNS.V2010_03_31.ListSubscriptionsByTopic
import Network.AWS.SNS.V2010_03_31.ListTopics
import Network.AWS.SNS.V2010_03_31.Publish
import Network.AWS.SNS.V2010_03_31.RemovePermission
import Network.AWS.SNS.V2010_03_31.SetEndpointAttributes
import Network.AWS.SNS.V2010_03_31.SetPlatformApplicationAttributes
import Network.AWS.SNS.V2010_03_31.SetSubscriptionAttributes
import Network.AWS.SNS.V2010_03_31.SetTopicAttributes
import Network.AWS.SNS.V2010_03_31.Subscribe
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.SNS.V2010_03_31.Unsubscribe
