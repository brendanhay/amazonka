{-# LANGUAGE TemplateHaskell             #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.Lenses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.SNS.V2010_03_31.Lenses where

import Control.Lens.TH
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.SNS.V2010_03_31.DeleteEndpoint
import Network.AWS.SNS.V2010_03_31.RemovePermission
import Network.AWS.SNS.V2010_03_31.DeleteTopic
import Network.AWS.SNS.V2010_03_31.ListTopics
import Network.AWS.SNS.V2010_03_31.CreatePlatformEndpoint
import Network.AWS.SNS.V2010_03_31.SetPlatformApplicationAttributes
import Network.AWS.SNS.V2010_03_31.ListSubscriptionsByTopic
import Network.AWS.SNS.V2010_03_31.GetTopicAttributes
import Network.AWS.SNS.V2010_03_31.CreatePlatformApplication
import Network.AWS.SNS.V2010_03_31.GetPlatformApplicationAttributes
import Network.AWS.SNS.V2010_03_31.ListEndpointsByPlatformApplication
import Network.AWS.SNS.V2010_03_31.SetTopicAttributes
import Network.AWS.SNS.V2010_03_31.DeletePlatformApplication
import Network.AWS.SNS.V2010_03_31.ListPlatformApplications
import Network.AWS.SNS.V2010_03_31.AddPermission
import Network.AWS.SNS.V2010_03_31.GetEndpointAttributes
import Network.AWS.SNS.V2010_03_31.ListSubscriptions
import Network.AWS.SNS.V2010_03_31.GetSubscriptionAttributes
import Network.AWS.SNS.V2010_03_31.CreateTopic
import Network.AWS.SNS.V2010_03_31.Subscribe
import Network.AWS.SNS.V2010_03_31.Unsubscribe
import Network.AWS.SNS.V2010_03_31.SetEndpointAttributes
import Network.AWS.SNS.V2010_03_31.SetSubscriptionAttributes
import Network.AWS.SNS.V2010_03_31.ConfirmSubscription
import Network.AWS.SNS.V2010_03_31.Publish

-- Newtypes
makeIso ''Topic

-- Products
makeLenses ''Endpoint
makeLenses ''MessageAttributeValue
makeLenses ''PlatformApplication
makeLenses ''Subscription

-- Requests
makeLenses ''DeleteEndpoint
makeLenses ''RemovePermission
makeLenses ''DeleteTopic
makeLenses ''ListTopics
makeLenses ''CreatePlatformEndpoint
makeLenses ''SetPlatformApplicationAttributes
makeLenses ''ListSubscriptionsByTopic
makeLenses ''GetTopicAttributes
makeLenses ''CreatePlatformApplication
makeLenses ''GetPlatformApplicationAttributes
makeLenses ''ListEndpointsByPlatformApplication
makeLenses ''SetTopicAttributes
makeLenses ''DeletePlatformApplication
makeLenses ''ListPlatformApplications
makeLenses ''AddPermission
makeLenses ''GetEndpointAttributes
makeLenses ''ListSubscriptions
makeLenses ''GetSubscriptionAttributes
makeLenses ''CreateTopic
makeLenses ''Subscribe
makeLenses ''Unsubscribe
makeLenses ''SetEndpointAttributes
makeLenses ''SetSubscriptionAttributes
makeLenses ''ConfirmSubscription
makeLenses ''Publish

-- Responses
makeLenses ''DeleteEndpointResponse
makeLenses ''RemovePermissionResponse
makeLenses ''DeleteTopicResponse
makeLenses ''ListTopicsResponse
makeLenses ''CreatePlatformEndpointResponse
makeLenses ''SetPlatformApplicationAttributesResponse
makeLenses ''ListSubscriptionsByTopicResponse
makeLenses ''GetTopicAttributesResponse
makeLenses ''CreatePlatformApplicationResponse
makeLenses ''GetPlatformApplicationAttributesResponse
makeLenses ''ListEndpointsByPlatformApplicationResponse
makeLenses ''SetTopicAttributesResponse
makeLenses ''DeletePlatformApplicationResponse
makeLenses ''ListPlatformApplicationsResponse
makeLenses ''AddPermissionResponse
makeLenses ''GetEndpointAttributesResponse
makeLenses ''ListSubscriptionsResponse
makeLenses ''GetSubscriptionAttributesResponse
makeLenses ''CreateTopicResponse
makeLenses ''SubscribeResponse
makeLenses ''UnsubscribeResponse
makeLenses ''SetEndpointAttributesResponse
makeLenses ''SetSubscriptionAttributesResponse
makeLenses ''ConfirmSubscriptionResponse
makeLenses ''PublishResponse
