{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Configures an Auto Scaling group to send notifications when specified
-- events take place. Subscribers to this topic can have messages for events
-- delivered to an endpoint such as a web server or email address. For more
-- information see Get Email Notifications When Your Auto Scaling Group
-- Changes A new PutNotificationConfiguration overwrites an existing
-- configuration.
module Network.AWS.AutoScaling
    (
    -- * Request
      PutNotificationConfiguration
    -- ** Request constructor
    , mkPutNotificationConfiguration
    -- ** Request lenses
    , pncAutoScalingGroupName
    , pncTopicARN
    , pncNotificationTypes

    -- * Response
    , PutNotificationConfigurationResponse
    -- ** Response constructor
    , mkPutNotificationConfigurationResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

data PutNotificationConfiguration = PutNotificationConfiguration
    { _pncAutoScalingGroupName :: !Text
    , _pncTopicARN :: !Text
    , _pncNotificationTypes :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutNotificationConfiguration' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingGroupName ::@ @Text@
--
-- * @TopicARN ::@ @Text@
--
-- * @NotificationTypes ::@ @[Text]@
--
mkPutNotificationConfiguration :: Text -- ^ 'pncAutoScalingGroupName'
                               -> Text -- ^ 'pncTopicARN'
                               -> [Text] -- ^ 'pncNotificationTypes'
                               -> PutNotificationConfiguration
mkPutNotificationConfiguration p1 p2 p3 = PutNotificationConfiguration
    { _pncAutoScalingGroupName = p1
    , _pncTopicARN = p2
    , _pncNotificationTypes = p3
    }

-- | The name of the Auto Scaling group.
pncAutoScalingGroupName :: Lens' PutNotificationConfiguration Text
pncAutoScalingGroupName =
    lens _pncAutoScalingGroupName
         (\s a -> s { _pncAutoScalingGroupName = a })

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic.
pncTopicARN :: Lens' PutNotificationConfiguration Text
pncTopicARN = lens _pncTopicARN (\s a -> s { _pncTopicARN = a })

-- | The type of event that will cause the notification to be sent. For details
-- about notification types supported by Auto Scaling, see
-- DescribeAutoScalingNotificationTypes.
pncNotificationTypes :: Lens' PutNotificationConfiguration [Text]
pncNotificationTypes =
    lens _pncNotificationTypes (\s a -> s { _pncNotificationTypes = a })

instance ToQuery PutNotificationConfiguration where
    toQuery = genericQuery def

data PutNotificationConfigurationResponse = PutNotificationConfigurationResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutNotificationConfigurationResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkPutNotificationConfigurationResponse :: PutNotificationConfigurationResponse
mkPutNotificationConfigurationResponse = PutNotificationConfigurationResponse

instance AWSRequest PutNotificationConfiguration where
    type Sv PutNotificationConfiguration = AutoScaling
    type Rs PutNotificationConfiguration = PutNotificationConfigurationResponse

    request = post "PutNotificationConfiguration"
    response _ = nullaryResponse PutNotificationConfigurationResponse
