{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.AutoScaling.PutNotificationConfiguration
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
module Network.AWS.AutoScaling.PutNotificationConfiguration
    (
    -- * Request
      PutNotificationConfigurationType
    -- ** Request constructor
    , putNotificationConfigurationType
    -- ** Request lenses
    , pnctAutoScalingGroupName
    , pnctNotificationTypes
    , pnctTopicARN

    -- * Response
    , PutNotificationConfigurationResponse
    -- ** Response constructor
    , putNotificationConfigurationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data PutNotificationConfigurationType = PutNotificationConfigurationType
    { _pnctAutoScalingGroupName :: Text
    , _pnctNotificationTypes    :: [Text]
    , _pnctTopicARN             :: Text
    } (Eq, Ord, Show, Generic)

-- | 'PutNotificationConfigurationType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pnctAutoScalingGroupName' @::@ 'Text'
--
-- * 'pnctNotificationTypes' @::@ ['Text']
--
-- * 'pnctTopicARN' @::@ 'Text'
--
putNotificationConfigurationType :: Text -- ^ 'pnctAutoScalingGroupName'
                                 -> Text -- ^ 'pnctTopicARN'
                                 -> PutNotificationConfigurationType
putNotificationConfigurationType p1 p2 = PutNotificationConfigurationType
    { _pnctAutoScalingGroupName = p1
    , _pnctTopicARN             = p2
    , _pnctNotificationTypes    = mempty
    }

-- | The name of the Auto Scaling group.
pnctAutoScalingGroupName :: Lens' PutNotificationConfigurationType Text
pnctAutoScalingGroupName =
    lens _pnctAutoScalingGroupName
        (\s a -> s { _pnctAutoScalingGroupName = a })

-- | The type of event that will cause the notification to be sent. For
-- details about notification types supported by Auto Scaling, see
-- DescribeAutoScalingNotificationTypes.
pnctNotificationTypes :: Lens' PutNotificationConfigurationType [Text]
pnctNotificationTypes =
    lens _pnctNotificationTypes (\s a -> s { _pnctNotificationTypes = a })

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic.
pnctTopicARN :: Lens' PutNotificationConfigurationType Text
pnctTopicARN = lens _pnctTopicARN (\s a -> s { _pnctTopicARN = a })
instance ToQuery PutNotificationConfigurationType

instance ToPath PutNotificationConfigurationType where
    toPath = const "/"

data PutNotificationConfigurationResponse = PutNotificationConfigurationResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutNotificationConfigurationResponse' constructor.
putNotificationConfigurationResponse :: PutNotificationConfigurationResponse
putNotificationConfigurationResponse = PutNotificationConfigurationResponse

instance FromXML PutNotificationConfigurationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PutNotificationConfigurationResponse"

instance AWSRequest PutNotificationConfigurationType where
    type Sv PutNotificationConfigurationType = AutoScaling
    type Rs PutNotificationConfigurationType = PutNotificationConfigurationResponse

    request  = post "PutNotificationConfiguration"
    response = nullaryResponse PutNotificationConfigurationResponse
