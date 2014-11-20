{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_PutNotificationConfiguration.html>
module Network.AWS.AutoScaling.PutNotificationConfiguration
    (
    -- * Request
      PutNotificationConfiguration
    -- ** Request constructor
    , putNotificationConfiguration
    -- ** Request lenses
    , pncAutoScalingGroupName
    , pncNotificationTypes
    , pncTopicARN

    -- * Response
    , PutNotificationConfigurationResponse
    -- ** Response constructor
    , putNotificationConfigurationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data PutNotificationConfiguration = PutNotificationConfiguration
    { _pncAutoScalingGroupName :: Text
    , _pncNotificationTypes    :: List "LifecycleHookTypes" Text
    , _pncTopicARN             :: Text
    } deriving (Eq, Ord, Show)

-- | 'PutNotificationConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pncAutoScalingGroupName' @::@ 'Text'
--
-- * 'pncNotificationTypes' @::@ ['Text']
--
-- * 'pncTopicARN' @::@ 'Text'
--
putNotificationConfiguration :: Text -- ^ 'pncAutoScalingGroupName'
                             -> Text -- ^ 'pncTopicARN'
                             -> PutNotificationConfiguration
putNotificationConfiguration p1 p2 = PutNotificationConfiguration
    { _pncAutoScalingGroupName = p1
    , _pncTopicARN             = p2
    , _pncNotificationTypes    = mempty
    }

-- | The name of the Auto Scaling group.
pncAutoScalingGroupName :: Lens' PutNotificationConfiguration Text
pncAutoScalingGroupName =
    lens _pncAutoScalingGroupName (\s a -> s { _pncAutoScalingGroupName = a })

-- | The type of event that will cause the notification to be sent. For
-- details about notification types supported by Auto Scaling, see
-- DescribeAutoScalingNotificationTypes.
pncNotificationTypes :: Lens' PutNotificationConfiguration [Text]
pncNotificationTypes =
    lens _pncNotificationTypes (\s a -> s { _pncNotificationTypes = a })
        . _List

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic.
pncTopicARN :: Lens' PutNotificationConfiguration Text
pncTopicARN = lens _pncTopicARN (\s a -> s { _pncTopicARN = a })

data PutNotificationConfigurationResponse = PutNotificationConfigurationResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutNotificationConfigurationResponse' constructor.
putNotificationConfigurationResponse :: PutNotificationConfigurationResponse
putNotificationConfigurationResponse = PutNotificationConfigurationResponse

instance ToPath PutNotificationConfiguration where
    toPath = const "/"

instance ToQuery PutNotificationConfiguration where
    toQuery PutNotificationConfiguration{..} = mconcat
        [ "AutoScalingGroupName" =? _pncAutoScalingGroupName
        , "NotificationTypes"    =? _pncNotificationTypes
        , "TopicARN"             =? _pncTopicARN
        ]

instance ToHeaders PutNotificationConfiguration

query

instance AWSRequest PutNotificationConfiguration where
    type Sv PutNotificationConfiguration = AutoScaling
    type Rs PutNotificationConfiguration = PutNotificationConfigurationResponse

    request  = post "PutNotificationConfiguration"
    response = nullResponse PutNotificationConfigurationResponse
