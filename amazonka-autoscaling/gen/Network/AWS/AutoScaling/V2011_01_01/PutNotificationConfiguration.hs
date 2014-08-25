{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.PutNotificationConfiguration
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
module Network.AWS.AutoScaling.V2011_01_01.PutNotificationConfiguration where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

data PutNotificationConfiguration = PutNotificationConfiguration
    { _pnctNotificationTypes :: [Text]
      -- ^ The type of event that will cause the notification to be sent.
      -- For details about notification types supported by Auto Scaling,
      -- see DescribeAutoScalingNotificationTypes.
    , _pnctTopicARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification
      -- Service (SNS) topic.
    , _pnctAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group.
    } deriving (Show, Generic)

makeLenses ''PutNotificationConfiguration

instance ToQuery PutNotificationConfiguration where
    toQuery = genericQuery def

data PutNotificationConfigurationResponse = PutNotificationConfigurationResponse
    deriving (Eq, Show, Generic)

makeLenses ''PutNotificationConfigurationResponse

instance AWSRequest PutNotificationConfiguration where
    type Sv PutNotificationConfiguration = AutoScaling
    type Rs PutNotificationConfiguration = PutNotificationConfigurationResponse

    request = post "PutNotificationConfiguration"
    response _ = nullaryResponse PutNotificationConfigurationResponse
