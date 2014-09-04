{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DeleteNotificationConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes notifications created by PutNotificationConfiguration.
module Network.AWS.AutoScaling.V2011_01_01.DeleteNotificationConfiguration
    (
    -- * Request
      DeleteNotificationConfiguration
    -- ** Request constructor
    , mkDeleteNotificationConfigurationType
    -- ** Request lenses
    , dnctAutoScalingGroupName
    , dnctTopicARN

    -- * Response
    , DeleteNotificationConfigurationResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteNotificationConfiguration' request.
mkDeleteNotificationConfigurationType :: Text -- ^ 'dnctAutoScalingGroupName'
                                      -> Text -- ^ 'dnctTopicARN'
                                      -> DeleteNotificationConfiguration
mkDeleteNotificationConfigurationType p1 p2 = DeleteNotificationConfiguration
    { _dnctAutoScalingGroupName = p1
    , _dnctTopicARN = p2
    }
{-# INLINE mkDeleteNotificationConfigurationType #-}

data DeleteNotificationConfiguration = DeleteNotificationConfiguration
    { _dnctAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group.
    , _dnctTopicARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification
      -- Service (SNS) topic.
    } deriving (Show, Generic)

-- | The name of the Auto Scaling group.
dnctAutoScalingGroupName :: Lens' DeleteNotificationConfiguration (Text)
dnctAutoScalingGroupName = lens _dnctAutoScalingGroupName (\s a -> s { _dnctAutoScalingGroupName = a })
{-# INLINE dnctAutoScalingGroupName #-}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic.
dnctTopicARN :: Lens' DeleteNotificationConfiguration (Text)
dnctTopicARN = lens _dnctTopicARN (\s a -> s { _dnctTopicARN = a })
{-# INLINE dnctTopicARN #-}

instance ToQuery DeleteNotificationConfiguration where
    toQuery = genericQuery def

data DeleteNotificationConfigurationResponse = DeleteNotificationConfigurationResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteNotificationConfiguration where
    type Sv DeleteNotificationConfiguration = AutoScaling
    type Rs DeleteNotificationConfiguration = DeleteNotificationConfigurationResponse

    request = post "DeleteNotificationConfiguration"
    response _ = nullaryResponse DeleteNotificationConfigurationResponse
