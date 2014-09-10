{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DeleteNotificationConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes notifications created by PutNotificationConfiguration.
module Network.AWS.AutoScaling.DeleteNotificationConfiguration
    (
    -- * Request
      DeleteNotificationConfiguration
    -- ** Request constructor
    , mkDeleteNotificationConfiguration
    -- ** Request lenses
    , dncAutoScalingGroupName
    , dncTopicARN

    -- * Response
    , DeleteNotificationConfigurationResponse
    -- ** Response constructor
    , mkDeleteNotificationConfigurationResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

-- | 
data DeleteNotificationConfiguration = DeleteNotificationConfiguration
    { _dncAutoScalingGroupName :: !Text
    , _dncTopicARN :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteNotificationConfiguration' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingGroupName ::@ @Text@
--
-- * @TopicARN ::@ @Text@
--
mkDeleteNotificationConfiguration :: Text -- ^ 'dncAutoScalingGroupName'
                                  -> Text -- ^ 'dncTopicARN'
                                  -> DeleteNotificationConfiguration
mkDeleteNotificationConfiguration p1 p2 = DeleteNotificationConfiguration
    { _dncAutoScalingGroupName = p1
    , _dncTopicARN = p2
    }

-- | The name of the Auto Scaling group.
dncAutoScalingGroupName :: Lens' DeleteNotificationConfiguration Text
dncAutoScalingGroupName =
    lens _dncAutoScalingGroupName
         (\s a -> s { _dncAutoScalingGroupName = a })

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic.
dncTopicARN :: Lens' DeleteNotificationConfiguration Text
dncTopicARN = lens _dncTopicARN (\s a -> s { _dncTopicARN = a })

instance ToQuery DeleteNotificationConfiguration where
    toQuery = genericQuery def

data DeleteNotificationConfigurationResponse = DeleteNotificationConfigurationResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteNotificationConfigurationResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteNotificationConfigurationResponse :: DeleteNotificationConfigurationResponse
mkDeleteNotificationConfigurationResponse = DeleteNotificationConfigurationResponse

instance AWSRequest DeleteNotificationConfiguration where
    type Sv DeleteNotificationConfiguration = AutoScaling
    type Rs DeleteNotificationConfiguration = DeleteNotificationConfigurationResponse

    request = post "DeleteNotificationConfiguration"
    response _ = nullaryResponse DeleteNotificationConfigurationResponse
