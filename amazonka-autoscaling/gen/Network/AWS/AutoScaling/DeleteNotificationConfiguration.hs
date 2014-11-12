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
      DeleteNotificationConfigurationType
    -- ** Request constructor
    , deleteNotificationConfigurationType
    -- ** Request lenses
    , dnctAutoScalingGroupName
    , dnctTopicARN

    -- * Response
    , DeleteNotificationConfigurationResponse
    -- ** Response constructor
    , deleteNotificationConfigurationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DeleteNotificationConfigurationType = DeleteNotificationConfigurationType
    { _dnctAutoScalingGroupName :: Text
    , _dnctTopicARN             :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteNotificationConfigurationType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dnctAutoScalingGroupName' @::@ 'Text'
--
-- * 'dnctTopicARN' @::@ 'Text'
--
deleteNotificationConfigurationType :: Text -- ^ 'dnctAutoScalingGroupName'
                                    -> Text -- ^ 'dnctTopicARN'
                                    -> DeleteNotificationConfigurationType
deleteNotificationConfigurationType p1 p2 = DeleteNotificationConfigurationType
    { _dnctAutoScalingGroupName = p1
    , _dnctTopicARN             = p2
    }

-- | The name of the Auto Scaling group.
dnctAutoScalingGroupName :: Lens' DeleteNotificationConfigurationType Text
dnctAutoScalingGroupName =
    lens _dnctAutoScalingGroupName
        (\s a -> s { _dnctAutoScalingGroupName = a })

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic.
dnctTopicARN :: Lens' DeleteNotificationConfigurationType Text
dnctTopicARN = lens _dnctTopicARN (\s a -> s { _dnctTopicARN = a })

instance ToQuery DeleteNotificationConfigurationType

instance ToPath DeleteNotificationConfigurationType where
    toPath = const "/"

data DeleteNotificationConfigurationResponse = DeleteNotificationConfigurationResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteNotificationConfigurationResponse' constructor.
deleteNotificationConfigurationResponse :: DeleteNotificationConfigurationResponse
deleteNotificationConfigurationResponse = DeleteNotificationConfigurationResponse

instance FromXML DeleteNotificationConfigurationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteNotificationConfigurationResponse"

instance AWSRequest DeleteNotificationConfigurationType where
    type Sv DeleteNotificationConfigurationType = AutoScaling
    type Rs DeleteNotificationConfigurationType = DeleteNotificationConfigurationResponse

    request  = post "DeleteNotificationConfiguration"
    response = nullaryResponse DeleteNotificationConfigurationResponse
