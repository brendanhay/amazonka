{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.PutNotificationConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an Auto Scaling group to send notifications when specified
-- events take place. Subscribers to this topic can have messages for
-- events delivered to an endpoint such as a web server or email address.
--
-- For more information see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/ASGettingNotifications.html Getting Notifications When Your Auto Scaling Group Changes>
-- in the /Auto Scaling Developer Guide/.
--
-- This configuration overwrites an existing configuration.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_PutNotificationConfiguration.html AWS API Reference> for PutNotificationConfiguration.
module Network.AWS.AutoScaling.PutNotificationConfiguration
    (
    -- * Creating a Request
      putNotificationConfiguration
    , PutNotificationConfiguration
    -- * Request Lenses
    , pncAutoScalingGroupName
    , pncTopicARN
    , pncNotificationTypes

    -- * Destructuring the Response
    , putNotificationConfigurationResponse
    , PutNotificationConfigurationResponse
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.AutoScaling.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putNotificationConfiguration' smart constructor.
data PutNotificationConfiguration = PutNotificationConfiguration'
    { _pncAutoScalingGroupName :: !Text
    , _pncTopicARN             :: !Text
    , _pncNotificationTypes    :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutNotificationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pncAutoScalingGroupName'
--
-- * 'pncTopicARN'
--
-- * 'pncNotificationTypes'
putNotificationConfiguration
    :: Text -- ^ 'pncAutoScalingGroupName'
    -> Text -- ^ 'pncTopicARN'
    -> PutNotificationConfiguration
putNotificationConfiguration pAutoScalingGroupName_ pTopicARN_ =
    PutNotificationConfiguration'
    { _pncAutoScalingGroupName = pAutoScalingGroupName_
    , _pncTopicARN = pTopicARN_
    , _pncNotificationTypes = mempty
    }

-- | The name of the Auto Scaling group.
pncAutoScalingGroupName :: Lens' PutNotificationConfiguration Text
pncAutoScalingGroupName = lens _pncAutoScalingGroupName (\ s a -> s{_pncAutoScalingGroupName = a});

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic.
pncTopicARN :: Lens' PutNotificationConfiguration Text
pncTopicARN = lens _pncTopicARN (\ s a -> s{_pncTopicARN = a});

-- | The type of event that will cause the notification to be sent. For
-- details about notification types supported by Auto Scaling, see
-- DescribeAutoScalingNotificationTypes.
pncNotificationTypes :: Lens' PutNotificationConfiguration [Text]
pncNotificationTypes = lens _pncNotificationTypes (\ s a -> s{_pncNotificationTypes = a}) . _Coerce;

instance AWSRequest PutNotificationConfiguration
         where
        type Sv PutNotificationConfiguration = AutoScaling
        type Rs PutNotificationConfiguration =
             PutNotificationConfigurationResponse
        request = postQuery
        response
          = receiveNull PutNotificationConfigurationResponse'

instance ToHeaders PutNotificationConfiguration where
        toHeaders = const mempty

instance ToPath PutNotificationConfiguration where
        toPath = const "/"

instance ToQuery PutNotificationConfiguration where
        toQuery PutNotificationConfiguration'{..}
          = mconcat
              ["Action" =:
                 ("PutNotificationConfiguration" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "AutoScalingGroupName" =: _pncAutoScalingGroupName,
               "TopicARN" =: _pncTopicARN,
               "NotificationTypes" =:
                 toQueryList "member" _pncNotificationTypes]

-- | /See:/ 'putNotificationConfigurationResponse' smart constructor.
data PutNotificationConfigurationResponse =
    PutNotificationConfigurationResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutNotificationConfigurationResponse' with the minimum fields required to make a request.
--
putNotificationConfigurationResponse
    :: PutNotificationConfigurationResponse
putNotificationConfigurationResponse = PutNotificationConfigurationResponse'
