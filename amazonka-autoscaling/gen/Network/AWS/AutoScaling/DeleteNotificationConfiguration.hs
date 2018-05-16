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
-- Module      : Network.AWS.AutoScaling.DeleteNotificationConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified notification.
--
--
module Network.AWS.AutoScaling.DeleteNotificationConfiguration
    (
    -- * Creating a Request
      deleteNotificationConfiguration
    , DeleteNotificationConfiguration
    -- * Request Lenses
    , dncAutoScalingGroupName
    , dncTopicARN

    -- * Destructuring the Response
    , deleteNotificationConfigurationResponse
    , DeleteNotificationConfigurationResponse
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteNotificationConfiguration' smart constructor.
data DeleteNotificationConfiguration = DeleteNotificationConfiguration'
  { _dncAutoScalingGroupName :: !Text
  , _dncTopicARN             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNotificationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dncAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'dncTopicARN' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) topic.
deleteNotificationConfiguration
    :: Text -- ^ 'dncAutoScalingGroupName'
    -> Text -- ^ 'dncTopicARN'
    -> DeleteNotificationConfiguration
deleteNotificationConfiguration pAutoScalingGroupName_ pTopicARN_ =
  DeleteNotificationConfiguration'
    { _dncAutoScalingGroupName = pAutoScalingGroupName_
    , _dncTopicARN = pTopicARN_
    }


-- | The name of the Auto Scaling group.
dncAutoScalingGroupName :: Lens' DeleteNotificationConfiguration Text
dncAutoScalingGroupName = lens _dncAutoScalingGroupName (\ s a -> s{_dncAutoScalingGroupName = a})

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) topic.
dncTopicARN :: Lens' DeleteNotificationConfiguration Text
dncTopicARN = lens _dncTopicARN (\ s a -> s{_dncTopicARN = a})

instance AWSRequest DeleteNotificationConfiguration
         where
        type Rs DeleteNotificationConfiguration =
             DeleteNotificationConfigurationResponse
        request = postQuery autoScaling
        response
          = receiveNull
              DeleteNotificationConfigurationResponse'

instance Hashable DeleteNotificationConfiguration
         where

instance NFData DeleteNotificationConfiguration where

instance ToHeaders DeleteNotificationConfiguration
         where
        toHeaders = const mempty

instance ToPath DeleteNotificationConfiguration where
        toPath = const "/"

instance ToQuery DeleteNotificationConfiguration
         where
        toQuery DeleteNotificationConfiguration'{..}
          = mconcat
              ["Action" =:
                 ("DeleteNotificationConfiguration" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "AutoScalingGroupName" =: _dncAutoScalingGroupName,
               "TopicARN" =: _dncTopicARN]

-- | /See:/ 'deleteNotificationConfigurationResponse' smart constructor.
data DeleteNotificationConfigurationResponse =
  DeleteNotificationConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNotificationConfigurationResponse' with the minimum fields required to make a request.
--
deleteNotificationConfigurationResponse
    :: DeleteNotificationConfigurationResponse
deleteNotificationConfigurationResponse =
  DeleteNotificationConfigurationResponse'


instance NFData
           DeleteNotificationConfigurationResponse
         where
