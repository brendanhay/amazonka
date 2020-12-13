{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.PutNotificationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an Auto Scaling group to send notifications when specified events take place. Subscribers to the specified topic can have messages delivered to an endpoint such as a web server or an email address.
--
-- This configuration overwrites any existing configuration.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ASGettingNotifications.html Getting Amazon SNS notifications when your Auto Scaling group scales> in the /Amazon EC2 Auto Scaling User Guide/ .
-- If you exceed your maximum limit of SNS topics, which is 10 per Auto Scaling group, the call fails.
module Network.AWS.AutoScaling.PutNotificationConfiguration
  ( -- * Creating a request
    PutNotificationConfiguration (..),
    mkPutNotificationConfiguration,

    -- ** Request lenses
    pncNotificationTypes,
    pncTopicARN,
    pncAutoScalingGroupName,

    -- * Destructuring the response
    PutNotificationConfigurationResponse (..),
    mkPutNotificationConfigurationResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutNotificationConfiguration' smart constructor.
data PutNotificationConfiguration = PutNotificationConfiguration'
  { -- | The type of event that causes the notification to be sent. To query the notification types supported by Amazon EC2 Auto Scaling, call the 'DescribeAutoScalingNotificationTypes' API.
    notificationTypes :: [Lude.Text],
    -- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (Amazon SNS) topic.
    topicARN :: Lude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutNotificationConfiguration' with the minimum fields required to make a request.
--
-- * 'notificationTypes' - The type of event that causes the notification to be sent. To query the notification types supported by Amazon EC2 Auto Scaling, call the 'DescribeAutoScalingNotificationTypes' API.
-- * 'topicARN' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (Amazon SNS) topic.
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
mkPutNotificationConfiguration ::
  -- | 'topicARN'
  Lude.Text ->
  -- | 'autoScalingGroupName'
  Lude.Text ->
  PutNotificationConfiguration
mkPutNotificationConfiguration pTopicARN_ pAutoScalingGroupName_ =
  PutNotificationConfiguration'
    { notificationTypes = Lude.mempty,
      topicARN = pTopicARN_,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | The type of event that causes the notification to be sent. To query the notification types supported by Amazon EC2 Auto Scaling, call the 'DescribeAutoScalingNotificationTypes' API.
--
-- /Note:/ Consider using 'notificationTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pncNotificationTypes :: Lens.Lens' PutNotificationConfiguration [Lude.Text]
pncNotificationTypes = Lens.lens (notificationTypes :: PutNotificationConfiguration -> [Lude.Text]) (\s a -> s {notificationTypes = a} :: PutNotificationConfiguration)
{-# DEPRECATED pncNotificationTypes "Use generic-lens or generic-optics with 'notificationTypes' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (Amazon SNS) topic.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pncTopicARN :: Lens.Lens' PutNotificationConfiguration Lude.Text
pncTopicARN = Lens.lens (topicARN :: PutNotificationConfiguration -> Lude.Text) (\s a -> s {topicARN = a} :: PutNotificationConfiguration)
{-# DEPRECATED pncTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pncAutoScalingGroupName :: Lens.Lens' PutNotificationConfiguration Lude.Text
pncAutoScalingGroupName = Lens.lens (autoScalingGroupName :: PutNotificationConfiguration -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: PutNotificationConfiguration)
{-# DEPRECATED pncAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Lude.AWSRequest PutNotificationConfiguration where
  type
    Rs PutNotificationConfiguration =
      PutNotificationConfigurationResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull PutNotificationConfigurationResponse'

instance Lude.ToHeaders PutNotificationConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutNotificationConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery PutNotificationConfiguration where
  toQuery PutNotificationConfiguration' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("PutNotificationConfiguration" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "NotificationTypes"
          Lude.=: Lude.toQueryList "member" notificationTypes,
        "TopicARN" Lude.=: topicARN,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]

-- | /See:/ 'mkPutNotificationConfigurationResponse' smart constructor.
data PutNotificationConfigurationResponse = PutNotificationConfigurationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutNotificationConfigurationResponse' with the minimum fields required to make a request.
mkPutNotificationConfigurationResponse ::
  PutNotificationConfigurationResponse
mkPutNotificationConfigurationResponse =
  PutNotificationConfigurationResponse'
