-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.NotificationConfiguration
  ( NotificationConfiguration (..),

    -- * Smart constructor
    mkNotificationConfiguration,

    -- * Lenses
    ncTopicARN,
    ncAutoScalingGroupName,
    ncNotificationType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a notification.
--
-- /See:/ 'mkNotificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { topicARN ::
      Lude.Maybe Lude.Text,
    autoScalingGroupName ::
      Lude.Maybe Lude.Text,
    notificationType ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotificationConfiguration' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'notificationType' - One of the following event notification types:
--
--
--     * @autoscaling:EC2_INSTANCE_LAUNCH@
--
--
--     * @autoscaling:EC2_INSTANCE_LAUNCH_ERROR@
--
--
--     * @autoscaling:EC2_INSTANCE_TERMINATE@
--
--
--     * @autoscaling:EC2_INSTANCE_TERMINATE_ERROR@
--
--
--     * @autoscaling:TEST_NOTIFICATION@
--
--
-- * 'topicARN' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (Amazon SNS) topic.
mkNotificationConfiguration ::
  NotificationConfiguration
mkNotificationConfiguration =
  NotificationConfiguration'
    { topicARN = Lude.Nothing,
      autoScalingGroupName = Lude.Nothing,
      notificationType = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (Amazon SNS) topic.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncTopicARN :: Lens.Lens' NotificationConfiguration (Lude.Maybe Lude.Text)
ncTopicARN = Lens.lens (topicARN :: NotificationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {topicARN = a} :: NotificationConfiguration)
{-# DEPRECATED ncTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncAutoScalingGroupName :: Lens.Lens' NotificationConfiguration (Lude.Maybe Lude.Text)
ncAutoScalingGroupName = Lens.lens (autoScalingGroupName :: NotificationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {autoScalingGroupName = a} :: NotificationConfiguration)
{-# DEPRECATED ncAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | One of the following event notification types:
--
--
--     * @autoscaling:EC2_INSTANCE_LAUNCH@
--
--
--     * @autoscaling:EC2_INSTANCE_LAUNCH_ERROR@
--
--
--     * @autoscaling:EC2_INSTANCE_TERMINATE@
--
--
--     * @autoscaling:EC2_INSTANCE_TERMINATE_ERROR@
--
--
--     * @autoscaling:TEST_NOTIFICATION@
--
--
--
-- /Note:/ Consider using 'notificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncNotificationType :: Lens.Lens' NotificationConfiguration (Lude.Maybe Lude.Text)
ncNotificationType = Lens.lens (notificationType :: NotificationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {notificationType = a} :: NotificationConfiguration)
{-# DEPRECATED ncNotificationType "Use generic-lens or generic-optics with 'notificationType' instead." #-}

instance Lude.FromXML NotificationConfiguration where
  parseXML x =
    NotificationConfiguration'
      Lude.<$> (x Lude..@? "TopicARN")
      Lude.<*> (x Lude..@? "AutoScalingGroupName")
      Lude.<*> (x Lude..@? "NotificationType")
