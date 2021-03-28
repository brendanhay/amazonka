{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.NotificationConfiguration
  ( NotificationConfiguration (..)
  -- * Smart constructor
  , mkNotificationConfiguration
  -- * Lenses
  , ncAutoScalingGroupName
  , ncNotificationType
  , ncTopicARN
  ) where

import qualified Network.AWS.AutoScaling.Types.AutoScalingGroupName as Types
import qualified Network.AWS.AutoScaling.Types.NotificationType as Types
import qualified Network.AWS.AutoScaling.Types.TopicARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a notification.
--
-- /See:/ 'mkNotificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { autoScalingGroupName :: Core.Maybe Types.AutoScalingGroupName
    -- ^ The name of the Auto Scaling group.
  , notificationType :: Core.Maybe Types.NotificationType
    -- ^ One of the following event notification types:
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
  , topicARN :: Core.Maybe Types.TopicARN
    -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (Amazon SNS) topic.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NotificationConfiguration' value with any optional fields omitted.
mkNotificationConfiguration
    :: NotificationConfiguration
mkNotificationConfiguration
  = NotificationConfiguration'{autoScalingGroupName = Core.Nothing,
                               notificationType = Core.Nothing, topicARN = Core.Nothing}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncAutoScalingGroupName :: Lens.Lens' NotificationConfiguration (Core.Maybe Types.AutoScalingGroupName)
ncAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE ncAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

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
ncNotificationType :: Lens.Lens' NotificationConfiguration (Core.Maybe Types.NotificationType)
ncNotificationType = Lens.field @"notificationType"
{-# INLINEABLE ncNotificationType #-}
{-# DEPRECATED notificationType "Use generic-lens or generic-optics with 'notificationType' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (Amazon SNS) topic.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncTopicARN :: Lens.Lens' NotificationConfiguration (Core.Maybe Types.TopicARN)
ncTopicARN = Lens.field @"topicARN"
{-# INLINEABLE ncTopicARN #-}
{-# DEPRECATED topicARN "Use generic-lens or generic-optics with 'topicARN' instead"  #-}

instance Core.FromXML NotificationConfiguration where
        parseXML x
          = NotificationConfiguration' Core.<$>
              (x Core..@? "AutoScalingGroupName") Core.<*>
                x Core..@? "NotificationType"
                Core.<*> x Core..@? "TopicARN"
