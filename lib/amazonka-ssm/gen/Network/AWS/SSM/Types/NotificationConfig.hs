{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.NotificationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.NotificationConfig
  ( NotificationConfig (..),

    -- * Smart constructor
    mkNotificationConfig,

    -- * Lenses
    ncNotificationArn,
    ncNotificationEvents,
    ncNotificationType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.NotificationArn as Types
import qualified Network.AWS.SSM.Types.NotificationEvent as Types
import qualified Network.AWS.SSM.Types.NotificationType as Types

-- | Configurations for sending notifications.
--
-- /See:/ 'mkNotificationConfig' smart constructor.
data NotificationConfig = NotificationConfig'
  { -- | An Amazon Resource Name (ARN) for an Amazon Simple Notification Service (Amazon SNS) topic. Run Command pushes notifications about command status changes to this topic.
    notificationArn :: Core.Maybe Types.NotificationArn,
    -- | The different events for which you can receive notifications. These events include the following: All (events), InProgress, Success, TimedOut, Cancelled, Failed. To learn more about these events, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitoring-sns-notifications.html Monitoring Systems Manager status changes using Amazon SNS notifications> in the /AWS Systems Manager User Guide/ .
    notificationEvents :: Core.Maybe [Types.NotificationEvent],
    -- | Command: Receive notification when the status of a command changes. Invocation: For commands sent to multiple instances, receive notification on a per-instance basis when the status of a command changes.
    notificationType :: Core.Maybe Types.NotificationType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NotificationConfig' value with any optional fields omitted.
mkNotificationConfig ::
  NotificationConfig
mkNotificationConfig =
  NotificationConfig'
    { notificationArn = Core.Nothing,
      notificationEvents = Core.Nothing,
      notificationType = Core.Nothing
    }

-- | An Amazon Resource Name (ARN) for an Amazon Simple Notification Service (Amazon SNS) topic. Run Command pushes notifications about command status changes to this topic.
--
-- /Note:/ Consider using 'notificationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncNotificationArn :: Lens.Lens' NotificationConfig (Core.Maybe Types.NotificationArn)
ncNotificationArn = Lens.field @"notificationArn"
{-# DEPRECATED ncNotificationArn "Use generic-lens or generic-optics with 'notificationArn' instead." #-}

-- | The different events for which you can receive notifications. These events include the following: All (events), InProgress, Success, TimedOut, Cancelled, Failed. To learn more about these events, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitoring-sns-notifications.html Monitoring Systems Manager status changes using Amazon SNS notifications> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'notificationEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncNotificationEvents :: Lens.Lens' NotificationConfig (Core.Maybe [Types.NotificationEvent])
ncNotificationEvents = Lens.field @"notificationEvents"
{-# DEPRECATED ncNotificationEvents "Use generic-lens or generic-optics with 'notificationEvents' instead." #-}

-- | Command: Receive notification when the status of a command changes. Invocation: For commands sent to multiple instances, receive notification on a per-instance basis when the status of a command changes.
--
-- /Note:/ Consider using 'notificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncNotificationType :: Lens.Lens' NotificationConfig (Core.Maybe Types.NotificationType)
ncNotificationType = Lens.field @"notificationType"
{-# DEPRECATED ncNotificationType "Use generic-lens or generic-optics with 'notificationType' instead." #-}

instance Core.FromJSON NotificationConfig where
  toJSON NotificationConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("NotificationArn" Core..=) Core.<$> notificationArn,
            ("NotificationEvents" Core..=) Core.<$> notificationEvents,
            ("NotificationType" Core..=) Core.<$> notificationType
          ]
      )

instance Core.FromJSON NotificationConfig where
  parseJSON =
    Core.withObject "NotificationConfig" Core.$
      \x ->
        NotificationConfig'
          Core.<$> (x Core..:? "NotificationArn")
          Core.<*> (x Core..:? "NotificationEvents")
          Core.<*> (x Core..:? "NotificationType")
