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
    ncNotificationEvents,
    ncNotificationType,
    ncNotificationARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.NotificationEvent
import Network.AWS.SSM.Types.NotificationType

-- | Configurations for sending notifications.
--
-- /See:/ 'mkNotificationConfig' smart constructor.
data NotificationConfig = NotificationConfig'
  { -- | The different events for which you can receive notifications. These events include the following: All (events), InProgress, Success, TimedOut, Cancelled, Failed. To learn more about these events, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitoring-sns-notifications.html Monitoring Systems Manager status changes using Amazon SNS notifications> in the /AWS Systems Manager User Guide/ .
    notificationEvents :: Lude.Maybe [NotificationEvent],
    -- | Command: Receive notification when the status of a command changes. Invocation: For commands sent to multiple instances, receive notification on a per-instance basis when the status of a command changes.
    notificationType :: Lude.Maybe NotificationType,
    -- | An Amazon Resource Name (ARN) for an Amazon Simple Notification Service (Amazon SNS) topic. Run Command pushes notifications about command status changes to this topic.
    notificationARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotificationConfig' with the minimum fields required to make a request.
--
-- * 'notificationEvents' - The different events for which you can receive notifications. These events include the following: All (events), InProgress, Success, TimedOut, Cancelled, Failed. To learn more about these events, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitoring-sns-notifications.html Monitoring Systems Manager status changes using Amazon SNS notifications> in the /AWS Systems Manager User Guide/ .
-- * 'notificationType' - Command: Receive notification when the status of a command changes. Invocation: For commands sent to multiple instances, receive notification on a per-instance basis when the status of a command changes.
-- * 'notificationARN' - An Amazon Resource Name (ARN) for an Amazon Simple Notification Service (Amazon SNS) topic. Run Command pushes notifications about command status changes to this topic.
mkNotificationConfig ::
  NotificationConfig
mkNotificationConfig =
  NotificationConfig'
    { notificationEvents = Lude.Nothing,
      notificationType = Lude.Nothing,
      notificationARN = Lude.Nothing
    }

-- | The different events for which you can receive notifications. These events include the following: All (events), InProgress, Success, TimedOut, Cancelled, Failed. To learn more about these events, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitoring-sns-notifications.html Monitoring Systems Manager status changes using Amazon SNS notifications> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'notificationEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncNotificationEvents :: Lens.Lens' NotificationConfig (Lude.Maybe [NotificationEvent])
ncNotificationEvents = Lens.lens (notificationEvents :: NotificationConfig -> Lude.Maybe [NotificationEvent]) (\s a -> s {notificationEvents = a} :: NotificationConfig)
{-# DEPRECATED ncNotificationEvents "Use generic-lens or generic-optics with 'notificationEvents' instead." #-}

-- | Command: Receive notification when the status of a command changes. Invocation: For commands sent to multiple instances, receive notification on a per-instance basis when the status of a command changes.
--
-- /Note:/ Consider using 'notificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncNotificationType :: Lens.Lens' NotificationConfig (Lude.Maybe NotificationType)
ncNotificationType = Lens.lens (notificationType :: NotificationConfig -> Lude.Maybe NotificationType) (\s a -> s {notificationType = a} :: NotificationConfig)
{-# DEPRECATED ncNotificationType "Use generic-lens or generic-optics with 'notificationType' instead." #-}

-- | An Amazon Resource Name (ARN) for an Amazon Simple Notification Service (Amazon SNS) topic. Run Command pushes notifications about command status changes to this topic.
--
-- /Note:/ Consider using 'notificationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncNotificationARN :: Lens.Lens' NotificationConfig (Lude.Maybe Lude.Text)
ncNotificationARN = Lens.lens (notificationARN :: NotificationConfig -> Lude.Maybe Lude.Text) (\s a -> s {notificationARN = a} :: NotificationConfig)
{-# DEPRECATED ncNotificationARN "Use generic-lens or generic-optics with 'notificationARN' instead." #-}

instance Lude.FromJSON NotificationConfig where
  parseJSON =
    Lude.withObject
      "NotificationConfig"
      ( \x ->
          NotificationConfig'
            Lude.<$> (x Lude..:? "NotificationEvents" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NotificationType")
            Lude.<*> (x Lude..:? "NotificationArn")
      )

instance Lude.ToJSON NotificationConfig where
  toJSON NotificationConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NotificationEvents" Lude..=) Lude.<$> notificationEvents,
            ("NotificationType" Lude..=) Lude.<$> notificationType,
            ("NotificationArn" Lude..=) Lude.<$> notificationARN
          ]
      )
