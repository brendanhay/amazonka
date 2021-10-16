{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.NotificationConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.NotificationConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.NotificationEvent
import Network.AWS.SSM.Types.NotificationType

-- | Configurations for sending notifications.
--
-- /See:/ 'newNotificationConfig' smart constructor.
data NotificationConfig = NotificationConfig'
  { -- | An Amazon Resource Name (ARN) for an Amazon Simple Notification Service
    -- (Amazon SNS) topic. Run Command pushes notifications about command
    -- status changes to this topic.
    notificationArn :: Prelude.Maybe Prelude.Text,
    -- | The type of notification.
    --
    -- -   @Command@: Receive notification when the status of a command
    --     changes.
    --
    -- -   @Invocation@: For commands sent to multiple instances, receive
    --     notification on a per-instance basis when the status of a command
    --     changes.
    notificationType :: Prelude.Maybe NotificationType,
    -- | The different events for which you can receive notifications. To learn
    -- more about these events, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitoring-sns-notifications.html Monitoring Systems Manager status changes using Amazon SNS notifications>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    notificationEvents :: Prelude.Maybe [NotificationEvent]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationArn', 'notificationConfig_notificationArn' - An Amazon Resource Name (ARN) for an Amazon Simple Notification Service
-- (Amazon SNS) topic. Run Command pushes notifications about command
-- status changes to this topic.
--
-- 'notificationType', 'notificationConfig_notificationType' - The type of notification.
--
-- -   @Command@: Receive notification when the status of a command
--     changes.
--
-- -   @Invocation@: For commands sent to multiple instances, receive
--     notification on a per-instance basis when the status of a command
--     changes.
--
-- 'notificationEvents', 'notificationConfig_notificationEvents' - The different events for which you can receive notifications. To learn
-- more about these events, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitoring-sns-notifications.html Monitoring Systems Manager status changes using Amazon SNS notifications>
-- in the /Amazon Web Services Systems Manager User Guide/.
newNotificationConfig ::
  NotificationConfig
newNotificationConfig =
  NotificationConfig'
    { notificationArn =
        Prelude.Nothing,
      notificationType = Prelude.Nothing,
      notificationEvents = Prelude.Nothing
    }

-- | An Amazon Resource Name (ARN) for an Amazon Simple Notification Service
-- (Amazon SNS) topic. Run Command pushes notifications about command
-- status changes to this topic.
notificationConfig_notificationArn :: Lens.Lens' NotificationConfig (Prelude.Maybe Prelude.Text)
notificationConfig_notificationArn = Lens.lens (\NotificationConfig' {notificationArn} -> notificationArn) (\s@NotificationConfig' {} a -> s {notificationArn = a} :: NotificationConfig)

-- | The type of notification.
--
-- -   @Command@: Receive notification when the status of a command
--     changes.
--
-- -   @Invocation@: For commands sent to multiple instances, receive
--     notification on a per-instance basis when the status of a command
--     changes.
notificationConfig_notificationType :: Lens.Lens' NotificationConfig (Prelude.Maybe NotificationType)
notificationConfig_notificationType = Lens.lens (\NotificationConfig' {notificationType} -> notificationType) (\s@NotificationConfig' {} a -> s {notificationType = a} :: NotificationConfig)

-- | The different events for which you can receive notifications. To learn
-- more about these events, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitoring-sns-notifications.html Monitoring Systems Manager status changes using Amazon SNS notifications>
-- in the /Amazon Web Services Systems Manager User Guide/.
notificationConfig_notificationEvents :: Lens.Lens' NotificationConfig (Prelude.Maybe [NotificationEvent])
notificationConfig_notificationEvents = Lens.lens (\NotificationConfig' {notificationEvents} -> notificationEvents) (\s@NotificationConfig' {} a -> s {notificationEvents = a} :: NotificationConfig) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON NotificationConfig where
  parseJSON =
    Core.withObject
      "NotificationConfig"
      ( \x ->
          NotificationConfig'
            Prelude.<$> (x Core..:? "NotificationArn")
            Prelude.<*> (x Core..:? "NotificationType")
            Prelude.<*> ( x Core..:? "NotificationEvents"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable NotificationConfig

instance Prelude.NFData NotificationConfig

instance Core.ToJSON NotificationConfig where
  toJSON NotificationConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NotificationArn" Core..=)
              Prelude.<$> notificationArn,
            ("NotificationType" Core..=)
              Prelude.<$> notificationType,
            ("NotificationEvents" Core..=)
              Prelude.<$> notificationEvents
          ]
      )
