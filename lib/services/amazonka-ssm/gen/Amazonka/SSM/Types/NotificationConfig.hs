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
-- Module      : Amazonka.SSM.Types.NotificationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.NotificationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.NotificationEvent
import Amazonka.SSM.Types.NotificationType

-- | Configurations for sending notifications.
--
-- /See:/ 'newNotificationConfig' smart constructor.
data NotificationConfig = NotificationConfig'
  { -- | An Amazon Resource Name (ARN) for an Amazon Simple Notification Service
    -- (Amazon SNS) topic. Run Command pushes notifications about command
    -- status changes to this topic.
    notificationArn :: Prelude.Maybe Prelude.Text,
    -- | The different events for which you can receive notifications. To learn
    -- more about these events, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitoring-sns-notifications.html Monitoring Systems Manager status changes using Amazon SNS notifications>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    notificationEvents :: Prelude.Maybe [NotificationEvent],
    -- | The type of notification.
    --
    -- -   @Command@: Receive notification when the status of a command
    --     changes.
    --
    -- -   @Invocation@: For commands sent to multiple managed nodes, receive
    --     notification on a per-node basis when the status of a command
    --     changes.
    notificationType :: Prelude.Maybe NotificationType
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
-- 'notificationEvents', 'notificationConfig_notificationEvents' - The different events for which you can receive notifications. To learn
-- more about these events, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitoring-sns-notifications.html Monitoring Systems Manager status changes using Amazon SNS notifications>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'notificationType', 'notificationConfig_notificationType' - The type of notification.
--
-- -   @Command@: Receive notification when the status of a command
--     changes.
--
-- -   @Invocation@: For commands sent to multiple managed nodes, receive
--     notification on a per-node basis when the status of a command
--     changes.
newNotificationConfig ::
  NotificationConfig
newNotificationConfig =
  NotificationConfig'
    { notificationArn =
        Prelude.Nothing,
      notificationEvents = Prelude.Nothing,
      notificationType = Prelude.Nothing
    }

-- | An Amazon Resource Name (ARN) for an Amazon Simple Notification Service
-- (Amazon SNS) topic. Run Command pushes notifications about command
-- status changes to this topic.
notificationConfig_notificationArn :: Lens.Lens' NotificationConfig (Prelude.Maybe Prelude.Text)
notificationConfig_notificationArn = Lens.lens (\NotificationConfig' {notificationArn} -> notificationArn) (\s@NotificationConfig' {} a -> s {notificationArn = a} :: NotificationConfig)

-- | The different events for which you can receive notifications. To learn
-- more about these events, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitoring-sns-notifications.html Monitoring Systems Manager status changes using Amazon SNS notifications>
-- in the /Amazon Web Services Systems Manager User Guide/.
notificationConfig_notificationEvents :: Lens.Lens' NotificationConfig (Prelude.Maybe [NotificationEvent])
notificationConfig_notificationEvents = Lens.lens (\NotificationConfig' {notificationEvents} -> notificationEvents) (\s@NotificationConfig' {} a -> s {notificationEvents = a} :: NotificationConfig) Prelude.. Lens.mapping Lens.coerced

-- | The type of notification.
--
-- -   @Command@: Receive notification when the status of a command
--     changes.
--
-- -   @Invocation@: For commands sent to multiple managed nodes, receive
--     notification on a per-node basis when the status of a command
--     changes.
notificationConfig_notificationType :: Lens.Lens' NotificationConfig (Prelude.Maybe NotificationType)
notificationConfig_notificationType = Lens.lens (\NotificationConfig' {notificationType} -> notificationType) (\s@NotificationConfig' {} a -> s {notificationType = a} :: NotificationConfig)

instance Data.FromJSON NotificationConfig where
  parseJSON =
    Data.withObject
      "NotificationConfig"
      ( \x ->
          NotificationConfig'
            Prelude.<$> (x Data..:? "NotificationArn")
            Prelude.<*> ( x Data..:? "NotificationEvents"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "NotificationType")
      )

instance Prelude.Hashable NotificationConfig where
  hashWithSalt _salt NotificationConfig' {..} =
    _salt `Prelude.hashWithSalt` notificationArn
      `Prelude.hashWithSalt` notificationEvents
      `Prelude.hashWithSalt` notificationType

instance Prelude.NFData NotificationConfig where
  rnf NotificationConfig' {..} =
    Prelude.rnf notificationArn
      `Prelude.seq` Prelude.rnf notificationEvents
      `Prelude.seq` Prelude.rnf notificationType

instance Data.ToJSON NotificationConfig where
  toJSON NotificationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NotificationArn" Data..=)
              Prelude.<$> notificationArn,
            ("NotificationEvents" Data..=)
              Prelude.<$> notificationEvents,
            ("NotificationType" Data..=)
              Prelude.<$> notificationType
          ]
      )
