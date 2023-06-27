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
-- Module      : Amazonka.RolesAnywhere.Types.NotificationSetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.NotificationSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RolesAnywhere.Types.NotificationChannel
import Amazonka.RolesAnywhere.Types.NotificationEvent

-- | Customizable notification settings that will be applied to notification
-- events. IAM Roles Anywhere consumes these settings while notifying
-- across multiple channels - CloudWatch metrics, EventBridge, and Health
-- Dashboard.
--
-- /See:/ 'newNotificationSetting' smart constructor.
data NotificationSetting = NotificationSetting'
  { -- | The specified channel of notification. IAM Roles Anywhere uses
    -- CloudWatch metrics, EventBridge, and Health Dashboard to notify for an
    -- event.
    --
    -- In the absence of a specific channel, IAM Roles Anywhere applies this
    -- setting to \'ALL\' channels.
    channel :: Prelude.Maybe NotificationChannel,
    -- | The number of days before a notification event. This value is required
    -- for a notification setting that is enabled.
    threshold :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether the notification setting is enabled.
    enabled :: Prelude.Bool,
    -- | The event to which this notification setting is applied.
    event :: NotificationEvent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channel', 'notificationSetting_channel' - The specified channel of notification. IAM Roles Anywhere uses
-- CloudWatch metrics, EventBridge, and Health Dashboard to notify for an
-- event.
--
-- In the absence of a specific channel, IAM Roles Anywhere applies this
-- setting to \'ALL\' channels.
--
-- 'threshold', 'notificationSetting_threshold' - The number of days before a notification event. This value is required
-- for a notification setting that is enabled.
--
-- 'enabled', 'notificationSetting_enabled' - Indicates whether the notification setting is enabled.
--
-- 'event', 'notificationSetting_event' - The event to which this notification setting is applied.
newNotificationSetting ::
  -- | 'enabled'
  Prelude.Bool ->
  -- | 'event'
  NotificationEvent ->
  NotificationSetting
newNotificationSetting pEnabled_ pEvent_ =
  NotificationSetting'
    { channel = Prelude.Nothing,
      threshold = Prelude.Nothing,
      enabled = pEnabled_,
      event = pEvent_
    }

-- | The specified channel of notification. IAM Roles Anywhere uses
-- CloudWatch metrics, EventBridge, and Health Dashboard to notify for an
-- event.
--
-- In the absence of a specific channel, IAM Roles Anywhere applies this
-- setting to \'ALL\' channels.
notificationSetting_channel :: Lens.Lens' NotificationSetting (Prelude.Maybe NotificationChannel)
notificationSetting_channel = Lens.lens (\NotificationSetting' {channel} -> channel) (\s@NotificationSetting' {} a -> s {channel = a} :: NotificationSetting)

-- | The number of days before a notification event. This value is required
-- for a notification setting that is enabled.
notificationSetting_threshold :: Lens.Lens' NotificationSetting (Prelude.Maybe Prelude.Natural)
notificationSetting_threshold = Lens.lens (\NotificationSetting' {threshold} -> threshold) (\s@NotificationSetting' {} a -> s {threshold = a} :: NotificationSetting)

-- | Indicates whether the notification setting is enabled.
notificationSetting_enabled :: Lens.Lens' NotificationSetting Prelude.Bool
notificationSetting_enabled = Lens.lens (\NotificationSetting' {enabled} -> enabled) (\s@NotificationSetting' {} a -> s {enabled = a} :: NotificationSetting)

-- | The event to which this notification setting is applied.
notificationSetting_event :: Lens.Lens' NotificationSetting NotificationEvent
notificationSetting_event = Lens.lens (\NotificationSetting' {event} -> event) (\s@NotificationSetting' {} a -> s {event = a} :: NotificationSetting)

instance Prelude.Hashable NotificationSetting where
  hashWithSalt _salt NotificationSetting' {..} =
    _salt
      `Prelude.hashWithSalt` channel
      `Prelude.hashWithSalt` threshold
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` event

instance Prelude.NFData NotificationSetting where
  rnf NotificationSetting' {..} =
    Prelude.rnf channel
      `Prelude.seq` Prelude.rnf threshold
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf event

instance Data.ToJSON NotificationSetting where
  toJSON NotificationSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("channel" Data..=) Prelude.<$> channel,
            ("threshold" Data..=) Prelude.<$> threshold,
            Prelude.Just ("enabled" Data..= enabled),
            Prelude.Just ("event" Data..= event)
          ]
      )
