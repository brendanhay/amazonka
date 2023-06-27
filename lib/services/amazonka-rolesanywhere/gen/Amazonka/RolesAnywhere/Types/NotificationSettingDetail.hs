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
-- Module      : Amazonka.RolesAnywhere.Types.NotificationSettingDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.NotificationSettingDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RolesAnywhere.Types.NotificationChannel
import Amazonka.RolesAnywhere.Types.NotificationEvent

-- | The state of a notification setting.
--
-- A notification setting includes information such as event name,
-- threshold, status of the notification setting, and the channel to
-- notify.
--
-- /See:/ 'newNotificationSettingDetail' smart constructor.
data NotificationSettingDetail = NotificationSettingDetail'
  { -- | The specified channel of notification. IAM Roles Anywhere uses
    -- CloudWatch metrics, EventBridge, and Health Dashboard to notify for an
    -- event.
    --
    -- In the absence of a specific channel, IAM Roles Anywhere applies this
    -- setting to \'ALL\' channels.
    channel :: Prelude.Maybe NotificationChannel,
    -- | The principal that configured the notification setting. For default
    -- settings configured by IAM Roles Anywhere, the value is
    -- @rolesanywhere.amazonaws.com@, and for customized notifications
    -- settings, it is the respective account ID.
    configuredBy :: Prelude.Maybe Prelude.Text,
    -- | The number of days before a notification event.
    threshold :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether the notification setting is enabled.
    enabled :: Prelude.Bool,
    -- | The event to which this notification setting is applied.
    event :: NotificationEvent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationSettingDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channel', 'notificationSettingDetail_channel' - The specified channel of notification. IAM Roles Anywhere uses
-- CloudWatch metrics, EventBridge, and Health Dashboard to notify for an
-- event.
--
-- In the absence of a specific channel, IAM Roles Anywhere applies this
-- setting to \'ALL\' channels.
--
-- 'configuredBy', 'notificationSettingDetail_configuredBy' - The principal that configured the notification setting. For default
-- settings configured by IAM Roles Anywhere, the value is
-- @rolesanywhere.amazonaws.com@, and for customized notifications
-- settings, it is the respective account ID.
--
-- 'threshold', 'notificationSettingDetail_threshold' - The number of days before a notification event.
--
-- 'enabled', 'notificationSettingDetail_enabled' - Indicates whether the notification setting is enabled.
--
-- 'event', 'notificationSettingDetail_event' - The event to which this notification setting is applied.
newNotificationSettingDetail ::
  -- | 'enabled'
  Prelude.Bool ->
  -- | 'event'
  NotificationEvent ->
  NotificationSettingDetail
newNotificationSettingDetail pEnabled_ pEvent_ =
  NotificationSettingDetail'
    { channel =
        Prelude.Nothing,
      configuredBy = Prelude.Nothing,
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
notificationSettingDetail_channel :: Lens.Lens' NotificationSettingDetail (Prelude.Maybe NotificationChannel)
notificationSettingDetail_channel = Lens.lens (\NotificationSettingDetail' {channel} -> channel) (\s@NotificationSettingDetail' {} a -> s {channel = a} :: NotificationSettingDetail)

-- | The principal that configured the notification setting. For default
-- settings configured by IAM Roles Anywhere, the value is
-- @rolesanywhere.amazonaws.com@, and for customized notifications
-- settings, it is the respective account ID.
notificationSettingDetail_configuredBy :: Lens.Lens' NotificationSettingDetail (Prelude.Maybe Prelude.Text)
notificationSettingDetail_configuredBy = Lens.lens (\NotificationSettingDetail' {configuredBy} -> configuredBy) (\s@NotificationSettingDetail' {} a -> s {configuredBy = a} :: NotificationSettingDetail)

-- | The number of days before a notification event.
notificationSettingDetail_threshold :: Lens.Lens' NotificationSettingDetail (Prelude.Maybe Prelude.Natural)
notificationSettingDetail_threshold = Lens.lens (\NotificationSettingDetail' {threshold} -> threshold) (\s@NotificationSettingDetail' {} a -> s {threshold = a} :: NotificationSettingDetail)

-- | Indicates whether the notification setting is enabled.
notificationSettingDetail_enabled :: Lens.Lens' NotificationSettingDetail Prelude.Bool
notificationSettingDetail_enabled = Lens.lens (\NotificationSettingDetail' {enabled} -> enabled) (\s@NotificationSettingDetail' {} a -> s {enabled = a} :: NotificationSettingDetail)

-- | The event to which this notification setting is applied.
notificationSettingDetail_event :: Lens.Lens' NotificationSettingDetail NotificationEvent
notificationSettingDetail_event = Lens.lens (\NotificationSettingDetail' {event} -> event) (\s@NotificationSettingDetail' {} a -> s {event = a} :: NotificationSettingDetail)

instance Data.FromJSON NotificationSettingDetail where
  parseJSON =
    Data.withObject
      "NotificationSettingDetail"
      ( \x ->
          NotificationSettingDetail'
            Prelude.<$> (x Data..:? "channel")
            Prelude.<*> (x Data..:? "configuredBy")
            Prelude.<*> (x Data..:? "threshold")
            Prelude.<*> (x Data..: "enabled")
            Prelude.<*> (x Data..: "event")
      )

instance Prelude.Hashable NotificationSettingDetail where
  hashWithSalt _salt NotificationSettingDetail' {..} =
    _salt
      `Prelude.hashWithSalt` channel
      `Prelude.hashWithSalt` configuredBy
      `Prelude.hashWithSalt` threshold
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` event

instance Prelude.NFData NotificationSettingDetail where
  rnf NotificationSettingDetail' {..} =
    Prelude.rnf channel
      `Prelude.seq` Prelude.rnf configuredBy
      `Prelude.seq` Prelude.rnf threshold
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf event
