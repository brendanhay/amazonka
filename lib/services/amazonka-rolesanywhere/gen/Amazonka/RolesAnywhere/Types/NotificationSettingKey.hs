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
-- Module      : Amazonka.RolesAnywhere.Types.NotificationSettingKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.NotificationSettingKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RolesAnywhere.Types.NotificationChannel
import Amazonka.RolesAnywhere.Types.NotificationEvent

-- | A notification setting key to reset. A notification setting key includes
-- the event and the channel.
--
-- /See:/ 'newNotificationSettingKey' smart constructor.
data NotificationSettingKey = NotificationSettingKey'
  { -- | The specified channel of notification.
    channel :: Prelude.Maybe NotificationChannel,
    -- | The notification setting event to reset.
    event :: NotificationEvent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationSettingKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channel', 'notificationSettingKey_channel' - The specified channel of notification.
--
-- 'event', 'notificationSettingKey_event' - The notification setting event to reset.
newNotificationSettingKey ::
  -- | 'event'
  NotificationEvent ->
  NotificationSettingKey
newNotificationSettingKey pEvent_ =
  NotificationSettingKey'
    { channel = Prelude.Nothing,
      event = pEvent_
    }

-- | The specified channel of notification.
notificationSettingKey_channel :: Lens.Lens' NotificationSettingKey (Prelude.Maybe NotificationChannel)
notificationSettingKey_channel = Lens.lens (\NotificationSettingKey' {channel} -> channel) (\s@NotificationSettingKey' {} a -> s {channel = a} :: NotificationSettingKey)

-- | The notification setting event to reset.
notificationSettingKey_event :: Lens.Lens' NotificationSettingKey NotificationEvent
notificationSettingKey_event = Lens.lens (\NotificationSettingKey' {event} -> event) (\s@NotificationSettingKey' {} a -> s {event = a} :: NotificationSettingKey)

instance Prelude.Hashable NotificationSettingKey where
  hashWithSalt _salt NotificationSettingKey' {..} =
    _salt
      `Prelude.hashWithSalt` channel
      `Prelude.hashWithSalt` event

instance Prelude.NFData NotificationSettingKey where
  rnf NotificationSettingKey' {..} =
    Prelude.rnf channel `Prelude.seq` Prelude.rnf event

instance Data.ToJSON NotificationSettingKey where
  toJSON NotificationSettingKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("channel" Data..=) Prelude.<$> channel,
            Prelude.Just ("event" Data..= event)
          ]
      )
