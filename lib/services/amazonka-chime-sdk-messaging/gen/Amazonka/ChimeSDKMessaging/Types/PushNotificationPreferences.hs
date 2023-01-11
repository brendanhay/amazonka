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
-- Module      : Amazonka.ChimeSDKMessaging.Types.PushNotificationPreferences
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.PushNotificationPreferences where

import Amazonka.ChimeSDKMessaging.Types.AllowNotifications
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The channel membership preferences for push notification.
--
-- /See:/ 'newPushNotificationPreferences' smart constructor.
data PushNotificationPreferences = PushNotificationPreferences'
  { -- | The simple JSON object used to send a subset of a push notification to
    -- the requested member.
    filterRule :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Enum value that indicates which push notifications to send to the
    -- requested member of a channel. @ALL@ sends all push notifications,
    -- @NONE@ sends no push notifications, @FILTERED@ sends only filtered push
    -- notifications.
    allowNotifications :: AllowNotifications
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PushNotificationPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterRule', 'pushNotificationPreferences_filterRule' - The simple JSON object used to send a subset of a push notification to
-- the requested member.
--
-- 'allowNotifications', 'pushNotificationPreferences_allowNotifications' - Enum value that indicates which push notifications to send to the
-- requested member of a channel. @ALL@ sends all push notifications,
-- @NONE@ sends no push notifications, @FILTERED@ sends only filtered push
-- notifications.
newPushNotificationPreferences ::
  -- | 'allowNotifications'
  AllowNotifications ->
  PushNotificationPreferences
newPushNotificationPreferences pAllowNotifications_ =
  PushNotificationPreferences'
    { filterRule =
        Prelude.Nothing,
      allowNotifications = pAllowNotifications_
    }

-- | The simple JSON object used to send a subset of a push notification to
-- the requested member.
pushNotificationPreferences_filterRule :: Lens.Lens' PushNotificationPreferences (Prelude.Maybe Prelude.Text)
pushNotificationPreferences_filterRule = Lens.lens (\PushNotificationPreferences' {filterRule} -> filterRule) (\s@PushNotificationPreferences' {} a -> s {filterRule = a} :: PushNotificationPreferences) Prelude.. Lens.mapping Data._Sensitive

-- | Enum value that indicates which push notifications to send to the
-- requested member of a channel. @ALL@ sends all push notifications,
-- @NONE@ sends no push notifications, @FILTERED@ sends only filtered push
-- notifications.
pushNotificationPreferences_allowNotifications :: Lens.Lens' PushNotificationPreferences AllowNotifications
pushNotificationPreferences_allowNotifications = Lens.lens (\PushNotificationPreferences' {allowNotifications} -> allowNotifications) (\s@PushNotificationPreferences' {} a -> s {allowNotifications = a} :: PushNotificationPreferences)

instance Data.FromJSON PushNotificationPreferences where
  parseJSON =
    Data.withObject
      "PushNotificationPreferences"
      ( \x ->
          PushNotificationPreferences'
            Prelude.<$> (x Data..:? "FilterRule")
            Prelude.<*> (x Data..: "AllowNotifications")
      )

instance Prelude.Hashable PushNotificationPreferences where
  hashWithSalt _salt PushNotificationPreferences' {..} =
    _salt `Prelude.hashWithSalt` filterRule
      `Prelude.hashWithSalt` allowNotifications

instance Prelude.NFData PushNotificationPreferences where
  rnf PushNotificationPreferences' {..} =
    Prelude.rnf filterRule
      `Prelude.seq` Prelude.rnf allowNotifications

instance Data.ToJSON PushNotificationPreferences where
  toJSON PushNotificationPreferences' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FilterRule" Data..=) Prelude.<$> filterRule,
            Prelude.Just
              ("AllowNotifications" Data..= allowNotifications)
          ]
      )
