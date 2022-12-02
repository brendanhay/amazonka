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
-- Module      : Amazonka.ChimeSDKMessaging.Types.ChannelMembershipPreferences
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.ChannelMembershipPreferences where

import Amazonka.ChimeSDKMessaging.Types.PushNotificationPreferences
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The channel membership preferences for an @AppInstanceUser@.
--
-- /See:/ 'newChannelMembershipPreferences' smart constructor.
data ChannelMembershipPreferences = ChannelMembershipPreferences'
  { -- | The push notification configuration of a message.
    pushNotifications :: Prelude.Maybe PushNotificationPreferences
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelMembershipPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pushNotifications', 'channelMembershipPreferences_pushNotifications' - The push notification configuration of a message.
newChannelMembershipPreferences ::
  ChannelMembershipPreferences
newChannelMembershipPreferences =
  ChannelMembershipPreferences'
    { pushNotifications =
        Prelude.Nothing
    }

-- | The push notification configuration of a message.
channelMembershipPreferences_pushNotifications :: Lens.Lens' ChannelMembershipPreferences (Prelude.Maybe PushNotificationPreferences)
channelMembershipPreferences_pushNotifications = Lens.lens (\ChannelMembershipPreferences' {pushNotifications} -> pushNotifications) (\s@ChannelMembershipPreferences' {} a -> s {pushNotifications = a} :: ChannelMembershipPreferences)

instance Data.FromJSON ChannelMembershipPreferences where
  parseJSON =
    Data.withObject
      "ChannelMembershipPreferences"
      ( \x ->
          ChannelMembershipPreferences'
            Prelude.<$> (x Data..:? "PushNotifications")
      )

instance
  Prelude.Hashable
    ChannelMembershipPreferences
  where
  hashWithSalt _salt ChannelMembershipPreferences' {..} =
    _salt `Prelude.hashWithSalt` pushNotifications

instance Prelude.NFData ChannelMembershipPreferences where
  rnf ChannelMembershipPreferences' {..} =
    Prelude.rnf pushNotifications

instance Data.ToJSON ChannelMembershipPreferences where
  toJSON ChannelMembershipPreferences' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PushNotifications" Data..=)
              Prelude.<$> pushNotifications
          ]
      )
