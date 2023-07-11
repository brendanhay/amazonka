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
-- Module      : Amazonka.ChimeSDKMessaging.Types.ChannelMembershipForAppInstanceUserSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.ChannelMembershipForAppInstanceUserSummary where

import Amazonka.ChimeSDKMessaging.Types.AppInstanceUserMembershipSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary of the channel membership details of an @AppInstanceUser@.
--
-- /See:/ 'newChannelMembershipForAppInstanceUserSummary' smart constructor.
data ChannelMembershipForAppInstanceUserSummary = ChannelMembershipForAppInstanceUserSummary'
  { -- | Returns the channel membership data for an @AppInstance@.
    appInstanceUserMembershipSummary :: Prelude.Maybe AppInstanceUserMembershipSummary,
    -- | Returns the channel data for an @AppInstance@.
    channelSummary :: Prelude.Maybe ChannelSummary
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelMembershipForAppInstanceUserSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceUserMembershipSummary', 'channelMembershipForAppInstanceUserSummary_appInstanceUserMembershipSummary' - Returns the channel membership data for an @AppInstance@.
--
-- 'channelSummary', 'channelMembershipForAppInstanceUserSummary_channelSummary' - Returns the channel data for an @AppInstance@.
newChannelMembershipForAppInstanceUserSummary ::
  ChannelMembershipForAppInstanceUserSummary
newChannelMembershipForAppInstanceUserSummary =
  ChannelMembershipForAppInstanceUserSummary'
    { appInstanceUserMembershipSummary =
        Prelude.Nothing,
      channelSummary =
        Prelude.Nothing
    }

-- | Returns the channel membership data for an @AppInstance@.
channelMembershipForAppInstanceUserSummary_appInstanceUserMembershipSummary :: Lens.Lens' ChannelMembershipForAppInstanceUserSummary (Prelude.Maybe AppInstanceUserMembershipSummary)
channelMembershipForAppInstanceUserSummary_appInstanceUserMembershipSummary = Lens.lens (\ChannelMembershipForAppInstanceUserSummary' {appInstanceUserMembershipSummary} -> appInstanceUserMembershipSummary) (\s@ChannelMembershipForAppInstanceUserSummary' {} a -> s {appInstanceUserMembershipSummary = a} :: ChannelMembershipForAppInstanceUserSummary)

-- | Returns the channel data for an @AppInstance@.
channelMembershipForAppInstanceUserSummary_channelSummary :: Lens.Lens' ChannelMembershipForAppInstanceUserSummary (Prelude.Maybe ChannelSummary)
channelMembershipForAppInstanceUserSummary_channelSummary = Lens.lens (\ChannelMembershipForAppInstanceUserSummary' {channelSummary} -> channelSummary) (\s@ChannelMembershipForAppInstanceUserSummary' {} a -> s {channelSummary = a} :: ChannelMembershipForAppInstanceUserSummary)

instance
  Data.FromJSON
    ChannelMembershipForAppInstanceUserSummary
  where
  parseJSON =
    Data.withObject
      "ChannelMembershipForAppInstanceUserSummary"
      ( \x ->
          ChannelMembershipForAppInstanceUserSummary'
            Prelude.<$> (x Data..:? "AppInstanceUserMembershipSummary")
            Prelude.<*> (x Data..:? "ChannelSummary")
      )

instance
  Prelude.Hashable
    ChannelMembershipForAppInstanceUserSummary
  where
  hashWithSalt
    _salt
    ChannelMembershipForAppInstanceUserSummary' {..} =
      _salt
        `Prelude.hashWithSalt` appInstanceUserMembershipSummary
        `Prelude.hashWithSalt` channelSummary

instance
  Prelude.NFData
    ChannelMembershipForAppInstanceUserSummary
  where
  rnf ChannelMembershipForAppInstanceUserSummary' {..} =
    Prelude.rnf appInstanceUserMembershipSummary
      `Prelude.seq` Prelude.rnf channelSummary
