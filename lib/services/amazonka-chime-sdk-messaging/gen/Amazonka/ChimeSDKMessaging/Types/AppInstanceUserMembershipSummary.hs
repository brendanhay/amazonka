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
-- Module      : Amazonka.ChimeSDKMessaging.Types.AppInstanceUserMembershipSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.AppInstanceUserMembershipSummary where

import Amazonka.ChimeSDKMessaging.Types.ChannelMembershipType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary of the membership details of an @AppInstanceUser@.
--
-- /See:/ 'newAppInstanceUserMembershipSummary' smart constructor.
data AppInstanceUserMembershipSummary = AppInstanceUserMembershipSummary'
  { -- | The time at which an @AppInstanceUser@ last marked a channel as read.
    readMarkerTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The ID of the SubChannel that the @AppInstanceUser@ is a member of.
    subChannelId :: Prelude.Maybe Prelude.Text,
    -- | The type of @ChannelMembership@.
    type' :: Prelude.Maybe ChannelMembershipType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppInstanceUserMembershipSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readMarkerTimestamp', 'appInstanceUserMembershipSummary_readMarkerTimestamp' - The time at which an @AppInstanceUser@ last marked a channel as read.
--
-- 'subChannelId', 'appInstanceUserMembershipSummary_subChannelId' - The ID of the SubChannel that the @AppInstanceUser@ is a member of.
--
-- 'type'', 'appInstanceUserMembershipSummary_type' - The type of @ChannelMembership@.
newAppInstanceUserMembershipSummary ::
  AppInstanceUserMembershipSummary
newAppInstanceUserMembershipSummary =
  AppInstanceUserMembershipSummary'
    { readMarkerTimestamp =
        Prelude.Nothing,
      subChannelId = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The time at which an @AppInstanceUser@ last marked a channel as read.
appInstanceUserMembershipSummary_readMarkerTimestamp :: Lens.Lens' AppInstanceUserMembershipSummary (Prelude.Maybe Prelude.UTCTime)
appInstanceUserMembershipSummary_readMarkerTimestamp = Lens.lens (\AppInstanceUserMembershipSummary' {readMarkerTimestamp} -> readMarkerTimestamp) (\s@AppInstanceUserMembershipSummary' {} a -> s {readMarkerTimestamp = a} :: AppInstanceUserMembershipSummary) Prelude.. Lens.mapping Data._Time

-- | The ID of the SubChannel that the @AppInstanceUser@ is a member of.
appInstanceUserMembershipSummary_subChannelId :: Lens.Lens' AppInstanceUserMembershipSummary (Prelude.Maybe Prelude.Text)
appInstanceUserMembershipSummary_subChannelId = Lens.lens (\AppInstanceUserMembershipSummary' {subChannelId} -> subChannelId) (\s@AppInstanceUserMembershipSummary' {} a -> s {subChannelId = a} :: AppInstanceUserMembershipSummary)

-- | The type of @ChannelMembership@.
appInstanceUserMembershipSummary_type :: Lens.Lens' AppInstanceUserMembershipSummary (Prelude.Maybe ChannelMembershipType)
appInstanceUserMembershipSummary_type = Lens.lens (\AppInstanceUserMembershipSummary' {type'} -> type') (\s@AppInstanceUserMembershipSummary' {} a -> s {type' = a} :: AppInstanceUserMembershipSummary)

instance
  Data.FromJSON
    AppInstanceUserMembershipSummary
  where
  parseJSON =
    Data.withObject
      "AppInstanceUserMembershipSummary"
      ( \x ->
          AppInstanceUserMembershipSummary'
            Prelude.<$> (x Data..:? "ReadMarkerTimestamp")
            Prelude.<*> (x Data..:? "SubChannelId")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    AppInstanceUserMembershipSummary
  where
  hashWithSalt
    _salt
    AppInstanceUserMembershipSummary' {..} =
      _salt
        `Prelude.hashWithSalt` readMarkerTimestamp
        `Prelude.hashWithSalt` subChannelId
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AppInstanceUserMembershipSummary
  where
  rnf AppInstanceUserMembershipSummary' {..} =
    Prelude.rnf readMarkerTimestamp
      `Prelude.seq` Prelude.rnf subChannelId
      `Prelude.seq` Prelude.rnf type'
