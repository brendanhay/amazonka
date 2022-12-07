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
-- Module      : Amazonka.ChimeSDKMessaging.Types.SubChannelSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.SubChannelSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary of the sub-channels associated with the elastic channel.
--
-- /See:/ 'newSubChannelSummary' smart constructor.
data SubChannelSummary = SubChannelSummary'
  { -- | The number of members in a SubChannel.
    membershipCount :: Prelude.Maybe Prelude.Int,
    -- | The unique ID of a SubChannel.
    subChannelId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubChannelSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'membershipCount', 'subChannelSummary_membershipCount' - The number of members in a SubChannel.
--
-- 'subChannelId', 'subChannelSummary_subChannelId' - The unique ID of a SubChannel.
newSubChannelSummary ::
  SubChannelSummary
newSubChannelSummary =
  SubChannelSummary'
    { membershipCount =
        Prelude.Nothing,
      subChannelId = Prelude.Nothing
    }

-- | The number of members in a SubChannel.
subChannelSummary_membershipCount :: Lens.Lens' SubChannelSummary (Prelude.Maybe Prelude.Int)
subChannelSummary_membershipCount = Lens.lens (\SubChannelSummary' {membershipCount} -> membershipCount) (\s@SubChannelSummary' {} a -> s {membershipCount = a} :: SubChannelSummary)

-- | The unique ID of a SubChannel.
subChannelSummary_subChannelId :: Lens.Lens' SubChannelSummary (Prelude.Maybe Prelude.Text)
subChannelSummary_subChannelId = Lens.lens (\SubChannelSummary' {subChannelId} -> subChannelId) (\s@SubChannelSummary' {} a -> s {subChannelId = a} :: SubChannelSummary)

instance Data.FromJSON SubChannelSummary where
  parseJSON =
    Data.withObject
      "SubChannelSummary"
      ( \x ->
          SubChannelSummary'
            Prelude.<$> (x Data..:? "MembershipCount")
            Prelude.<*> (x Data..:? "SubChannelId")
      )

instance Prelude.Hashable SubChannelSummary where
  hashWithSalt _salt SubChannelSummary' {..} =
    _salt `Prelude.hashWithSalt` membershipCount
      `Prelude.hashWithSalt` subChannelId

instance Prelude.NFData SubChannelSummary where
  rnf SubChannelSummary' {..} =
    Prelude.rnf membershipCount
      `Prelude.seq` Prelude.rnf subChannelId
