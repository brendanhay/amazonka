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
-- Module      : Amazonka.FSx.Types.TieringPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.TieringPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.TieringPolicyName
import qualified Amazonka.Prelude as Prelude

-- | Describes the data tiering policy for an ONTAP volume. When enabled,
-- Amazon FSx for ONTAP\'s intelligent tiering automatically transitions a
-- volume\'s data between the file system\'s primary storage and capacity
-- pool storage based on your access patterns.
--
-- Valid tiering policies are the following:
--
-- -   @SNAPSHOT_ONLY@ - (Default value) moves cold snapshots to the
--     capacity pool storage tier.
--
-- -   @AUTO@ - moves cold user data and snapshots to the capacity pool
--     storage tier based on your access patterns.
--
-- -   @ALL@ - moves all user data blocks in both the active file system
--     and Snapshot copies to the storage pool tier.
--
-- -   @NONE@ - keeps a volume\'s data in the primary storage tier,
--     preventing it from being moved to the capacity pool tier.
--
-- /See:/ 'newTieringPolicy' smart constructor.
data TieringPolicy = TieringPolicy'
  { -- | Specifies the number of days that user data in a volume must remain
    -- inactive before it is considered \"cold\" and moved to the capacity
    -- pool. Used with the @AUTO@ and @SNAPSHOT_ONLY@ tiering policies. Enter a
    -- whole number between 2 and 183. Default values are 31 days for @AUTO@
    -- and 2 days for @SNAPSHOT_ONLY@.
    coolingPeriod :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the tiering policy used to transition data. Default value is
    -- @SNAPSHOT_ONLY@.
    --
    -- -   @SNAPSHOT_ONLY@ - moves cold snapshots to the capacity pool storage
    --     tier.
    --
    -- -   @AUTO@ - moves cold user data and snapshots to the capacity pool
    --     storage tier based on your access patterns.
    --
    -- -   @ALL@ - moves all user data blocks in both the active file system
    --     and Snapshot copies to the storage pool tier.
    --
    -- -   @NONE@ - keeps a volume\'s data in the primary storage tier,
    --     preventing it from being moved to the capacity pool tier.
    name :: Prelude.Maybe TieringPolicyName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TieringPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coolingPeriod', 'tieringPolicy_coolingPeriod' - Specifies the number of days that user data in a volume must remain
-- inactive before it is considered \"cold\" and moved to the capacity
-- pool. Used with the @AUTO@ and @SNAPSHOT_ONLY@ tiering policies. Enter a
-- whole number between 2 and 183. Default values are 31 days for @AUTO@
-- and 2 days for @SNAPSHOT_ONLY@.
--
-- 'name', 'tieringPolicy_name' - Specifies the tiering policy used to transition data. Default value is
-- @SNAPSHOT_ONLY@.
--
-- -   @SNAPSHOT_ONLY@ - moves cold snapshots to the capacity pool storage
--     tier.
--
-- -   @AUTO@ - moves cold user data and snapshots to the capacity pool
--     storage tier based on your access patterns.
--
-- -   @ALL@ - moves all user data blocks in both the active file system
--     and Snapshot copies to the storage pool tier.
--
-- -   @NONE@ - keeps a volume\'s data in the primary storage tier,
--     preventing it from being moved to the capacity pool tier.
newTieringPolicy ::
  TieringPolicy
newTieringPolicy =
  TieringPolicy'
    { coolingPeriod = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | Specifies the number of days that user data in a volume must remain
-- inactive before it is considered \"cold\" and moved to the capacity
-- pool. Used with the @AUTO@ and @SNAPSHOT_ONLY@ tiering policies. Enter a
-- whole number between 2 and 183. Default values are 31 days for @AUTO@
-- and 2 days for @SNAPSHOT_ONLY@.
tieringPolicy_coolingPeriod :: Lens.Lens' TieringPolicy (Prelude.Maybe Prelude.Natural)
tieringPolicy_coolingPeriod = Lens.lens (\TieringPolicy' {coolingPeriod} -> coolingPeriod) (\s@TieringPolicy' {} a -> s {coolingPeriod = a} :: TieringPolicy)

-- | Specifies the tiering policy used to transition data. Default value is
-- @SNAPSHOT_ONLY@.
--
-- -   @SNAPSHOT_ONLY@ - moves cold snapshots to the capacity pool storage
--     tier.
--
-- -   @AUTO@ - moves cold user data and snapshots to the capacity pool
--     storage tier based on your access patterns.
--
-- -   @ALL@ - moves all user data blocks in both the active file system
--     and Snapshot copies to the storage pool tier.
--
-- -   @NONE@ - keeps a volume\'s data in the primary storage tier,
--     preventing it from being moved to the capacity pool tier.
tieringPolicy_name :: Lens.Lens' TieringPolicy (Prelude.Maybe TieringPolicyName)
tieringPolicy_name = Lens.lens (\TieringPolicy' {name} -> name) (\s@TieringPolicy' {} a -> s {name = a} :: TieringPolicy)

instance Data.FromJSON TieringPolicy where
  parseJSON =
    Data.withObject
      "TieringPolicy"
      ( \x ->
          TieringPolicy'
            Prelude.<$> (x Data..:? "CoolingPeriod")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable TieringPolicy where
  hashWithSalt _salt TieringPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` coolingPeriod
      `Prelude.hashWithSalt` name

instance Prelude.NFData TieringPolicy where
  rnf TieringPolicy' {..} =
    Prelude.rnf coolingPeriod
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON TieringPolicy where
  toJSON TieringPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CoolingPeriod" Data..=) Prelude.<$> coolingPeriod,
            ("Name" Data..=) Prelude.<$> name
          ]
      )
