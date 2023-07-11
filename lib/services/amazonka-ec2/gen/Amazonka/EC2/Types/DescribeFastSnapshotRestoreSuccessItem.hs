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
-- Module      : Amazonka.EC2.Types.DescribeFastSnapshotRestoreSuccessItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DescribeFastSnapshotRestoreSuccessItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.FastSnapshotRestoreStateCode
import qualified Amazonka.Prelude as Prelude

-- | Describes fast snapshot restores for a snapshot.
--
-- /See:/ 'newDescribeFastSnapshotRestoreSuccessItem' smart constructor.
data DescribeFastSnapshotRestoreSuccessItem = DescribeFastSnapshotRestoreSuccessItem'
  { -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The time at which fast snapshot restores entered the @disabled@ state.
    disabledTime :: Prelude.Maybe Data.ISO8601,
    -- | The time at which fast snapshot restores entered the @disabling@ state.
    disablingTime :: Prelude.Maybe Data.ISO8601,
    -- | The time at which fast snapshot restores entered the @enabled@ state.
    enabledTime :: Prelude.Maybe Data.ISO8601,
    -- | The time at which fast snapshot restores entered the @enabling@ state.
    enablingTime :: Prelude.Maybe Data.ISO8601,
    -- | The time at which fast snapshot restores entered the @optimizing@ state.
    optimizingTime :: Prelude.Maybe Data.ISO8601,
    -- | The Amazon Web Services owner alias that enabled fast snapshot restores
    -- on the snapshot. This is intended for future use.
    ownerAlias :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that enabled fast snapshot
    -- restores on the snapshot.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The state of fast snapshot restores.
    state :: Prelude.Maybe FastSnapshotRestoreStateCode,
    -- | The reason for the state transition. The possible values are as follows:
    --
    -- -   @Client.UserInitiated@ - The state successfully transitioned to
    --     @enabling@ or @disabling@.
    --
    -- -   @Client.UserInitiated - Lifecycle state transition@ - The state
    --     successfully transitioned to @optimizing@, @enabled@, or @disabled@.
    stateTransitionReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFastSnapshotRestoreSuccessItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'describeFastSnapshotRestoreSuccessItem_availabilityZone' - The Availability Zone.
--
-- 'disabledTime', 'describeFastSnapshotRestoreSuccessItem_disabledTime' - The time at which fast snapshot restores entered the @disabled@ state.
--
-- 'disablingTime', 'describeFastSnapshotRestoreSuccessItem_disablingTime' - The time at which fast snapshot restores entered the @disabling@ state.
--
-- 'enabledTime', 'describeFastSnapshotRestoreSuccessItem_enabledTime' - The time at which fast snapshot restores entered the @enabled@ state.
--
-- 'enablingTime', 'describeFastSnapshotRestoreSuccessItem_enablingTime' - The time at which fast snapshot restores entered the @enabling@ state.
--
-- 'optimizingTime', 'describeFastSnapshotRestoreSuccessItem_optimizingTime' - The time at which fast snapshot restores entered the @optimizing@ state.
--
-- 'ownerAlias', 'describeFastSnapshotRestoreSuccessItem_ownerAlias' - The Amazon Web Services owner alias that enabled fast snapshot restores
-- on the snapshot. This is intended for future use.
--
-- 'ownerId', 'describeFastSnapshotRestoreSuccessItem_ownerId' - The ID of the Amazon Web Services account that enabled fast snapshot
-- restores on the snapshot.
--
-- 'snapshotId', 'describeFastSnapshotRestoreSuccessItem_snapshotId' - The ID of the snapshot.
--
-- 'state', 'describeFastSnapshotRestoreSuccessItem_state' - The state of fast snapshot restores.
--
-- 'stateTransitionReason', 'describeFastSnapshotRestoreSuccessItem_stateTransitionReason' - The reason for the state transition. The possible values are as follows:
--
-- -   @Client.UserInitiated@ - The state successfully transitioned to
--     @enabling@ or @disabling@.
--
-- -   @Client.UserInitiated - Lifecycle state transition@ - The state
--     successfully transitioned to @optimizing@, @enabled@, or @disabled@.
newDescribeFastSnapshotRestoreSuccessItem ::
  DescribeFastSnapshotRestoreSuccessItem
newDescribeFastSnapshotRestoreSuccessItem =
  DescribeFastSnapshotRestoreSuccessItem'
    { availabilityZone =
        Prelude.Nothing,
      disabledTime = Prelude.Nothing,
      disablingTime = Prelude.Nothing,
      enabledTime = Prelude.Nothing,
      enablingTime = Prelude.Nothing,
      optimizingTime = Prelude.Nothing,
      ownerAlias = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      state = Prelude.Nothing,
      stateTransitionReason =
        Prelude.Nothing
    }

-- | The Availability Zone.
describeFastSnapshotRestoreSuccessItem_availabilityZone :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
describeFastSnapshotRestoreSuccessItem_availabilityZone = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {availabilityZone} -> availabilityZone) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {availabilityZone = a} :: DescribeFastSnapshotRestoreSuccessItem)

-- | The time at which fast snapshot restores entered the @disabled@ state.
describeFastSnapshotRestoreSuccessItem_disabledTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
describeFastSnapshotRestoreSuccessItem_disabledTime = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {disabledTime} -> disabledTime) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {disabledTime = a} :: DescribeFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Data._Time

-- | The time at which fast snapshot restores entered the @disabling@ state.
describeFastSnapshotRestoreSuccessItem_disablingTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
describeFastSnapshotRestoreSuccessItem_disablingTime = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {disablingTime} -> disablingTime) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {disablingTime = a} :: DescribeFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Data._Time

-- | The time at which fast snapshot restores entered the @enabled@ state.
describeFastSnapshotRestoreSuccessItem_enabledTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
describeFastSnapshotRestoreSuccessItem_enabledTime = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {enabledTime} -> enabledTime) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {enabledTime = a} :: DescribeFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Data._Time

-- | The time at which fast snapshot restores entered the @enabling@ state.
describeFastSnapshotRestoreSuccessItem_enablingTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
describeFastSnapshotRestoreSuccessItem_enablingTime = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {enablingTime} -> enablingTime) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {enablingTime = a} :: DescribeFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Data._Time

-- | The time at which fast snapshot restores entered the @optimizing@ state.
describeFastSnapshotRestoreSuccessItem_optimizingTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
describeFastSnapshotRestoreSuccessItem_optimizingTime = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {optimizingTime} -> optimizingTime) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {optimizingTime = a} :: DescribeFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Data._Time

-- | The Amazon Web Services owner alias that enabled fast snapshot restores
-- on the snapshot. This is intended for future use.
describeFastSnapshotRestoreSuccessItem_ownerAlias :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
describeFastSnapshotRestoreSuccessItem_ownerAlias = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {ownerAlias} -> ownerAlias) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {ownerAlias = a} :: DescribeFastSnapshotRestoreSuccessItem)

-- | The ID of the Amazon Web Services account that enabled fast snapshot
-- restores on the snapshot.
describeFastSnapshotRestoreSuccessItem_ownerId :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
describeFastSnapshotRestoreSuccessItem_ownerId = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {ownerId} -> ownerId) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {ownerId = a} :: DescribeFastSnapshotRestoreSuccessItem)

-- | The ID of the snapshot.
describeFastSnapshotRestoreSuccessItem_snapshotId :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
describeFastSnapshotRestoreSuccessItem_snapshotId = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {snapshotId} -> snapshotId) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {snapshotId = a} :: DescribeFastSnapshotRestoreSuccessItem)

-- | The state of fast snapshot restores.
describeFastSnapshotRestoreSuccessItem_state :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe FastSnapshotRestoreStateCode)
describeFastSnapshotRestoreSuccessItem_state = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {state} -> state) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {state = a} :: DescribeFastSnapshotRestoreSuccessItem)

-- | The reason for the state transition. The possible values are as follows:
--
-- -   @Client.UserInitiated@ - The state successfully transitioned to
--     @enabling@ or @disabling@.
--
-- -   @Client.UserInitiated - Lifecycle state transition@ - The state
--     successfully transitioned to @optimizing@, @enabled@, or @disabled@.
describeFastSnapshotRestoreSuccessItem_stateTransitionReason :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
describeFastSnapshotRestoreSuccessItem_stateTransitionReason = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {stateTransitionReason} -> stateTransitionReason) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {stateTransitionReason = a} :: DescribeFastSnapshotRestoreSuccessItem)

instance
  Data.FromXML
    DescribeFastSnapshotRestoreSuccessItem
  where
  parseXML x =
    DescribeFastSnapshotRestoreSuccessItem'
      Prelude.<$> (x Data..@? "availabilityZone")
      Prelude.<*> (x Data..@? "disabledTime")
      Prelude.<*> (x Data..@? "disablingTime")
      Prelude.<*> (x Data..@? "enabledTime")
      Prelude.<*> (x Data..@? "enablingTime")
      Prelude.<*> (x Data..@? "optimizingTime")
      Prelude.<*> (x Data..@? "ownerAlias")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "snapshotId")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "stateTransitionReason")

instance
  Prelude.Hashable
    DescribeFastSnapshotRestoreSuccessItem
  where
  hashWithSalt
    _salt
    DescribeFastSnapshotRestoreSuccessItem' {..} =
      _salt
        `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` disabledTime
        `Prelude.hashWithSalt` disablingTime
        `Prelude.hashWithSalt` enabledTime
        `Prelude.hashWithSalt` enablingTime
        `Prelude.hashWithSalt` optimizingTime
        `Prelude.hashWithSalt` ownerAlias
        `Prelude.hashWithSalt` ownerId
        `Prelude.hashWithSalt` snapshotId
        `Prelude.hashWithSalt` state
        `Prelude.hashWithSalt` stateTransitionReason

instance
  Prelude.NFData
    DescribeFastSnapshotRestoreSuccessItem
  where
  rnf DescribeFastSnapshotRestoreSuccessItem' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf disabledTime
      `Prelude.seq` Prelude.rnf disablingTime
      `Prelude.seq` Prelude.rnf enabledTime
      `Prelude.seq` Prelude.rnf enablingTime
      `Prelude.seq` Prelude.rnf optimizingTime
      `Prelude.seq` Prelude.rnf ownerAlias
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf stateTransitionReason
