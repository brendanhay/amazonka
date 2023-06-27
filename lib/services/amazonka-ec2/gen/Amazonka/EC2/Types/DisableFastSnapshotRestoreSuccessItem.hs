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
-- Module      : Amazonka.EC2.Types.DisableFastSnapshotRestoreSuccessItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DisableFastSnapshotRestoreSuccessItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.FastSnapshotRestoreStateCode
import qualified Amazonka.Prelude as Prelude

-- | Describes fast snapshot restores that were successfully disabled.
--
-- /See:/ 'newDisableFastSnapshotRestoreSuccessItem' smart constructor.
data DisableFastSnapshotRestoreSuccessItem = DisableFastSnapshotRestoreSuccessItem'
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
    -- | The state of fast snapshot restores for the snapshot.
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
-- Create a value of 'DisableFastSnapshotRestoreSuccessItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'disableFastSnapshotRestoreSuccessItem_availabilityZone' - The Availability Zone.
--
-- 'disabledTime', 'disableFastSnapshotRestoreSuccessItem_disabledTime' - The time at which fast snapshot restores entered the @disabled@ state.
--
-- 'disablingTime', 'disableFastSnapshotRestoreSuccessItem_disablingTime' - The time at which fast snapshot restores entered the @disabling@ state.
--
-- 'enabledTime', 'disableFastSnapshotRestoreSuccessItem_enabledTime' - The time at which fast snapshot restores entered the @enabled@ state.
--
-- 'enablingTime', 'disableFastSnapshotRestoreSuccessItem_enablingTime' - The time at which fast snapshot restores entered the @enabling@ state.
--
-- 'optimizingTime', 'disableFastSnapshotRestoreSuccessItem_optimizingTime' - The time at which fast snapshot restores entered the @optimizing@ state.
--
-- 'ownerAlias', 'disableFastSnapshotRestoreSuccessItem_ownerAlias' - The Amazon Web Services owner alias that enabled fast snapshot restores
-- on the snapshot. This is intended for future use.
--
-- 'ownerId', 'disableFastSnapshotRestoreSuccessItem_ownerId' - The ID of the Amazon Web Services account that enabled fast snapshot
-- restores on the snapshot.
--
-- 'snapshotId', 'disableFastSnapshotRestoreSuccessItem_snapshotId' - The ID of the snapshot.
--
-- 'state', 'disableFastSnapshotRestoreSuccessItem_state' - The state of fast snapshot restores for the snapshot.
--
-- 'stateTransitionReason', 'disableFastSnapshotRestoreSuccessItem_stateTransitionReason' - The reason for the state transition. The possible values are as follows:
--
-- -   @Client.UserInitiated@ - The state successfully transitioned to
--     @enabling@ or @disabling@.
--
-- -   @Client.UserInitiated - Lifecycle state transition@ - The state
--     successfully transitioned to @optimizing@, @enabled@, or @disabled@.
newDisableFastSnapshotRestoreSuccessItem ::
  DisableFastSnapshotRestoreSuccessItem
newDisableFastSnapshotRestoreSuccessItem =
  DisableFastSnapshotRestoreSuccessItem'
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
disableFastSnapshotRestoreSuccessItem_availabilityZone :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
disableFastSnapshotRestoreSuccessItem_availabilityZone = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {availabilityZone} -> availabilityZone) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {availabilityZone = a} :: DisableFastSnapshotRestoreSuccessItem)

-- | The time at which fast snapshot restores entered the @disabled@ state.
disableFastSnapshotRestoreSuccessItem_disabledTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
disableFastSnapshotRestoreSuccessItem_disabledTime = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {disabledTime} -> disabledTime) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {disabledTime = a} :: DisableFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Data._Time

-- | The time at which fast snapshot restores entered the @disabling@ state.
disableFastSnapshotRestoreSuccessItem_disablingTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
disableFastSnapshotRestoreSuccessItem_disablingTime = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {disablingTime} -> disablingTime) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {disablingTime = a} :: DisableFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Data._Time

-- | The time at which fast snapshot restores entered the @enabled@ state.
disableFastSnapshotRestoreSuccessItem_enabledTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
disableFastSnapshotRestoreSuccessItem_enabledTime = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {enabledTime} -> enabledTime) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {enabledTime = a} :: DisableFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Data._Time

-- | The time at which fast snapshot restores entered the @enabling@ state.
disableFastSnapshotRestoreSuccessItem_enablingTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
disableFastSnapshotRestoreSuccessItem_enablingTime = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {enablingTime} -> enablingTime) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {enablingTime = a} :: DisableFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Data._Time

-- | The time at which fast snapshot restores entered the @optimizing@ state.
disableFastSnapshotRestoreSuccessItem_optimizingTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
disableFastSnapshotRestoreSuccessItem_optimizingTime = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {optimizingTime} -> optimizingTime) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {optimizingTime = a} :: DisableFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Data._Time

-- | The Amazon Web Services owner alias that enabled fast snapshot restores
-- on the snapshot. This is intended for future use.
disableFastSnapshotRestoreSuccessItem_ownerAlias :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
disableFastSnapshotRestoreSuccessItem_ownerAlias = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {ownerAlias} -> ownerAlias) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {ownerAlias = a} :: DisableFastSnapshotRestoreSuccessItem)

-- | The ID of the Amazon Web Services account that enabled fast snapshot
-- restores on the snapshot.
disableFastSnapshotRestoreSuccessItem_ownerId :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
disableFastSnapshotRestoreSuccessItem_ownerId = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {ownerId} -> ownerId) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {ownerId = a} :: DisableFastSnapshotRestoreSuccessItem)

-- | The ID of the snapshot.
disableFastSnapshotRestoreSuccessItem_snapshotId :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
disableFastSnapshotRestoreSuccessItem_snapshotId = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {snapshotId} -> snapshotId) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {snapshotId = a} :: DisableFastSnapshotRestoreSuccessItem)

-- | The state of fast snapshot restores for the snapshot.
disableFastSnapshotRestoreSuccessItem_state :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe FastSnapshotRestoreStateCode)
disableFastSnapshotRestoreSuccessItem_state = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {state} -> state) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {state = a} :: DisableFastSnapshotRestoreSuccessItem)

-- | The reason for the state transition. The possible values are as follows:
--
-- -   @Client.UserInitiated@ - The state successfully transitioned to
--     @enabling@ or @disabling@.
--
-- -   @Client.UserInitiated - Lifecycle state transition@ - The state
--     successfully transitioned to @optimizing@, @enabled@, or @disabled@.
disableFastSnapshotRestoreSuccessItem_stateTransitionReason :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
disableFastSnapshotRestoreSuccessItem_stateTransitionReason = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {stateTransitionReason} -> stateTransitionReason) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {stateTransitionReason = a} :: DisableFastSnapshotRestoreSuccessItem)

instance
  Data.FromXML
    DisableFastSnapshotRestoreSuccessItem
  where
  parseXML x =
    DisableFastSnapshotRestoreSuccessItem'
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
    DisableFastSnapshotRestoreSuccessItem
  where
  hashWithSalt
    _salt
    DisableFastSnapshotRestoreSuccessItem' {..} =
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
    DisableFastSnapshotRestoreSuccessItem
  where
  rnf DisableFastSnapshotRestoreSuccessItem' {..} =
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
