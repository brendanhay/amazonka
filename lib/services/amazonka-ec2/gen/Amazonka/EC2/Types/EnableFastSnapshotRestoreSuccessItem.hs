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
-- Module      : Amazonka.EC2.Types.EnableFastSnapshotRestoreSuccessItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.EnableFastSnapshotRestoreSuccessItem where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.FastSnapshotRestoreStateCode
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes fast snapshot restores that were successfully enabled.
--
-- /See:/ 'newEnableFastSnapshotRestoreSuccessItem' smart constructor.
data EnableFastSnapshotRestoreSuccessItem = EnableFastSnapshotRestoreSuccessItem'
  { -- | The time at which fast snapshot restores entered the @disabling@ state.
    disablingTime :: Prelude.Maybe Core.ISO8601,
    -- | The state of fast snapshot restores.
    state :: Prelude.Maybe FastSnapshotRestoreStateCode,
    -- | The Amazon Web Services owner alias that enabled fast snapshot restores
    -- on the snapshot. This is intended for future use.
    ownerAlias :: Prelude.Maybe Prelude.Text,
    -- | The time at which fast snapshot restores entered the @disabled@ state.
    disabledTime :: Prelude.Maybe Core.ISO8601,
    -- | The time at which fast snapshot restores entered the @enabled@ state.
    enabledTime :: Prelude.Maybe Core.ISO8601,
    -- | The time at which fast snapshot restores entered the @optimizing@ state.
    optimizingTime :: Prelude.Maybe Core.ISO8601,
    -- | The ID of the Amazon Web Services account that enabled fast snapshot
    -- restores on the snapshot.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The reason for the state transition. The possible values are as follows:
    --
    -- -   @Client.UserInitiated@ - The state successfully transitioned to
    --     @enabling@ or @disabling@.
    --
    -- -   @Client.UserInitiated - Lifecycle state transition@ - The state
    --     successfully transitioned to @optimizing@, @enabled@, or @disabled@.
    stateTransitionReason :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The time at which fast snapshot restores entered the @enabling@ state.
    enablingTime :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableFastSnapshotRestoreSuccessItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disablingTime', 'enableFastSnapshotRestoreSuccessItem_disablingTime' - The time at which fast snapshot restores entered the @disabling@ state.
--
-- 'state', 'enableFastSnapshotRestoreSuccessItem_state' - The state of fast snapshot restores.
--
-- 'ownerAlias', 'enableFastSnapshotRestoreSuccessItem_ownerAlias' - The Amazon Web Services owner alias that enabled fast snapshot restores
-- on the snapshot. This is intended for future use.
--
-- 'disabledTime', 'enableFastSnapshotRestoreSuccessItem_disabledTime' - The time at which fast snapshot restores entered the @disabled@ state.
--
-- 'enabledTime', 'enableFastSnapshotRestoreSuccessItem_enabledTime' - The time at which fast snapshot restores entered the @enabled@ state.
--
-- 'optimizingTime', 'enableFastSnapshotRestoreSuccessItem_optimizingTime' - The time at which fast snapshot restores entered the @optimizing@ state.
--
-- 'ownerId', 'enableFastSnapshotRestoreSuccessItem_ownerId' - The ID of the Amazon Web Services account that enabled fast snapshot
-- restores on the snapshot.
--
-- 'stateTransitionReason', 'enableFastSnapshotRestoreSuccessItem_stateTransitionReason' - The reason for the state transition. The possible values are as follows:
--
-- -   @Client.UserInitiated@ - The state successfully transitioned to
--     @enabling@ or @disabling@.
--
-- -   @Client.UserInitiated - Lifecycle state transition@ - The state
--     successfully transitioned to @optimizing@, @enabled@, or @disabled@.
--
-- 'availabilityZone', 'enableFastSnapshotRestoreSuccessItem_availabilityZone' - The Availability Zone.
--
-- 'snapshotId', 'enableFastSnapshotRestoreSuccessItem_snapshotId' - The ID of the snapshot.
--
-- 'enablingTime', 'enableFastSnapshotRestoreSuccessItem_enablingTime' - The time at which fast snapshot restores entered the @enabling@ state.
newEnableFastSnapshotRestoreSuccessItem ::
  EnableFastSnapshotRestoreSuccessItem
newEnableFastSnapshotRestoreSuccessItem =
  EnableFastSnapshotRestoreSuccessItem'
    { disablingTime =
        Prelude.Nothing,
      state = Prelude.Nothing,
      ownerAlias = Prelude.Nothing,
      disabledTime = Prelude.Nothing,
      enabledTime = Prelude.Nothing,
      optimizingTime = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      stateTransitionReason =
        Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      enablingTime = Prelude.Nothing
    }

-- | The time at which fast snapshot restores entered the @disabling@ state.
enableFastSnapshotRestoreSuccessItem_disablingTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
enableFastSnapshotRestoreSuccessItem_disablingTime = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {disablingTime} -> disablingTime) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {disablingTime = a} :: EnableFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Core._Time

-- | The state of fast snapshot restores.
enableFastSnapshotRestoreSuccessItem_state :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Prelude.Maybe FastSnapshotRestoreStateCode)
enableFastSnapshotRestoreSuccessItem_state = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {state} -> state) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {state = a} :: EnableFastSnapshotRestoreSuccessItem)

-- | The Amazon Web Services owner alias that enabled fast snapshot restores
-- on the snapshot. This is intended for future use.
enableFastSnapshotRestoreSuccessItem_ownerAlias :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
enableFastSnapshotRestoreSuccessItem_ownerAlias = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {ownerAlias} -> ownerAlias) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {ownerAlias = a} :: EnableFastSnapshotRestoreSuccessItem)

-- | The time at which fast snapshot restores entered the @disabled@ state.
enableFastSnapshotRestoreSuccessItem_disabledTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
enableFastSnapshotRestoreSuccessItem_disabledTime = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {disabledTime} -> disabledTime) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {disabledTime = a} :: EnableFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Core._Time

-- | The time at which fast snapshot restores entered the @enabled@ state.
enableFastSnapshotRestoreSuccessItem_enabledTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
enableFastSnapshotRestoreSuccessItem_enabledTime = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {enabledTime} -> enabledTime) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {enabledTime = a} :: EnableFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Core._Time

-- | The time at which fast snapshot restores entered the @optimizing@ state.
enableFastSnapshotRestoreSuccessItem_optimizingTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
enableFastSnapshotRestoreSuccessItem_optimizingTime = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {optimizingTime} -> optimizingTime) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {optimizingTime = a} :: EnableFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Core._Time

-- | The ID of the Amazon Web Services account that enabled fast snapshot
-- restores on the snapshot.
enableFastSnapshotRestoreSuccessItem_ownerId :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
enableFastSnapshotRestoreSuccessItem_ownerId = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {ownerId} -> ownerId) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {ownerId = a} :: EnableFastSnapshotRestoreSuccessItem)

-- | The reason for the state transition. The possible values are as follows:
--
-- -   @Client.UserInitiated@ - The state successfully transitioned to
--     @enabling@ or @disabling@.
--
-- -   @Client.UserInitiated - Lifecycle state transition@ - The state
--     successfully transitioned to @optimizing@, @enabled@, or @disabled@.
enableFastSnapshotRestoreSuccessItem_stateTransitionReason :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
enableFastSnapshotRestoreSuccessItem_stateTransitionReason = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {stateTransitionReason} -> stateTransitionReason) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {stateTransitionReason = a} :: EnableFastSnapshotRestoreSuccessItem)

-- | The Availability Zone.
enableFastSnapshotRestoreSuccessItem_availabilityZone :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
enableFastSnapshotRestoreSuccessItem_availabilityZone = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {availabilityZone} -> availabilityZone) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {availabilityZone = a} :: EnableFastSnapshotRestoreSuccessItem)

-- | The ID of the snapshot.
enableFastSnapshotRestoreSuccessItem_snapshotId :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
enableFastSnapshotRestoreSuccessItem_snapshotId = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {snapshotId} -> snapshotId) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {snapshotId = a} :: EnableFastSnapshotRestoreSuccessItem)

-- | The time at which fast snapshot restores entered the @enabling@ state.
enableFastSnapshotRestoreSuccessItem_enablingTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
enableFastSnapshotRestoreSuccessItem_enablingTime = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {enablingTime} -> enablingTime) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {enablingTime = a} :: EnableFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Core._Time

instance
  Core.FromXML
    EnableFastSnapshotRestoreSuccessItem
  where
  parseXML x =
    EnableFastSnapshotRestoreSuccessItem'
      Prelude.<$> (x Core..@? "disablingTime")
      Prelude.<*> (x Core..@? "state")
      Prelude.<*> (x Core..@? "ownerAlias")
      Prelude.<*> (x Core..@? "disabledTime")
      Prelude.<*> (x Core..@? "enabledTime")
      Prelude.<*> (x Core..@? "optimizingTime")
      Prelude.<*> (x Core..@? "ownerId")
      Prelude.<*> (x Core..@? "stateTransitionReason")
      Prelude.<*> (x Core..@? "availabilityZone")
      Prelude.<*> (x Core..@? "snapshotId")
      Prelude.<*> (x Core..@? "enablingTime")

instance
  Prelude.Hashable
    EnableFastSnapshotRestoreSuccessItem
  where
  hashWithSalt
    _salt
    EnableFastSnapshotRestoreSuccessItem' {..} =
      _salt `Prelude.hashWithSalt` disablingTime
        `Prelude.hashWithSalt` state
        `Prelude.hashWithSalt` ownerAlias
        `Prelude.hashWithSalt` disabledTime
        `Prelude.hashWithSalt` enabledTime
        `Prelude.hashWithSalt` optimizingTime
        `Prelude.hashWithSalt` ownerId
        `Prelude.hashWithSalt` stateTransitionReason
        `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` snapshotId
        `Prelude.hashWithSalt` enablingTime

instance
  Prelude.NFData
    EnableFastSnapshotRestoreSuccessItem
  where
  rnf EnableFastSnapshotRestoreSuccessItem' {..} =
    Prelude.rnf disablingTime
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf ownerAlias
      `Prelude.seq` Prelude.rnf disabledTime
      `Prelude.seq` Prelude.rnf enabledTime
      `Prelude.seq` Prelude.rnf optimizingTime
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf stateTransitionReason
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf enablingTime
