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
-- Module      : Network.AWS.EC2.Types.EnableFastSnapshotRestoreSuccessItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnableFastSnapshotRestoreSuccessItem where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FastSnapshotRestoreStateCode
import qualified Network.AWS.Lens as Lens

-- | Describes fast snapshot restores that were successfully enabled.
--
-- /See:/ 'newEnableFastSnapshotRestoreSuccessItem' smart constructor.
data EnableFastSnapshotRestoreSuccessItem = EnableFastSnapshotRestoreSuccessItem'
  { -- | The AWS owner alias that enabled fast snapshot restores on the snapshot.
    -- This is intended for future use.
    ownerAlias :: Core.Maybe Core.Text,
    -- | The ID of the AWS account that enabled fast snapshot restores on the
    -- snapshot.
    ownerId :: Core.Maybe Core.Text,
    -- | The reason for the state transition. The possible values are as follows:
    --
    -- -   @Client.UserInitiated@ - The state successfully transitioned to
    --     @enabling@ or @disabling@.
    --
    -- -   @Client.UserInitiated - Lifecycle state transition@ - The state
    --     successfully transitioned to @optimizing@, @enabled@, or @disabled@.
    stateTransitionReason :: Core.Maybe Core.Text,
    -- | The time at which fast snapshot restores entered the @optimizing@ state.
    optimizingTime :: Core.Maybe Core.ISO8601,
    -- | The state of fast snapshot restores.
    state :: Core.Maybe FastSnapshotRestoreStateCode,
    -- | The Availability Zone.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The time at which fast snapshot restores entered the @disabling@ state.
    disablingTime :: Core.Maybe Core.ISO8601,
    -- | The ID of the snapshot.
    snapshotId :: Core.Maybe Core.Text,
    -- | The time at which fast snapshot restores entered the @enabling@ state.
    enablingTime :: Core.Maybe Core.ISO8601,
    -- | The time at which fast snapshot restores entered the @enabled@ state.
    enabledTime :: Core.Maybe Core.ISO8601,
    -- | The time at which fast snapshot restores entered the @disabled@ state.
    disabledTime :: Core.Maybe Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableFastSnapshotRestoreSuccessItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerAlias', 'enableFastSnapshotRestoreSuccessItem_ownerAlias' - The AWS owner alias that enabled fast snapshot restores on the snapshot.
-- This is intended for future use.
--
-- 'ownerId', 'enableFastSnapshotRestoreSuccessItem_ownerId' - The ID of the AWS account that enabled fast snapshot restores on the
-- snapshot.
--
-- 'stateTransitionReason', 'enableFastSnapshotRestoreSuccessItem_stateTransitionReason' - The reason for the state transition. The possible values are as follows:
--
-- -   @Client.UserInitiated@ - The state successfully transitioned to
--     @enabling@ or @disabling@.
--
-- -   @Client.UserInitiated - Lifecycle state transition@ - The state
--     successfully transitioned to @optimizing@, @enabled@, or @disabled@.
--
-- 'optimizingTime', 'enableFastSnapshotRestoreSuccessItem_optimizingTime' - The time at which fast snapshot restores entered the @optimizing@ state.
--
-- 'state', 'enableFastSnapshotRestoreSuccessItem_state' - The state of fast snapshot restores.
--
-- 'availabilityZone', 'enableFastSnapshotRestoreSuccessItem_availabilityZone' - The Availability Zone.
--
-- 'disablingTime', 'enableFastSnapshotRestoreSuccessItem_disablingTime' - The time at which fast snapshot restores entered the @disabling@ state.
--
-- 'snapshotId', 'enableFastSnapshotRestoreSuccessItem_snapshotId' - The ID of the snapshot.
--
-- 'enablingTime', 'enableFastSnapshotRestoreSuccessItem_enablingTime' - The time at which fast snapshot restores entered the @enabling@ state.
--
-- 'enabledTime', 'enableFastSnapshotRestoreSuccessItem_enabledTime' - The time at which fast snapshot restores entered the @enabled@ state.
--
-- 'disabledTime', 'enableFastSnapshotRestoreSuccessItem_disabledTime' - The time at which fast snapshot restores entered the @disabled@ state.
newEnableFastSnapshotRestoreSuccessItem ::
  EnableFastSnapshotRestoreSuccessItem
newEnableFastSnapshotRestoreSuccessItem =
  EnableFastSnapshotRestoreSuccessItem'
    { ownerAlias =
        Core.Nothing,
      ownerId = Core.Nothing,
      stateTransitionReason = Core.Nothing,
      optimizingTime = Core.Nothing,
      state = Core.Nothing,
      availabilityZone = Core.Nothing,
      disablingTime = Core.Nothing,
      snapshotId = Core.Nothing,
      enablingTime = Core.Nothing,
      enabledTime = Core.Nothing,
      disabledTime = Core.Nothing
    }

-- | The AWS owner alias that enabled fast snapshot restores on the snapshot.
-- This is intended for future use.
enableFastSnapshotRestoreSuccessItem_ownerAlias :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
enableFastSnapshotRestoreSuccessItem_ownerAlias = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {ownerAlias} -> ownerAlias) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {ownerAlias = a} :: EnableFastSnapshotRestoreSuccessItem)

-- | The ID of the AWS account that enabled fast snapshot restores on the
-- snapshot.
enableFastSnapshotRestoreSuccessItem_ownerId :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
enableFastSnapshotRestoreSuccessItem_ownerId = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {ownerId} -> ownerId) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {ownerId = a} :: EnableFastSnapshotRestoreSuccessItem)

-- | The reason for the state transition. The possible values are as follows:
--
-- -   @Client.UserInitiated@ - The state successfully transitioned to
--     @enabling@ or @disabling@.
--
-- -   @Client.UserInitiated - Lifecycle state transition@ - The state
--     successfully transitioned to @optimizing@, @enabled@, or @disabled@.
enableFastSnapshotRestoreSuccessItem_stateTransitionReason :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
enableFastSnapshotRestoreSuccessItem_stateTransitionReason = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {stateTransitionReason} -> stateTransitionReason) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {stateTransitionReason = a} :: EnableFastSnapshotRestoreSuccessItem)

-- | The time at which fast snapshot restores entered the @optimizing@ state.
enableFastSnapshotRestoreSuccessItem_optimizingTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
enableFastSnapshotRestoreSuccessItem_optimizingTime = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {optimizingTime} -> optimizingTime) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {optimizingTime = a} :: EnableFastSnapshotRestoreSuccessItem) Core.. Lens.mapping Core._Time

-- | The state of fast snapshot restores.
enableFastSnapshotRestoreSuccessItem_state :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe FastSnapshotRestoreStateCode)
enableFastSnapshotRestoreSuccessItem_state = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {state} -> state) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {state = a} :: EnableFastSnapshotRestoreSuccessItem)

-- | The Availability Zone.
enableFastSnapshotRestoreSuccessItem_availabilityZone :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
enableFastSnapshotRestoreSuccessItem_availabilityZone = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {availabilityZone} -> availabilityZone) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {availabilityZone = a} :: EnableFastSnapshotRestoreSuccessItem)

-- | The time at which fast snapshot restores entered the @disabling@ state.
enableFastSnapshotRestoreSuccessItem_disablingTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
enableFastSnapshotRestoreSuccessItem_disablingTime = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {disablingTime} -> disablingTime) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {disablingTime = a} :: EnableFastSnapshotRestoreSuccessItem) Core.. Lens.mapping Core._Time

-- | The ID of the snapshot.
enableFastSnapshotRestoreSuccessItem_snapshotId :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
enableFastSnapshotRestoreSuccessItem_snapshotId = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {snapshotId} -> snapshotId) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {snapshotId = a} :: EnableFastSnapshotRestoreSuccessItem)

-- | The time at which fast snapshot restores entered the @enabling@ state.
enableFastSnapshotRestoreSuccessItem_enablingTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
enableFastSnapshotRestoreSuccessItem_enablingTime = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {enablingTime} -> enablingTime) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {enablingTime = a} :: EnableFastSnapshotRestoreSuccessItem) Core.. Lens.mapping Core._Time

-- | The time at which fast snapshot restores entered the @enabled@ state.
enableFastSnapshotRestoreSuccessItem_enabledTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
enableFastSnapshotRestoreSuccessItem_enabledTime = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {enabledTime} -> enabledTime) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {enabledTime = a} :: EnableFastSnapshotRestoreSuccessItem) Core.. Lens.mapping Core._Time

-- | The time at which fast snapshot restores entered the @disabled@ state.
enableFastSnapshotRestoreSuccessItem_disabledTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
enableFastSnapshotRestoreSuccessItem_disabledTime = Lens.lens (\EnableFastSnapshotRestoreSuccessItem' {disabledTime} -> disabledTime) (\s@EnableFastSnapshotRestoreSuccessItem' {} a -> s {disabledTime = a} :: EnableFastSnapshotRestoreSuccessItem) Core.. Lens.mapping Core._Time

instance
  Core.FromXML
    EnableFastSnapshotRestoreSuccessItem
  where
  parseXML x =
    EnableFastSnapshotRestoreSuccessItem'
      Core.<$> (x Core..@? "ownerAlias")
      Core.<*> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "stateTransitionReason")
      Core.<*> (x Core..@? "optimizingTime")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "disablingTime")
      Core.<*> (x Core..@? "snapshotId")
      Core.<*> (x Core..@? "enablingTime")
      Core.<*> (x Core..@? "enabledTime")
      Core.<*> (x Core..@? "disabledTime")

instance
  Core.Hashable
    EnableFastSnapshotRestoreSuccessItem

instance
  Core.NFData
    EnableFastSnapshotRestoreSuccessItem
