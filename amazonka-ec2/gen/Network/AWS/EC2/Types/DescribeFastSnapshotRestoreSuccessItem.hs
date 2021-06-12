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
-- Module      : Network.AWS.EC2.Types.DescribeFastSnapshotRestoreSuccessItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DescribeFastSnapshotRestoreSuccessItem where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FastSnapshotRestoreStateCode
import qualified Network.AWS.Lens as Lens

-- | Describes fast snapshot restores for a snapshot.
--
-- /See:/ 'newDescribeFastSnapshotRestoreSuccessItem' smart constructor.
data DescribeFastSnapshotRestoreSuccessItem = DescribeFastSnapshotRestoreSuccessItem'
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
-- Create a value of 'DescribeFastSnapshotRestoreSuccessItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerAlias', 'describeFastSnapshotRestoreSuccessItem_ownerAlias' - The AWS owner alias that enabled fast snapshot restores on the snapshot.
-- This is intended for future use.
--
-- 'ownerId', 'describeFastSnapshotRestoreSuccessItem_ownerId' - The ID of the AWS account that enabled fast snapshot restores on the
-- snapshot.
--
-- 'stateTransitionReason', 'describeFastSnapshotRestoreSuccessItem_stateTransitionReason' - The reason for the state transition. The possible values are as follows:
--
-- -   @Client.UserInitiated@ - The state successfully transitioned to
--     @enabling@ or @disabling@.
--
-- -   @Client.UserInitiated - Lifecycle state transition@ - The state
--     successfully transitioned to @optimizing@, @enabled@, or @disabled@.
--
-- 'optimizingTime', 'describeFastSnapshotRestoreSuccessItem_optimizingTime' - The time at which fast snapshot restores entered the @optimizing@ state.
--
-- 'state', 'describeFastSnapshotRestoreSuccessItem_state' - The state of fast snapshot restores.
--
-- 'availabilityZone', 'describeFastSnapshotRestoreSuccessItem_availabilityZone' - The Availability Zone.
--
-- 'disablingTime', 'describeFastSnapshotRestoreSuccessItem_disablingTime' - The time at which fast snapshot restores entered the @disabling@ state.
--
-- 'snapshotId', 'describeFastSnapshotRestoreSuccessItem_snapshotId' - The ID of the snapshot.
--
-- 'enablingTime', 'describeFastSnapshotRestoreSuccessItem_enablingTime' - The time at which fast snapshot restores entered the @enabling@ state.
--
-- 'enabledTime', 'describeFastSnapshotRestoreSuccessItem_enabledTime' - The time at which fast snapshot restores entered the @enabled@ state.
--
-- 'disabledTime', 'describeFastSnapshotRestoreSuccessItem_disabledTime' - The time at which fast snapshot restores entered the @disabled@ state.
newDescribeFastSnapshotRestoreSuccessItem ::
  DescribeFastSnapshotRestoreSuccessItem
newDescribeFastSnapshotRestoreSuccessItem =
  DescribeFastSnapshotRestoreSuccessItem'
    { ownerAlias =
        Core.Nothing,
      ownerId = Core.Nothing,
      stateTransitionReason =
        Core.Nothing,
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
describeFastSnapshotRestoreSuccessItem_ownerAlias :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
describeFastSnapshotRestoreSuccessItem_ownerAlias = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {ownerAlias} -> ownerAlias) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {ownerAlias = a} :: DescribeFastSnapshotRestoreSuccessItem)

-- | The ID of the AWS account that enabled fast snapshot restores on the
-- snapshot.
describeFastSnapshotRestoreSuccessItem_ownerId :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
describeFastSnapshotRestoreSuccessItem_ownerId = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {ownerId} -> ownerId) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {ownerId = a} :: DescribeFastSnapshotRestoreSuccessItem)

-- | The reason for the state transition. The possible values are as follows:
--
-- -   @Client.UserInitiated@ - The state successfully transitioned to
--     @enabling@ or @disabling@.
--
-- -   @Client.UserInitiated - Lifecycle state transition@ - The state
--     successfully transitioned to @optimizing@, @enabled@, or @disabled@.
describeFastSnapshotRestoreSuccessItem_stateTransitionReason :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
describeFastSnapshotRestoreSuccessItem_stateTransitionReason = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {stateTransitionReason} -> stateTransitionReason) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {stateTransitionReason = a} :: DescribeFastSnapshotRestoreSuccessItem)

-- | The time at which fast snapshot restores entered the @optimizing@ state.
describeFastSnapshotRestoreSuccessItem_optimizingTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
describeFastSnapshotRestoreSuccessItem_optimizingTime = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {optimizingTime} -> optimizingTime) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {optimizingTime = a} :: DescribeFastSnapshotRestoreSuccessItem) Core.. Lens.mapping Core._Time

-- | The state of fast snapshot restores.
describeFastSnapshotRestoreSuccessItem_state :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Core.Maybe FastSnapshotRestoreStateCode)
describeFastSnapshotRestoreSuccessItem_state = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {state} -> state) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {state = a} :: DescribeFastSnapshotRestoreSuccessItem)

-- | The Availability Zone.
describeFastSnapshotRestoreSuccessItem_availabilityZone :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
describeFastSnapshotRestoreSuccessItem_availabilityZone = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {availabilityZone} -> availabilityZone) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {availabilityZone = a} :: DescribeFastSnapshotRestoreSuccessItem)

-- | The time at which fast snapshot restores entered the @disabling@ state.
describeFastSnapshotRestoreSuccessItem_disablingTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
describeFastSnapshotRestoreSuccessItem_disablingTime = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {disablingTime} -> disablingTime) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {disablingTime = a} :: DescribeFastSnapshotRestoreSuccessItem) Core.. Lens.mapping Core._Time

-- | The ID of the snapshot.
describeFastSnapshotRestoreSuccessItem_snapshotId :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
describeFastSnapshotRestoreSuccessItem_snapshotId = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {snapshotId} -> snapshotId) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {snapshotId = a} :: DescribeFastSnapshotRestoreSuccessItem)

-- | The time at which fast snapshot restores entered the @enabling@ state.
describeFastSnapshotRestoreSuccessItem_enablingTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
describeFastSnapshotRestoreSuccessItem_enablingTime = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {enablingTime} -> enablingTime) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {enablingTime = a} :: DescribeFastSnapshotRestoreSuccessItem) Core.. Lens.mapping Core._Time

-- | The time at which fast snapshot restores entered the @enabled@ state.
describeFastSnapshotRestoreSuccessItem_enabledTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
describeFastSnapshotRestoreSuccessItem_enabledTime = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {enabledTime} -> enabledTime) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {enabledTime = a} :: DescribeFastSnapshotRestoreSuccessItem) Core.. Lens.mapping Core._Time

-- | The time at which fast snapshot restores entered the @disabled@ state.
describeFastSnapshotRestoreSuccessItem_disabledTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
describeFastSnapshotRestoreSuccessItem_disabledTime = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {disabledTime} -> disabledTime) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {disabledTime = a} :: DescribeFastSnapshotRestoreSuccessItem) Core.. Lens.mapping Core._Time

instance
  Core.FromXML
    DescribeFastSnapshotRestoreSuccessItem
  where
  parseXML x =
    DescribeFastSnapshotRestoreSuccessItem'
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
    DescribeFastSnapshotRestoreSuccessItem

instance
  Core.NFData
    DescribeFastSnapshotRestoreSuccessItem
