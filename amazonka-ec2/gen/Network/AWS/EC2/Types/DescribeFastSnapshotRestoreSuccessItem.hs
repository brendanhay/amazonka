{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FastSnapshotRestoreStateCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes fast snapshot restores for a snapshot.
--
-- /See:/ 'newDescribeFastSnapshotRestoreSuccessItem' smart constructor.
data DescribeFastSnapshotRestoreSuccessItem = DescribeFastSnapshotRestoreSuccessItem'
  { -- | The AWS owner alias that enabled fast snapshot restores on the snapshot.
    -- This is intended for future use.
    ownerAlias :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AWS account that enabled fast snapshot restores on the
    -- snapshot.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The reason for the state transition. The possible values are as follows:
    --
    -- -   @Client.UserInitiated@ - The state successfully transitioned to
    --     @enabling@ or @disabling@.
    --
    -- -   @Client.UserInitiated - Lifecycle state transition@ - The state
    --     successfully transitioned to @optimizing@, @enabled@, or @disabled@.
    stateTransitionReason :: Prelude.Maybe Prelude.Text,
    -- | The time at which fast snapshot restores entered the @optimizing@ state.
    optimizingTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The state of fast snapshot restores.
    state :: Prelude.Maybe FastSnapshotRestoreStateCode,
    -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The time at which fast snapshot restores entered the @disabling@ state.
    disablingTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The time at which fast snapshot restores entered the @enabling@ state.
    enablingTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The time at which fast snapshot restores entered the @enabled@ state.
    enabledTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The time at which fast snapshot restores entered the @disabled@ state.
    disabledTime :: Prelude.Maybe Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      ownerId = Prelude.Nothing,
      stateTransitionReason =
        Prelude.Nothing,
      optimizingTime = Prelude.Nothing,
      state = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      disablingTime = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      enablingTime = Prelude.Nothing,
      enabledTime = Prelude.Nothing,
      disabledTime = Prelude.Nothing
    }

-- | The AWS owner alias that enabled fast snapshot restores on the snapshot.
-- This is intended for future use.
describeFastSnapshotRestoreSuccessItem_ownerAlias :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
describeFastSnapshotRestoreSuccessItem_ownerAlias = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {ownerAlias} -> ownerAlias) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {ownerAlias = a} :: DescribeFastSnapshotRestoreSuccessItem)

-- | The ID of the AWS account that enabled fast snapshot restores on the
-- snapshot.
describeFastSnapshotRestoreSuccessItem_ownerId :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
describeFastSnapshotRestoreSuccessItem_ownerId = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {ownerId} -> ownerId) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {ownerId = a} :: DescribeFastSnapshotRestoreSuccessItem)

-- | The reason for the state transition. The possible values are as follows:
--
-- -   @Client.UserInitiated@ - The state successfully transitioned to
--     @enabling@ or @disabling@.
--
-- -   @Client.UserInitiated - Lifecycle state transition@ - The state
--     successfully transitioned to @optimizing@, @enabled@, or @disabled@.
describeFastSnapshotRestoreSuccessItem_stateTransitionReason :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
describeFastSnapshotRestoreSuccessItem_stateTransitionReason = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {stateTransitionReason} -> stateTransitionReason) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {stateTransitionReason = a} :: DescribeFastSnapshotRestoreSuccessItem)

-- | The time at which fast snapshot restores entered the @optimizing@ state.
describeFastSnapshotRestoreSuccessItem_optimizingTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
describeFastSnapshotRestoreSuccessItem_optimizingTime = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {optimizingTime} -> optimizingTime) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {optimizingTime = a} :: DescribeFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Prelude._Time

-- | The state of fast snapshot restores.
describeFastSnapshotRestoreSuccessItem_state :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe FastSnapshotRestoreStateCode)
describeFastSnapshotRestoreSuccessItem_state = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {state} -> state) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {state = a} :: DescribeFastSnapshotRestoreSuccessItem)

-- | The Availability Zone.
describeFastSnapshotRestoreSuccessItem_availabilityZone :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
describeFastSnapshotRestoreSuccessItem_availabilityZone = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {availabilityZone} -> availabilityZone) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {availabilityZone = a} :: DescribeFastSnapshotRestoreSuccessItem)

-- | The time at which fast snapshot restores entered the @disabling@ state.
describeFastSnapshotRestoreSuccessItem_disablingTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
describeFastSnapshotRestoreSuccessItem_disablingTime = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {disablingTime} -> disablingTime) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {disablingTime = a} :: DescribeFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the snapshot.
describeFastSnapshotRestoreSuccessItem_snapshotId :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
describeFastSnapshotRestoreSuccessItem_snapshotId = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {snapshotId} -> snapshotId) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {snapshotId = a} :: DescribeFastSnapshotRestoreSuccessItem)

-- | The time at which fast snapshot restores entered the @enabling@ state.
describeFastSnapshotRestoreSuccessItem_enablingTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
describeFastSnapshotRestoreSuccessItem_enablingTime = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {enablingTime} -> enablingTime) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {enablingTime = a} :: DescribeFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Prelude._Time

-- | The time at which fast snapshot restores entered the @enabled@ state.
describeFastSnapshotRestoreSuccessItem_enabledTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
describeFastSnapshotRestoreSuccessItem_enabledTime = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {enabledTime} -> enabledTime) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {enabledTime = a} :: DescribeFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Prelude._Time

-- | The time at which fast snapshot restores entered the @disabled@ state.
describeFastSnapshotRestoreSuccessItem_disabledTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
describeFastSnapshotRestoreSuccessItem_disabledTime = Lens.lens (\DescribeFastSnapshotRestoreSuccessItem' {disabledTime} -> disabledTime) (\s@DescribeFastSnapshotRestoreSuccessItem' {} a -> s {disabledTime = a} :: DescribeFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Prelude._Time

instance
  Prelude.FromXML
    DescribeFastSnapshotRestoreSuccessItem
  where
  parseXML x =
    DescribeFastSnapshotRestoreSuccessItem'
      Prelude.<$> (x Prelude..@? "ownerAlias")
      Prelude.<*> (x Prelude..@? "ownerId")
      Prelude.<*> (x Prelude..@? "stateTransitionReason")
      Prelude.<*> (x Prelude..@? "optimizingTime")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "availabilityZone")
      Prelude.<*> (x Prelude..@? "disablingTime")
      Prelude.<*> (x Prelude..@? "snapshotId")
      Prelude.<*> (x Prelude..@? "enablingTime")
      Prelude.<*> (x Prelude..@? "enabledTime")
      Prelude.<*> (x Prelude..@? "disabledTime")

instance
  Prelude.Hashable
    DescribeFastSnapshotRestoreSuccessItem

instance
  Prelude.NFData
    DescribeFastSnapshotRestoreSuccessItem
