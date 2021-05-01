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
-- Module      : Network.AWS.EC2.Types.DisableFastSnapshotRestoreSuccessItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DisableFastSnapshotRestoreSuccessItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FastSnapshotRestoreStateCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes fast snapshot restores that were successfully disabled.
--
-- /See:/ 'newDisableFastSnapshotRestoreSuccessItem' smart constructor.
data DisableFastSnapshotRestoreSuccessItem = DisableFastSnapshotRestoreSuccessItem'
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
    -- | The state of fast snapshot restores for the snapshot.
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
-- Create a value of 'DisableFastSnapshotRestoreSuccessItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerAlias', 'disableFastSnapshotRestoreSuccessItem_ownerAlias' - The AWS owner alias that enabled fast snapshot restores on the snapshot.
-- This is intended for future use.
--
-- 'ownerId', 'disableFastSnapshotRestoreSuccessItem_ownerId' - The ID of the AWS account that enabled fast snapshot restores on the
-- snapshot.
--
-- 'stateTransitionReason', 'disableFastSnapshotRestoreSuccessItem_stateTransitionReason' - The reason for the state transition. The possible values are as follows:
--
-- -   @Client.UserInitiated@ - The state successfully transitioned to
--     @enabling@ or @disabling@.
--
-- -   @Client.UserInitiated - Lifecycle state transition@ - The state
--     successfully transitioned to @optimizing@, @enabled@, or @disabled@.
--
-- 'optimizingTime', 'disableFastSnapshotRestoreSuccessItem_optimizingTime' - The time at which fast snapshot restores entered the @optimizing@ state.
--
-- 'state', 'disableFastSnapshotRestoreSuccessItem_state' - The state of fast snapshot restores for the snapshot.
--
-- 'availabilityZone', 'disableFastSnapshotRestoreSuccessItem_availabilityZone' - The Availability Zone.
--
-- 'disablingTime', 'disableFastSnapshotRestoreSuccessItem_disablingTime' - The time at which fast snapshot restores entered the @disabling@ state.
--
-- 'snapshotId', 'disableFastSnapshotRestoreSuccessItem_snapshotId' - The ID of the snapshot.
--
-- 'enablingTime', 'disableFastSnapshotRestoreSuccessItem_enablingTime' - The time at which fast snapshot restores entered the @enabling@ state.
--
-- 'enabledTime', 'disableFastSnapshotRestoreSuccessItem_enabledTime' - The time at which fast snapshot restores entered the @enabled@ state.
--
-- 'disabledTime', 'disableFastSnapshotRestoreSuccessItem_disabledTime' - The time at which fast snapshot restores entered the @disabled@ state.
newDisableFastSnapshotRestoreSuccessItem ::
  DisableFastSnapshotRestoreSuccessItem
newDisableFastSnapshotRestoreSuccessItem =
  DisableFastSnapshotRestoreSuccessItem'
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
disableFastSnapshotRestoreSuccessItem_ownerAlias :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
disableFastSnapshotRestoreSuccessItem_ownerAlias = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {ownerAlias} -> ownerAlias) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {ownerAlias = a} :: DisableFastSnapshotRestoreSuccessItem)

-- | The ID of the AWS account that enabled fast snapshot restores on the
-- snapshot.
disableFastSnapshotRestoreSuccessItem_ownerId :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
disableFastSnapshotRestoreSuccessItem_ownerId = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {ownerId} -> ownerId) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {ownerId = a} :: DisableFastSnapshotRestoreSuccessItem)

-- | The reason for the state transition. The possible values are as follows:
--
-- -   @Client.UserInitiated@ - The state successfully transitioned to
--     @enabling@ or @disabling@.
--
-- -   @Client.UserInitiated - Lifecycle state transition@ - The state
--     successfully transitioned to @optimizing@, @enabled@, or @disabled@.
disableFastSnapshotRestoreSuccessItem_stateTransitionReason :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
disableFastSnapshotRestoreSuccessItem_stateTransitionReason = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {stateTransitionReason} -> stateTransitionReason) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {stateTransitionReason = a} :: DisableFastSnapshotRestoreSuccessItem)

-- | The time at which fast snapshot restores entered the @optimizing@ state.
disableFastSnapshotRestoreSuccessItem_optimizingTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
disableFastSnapshotRestoreSuccessItem_optimizingTime = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {optimizingTime} -> optimizingTime) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {optimizingTime = a} :: DisableFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Prelude._Time

-- | The state of fast snapshot restores for the snapshot.
disableFastSnapshotRestoreSuccessItem_state :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe FastSnapshotRestoreStateCode)
disableFastSnapshotRestoreSuccessItem_state = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {state} -> state) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {state = a} :: DisableFastSnapshotRestoreSuccessItem)

-- | The Availability Zone.
disableFastSnapshotRestoreSuccessItem_availabilityZone :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
disableFastSnapshotRestoreSuccessItem_availabilityZone = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {availabilityZone} -> availabilityZone) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {availabilityZone = a} :: DisableFastSnapshotRestoreSuccessItem)

-- | The time at which fast snapshot restores entered the @disabling@ state.
disableFastSnapshotRestoreSuccessItem_disablingTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
disableFastSnapshotRestoreSuccessItem_disablingTime = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {disablingTime} -> disablingTime) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {disablingTime = a} :: DisableFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the snapshot.
disableFastSnapshotRestoreSuccessItem_snapshotId :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.Text)
disableFastSnapshotRestoreSuccessItem_snapshotId = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {snapshotId} -> snapshotId) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {snapshotId = a} :: DisableFastSnapshotRestoreSuccessItem)

-- | The time at which fast snapshot restores entered the @enabling@ state.
disableFastSnapshotRestoreSuccessItem_enablingTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
disableFastSnapshotRestoreSuccessItem_enablingTime = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {enablingTime} -> enablingTime) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {enablingTime = a} :: DisableFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Prelude._Time

-- | The time at which fast snapshot restores entered the @enabled@ state.
disableFastSnapshotRestoreSuccessItem_enabledTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
disableFastSnapshotRestoreSuccessItem_enabledTime = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {enabledTime} -> enabledTime) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {enabledTime = a} :: DisableFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Prelude._Time

-- | The time at which fast snapshot restores entered the @disabled@ state.
disableFastSnapshotRestoreSuccessItem_disabledTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Prelude.Maybe Prelude.UTCTime)
disableFastSnapshotRestoreSuccessItem_disabledTime = Lens.lens (\DisableFastSnapshotRestoreSuccessItem' {disabledTime} -> disabledTime) (\s@DisableFastSnapshotRestoreSuccessItem' {} a -> s {disabledTime = a} :: DisableFastSnapshotRestoreSuccessItem) Prelude.. Lens.mapping Prelude._Time

instance
  Prelude.FromXML
    DisableFastSnapshotRestoreSuccessItem
  where
  parseXML x =
    DisableFastSnapshotRestoreSuccessItem'
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
    DisableFastSnapshotRestoreSuccessItem

instance
  Prelude.NFData
    DisableFastSnapshotRestoreSuccessItem
