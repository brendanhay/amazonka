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
-- Module      : Network.AWS.SSM.Types.InstancePatchState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstancePatchState where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.PatchOperationType
import Network.AWS.SSM.Types.RebootOption

-- | Defines the high-level patch compliance state for a managed instance,
-- providing information about the number of installed, missing, not
-- applicable, and failed patches along with metadata about the operation
-- when this information was gathered for the instance.
--
-- /See:/ 'newInstancePatchState' smart constructor.
data InstancePatchState = InstancePatchState'
  { -- | An https URL or an Amazon S3 path-style URL to a list of patches to be
    -- installed. This patch installation list, which you maintain in an S3
    -- bucket in YAML format and specify in the SSM document
    -- @AWS-RunPatchBaseline@, overrides the patches specified by the default
    -- patch baseline.
    --
    -- For more information about the @InstallOverrideList@ parameter, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-about-aws-runpatchbaseline.html About the SSM document AWS-RunPatchBaseline>
    -- in the /AWS Systems Manager User Guide/.
    installOverrideList :: Prelude.Maybe Prelude.Text,
    -- | The number of patches beyond the supported limit of @NotApplicableCount@
    -- that are not reported by name to Systems Manager Inventory.
    unreportedNotApplicableCount :: Prelude.Maybe Prelude.Int,
    -- | The number of patches not specified in the patch baseline that are
    -- installed on the instance.
    installedOtherCount :: Prelude.Maybe Prelude.Int,
    -- | The number of patches installed by Patch Manager since the last time the
    -- instance was rebooted.
    installedPendingRebootCount :: Prelude.Maybe Prelude.Int,
    -- | Indicates the reboot option specified in the patch baseline.
    --
    -- Reboot options apply to @Install@ operations only. Reboots are not
    -- attempted for Patch Manager @Scan@ operations.
    --
    -- -   __RebootIfNeeded__: Patch Manager tries to reboot the instance if it
    --     installed any patches, or if any patches are detected with a status
    --     of @InstalledPendingReboot@.
    --
    -- -   __NoReboot__: Patch Manager attempts to install missing packages
    --     without trying to reboot the system. Patches installed with this
    --     option are assigned a status of @InstalledPendingReboot@. These
    --     patches might not be in effect until a reboot is performed.
    rebootOption :: Prelude.Maybe RebootOption,
    -- | The number of patches from the patch baseline that are applicable for
    -- the instance but aren\'t currently installed.
    missingCount :: Prelude.Maybe Prelude.Int,
    -- | The ID of the patch baseline snapshot used during the patching operation
    -- when this compliance data was collected.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The number of patches from the patch baseline that are installed on the
    -- instance.
    installedCount :: Prelude.Maybe Prelude.Int,
    -- | The time of the last attempt to patch the instance with @NoReboot@
    -- specified as the reboot option.
    lastNoRebootInstallOperationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The number of patches from the patch baseline that aren\'t applicable
    -- for the instance and therefore aren\'t installed on the instance. This
    -- number may be truncated if the list of patch names is very large. The
    -- number of patches beyond this limit are reported in
    -- @UnreportedNotApplicableCount@.
    notApplicableCount :: Prelude.Maybe Prelude.Int,
    -- | The number of patches from the patch baseline that were attempted to be
    -- installed during the last patching operation, but failed to install.
    failedCount :: Prelude.Maybe Prelude.Int,
    -- | Placeholder information. This field will always be empty in the current
    -- release of the service.
    ownerInformation :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The number of patches installed on an instance that are specified in a
    -- @RejectedPatches@ list. Patches with a status of /InstalledRejected/
    -- were typically installed before they were added to a @RejectedPatches@
    -- list.
    --
    -- If @ALLOW_AS_DEPENDENCY@ is the specified option for
    -- @RejectedPatchesAction@, the value of @InstalledRejectedCount@ will
    -- always be @0@ (zero).
    installedRejectedCount :: Prelude.Maybe Prelude.Int,
    -- | The ID of the managed instance the high-level patch compliance
    -- information was collected for.
    instanceId :: Prelude.Text,
    -- | The name of the patch group the managed instance belongs to.
    patchGroup :: Prelude.Text,
    -- | The ID of the patch baseline used to patch the instance.
    baselineId :: Prelude.Text,
    -- | The time the most recent patching operation was started on the instance.
    operationStartTime :: Prelude.POSIX,
    -- | The time the most recent patching operation completed on the instance.
    operationEndTime :: Prelude.POSIX,
    -- | The type of patching operation that was performed: @SCAN@ (assess patch
    -- compliance state) or @INSTALL@ (install missing patches).
    operation :: PatchOperationType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstancePatchState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'installOverrideList', 'instancePatchState_installOverrideList' - An https URL or an Amazon S3 path-style URL to a list of patches to be
-- installed. This patch installation list, which you maintain in an S3
-- bucket in YAML format and specify in the SSM document
-- @AWS-RunPatchBaseline@, overrides the patches specified by the default
-- patch baseline.
--
-- For more information about the @InstallOverrideList@ parameter, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-about-aws-runpatchbaseline.html About the SSM document AWS-RunPatchBaseline>
-- in the /AWS Systems Manager User Guide/.
--
-- 'unreportedNotApplicableCount', 'instancePatchState_unreportedNotApplicableCount' - The number of patches beyond the supported limit of @NotApplicableCount@
-- that are not reported by name to Systems Manager Inventory.
--
-- 'installedOtherCount', 'instancePatchState_installedOtherCount' - The number of patches not specified in the patch baseline that are
-- installed on the instance.
--
-- 'installedPendingRebootCount', 'instancePatchState_installedPendingRebootCount' - The number of patches installed by Patch Manager since the last time the
-- instance was rebooted.
--
-- 'rebootOption', 'instancePatchState_rebootOption' - Indicates the reboot option specified in the patch baseline.
--
-- Reboot options apply to @Install@ operations only. Reboots are not
-- attempted for Patch Manager @Scan@ operations.
--
-- -   __RebootIfNeeded__: Patch Manager tries to reboot the instance if it
--     installed any patches, or if any patches are detected with a status
--     of @InstalledPendingReboot@.
--
-- -   __NoReboot__: Patch Manager attempts to install missing packages
--     without trying to reboot the system. Patches installed with this
--     option are assigned a status of @InstalledPendingReboot@. These
--     patches might not be in effect until a reboot is performed.
--
-- 'missingCount', 'instancePatchState_missingCount' - The number of patches from the patch baseline that are applicable for
-- the instance but aren\'t currently installed.
--
-- 'snapshotId', 'instancePatchState_snapshotId' - The ID of the patch baseline snapshot used during the patching operation
-- when this compliance data was collected.
--
-- 'installedCount', 'instancePatchState_installedCount' - The number of patches from the patch baseline that are installed on the
-- instance.
--
-- 'lastNoRebootInstallOperationTime', 'instancePatchState_lastNoRebootInstallOperationTime' - The time of the last attempt to patch the instance with @NoReboot@
-- specified as the reboot option.
--
-- 'notApplicableCount', 'instancePatchState_notApplicableCount' - The number of patches from the patch baseline that aren\'t applicable
-- for the instance and therefore aren\'t installed on the instance. This
-- number may be truncated if the list of patch names is very large. The
-- number of patches beyond this limit are reported in
-- @UnreportedNotApplicableCount@.
--
-- 'failedCount', 'instancePatchState_failedCount' - The number of patches from the patch baseline that were attempted to be
-- installed during the last patching operation, but failed to install.
--
-- 'ownerInformation', 'instancePatchState_ownerInformation' - Placeholder information. This field will always be empty in the current
-- release of the service.
--
-- 'installedRejectedCount', 'instancePatchState_installedRejectedCount' - The number of patches installed on an instance that are specified in a
-- @RejectedPatches@ list. Patches with a status of /InstalledRejected/
-- were typically installed before they were added to a @RejectedPatches@
-- list.
--
-- If @ALLOW_AS_DEPENDENCY@ is the specified option for
-- @RejectedPatchesAction@, the value of @InstalledRejectedCount@ will
-- always be @0@ (zero).
--
-- 'instanceId', 'instancePatchState_instanceId' - The ID of the managed instance the high-level patch compliance
-- information was collected for.
--
-- 'patchGroup', 'instancePatchState_patchGroup' - The name of the patch group the managed instance belongs to.
--
-- 'baselineId', 'instancePatchState_baselineId' - The ID of the patch baseline used to patch the instance.
--
-- 'operationStartTime', 'instancePatchState_operationStartTime' - The time the most recent patching operation was started on the instance.
--
-- 'operationEndTime', 'instancePatchState_operationEndTime' - The time the most recent patching operation completed on the instance.
--
-- 'operation', 'instancePatchState_operation' - The type of patching operation that was performed: @SCAN@ (assess patch
-- compliance state) or @INSTALL@ (install missing patches).
newInstancePatchState ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'patchGroup'
  Prelude.Text ->
  -- | 'baselineId'
  Prelude.Text ->
  -- | 'operationStartTime'
  Prelude.UTCTime ->
  -- | 'operationEndTime'
  Prelude.UTCTime ->
  -- | 'operation'
  PatchOperationType ->
  InstancePatchState
newInstancePatchState
  pInstanceId_
  pPatchGroup_
  pBaselineId_
  pOperationStartTime_
  pOperationEndTime_
  pOperation_ =
    InstancePatchState'
      { installOverrideList =
          Prelude.Nothing,
        unreportedNotApplicableCount = Prelude.Nothing,
        installedOtherCount = Prelude.Nothing,
        installedPendingRebootCount = Prelude.Nothing,
        rebootOption = Prelude.Nothing,
        missingCount = Prelude.Nothing,
        snapshotId = Prelude.Nothing,
        installedCount = Prelude.Nothing,
        lastNoRebootInstallOperationTime = Prelude.Nothing,
        notApplicableCount = Prelude.Nothing,
        failedCount = Prelude.Nothing,
        ownerInformation = Prelude.Nothing,
        installedRejectedCount = Prelude.Nothing,
        instanceId = pInstanceId_,
        patchGroup = pPatchGroup_,
        baselineId = pBaselineId_,
        operationStartTime =
          Prelude._Time Lens.# pOperationStartTime_,
        operationEndTime =
          Prelude._Time Lens.# pOperationEndTime_,
        operation = pOperation_
      }

-- | An https URL or an Amazon S3 path-style URL to a list of patches to be
-- installed. This patch installation list, which you maintain in an S3
-- bucket in YAML format and specify in the SSM document
-- @AWS-RunPatchBaseline@, overrides the patches specified by the default
-- patch baseline.
--
-- For more information about the @InstallOverrideList@ parameter, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-about-aws-runpatchbaseline.html About the SSM document AWS-RunPatchBaseline>
-- in the /AWS Systems Manager User Guide/.
instancePatchState_installOverrideList :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Text)
instancePatchState_installOverrideList = Lens.lens (\InstancePatchState' {installOverrideList} -> installOverrideList) (\s@InstancePatchState' {} a -> s {installOverrideList = a} :: InstancePatchState)

-- | The number of patches beyond the supported limit of @NotApplicableCount@
-- that are not reported by name to Systems Manager Inventory.
instancePatchState_unreportedNotApplicableCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_unreportedNotApplicableCount = Lens.lens (\InstancePatchState' {unreportedNotApplicableCount} -> unreportedNotApplicableCount) (\s@InstancePatchState' {} a -> s {unreportedNotApplicableCount = a} :: InstancePatchState)

-- | The number of patches not specified in the patch baseline that are
-- installed on the instance.
instancePatchState_installedOtherCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_installedOtherCount = Lens.lens (\InstancePatchState' {installedOtherCount} -> installedOtherCount) (\s@InstancePatchState' {} a -> s {installedOtherCount = a} :: InstancePatchState)

-- | The number of patches installed by Patch Manager since the last time the
-- instance was rebooted.
instancePatchState_installedPendingRebootCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_installedPendingRebootCount = Lens.lens (\InstancePatchState' {installedPendingRebootCount} -> installedPendingRebootCount) (\s@InstancePatchState' {} a -> s {installedPendingRebootCount = a} :: InstancePatchState)

-- | Indicates the reboot option specified in the patch baseline.
--
-- Reboot options apply to @Install@ operations only. Reboots are not
-- attempted for Patch Manager @Scan@ operations.
--
-- -   __RebootIfNeeded__: Patch Manager tries to reboot the instance if it
--     installed any patches, or if any patches are detected with a status
--     of @InstalledPendingReboot@.
--
-- -   __NoReboot__: Patch Manager attempts to install missing packages
--     without trying to reboot the system. Patches installed with this
--     option are assigned a status of @InstalledPendingReboot@. These
--     patches might not be in effect until a reboot is performed.
instancePatchState_rebootOption :: Lens.Lens' InstancePatchState (Prelude.Maybe RebootOption)
instancePatchState_rebootOption = Lens.lens (\InstancePatchState' {rebootOption} -> rebootOption) (\s@InstancePatchState' {} a -> s {rebootOption = a} :: InstancePatchState)

-- | The number of patches from the patch baseline that are applicable for
-- the instance but aren\'t currently installed.
instancePatchState_missingCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_missingCount = Lens.lens (\InstancePatchState' {missingCount} -> missingCount) (\s@InstancePatchState' {} a -> s {missingCount = a} :: InstancePatchState)

-- | The ID of the patch baseline snapshot used during the patching operation
-- when this compliance data was collected.
instancePatchState_snapshotId :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Text)
instancePatchState_snapshotId = Lens.lens (\InstancePatchState' {snapshotId} -> snapshotId) (\s@InstancePatchState' {} a -> s {snapshotId = a} :: InstancePatchState)

-- | The number of patches from the patch baseline that are installed on the
-- instance.
instancePatchState_installedCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_installedCount = Lens.lens (\InstancePatchState' {installedCount} -> installedCount) (\s@InstancePatchState' {} a -> s {installedCount = a} :: InstancePatchState)

-- | The time of the last attempt to patch the instance with @NoReboot@
-- specified as the reboot option.
instancePatchState_lastNoRebootInstallOperationTime :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.UTCTime)
instancePatchState_lastNoRebootInstallOperationTime = Lens.lens (\InstancePatchState' {lastNoRebootInstallOperationTime} -> lastNoRebootInstallOperationTime) (\s@InstancePatchState' {} a -> s {lastNoRebootInstallOperationTime = a} :: InstancePatchState) Prelude.. Lens.mapping Prelude._Time

-- | The number of patches from the patch baseline that aren\'t applicable
-- for the instance and therefore aren\'t installed on the instance. This
-- number may be truncated if the list of patch names is very large. The
-- number of patches beyond this limit are reported in
-- @UnreportedNotApplicableCount@.
instancePatchState_notApplicableCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_notApplicableCount = Lens.lens (\InstancePatchState' {notApplicableCount} -> notApplicableCount) (\s@InstancePatchState' {} a -> s {notApplicableCount = a} :: InstancePatchState)

-- | The number of patches from the patch baseline that were attempted to be
-- installed during the last patching operation, but failed to install.
instancePatchState_failedCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_failedCount = Lens.lens (\InstancePatchState' {failedCount} -> failedCount) (\s@InstancePatchState' {} a -> s {failedCount = a} :: InstancePatchState)

-- | Placeholder information. This field will always be empty in the current
-- release of the service.
instancePatchState_ownerInformation :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Text)
instancePatchState_ownerInformation = Lens.lens (\InstancePatchState' {ownerInformation} -> ownerInformation) (\s@InstancePatchState' {} a -> s {ownerInformation = a} :: InstancePatchState) Prelude.. Lens.mapping Prelude._Sensitive

-- | The number of patches installed on an instance that are specified in a
-- @RejectedPatches@ list. Patches with a status of /InstalledRejected/
-- were typically installed before they were added to a @RejectedPatches@
-- list.
--
-- If @ALLOW_AS_DEPENDENCY@ is the specified option for
-- @RejectedPatchesAction@, the value of @InstalledRejectedCount@ will
-- always be @0@ (zero).
instancePatchState_installedRejectedCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_installedRejectedCount = Lens.lens (\InstancePatchState' {installedRejectedCount} -> installedRejectedCount) (\s@InstancePatchState' {} a -> s {installedRejectedCount = a} :: InstancePatchState)

-- | The ID of the managed instance the high-level patch compliance
-- information was collected for.
instancePatchState_instanceId :: Lens.Lens' InstancePatchState Prelude.Text
instancePatchState_instanceId = Lens.lens (\InstancePatchState' {instanceId} -> instanceId) (\s@InstancePatchState' {} a -> s {instanceId = a} :: InstancePatchState)

-- | The name of the patch group the managed instance belongs to.
instancePatchState_patchGroup :: Lens.Lens' InstancePatchState Prelude.Text
instancePatchState_patchGroup = Lens.lens (\InstancePatchState' {patchGroup} -> patchGroup) (\s@InstancePatchState' {} a -> s {patchGroup = a} :: InstancePatchState)

-- | The ID of the patch baseline used to patch the instance.
instancePatchState_baselineId :: Lens.Lens' InstancePatchState Prelude.Text
instancePatchState_baselineId = Lens.lens (\InstancePatchState' {baselineId} -> baselineId) (\s@InstancePatchState' {} a -> s {baselineId = a} :: InstancePatchState)

-- | The time the most recent patching operation was started on the instance.
instancePatchState_operationStartTime :: Lens.Lens' InstancePatchState Prelude.UTCTime
instancePatchState_operationStartTime = Lens.lens (\InstancePatchState' {operationStartTime} -> operationStartTime) (\s@InstancePatchState' {} a -> s {operationStartTime = a} :: InstancePatchState) Prelude.. Prelude._Time

-- | The time the most recent patching operation completed on the instance.
instancePatchState_operationEndTime :: Lens.Lens' InstancePatchState Prelude.UTCTime
instancePatchState_operationEndTime = Lens.lens (\InstancePatchState' {operationEndTime} -> operationEndTime) (\s@InstancePatchState' {} a -> s {operationEndTime = a} :: InstancePatchState) Prelude.. Prelude._Time

-- | The type of patching operation that was performed: @SCAN@ (assess patch
-- compliance state) or @INSTALL@ (install missing patches).
instancePatchState_operation :: Lens.Lens' InstancePatchState PatchOperationType
instancePatchState_operation = Lens.lens (\InstancePatchState' {operation} -> operation) (\s@InstancePatchState' {} a -> s {operation = a} :: InstancePatchState)

instance Prelude.FromJSON InstancePatchState where
  parseJSON =
    Prelude.withObject
      "InstancePatchState"
      ( \x ->
          InstancePatchState'
            Prelude.<$> (x Prelude..:? "InstallOverrideList")
            Prelude.<*> (x Prelude..:? "UnreportedNotApplicableCount")
            Prelude.<*> (x Prelude..:? "InstalledOtherCount")
            Prelude.<*> (x Prelude..:? "InstalledPendingRebootCount")
            Prelude.<*> (x Prelude..:? "RebootOption")
            Prelude.<*> (x Prelude..:? "MissingCount")
            Prelude.<*> (x Prelude..:? "SnapshotId")
            Prelude.<*> (x Prelude..:? "InstalledCount")
            Prelude.<*> (x Prelude..:? "LastNoRebootInstallOperationTime")
            Prelude.<*> (x Prelude..:? "NotApplicableCount")
            Prelude.<*> (x Prelude..:? "FailedCount")
            Prelude.<*> (x Prelude..:? "OwnerInformation")
            Prelude.<*> (x Prelude..:? "InstalledRejectedCount")
            Prelude.<*> (x Prelude..: "InstanceId")
            Prelude.<*> (x Prelude..: "PatchGroup")
            Prelude.<*> (x Prelude..: "BaselineId")
            Prelude.<*> (x Prelude..: "OperationStartTime")
            Prelude.<*> (x Prelude..: "OperationEndTime")
            Prelude.<*> (x Prelude..: "Operation")
      )

instance Prelude.Hashable InstancePatchState

instance Prelude.NFData InstancePatchState
