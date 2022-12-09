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
-- Module      : Amazonka.SSM.Types.InstancePatchState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InstancePatchState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.PatchOperationType
import Amazonka.SSM.Types.RebootOption

-- | Defines the high-level patch compliance state for a managed node,
-- providing information about the number of installed, missing, not
-- applicable, and failed patches along with metadata about the operation
-- when this information was gathered for the managed node.
--
-- /See:/ 'newInstancePatchState' smart constructor.
data InstancePatchState = InstancePatchState'
  { -- | The number of patches per node that are specified as @Critical@ for
    -- compliance reporting in the patch baseline aren\'t installed. These
    -- patches might be missing, have failed installation, were rejected, or
    -- were installed but awaiting a required managed node reboot. The status
    -- of these managed nodes is @NON_COMPLIANT@.
    criticalNonCompliantCount :: Prelude.Maybe Prelude.Int,
    -- | The number of patches from the patch baseline that were attempted to be
    -- installed during the last patching operation, but failed to install.
    failedCount :: Prelude.Maybe Prelude.Int,
    -- | An https URL or an Amazon Simple Storage Service (Amazon S3) path-style
    -- URL to a list of patches to be installed. This patch installation list,
    -- which you maintain in an S3 bucket in YAML format and specify in the SSM
    -- document @AWS-RunPatchBaseline@, overrides the patches specified by the
    -- default patch baseline.
    --
    -- For more information about the @InstallOverrideList@ parameter, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-about-aws-runpatchbaseline.html About the AWS-RunPatchBaseline>
    -- SSM document in the /Amazon Web Services Systems Manager User Guide/.
    installOverrideList :: Prelude.Maybe Prelude.Text,
    -- | The number of patches from the patch baseline that are installed on the
    -- managed node.
    installedCount :: Prelude.Maybe Prelude.Int,
    -- | The number of patches not specified in the patch baseline that are
    -- installed on the managed node.
    installedOtherCount :: Prelude.Maybe Prelude.Int,
    -- | The number of patches installed by Patch Manager since the last time the
    -- managed node was rebooted.
    installedPendingRebootCount :: Prelude.Maybe Prelude.Int,
    -- | The number of patches installed on a managed node that are specified in
    -- a @RejectedPatches@ list. Patches with a status of @InstalledRejected@
    -- were typically installed before they were added to a @RejectedPatches@
    -- list.
    --
    -- If @ALLOW_AS_DEPENDENCY@ is the specified option for
    -- @RejectedPatchesAction@, the value of @InstalledRejectedCount@ will
    -- always be @0@ (zero).
    installedRejectedCount :: Prelude.Maybe Prelude.Int,
    -- | The time of the last attempt to patch the managed node with @NoReboot@
    -- specified as the reboot option.
    lastNoRebootInstallOperationTime :: Prelude.Maybe Data.POSIX,
    -- | The number of patches from the patch baseline that are applicable for
    -- the managed node but aren\'t currently installed.
    missingCount :: Prelude.Maybe Prelude.Int,
    -- | The number of patches from the patch baseline that aren\'t applicable
    -- for the managed node and therefore aren\'t installed on the node. This
    -- number may be truncated if the list of patch names is very large. The
    -- number of patches beyond this limit are reported in
    -- @UnreportedNotApplicableCount@.
    notApplicableCount :: Prelude.Maybe Prelude.Int,
    -- | The number of patches per node that are specified as other than
    -- @Critical@ or @Security@ but aren\'t compliant with the patch baseline.
    -- The status of these managed nodes is @NON_COMPLIANT@.
    otherNonCompliantCount :: Prelude.Maybe Prelude.Int,
    -- | Placeholder information. This field will always be empty in the current
    -- release of the service.
    ownerInformation :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Indicates the reboot option specified in the patch baseline.
    --
    -- Reboot options apply to @Install@ operations only. Reboots aren\'t
    -- attempted for Patch Manager @Scan@ operations.
    --
    -- -   @RebootIfNeeded@: Patch Manager tries to reboot the managed node if
    --     it installed any patches, or if any patches are detected with a
    --     status of @InstalledPendingReboot@.
    --
    -- -   @NoReboot@: Patch Manager attempts to install missing packages
    --     without trying to reboot the system. Patches installed with this
    --     option are assigned a status of @InstalledPendingReboot@. These
    --     patches might not be in effect until a reboot is performed.
    rebootOption :: Prelude.Maybe RebootOption,
    -- | The number of patches per node that are specified as @Security@ in a
    -- patch advisory aren\'t installed. These patches might be missing, have
    -- failed installation, were rejected, or were installed but awaiting a
    -- required managed node reboot. The status of these managed nodes is
    -- @NON_COMPLIANT@.
    securityNonCompliantCount :: Prelude.Maybe Prelude.Int,
    -- | The ID of the patch baseline snapshot used during the patching operation
    -- when this compliance data was collected.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The number of patches beyond the supported limit of @NotApplicableCount@
    -- that aren\'t reported by name to Inventory. Inventory is a capability of
    -- Amazon Web Services Systems Manager.
    unreportedNotApplicableCount :: Prelude.Maybe Prelude.Int,
    -- | The ID of the managed node the high-level patch compliance information
    -- was collected for.
    instanceId :: Prelude.Text,
    -- | The name of the patch group the managed node belongs to.
    patchGroup :: Prelude.Text,
    -- | The ID of the patch baseline used to patch the managed node.
    baselineId :: Prelude.Text,
    -- | The time the most recent patching operation was started on the managed
    -- node.
    operationStartTime :: Data.POSIX,
    -- | The time the most recent patching operation completed on the managed
    -- node.
    operationEndTime :: Data.POSIX,
    -- | The type of patching operation that was performed: or
    --
    -- -   @SCAN@ assesses the patch compliance state.
    --
    -- -   @INSTALL@ installs missing patches.
    operation :: PatchOperationType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstancePatchState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'criticalNonCompliantCount', 'instancePatchState_criticalNonCompliantCount' - The number of patches per node that are specified as @Critical@ for
-- compliance reporting in the patch baseline aren\'t installed. These
-- patches might be missing, have failed installation, were rejected, or
-- were installed but awaiting a required managed node reboot. The status
-- of these managed nodes is @NON_COMPLIANT@.
--
-- 'failedCount', 'instancePatchState_failedCount' - The number of patches from the patch baseline that were attempted to be
-- installed during the last patching operation, but failed to install.
--
-- 'installOverrideList', 'instancePatchState_installOverrideList' - An https URL or an Amazon Simple Storage Service (Amazon S3) path-style
-- URL to a list of patches to be installed. This patch installation list,
-- which you maintain in an S3 bucket in YAML format and specify in the SSM
-- document @AWS-RunPatchBaseline@, overrides the patches specified by the
-- default patch baseline.
--
-- For more information about the @InstallOverrideList@ parameter, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-about-aws-runpatchbaseline.html About the AWS-RunPatchBaseline>
-- SSM document in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'installedCount', 'instancePatchState_installedCount' - The number of patches from the patch baseline that are installed on the
-- managed node.
--
-- 'installedOtherCount', 'instancePatchState_installedOtherCount' - The number of patches not specified in the patch baseline that are
-- installed on the managed node.
--
-- 'installedPendingRebootCount', 'instancePatchState_installedPendingRebootCount' - The number of patches installed by Patch Manager since the last time the
-- managed node was rebooted.
--
-- 'installedRejectedCount', 'instancePatchState_installedRejectedCount' - The number of patches installed on a managed node that are specified in
-- a @RejectedPatches@ list. Patches with a status of @InstalledRejected@
-- were typically installed before they were added to a @RejectedPatches@
-- list.
--
-- If @ALLOW_AS_DEPENDENCY@ is the specified option for
-- @RejectedPatchesAction@, the value of @InstalledRejectedCount@ will
-- always be @0@ (zero).
--
-- 'lastNoRebootInstallOperationTime', 'instancePatchState_lastNoRebootInstallOperationTime' - The time of the last attempt to patch the managed node with @NoReboot@
-- specified as the reboot option.
--
-- 'missingCount', 'instancePatchState_missingCount' - The number of patches from the patch baseline that are applicable for
-- the managed node but aren\'t currently installed.
--
-- 'notApplicableCount', 'instancePatchState_notApplicableCount' - The number of patches from the patch baseline that aren\'t applicable
-- for the managed node and therefore aren\'t installed on the node. This
-- number may be truncated if the list of patch names is very large. The
-- number of patches beyond this limit are reported in
-- @UnreportedNotApplicableCount@.
--
-- 'otherNonCompliantCount', 'instancePatchState_otherNonCompliantCount' - The number of patches per node that are specified as other than
-- @Critical@ or @Security@ but aren\'t compliant with the patch baseline.
-- The status of these managed nodes is @NON_COMPLIANT@.
--
-- 'ownerInformation', 'instancePatchState_ownerInformation' - Placeholder information. This field will always be empty in the current
-- release of the service.
--
-- 'rebootOption', 'instancePatchState_rebootOption' - Indicates the reboot option specified in the patch baseline.
--
-- Reboot options apply to @Install@ operations only. Reboots aren\'t
-- attempted for Patch Manager @Scan@ operations.
--
-- -   @RebootIfNeeded@: Patch Manager tries to reboot the managed node if
--     it installed any patches, or if any patches are detected with a
--     status of @InstalledPendingReboot@.
--
-- -   @NoReboot@: Patch Manager attempts to install missing packages
--     without trying to reboot the system. Patches installed with this
--     option are assigned a status of @InstalledPendingReboot@. These
--     patches might not be in effect until a reboot is performed.
--
-- 'securityNonCompliantCount', 'instancePatchState_securityNonCompliantCount' - The number of patches per node that are specified as @Security@ in a
-- patch advisory aren\'t installed. These patches might be missing, have
-- failed installation, were rejected, or were installed but awaiting a
-- required managed node reboot. The status of these managed nodes is
-- @NON_COMPLIANT@.
--
-- 'snapshotId', 'instancePatchState_snapshotId' - The ID of the patch baseline snapshot used during the patching operation
-- when this compliance data was collected.
--
-- 'unreportedNotApplicableCount', 'instancePatchState_unreportedNotApplicableCount' - The number of patches beyond the supported limit of @NotApplicableCount@
-- that aren\'t reported by name to Inventory. Inventory is a capability of
-- Amazon Web Services Systems Manager.
--
-- 'instanceId', 'instancePatchState_instanceId' - The ID of the managed node the high-level patch compliance information
-- was collected for.
--
-- 'patchGroup', 'instancePatchState_patchGroup' - The name of the patch group the managed node belongs to.
--
-- 'baselineId', 'instancePatchState_baselineId' - The ID of the patch baseline used to patch the managed node.
--
-- 'operationStartTime', 'instancePatchState_operationStartTime' - The time the most recent patching operation was started on the managed
-- node.
--
-- 'operationEndTime', 'instancePatchState_operationEndTime' - The time the most recent patching operation completed on the managed
-- node.
--
-- 'operation', 'instancePatchState_operation' - The type of patching operation that was performed: or
--
-- -   @SCAN@ assesses the patch compliance state.
--
-- -   @INSTALL@ installs missing patches.
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
      { criticalNonCompliantCount =
          Prelude.Nothing,
        failedCount = Prelude.Nothing,
        installOverrideList = Prelude.Nothing,
        installedCount = Prelude.Nothing,
        installedOtherCount = Prelude.Nothing,
        installedPendingRebootCount = Prelude.Nothing,
        installedRejectedCount = Prelude.Nothing,
        lastNoRebootInstallOperationTime = Prelude.Nothing,
        missingCount = Prelude.Nothing,
        notApplicableCount = Prelude.Nothing,
        otherNonCompliantCount = Prelude.Nothing,
        ownerInformation = Prelude.Nothing,
        rebootOption = Prelude.Nothing,
        securityNonCompliantCount = Prelude.Nothing,
        snapshotId = Prelude.Nothing,
        unreportedNotApplicableCount = Prelude.Nothing,
        instanceId = pInstanceId_,
        patchGroup = pPatchGroup_,
        baselineId = pBaselineId_,
        operationStartTime =
          Data._Time Lens.# pOperationStartTime_,
        operationEndTime =
          Data._Time Lens.# pOperationEndTime_,
        operation = pOperation_
      }

-- | The number of patches per node that are specified as @Critical@ for
-- compliance reporting in the patch baseline aren\'t installed. These
-- patches might be missing, have failed installation, were rejected, or
-- were installed but awaiting a required managed node reboot. The status
-- of these managed nodes is @NON_COMPLIANT@.
instancePatchState_criticalNonCompliantCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_criticalNonCompliantCount = Lens.lens (\InstancePatchState' {criticalNonCompliantCount} -> criticalNonCompliantCount) (\s@InstancePatchState' {} a -> s {criticalNonCompliantCount = a} :: InstancePatchState)

-- | The number of patches from the patch baseline that were attempted to be
-- installed during the last patching operation, but failed to install.
instancePatchState_failedCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_failedCount = Lens.lens (\InstancePatchState' {failedCount} -> failedCount) (\s@InstancePatchState' {} a -> s {failedCount = a} :: InstancePatchState)

-- | An https URL or an Amazon Simple Storage Service (Amazon S3) path-style
-- URL to a list of patches to be installed. This patch installation list,
-- which you maintain in an S3 bucket in YAML format and specify in the SSM
-- document @AWS-RunPatchBaseline@, overrides the patches specified by the
-- default patch baseline.
--
-- For more information about the @InstallOverrideList@ parameter, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-about-aws-runpatchbaseline.html About the AWS-RunPatchBaseline>
-- SSM document in the /Amazon Web Services Systems Manager User Guide/.
instancePatchState_installOverrideList :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Text)
instancePatchState_installOverrideList = Lens.lens (\InstancePatchState' {installOverrideList} -> installOverrideList) (\s@InstancePatchState' {} a -> s {installOverrideList = a} :: InstancePatchState)

-- | The number of patches from the patch baseline that are installed on the
-- managed node.
instancePatchState_installedCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_installedCount = Lens.lens (\InstancePatchState' {installedCount} -> installedCount) (\s@InstancePatchState' {} a -> s {installedCount = a} :: InstancePatchState)

-- | The number of patches not specified in the patch baseline that are
-- installed on the managed node.
instancePatchState_installedOtherCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_installedOtherCount = Lens.lens (\InstancePatchState' {installedOtherCount} -> installedOtherCount) (\s@InstancePatchState' {} a -> s {installedOtherCount = a} :: InstancePatchState)

-- | The number of patches installed by Patch Manager since the last time the
-- managed node was rebooted.
instancePatchState_installedPendingRebootCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_installedPendingRebootCount = Lens.lens (\InstancePatchState' {installedPendingRebootCount} -> installedPendingRebootCount) (\s@InstancePatchState' {} a -> s {installedPendingRebootCount = a} :: InstancePatchState)

-- | The number of patches installed on a managed node that are specified in
-- a @RejectedPatches@ list. Patches with a status of @InstalledRejected@
-- were typically installed before they were added to a @RejectedPatches@
-- list.
--
-- If @ALLOW_AS_DEPENDENCY@ is the specified option for
-- @RejectedPatchesAction@, the value of @InstalledRejectedCount@ will
-- always be @0@ (zero).
instancePatchState_installedRejectedCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_installedRejectedCount = Lens.lens (\InstancePatchState' {installedRejectedCount} -> installedRejectedCount) (\s@InstancePatchState' {} a -> s {installedRejectedCount = a} :: InstancePatchState)

-- | The time of the last attempt to patch the managed node with @NoReboot@
-- specified as the reboot option.
instancePatchState_lastNoRebootInstallOperationTime :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.UTCTime)
instancePatchState_lastNoRebootInstallOperationTime = Lens.lens (\InstancePatchState' {lastNoRebootInstallOperationTime} -> lastNoRebootInstallOperationTime) (\s@InstancePatchState' {} a -> s {lastNoRebootInstallOperationTime = a} :: InstancePatchState) Prelude.. Lens.mapping Data._Time

-- | The number of patches from the patch baseline that are applicable for
-- the managed node but aren\'t currently installed.
instancePatchState_missingCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_missingCount = Lens.lens (\InstancePatchState' {missingCount} -> missingCount) (\s@InstancePatchState' {} a -> s {missingCount = a} :: InstancePatchState)

-- | The number of patches from the patch baseline that aren\'t applicable
-- for the managed node and therefore aren\'t installed on the node. This
-- number may be truncated if the list of patch names is very large. The
-- number of patches beyond this limit are reported in
-- @UnreportedNotApplicableCount@.
instancePatchState_notApplicableCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_notApplicableCount = Lens.lens (\InstancePatchState' {notApplicableCount} -> notApplicableCount) (\s@InstancePatchState' {} a -> s {notApplicableCount = a} :: InstancePatchState)

-- | The number of patches per node that are specified as other than
-- @Critical@ or @Security@ but aren\'t compliant with the patch baseline.
-- The status of these managed nodes is @NON_COMPLIANT@.
instancePatchState_otherNonCompliantCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_otherNonCompliantCount = Lens.lens (\InstancePatchState' {otherNonCompliantCount} -> otherNonCompliantCount) (\s@InstancePatchState' {} a -> s {otherNonCompliantCount = a} :: InstancePatchState)

-- | Placeholder information. This field will always be empty in the current
-- release of the service.
instancePatchState_ownerInformation :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Text)
instancePatchState_ownerInformation = Lens.lens (\InstancePatchState' {ownerInformation} -> ownerInformation) (\s@InstancePatchState' {} a -> s {ownerInformation = a} :: InstancePatchState) Prelude.. Lens.mapping Data._Sensitive

-- | Indicates the reboot option specified in the patch baseline.
--
-- Reboot options apply to @Install@ operations only. Reboots aren\'t
-- attempted for Patch Manager @Scan@ operations.
--
-- -   @RebootIfNeeded@: Patch Manager tries to reboot the managed node if
--     it installed any patches, or if any patches are detected with a
--     status of @InstalledPendingReboot@.
--
-- -   @NoReboot@: Patch Manager attempts to install missing packages
--     without trying to reboot the system. Patches installed with this
--     option are assigned a status of @InstalledPendingReboot@. These
--     patches might not be in effect until a reboot is performed.
instancePatchState_rebootOption :: Lens.Lens' InstancePatchState (Prelude.Maybe RebootOption)
instancePatchState_rebootOption = Lens.lens (\InstancePatchState' {rebootOption} -> rebootOption) (\s@InstancePatchState' {} a -> s {rebootOption = a} :: InstancePatchState)

-- | The number of patches per node that are specified as @Security@ in a
-- patch advisory aren\'t installed. These patches might be missing, have
-- failed installation, were rejected, or were installed but awaiting a
-- required managed node reboot. The status of these managed nodes is
-- @NON_COMPLIANT@.
instancePatchState_securityNonCompliantCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_securityNonCompliantCount = Lens.lens (\InstancePatchState' {securityNonCompliantCount} -> securityNonCompliantCount) (\s@InstancePatchState' {} a -> s {securityNonCompliantCount = a} :: InstancePatchState)

-- | The ID of the patch baseline snapshot used during the patching operation
-- when this compliance data was collected.
instancePatchState_snapshotId :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Text)
instancePatchState_snapshotId = Lens.lens (\InstancePatchState' {snapshotId} -> snapshotId) (\s@InstancePatchState' {} a -> s {snapshotId = a} :: InstancePatchState)

-- | The number of patches beyond the supported limit of @NotApplicableCount@
-- that aren\'t reported by name to Inventory. Inventory is a capability of
-- Amazon Web Services Systems Manager.
instancePatchState_unreportedNotApplicableCount :: Lens.Lens' InstancePatchState (Prelude.Maybe Prelude.Int)
instancePatchState_unreportedNotApplicableCount = Lens.lens (\InstancePatchState' {unreportedNotApplicableCount} -> unreportedNotApplicableCount) (\s@InstancePatchState' {} a -> s {unreportedNotApplicableCount = a} :: InstancePatchState)

-- | The ID of the managed node the high-level patch compliance information
-- was collected for.
instancePatchState_instanceId :: Lens.Lens' InstancePatchState Prelude.Text
instancePatchState_instanceId = Lens.lens (\InstancePatchState' {instanceId} -> instanceId) (\s@InstancePatchState' {} a -> s {instanceId = a} :: InstancePatchState)

-- | The name of the patch group the managed node belongs to.
instancePatchState_patchGroup :: Lens.Lens' InstancePatchState Prelude.Text
instancePatchState_patchGroup = Lens.lens (\InstancePatchState' {patchGroup} -> patchGroup) (\s@InstancePatchState' {} a -> s {patchGroup = a} :: InstancePatchState)

-- | The ID of the patch baseline used to patch the managed node.
instancePatchState_baselineId :: Lens.Lens' InstancePatchState Prelude.Text
instancePatchState_baselineId = Lens.lens (\InstancePatchState' {baselineId} -> baselineId) (\s@InstancePatchState' {} a -> s {baselineId = a} :: InstancePatchState)

-- | The time the most recent patching operation was started on the managed
-- node.
instancePatchState_operationStartTime :: Lens.Lens' InstancePatchState Prelude.UTCTime
instancePatchState_operationStartTime = Lens.lens (\InstancePatchState' {operationStartTime} -> operationStartTime) (\s@InstancePatchState' {} a -> s {operationStartTime = a} :: InstancePatchState) Prelude.. Data._Time

-- | The time the most recent patching operation completed on the managed
-- node.
instancePatchState_operationEndTime :: Lens.Lens' InstancePatchState Prelude.UTCTime
instancePatchState_operationEndTime = Lens.lens (\InstancePatchState' {operationEndTime} -> operationEndTime) (\s@InstancePatchState' {} a -> s {operationEndTime = a} :: InstancePatchState) Prelude.. Data._Time

-- | The type of patching operation that was performed: or
--
-- -   @SCAN@ assesses the patch compliance state.
--
-- -   @INSTALL@ installs missing patches.
instancePatchState_operation :: Lens.Lens' InstancePatchState PatchOperationType
instancePatchState_operation = Lens.lens (\InstancePatchState' {operation} -> operation) (\s@InstancePatchState' {} a -> s {operation = a} :: InstancePatchState)

instance Data.FromJSON InstancePatchState where
  parseJSON =
    Data.withObject
      "InstancePatchState"
      ( \x ->
          InstancePatchState'
            Prelude.<$> (x Data..:? "CriticalNonCompliantCount")
            Prelude.<*> (x Data..:? "FailedCount")
            Prelude.<*> (x Data..:? "InstallOverrideList")
            Prelude.<*> (x Data..:? "InstalledCount")
            Prelude.<*> (x Data..:? "InstalledOtherCount")
            Prelude.<*> (x Data..:? "InstalledPendingRebootCount")
            Prelude.<*> (x Data..:? "InstalledRejectedCount")
            Prelude.<*> (x Data..:? "LastNoRebootInstallOperationTime")
            Prelude.<*> (x Data..:? "MissingCount")
            Prelude.<*> (x Data..:? "NotApplicableCount")
            Prelude.<*> (x Data..:? "OtherNonCompliantCount")
            Prelude.<*> (x Data..:? "OwnerInformation")
            Prelude.<*> (x Data..:? "RebootOption")
            Prelude.<*> (x Data..:? "SecurityNonCompliantCount")
            Prelude.<*> (x Data..:? "SnapshotId")
            Prelude.<*> (x Data..:? "UnreportedNotApplicableCount")
            Prelude.<*> (x Data..: "InstanceId")
            Prelude.<*> (x Data..: "PatchGroup")
            Prelude.<*> (x Data..: "BaselineId")
            Prelude.<*> (x Data..: "OperationStartTime")
            Prelude.<*> (x Data..: "OperationEndTime")
            Prelude.<*> (x Data..: "Operation")
      )

instance Prelude.Hashable InstancePatchState where
  hashWithSalt _salt InstancePatchState' {..} =
    _salt
      `Prelude.hashWithSalt` criticalNonCompliantCount
      `Prelude.hashWithSalt` failedCount
      `Prelude.hashWithSalt` installOverrideList
      `Prelude.hashWithSalt` installedCount
      `Prelude.hashWithSalt` installedOtherCount
      `Prelude.hashWithSalt` installedPendingRebootCount
      `Prelude.hashWithSalt` installedRejectedCount
      `Prelude.hashWithSalt` lastNoRebootInstallOperationTime
      `Prelude.hashWithSalt` missingCount
      `Prelude.hashWithSalt` notApplicableCount
      `Prelude.hashWithSalt` otherNonCompliantCount
      `Prelude.hashWithSalt` ownerInformation
      `Prelude.hashWithSalt` rebootOption
      `Prelude.hashWithSalt` securityNonCompliantCount
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` unreportedNotApplicableCount
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` patchGroup
      `Prelude.hashWithSalt` baselineId
      `Prelude.hashWithSalt` operationStartTime
      `Prelude.hashWithSalt` operationEndTime
      `Prelude.hashWithSalt` operation

instance Prelude.NFData InstancePatchState where
  rnf InstancePatchState' {..} =
    Prelude.rnf criticalNonCompliantCount
      `Prelude.seq` Prelude.rnf failedCount
      `Prelude.seq` Prelude.rnf installOverrideList
      `Prelude.seq` Prelude.rnf installedCount
      `Prelude.seq` Prelude.rnf installedOtherCount
      `Prelude.seq` Prelude.rnf installedPendingRebootCount
      `Prelude.seq` Prelude.rnf installedRejectedCount
      `Prelude.seq` Prelude.rnf lastNoRebootInstallOperationTime
      `Prelude.seq` Prelude.rnf missingCount
      `Prelude.seq` Prelude.rnf notApplicableCount
      `Prelude.seq` Prelude.rnf otherNonCompliantCount
      `Prelude.seq` Prelude.rnf ownerInformation
      `Prelude.seq` Prelude.rnf rebootOption
      `Prelude.seq` Prelude.rnf securityNonCompliantCount
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf
        unreportedNotApplicableCount
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf patchGroup
      `Prelude.seq` Prelude.rnf baselineId
      `Prelude.seq` Prelude.rnf operationStartTime
      `Prelude.seq` Prelude.rnf operationEndTime
      `Prelude.seq` Prelude.rnf operation
