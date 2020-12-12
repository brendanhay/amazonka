{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstancePatchState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstancePatchState
  ( InstancePatchState (..),

    -- * Smart constructor
    mkInstancePatchState,

    -- * Lenses
    ipsUnreportedNotApplicableCount,
    ipsRebootOption,
    ipsInstalledPendingRebootCount,
    ipsOwnerInformation,
    ipsInstalledRejectedCount,
    ipsFailedCount,
    ipsInstalledOtherCount,
    ipsMissingCount,
    ipsInstallOverrideList,
    ipsNotApplicableCount,
    ipsInstalledCount,
    ipsLastNoRebootInstallOperationTime,
    ipsSnapshotId,
    ipsInstanceId,
    ipsPatchGroup,
    ipsBaselineId,
    ipsOperationStartTime,
    ipsOperationEndTime,
    ipsOperation,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.PatchOperationType
import Network.AWS.SSM.Types.RebootOption

-- | Defines the high-level patch compliance state for a managed instance, providing information about the number of installed, missing, not applicable, and failed patches along with metadata about the operation when this information was gathered for the instance.
--
-- /See:/ 'mkInstancePatchState' smart constructor.
data InstancePatchState = InstancePatchState'
  { unreportedNotApplicableCount ::
      Lude.Maybe Lude.Int,
    rebootOption :: Lude.Maybe RebootOption,
    installedPendingRebootCount :: Lude.Maybe Lude.Int,
    ownerInformation ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    installedRejectedCount :: Lude.Maybe Lude.Int,
    failedCount :: Lude.Maybe Lude.Int,
    installedOtherCount :: Lude.Maybe Lude.Int,
    missingCount :: Lude.Maybe Lude.Int,
    installOverrideList :: Lude.Maybe Lude.Text,
    notApplicableCount :: Lude.Maybe Lude.Int,
    installedCount :: Lude.Maybe Lude.Int,
    lastNoRebootInstallOperationTime ::
      Lude.Maybe Lude.Timestamp,
    snapshotId :: Lude.Maybe Lude.Text,
    instanceId :: Lude.Text,
    patchGroup :: Lude.Text,
    baselineId :: Lude.Text,
    operationStartTime :: Lude.Timestamp,
    operationEndTime :: Lude.Timestamp,
    operation :: PatchOperationType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstancePatchState' with the minimum fields required to make a request.
--
-- * 'baselineId' - The ID of the patch baseline used to patch the instance.
-- * 'failedCount' - The number of patches from the patch baseline that were attempted to be installed during the last patching operation, but failed to install.
-- * 'installOverrideList' - An https URL or an Amazon S3 path-style URL to a list of patches to be installed. This patch installation list, which you maintain in an S3 bucket in YAML format and specify in the SSM document @AWS-RunPatchBaseline@ , overrides the patches specified by the default patch baseline.
--
-- For more information about the @InstallOverrideList@ parameter, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-about-aws-runpatchbaseline.html About the SSM document AWS-RunPatchBaseline> in the /AWS Systems Manager User Guide/ .
-- * 'installedCount' - The number of patches from the patch baseline that are installed on the instance.
-- * 'installedOtherCount' - The number of patches not specified in the patch baseline that are installed on the instance.
-- * 'installedPendingRebootCount' - The number of patches installed by Patch Manager since the last time the instance was rebooted.
-- * 'installedRejectedCount' - The number of patches installed on an instance that are specified in a @RejectedPatches@ list. Patches with a status of /InstalledRejected/ were typically installed before they were added to a @RejectedPatches@ list.
-- * 'instanceId' - The ID of the managed instance the high-level patch compliance information was collected for.
-- * 'lastNoRebootInstallOperationTime' - The time of the last attempt to patch the instance with @NoReboot@ specified as the reboot option.
-- * 'missingCount' - The number of patches from the patch baseline that are applicable for the instance but aren't currently installed.
-- * 'notApplicableCount' - The number of patches from the patch baseline that aren't applicable for the instance and therefore aren't installed on the instance. This number may be truncated if the list of patch names is very large. The number of patches beyond this limit are reported in @UnreportedNotApplicableCount@ .
-- * 'operation' - The type of patching operation that was performed: @SCAN@ (assess patch compliance state) or @INSTALL@ (install missing patches).
-- * 'operationEndTime' - The time the most recent patching operation completed on the instance.
-- * 'operationStartTime' - The time the most recent patching operation was started on the instance.
-- * 'ownerInformation' - Placeholder information. This field will always be empty in the current release of the service.
-- * 'patchGroup' - The name of the patch group the managed instance belongs to.
-- * 'rebootOption' - Indicates the reboot option specified in the patch baseline.
--
--
--     * __RebootIfNeeded__ : Patch Manager tries to reboot the instance if it installed any patches, or if any patches are detected with a status of @InstalledPendingReboot@ .
--
--
--     * __NoReboot__ : Patch Manager attempts to install missing packages without trying to reboot the system. Patches installed with this option are assigned a status of @InstalledPendingReboot@ . These patches might not be in effect until a reboot is performed.
--
--
-- * 'snapshotId' - The ID of the patch baseline snapshot used during the patching operation when this compliance data was collected.
-- * 'unreportedNotApplicableCount' - The number of patches beyond the supported limit of @NotApplicableCount@ that are not reported by name to Systems Manager Inventory.
mkInstancePatchState ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'patchGroup'
  Lude.Text ->
  -- | 'baselineId'
  Lude.Text ->
  -- | 'operationStartTime'
  Lude.Timestamp ->
  -- | 'operationEndTime'
  Lude.Timestamp ->
  -- | 'operation'
  PatchOperationType ->
  InstancePatchState
mkInstancePatchState
  pInstanceId_
  pPatchGroup_
  pBaselineId_
  pOperationStartTime_
  pOperationEndTime_
  pOperation_ =
    InstancePatchState'
      { unreportedNotApplicableCount = Lude.Nothing,
        rebootOption = Lude.Nothing,
        installedPendingRebootCount = Lude.Nothing,
        ownerInformation = Lude.Nothing,
        installedRejectedCount = Lude.Nothing,
        failedCount = Lude.Nothing,
        installedOtherCount = Lude.Nothing,
        missingCount = Lude.Nothing,
        installOverrideList = Lude.Nothing,
        notApplicableCount = Lude.Nothing,
        installedCount = Lude.Nothing,
        lastNoRebootInstallOperationTime = Lude.Nothing,
        snapshotId = Lude.Nothing,
        instanceId = pInstanceId_,
        patchGroup = pPatchGroup_,
        baselineId = pBaselineId_,
        operationStartTime = pOperationStartTime_,
        operationEndTime = pOperationEndTime_,
        operation = pOperation_
      }

-- | The number of patches beyond the supported limit of @NotApplicableCount@ that are not reported by name to Systems Manager Inventory.
--
-- /Note:/ Consider using 'unreportedNotApplicableCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsUnreportedNotApplicableCount :: Lens.Lens' InstancePatchState (Lude.Maybe Lude.Int)
ipsUnreportedNotApplicableCount = Lens.lens (unreportedNotApplicableCount :: InstancePatchState -> Lude.Maybe Lude.Int) (\s a -> s {unreportedNotApplicableCount = a} :: InstancePatchState)
{-# DEPRECATED ipsUnreportedNotApplicableCount "Use generic-lens or generic-optics with 'unreportedNotApplicableCount' instead." #-}

-- | Indicates the reboot option specified in the patch baseline.
--
--
--     * __RebootIfNeeded__ : Patch Manager tries to reboot the instance if it installed any patches, or if any patches are detected with a status of @InstalledPendingReboot@ .
--
--
--     * __NoReboot__ : Patch Manager attempts to install missing packages without trying to reboot the system. Patches installed with this option are assigned a status of @InstalledPendingReboot@ . These patches might not be in effect until a reboot is performed.
--
--
--
-- /Note:/ Consider using 'rebootOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsRebootOption :: Lens.Lens' InstancePatchState (Lude.Maybe RebootOption)
ipsRebootOption = Lens.lens (rebootOption :: InstancePatchState -> Lude.Maybe RebootOption) (\s a -> s {rebootOption = a} :: InstancePatchState)
{-# DEPRECATED ipsRebootOption "Use generic-lens or generic-optics with 'rebootOption' instead." #-}

-- | The number of patches installed by Patch Manager since the last time the instance was rebooted.
--
-- /Note:/ Consider using 'installedPendingRebootCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsInstalledPendingRebootCount :: Lens.Lens' InstancePatchState (Lude.Maybe Lude.Int)
ipsInstalledPendingRebootCount = Lens.lens (installedPendingRebootCount :: InstancePatchState -> Lude.Maybe Lude.Int) (\s a -> s {installedPendingRebootCount = a} :: InstancePatchState)
{-# DEPRECATED ipsInstalledPendingRebootCount "Use generic-lens or generic-optics with 'installedPendingRebootCount' instead." #-}

-- | Placeholder information. This field will always be empty in the current release of the service.
--
-- /Note:/ Consider using 'ownerInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsOwnerInformation :: Lens.Lens' InstancePatchState (Lude.Maybe (Lude.Sensitive Lude.Text))
ipsOwnerInformation = Lens.lens (ownerInformation :: InstancePatchState -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {ownerInformation = a} :: InstancePatchState)
{-# DEPRECATED ipsOwnerInformation "Use generic-lens or generic-optics with 'ownerInformation' instead." #-}

-- | The number of patches installed on an instance that are specified in a @RejectedPatches@ list. Patches with a status of /InstalledRejected/ were typically installed before they were added to a @RejectedPatches@ list.
--
-- /Note:/ Consider using 'installedRejectedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsInstalledRejectedCount :: Lens.Lens' InstancePatchState (Lude.Maybe Lude.Int)
ipsInstalledRejectedCount = Lens.lens (installedRejectedCount :: InstancePatchState -> Lude.Maybe Lude.Int) (\s a -> s {installedRejectedCount = a} :: InstancePatchState)
{-# DEPRECATED ipsInstalledRejectedCount "Use generic-lens or generic-optics with 'installedRejectedCount' instead." #-}

-- | The number of patches from the patch baseline that were attempted to be installed during the last patching operation, but failed to install.
--
-- /Note:/ Consider using 'failedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsFailedCount :: Lens.Lens' InstancePatchState (Lude.Maybe Lude.Int)
ipsFailedCount = Lens.lens (failedCount :: InstancePatchState -> Lude.Maybe Lude.Int) (\s a -> s {failedCount = a} :: InstancePatchState)
{-# DEPRECATED ipsFailedCount "Use generic-lens or generic-optics with 'failedCount' instead." #-}

-- | The number of patches not specified in the patch baseline that are installed on the instance.
--
-- /Note:/ Consider using 'installedOtherCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsInstalledOtherCount :: Lens.Lens' InstancePatchState (Lude.Maybe Lude.Int)
ipsInstalledOtherCount = Lens.lens (installedOtherCount :: InstancePatchState -> Lude.Maybe Lude.Int) (\s a -> s {installedOtherCount = a} :: InstancePatchState)
{-# DEPRECATED ipsInstalledOtherCount "Use generic-lens or generic-optics with 'installedOtherCount' instead." #-}

-- | The number of patches from the patch baseline that are applicable for the instance but aren't currently installed.
--
-- /Note:/ Consider using 'missingCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsMissingCount :: Lens.Lens' InstancePatchState (Lude.Maybe Lude.Int)
ipsMissingCount = Lens.lens (missingCount :: InstancePatchState -> Lude.Maybe Lude.Int) (\s a -> s {missingCount = a} :: InstancePatchState)
{-# DEPRECATED ipsMissingCount "Use generic-lens or generic-optics with 'missingCount' instead." #-}

-- | An https URL or an Amazon S3 path-style URL to a list of patches to be installed. This patch installation list, which you maintain in an S3 bucket in YAML format and specify in the SSM document @AWS-RunPatchBaseline@ , overrides the patches specified by the default patch baseline.
--
-- For more information about the @InstallOverrideList@ parameter, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-about-aws-runpatchbaseline.html About the SSM document AWS-RunPatchBaseline> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'installOverrideList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsInstallOverrideList :: Lens.Lens' InstancePatchState (Lude.Maybe Lude.Text)
ipsInstallOverrideList = Lens.lens (installOverrideList :: InstancePatchState -> Lude.Maybe Lude.Text) (\s a -> s {installOverrideList = a} :: InstancePatchState)
{-# DEPRECATED ipsInstallOverrideList "Use generic-lens or generic-optics with 'installOverrideList' instead." #-}

-- | The number of patches from the patch baseline that aren't applicable for the instance and therefore aren't installed on the instance. This number may be truncated if the list of patch names is very large. The number of patches beyond this limit are reported in @UnreportedNotApplicableCount@ .
--
-- /Note:/ Consider using 'notApplicableCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsNotApplicableCount :: Lens.Lens' InstancePatchState (Lude.Maybe Lude.Int)
ipsNotApplicableCount = Lens.lens (notApplicableCount :: InstancePatchState -> Lude.Maybe Lude.Int) (\s a -> s {notApplicableCount = a} :: InstancePatchState)
{-# DEPRECATED ipsNotApplicableCount "Use generic-lens or generic-optics with 'notApplicableCount' instead." #-}

-- | The number of patches from the patch baseline that are installed on the instance.
--
-- /Note:/ Consider using 'installedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsInstalledCount :: Lens.Lens' InstancePatchState (Lude.Maybe Lude.Int)
ipsInstalledCount = Lens.lens (installedCount :: InstancePatchState -> Lude.Maybe Lude.Int) (\s a -> s {installedCount = a} :: InstancePatchState)
{-# DEPRECATED ipsInstalledCount "Use generic-lens or generic-optics with 'installedCount' instead." #-}

-- | The time of the last attempt to patch the instance with @NoReboot@ specified as the reboot option.
--
-- /Note:/ Consider using 'lastNoRebootInstallOperationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsLastNoRebootInstallOperationTime :: Lens.Lens' InstancePatchState (Lude.Maybe Lude.Timestamp)
ipsLastNoRebootInstallOperationTime = Lens.lens (lastNoRebootInstallOperationTime :: InstancePatchState -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastNoRebootInstallOperationTime = a} :: InstancePatchState)
{-# DEPRECATED ipsLastNoRebootInstallOperationTime "Use generic-lens or generic-optics with 'lastNoRebootInstallOperationTime' instead." #-}

-- | The ID of the patch baseline snapshot used during the patching operation when this compliance data was collected.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsSnapshotId :: Lens.Lens' InstancePatchState (Lude.Maybe Lude.Text)
ipsSnapshotId = Lens.lens (snapshotId :: InstancePatchState -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: InstancePatchState)
{-# DEPRECATED ipsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The ID of the managed instance the high-level patch compliance information was collected for.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsInstanceId :: Lens.Lens' InstancePatchState Lude.Text
ipsInstanceId = Lens.lens (instanceId :: InstancePatchState -> Lude.Text) (\s a -> s {instanceId = a} :: InstancePatchState)
{-# DEPRECATED ipsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the patch group the managed instance belongs to.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsPatchGroup :: Lens.Lens' InstancePatchState Lude.Text
ipsPatchGroup = Lens.lens (patchGroup :: InstancePatchState -> Lude.Text) (\s a -> s {patchGroup = a} :: InstancePatchState)
{-# DEPRECATED ipsPatchGroup "Use generic-lens or generic-optics with 'patchGroup' instead." #-}

-- | The ID of the patch baseline used to patch the instance.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsBaselineId :: Lens.Lens' InstancePatchState Lude.Text
ipsBaselineId = Lens.lens (baselineId :: InstancePatchState -> Lude.Text) (\s a -> s {baselineId = a} :: InstancePatchState)
{-# DEPRECATED ipsBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The time the most recent patching operation was started on the instance.
--
-- /Note:/ Consider using 'operationStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsOperationStartTime :: Lens.Lens' InstancePatchState Lude.Timestamp
ipsOperationStartTime = Lens.lens (operationStartTime :: InstancePatchState -> Lude.Timestamp) (\s a -> s {operationStartTime = a} :: InstancePatchState)
{-# DEPRECATED ipsOperationStartTime "Use generic-lens or generic-optics with 'operationStartTime' instead." #-}

-- | The time the most recent patching operation completed on the instance.
--
-- /Note:/ Consider using 'operationEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsOperationEndTime :: Lens.Lens' InstancePatchState Lude.Timestamp
ipsOperationEndTime = Lens.lens (operationEndTime :: InstancePatchState -> Lude.Timestamp) (\s a -> s {operationEndTime = a} :: InstancePatchState)
{-# DEPRECATED ipsOperationEndTime "Use generic-lens or generic-optics with 'operationEndTime' instead." #-}

-- | The type of patching operation that was performed: @SCAN@ (assess patch compliance state) or @INSTALL@ (install missing patches).
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsOperation :: Lens.Lens' InstancePatchState PatchOperationType
ipsOperation = Lens.lens (operation :: InstancePatchState -> PatchOperationType) (\s a -> s {operation = a} :: InstancePatchState)
{-# DEPRECATED ipsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

instance Lude.FromJSON InstancePatchState where
  parseJSON =
    Lude.withObject
      "InstancePatchState"
      ( \x ->
          InstancePatchState'
            Lude.<$> (x Lude..:? "UnreportedNotApplicableCount")
            Lude.<*> (x Lude..:? "RebootOption")
            Lude.<*> (x Lude..:? "InstalledPendingRebootCount")
            Lude.<*> (x Lude..:? "OwnerInformation")
            Lude.<*> (x Lude..:? "InstalledRejectedCount")
            Lude.<*> (x Lude..:? "FailedCount")
            Lude.<*> (x Lude..:? "InstalledOtherCount")
            Lude.<*> (x Lude..:? "MissingCount")
            Lude.<*> (x Lude..:? "InstallOverrideList")
            Lude.<*> (x Lude..:? "NotApplicableCount")
            Lude.<*> (x Lude..:? "InstalledCount")
            Lude.<*> (x Lude..:? "LastNoRebootInstallOperationTime")
            Lude.<*> (x Lude..:? "SnapshotId")
            Lude.<*> (x Lude..: "InstanceId")
            Lude.<*> (x Lude..: "PatchGroup")
            Lude.<*> (x Lude..: "BaselineId")
            Lude.<*> (x Lude..: "OperationStartTime")
            Lude.<*> (x Lude..: "OperationEndTime")
            Lude.<*> (x Lude..: "Operation")
      )
