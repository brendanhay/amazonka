{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstancePatchState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.InstancePatchState
  ( InstancePatchState (..)
  -- * Smart constructor
  , mkInstancePatchState
  -- * Lenses
  , ipsInstanceId
  , ipsPatchGroup
  , ipsBaselineId
  , ipsOperationStartTime
  , ipsOperationEndTime
  , ipsOperation
  , ipsFailedCount
  , ipsInstallOverrideList
  , ipsInstalledCount
  , ipsInstalledOtherCount
  , ipsInstalledPendingRebootCount
  , ipsInstalledRejectedCount
  , ipsLastNoRebootInstallOperationTime
  , ipsMissingCount
  , ipsNotApplicableCount
  , ipsOwnerInformation
  , ipsRebootOption
  , ipsSnapshotId
  , ipsUnreportedNotApplicableCount
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.BaselineId as Types
import qualified Network.AWS.SSM.Types.InstallOverrideList as Types
import qualified Network.AWS.SSM.Types.InstanceId as Types
import qualified Network.AWS.SSM.Types.OwnerInformation as Types
import qualified Network.AWS.SSM.Types.PatchGroup as Types
import qualified Network.AWS.SSM.Types.PatchOperationType as Types
import qualified Network.AWS.SSM.Types.RebootOption as Types
import qualified Network.AWS.SSM.Types.SnapshotId as Types

-- | Defines the high-level patch compliance state for a managed instance, providing information about the number of installed, missing, not applicable, and failed patches along with metadata about the operation when this information was gathered for the instance.
--
-- /See:/ 'mkInstancePatchState' smart constructor.
data InstancePatchState = InstancePatchState'
  { instanceId :: Types.InstanceId
    -- ^ The ID of the managed instance the high-level patch compliance information was collected for.
  , patchGroup :: Types.PatchGroup
    -- ^ The name of the patch group the managed instance belongs to.
  , baselineId :: Types.BaselineId
    -- ^ The ID of the patch baseline used to patch the instance.
  , operationStartTime :: Core.NominalDiffTime
    -- ^ The time the most recent patching operation was started on the instance.
  , operationEndTime :: Core.NominalDiffTime
    -- ^ The time the most recent patching operation completed on the instance.
  , operation :: Types.PatchOperationType
    -- ^ The type of patching operation that was performed: @SCAN@ (assess patch compliance state) or @INSTALL@ (install missing patches).
  , failedCount :: Core.Maybe Core.Int
    -- ^ The number of patches from the patch baseline that were attempted to be installed during the last patching operation, but failed to install.
  , installOverrideList :: Core.Maybe Types.InstallOverrideList
    -- ^ An https URL or an Amazon S3 path-style URL to a list of patches to be installed. This patch installation list, which you maintain in an S3 bucket in YAML format and specify in the SSM document @AWS-RunPatchBaseline@ , overrides the patches specified by the default patch baseline.
--
-- For more information about the @InstallOverrideList@ parameter, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-about-aws-runpatchbaseline.html About the SSM document AWS-RunPatchBaseline> in the /AWS Systems Manager User Guide/ .
  , installedCount :: Core.Maybe Core.Int
    -- ^ The number of patches from the patch baseline that are installed on the instance.
  , installedOtherCount :: Core.Maybe Core.Int
    -- ^ The number of patches not specified in the patch baseline that are installed on the instance.
  , installedPendingRebootCount :: Core.Maybe Core.Int
    -- ^ The number of patches installed by Patch Manager since the last time the instance was rebooted.
  , installedRejectedCount :: Core.Maybe Core.Int
    -- ^ The number of patches installed on an instance that are specified in a @RejectedPatches@ list. Patches with a status of /InstalledRejected/ were typically installed before they were added to a @RejectedPatches@ list.
  , lastNoRebootInstallOperationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time of the last attempt to patch the instance with @NoReboot@ specified as the reboot option.
  , missingCount :: Core.Maybe Core.Int
    -- ^ The number of patches from the patch baseline that are applicable for the instance but aren't currently installed.
  , notApplicableCount :: Core.Maybe Core.Int
    -- ^ The number of patches from the patch baseline that aren't applicable for the instance and therefore aren't installed on the instance. This number may be truncated if the list of patch names is very large. The number of patches beyond this limit are reported in @UnreportedNotApplicableCount@ .
  , ownerInformation :: Core.Maybe Types.OwnerInformation
    -- ^ Placeholder information. This field will always be empty in the current release of the service.
  , rebootOption :: Core.Maybe Types.RebootOption
    -- ^ Indicates the reboot option specified in the patch baseline.
--
--
--     * __RebootIfNeeded__ : Patch Manager tries to reboot the instance if it installed any patches, or if any patches are detected with a status of @InstalledPendingReboot@ .
--
--
--     * __NoReboot__ : Patch Manager attempts to install missing packages without trying to reboot the system. Patches installed with this option are assigned a status of @InstalledPendingReboot@ . These patches might not be in effect until a reboot is performed.
--
--
  , snapshotId :: Core.Maybe Types.SnapshotId
    -- ^ The ID of the patch baseline snapshot used during the patching operation when this compliance data was collected.
  , unreportedNotApplicableCount :: Core.Maybe Core.Int
    -- ^ The number of patches beyond the supported limit of @NotApplicableCount@ that are not reported by name to Systems Manager Inventory.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstancePatchState' value with any optional fields omitted.
mkInstancePatchState
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.PatchGroup -- ^ 'patchGroup'
    -> Types.BaselineId -- ^ 'baselineId'
    -> Core.NominalDiffTime -- ^ 'operationStartTime'
    -> Core.NominalDiffTime -- ^ 'operationEndTime'
    -> Types.PatchOperationType -- ^ 'operation'
    -> InstancePatchState
mkInstancePatchState instanceId patchGroup baselineId
  operationStartTime operationEndTime operation
  = InstancePatchState'{instanceId, patchGroup, baselineId,
                        operationStartTime, operationEndTime, operation,
                        failedCount = Core.Nothing, installOverrideList = Core.Nothing,
                        installedCount = Core.Nothing, installedOtherCount = Core.Nothing,
                        installedPendingRebootCount = Core.Nothing,
                        installedRejectedCount = Core.Nothing,
                        lastNoRebootInstallOperationTime = Core.Nothing,
                        missingCount = Core.Nothing, notApplicableCount = Core.Nothing,
                        ownerInformation = Core.Nothing, rebootOption = Core.Nothing,
                        snapshotId = Core.Nothing,
                        unreportedNotApplicableCount = Core.Nothing}

-- | The ID of the managed instance the high-level patch compliance information was collected for.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsInstanceId :: Lens.Lens' InstancePatchState Types.InstanceId
ipsInstanceId = Lens.field @"instanceId"
{-# INLINEABLE ipsInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The name of the patch group the managed instance belongs to.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsPatchGroup :: Lens.Lens' InstancePatchState Types.PatchGroup
ipsPatchGroup = Lens.field @"patchGroup"
{-# INLINEABLE ipsPatchGroup #-}
{-# DEPRECATED patchGroup "Use generic-lens or generic-optics with 'patchGroup' instead"  #-}

-- | The ID of the patch baseline used to patch the instance.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsBaselineId :: Lens.Lens' InstancePatchState Types.BaselineId
ipsBaselineId = Lens.field @"baselineId"
{-# INLINEABLE ipsBaselineId #-}
{-# DEPRECATED baselineId "Use generic-lens or generic-optics with 'baselineId' instead"  #-}

-- | The time the most recent patching operation was started on the instance.
--
-- /Note:/ Consider using 'operationStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsOperationStartTime :: Lens.Lens' InstancePatchState Core.NominalDiffTime
ipsOperationStartTime = Lens.field @"operationStartTime"
{-# INLINEABLE ipsOperationStartTime #-}
{-# DEPRECATED operationStartTime "Use generic-lens or generic-optics with 'operationStartTime' instead"  #-}

-- | The time the most recent patching operation completed on the instance.
--
-- /Note:/ Consider using 'operationEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsOperationEndTime :: Lens.Lens' InstancePatchState Core.NominalDiffTime
ipsOperationEndTime = Lens.field @"operationEndTime"
{-# INLINEABLE ipsOperationEndTime #-}
{-# DEPRECATED operationEndTime "Use generic-lens or generic-optics with 'operationEndTime' instead"  #-}

-- | The type of patching operation that was performed: @SCAN@ (assess patch compliance state) or @INSTALL@ (install missing patches).
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsOperation :: Lens.Lens' InstancePatchState Types.PatchOperationType
ipsOperation = Lens.field @"operation"
{-# INLINEABLE ipsOperation #-}
{-# DEPRECATED operation "Use generic-lens or generic-optics with 'operation' instead"  #-}

-- | The number of patches from the patch baseline that were attempted to be installed during the last patching operation, but failed to install.
--
-- /Note:/ Consider using 'failedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsFailedCount :: Lens.Lens' InstancePatchState (Core.Maybe Core.Int)
ipsFailedCount = Lens.field @"failedCount"
{-# INLINEABLE ipsFailedCount #-}
{-# DEPRECATED failedCount "Use generic-lens or generic-optics with 'failedCount' instead"  #-}

-- | An https URL or an Amazon S3 path-style URL to a list of patches to be installed. This patch installation list, which you maintain in an S3 bucket in YAML format and specify in the SSM document @AWS-RunPatchBaseline@ , overrides the patches specified by the default patch baseline.
--
-- For more information about the @InstallOverrideList@ parameter, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-about-aws-runpatchbaseline.html About the SSM document AWS-RunPatchBaseline> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'installOverrideList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsInstallOverrideList :: Lens.Lens' InstancePatchState (Core.Maybe Types.InstallOverrideList)
ipsInstallOverrideList = Lens.field @"installOverrideList"
{-# INLINEABLE ipsInstallOverrideList #-}
{-# DEPRECATED installOverrideList "Use generic-lens or generic-optics with 'installOverrideList' instead"  #-}

-- | The number of patches from the patch baseline that are installed on the instance.
--
-- /Note:/ Consider using 'installedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsInstalledCount :: Lens.Lens' InstancePatchState (Core.Maybe Core.Int)
ipsInstalledCount = Lens.field @"installedCount"
{-# INLINEABLE ipsInstalledCount #-}
{-# DEPRECATED installedCount "Use generic-lens or generic-optics with 'installedCount' instead"  #-}

-- | The number of patches not specified in the patch baseline that are installed on the instance.
--
-- /Note:/ Consider using 'installedOtherCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsInstalledOtherCount :: Lens.Lens' InstancePatchState (Core.Maybe Core.Int)
ipsInstalledOtherCount = Lens.field @"installedOtherCount"
{-# INLINEABLE ipsInstalledOtherCount #-}
{-# DEPRECATED installedOtherCount "Use generic-lens or generic-optics with 'installedOtherCount' instead"  #-}

-- | The number of patches installed by Patch Manager since the last time the instance was rebooted.
--
-- /Note:/ Consider using 'installedPendingRebootCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsInstalledPendingRebootCount :: Lens.Lens' InstancePatchState (Core.Maybe Core.Int)
ipsInstalledPendingRebootCount = Lens.field @"installedPendingRebootCount"
{-# INLINEABLE ipsInstalledPendingRebootCount #-}
{-# DEPRECATED installedPendingRebootCount "Use generic-lens or generic-optics with 'installedPendingRebootCount' instead"  #-}

-- | The number of patches installed on an instance that are specified in a @RejectedPatches@ list. Patches with a status of /InstalledRejected/ were typically installed before they were added to a @RejectedPatches@ list.
--
-- /Note:/ Consider using 'installedRejectedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsInstalledRejectedCount :: Lens.Lens' InstancePatchState (Core.Maybe Core.Int)
ipsInstalledRejectedCount = Lens.field @"installedRejectedCount"
{-# INLINEABLE ipsInstalledRejectedCount #-}
{-# DEPRECATED installedRejectedCount "Use generic-lens or generic-optics with 'installedRejectedCount' instead"  #-}

-- | The time of the last attempt to patch the instance with @NoReboot@ specified as the reboot option.
--
-- /Note:/ Consider using 'lastNoRebootInstallOperationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsLastNoRebootInstallOperationTime :: Lens.Lens' InstancePatchState (Core.Maybe Core.NominalDiffTime)
ipsLastNoRebootInstallOperationTime = Lens.field @"lastNoRebootInstallOperationTime"
{-# INLINEABLE ipsLastNoRebootInstallOperationTime #-}
{-# DEPRECATED lastNoRebootInstallOperationTime "Use generic-lens or generic-optics with 'lastNoRebootInstallOperationTime' instead"  #-}

-- | The number of patches from the patch baseline that are applicable for the instance but aren't currently installed.
--
-- /Note:/ Consider using 'missingCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsMissingCount :: Lens.Lens' InstancePatchState (Core.Maybe Core.Int)
ipsMissingCount = Lens.field @"missingCount"
{-# INLINEABLE ipsMissingCount #-}
{-# DEPRECATED missingCount "Use generic-lens or generic-optics with 'missingCount' instead"  #-}

-- | The number of patches from the patch baseline that aren't applicable for the instance and therefore aren't installed on the instance. This number may be truncated if the list of patch names is very large. The number of patches beyond this limit are reported in @UnreportedNotApplicableCount@ .
--
-- /Note:/ Consider using 'notApplicableCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsNotApplicableCount :: Lens.Lens' InstancePatchState (Core.Maybe Core.Int)
ipsNotApplicableCount = Lens.field @"notApplicableCount"
{-# INLINEABLE ipsNotApplicableCount #-}
{-# DEPRECATED notApplicableCount "Use generic-lens or generic-optics with 'notApplicableCount' instead"  #-}

-- | Placeholder information. This field will always be empty in the current release of the service.
--
-- /Note:/ Consider using 'ownerInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsOwnerInformation :: Lens.Lens' InstancePatchState (Core.Maybe Types.OwnerInformation)
ipsOwnerInformation = Lens.field @"ownerInformation"
{-# INLINEABLE ipsOwnerInformation #-}
{-# DEPRECATED ownerInformation "Use generic-lens or generic-optics with 'ownerInformation' instead"  #-}

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
ipsRebootOption :: Lens.Lens' InstancePatchState (Core.Maybe Types.RebootOption)
ipsRebootOption = Lens.field @"rebootOption"
{-# INLINEABLE ipsRebootOption #-}
{-# DEPRECATED rebootOption "Use generic-lens or generic-optics with 'rebootOption' instead"  #-}

-- | The ID of the patch baseline snapshot used during the patching operation when this compliance data was collected.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsSnapshotId :: Lens.Lens' InstancePatchState (Core.Maybe Types.SnapshotId)
ipsSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE ipsSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | The number of patches beyond the supported limit of @NotApplicableCount@ that are not reported by name to Systems Manager Inventory.
--
-- /Note:/ Consider using 'unreportedNotApplicableCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsUnreportedNotApplicableCount :: Lens.Lens' InstancePatchState (Core.Maybe Core.Int)
ipsUnreportedNotApplicableCount = Lens.field @"unreportedNotApplicableCount"
{-# INLINEABLE ipsUnreportedNotApplicableCount #-}
{-# DEPRECATED unreportedNotApplicableCount "Use generic-lens or generic-optics with 'unreportedNotApplicableCount' instead"  #-}

instance Core.FromJSON InstancePatchState where
        parseJSON
          = Core.withObject "InstancePatchState" Core.$
              \ x ->
                InstancePatchState' Core.<$>
                  (x Core..: "InstanceId") Core.<*> x Core..: "PatchGroup" Core.<*>
                    x Core..: "BaselineId"
                    Core.<*> x Core..: "OperationStartTime"
                    Core.<*> x Core..: "OperationEndTime"
                    Core.<*> x Core..: "Operation"
                    Core.<*> x Core..:? "FailedCount"
                    Core.<*> x Core..:? "InstallOverrideList"
                    Core.<*> x Core..:? "InstalledCount"
                    Core.<*> x Core..:? "InstalledOtherCount"
                    Core.<*> x Core..:? "InstalledPendingRebootCount"
                    Core.<*> x Core..:? "InstalledRejectedCount"
                    Core.<*> x Core..:? "LastNoRebootInstallOperationTime"
                    Core.<*> x Core..:? "MissingCount"
                    Core.<*> x Core..:? "NotApplicableCount"
                    Core.<*> x Core..:? "OwnerInformation"
                    Core.<*> x Core..:? "RebootOption"
                    Core.<*> x Core..:? "SnapshotId"
                    Core.<*> x Core..:? "UnreportedNotApplicableCount"
