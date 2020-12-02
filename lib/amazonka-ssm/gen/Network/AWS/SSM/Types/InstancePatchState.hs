{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstancePatchState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstancePatchState where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.PatchOperationType
import Network.AWS.SSM.Types.RebootOption

-- | Defines the high-level patch compliance state for a managed instance, providing information about the number of installed, missing, not applicable, and failed patches along with metadata about the operation when this information was gathered for the instance.
--
--
--
-- /See:/ 'instancePatchState' smart constructor.
data InstancePatchState = InstancePatchState'
  { _ipsUnreportedNotApplicableCount ::
      !(Maybe Int),
    _ipsRebootOption :: !(Maybe RebootOption),
    _ipsInstalledPendingRebootCount :: !(Maybe Int),
    _ipsOwnerInformation :: !(Maybe (Sensitive Text)),
    _ipsInstalledRejectedCount :: !(Maybe Int),
    _ipsFailedCount :: !(Maybe Int),
    _ipsInstalledOtherCount :: !(Maybe Int),
    _ipsMissingCount :: !(Maybe Int),
    _ipsInstallOverrideList :: !(Maybe Text),
    _ipsNotApplicableCount :: !(Maybe Int),
    _ipsInstalledCount :: !(Maybe Int),
    _ipsLastNoRebootInstallOperationTime ::
      !(Maybe POSIX),
    _ipsSnapshotId :: !(Maybe Text),
    _ipsInstanceId :: !Text,
    _ipsPatchGroup :: !Text,
    _ipsBaselineId :: !Text,
    _ipsOperationStartTime :: !POSIX,
    _ipsOperationEndTime :: !POSIX,
    _ipsOperation :: !PatchOperationType
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstancePatchState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipsUnreportedNotApplicableCount' - The number of patches beyond the supported limit of @NotApplicableCount@ that are not reported by name to Systems Manager Inventory.
--
-- * 'ipsRebootOption' - Indicates the reboot option specified in the patch baseline.     * __RebootIfNeeded__ : Patch Manager tries to reboot the instance if it installed any patches, or if any patches are detected with a status of @InstalledPendingReboot@ .     * __NoReboot__ : Patch Manager attempts to install missing packages without trying to reboot the system. Patches installed with this option are assigned a status of @InstalledPendingReboot@ . These patches might not be in effect until a reboot is performed.
--
-- * 'ipsInstalledPendingRebootCount' - The number of patches installed by Patch Manager since the last time the instance was rebooted.
--
-- * 'ipsOwnerInformation' - Placeholder information. This field will always be empty in the current release of the service.
--
-- * 'ipsInstalledRejectedCount' - The number of patches installed on an instance that are specified in a @RejectedPatches@ list. Patches with a status of /InstalledRejected/ were typically installed before they were added to a @RejectedPatches@ list.
--
-- * 'ipsFailedCount' - The number of patches from the patch baseline that were attempted to be installed during the last patching operation, but failed to install.
--
-- * 'ipsInstalledOtherCount' - The number of patches not specified in the patch baseline that are installed on the instance.
--
-- * 'ipsMissingCount' - The number of patches from the patch baseline that are applicable for the instance but aren't currently installed.
--
-- * 'ipsInstallOverrideList' - An https URL or an Amazon S3 path-style URL to a list of patches to be installed. This patch installation list, which you maintain in an S3 bucket in YAML format and specify in the SSM document @AWS-RunPatchBaseline@ , overrides the patches specified by the default patch baseline. For more information about the @InstallOverrideList@ parameter, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-about-aws-runpatchbaseline.html About the SSM document AWS-RunPatchBaseline> in the /AWS Systems Manager User Guide/ .
--
-- * 'ipsNotApplicableCount' - The number of patches from the patch baseline that aren't applicable for the instance and therefore aren't installed on the instance. This number may be truncated if the list of patch names is very large. The number of patches beyond this limit are reported in @UnreportedNotApplicableCount@ .
--
-- * 'ipsInstalledCount' - The number of patches from the patch baseline that are installed on the instance.
--
-- * 'ipsLastNoRebootInstallOperationTime' - The time of the last attempt to patch the instance with @NoReboot@ specified as the reboot option.
--
-- * 'ipsSnapshotId' - The ID of the patch baseline snapshot used during the patching operation when this compliance data was collected.
--
-- * 'ipsInstanceId' - The ID of the managed instance the high-level patch compliance information was collected for.
--
-- * 'ipsPatchGroup' - The name of the patch group the managed instance belongs to.
--
-- * 'ipsBaselineId' - The ID of the patch baseline used to patch the instance.
--
-- * 'ipsOperationStartTime' - The time the most recent patching operation was started on the instance.
--
-- * 'ipsOperationEndTime' - The time the most recent patching operation completed on the instance.
--
-- * 'ipsOperation' - The type of patching operation that was performed: @SCAN@ (assess patch compliance state) or @INSTALL@ (install missing patches).
instancePatchState ::
  -- | 'ipsInstanceId'
  Text ->
  -- | 'ipsPatchGroup'
  Text ->
  -- | 'ipsBaselineId'
  Text ->
  -- | 'ipsOperationStartTime'
  UTCTime ->
  -- | 'ipsOperationEndTime'
  UTCTime ->
  -- | 'ipsOperation'
  PatchOperationType ->
  InstancePatchState
instancePatchState
  pInstanceId_
  pPatchGroup_
  pBaselineId_
  pOperationStartTime_
  pOperationEndTime_
  pOperation_ =
    InstancePatchState'
      { _ipsUnreportedNotApplicableCount = Nothing,
        _ipsRebootOption = Nothing,
        _ipsInstalledPendingRebootCount = Nothing,
        _ipsOwnerInformation = Nothing,
        _ipsInstalledRejectedCount = Nothing,
        _ipsFailedCount = Nothing,
        _ipsInstalledOtherCount = Nothing,
        _ipsMissingCount = Nothing,
        _ipsInstallOverrideList = Nothing,
        _ipsNotApplicableCount = Nothing,
        _ipsInstalledCount = Nothing,
        _ipsLastNoRebootInstallOperationTime = Nothing,
        _ipsSnapshotId = Nothing,
        _ipsInstanceId = pInstanceId_,
        _ipsPatchGroup = pPatchGroup_,
        _ipsBaselineId = pBaselineId_,
        _ipsOperationStartTime = _Time # pOperationStartTime_,
        _ipsOperationEndTime = _Time # pOperationEndTime_,
        _ipsOperation = pOperation_
      }

-- | The number of patches beyond the supported limit of @NotApplicableCount@ that are not reported by name to Systems Manager Inventory.
ipsUnreportedNotApplicableCount :: Lens' InstancePatchState (Maybe Int)
ipsUnreportedNotApplicableCount = lens _ipsUnreportedNotApplicableCount (\s a -> s {_ipsUnreportedNotApplicableCount = a})

-- | Indicates the reboot option specified in the patch baseline.     * __RebootIfNeeded__ : Patch Manager tries to reboot the instance if it installed any patches, or if any patches are detected with a status of @InstalledPendingReboot@ .     * __NoReboot__ : Patch Manager attempts to install missing packages without trying to reboot the system. Patches installed with this option are assigned a status of @InstalledPendingReboot@ . These patches might not be in effect until a reboot is performed.
ipsRebootOption :: Lens' InstancePatchState (Maybe RebootOption)
ipsRebootOption = lens _ipsRebootOption (\s a -> s {_ipsRebootOption = a})

-- | The number of patches installed by Patch Manager since the last time the instance was rebooted.
ipsInstalledPendingRebootCount :: Lens' InstancePatchState (Maybe Int)
ipsInstalledPendingRebootCount = lens _ipsInstalledPendingRebootCount (\s a -> s {_ipsInstalledPendingRebootCount = a})

-- | Placeholder information. This field will always be empty in the current release of the service.
ipsOwnerInformation :: Lens' InstancePatchState (Maybe Text)
ipsOwnerInformation = lens _ipsOwnerInformation (\s a -> s {_ipsOwnerInformation = a}) . mapping _Sensitive

-- | The number of patches installed on an instance that are specified in a @RejectedPatches@ list. Patches with a status of /InstalledRejected/ were typically installed before they were added to a @RejectedPatches@ list.
ipsInstalledRejectedCount :: Lens' InstancePatchState (Maybe Int)
ipsInstalledRejectedCount = lens _ipsInstalledRejectedCount (\s a -> s {_ipsInstalledRejectedCount = a})

-- | The number of patches from the patch baseline that were attempted to be installed during the last patching operation, but failed to install.
ipsFailedCount :: Lens' InstancePatchState (Maybe Int)
ipsFailedCount = lens _ipsFailedCount (\s a -> s {_ipsFailedCount = a})

-- | The number of patches not specified in the patch baseline that are installed on the instance.
ipsInstalledOtherCount :: Lens' InstancePatchState (Maybe Int)
ipsInstalledOtherCount = lens _ipsInstalledOtherCount (\s a -> s {_ipsInstalledOtherCount = a})

-- | The number of patches from the patch baseline that are applicable for the instance but aren't currently installed.
ipsMissingCount :: Lens' InstancePatchState (Maybe Int)
ipsMissingCount = lens _ipsMissingCount (\s a -> s {_ipsMissingCount = a})

-- | An https URL or an Amazon S3 path-style URL to a list of patches to be installed. This patch installation list, which you maintain in an S3 bucket in YAML format and specify in the SSM document @AWS-RunPatchBaseline@ , overrides the patches specified by the default patch baseline. For more information about the @InstallOverrideList@ parameter, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-about-aws-runpatchbaseline.html About the SSM document AWS-RunPatchBaseline> in the /AWS Systems Manager User Guide/ .
ipsInstallOverrideList :: Lens' InstancePatchState (Maybe Text)
ipsInstallOverrideList = lens _ipsInstallOverrideList (\s a -> s {_ipsInstallOverrideList = a})

-- | The number of patches from the patch baseline that aren't applicable for the instance and therefore aren't installed on the instance. This number may be truncated if the list of patch names is very large. The number of patches beyond this limit are reported in @UnreportedNotApplicableCount@ .
ipsNotApplicableCount :: Lens' InstancePatchState (Maybe Int)
ipsNotApplicableCount = lens _ipsNotApplicableCount (\s a -> s {_ipsNotApplicableCount = a})

-- | The number of patches from the patch baseline that are installed on the instance.
ipsInstalledCount :: Lens' InstancePatchState (Maybe Int)
ipsInstalledCount = lens _ipsInstalledCount (\s a -> s {_ipsInstalledCount = a})

-- | The time of the last attempt to patch the instance with @NoReboot@ specified as the reboot option.
ipsLastNoRebootInstallOperationTime :: Lens' InstancePatchState (Maybe UTCTime)
ipsLastNoRebootInstallOperationTime = lens _ipsLastNoRebootInstallOperationTime (\s a -> s {_ipsLastNoRebootInstallOperationTime = a}) . mapping _Time

-- | The ID of the patch baseline snapshot used during the patching operation when this compliance data was collected.
ipsSnapshotId :: Lens' InstancePatchState (Maybe Text)
ipsSnapshotId = lens _ipsSnapshotId (\s a -> s {_ipsSnapshotId = a})

-- | The ID of the managed instance the high-level patch compliance information was collected for.
ipsInstanceId :: Lens' InstancePatchState Text
ipsInstanceId = lens _ipsInstanceId (\s a -> s {_ipsInstanceId = a})

-- | The name of the patch group the managed instance belongs to.
ipsPatchGroup :: Lens' InstancePatchState Text
ipsPatchGroup = lens _ipsPatchGroup (\s a -> s {_ipsPatchGroup = a})

-- | The ID of the patch baseline used to patch the instance.
ipsBaselineId :: Lens' InstancePatchState Text
ipsBaselineId = lens _ipsBaselineId (\s a -> s {_ipsBaselineId = a})

-- | The time the most recent patching operation was started on the instance.
ipsOperationStartTime :: Lens' InstancePatchState UTCTime
ipsOperationStartTime = lens _ipsOperationStartTime (\s a -> s {_ipsOperationStartTime = a}) . _Time

-- | The time the most recent patching operation completed on the instance.
ipsOperationEndTime :: Lens' InstancePatchState UTCTime
ipsOperationEndTime = lens _ipsOperationEndTime (\s a -> s {_ipsOperationEndTime = a}) . _Time

-- | The type of patching operation that was performed: @SCAN@ (assess patch compliance state) or @INSTALL@ (install missing patches).
ipsOperation :: Lens' InstancePatchState PatchOperationType
ipsOperation = lens _ipsOperation (\s a -> s {_ipsOperation = a})

instance FromJSON InstancePatchState where
  parseJSON =
    withObject
      "InstancePatchState"
      ( \x ->
          InstancePatchState'
            <$> (x .:? "UnreportedNotApplicableCount")
            <*> (x .:? "RebootOption")
            <*> (x .:? "InstalledPendingRebootCount")
            <*> (x .:? "OwnerInformation")
            <*> (x .:? "InstalledRejectedCount")
            <*> (x .:? "FailedCount")
            <*> (x .:? "InstalledOtherCount")
            <*> (x .:? "MissingCount")
            <*> (x .:? "InstallOverrideList")
            <*> (x .:? "NotApplicableCount")
            <*> (x .:? "InstalledCount")
            <*> (x .:? "LastNoRebootInstallOperationTime")
            <*> (x .:? "SnapshotId")
            <*> (x .: "InstanceId")
            <*> (x .: "PatchGroup")
            <*> (x .: "BaselineId")
            <*> (x .: "OperationStartTime")
            <*> (x .: "OperationEndTime")
            <*> (x .: "Operation")
      )

instance Hashable InstancePatchState

instance NFData InstancePatchState
