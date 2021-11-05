{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.StorageGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.StorageGateway where

import Amazonka.StorageGateway
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.StorageGateway.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCancelArchival $
--             newCancelArchival
--
--         , requestCreateStorediSCSIVolume $
--             newCreateStorediSCSIVolume
--
--         , requestDescribeFileSystemAssociations $
--             newDescribeFileSystemAssociations
--
--         , requestUpdateSMBLocalGroups $
--             newUpdateSMBLocalGroups
--
--         , requestCreateNFSFileShare $
--             newCreateNFSFileShare
--
--         , requestAssociateFileSystem $
--             newAssociateFileSystem
--
--         , requestDetachVolume $
--             newDetachVolume
--
--         , requestDescribeChapCredentials $
--             newDescribeChapCredentials
--
--         , requestSetLocalConsolePassword $
--             newSetLocalConsolePassword
--
--         , requestCreateTapes $
--             newCreateTapes
--
--         , requestUpdateVTLDeviceType $
--             newUpdateVTLDeviceType
--
--         , requestCreateCachediSCSIVolume $
--             newCreateCachediSCSIVolume
--
--         , requestListFileShares $
--             newListFileShares
--
--         , requestJoinDomain $
--             newJoinDomain
--
--         , requestDeleteFileShare $
--             newDeleteFileShare
--
--         , requestListVolumeInitiators $
--             newListVolumeInitiators
--
--         , requestAddUploadBuffer $
--             newAddUploadBuffer
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestNotifyWhenUploaded $
--             newNotifyWhenUploaded
--
--         , requestListTapePools $
--             newListTapePools
--
--         , requestDeleteTapePool $
--             newDeleteTapePool
--
--         , requestUpdateGatewayInformation $
--             newUpdateGatewayInformation
--
--         , requestDescribeMaintenanceStartTime $
--             newDescribeMaintenanceStartTime
--
--         , requestAssignTapePool $
--             newAssignTapePool
--
--         , requestDescribeWorkingStorage $
--             newDescribeWorkingStorage
--
--         , requestDescribeCachediSCSIVolumes $
--             newDescribeCachediSCSIVolumes
--
--         , requestAddCache $
--             newAddCache
--
--         , requestCreateTapePool $
--             newCreateTapePool
--
--         , requestStartGateway $
--             newStartGateway
--
--         , requestShutdownGateway $
--             newShutdownGateway
--
--         , requestListAutomaticTapeCreationPolicies $
--             newListAutomaticTapeCreationPolicies
--
--         , requestUpdateGatewaySoftwareNow $
--             newUpdateGatewaySoftwareNow
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestUpdateFileSystemAssociation $
--             newUpdateFileSystemAssociation
--
--         , requestCreateSMBFileShare $
--             newCreateSMBFileShare
--
--         , requestDeleteChapCredentials $
--             newDeleteChapCredentials
--
--         , requestUpdateChapCredentials $
--             newUpdateChapCredentials
--
--         , requestAttachVolume $
--             newAttachVolume
--
--         , requestDescribeAvailabilityMonitorTest $
--             newDescribeAvailabilityMonitorTest
--
--         , requestDescribeUploadBuffer $
--             newDescribeUploadBuffer
--
--         , requestDescribeTapes $
--             newDescribeTapes
--
--         , requestDescribeStorediSCSIVolumes $
--             newDescribeStorediSCSIVolumes
--
--         , requestDisassociateFileSystem $
--             newDisassociateFileSystem
--
--         , requestSetSMBGuestPassword $
--             newSetSMBGuestPassword
--
--         , requestCreateSnapshotFromVolumeRecoveryPoint $
--             newCreateSnapshotFromVolumeRecoveryPoint
--
--         , requestRetrieveTapeRecoveryPoint $
--             newRetrieveTapeRecoveryPoint
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestDeleteGateway $
--             newDeleteGateway
--
--         , requestUpdateMaintenanceStartTime $
--             newUpdateMaintenanceStartTime
--
--         , requestDescribeGatewayInformation $
--             newDescribeGatewayInformation
--
--         , requestRefreshCache $
--             newRefreshCache
--
--         , requestUpdateNFSFileShare $
--             newUpdateNFSFileShare
--
--         , requestRetrieveTapeArchive $
--             newRetrieveTapeArchive
--
--         , requestDescribeTapeArchives $
--             newDescribeTapeArchives
--
--         , requestUpdateBandwidthRateLimitSchedule $
--             newUpdateBandwidthRateLimitSchedule
--
--         , requestDisableGateway $
--             newDisableGateway
--
--         , requestDescribeSMBSettings $
--             newDescribeSMBSettings
--
--         , requestDescribeSnapshotSchedule $
--             newDescribeSnapshotSchedule
--
--         , requestCreateTapeWithBarcode $
--             newCreateTapeWithBarcode
--
--         , requestDescribeBandwidthRateLimit $
--             newDescribeBandwidthRateLimit
--
--         , requestDeleteAutomaticTapeCreationPolicy $
--             newDeleteAutomaticTapeCreationPolicy
--
--         , requestUpdateAutomaticTapeCreationPolicy $
--             newUpdateAutomaticTapeCreationPolicy
--
--         , requestUpdateSMBFileShareVisibility $
--             newUpdateSMBFileShareVisibility
--
--         , requestDeleteSnapshotSchedule $
--             newDeleteSnapshotSchedule
--
--         , requestUpdateSnapshotSchedule $
--             newUpdateSnapshotSchedule
--
--         , requestDescribeBandwidthRateLimitSchedule $
--             newDescribeBandwidthRateLimitSchedule
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestUpdateSMBSecurityStrategy $
--             newUpdateSMBSecurityStrategy
--
--         , requestCancelRetrieval $
--             newCancelRetrieval
--
--         , requestDescribeVTLDevices $
--             newDescribeVTLDevices
--
--         , requestStartAvailabilityMonitorTest $
--             newStartAvailabilityMonitorTest
--
--         , requestDeleteTapeArchive $
--             newDeleteTapeArchive
--
--         , requestListFileSystemAssociations $
--             newListFileSystemAssociations
--
--         , requestUpdateSMBFileShare $
--             newUpdateSMBFileShare
--
--         , requestDescribeNFSFileShares $
--             newDescribeNFSFileShares
--
--         , requestListVolumeRecoveryPoints $
--             newListVolumeRecoveryPoints
--
--         , requestListTapes $
--             newListTapes
--
--         , requestResetCache $
--             newResetCache
--
--         , requestDescribeSMBFileShares $
--             newDescribeSMBFileShares
--
--         , requestListGateways $
--             newListGateways
--
--         , requestDeleteTape $
--             newDeleteTape
--
--         , requestListLocalDisks $
--             newListLocalDisks
--
--         , requestListVolumes $
--             newListVolumes
--
--         , requestUpdateBandwidthRateLimit $
--             newUpdateBandwidthRateLimit
--
--         , requestAddWorkingStorage $
--             newAddWorkingStorage
--
--         , requestDescribeTapeRecoveryPoints $
--             newDescribeTapeRecoveryPoints
--
--         , requestDeleteBandwidthRateLimit $
--             newDeleteBandwidthRateLimit
--
--         , requestActivateGateway $
--             newActivateGateway
--
--         , requestDescribeCache $
--             newDescribeCache
--
--         , requestDeleteVolume $
--             newDeleteVolume
--
--           ]

--     , testGroup "response"
--         [ responseCancelArchival $
--             newCancelArchivalResponse
--
--         , responseCreateStorediSCSIVolume $
--             newCreateStorediSCSIVolumeResponse
--
--         , responseDescribeFileSystemAssociations $
--             newDescribeFileSystemAssociationsResponse
--
--         , responseUpdateSMBLocalGroups $
--             newUpdateSMBLocalGroupsResponse
--
--         , responseCreateNFSFileShare $
--             newCreateNFSFileShareResponse
--
--         , responseAssociateFileSystem $
--             newAssociateFileSystemResponse
--
--         , responseDetachVolume $
--             newDetachVolumeResponse
--
--         , responseDescribeChapCredentials $
--             newDescribeChapCredentialsResponse
--
--         , responseSetLocalConsolePassword $
--             newSetLocalConsolePasswordResponse
--
--         , responseCreateTapes $
--             newCreateTapesResponse
--
--         , responseUpdateVTLDeviceType $
--             newUpdateVTLDeviceTypeResponse
--
--         , responseCreateCachediSCSIVolume $
--             newCreateCachediSCSIVolumeResponse
--
--         , responseListFileShares $
--             newListFileSharesResponse
--
--         , responseJoinDomain $
--             newJoinDomainResponse
--
--         , responseDeleteFileShare $
--             newDeleteFileShareResponse
--
--         , responseListVolumeInitiators $
--             newListVolumeInitiatorsResponse
--
--         , responseAddUploadBuffer $
--             newAddUploadBufferResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseNotifyWhenUploaded $
--             newNotifyWhenUploadedResponse
--
--         , responseListTapePools $
--             newListTapePoolsResponse
--
--         , responseDeleteTapePool $
--             newDeleteTapePoolResponse
--
--         , responseUpdateGatewayInformation $
--             newUpdateGatewayInformationResponse
--
--         , responseDescribeMaintenanceStartTime $
--             newDescribeMaintenanceStartTimeResponse
--
--         , responseAssignTapePool $
--             newAssignTapePoolResponse
--
--         , responseDescribeWorkingStorage $
--             newDescribeWorkingStorageResponse
--
--         , responseDescribeCachediSCSIVolumes $
--             newDescribeCachediSCSIVolumesResponse
--
--         , responseAddCache $
--             newAddCacheResponse
--
--         , responseCreateTapePool $
--             newCreateTapePoolResponse
--
--         , responseStartGateway $
--             newStartGatewayResponse
--
--         , responseShutdownGateway $
--             newShutdownGatewayResponse
--
--         , responseListAutomaticTapeCreationPolicies $
--             newListAutomaticTapeCreationPoliciesResponse
--
--         , responseUpdateGatewaySoftwareNow $
--             newUpdateGatewaySoftwareNowResponse
--
--         , responseRemoveTagsFromResource $
--             newRemoveTagsFromResourceResponse
--
--         , responseUpdateFileSystemAssociation $
--             newUpdateFileSystemAssociationResponse
--
--         , responseCreateSMBFileShare $
--             newCreateSMBFileShareResponse
--
--         , responseDeleteChapCredentials $
--             newDeleteChapCredentialsResponse
--
--         , responseUpdateChapCredentials $
--             newUpdateChapCredentialsResponse
--
--         , responseAttachVolume $
--             newAttachVolumeResponse
--
--         , responseDescribeAvailabilityMonitorTest $
--             newDescribeAvailabilityMonitorTestResponse
--
--         , responseDescribeUploadBuffer $
--             newDescribeUploadBufferResponse
--
--         , responseDescribeTapes $
--             newDescribeTapesResponse
--
--         , responseDescribeStorediSCSIVolumes $
--             newDescribeStorediSCSIVolumesResponse
--
--         , responseDisassociateFileSystem $
--             newDisassociateFileSystemResponse
--
--         , responseSetSMBGuestPassword $
--             newSetSMBGuestPasswordResponse
--
--         , responseCreateSnapshotFromVolumeRecoveryPoint $
--             newCreateSnapshotFromVolumeRecoveryPointResponse
--
--         , responseRetrieveTapeRecoveryPoint $
--             newRetrieveTapeRecoveryPointResponse
--
--         , responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseDeleteGateway $
--             newDeleteGatewayResponse
--
--         , responseUpdateMaintenanceStartTime $
--             newUpdateMaintenanceStartTimeResponse
--
--         , responseDescribeGatewayInformation $
--             newDescribeGatewayInformationResponse
--
--         , responseRefreshCache $
--             newRefreshCacheResponse
--
--         , responseUpdateNFSFileShare $
--             newUpdateNFSFileShareResponse
--
--         , responseRetrieveTapeArchive $
--             newRetrieveTapeArchiveResponse
--
--         , responseDescribeTapeArchives $
--             newDescribeTapeArchivesResponse
--
--         , responseUpdateBandwidthRateLimitSchedule $
--             newUpdateBandwidthRateLimitScheduleResponse
--
--         , responseDisableGateway $
--             newDisableGatewayResponse
--
--         , responseDescribeSMBSettings $
--             newDescribeSMBSettingsResponse
--
--         , responseDescribeSnapshotSchedule $
--             newDescribeSnapshotScheduleResponse
--
--         , responseCreateTapeWithBarcode $
--             newCreateTapeWithBarcodeResponse
--
--         , responseDescribeBandwidthRateLimit $
--             newDescribeBandwidthRateLimitResponse
--
--         , responseDeleteAutomaticTapeCreationPolicy $
--             newDeleteAutomaticTapeCreationPolicyResponse
--
--         , responseUpdateAutomaticTapeCreationPolicy $
--             newUpdateAutomaticTapeCreationPolicyResponse
--
--         , responseUpdateSMBFileShareVisibility $
--             newUpdateSMBFileShareVisibilityResponse
--
--         , responseDeleteSnapshotSchedule $
--             newDeleteSnapshotScheduleResponse
--
--         , responseUpdateSnapshotSchedule $
--             newUpdateSnapshotScheduleResponse
--
--         , responseDescribeBandwidthRateLimitSchedule $
--             newDescribeBandwidthRateLimitScheduleResponse
--
--         , responseCreateSnapshot $
--             newCreateSnapshotResponse
--
--         , responseUpdateSMBSecurityStrategy $
--             newUpdateSMBSecurityStrategyResponse
--
--         , responseCancelRetrieval $
--             newCancelRetrievalResponse
--
--         , responseDescribeVTLDevices $
--             newDescribeVTLDevicesResponse
--
--         , responseStartAvailabilityMonitorTest $
--             newStartAvailabilityMonitorTestResponse
--
--         , responseDeleteTapeArchive $
--             newDeleteTapeArchiveResponse
--
--         , responseListFileSystemAssociations $
--             newListFileSystemAssociationsResponse
--
--         , responseUpdateSMBFileShare $
--             newUpdateSMBFileShareResponse
--
--         , responseDescribeNFSFileShares $
--             newDescribeNFSFileSharesResponse
--
--         , responseListVolumeRecoveryPoints $
--             newListVolumeRecoveryPointsResponse
--
--         , responseListTapes $
--             newListTapesResponse
--
--         , responseResetCache $
--             newResetCacheResponse
--
--         , responseDescribeSMBFileShares $
--             newDescribeSMBFileSharesResponse
--
--         , responseListGateways $
--             newListGatewaysResponse
--
--         , responseDeleteTape $
--             newDeleteTapeResponse
--
--         , responseListLocalDisks $
--             newListLocalDisksResponse
--
--         , responseListVolumes $
--             newListVolumesResponse
--
--         , responseUpdateBandwidthRateLimit $
--             newUpdateBandwidthRateLimitResponse
--
--         , responseAddWorkingStorage $
--             newAddWorkingStorageResponse
--
--         , responseDescribeTapeRecoveryPoints $
--             newDescribeTapeRecoveryPointsResponse
--
--         , responseDeleteBandwidthRateLimit $
--             newDeleteBandwidthRateLimitResponse
--
--         , responseActivateGateway $
--             newActivateGatewayResponse
--
--         , responseDescribeCache $
--             newDescribeCacheResponse
--
--         , responseDeleteVolume $
--             newDeleteVolumeResponse
--
--           ]
--     ]

-- Requests

requestCancelArchival :: CancelArchival -> TestTree
requestCancelArchival =
  req
    "CancelArchival"
    "fixture/CancelArchival.yaml"

requestCreateStorediSCSIVolume :: CreateStorediSCSIVolume -> TestTree
requestCreateStorediSCSIVolume =
  req
    "CreateStorediSCSIVolume"
    "fixture/CreateStorediSCSIVolume.yaml"

requestDescribeFileSystemAssociations :: DescribeFileSystemAssociations -> TestTree
requestDescribeFileSystemAssociations =
  req
    "DescribeFileSystemAssociations"
    "fixture/DescribeFileSystemAssociations.yaml"

requestUpdateSMBLocalGroups :: UpdateSMBLocalGroups -> TestTree
requestUpdateSMBLocalGroups =
  req
    "UpdateSMBLocalGroups"
    "fixture/UpdateSMBLocalGroups.yaml"

requestCreateNFSFileShare :: CreateNFSFileShare -> TestTree
requestCreateNFSFileShare =
  req
    "CreateNFSFileShare"
    "fixture/CreateNFSFileShare.yaml"

requestAssociateFileSystem :: AssociateFileSystem -> TestTree
requestAssociateFileSystem =
  req
    "AssociateFileSystem"
    "fixture/AssociateFileSystem.yaml"

requestDetachVolume :: DetachVolume -> TestTree
requestDetachVolume =
  req
    "DetachVolume"
    "fixture/DetachVolume.yaml"

requestDescribeChapCredentials :: DescribeChapCredentials -> TestTree
requestDescribeChapCredentials =
  req
    "DescribeChapCredentials"
    "fixture/DescribeChapCredentials.yaml"

requestSetLocalConsolePassword :: SetLocalConsolePassword -> TestTree
requestSetLocalConsolePassword =
  req
    "SetLocalConsolePassword"
    "fixture/SetLocalConsolePassword.yaml"

requestCreateTapes :: CreateTapes -> TestTree
requestCreateTapes =
  req
    "CreateTapes"
    "fixture/CreateTapes.yaml"

requestUpdateVTLDeviceType :: UpdateVTLDeviceType -> TestTree
requestUpdateVTLDeviceType =
  req
    "UpdateVTLDeviceType"
    "fixture/UpdateVTLDeviceType.yaml"

requestCreateCachediSCSIVolume :: CreateCachediSCSIVolume -> TestTree
requestCreateCachediSCSIVolume =
  req
    "CreateCachediSCSIVolume"
    "fixture/CreateCachediSCSIVolume.yaml"

requestListFileShares :: ListFileShares -> TestTree
requestListFileShares =
  req
    "ListFileShares"
    "fixture/ListFileShares.yaml"

requestJoinDomain :: JoinDomain -> TestTree
requestJoinDomain =
  req
    "JoinDomain"
    "fixture/JoinDomain.yaml"

requestDeleteFileShare :: DeleteFileShare -> TestTree
requestDeleteFileShare =
  req
    "DeleteFileShare"
    "fixture/DeleteFileShare.yaml"

requestListVolumeInitiators :: ListVolumeInitiators -> TestTree
requestListVolumeInitiators =
  req
    "ListVolumeInitiators"
    "fixture/ListVolumeInitiators.yaml"

requestAddUploadBuffer :: AddUploadBuffer -> TestTree
requestAddUploadBuffer =
  req
    "AddUploadBuffer"
    "fixture/AddUploadBuffer.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestNotifyWhenUploaded :: NotifyWhenUploaded -> TestTree
requestNotifyWhenUploaded =
  req
    "NotifyWhenUploaded"
    "fixture/NotifyWhenUploaded.yaml"

requestListTapePools :: ListTapePools -> TestTree
requestListTapePools =
  req
    "ListTapePools"
    "fixture/ListTapePools.yaml"

requestDeleteTapePool :: DeleteTapePool -> TestTree
requestDeleteTapePool =
  req
    "DeleteTapePool"
    "fixture/DeleteTapePool.yaml"

requestUpdateGatewayInformation :: UpdateGatewayInformation -> TestTree
requestUpdateGatewayInformation =
  req
    "UpdateGatewayInformation"
    "fixture/UpdateGatewayInformation.yaml"

requestDescribeMaintenanceStartTime :: DescribeMaintenanceStartTime -> TestTree
requestDescribeMaintenanceStartTime =
  req
    "DescribeMaintenanceStartTime"
    "fixture/DescribeMaintenanceStartTime.yaml"

requestAssignTapePool :: AssignTapePool -> TestTree
requestAssignTapePool =
  req
    "AssignTapePool"
    "fixture/AssignTapePool.yaml"

requestDescribeWorkingStorage :: DescribeWorkingStorage -> TestTree
requestDescribeWorkingStorage =
  req
    "DescribeWorkingStorage"
    "fixture/DescribeWorkingStorage.yaml"

requestDescribeCachediSCSIVolumes :: DescribeCachediSCSIVolumes -> TestTree
requestDescribeCachediSCSIVolumes =
  req
    "DescribeCachediSCSIVolumes"
    "fixture/DescribeCachediSCSIVolumes.yaml"

requestAddCache :: AddCache -> TestTree
requestAddCache =
  req
    "AddCache"
    "fixture/AddCache.yaml"

requestCreateTapePool :: CreateTapePool -> TestTree
requestCreateTapePool =
  req
    "CreateTapePool"
    "fixture/CreateTapePool.yaml"

requestStartGateway :: StartGateway -> TestTree
requestStartGateway =
  req
    "StartGateway"
    "fixture/StartGateway.yaml"

requestShutdownGateway :: ShutdownGateway -> TestTree
requestShutdownGateway =
  req
    "ShutdownGateway"
    "fixture/ShutdownGateway.yaml"

requestListAutomaticTapeCreationPolicies :: ListAutomaticTapeCreationPolicies -> TestTree
requestListAutomaticTapeCreationPolicies =
  req
    "ListAutomaticTapeCreationPolicies"
    "fixture/ListAutomaticTapeCreationPolicies.yaml"

requestUpdateGatewaySoftwareNow :: UpdateGatewaySoftwareNow -> TestTree
requestUpdateGatewaySoftwareNow =
  req
    "UpdateGatewaySoftwareNow"
    "fixture/UpdateGatewaySoftwareNow.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestUpdateFileSystemAssociation :: UpdateFileSystemAssociation -> TestTree
requestUpdateFileSystemAssociation =
  req
    "UpdateFileSystemAssociation"
    "fixture/UpdateFileSystemAssociation.yaml"

requestCreateSMBFileShare :: CreateSMBFileShare -> TestTree
requestCreateSMBFileShare =
  req
    "CreateSMBFileShare"
    "fixture/CreateSMBFileShare.yaml"

requestDeleteChapCredentials :: DeleteChapCredentials -> TestTree
requestDeleteChapCredentials =
  req
    "DeleteChapCredentials"
    "fixture/DeleteChapCredentials.yaml"

requestUpdateChapCredentials :: UpdateChapCredentials -> TestTree
requestUpdateChapCredentials =
  req
    "UpdateChapCredentials"
    "fixture/UpdateChapCredentials.yaml"

requestAttachVolume :: AttachVolume -> TestTree
requestAttachVolume =
  req
    "AttachVolume"
    "fixture/AttachVolume.yaml"

requestDescribeAvailabilityMonitorTest :: DescribeAvailabilityMonitorTest -> TestTree
requestDescribeAvailabilityMonitorTest =
  req
    "DescribeAvailabilityMonitorTest"
    "fixture/DescribeAvailabilityMonitorTest.yaml"

requestDescribeUploadBuffer :: DescribeUploadBuffer -> TestTree
requestDescribeUploadBuffer =
  req
    "DescribeUploadBuffer"
    "fixture/DescribeUploadBuffer.yaml"

requestDescribeTapes :: DescribeTapes -> TestTree
requestDescribeTapes =
  req
    "DescribeTapes"
    "fixture/DescribeTapes.yaml"

requestDescribeStorediSCSIVolumes :: DescribeStorediSCSIVolumes -> TestTree
requestDescribeStorediSCSIVolumes =
  req
    "DescribeStorediSCSIVolumes"
    "fixture/DescribeStorediSCSIVolumes.yaml"

requestDisassociateFileSystem :: DisassociateFileSystem -> TestTree
requestDisassociateFileSystem =
  req
    "DisassociateFileSystem"
    "fixture/DisassociateFileSystem.yaml"

requestSetSMBGuestPassword :: SetSMBGuestPassword -> TestTree
requestSetSMBGuestPassword =
  req
    "SetSMBGuestPassword"
    "fixture/SetSMBGuestPassword.yaml"

requestCreateSnapshotFromVolumeRecoveryPoint :: CreateSnapshotFromVolumeRecoveryPoint -> TestTree
requestCreateSnapshotFromVolumeRecoveryPoint =
  req
    "CreateSnapshotFromVolumeRecoveryPoint"
    "fixture/CreateSnapshotFromVolumeRecoveryPoint.yaml"

requestRetrieveTapeRecoveryPoint :: RetrieveTapeRecoveryPoint -> TestTree
requestRetrieveTapeRecoveryPoint =
  req
    "RetrieveTapeRecoveryPoint"
    "fixture/RetrieveTapeRecoveryPoint.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestDeleteGateway :: DeleteGateway -> TestTree
requestDeleteGateway =
  req
    "DeleteGateway"
    "fixture/DeleteGateway.yaml"

requestUpdateMaintenanceStartTime :: UpdateMaintenanceStartTime -> TestTree
requestUpdateMaintenanceStartTime =
  req
    "UpdateMaintenanceStartTime"
    "fixture/UpdateMaintenanceStartTime.yaml"

requestDescribeGatewayInformation :: DescribeGatewayInformation -> TestTree
requestDescribeGatewayInformation =
  req
    "DescribeGatewayInformation"
    "fixture/DescribeGatewayInformation.yaml"

requestRefreshCache :: RefreshCache -> TestTree
requestRefreshCache =
  req
    "RefreshCache"
    "fixture/RefreshCache.yaml"

requestUpdateNFSFileShare :: UpdateNFSFileShare -> TestTree
requestUpdateNFSFileShare =
  req
    "UpdateNFSFileShare"
    "fixture/UpdateNFSFileShare.yaml"

requestRetrieveTapeArchive :: RetrieveTapeArchive -> TestTree
requestRetrieveTapeArchive =
  req
    "RetrieveTapeArchive"
    "fixture/RetrieveTapeArchive.yaml"

requestDescribeTapeArchives :: DescribeTapeArchives -> TestTree
requestDescribeTapeArchives =
  req
    "DescribeTapeArchives"
    "fixture/DescribeTapeArchives.yaml"

requestUpdateBandwidthRateLimitSchedule :: UpdateBandwidthRateLimitSchedule -> TestTree
requestUpdateBandwidthRateLimitSchedule =
  req
    "UpdateBandwidthRateLimitSchedule"
    "fixture/UpdateBandwidthRateLimitSchedule.yaml"

requestDisableGateway :: DisableGateway -> TestTree
requestDisableGateway =
  req
    "DisableGateway"
    "fixture/DisableGateway.yaml"

requestDescribeSMBSettings :: DescribeSMBSettings -> TestTree
requestDescribeSMBSettings =
  req
    "DescribeSMBSettings"
    "fixture/DescribeSMBSettings.yaml"

requestDescribeSnapshotSchedule :: DescribeSnapshotSchedule -> TestTree
requestDescribeSnapshotSchedule =
  req
    "DescribeSnapshotSchedule"
    "fixture/DescribeSnapshotSchedule.yaml"

requestCreateTapeWithBarcode :: CreateTapeWithBarcode -> TestTree
requestCreateTapeWithBarcode =
  req
    "CreateTapeWithBarcode"
    "fixture/CreateTapeWithBarcode.yaml"

requestDescribeBandwidthRateLimit :: DescribeBandwidthRateLimit -> TestTree
requestDescribeBandwidthRateLimit =
  req
    "DescribeBandwidthRateLimit"
    "fixture/DescribeBandwidthRateLimit.yaml"

requestDeleteAutomaticTapeCreationPolicy :: DeleteAutomaticTapeCreationPolicy -> TestTree
requestDeleteAutomaticTapeCreationPolicy =
  req
    "DeleteAutomaticTapeCreationPolicy"
    "fixture/DeleteAutomaticTapeCreationPolicy.yaml"

requestUpdateAutomaticTapeCreationPolicy :: UpdateAutomaticTapeCreationPolicy -> TestTree
requestUpdateAutomaticTapeCreationPolicy =
  req
    "UpdateAutomaticTapeCreationPolicy"
    "fixture/UpdateAutomaticTapeCreationPolicy.yaml"

requestUpdateSMBFileShareVisibility :: UpdateSMBFileShareVisibility -> TestTree
requestUpdateSMBFileShareVisibility =
  req
    "UpdateSMBFileShareVisibility"
    "fixture/UpdateSMBFileShareVisibility.yaml"

requestDeleteSnapshotSchedule :: DeleteSnapshotSchedule -> TestTree
requestDeleteSnapshotSchedule =
  req
    "DeleteSnapshotSchedule"
    "fixture/DeleteSnapshotSchedule.yaml"

requestUpdateSnapshotSchedule :: UpdateSnapshotSchedule -> TestTree
requestUpdateSnapshotSchedule =
  req
    "UpdateSnapshotSchedule"
    "fixture/UpdateSnapshotSchedule.yaml"

requestDescribeBandwidthRateLimitSchedule :: DescribeBandwidthRateLimitSchedule -> TestTree
requestDescribeBandwidthRateLimitSchedule =
  req
    "DescribeBandwidthRateLimitSchedule"
    "fixture/DescribeBandwidthRateLimitSchedule.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestUpdateSMBSecurityStrategy :: UpdateSMBSecurityStrategy -> TestTree
requestUpdateSMBSecurityStrategy =
  req
    "UpdateSMBSecurityStrategy"
    "fixture/UpdateSMBSecurityStrategy.yaml"

requestCancelRetrieval :: CancelRetrieval -> TestTree
requestCancelRetrieval =
  req
    "CancelRetrieval"
    "fixture/CancelRetrieval.yaml"

requestDescribeVTLDevices :: DescribeVTLDevices -> TestTree
requestDescribeVTLDevices =
  req
    "DescribeVTLDevices"
    "fixture/DescribeVTLDevices.yaml"

requestStartAvailabilityMonitorTest :: StartAvailabilityMonitorTest -> TestTree
requestStartAvailabilityMonitorTest =
  req
    "StartAvailabilityMonitorTest"
    "fixture/StartAvailabilityMonitorTest.yaml"

requestDeleteTapeArchive :: DeleteTapeArchive -> TestTree
requestDeleteTapeArchive =
  req
    "DeleteTapeArchive"
    "fixture/DeleteTapeArchive.yaml"

requestListFileSystemAssociations :: ListFileSystemAssociations -> TestTree
requestListFileSystemAssociations =
  req
    "ListFileSystemAssociations"
    "fixture/ListFileSystemAssociations.yaml"

requestUpdateSMBFileShare :: UpdateSMBFileShare -> TestTree
requestUpdateSMBFileShare =
  req
    "UpdateSMBFileShare"
    "fixture/UpdateSMBFileShare.yaml"

requestDescribeNFSFileShares :: DescribeNFSFileShares -> TestTree
requestDescribeNFSFileShares =
  req
    "DescribeNFSFileShares"
    "fixture/DescribeNFSFileShares.yaml"

requestListVolumeRecoveryPoints :: ListVolumeRecoveryPoints -> TestTree
requestListVolumeRecoveryPoints =
  req
    "ListVolumeRecoveryPoints"
    "fixture/ListVolumeRecoveryPoints.yaml"

requestListTapes :: ListTapes -> TestTree
requestListTapes =
  req
    "ListTapes"
    "fixture/ListTapes.yaml"

requestResetCache :: ResetCache -> TestTree
requestResetCache =
  req
    "ResetCache"
    "fixture/ResetCache.yaml"

requestDescribeSMBFileShares :: DescribeSMBFileShares -> TestTree
requestDescribeSMBFileShares =
  req
    "DescribeSMBFileShares"
    "fixture/DescribeSMBFileShares.yaml"

requestListGateways :: ListGateways -> TestTree
requestListGateways =
  req
    "ListGateways"
    "fixture/ListGateways.yaml"

requestDeleteTape :: DeleteTape -> TestTree
requestDeleteTape =
  req
    "DeleteTape"
    "fixture/DeleteTape.yaml"

requestListLocalDisks :: ListLocalDisks -> TestTree
requestListLocalDisks =
  req
    "ListLocalDisks"
    "fixture/ListLocalDisks.yaml"

requestListVolumes :: ListVolumes -> TestTree
requestListVolumes =
  req
    "ListVolumes"
    "fixture/ListVolumes.yaml"

requestUpdateBandwidthRateLimit :: UpdateBandwidthRateLimit -> TestTree
requestUpdateBandwidthRateLimit =
  req
    "UpdateBandwidthRateLimit"
    "fixture/UpdateBandwidthRateLimit.yaml"

requestAddWorkingStorage :: AddWorkingStorage -> TestTree
requestAddWorkingStorage =
  req
    "AddWorkingStorage"
    "fixture/AddWorkingStorage.yaml"

requestDescribeTapeRecoveryPoints :: DescribeTapeRecoveryPoints -> TestTree
requestDescribeTapeRecoveryPoints =
  req
    "DescribeTapeRecoveryPoints"
    "fixture/DescribeTapeRecoveryPoints.yaml"

requestDeleteBandwidthRateLimit :: DeleteBandwidthRateLimit -> TestTree
requestDeleteBandwidthRateLimit =
  req
    "DeleteBandwidthRateLimit"
    "fixture/DeleteBandwidthRateLimit.yaml"

requestActivateGateway :: ActivateGateway -> TestTree
requestActivateGateway =
  req
    "ActivateGateway"
    "fixture/ActivateGateway.yaml"

requestDescribeCache :: DescribeCache -> TestTree
requestDescribeCache =
  req
    "DescribeCache"
    "fixture/DescribeCache.yaml"

requestDeleteVolume :: DeleteVolume -> TestTree
requestDeleteVolume =
  req
    "DeleteVolume"
    "fixture/DeleteVolume.yaml"

-- Responses

responseCancelArchival :: CancelArchivalResponse -> TestTree
responseCancelArchival =
  res
    "CancelArchivalResponse"
    "fixture/CancelArchivalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelArchival)

responseCreateStorediSCSIVolume :: CreateStorediSCSIVolumeResponse -> TestTree
responseCreateStorediSCSIVolume =
  res
    "CreateStorediSCSIVolumeResponse"
    "fixture/CreateStorediSCSIVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStorediSCSIVolume)

responseDescribeFileSystemAssociations :: DescribeFileSystemAssociationsResponse -> TestTree
responseDescribeFileSystemAssociations =
  res
    "DescribeFileSystemAssociationsResponse"
    "fixture/DescribeFileSystemAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFileSystemAssociations)

responseUpdateSMBLocalGroups :: UpdateSMBLocalGroupsResponse -> TestTree
responseUpdateSMBLocalGroups =
  res
    "UpdateSMBLocalGroupsResponse"
    "fixture/UpdateSMBLocalGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSMBLocalGroups)

responseCreateNFSFileShare :: CreateNFSFileShareResponse -> TestTree
responseCreateNFSFileShare =
  res
    "CreateNFSFileShareResponse"
    "fixture/CreateNFSFileShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNFSFileShare)

responseAssociateFileSystem :: AssociateFileSystemResponse -> TestTree
responseAssociateFileSystem =
  res
    "AssociateFileSystemResponse"
    "fixture/AssociateFileSystemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateFileSystem)

responseDetachVolume :: DetachVolumeResponse -> TestTree
responseDetachVolume =
  res
    "DetachVolumeResponse"
    "fixture/DetachVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachVolume)

responseDescribeChapCredentials :: DescribeChapCredentialsResponse -> TestTree
responseDescribeChapCredentials =
  res
    "DescribeChapCredentialsResponse"
    "fixture/DescribeChapCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChapCredentials)

responseSetLocalConsolePassword :: SetLocalConsolePasswordResponse -> TestTree
responseSetLocalConsolePassword =
  res
    "SetLocalConsolePasswordResponse"
    "fixture/SetLocalConsolePasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetLocalConsolePassword)

responseCreateTapes :: CreateTapesResponse -> TestTree
responseCreateTapes =
  res
    "CreateTapesResponse"
    "fixture/CreateTapesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTapes)

responseUpdateVTLDeviceType :: UpdateVTLDeviceTypeResponse -> TestTree
responseUpdateVTLDeviceType =
  res
    "UpdateVTLDeviceTypeResponse"
    "fixture/UpdateVTLDeviceTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVTLDeviceType)

responseCreateCachediSCSIVolume :: CreateCachediSCSIVolumeResponse -> TestTree
responseCreateCachediSCSIVolume =
  res
    "CreateCachediSCSIVolumeResponse"
    "fixture/CreateCachediSCSIVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCachediSCSIVolume)

responseListFileShares :: ListFileSharesResponse -> TestTree
responseListFileShares =
  res
    "ListFileSharesResponse"
    "fixture/ListFileSharesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFileShares)

responseJoinDomain :: JoinDomainResponse -> TestTree
responseJoinDomain =
  res
    "JoinDomainResponse"
    "fixture/JoinDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy JoinDomain)

responseDeleteFileShare :: DeleteFileShareResponse -> TestTree
responseDeleteFileShare =
  res
    "DeleteFileShareResponse"
    "fixture/DeleteFileShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFileShare)

responseListVolumeInitiators :: ListVolumeInitiatorsResponse -> TestTree
responseListVolumeInitiators =
  res
    "ListVolumeInitiatorsResponse"
    "fixture/ListVolumeInitiatorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVolumeInitiators)

responseAddUploadBuffer :: AddUploadBufferResponse -> TestTree
responseAddUploadBuffer =
  res
    "AddUploadBufferResponse"
    "fixture/AddUploadBufferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddUploadBuffer)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseNotifyWhenUploaded :: NotifyWhenUploadedResponse -> TestTree
responseNotifyWhenUploaded =
  res
    "NotifyWhenUploadedResponse"
    "fixture/NotifyWhenUploadedResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy NotifyWhenUploaded)

responseListTapePools :: ListTapePoolsResponse -> TestTree
responseListTapePools =
  res
    "ListTapePoolsResponse"
    "fixture/ListTapePoolsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTapePools)

responseDeleteTapePool :: DeleteTapePoolResponse -> TestTree
responseDeleteTapePool =
  res
    "DeleteTapePoolResponse"
    "fixture/DeleteTapePoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTapePool)

responseUpdateGatewayInformation :: UpdateGatewayInformationResponse -> TestTree
responseUpdateGatewayInformation =
  res
    "UpdateGatewayInformationResponse"
    "fixture/UpdateGatewayInformationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGatewayInformation)

responseDescribeMaintenanceStartTime :: DescribeMaintenanceStartTimeResponse -> TestTree
responseDescribeMaintenanceStartTime =
  res
    "DescribeMaintenanceStartTimeResponse"
    "fixture/DescribeMaintenanceStartTimeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMaintenanceStartTime)

responseAssignTapePool :: AssignTapePoolResponse -> TestTree
responseAssignTapePool =
  res
    "AssignTapePoolResponse"
    "fixture/AssignTapePoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssignTapePool)

responseDescribeWorkingStorage :: DescribeWorkingStorageResponse -> TestTree
responseDescribeWorkingStorage =
  res
    "DescribeWorkingStorageResponse"
    "fixture/DescribeWorkingStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkingStorage)

responseDescribeCachediSCSIVolumes :: DescribeCachediSCSIVolumesResponse -> TestTree
responseDescribeCachediSCSIVolumes =
  res
    "DescribeCachediSCSIVolumesResponse"
    "fixture/DescribeCachediSCSIVolumesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCachediSCSIVolumes)

responseAddCache :: AddCacheResponse -> TestTree
responseAddCache =
  res
    "AddCacheResponse"
    "fixture/AddCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddCache)

responseCreateTapePool :: CreateTapePoolResponse -> TestTree
responseCreateTapePool =
  res
    "CreateTapePoolResponse"
    "fixture/CreateTapePoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTapePool)

responseStartGateway :: StartGatewayResponse -> TestTree
responseStartGateway =
  res
    "StartGatewayResponse"
    "fixture/StartGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartGateway)

responseShutdownGateway :: ShutdownGatewayResponse -> TestTree
responseShutdownGateway =
  res
    "ShutdownGatewayResponse"
    "fixture/ShutdownGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ShutdownGateway)

responseListAutomaticTapeCreationPolicies :: ListAutomaticTapeCreationPoliciesResponse -> TestTree
responseListAutomaticTapeCreationPolicies =
  res
    "ListAutomaticTapeCreationPoliciesResponse"
    "fixture/ListAutomaticTapeCreationPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAutomaticTapeCreationPolicies)

responseUpdateGatewaySoftwareNow :: UpdateGatewaySoftwareNowResponse -> TestTree
responseUpdateGatewaySoftwareNow =
  res
    "UpdateGatewaySoftwareNowResponse"
    "fixture/UpdateGatewaySoftwareNowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGatewaySoftwareNow)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromResource)

responseUpdateFileSystemAssociation :: UpdateFileSystemAssociationResponse -> TestTree
responseUpdateFileSystemAssociation =
  res
    "UpdateFileSystemAssociationResponse"
    "fixture/UpdateFileSystemAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFileSystemAssociation)

responseCreateSMBFileShare :: CreateSMBFileShareResponse -> TestTree
responseCreateSMBFileShare =
  res
    "CreateSMBFileShareResponse"
    "fixture/CreateSMBFileShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSMBFileShare)

responseDeleteChapCredentials :: DeleteChapCredentialsResponse -> TestTree
responseDeleteChapCredentials =
  res
    "DeleteChapCredentialsResponse"
    "fixture/DeleteChapCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChapCredentials)

responseUpdateChapCredentials :: UpdateChapCredentialsResponse -> TestTree
responseUpdateChapCredentials =
  res
    "UpdateChapCredentialsResponse"
    "fixture/UpdateChapCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChapCredentials)

responseAttachVolume :: AttachVolumeResponse -> TestTree
responseAttachVolume =
  res
    "AttachVolumeResponse"
    "fixture/AttachVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachVolume)

responseDescribeAvailabilityMonitorTest :: DescribeAvailabilityMonitorTestResponse -> TestTree
responseDescribeAvailabilityMonitorTest =
  res
    "DescribeAvailabilityMonitorTestResponse"
    "fixture/DescribeAvailabilityMonitorTestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAvailabilityMonitorTest)

responseDescribeUploadBuffer :: DescribeUploadBufferResponse -> TestTree
responseDescribeUploadBuffer =
  res
    "DescribeUploadBufferResponse"
    "fixture/DescribeUploadBufferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUploadBuffer)

responseDescribeTapes :: DescribeTapesResponse -> TestTree
responseDescribeTapes =
  res
    "DescribeTapesResponse"
    "fixture/DescribeTapesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTapes)

responseDescribeStorediSCSIVolumes :: DescribeStorediSCSIVolumesResponse -> TestTree
responseDescribeStorediSCSIVolumes =
  res
    "DescribeStorediSCSIVolumesResponse"
    "fixture/DescribeStorediSCSIVolumesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStorediSCSIVolumes)

responseDisassociateFileSystem :: DisassociateFileSystemResponse -> TestTree
responseDisassociateFileSystem =
  res
    "DisassociateFileSystemResponse"
    "fixture/DisassociateFileSystemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateFileSystem)

responseSetSMBGuestPassword :: SetSMBGuestPasswordResponse -> TestTree
responseSetSMBGuestPassword =
  res
    "SetSMBGuestPasswordResponse"
    "fixture/SetSMBGuestPasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetSMBGuestPassword)

responseCreateSnapshotFromVolumeRecoveryPoint :: CreateSnapshotFromVolumeRecoveryPointResponse -> TestTree
responseCreateSnapshotFromVolumeRecoveryPoint =
  res
    "CreateSnapshotFromVolumeRecoveryPointResponse"
    "fixture/CreateSnapshotFromVolumeRecoveryPointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshotFromVolumeRecoveryPoint)

responseRetrieveTapeRecoveryPoint :: RetrieveTapeRecoveryPointResponse -> TestTree
responseRetrieveTapeRecoveryPoint =
  res
    "RetrieveTapeRecoveryPointResponse"
    "fixture/RetrieveTapeRecoveryPointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetrieveTapeRecoveryPoint)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToResource)

responseDeleteGateway :: DeleteGatewayResponse -> TestTree
responseDeleteGateway =
  res
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGateway)

responseUpdateMaintenanceStartTime :: UpdateMaintenanceStartTimeResponse -> TestTree
responseUpdateMaintenanceStartTime =
  res
    "UpdateMaintenanceStartTimeResponse"
    "fixture/UpdateMaintenanceStartTimeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMaintenanceStartTime)

responseDescribeGatewayInformation :: DescribeGatewayInformationResponse -> TestTree
responseDescribeGatewayInformation =
  res
    "DescribeGatewayInformationResponse"
    "fixture/DescribeGatewayInformationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGatewayInformation)

responseRefreshCache :: RefreshCacheResponse -> TestTree
responseRefreshCache =
  res
    "RefreshCacheResponse"
    "fixture/RefreshCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RefreshCache)

responseUpdateNFSFileShare :: UpdateNFSFileShareResponse -> TestTree
responseUpdateNFSFileShare =
  res
    "UpdateNFSFileShareResponse"
    "fixture/UpdateNFSFileShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNFSFileShare)

responseRetrieveTapeArchive :: RetrieveTapeArchiveResponse -> TestTree
responseRetrieveTapeArchive =
  res
    "RetrieveTapeArchiveResponse"
    "fixture/RetrieveTapeArchiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetrieveTapeArchive)

responseDescribeTapeArchives :: DescribeTapeArchivesResponse -> TestTree
responseDescribeTapeArchives =
  res
    "DescribeTapeArchivesResponse"
    "fixture/DescribeTapeArchivesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTapeArchives)

responseUpdateBandwidthRateLimitSchedule :: UpdateBandwidthRateLimitScheduleResponse -> TestTree
responseUpdateBandwidthRateLimitSchedule =
  res
    "UpdateBandwidthRateLimitScheduleResponse"
    "fixture/UpdateBandwidthRateLimitScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBandwidthRateLimitSchedule)

responseDisableGateway :: DisableGatewayResponse -> TestTree
responseDisableGateway =
  res
    "DisableGatewayResponse"
    "fixture/DisableGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableGateway)

responseDescribeSMBSettings :: DescribeSMBSettingsResponse -> TestTree
responseDescribeSMBSettings =
  res
    "DescribeSMBSettingsResponse"
    "fixture/DescribeSMBSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSMBSettings)

responseDescribeSnapshotSchedule :: DescribeSnapshotScheduleResponse -> TestTree
responseDescribeSnapshotSchedule =
  res
    "DescribeSnapshotScheduleResponse"
    "fixture/DescribeSnapshotScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSnapshotSchedule)

responseCreateTapeWithBarcode :: CreateTapeWithBarcodeResponse -> TestTree
responseCreateTapeWithBarcode =
  res
    "CreateTapeWithBarcodeResponse"
    "fixture/CreateTapeWithBarcodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTapeWithBarcode)

responseDescribeBandwidthRateLimit :: DescribeBandwidthRateLimitResponse -> TestTree
responseDescribeBandwidthRateLimit =
  res
    "DescribeBandwidthRateLimitResponse"
    "fixture/DescribeBandwidthRateLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBandwidthRateLimit)

responseDeleteAutomaticTapeCreationPolicy :: DeleteAutomaticTapeCreationPolicyResponse -> TestTree
responseDeleteAutomaticTapeCreationPolicy =
  res
    "DeleteAutomaticTapeCreationPolicyResponse"
    "fixture/DeleteAutomaticTapeCreationPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAutomaticTapeCreationPolicy)

responseUpdateAutomaticTapeCreationPolicy :: UpdateAutomaticTapeCreationPolicyResponse -> TestTree
responseUpdateAutomaticTapeCreationPolicy =
  res
    "UpdateAutomaticTapeCreationPolicyResponse"
    "fixture/UpdateAutomaticTapeCreationPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAutomaticTapeCreationPolicy)

responseUpdateSMBFileShareVisibility :: UpdateSMBFileShareVisibilityResponse -> TestTree
responseUpdateSMBFileShareVisibility =
  res
    "UpdateSMBFileShareVisibilityResponse"
    "fixture/UpdateSMBFileShareVisibilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSMBFileShareVisibility)

responseDeleteSnapshotSchedule :: DeleteSnapshotScheduleResponse -> TestTree
responseDeleteSnapshotSchedule =
  res
    "DeleteSnapshotScheduleResponse"
    "fixture/DeleteSnapshotScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSnapshotSchedule)

responseUpdateSnapshotSchedule :: UpdateSnapshotScheduleResponse -> TestTree
responseUpdateSnapshotSchedule =
  res
    "UpdateSnapshotScheduleResponse"
    "fixture/UpdateSnapshotScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSnapshotSchedule)

responseDescribeBandwidthRateLimitSchedule :: DescribeBandwidthRateLimitScheduleResponse -> TestTree
responseDescribeBandwidthRateLimitSchedule =
  res
    "DescribeBandwidthRateLimitScheduleResponse"
    "fixture/DescribeBandwidthRateLimitScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBandwidthRateLimitSchedule)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshot)

responseUpdateSMBSecurityStrategy :: UpdateSMBSecurityStrategyResponse -> TestTree
responseUpdateSMBSecurityStrategy =
  res
    "UpdateSMBSecurityStrategyResponse"
    "fixture/UpdateSMBSecurityStrategyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSMBSecurityStrategy)

responseCancelRetrieval :: CancelRetrievalResponse -> TestTree
responseCancelRetrieval =
  res
    "CancelRetrievalResponse"
    "fixture/CancelRetrievalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelRetrieval)

responseDescribeVTLDevices :: DescribeVTLDevicesResponse -> TestTree
responseDescribeVTLDevices =
  res
    "DescribeVTLDevicesResponse"
    "fixture/DescribeVTLDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVTLDevices)

responseStartAvailabilityMonitorTest :: StartAvailabilityMonitorTestResponse -> TestTree
responseStartAvailabilityMonitorTest =
  res
    "StartAvailabilityMonitorTestResponse"
    "fixture/StartAvailabilityMonitorTestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAvailabilityMonitorTest)

responseDeleteTapeArchive :: DeleteTapeArchiveResponse -> TestTree
responseDeleteTapeArchive =
  res
    "DeleteTapeArchiveResponse"
    "fixture/DeleteTapeArchiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTapeArchive)

responseListFileSystemAssociations :: ListFileSystemAssociationsResponse -> TestTree
responseListFileSystemAssociations =
  res
    "ListFileSystemAssociationsResponse"
    "fixture/ListFileSystemAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFileSystemAssociations)

responseUpdateSMBFileShare :: UpdateSMBFileShareResponse -> TestTree
responseUpdateSMBFileShare =
  res
    "UpdateSMBFileShareResponse"
    "fixture/UpdateSMBFileShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSMBFileShare)

responseDescribeNFSFileShares :: DescribeNFSFileSharesResponse -> TestTree
responseDescribeNFSFileShares =
  res
    "DescribeNFSFileSharesResponse"
    "fixture/DescribeNFSFileSharesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNFSFileShares)

responseListVolumeRecoveryPoints :: ListVolumeRecoveryPointsResponse -> TestTree
responseListVolumeRecoveryPoints =
  res
    "ListVolumeRecoveryPointsResponse"
    "fixture/ListVolumeRecoveryPointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVolumeRecoveryPoints)

responseListTapes :: ListTapesResponse -> TestTree
responseListTapes =
  res
    "ListTapesResponse"
    "fixture/ListTapesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTapes)

responseResetCache :: ResetCacheResponse -> TestTree
responseResetCache =
  res
    "ResetCacheResponse"
    "fixture/ResetCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetCache)

responseDescribeSMBFileShares :: DescribeSMBFileSharesResponse -> TestTree
responseDescribeSMBFileShares =
  res
    "DescribeSMBFileSharesResponse"
    "fixture/DescribeSMBFileSharesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSMBFileShares)

responseListGateways :: ListGatewaysResponse -> TestTree
responseListGateways =
  res
    "ListGatewaysResponse"
    "fixture/ListGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGateways)

responseDeleteTape :: DeleteTapeResponse -> TestTree
responseDeleteTape =
  res
    "DeleteTapeResponse"
    "fixture/DeleteTapeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTape)

responseListLocalDisks :: ListLocalDisksResponse -> TestTree
responseListLocalDisks =
  res
    "ListLocalDisksResponse"
    "fixture/ListLocalDisksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLocalDisks)

responseListVolumes :: ListVolumesResponse -> TestTree
responseListVolumes =
  res
    "ListVolumesResponse"
    "fixture/ListVolumesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVolumes)

responseUpdateBandwidthRateLimit :: UpdateBandwidthRateLimitResponse -> TestTree
responseUpdateBandwidthRateLimit =
  res
    "UpdateBandwidthRateLimitResponse"
    "fixture/UpdateBandwidthRateLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBandwidthRateLimit)

responseAddWorkingStorage :: AddWorkingStorageResponse -> TestTree
responseAddWorkingStorage =
  res
    "AddWorkingStorageResponse"
    "fixture/AddWorkingStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddWorkingStorage)

responseDescribeTapeRecoveryPoints :: DescribeTapeRecoveryPointsResponse -> TestTree
responseDescribeTapeRecoveryPoints =
  res
    "DescribeTapeRecoveryPointsResponse"
    "fixture/DescribeTapeRecoveryPointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTapeRecoveryPoints)

responseDeleteBandwidthRateLimit :: DeleteBandwidthRateLimitResponse -> TestTree
responseDeleteBandwidthRateLimit =
  res
    "DeleteBandwidthRateLimitResponse"
    "fixture/DeleteBandwidthRateLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBandwidthRateLimit)

responseActivateGateway :: ActivateGatewayResponse -> TestTree
responseActivateGateway =
  res
    "ActivateGatewayResponse"
    "fixture/ActivateGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ActivateGateway)

responseDescribeCache :: DescribeCacheResponse -> TestTree
responseDescribeCache =
  res
    "DescribeCacheResponse"
    "fixture/DescribeCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCache)

responseDeleteVolume :: DeleteVolumeResponse -> TestTree
responseDeleteVolume =
  res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVolume)
