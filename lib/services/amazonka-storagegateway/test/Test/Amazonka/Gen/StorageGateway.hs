{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.StorageGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.StorageGateway where

import Amazonka.StorageGateway
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.StorageGateway.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestActivateGateway $
--             newActivateGateway
--
--         , requestAddCache $
--             newAddCache
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestAddUploadBuffer $
--             newAddUploadBuffer
--
--         , requestAddWorkingStorage $
--             newAddWorkingStorage
--
--         , requestAssignTapePool $
--             newAssignTapePool
--
--         , requestAssociateFileSystem $
--             newAssociateFileSystem
--
--         , requestAttachVolume $
--             newAttachVolume
--
--         , requestCancelArchival $
--             newCancelArchival
--
--         , requestCancelRetrieval $
--             newCancelRetrieval
--
--         , requestCreateCachediSCSIVolume $
--             newCreateCachediSCSIVolume
--
--         , requestCreateNFSFileShare $
--             newCreateNFSFileShare
--
--         , requestCreateSMBFileShare $
--             newCreateSMBFileShare
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestCreateSnapshotFromVolumeRecoveryPoint $
--             newCreateSnapshotFromVolumeRecoveryPoint
--
--         , requestCreateStorediSCSIVolume $
--             newCreateStorediSCSIVolume
--
--         , requestCreateTapePool $
--             newCreateTapePool
--
--         , requestCreateTapeWithBarcode $
--             newCreateTapeWithBarcode
--
--         , requestCreateTapes $
--             newCreateTapes
--
--         , requestDeleteAutomaticTapeCreationPolicy $
--             newDeleteAutomaticTapeCreationPolicy
--
--         , requestDeleteBandwidthRateLimit $
--             newDeleteBandwidthRateLimit
--
--         , requestDeleteChapCredentials $
--             newDeleteChapCredentials
--
--         , requestDeleteFileShare $
--             newDeleteFileShare
--
--         , requestDeleteGateway $
--             newDeleteGateway
--
--         , requestDeleteSnapshotSchedule $
--             newDeleteSnapshotSchedule
--
--         , requestDeleteTape $
--             newDeleteTape
--
--         , requestDeleteTapeArchive $
--             newDeleteTapeArchive
--
--         , requestDeleteTapePool $
--             newDeleteTapePool
--
--         , requestDeleteVolume $
--             newDeleteVolume
--
--         , requestDescribeAvailabilityMonitorTest $
--             newDescribeAvailabilityMonitorTest
--
--         , requestDescribeBandwidthRateLimit $
--             newDescribeBandwidthRateLimit
--
--         , requestDescribeBandwidthRateLimitSchedule $
--             newDescribeBandwidthRateLimitSchedule
--
--         , requestDescribeCache $
--             newDescribeCache
--
--         , requestDescribeCachediSCSIVolumes $
--             newDescribeCachediSCSIVolumes
--
--         , requestDescribeChapCredentials $
--             newDescribeChapCredentials
--
--         , requestDescribeFileSystemAssociations $
--             newDescribeFileSystemAssociations
--
--         , requestDescribeGatewayInformation $
--             newDescribeGatewayInformation
--
--         , requestDescribeMaintenanceStartTime $
--             newDescribeMaintenanceStartTime
--
--         , requestDescribeNFSFileShares $
--             newDescribeNFSFileShares
--
--         , requestDescribeSMBFileShares $
--             newDescribeSMBFileShares
--
--         , requestDescribeSMBSettings $
--             newDescribeSMBSettings
--
--         , requestDescribeSnapshotSchedule $
--             newDescribeSnapshotSchedule
--
--         , requestDescribeStorediSCSIVolumes $
--             newDescribeStorediSCSIVolumes
--
--         , requestDescribeTapeArchives $
--             newDescribeTapeArchives
--
--         , requestDescribeTapeRecoveryPoints $
--             newDescribeTapeRecoveryPoints
--
--         , requestDescribeTapes $
--             newDescribeTapes
--
--         , requestDescribeUploadBuffer $
--             newDescribeUploadBuffer
--
--         , requestDescribeVTLDevices $
--             newDescribeVTLDevices
--
--         , requestDescribeWorkingStorage $
--             newDescribeWorkingStorage
--
--         , requestDetachVolume $
--             newDetachVolume
--
--         , requestDisableGateway $
--             newDisableGateway
--
--         , requestDisassociateFileSystem $
--             newDisassociateFileSystem
--
--         , requestJoinDomain $
--             newJoinDomain
--
--         , requestListAutomaticTapeCreationPolicies $
--             newListAutomaticTapeCreationPolicies
--
--         , requestListFileShares $
--             newListFileShares
--
--         , requestListFileSystemAssociations $
--             newListFileSystemAssociations
--
--         , requestListGateways $
--             newListGateways
--
--         , requestListLocalDisks $
--             newListLocalDisks
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTapePools $
--             newListTapePools
--
--         , requestListTapes $
--             newListTapes
--
--         , requestListVolumeInitiators $
--             newListVolumeInitiators
--
--         , requestListVolumeRecoveryPoints $
--             newListVolumeRecoveryPoints
--
--         , requestListVolumes $
--             newListVolumes
--
--         , requestNotifyWhenUploaded $
--             newNotifyWhenUploaded
--
--         , requestRefreshCache $
--             newRefreshCache
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestResetCache $
--             newResetCache
--
--         , requestRetrieveTapeArchive $
--             newRetrieveTapeArchive
--
--         , requestRetrieveTapeRecoveryPoint $
--             newRetrieveTapeRecoveryPoint
--
--         , requestSetLocalConsolePassword $
--             newSetLocalConsolePassword
--
--         , requestSetSMBGuestPassword $
--             newSetSMBGuestPassword
--
--         , requestShutdownGateway $
--             newShutdownGateway
--
--         , requestStartAvailabilityMonitorTest $
--             newStartAvailabilityMonitorTest
--
--         , requestStartGateway $
--             newStartGateway
--
--         , requestUpdateAutomaticTapeCreationPolicy $
--             newUpdateAutomaticTapeCreationPolicy
--
--         , requestUpdateBandwidthRateLimit $
--             newUpdateBandwidthRateLimit
--
--         , requestUpdateBandwidthRateLimitSchedule $
--             newUpdateBandwidthRateLimitSchedule
--
--         , requestUpdateChapCredentials $
--             newUpdateChapCredentials
--
--         , requestUpdateFileSystemAssociation $
--             newUpdateFileSystemAssociation
--
--         , requestUpdateGatewayInformation $
--             newUpdateGatewayInformation
--
--         , requestUpdateGatewaySoftwareNow $
--             newUpdateGatewaySoftwareNow
--
--         , requestUpdateMaintenanceStartTime $
--             newUpdateMaintenanceStartTime
--
--         , requestUpdateNFSFileShare $
--             newUpdateNFSFileShare
--
--         , requestUpdateSMBFileShare $
--             newUpdateSMBFileShare
--
--         , requestUpdateSMBFileShareVisibility $
--             newUpdateSMBFileShareVisibility
--
--         , requestUpdateSMBLocalGroups $
--             newUpdateSMBLocalGroups
--
--         , requestUpdateSMBSecurityStrategy $
--             newUpdateSMBSecurityStrategy
--
--         , requestUpdateSnapshotSchedule $
--             newUpdateSnapshotSchedule
--
--         , requestUpdateVTLDeviceType $
--             newUpdateVTLDeviceType
--
--           ]

--     , testGroup "response"
--         [ responseActivateGateway $
--             newActivateGatewayResponse
--
--         , responseAddCache $
--             newAddCacheResponse
--
--         , responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseAddUploadBuffer $
--             newAddUploadBufferResponse
--
--         , responseAddWorkingStorage $
--             newAddWorkingStorageResponse
--
--         , responseAssignTapePool $
--             newAssignTapePoolResponse
--
--         , responseAssociateFileSystem $
--             newAssociateFileSystemResponse
--
--         , responseAttachVolume $
--             newAttachVolumeResponse
--
--         , responseCancelArchival $
--             newCancelArchivalResponse
--
--         , responseCancelRetrieval $
--             newCancelRetrievalResponse
--
--         , responseCreateCachediSCSIVolume $
--             newCreateCachediSCSIVolumeResponse
--
--         , responseCreateNFSFileShare $
--             newCreateNFSFileShareResponse
--
--         , responseCreateSMBFileShare $
--             newCreateSMBFileShareResponse
--
--         , responseCreateSnapshot $
--             newCreateSnapshotResponse
--
--         , responseCreateSnapshotFromVolumeRecoveryPoint $
--             newCreateSnapshotFromVolumeRecoveryPointResponse
--
--         , responseCreateStorediSCSIVolume $
--             newCreateStorediSCSIVolumeResponse
--
--         , responseCreateTapePool $
--             newCreateTapePoolResponse
--
--         , responseCreateTapeWithBarcode $
--             newCreateTapeWithBarcodeResponse
--
--         , responseCreateTapes $
--             newCreateTapesResponse
--
--         , responseDeleteAutomaticTapeCreationPolicy $
--             newDeleteAutomaticTapeCreationPolicyResponse
--
--         , responseDeleteBandwidthRateLimit $
--             newDeleteBandwidthRateLimitResponse
--
--         , responseDeleteChapCredentials $
--             newDeleteChapCredentialsResponse
--
--         , responseDeleteFileShare $
--             newDeleteFileShareResponse
--
--         , responseDeleteGateway $
--             newDeleteGatewayResponse
--
--         , responseDeleteSnapshotSchedule $
--             newDeleteSnapshotScheduleResponse
--
--         , responseDeleteTape $
--             newDeleteTapeResponse
--
--         , responseDeleteTapeArchive $
--             newDeleteTapeArchiveResponse
--
--         , responseDeleteTapePool $
--             newDeleteTapePoolResponse
--
--         , responseDeleteVolume $
--             newDeleteVolumeResponse
--
--         , responseDescribeAvailabilityMonitorTest $
--             newDescribeAvailabilityMonitorTestResponse
--
--         , responseDescribeBandwidthRateLimit $
--             newDescribeBandwidthRateLimitResponse
--
--         , responseDescribeBandwidthRateLimitSchedule $
--             newDescribeBandwidthRateLimitScheduleResponse
--
--         , responseDescribeCache $
--             newDescribeCacheResponse
--
--         , responseDescribeCachediSCSIVolumes $
--             newDescribeCachediSCSIVolumesResponse
--
--         , responseDescribeChapCredentials $
--             newDescribeChapCredentialsResponse
--
--         , responseDescribeFileSystemAssociations $
--             newDescribeFileSystemAssociationsResponse
--
--         , responseDescribeGatewayInformation $
--             newDescribeGatewayInformationResponse
--
--         , responseDescribeMaintenanceStartTime $
--             newDescribeMaintenanceStartTimeResponse
--
--         , responseDescribeNFSFileShares $
--             newDescribeNFSFileSharesResponse
--
--         , responseDescribeSMBFileShares $
--             newDescribeSMBFileSharesResponse
--
--         , responseDescribeSMBSettings $
--             newDescribeSMBSettingsResponse
--
--         , responseDescribeSnapshotSchedule $
--             newDescribeSnapshotScheduleResponse
--
--         , responseDescribeStorediSCSIVolumes $
--             newDescribeStorediSCSIVolumesResponse
--
--         , responseDescribeTapeArchives $
--             newDescribeTapeArchivesResponse
--
--         , responseDescribeTapeRecoveryPoints $
--             newDescribeTapeRecoveryPointsResponse
--
--         , responseDescribeTapes $
--             newDescribeTapesResponse
--
--         , responseDescribeUploadBuffer $
--             newDescribeUploadBufferResponse
--
--         , responseDescribeVTLDevices $
--             newDescribeVTLDevicesResponse
--
--         , responseDescribeWorkingStorage $
--             newDescribeWorkingStorageResponse
--
--         , responseDetachVolume $
--             newDetachVolumeResponse
--
--         , responseDisableGateway $
--             newDisableGatewayResponse
--
--         , responseDisassociateFileSystem $
--             newDisassociateFileSystemResponse
--
--         , responseJoinDomain $
--             newJoinDomainResponse
--
--         , responseListAutomaticTapeCreationPolicies $
--             newListAutomaticTapeCreationPoliciesResponse
--
--         , responseListFileShares $
--             newListFileSharesResponse
--
--         , responseListFileSystemAssociations $
--             newListFileSystemAssociationsResponse
--
--         , responseListGateways $
--             newListGatewaysResponse
--
--         , responseListLocalDisks $
--             newListLocalDisksResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTapePools $
--             newListTapePoolsResponse
--
--         , responseListTapes $
--             newListTapesResponse
--
--         , responseListVolumeInitiators $
--             newListVolumeInitiatorsResponse
--
--         , responseListVolumeRecoveryPoints $
--             newListVolumeRecoveryPointsResponse
--
--         , responseListVolumes $
--             newListVolumesResponse
--
--         , responseNotifyWhenUploaded $
--             newNotifyWhenUploadedResponse
--
--         , responseRefreshCache $
--             newRefreshCacheResponse
--
--         , responseRemoveTagsFromResource $
--             newRemoveTagsFromResourceResponse
--
--         , responseResetCache $
--             newResetCacheResponse
--
--         , responseRetrieveTapeArchive $
--             newRetrieveTapeArchiveResponse
--
--         , responseRetrieveTapeRecoveryPoint $
--             newRetrieveTapeRecoveryPointResponse
--
--         , responseSetLocalConsolePassword $
--             newSetLocalConsolePasswordResponse
--
--         , responseSetSMBGuestPassword $
--             newSetSMBGuestPasswordResponse
--
--         , responseShutdownGateway $
--             newShutdownGatewayResponse
--
--         , responseStartAvailabilityMonitorTest $
--             newStartAvailabilityMonitorTestResponse
--
--         , responseStartGateway $
--             newStartGatewayResponse
--
--         , responseUpdateAutomaticTapeCreationPolicy $
--             newUpdateAutomaticTapeCreationPolicyResponse
--
--         , responseUpdateBandwidthRateLimit $
--             newUpdateBandwidthRateLimitResponse
--
--         , responseUpdateBandwidthRateLimitSchedule $
--             newUpdateBandwidthRateLimitScheduleResponse
--
--         , responseUpdateChapCredentials $
--             newUpdateChapCredentialsResponse
--
--         , responseUpdateFileSystemAssociation $
--             newUpdateFileSystemAssociationResponse
--
--         , responseUpdateGatewayInformation $
--             newUpdateGatewayInformationResponse
--
--         , responseUpdateGatewaySoftwareNow $
--             newUpdateGatewaySoftwareNowResponse
--
--         , responseUpdateMaintenanceStartTime $
--             newUpdateMaintenanceStartTimeResponse
--
--         , responseUpdateNFSFileShare $
--             newUpdateNFSFileShareResponse
--
--         , responseUpdateSMBFileShare $
--             newUpdateSMBFileShareResponse
--
--         , responseUpdateSMBFileShareVisibility $
--             newUpdateSMBFileShareVisibilityResponse
--
--         , responseUpdateSMBLocalGroups $
--             newUpdateSMBLocalGroupsResponse
--
--         , responseUpdateSMBSecurityStrategy $
--             newUpdateSMBSecurityStrategyResponse
--
--         , responseUpdateSnapshotSchedule $
--             newUpdateSnapshotScheduleResponse
--
--         , responseUpdateVTLDeviceType $
--             newUpdateVTLDeviceTypeResponse
--
--           ]
--     ]

-- Requests

requestActivateGateway :: ActivateGateway -> TestTree
requestActivateGateway =
  req
    "ActivateGateway"
    "fixture/ActivateGateway.yaml"

requestAddCache :: AddCache -> TestTree
requestAddCache =
  req
    "AddCache"
    "fixture/AddCache.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestAddUploadBuffer :: AddUploadBuffer -> TestTree
requestAddUploadBuffer =
  req
    "AddUploadBuffer"
    "fixture/AddUploadBuffer.yaml"

requestAddWorkingStorage :: AddWorkingStorage -> TestTree
requestAddWorkingStorage =
  req
    "AddWorkingStorage"
    "fixture/AddWorkingStorage.yaml"

requestAssignTapePool :: AssignTapePool -> TestTree
requestAssignTapePool =
  req
    "AssignTapePool"
    "fixture/AssignTapePool.yaml"

requestAssociateFileSystem :: AssociateFileSystem -> TestTree
requestAssociateFileSystem =
  req
    "AssociateFileSystem"
    "fixture/AssociateFileSystem.yaml"

requestAttachVolume :: AttachVolume -> TestTree
requestAttachVolume =
  req
    "AttachVolume"
    "fixture/AttachVolume.yaml"

requestCancelArchival :: CancelArchival -> TestTree
requestCancelArchival =
  req
    "CancelArchival"
    "fixture/CancelArchival.yaml"

requestCancelRetrieval :: CancelRetrieval -> TestTree
requestCancelRetrieval =
  req
    "CancelRetrieval"
    "fixture/CancelRetrieval.yaml"

requestCreateCachediSCSIVolume :: CreateCachediSCSIVolume -> TestTree
requestCreateCachediSCSIVolume =
  req
    "CreateCachediSCSIVolume"
    "fixture/CreateCachediSCSIVolume.yaml"

requestCreateNFSFileShare :: CreateNFSFileShare -> TestTree
requestCreateNFSFileShare =
  req
    "CreateNFSFileShare"
    "fixture/CreateNFSFileShare.yaml"

requestCreateSMBFileShare :: CreateSMBFileShare -> TestTree
requestCreateSMBFileShare =
  req
    "CreateSMBFileShare"
    "fixture/CreateSMBFileShare.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestCreateSnapshotFromVolumeRecoveryPoint :: CreateSnapshotFromVolumeRecoveryPoint -> TestTree
requestCreateSnapshotFromVolumeRecoveryPoint =
  req
    "CreateSnapshotFromVolumeRecoveryPoint"
    "fixture/CreateSnapshotFromVolumeRecoveryPoint.yaml"

requestCreateStorediSCSIVolume :: CreateStorediSCSIVolume -> TestTree
requestCreateStorediSCSIVolume =
  req
    "CreateStorediSCSIVolume"
    "fixture/CreateStorediSCSIVolume.yaml"

requestCreateTapePool :: CreateTapePool -> TestTree
requestCreateTapePool =
  req
    "CreateTapePool"
    "fixture/CreateTapePool.yaml"

requestCreateTapeWithBarcode :: CreateTapeWithBarcode -> TestTree
requestCreateTapeWithBarcode =
  req
    "CreateTapeWithBarcode"
    "fixture/CreateTapeWithBarcode.yaml"

requestCreateTapes :: CreateTapes -> TestTree
requestCreateTapes =
  req
    "CreateTapes"
    "fixture/CreateTapes.yaml"

requestDeleteAutomaticTapeCreationPolicy :: DeleteAutomaticTapeCreationPolicy -> TestTree
requestDeleteAutomaticTapeCreationPolicy =
  req
    "DeleteAutomaticTapeCreationPolicy"
    "fixture/DeleteAutomaticTapeCreationPolicy.yaml"

requestDeleteBandwidthRateLimit :: DeleteBandwidthRateLimit -> TestTree
requestDeleteBandwidthRateLimit =
  req
    "DeleteBandwidthRateLimit"
    "fixture/DeleteBandwidthRateLimit.yaml"

requestDeleteChapCredentials :: DeleteChapCredentials -> TestTree
requestDeleteChapCredentials =
  req
    "DeleteChapCredentials"
    "fixture/DeleteChapCredentials.yaml"

requestDeleteFileShare :: DeleteFileShare -> TestTree
requestDeleteFileShare =
  req
    "DeleteFileShare"
    "fixture/DeleteFileShare.yaml"

requestDeleteGateway :: DeleteGateway -> TestTree
requestDeleteGateway =
  req
    "DeleteGateway"
    "fixture/DeleteGateway.yaml"

requestDeleteSnapshotSchedule :: DeleteSnapshotSchedule -> TestTree
requestDeleteSnapshotSchedule =
  req
    "DeleteSnapshotSchedule"
    "fixture/DeleteSnapshotSchedule.yaml"

requestDeleteTape :: DeleteTape -> TestTree
requestDeleteTape =
  req
    "DeleteTape"
    "fixture/DeleteTape.yaml"

requestDeleteTapeArchive :: DeleteTapeArchive -> TestTree
requestDeleteTapeArchive =
  req
    "DeleteTapeArchive"
    "fixture/DeleteTapeArchive.yaml"

requestDeleteTapePool :: DeleteTapePool -> TestTree
requestDeleteTapePool =
  req
    "DeleteTapePool"
    "fixture/DeleteTapePool.yaml"

requestDeleteVolume :: DeleteVolume -> TestTree
requestDeleteVolume =
  req
    "DeleteVolume"
    "fixture/DeleteVolume.yaml"

requestDescribeAvailabilityMonitorTest :: DescribeAvailabilityMonitorTest -> TestTree
requestDescribeAvailabilityMonitorTest =
  req
    "DescribeAvailabilityMonitorTest"
    "fixture/DescribeAvailabilityMonitorTest.yaml"

requestDescribeBandwidthRateLimit :: DescribeBandwidthRateLimit -> TestTree
requestDescribeBandwidthRateLimit =
  req
    "DescribeBandwidthRateLimit"
    "fixture/DescribeBandwidthRateLimit.yaml"

requestDescribeBandwidthRateLimitSchedule :: DescribeBandwidthRateLimitSchedule -> TestTree
requestDescribeBandwidthRateLimitSchedule =
  req
    "DescribeBandwidthRateLimitSchedule"
    "fixture/DescribeBandwidthRateLimitSchedule.yaml"

requestDescribeCache :: DescribeCache -> TestTree
requestDescribeCache =
  req
    "DescribeCache"
    "fixture/DescribeCache.yaml"

requestDescribeCachediSCSIVolumes :: DescribeCachediSCSIVolumes -> TestTree
requestDescribeCachediSCSIVolumes =
  req
    "DescribeCachediSCSIVolumes"
    "fixture/DescribeCachediSCSIVolumes.yaml"

requestDescribeChapCredentials :: DescribeChapCredentials -> TestTree
requestDescribeChapCredentials =
  req
    "DescribeChapCredentials"
    "fixture/DescribeChapCredentials.yaml"

requestDescribeFileSystemAssociations :: DescribeFileSystemAssociations -> TestTree
requestDescribeFileSystemAssociations =
  req
    "DescribeFileSystemAssociations"
    "fixture/DescribeFileSystemAssociations.yaml"

requestDescribeGatewayInformation :: DescribeGatewayInformation -> TestTree
requestDescribeGatewayInformation =
  req
    "DescribeGatewayInformation"
    "fixture/DescribeGatewayInformation.yaml"

requestDescribeMaintenanceStartTime :: DescribeMaintenanceStartTime -> TestTree
requestDescribeMaintenanceStartTime =
  req
    "DescribeMaintenanceStartTime"
    "fixture/DescribeMaintenanceStartTime.yaml"

requestDescribeNFSFileShares :: DescribeNFSFileShares -> TestTree
requestDescribeNFSFileShares =
  req
    "DescribeNFSFileShares"
    "fixture/DescribeNFSFileShares.yaml"

requestDescribeSMBFileShares :: DescribeSMBFileShares -> TestTree
requestDescribeSMBFileShares =
  req
    "DescribeSMBFileShares"
    "fixture/DescribeSMBFileShares.yaml"

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

requestDescribeStorediSCSIVolumes :: DescribeStorediSCSIVolumes -> TestTree
requestDescribeStorediSCSIVolumes =
  req
    "DescribeStorediSCSIVolumes"
    "fixture/DescribeStorediSCSIVolumes.yaml"

requestDescribeTapeArchives :: DescribeTapeArchives -> TestTree
requestDescribeTapeArchives =
  req
    "DescribeTapeArchives"
    "fixture/DescribeTapeArchives.yaml"

requestDescribeTapeRecoveryPoints :: DescribeTapeRecoveryPoints -> TestTree
requestDescribeTapeRecoveryPoints =
  req
    "DescribeTapeRecoveryPoints"
    "fixture/DescribeTapeRecoveryPoints.yaml"

requestDescribeTapes :: DescribeTapes -> TestTree
requestDescribeTapes =
  req
    "DescribeTapes"
    "fixture/DescribeTapes.yaml"

requestDescribeUploadBuffer :: DescribeUploadBuffer -> TestTree
requestDescribeUploadBuffer =
  req
    "DescribeUploadBuffer"
    "fixture/DescribeUploadBuffer.yaml"

requestDescribeVTLDevices :: DescribeVTLDevices -> TestTree
requestDescribeVTLDevices =
  req
    "DescribeVTLDevices"
    "fixture/DescribeVTLDevices.yaml"

requestDescribeWorkingStorage :: DescribeWorkingStorage -> TestTree
requestDescribeWorkingStorage =
  req
    "DescribeWorkingStorage"
    "fixture/DescribeWorkingStorage.yaml"

requestDetachVolume :: DetachVolume -> TestTree
requestDetachVolume =
  req
    "DetachVolume"
    "fixture/DetachVolume.yaml"

requestDisableGateway :: DisableGateway -> TestTree
requestDisableGateway =
  req
    "DisableGateway"
    "fixture/DisableGateway.yaml"

requestDisassociateFileSystem :: DisassociateFileSystem -> TestTree
requestDisassociateFileSystem =
  req
    "DisassociateFileSystem"
    "fixture/DisassociateFileSystem.yaml"

requestJoinDomain :: JoinDomain -> TestTree
requestJoinDomain =
  req
    "JoinDomain"
    "fixture/JoinDomain.yaml"

requestListAutomaticTapeCreationPolicies :: ListAutomaticTapeCreationPolicies -> TestTree
requestListAutomaticTapeCreationPolicies =
  req
    "ListAutomaticTapeCreationPolicies"
    "fixture/ListAutomaticTapeCreationPolicies.yaml"

requestListFileShares :: ListFileShares -> TestTree
requestListFileShares =
  req
    "ListFileShares"
    "fixture/ListFileShares.yaml"

requestListFileSystemAssociations :: ListFileSystemAssociations -> TestTree
requestListFileSystemAssociations =
  req
    "ListFileSystemAssociations"
    "fixture/ListFileSystemAssociations.yaml"

requestListGateways :: ListGateways -> TestTree
requestListGateways =
  req
    "ListGateways"
    "fixture/ListGateways.yaml"

requestListLocalDisks :: ListLocalDisks -> TestTree
requestListLocalDisks =
  req
    "ListLocalDisks"
    "fixture/ListLocalDisks.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTapePools :: ListTapePools -> TestTree
requestListTapePools =
  req
    "ListTapePools"
    "fixture/ListTapePools.yaml"

requestListTapes :: ListTapes -> TestTree
requestListTapes =
  req
    "ListTapes"
    "fixture/ListTapes.yaml"

requestListVolumeInitiators :: ListVolumeInitiators -> TestTree
requestListVolumeInitiators =
  req
    "ListVolumeInitiators"
    "fixture/ListVolumeInitiators.yaml"

requestListVolumeRecoveryPoints :: ListVolumeRecoveryPoints -> TestTree
requestListVolumeRecoveryPoints =
  req
    "ListVolumeRecoveryPoints"
    "fixture/ListVolumeRecoveryPoints.yaml"

requestListVolumes :: ListVolumes -> TestTree
requestListVolumes =
  req
    "ListVolumes"
    "fixture/ListVolumes.yaml"

requestNotifyWhenUploaded :: NotifyWhenUploaded -> TestTree
requestNotifyWhenUploaded =
  req
    "NotifyWhenUploaded"
    "fixture/NotifyWhenUploaded.yaml"

requestRefreshCache :: RefreshCache -> TestTree
requestRefreshCache =
  req
    "RefreshCache"
    "fixture/RefreshCache.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestResetCache :: ResetCache -> TestTree
requestResetCache =
  req
    "ResetCache"
    "fixture/ResetCache.yaml"

requestRetrieveTapeArchive :: RetrieveTapeArchive -> TestTree
requestRetrieveTapeArchive =
  req
    "RetrieveTapeArchive"
    "fixture/RetrieveTapeArchive.yaml"

requestRetrieveTapeRecoveryPoint :: RetrieveTapeRecoveryPoint -> TestTree
requestRetrieveTapeRecoveryPoint =
  req
    "RetrieveTapeRecoveryPoint"
    "fixture/RetrieveTapeRecoveryPoint.yaml"

requestSetLocalConsolePassword :: SetLocalConsolePassword -> TestTree
requestSetLocalConsolePassword =
  req
    "SetLocalConsolePassword"
    "fixture/SetLocalConsolePassword.yaml"

requestSetSMBGuestPassword :: SetSMBGuestPassword -> TestTree
requestSetSMBGuestPassword =
  req
    "SetSMBGuestPassword"
    "fixture/SetSMBGuestPassword.yaml"

requestShutdownGateway :: ShutdownGateway -> TestTree
requestShutdownGateway =
  req
    "ShutdownGateway"
    "fixture/ShutdownGateway.yaml"

requestStartAvailabilityMonitorTest :: StartAvailabilityMonitorTest -> TestTree
requestStartAvailabilityMonitorTest =
  req
    "StartAvailabilityMonitorTest"
    "fixture/StartAvailabilityMonitorTest.yaml"

requestStartGateway :: StartGateway -> TestTree
requestStartGateway =
  req
    "StartGateway"
    "fixture/StartGateway.yaml"

requestUpdateAutomaticTapeCreationPolicy :: UpdateAutomaticTapeCreationPolicy -> TestTree
requestUpdateAutomaticTapeCreationPolicy =
  req
    "UpdateAutomaticTapeCreationPolicy"
    "fixture/UpdateAutomaticTapeCreationPolicy.yaml"

requestUpdateBandwidthRateLimit :: UpdateBandwidthRateLimit -> TestTree
requestUpdateBandwidthRateLimit =
  req
    "UpdateBandwidthRateLimit"
    "fixture/UpdateBandwidthRateLimit.yaml"

requestUpdateBandwidthRateLimitSchedule :: UpdateBandwidthRateLimitSchedule -> TestTree
requestUpdateBandwidthRateLimitSchedule =
  req
    "UpdateBandwidthRateLimitSchedule"
    "fixture/UpdateBandwidthRateLimitSchedule.yaml"

requestUpdateChapCredentials :: UpdateChapCredentials -> TestTree
requestUpdateChapCredentials =
  req
    "UpdateChapCredentials"
    "fixture/UpdateChapCredentials.yaml"

requestUpdateFileSystemAssociation :: UpdateFileSystemAssociation -> TestTree
requestUpdateFileSystemAssociation =
  req
    "UpdateFileSystemAssociation"
    "fixture/UpdateFileSystemAssociation.yaml"

requestUpdateGatewayInformation :: UpdateGatewayInformation -> TestTree
requestUpdateGatewayInformation =
  req
    "UpdateGatewayInformation"
    "fixture/UpdateGatewayInformation.yaml"

requestUpdateGatewaySoftwareNow :: UpdateGatewaySoftwareNow -> TestTree
requestUpdateGatewaySoftwareNow =
  req
    "UpdateGatewaySoftwareNow"
    "fixture/UpdateGatewaySoftwareNow.yaml"

requestUpdateMaintenanceStartTime :: UpdateMaintenanceStartTime -> TestTree
requestUpdateMaintenanceStartTime =
  req
    "UpdateMaintenanceStartTime"
    "fixture/UpdateMaintenanceStartTime.yaml"

requestUpdateNFSFileShare :: UpdateNFSFileShare -> TestTree
requestUpdateNFSFileShare =
  req
    "UpdateNFSFileShare"
    "fixture/UpdateNFSFileShare.yaml"

requestUpdateSMBFileShare :: UpdateSMBFileShare -> TestTree
requestUpdateSMBFileShare =
  req
    "UpdateSMBFileShare"
    "fixture/UpdateSMBFileShare.yaml"

requestUpdateSMBFileShareVisibility :: UpdateSMBFileShareVisibility -> TestTree
requestUpdateSMBFileShareVisibility =
  req
    "UpdateSMBFileShareVisibility"
    "fixture/UpdateSMBFileShareVisibility.yaml"

requestUpdateSMBLocalGroups :: UpdateSMBLocalGroups -> TestTree
requestUpdateSMBLocalGroups =
  req
    "UpdateSMBLocalGroups"
    "fixture/UpdateSMBLocalGroups.yaml"

requestUpdateSMBSecurityStrategy :: UpdateSMBSecurityStrategy -> TestTree
requestUpdateSMBSecurityStrategy =
  req
    "UpdateSMBSecurityStrategy"
    "fixture/UpdateSMBSecurityStrategy.yaml"

requestUpdateSnapshotSchedule :: UpdateSnapshotSchedule -> TestTree
requestUpdateSnapshotSchedule =
  req
    "UpdateSnapshotSchedule"
    "fixture/UpdateSnapshotSchedule.yaml"

requestUpdateVTLDeviceType :: UpdateVTLDeviceType -> TestTree
requestUpdateVTLDeviceType =
  req
    "UpdateVTLDeviceType"
    "fixture/UpdateVTLDeviceType.yaml"

-- Responses

responseActivateGateway :: ActivateGatewayResponse -> TestTree
responseActivateGateway =
  res
    "ActivateGatewayResponse"
    "fixture/ActivateGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ActivateGateway)

responseAddCache :: AddCacheResponse -> TestTree
responseAddCache =
  res
    "AddCacheResponse"
    "fixture/AddCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddCache)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToResource)

responseAddUploadBuffer :: AddUploadBufferResponse -> TestTree
responseAddUploadBuffer =
  res
    "AddUploadBufferResponse"
    "fixture/AddUploadBufferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddUploadBuffer)

responseAddWorkingStorage :: AddWorkingStorageResponse -> TestTree
responseAddWorkingStorage =
  res
    "AddWorkingStorageResponse"
    "fixture/AddWorkingStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddWorkingStorage)

responseAssignTapePool :: AssignTapePoolResponse -> TestTree
responseAssignTapePool =
  res
    "AssignTapePoolResponse"
    "fixture/AssignTapePoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssignTapePool)

responseAssociateFileSystem :: AssociateFileSystemResponse -> TestTree
responseAssociateFileSystem =
  res
    "AssociateFileSystemResponse"
    "fixture/AssociateFileSystemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateFileSystem)

responseAttachVolume :: AttachVolumeResponse -> TestTree
responseAttachVolume =
  res
    "AttachVolumeResponse"
    "fixture/AttachVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachVolume)

responseCancelArchival :: CancelArchivalResponse -> TestTree
responseCancelArchival =
  res
    "CancelArchivalResponse"
    "fixture/CancelArchivalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelArchival)

responseCancelRetrieval :: CancelRetrievalResponse -> TestTree
responseCancelRetrieval =
  res
    "CancelRetrievalResponse"
    "fixture/CancelRetrievalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelRetrieval)

responseCreateCachediSCSIVolume :: CreateCachediSCSIVolumeResponse -> TestTree
responseCreateCachediSCSIVolume =
  res
    "CreateCachediSCSIVolumeResponse"
    "fixture/CreateCachediSCSIVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCachediSCSIVolume)

responseCreateNFSFileShare :: CreateNFSFileShareResponse -> TestTree
responseCreateNFSFileShare =
  res
    "CreateNFSFileShareResponse"
    "fixture/CreateNFSFileShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNFSFileShare)

responseCreateSMBFileShare :: CreateSMBFileShareResponse -> TestTree
responseCreateSMBFileShare =
  res
    "CreateSMBFileShareResponse"
    "fixture/CreateSMBFileShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSMBFileShare)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshot)

responseCreateSnapshotFromVolumeRecoveryPoint :: CreateSnapshotFromVolumeRecoveryPointResponse -> TestTree
responseCreateSnapshotFromVolumeRecoveryPoint =
  res
    "CreateSnapshotFromVolumeRecoveryPointResponse"
    "fixture/CreateSnapshotFromVolumeRecoveryPointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshotFromVolumeRecoveryPoint)

responseCreateStorediSCSIVolume :: CreateStorediSCSIVolumeResponse -> TestTree
responseCreateStorediSCSIVolume =
  res
    "CreateStorediSCSIVolumeResponse"
    "fixture/CreateStorediSCSIVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStorediSCSIVolume)

responseCreateTapePool :: CreateTapePoolResponse -> TestTree
responseCreateTapePool =
  res
    "CreateTapePoolResponse"
    "fixture/CreateTapePoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTapePool)

responseCreateTapeWithBarcode :: CreateTapeWithBarcodeResponse -> TestTree
responseCreateTapeWithBarcode =
  res
    "CreateTapeWithBarcodeResponse"
    "fixture/CreateTapeWithBarcodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTapeWithBarcode)

responseCreateTapes :: CreateTapesResponse -> TestTree
responseCreateTapes =
  res
    "CreateTapesResponse"
    "fixture/CreateTapesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTapes)

responseDeleteAutomaticTapeCreationPolicy :: DeleteAutomaticTapeCreationPolicyResponse -> TestTree
responseDeleteAutomaticTapeCreationPolicy =
  res
    "DeleteAutomaticTapeCreationPolicyResponse"
    "fixture/DeleteAutomaticTapeCreationPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAutomaticTapeCreationPolicy)

responseDeleteBandwidthRateLimit :: DeleteBandwidthRateLimitResponse -> TestTree
responseDeleteBandwidthRateLimit =
  res
    "DeleteBandwidthRateLimitResponse"
    "fixture/DeleteBandwidthRateLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBandwidthRateLimit)

responseDeleteChapCredentials :: DeleteChapCredentialsResponse -> TestTree
responseDeleteChapCredentials =
  res
    "DeleteChapCredentialsResponse"
    "fixture/DeleteChapCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChapCredentials)

responseDeleteFileShare :: DeleteFileShareResponse -> TestTree
responseDeleteFileShare =
  res
    "DeleteFileShareResponse"
    "fixture/DeleteFileShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFileShare)

responseDeleteGateway :: DeleteGatewayResponse -> TestTree
responseDeleteGateway =
  res
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGateway)

responseDeleteSnapshotSchedule :: DeleteSnapshotScheduleResponse -> TestTree
responseDeleteSnapshotSchedule =
  res
    "DeleteSnapshotScheduleResponse"
    "fixture/DeleteSnapshotScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSnapshotSchedule)

responseDeleteTape :: DeleteTapeResponse -> TestTree
responseDeleteTape =
  res
    "DeleteTapeResponse"
    "fixture/DeleteTapeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTape)

responseDeleteTapeArchive :: DeleteTapeArchiveResponse -> TestTree
responseDeleteTapeArchive =
  res
    "DeleteTapeArchiveResponse"
    "fixture/DeleteTapeArchiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTapeArchive)

responseDeleteTapePool :: DeleteTapePoolResponse -> TestTree
responseDeleteTapePool =
  res
    "DeleteTapePoolResponse"
    "fixture/DeleteTapePoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTapePool)

responseDeleteVolume :: DeleteVolumeResponse -> TestTree
responseDeleteVolume =
  res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVolume)

responseDescribeAvailabilityMonitorTest :: DescribeAvailabilityMonitorTestResponse -> TestTree
responseDescribeAvailabilityMonitorTest =
  res
    "DescribeAvailabilityMonitorTestResponse"
    "fixture/DescribeAvailabilityMonitorTestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAvailabilityMonitorTest)

responseDescribeBandwidthRateLimit :: DescribeBandwidthRateLimitResponse -> TestTree
responseDescribeBandwidthRateLimit =
  res
    "DescribeBandwidthRateLimitResponse"
    "fixture/DescribeBandwidthRateLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBandwidthRateLimit)

responseDescribeBandwidthRateLimitSchedule :: DescribeBandwidthRateLimitScheduleResponse -> TestTree
responseDescribeBandwidthRateLimitSchedule =
  res
    "DescribeBandwidthRateLimitScheduleResponse"
    "fixture/DescribeBandwidthRateLimitScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBandwidthRateLimitSchedule)

responseDescribeCache :: DescribeCacheResponse -> TestTree
responseDescribeCache =
  res
    "DescribeCacheResponse"
    "fixture/DescribeCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCache)

responseDescribeCachediSCSIVolumes :: DescribeCachediSCSIVolumesResponse -> TestTree
responseDescribeCachediSCSIVolumes =
  res
    "DescribeCachediSCSIVolumesResponse"
    "fixture/DescribeCachediSCSIVolumesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCachediSCSIVolumes)

responseDescribeChapCredentials :: DescribeChapCredentialsResponse -> TestTree
responseDescribeChapCredentials =
  res
    "DescribeChapCredentialsResponse"
    "fixture/DescribeChapCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChapCredentials)

responseDescribeFileSystemAssociations :: DescribeFileSystemAssociationsResponse -> TestTree
responseDescribeFileSystemAssociations =
  res
    "DescribeFileSystemAssociationsResponse"
    "fixture/DescribeFileSystemAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFileSystemAssociations)

responseDescribeGatewayInformation :: DescribeGatewayInformationResponse -> TestTree
responseDescribeGatewayInformation =
  res
    "DescribeGatewayInformationResponse"
    "fixture/DescribeGatewayInformationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGatewayInformation)

responseDescribeMaintenanceStartTime :: DescribeMaintenanceStartTimeResponse -> TestTree
responseDescribeMaintenanceStartTime =
  res
    "DescribeMaintenanceStartTimeResponse"
    "fixture/DescribeMaintenanceStartTimeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMaintenanceStartTime)

responseDescribeNFSFileShares :: DescribeNFSFileSharesResponse -> TestTree
responseDescribeNFSFileShares =
  res
    "DescribeNFSFileSharesResponse"
    "fixture/DescribeNFSFileSharesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNFSFileShares)

responseDescribeSMBFileShares :: DescribeSMBFileSharesResponse -> TestTree
responseDescribeSMBFileShares =
  res
    "DescribeSMBFileSharesResponse"
    "fixture/DescribeSMBFileSharesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSMBFileShares)

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

responseDescribeStorediSCSIVolumes :: DescribeStorediSCSIVolumesResponse -> TestTree
responseDescribeStorediSCSIVolumes =
  res
    "DescribeStorediSCSIVolumesResponse"
    "fixture/DescribeStorediSCSIVolumesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStorediSCSIVolumes)

responseDescribeTapeArchives :: DescribeTapeArchivesResponse -> TestTree
responseDescribeTapeArchives =
  res
    "DescribeTapeArchivesResponse"
    "fixture/DescribeTapeArchivesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTapeArchives)

responseDescribeTapeRecoveryPoints :: DescribeTapeRecoveryPointsResponse -> TestTree
responseDescribeTapeRecoveryPoints =
  res
    "DescribeTapeRecoveryPointsResponse"
    "fixture/DescribeTapeRecoveryPointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTapeRecoveryPoints)

responseDescribeTapes :: DescribeTapesResponse -> TestTree
responseDescribeTapes =
  res
    "DescribeTapesResponse"
    "fixture/DescribeTapesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTapes)

responseDescribeUploadBuffer :: DescribeUploadBufferResponse -> TestTree
responseDescribeUploadBuffer =
  res
    "DescribeUploadBufferResponse"
    "fixture/DescribeUploadBufferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUploadBuffer)

responseDescribeVTLDevices :: DescribeVTLDevicesResponse -> TestTree
responseDescribeVTLDevices =
  res
    "DescribeVTLDevicesResponse"
    "fixture/DescribeVTLDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVTLDevices)

responseDescribeWorkingStorage :: DescribeWorkingStorageResponse -> TestTree
responseDescribeWorkingStorage =
  res
    "DescribeWorkingStorageResponse"
    "fixture/DescribeWorkingStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkingStorage)

responseDetachVolume :: DetachVolumeResponse -> TestTree
responseDetachVolume =
  res
    "DetachVolumeResponse"
    "fixture/DetachVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachVolume)

responseDisableGateway :: DisableGatewayResponse -> TestTree
responseDisableGateway =
  res
    "DisableGatewayResponse"
    "fixture/DisableGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableGateway)

responseDisassociateFileSystem :: DisassociateFileSystemResponse -> TestTree
responseDisassociateFileSystem =
  res
    "DisassociateFileSystemResponse"
    "fixture/DisassociateFileSystemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateFileSystem)

responseJoinDomain :: JoinDomainResponse -> TestTree
responseJoinDomain =
  res
    "JoinDomainResponse"
    "fixture/JoinDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy JoinDomain)

responseListAutomaticTapeCreationPolicies :: ListAutomaticTapeCreationPoliciesResponse -> TestTree
responseListAutomaticTapeCreationPolicies =
  res
    "ListAutomaticTapeCreationPoliciesResponse"
    "fixture/ListAutomaticTapeCreationPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAutomaticTapeCreationPolicies)

responseListFileShares :: ListFileSharesResponse -> TestTree
responseListFileShares =
  res
    "ListFileSharesResponse"
    "fixture/ListFileSharesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFileShares)

responseListFileSystemAssociations :: ListFileSystemAssociationsResponse -> TestTree
responseListFileSystemAssociations =
  res
    "ListFileSystemAssociationsResponse"
    "fixture/ListFileSystemAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFileSystemAssociations)

responseListGateways :: ListGatewaysResponse -> TestTree
responseListGateways =
  res
    "ListGatewaysResponse"
    "fixture/ListGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGateways)

responseListLocalDisks :: ListLocalDisksResponse -> TestTree
responseListLocalDisks =
  res
    "ListLocalDisksResponse"
    "fixture/ListLocalDisksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLocalDisks)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTapePools :: ListTapePoolsResponse -> TestTree
responseListTapePools =
  res
    "ListTapePoolsResponse"
    "fixture/ListTapePoolsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTapePools)

responseListTapes :: ListTapesResponse -> TestTree
responseListTapes =
  res
    "ListTapesResponse"
    "fixture/ListTapesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTapes)

responseListVolumeInitiators :: ListVolumeInitiatorsResponse -> TestTree
responseListVolumeInitiators =
  res
    "ListVolumeInitiatorsResponse"
    "fixture/ListVolumeInitiatorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVolumeInitiators)

responseListVolumeRecoveryPoints :: ListVolumeRecoveryPointsResponse -> TestTree
responseListVolumeRecoveryPoints =
  res
    "ListVolumeRecoveryPointsResponse"
    "fixture/ListVolumeRecoveryPointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVolumeRecoveryPoints)

responseListVolumes :: ListVolumesResponse -> TestTree
responseListVolumes =
  res
    "ListVolumesResponse"
    "fixture/ListVolumesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVolumes)

responseNotifyWhenUploaded :: NotifyWhenUploadedResponse -> TestTree
responseNotifyWhenUploaded =
  res
    "NotifyWhenUploadedResponse"
    "fixture/NotifyWhenUploadedResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy NotifyWhenUploaded)

responseRefreshCache :: RefreshCacheResponse -> TestTree
responseRefreshCache =
  res
    "RefreshCacheResponse"
    "fixture/RefreshCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RefreshCache)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromResource)

responseResetCache :: ResetCacheResponse -> TestTree
responseResetCache =
  res
    "ResetCacheResponse"
    "fixture/ResetCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetCache)

responseRetrieveTapeArchive :: RetrieveTapeArchiveResponse -> TestTree
responseRetrieveTapeArchive =
  res
    "RetrieveTapeArchiveResponse"
    "fixture/RetrieveTapeArchiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetrieveTapeArchive)

responseRetrieveTapeRecoveryPoint :: RetrieveTapeRecoveryPointResponse -> TestTree
responseRetrieveTapeRecoveryPoint =
  res
    "RetrieveTapeRecoveryPointResponse"
    "fixture/RetrieveTapeRecoveryPointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetrieveTapeRecoveryPoint)

responseSetLocalConsolePassword :: SetLocalConsolePasswordResponse -> TestTree
responseSetLocalConsolePassword =
  res
    "SetLocalConsolePasswordResponse"
    "fixture/SetLocalConsolePasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetLocalConsolePassword)

responseSetSMBGuestPassword :: SetSMBGuestPasswordResponse -> TestTree
responseSetSMBGuestPassword =
  res
    "SetSMBGuestPasswordResponse"
    "fixture/SetSMBGuestPasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetSMBGuestPassword)

responseShutdownGateway :: ShutdownGatewayResponse -> TestTree
responseShutdownGateway =
  res
    "ShutdownGatewayResponse"
    "fixture/ShutdownGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ShutdownGateway)

responseStartAvailabilityMonitorTest :: StartAvailabilityMonitorTestResponse -> TestTree
responseStartAvailabilityMonitorTest =
  res
    "StartAvailabilityMonitorTestResponse"
    "fixture/StartAvailabilityMonitorTestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAvailabilityMonitorTest)

responseStartGateway :: StartGatewayResponse -> TestTree
responseStartGateway =
  res
    "StartGatewayResponse"
    "fixture/StartGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartGateway)

responseUpdateAutomaticTapeCreationPolicy :: UpdateAutomaticTapeCreationPolicyResponse -> TestTree
responseUpdateAutomaticTapeCreationPolicy =
  res
    "UpdateAutomaticTapeCreationPolicyResponse"
    "fixture/UpdateAutomaticTapeCreationPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAutomaticTapeCreationPolicy)

responseUpdateBandwidthRateLimit :: UpdateBandwidthRateLimitResponse -> TestTree
responseUpdateBandwidthRateLimit =
  res
    "UpdateBandwidthRateLimitResponse"
    "fixture/UpdateBandwidthRateLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBandwidthRateLimit)

responseUpdateBandwidthRateLimitSchedule :: UpdateBandwidthRateLimitScheduleResponse -> TestTree
responseUpdateBandwidthRateLimitSchedule =
  res
    "UpdateBandwidthRateLimitScheduleResponse"
    "fixture/UpdateBandwidthRateLimitScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBandwidthRateLimitSchedule)

responseUpdateChapCredentials :: UpdateChapCredentialsResponse -> TestTree
responseUpdateChapCredentials =
  res
    "UpdateChapCredentialsResponse"
    "fixture/UpdateChapCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChapCredentials)

responseUpdateFileSystemAssociation :: UpdateFileSystemAssociationResponse -> TestTree
responseUpdateFileSystemAssociation =
  res
    "UpdateFileSystemAssociationResponse"
    "fixture/UpdateFileSystemAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFileSystemAssociation)

responseUpdateGatewayInformation :: UpdateGatewayInformationResponse -> TestTree
responseUpdateGatewayInformation =
  res
    "UpdateGatewayInformationResponse"
    "fixture/UpdateGatewayInformationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGatewayInformation)

responseUpdateGatewaySoftwareNow :: UpdateGatewaySoftwareNowResponse -> TestTree
responseUpdateGatewaySoftwareNow =
  res
    "UpdateGatewaySoftwareNowResponse"
    "fixture/UpdateGatewaySoftwareNowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGatewaySoftwareNow)

responseUpdateMaintenanceStartTime :: UpdateMaintenanceStartTimeResponse -> TestTree
responseUpdateMaintenanceStartTime =
  res
    "UpdateMaintenanceStartTimeResponse"
    "fixture/UpdateMaintenanceStartTimeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMaintenanceStartTime)

responseUpdateNFSFileShare :: UpdateNFSFileShareResponse -> TestTree
responseUpdateNFSFileShare =
  res
    "UpdateNFSFileShareResponse"
    "fixture/UpdateNFSFileShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNFSFileShare)

responseUpdateSMBFileShare :: UpdateSMBFileShareResponse -> TestTree
responseUpdateSMBFileShare =
  res
    "UpdateSMBFileShareResponse"
    "fixture/UpdateSMBFileShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSMBFileShare)

responseUpdateSMBFileShareVisibility :: UpdateSMBFileShareVisibilityResponse -> TestTree
responseUpdateSMBFileShareVisibility =
  res
    "UpdateSMBFileShareVisibilityResponse"
    "fixture/UpdateSMBFileShareVisibilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSMBFileShareVisibility)

responseUpdateSMBLocalGroups :: UpdateSMBLocalGroupsResponse -> TestTree
responseUpdateSMBLocalGroups =
  res
    "UpdateSMBLocalGroupsResponse"
    "fixture/UpdateSMBLocalGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSMBLocalGroups)

responseUpdateSMBSecurityStrategy :: UpdateSMBSecurityStrategyResponse -> TestTree
responseUpdateSMBSecurityStrategy =
  res
    "UpdateSMBSecurityStrategyResponse"
    "fixture/UpdateSMBSecurityStrategyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSMBSecurityStrategy)

responseUpdateSnapshotSchedule :: UpdateSnapshotScheduleResponse -> TestTree
responseUpdateSnapshotSchedule =
  res
    "UpdateSnapshotScheduleResponse"
    "fixture/UpdateSnapshotScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSnapshotSchedule)

responseUpdateVTLDeviceType :: UpdateVTLDeviceTypeResponse -> TestTree
responseUpdateVTLDeviceType =
  res
    "UpdateVTLDeviceTypeResponse"
    "fixture/UpdateVTLDeviceTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVTLDeviceType)
