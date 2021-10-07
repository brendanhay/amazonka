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

import Data.Proxy
import Network.AWS.StorageGateway
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
--         [ requestDetachVolume $
--             newDetachVolume
--
--         , requestDescribeSMBSettings $
--             newDescribeSMBSettings
--
--         , requestRetrieveTapeArchive $
--             newRetrieveTapeArchive
--
--         , requestCancelArchival $
--             newCancelArchival
--
--         , requestDescribeFileSystemAssociations $
--             newDescribeFileSystemAssociations
--
--         , requestCreateNFSFileShare $
--             newCreateNFSFileShare
--
--         , requestDisassociateFileSystem $
--             newDisassociateFileSystem
--
--         , requestDescribeCache $
--             newDescribeCache
--
--         , requestRefreshCache $
--             newRefreshCache
--
--         , requestListTapes $
--             newListTapes
--
--         , requestDescribeSMBFileShares $
--             newDescribeSMBFileShares
--
--         , requestUpdateNFSFileShare $
--             newUpdateNFSFileShare
--
--         , requestDescribeGatewayInformation $
--             newDescribeGatewayInformation
--
--         , requestListVolumes $
--             newListVolumes
--
--         , requestAddWorkingStorage $
--             newAddWorkingStorage
--
--         , requestUpdateMaintenanceStartTime $
--             newUpdateMaintenanceStartTime
--
--         , requestActivateGateway $
--             newActivateGateway
--
--         , requestListVolumeRecoveryPoints $
--             newListVolumeRecoveryPoints
--
--         , requestDescribeAvailabilityMonitorTest $
--             newDescribeAvailabilityMonitorTest
--
--         , requestUpdateSMBFileShare $
--             newUpdateSMBFileShare
--
--         , requestDescribeTapes $
--             newDescribeTapes
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestDeleteTapeArchive $
--             newDeleteTapeArchive
--
--         , requestAttachVolume $
--             newAttachVolume
--
--         , requestCreateSMBFileShare $
--             newCreateSMBFileShare
--
--         , requestUpdateSMBSecurityStrategy $
--             newUpdateSMBSecurityStrategy
--
--         , requestStartGateway $
--             newStartGateway
--
--         , requestUpdateGatewaySoftwareNow $
--             newUpdateGatewaySoftwareNow
--
--         , requestCancelRetrieval $
--             newCancelRetrieval
--
--         , requestCreateTapePool $
--             newCreateTapePool
--
--         , requestListTapePools $
--             newListTapePools
--
--         , requestDescribeBandwidthRateLimit $
--             newDescribeBandwidthRateLimit
--
--         , requestCreateTapeWithBarcode $
--             newCreateTapeWithBarcode
--
--         , requestJoinDomain $
--             newJoinDomain
--
--         , requestListVolumeInitiators $
--             newListVolumeInitiators
--
--         , requestListFileShares $
--             newListFileShares
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
--         , requestSetLocalConsolePassword $
--             newSetLocalConsolePassword
--
--         , requestAssociateFileSystem $
--             newAssociateFileSystem
--
--         , requestDescribeChapCredentials $
--             newDescribeChapCredentials
--
--         , requestUpdateBandwidthRateLimitSchedule $
--             newUpdateBandwidthRateLimitSchedule
--
--         , requestDisableGateway $
--             newDisableGateway
--
--         , requestDescribeSnapshotSchedule $
--             newDescribeSnapshotSchedule
--
--         , requestDescribeTapeArchives $
--             newDescribeTapeArchives
--
--         , requestCreateStorediSCSIVolume $
--             newCreateStorediSCSIVolume
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestDeleteGateway $
--             newDeleteGateway
--
--         , requestCreateSnapshotFromVolumeRecoveryPoint $
--             newCreateSnapshotFromVolumeRecoveryPoint
--
--         , requestDeleteBandwidthRateLimit $
--             newDeleteBandwidthRateLimit
--
--         , requestDescribeTapeRecoveryPoints $
--             newDescribeTapeRecoveryPoints
--
--         , requestSetSMBGuestPassword $
--             newSetSMBGuestPassword
--
--         , requestUpdateBandwidthRateLimit $
--             newUpdateBandwidthRateLimit
--
--         , requestRetrieveTapeRecoveryPoint $
--             newRetrieveTapeRecoveryPoint
--
--         , requestListGateways $
--             newListGateways
--
--         , requestListLocalDisks $
--             newListLocalDisks
--
--         , requestDeleteVolume $
--             newDeleteVolume
--
--         , requestDeleteTape $
--             newDeleteTape
--
--         , requestResetCache $
--             newResetCache
--
--         , requestDescribeUploadBuffer $
--             newDescribeUploadBuffer
--
--         , requestDescribeNFSFileShares $
--             newDescribeNFSFileShares
--
--         , requestDescribeStorediSCSIVolumes $
--             newDescribeStorediSCSIVolumes
--
--         , requestDeleteChapCredentials $
--             newDeleteChapCredentials
--
--         , requestListFileSystemAssociations $
--             newListFileSystemAssociations
--
--         , requestUpdateFileSystemAssociation $
--             newUpdateFileSystemAssociation
--
--         , requestUpdateChapCredentials $
--             newUpdateChapCredentials
--
--         , requestStartAvailabilityMonitorTest $
--             newStartAvailabilityMonitorTest
--
--         , requestUpdateSnapshotSchedule $
--             newUpdateSnapshotSchedule
--
--         , requestDescribeVTLDevices $
--             newDescribeVTLDevices
--
--         , requestDeleteSnapshotSchedule $
--             newDeleteSnapshotSchedule
--
--         , requestListAutomaticTapeCreationPolicies $
--             newListAutomaticTapeCreationPolicies
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestShutdownGateway $
--             newShutdownGateway
--
--         , requestUpdateSMBFileShareVisibility $
--             newUpdateSMBFileShareVisibility
--
--         , requestDescribeBandwidthRateLimitSchedule $
--             newDescribeBandwidthRateLimitSchedule
--
--         , requestAssignTapePool $
--             newAssignTapePool
--
--         , requestDescribeCachediSCSIVolumes $
--             newDescribeCachediSCSIVolumes
--
--         , requestNotifyWhenUploaded $
--             newNotifyWhenUploaded
--
--         , requestDescribeMaintenanceStartTime $
--             newDescribeMaintenanceStartTime
--
--         , requestUpdateGatewayInformation $
--             newUpdateGatewayInformation
--
--         , requestDeleteAutomaticTapeCreationPolicy $
--             newDeleteAutomaticTapeCreationPolicy
--
--         , requestDescribeWorkingStorage $
--             newDescribeWorkingStorage
--
--         , requestDeleteTapePool $
--             newDeleteTapePool
--
--         , requestUpdateAutomaticTapeCreationPolicy $
--             newUpdateAutomaticTapeCreationPolicy
--
--         , requestAddCache $
--             newAddCache
--
--         , requestAddUploadBuffer $
--             newAddUploadBuffer
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDeleteFileShare $
--             newDeleteFileShare
--
--           ]

--     , testGroup "response"
--         [ responseDetachVolume $
--             newDetachVolumeResponse
--
--         , responseDescribeSMBSettings $
--             newDescribeSMBSettingsResponse
--
--         , responseRetrieveTapeArchive $
--             newRetrieveTapeArchiveResponse
--
--         , responseCancelArchival $
--             newCancelArchivalResponse
--
--         , responseDescribeFileSystemAssociations $
--             newDescribeFileSystemAssociationsResponse
--
--         , responseCreateNFSFileShare $
--             newCreateNFSFileShareResponse
--
--         , responseDisassociateFileSystem $
--             newDisassociateFileSystemResponse
--
--         , responseDescribeCache $
--             newDescribeCacheResponse
--
--         , responseRefreshCache $
--             newRefreshCacheResponse
--
--         , responseListTapes $
--             newListTapesResponse
--
--         , responseDescribeSMBFileShares $
--             newDescribeSMBFileSharesResponse
--
--         , responseUpdateNFSFileShare $
--             newUpdateNFSFileShareResponse
--
--         , responseDescribeGatewayInformation $
--             newDescribeGatewayInformationResponse
--
--         , responseListVolumes $
--             newListVolumesResponse
--
--         , responseAddWorkingStorage $
--             newAddWorkingStorageResponse
--
--         , responseUpdateMaintenanceStartTime $
--             newUpdateMaintenanceStartTimeResponse
--
--         , responseActivateGateway $
--             newActivateGatewayResponse
--
--         , responseListVolumeRecoveryPoints $
--             newListVolumeRecoveryPointsResponse
--
--         , responseDescribeAvailabilityMonitorTest $
--             newDescribeAvailabilityMonitorTestResponse
--
--         , responseUpdateSMBFileShare $
--             newUpdateSMBFileShareResponse
--
--         , responseDescribeTapes $
--             newDescribeTapesResponse
--
--         , responseRemoveTagsFromResource $
--             newRemoveTagsFromResourceResponse
--
--         , responseDeleteTapeArchive $
--             newDeleteTapeArchiveResponse
--
--         , responseAttachVolume $
--             newAttachVolumeResponse
--
--         , responseCreateSMBFileShare $
--             newCreateSMBFileShareResponse
--
--         , responseUpdateSMBSecurityStrategy $
--             newUpdateSMBSecurityStrategyResponse
--
--         , responseStartGateway $
--             newStartGatewayResponse
--
--         , responseUpdateGatewaySoftwareNow $
--             newUpdateGatewaySoftwareNowResponse
--
--         , responseCancelRetrieval $
--             newCancelRetrievalResponse
--
--         , responseCreateTapePool $
--             newCreateTapePoolResponse
--
--         , responseListTapePools $
--             newListTapePoolsResponse
--
--         , responseDescribeBandwidthRateLimit $
--             newDescribeBandwidthRateLimitResponse
--
--         , responseCreateTapeWithBarcode $
--             newCreateTapeWithBarcodeResponse
--
--         , responseJoinDomain $
--             newJoinDomainResponse
--
--         , responseListVolumeInitiators $
--             newListVolumeInitiatorsResponse
--
--         , responseListFileShares $
--             newListFileSharesResponse
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
--         , responseSetLocalConsolePassword $
--             newSetLocalConsolePasswordResponse
--
--         , responseAssociateFileSystem $
--             newAssociateFileSystemResponse
--
--         , responseDescribeChapCredentials $
--             newDescribeChapCredentialsResponse
--
--         , responseUpdateBandwidthRateLimitSchedule $
--             newUpdateBandwidthRateLimitScheduleResponse
--
--         , responseDisableGateway $
--             newDisableGatewayResponse
--
--         , responseDescribeSnapshotSchedule $
--             newDescribeSnapshotScheduleResponse
--
--         , responseDescribeTapeArchives $
--             newDescribeTapeArchivesResponse
--
--         , responseCreateStorediSCSIVolume $
--             newCreateStorediSCSIVolumeResponse
--
--         , responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseDeleteGateway $
--             newDeleteGatewayResponse
--
--         , responseCreateSnapshotFromVolumeRecoveryPoint $
--             newCreateSnapshotFromVolumeRecoveryPointResponse
--
--         , responseDeleteBandwidthRateLimit $
--             newDeleteBandwidthRateLimitResponse
--
--         , responseDescribeTapeRecoveryPoints $
--             newDescribeTapeRecoveryPointsResponse
--
--         , responseSetSMBGuestPassword $
--             newSetSMBGuestPasswordResponse
--
--         , responseUpdateBandwidthRateLimit $
--             newUpdateBandwidthRateLimitResponse
--
--         , responseRetrieveTapeRecoveryPoint $
--             newRetrieveTapeRecoveryPointResponse
--
--         , responseListGateways $
--             newListGatewaysResponse
--
--         , responseListLocalDisks $
--             newListLocalDisksResponse
--
--         , responseDeleteVolume $
--             newDeleteVolumeResponse
--
--         , responseDeleteTape $
--             newDeleteTapeResponse
--
--         , responseResetCache $
--             newResetCacheResponse
--
--         , responseDescribeUploadBuffer $
--             newDescribeUploadBufferResponse
--
--         , responseDescribeNFSFileShares $
--             newDescribeNFSFileSharesResponse
--
--         , responseDescribeStorediSCSIVolumes $
--             newDescribeStorediSCSIVolumesResponse
--
--         , responseDeleteChapCredentials $
--             newDeleteChapCredentialsResponse
--
--         , responseListFileSystemAssociations $
--             newListFileSystemAssociationsResponse
--
--         , responseUpdateFileSystemAssociation $
--             newUpdateFileSystemAssociationResponse
--
--         , responseUpdateChapCredentials $
--             newUpdateChapCredentialsResponse
--
--         , responseStartAvailabilityMonitorTest $
--             newStartAvailabilityMonitorTestResponse
--
--         , responseUpdateSnapshotSchedule $
--             newUpdateSnapshotScheduleResponse
--
--         , responseDescribeVTLDevices $
--             newDescribeVTLDevicesResponse
--
--         , responseDeleteSnapshotSchedule $
--             newDeleteSnapshotScheduleResponse
--
--         , responseListAutomaticTapeCreationPolicies $
--             newListAutomaticTapeCreationPoliciesResponse
--
--         , responseCreateSnapshot $
--             newCreateSnapshotResponse
--
--         , responseShutdownGateway $
--             newShutdownGatewayResponse
--
--         , responseUpdateSMBFileShareVisibility $
--             newUpdateSMBFileShareVisibilityResponse
--
--         , responseDescribeBandwidthRateLimitSchedule $
--             newDescribeBandwidthRateLimitScheduleResponse
--
--         , responseAssignTapePool $
--             newAssignTapePoolResponse
--
--         , responseDescribeCachediSCSIVolumes $
--             newDescribeCachediSCSIVolumesResponse
--
--         , responseNotifyWhenUploaded $
--             newNotifyWhenUploadedResponse
--
--         , responseDescribeMaintenanceStartTime $
--             newDescribeMaintenanceStartTimeResponse
--
--         , responseUpdateGatewayInformation $
--             newUpdateGatewayInformationResponse
--
--         , responseDeleteAutomaticTapeCreationPolicy $
--             newDeleteAutomaticTapeCreationPolicyResponse
--
--         , responseDescribeWorkingStorage $
--             newDescribeWorkingStorageResponse
--
--         , responseDeleteTapePool $
--             newDeleteTapePoolResponse
--
--         , responseUpdateAutomaticTapeCreationPolicy $
--             newUpdateAutomaticTapeCreationPolicyResponse
--
--         , responseAddCache $
--             newAddCacheResponse
--
--         , responseAddUploadBuffer $
--             newAddUploadBufferResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDeleteFileShare $
--             newDeleteFileShareResponse
--
--           ]
--     ]

-- Requests

requestDetachVolume :: DetachVolume -> TestTree
requestDetachVolume =
  req
    "DetachVolume"
    "fixture/DetachVolume.yaml"

requestDescribeSMBSettings :: DescribeSMBSettings -> TestTree
requestDescribeSMBSettings =
  req
    "DescribeSMBSettings"
    "fixture/DescribeSMBSettings.yaml"

requestRetrieveTapeArchive :: RetrieveTapeArchive -> TestTree
requestRetrieveTapeArchive =
  req
    "RetrieveTapeArchive"
    "fixture/RetrieveTapeArchive.yaml"

requestCancelArchival :: CancelArchival -> TestTree
requestCancelArchival =
  req
    "CancelArchival"
    "fixture/CancelArchival.yaml"

requestDescribeFileSystemAssociations :: DescribeFileSystemAssociations -> TestTree
requestDescribeFileSystemAssociations =
  req
    "DescribeFileSystemAssociations"
    "fixture/DescribeFileSystemAssociations.yaml"

requestCreateNFSFileShare :: CreateNFSFileShare -> TestTree
requestCreateNFSFileShare =
  req
    "CreateNFSFileShare"
    "fixture/CreateNFSFileShare.yaml"

requestDisassociateFileSystem :: DisassociateFileSystem -> TestTree
requestDisassociateFileSystem =
  req
    "DisassociateFileSystem"
    "fixture/DisassociateFileSystem.yaml"

requestDescribeCache :: DescribeCache -> TestTree
requestDescribeCache =
  req
    "DescribeCache"
    "fixture/DescribeCache.yaml"

requestRefreshCache :: RefreshCache -> TestTree
requestRefreshCache =
  req
    "RefreshCache"
    "fixture/RefreshCache.yaml"

requestListTapes :: ListTapes -> TestTree
requestListTapes =
  req
    "ListTapes"
    "fixture/ListTapes.yaml"

requestDescribeSMBFileShares :: DescribeSMBFileShares -> TestTree
requestDescribeSMBFileShares =
  req
    "DescribeSMBFileShares"
    "fixture/DescribeSMBFileShares.yaml"

requestUpdateNFSFileShare :: UpdateNFSFileShare -> TestTree
requestUpdateNFSFileShare =
  req
    "UpdateNFSFileShare"
    "fixture/UpdateNFSFileShare.yaml"

requestDescribeGatewayInformation :: DescribeGatewayInformation -> TestTree
requestDescribeGatewayInformation =
  req
    "DescribeGatewayInformation"
    "fixture/DescribeGatewayInformation.yaml"

requestListVolumes :: ListVolumes -> TestTree
requestListVolumes =
  req
    "ListVolumes"
    "fixture/ListVolumes.yaml"

requestAddWorkingStorage :: AddWorkingStorage -> TestTree
requestAddWorkingStorage =
  req
    "AddWorkingStorage"
    "fixture/AddWorkingStorage.yaml"

requestUpdateMaintenanceStartTime :: UpdateMaintenanceStartTime -> TestTree
requestUpdateMaintenanceStartTime =
  req
    "UpdateMaintenanceStartTime"
    "fixture/UpdateMaintenanceStartTime.yaml"

requestActivateGateway :: ActivateGateway -> TestTree
requestActivateGateway =
  req
    "ActivateGateway"
    "fixture/ActivateGateway.yaml"

requestListVolumeRecoveryPoints :: ListVolumeRecoveryPoints -> TestTree
requestListVolumeRecoveryPoints =
  req
    "ListVolumeRecoveryPoints"
    "fixture/ListVolumeRecoveryPoints.yaml"

requestDescribeAvailabilityMonitorTest :: DescribeAvailabilityMonitorTest -> TestTree
requestDescribeAvailabilityMonitorTest =
  req
    "DescribeAvailabilityMonitorTest"
    "fixture/DescribeAvailabilityMonitorTest.yaml"

requestUpdateSMBFileShare :: UpdateSMBFileShare -> TestTree
requestUpdateSMBFileShare =
  req
    "UpdateSMBFileShare"
    "fixture/UpdateSMBFileShare.yaml"

requestDescribeTapes :: DescribeTapes -> TestTree
requestDescribeTapes =
  req
    "DescribeTapes"
    "fixture/DescribeTapes.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestDeleteTapeArchive :: DeleteTapeArchive -> TestTree
requestDeleteTapeArchive =
  req
    "DeleteTapeArchive"
    "fixture/DeleteTapeArchive.yaml"

requestAttachVolume :: AttachVolume -> TestTree
requestAttachVolume =
  req
    "AttachVolume"
    "fixture/AttachVolume.yaml"

requestCreateSMBFileShare :: CreateSMBFileShare -> TestTree
requestCreateSMBFileShare =
  req
    "CreateSMBFileShare"
    "fixture/CreateSMBFileShare.yaml"

requestUpdateSMBSecurityStrategy :: UpdateSMBSecurityStrategy -> TestTree
requestUpdateSMBSecurityStrategy =
  req
    "UpdateSMBSecurityStrategy"
    "fixture/UpdateSMBSecurityStrategy.yaml"

requestStartGateway :: StartGateway -> TestTree
requestStartGateway =
  req
    "StartGateway"
    "fixture/StartGateway.yaml"

requestUpdateGatewaySoftwareNow :: UpdateGatewaySoftwareNow -> TestTree
requestUpdateGatewaySoftwareNow =
  req
    "UpdateGatewaySoftwareNow"
    "fixture/UpdateGatewaySoftwareNow.yaml"

requestCancelRetrieval :: CancelRetrieval -> TestTree
requestCancelRetrieval =
  req
    "CancelRetrieval"
    "fixture/CancelRetrieval.yaml"

requestCreateTapePool :: CreateTapePool -> TestTree
requestCreateTapePool =
  req
    "CreateTapePool"
    "fixture/CreateTapePool.yaml"

requestListTapePools :: ListTapePools -> TestTree
requestListTapePools =
  req
    "ListTapePools"
    "fixture/ListTapePools.yaml"

requestDescribeBandwidthRateLimit :: DescribeBandwidthRateLimit -> TestTree
requestDescribeBandwidthRateLimit =
  req
    "DescribeBandwidthRateLimit"
    "fixture/DescribeBandwidthRateLimit.yaml"

requestCreateTapeWithBarcode :: CreateTapeWithBarcode -> TestTree
requestCreateTapeWithBarcode =
  req
    "CreateTapeWithBarcode"
    "fixture/CreateTapeWithBarcode.yaml"

requestJoinDomain :: JoinDomain -> TestTree
requestJoinDomain =
  req
    "JoinDomain"
    "fixture/JoinDomain.yaml"

requestListVolumeInitiators :: ListVolumeInitiators -> TestTree
requestListVolumeInitiators =
  req
    "ListVolumeInitiators"
    "fixture/ListVolumeInitiators.yaml"

requestListFileShares :: ListFileShares -> TestTree
requestListFileShares =
  req
    "ListFileShares"
    "fixture/ListFileShares.yaml"

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

requestSetLocalConsolePassword :: SetLocalConsolePassword -> TestTree
requestSetLocalConsolePassword =
  req
    "SetLocalConsolePassword"
    "fixture/SetLocalConsolePassword.yaml"

requestAssociateFileSystem :: AssociateFileSystem -> TestTree
requestAssociateFileSystem =
  req
    "AssociateFileSystem"
    "fixture/AssociateFileSystem.yaml"

requestDescribeChapCredentials :: DescribeChapCredentials -> TestTree
requestDescribeChapCredentials =
  req
    "DescribeChapCredentials"
    "fixture/DescribeChapCredentials.yaml"

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

requestDescribeSnapshotSchedule :: DescribeSnapshotSchedule -> TestTree
requestDescribeSnapshotSchedule =
  req
    "DescribeSnapshotSchedule"
    "fixture/DescribeSnapshotSchedule.yaml"

requestDescribeTapeArchives :: DescribeTapeArchives -> TestTree
requestDescribeTapeArchives =
  req
    "DescribeTapeArchives"
    "fixture/DescribeTapeArchives.yaml"

requestCreateStorediSCSIVolume :: CreateStorediSCSIVolume -> TestTree
requestCreateStorediSCSIVolume =
  req
    "CreateStorediSCSIVolume"
    "fixture/CreateStorediSCSIVolume.yaml"

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

requestCreateSnapshotFromVolumeRecoveryPoint :: CreateSnapshotFromVolumeRecoveryPoint -> TestTree
requestCreateSnapshotFromVolumeRecoveryPoint =
  req
    "CreateSnapshotFromVolumeRecoveryPoint"
    "fixture/CreateSnapshotFromVolumeRecoveryPoint.yaml"

requestDeleteBandwidthRateLimit :: DeleteBandwidthRateLimit -> TestTree
requestDeleteBandwidthRateLimit =
  req
    "DeleteBandwidthRateLimit"
    "fixture/DeleteBandwidthRateLimit.yaml"

requestDescribeTapeRecoveryPoints :: DescribeTapeRecoveryPoints -> TestTree
requestDescribeTapeRecoveryPoints =
  req
    "DescribeTapeRecoveryPoints"
    "fixture/DescribeTapeRecoveryPoints.yaml"

requestSetSMBGuestPassword :: SetSMBGuestPassword -> TestTree
requestSetSMBGuestPassword =
  req
    "SetSMBGuestPassword"
    "fixture/SetSMBGuestPassword.yaml"

requestUpdateBandwidthRateLimit :: UpdateBandwidthRateLimit -> TestTree
requestUpdateBandwidthRateLimit =
  req
    "UpdateBandwidthRateLimit"
    "fixture/UpdateBandwidthRateLimit.yaml"

requestRetrieveTapeRecoveryPoint :: RetrieveTapeRecoveryPoint -> TestTree
requestRetrieveTapeRecoveryPoint =
  req
    "RetrieveTapeRecoveryPoint"
    "fixture/RetrieveTapeRecoveryPoint.yaml"

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

requestDeleteVolume :: DeleteVolume -> TestTree
requestDeleteVolume =
  req
    "DeleteVolume"
    "fixture/DeleteVolume.yaml"

requestDeleteTape :: DeleteTape -> TestTree
requestDeleteTape =
  req
    "DeleteTape"
    "fixture/DeleteTape.yaml"

requestResetCache :: ResetCache -> TestTree
requestResetCache =
  req
    "ResetCache"
    "fixture/ResetCache.yaml"

requestDescribeUploadBuffer :: DescribeUploadBuffer -> TestTree
requestDescribeUploadBuffer =
  req
    "DescribeUploadBuffer"
    "fixture/DescribeUploadBuffer.yaml"

requestDescribeNFSFileShares :: DescribeNFSFileShares -> TestTree
requestDescribeNFSFileShares =
  req
    "DescribeNFSFileShares"
    "fixture/DescribeNFSFileShares.yaml"

requestDescribeStorediSCSIVolumes :: DescribeStorediSCSIVolumes -> TestTree
requestDescribeStorediSCSIVolumes =
  req
    "DescribeStorediSCSIVolumes"
    "fixture/DescribeStorediSCSIVolumes.yaml"

requestDeleteChapCredentials :: DeleteChapCredentials -> TestTree
requestDeleteChapCredentials =
  req
    "DeleteChapCredentials"
    "fixture/DeleteChapCredentials.yaml"

requestListFileSystemAssociations :: ListFileSystemAssociations -> TestTree
requestListFileSystemAssociations =
  req
    "ListFileSystemAssociations"
    "fixture/ListFileSystemAssociations.yaml"

requestUpdateFileSystemAssociation :: UpdateFileSystemAssociation -> TestTree
requestUpdateFileSystemAssociation =
  req
    "UpdateFileSystemAssociation"
    "fixture/UpdateFileSystemAssociation.yaml"

requestUpdateChapCredentials :: UpdateChapCredentials -> TestTree
requestUpdateChapCredentials =
  req
    "UpdateChapCredentials"
    "fixture/UpdateChapCredentials.yaml"

requestStartAvailabilityMonitorTest :: StartAvailabilityMonitorTest -> TestTree
requestStartAvailabilityMonitorTest =
  req
    "StartAvailabilityMonitorTest"
    "fixture/StartAvailabilityMonitorTest.yaml"

requestUpdateSnapshotSchedule :: UpdateSnapshotSchedule -> TestTree
requestUpdateSnapshotSchedule =
  req
    "UpdateSnapshotSchedule"
    "fixture/UpdateSnapshotSchedule.yaml"

requestDescribeVTLDevices :: DescribeVTLDevices -> TestTree
requestDescribeVTLDevices =
  req
    "DescribeVTLDevices"
    "fixture/DescribeVTLDevices.yaml"

requestDeleteSnapshotSchedule :: DeleteSnapshotSchedule -> TestTree
requestDeleteSnapshotSchedule =
  req
    "DeleteSnapshotSchedule"
    "fixture/DeleteSnapshotSchedule.yaml"

requestListAutomaticTapeCreationPolicies :: ListAutomaticTapeCreationPolicies -> TestTree
requestListAutomaticTapeCreationPolicies =
  req
    "ListAutomaticTapeCreationPolicies"
    "fixture/ListAutomaticTapeCreationPolicies.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestShutdownGateway :: ShutdownGateway -> TestTree
requestShutdownGateway =
  req
    "ShutdownGateway"
    "fixture/ShutdownGateway.yaml"

requestUpdateSMBFileShareVisibility :: UpdateSMBFileShareVisibility -> TestTree
requestUpdateSMBFileShareVisibility =
  req
    "UpdateSMBFileShareVisibility"
    "fixture/UpdateSMBFileShareVisibility.yaml"

requestDescribeBandwidthRateLimitSchedule :: DescribeBandwidthRateLimitSchedule -> TestTree
requestDescribeBandwidthRateLimitSchedule =
  req
    "DescribeBandwidthRateLimitSchedule"
    "fixture/DescribeBandwidthRateLimitSchedule.yaml"

requestAssignTapePool :: AssignTapePool -> TestTree
requestAssignTapePool =
  req
    "AssignTapePool"
    "fixture/AssignTapePool.yaml"

requestDescribeCachediSCSIVolumes :: DescribeCachediSCSIVolumes -> TestTree
requestDescribeCachediSCSIVolumes =
  req
    "DescribeCachediSCSIVolumes"
    "fixture/DescribeCachediSCSIVolumes.yaml"

requestNotifyWhenUploaded :: NotifyWhenUploaded -> TestTree
requestNotifyWhenUploaded =
  req
    "NotifyWhenUploaded"
    "fixture/NotifyWhenUploaded.yaml"

requestDescribeMaintenanceStartTime :: DescribeMaintenanceStartTime -> TestTree
requestDescribeMaintenanceStartTime =
  req
    "DescribeMaintenanceStartTime"
    "fixture/DescribeMaintenanceStartTime.yaml"

requestUpdateGatewayInformation :: UpdateGatewayInformation -> TestTree
requestUpdateGatewayInformation =
  req
    "UpdateGatewayInformation"
    "fixture/UpdateGatewayInformation.yaml"

requestDeleteAutomaticTapeCreationPolicy :: DeleteAutomaticTapeCreationPolicy -> TestTree
requestDeleteAutomaticTapeCreationPolicy =
  req
    "DeleteAutomaticTapeCreationPolicy"
    "fixture/DeleteAutomaticTapeCreationPolicy.yaml"

requestDescribeWorkingStorage :: DescribeWorkingStorage -> TestTree
requestDescribeWorkingStorage =
  req
    "DescribeWorkingStorage"
    "fixture/DescribeWorkingStorage.yaml"

requestDeleteTapePool :: DeleteTapePool -> TestTree
requestDeleteTapePool =
  req
    "DeleteTapePool"
    "fixture/DeleteTapePool.yaml"

requestUpdateAutomaticTapeCreationPolicy :: UpdateAutomaticTapeCreationPolicy -> TestTree
requestUpdateAutomaticTapeCreationPolicy =
  req
    "UpdateAutomaticTapeCreationPolicy"
    "fixture/UpdateAutomaticTapeCreationPolicy.yaml"

requestAddCache :: AddCache -> TestTree
requestAddCache =
  req
    "AddCache"
    "fixture/AddCache.yaml"

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

requestDeleteFileShare :: DeleteFileShare -> TestTree
requestDeleteFileShare =
  req
    "DeleteFileShare"
    "fixture/DeleteFileShare.yaml"

-- Responses

responseDetachVolume :: DetachVolumeResponse -> TestTree
responseDetachVolume =
  res
    "DetachVolumeResponse"
    "fixture/DetachVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy DetachVolume)

responseDescribeSMBSettings :: DescribeSMBSettingsResponse -> TestTree
responseDescribeSMBSettings =
  res
    "DescribeSMBSettingsResponse"
    "fixture/DescribeSMBSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSMBSettings)

responseRetrieveTapeArchive :: RetrieveTapeArchiveResponse -> TestTree
responseRetrieveTapeArchive =
  res
    "RetrieveTapeArchiveResponse"
    "fixture/RetrieveTapeArchiveResponse.proto"
    defaultService
    (Proxy :: Proxy RetrieveTapeArchive)

responseCancelArchival :: CancelArchivalResponse -> TestTree
responseCancelArchival =
  res
    "CancelArchivalResponse"
    "fixture/CancelArchivalResponse.proto"
    defaultService
    (Proxy :: Proxy CancelArchival)

responseDescribeFileSystemAssociations :: DescribeFileSystemAssociationsResponse -> TestTree
responseDescribeFileSystemAssociations =
  res
    "DescribeFileSystemAssociationsResponse"
    "fixture/DescribeFileSystemAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFileSystemAssociations)

responseCreateNFSFileShare :: CreateNFSFileShareResponse -> TestTree
responseCreateNFSFileShare =
  res
    "CreateNFSFileShareResponse"
    "fixture/CreateNFSFileShareResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNFSFileShare)

responseDisassociateFileSystem :: DisassociateFileSystemResponse -> TestTree
responseDisassociateFileSystem =
  res
    "DisassociateFileSystemResponse"
    "fixture/DisassociateFileSystemResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateFileSystem)

responseDescribeCache :: DescribeCacheResponse -> TestTree
responseDescribeCache =
  res
    "DescribeCacheResponse"
    "fixture/DescribeCacheResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCache)

responseRefreshCache :: RefreshCacheResponse -> TestTree
responseRefreshCache =
  res
    "RefreshCacheResponse"
    "fixture/RefreshCacheResponse.proto"
    defaultService
    (Proxy :: Proxy RefreshCache)

responseListTapes :: ListTapesResponse -> TestTree
responseListTapes =
  res
    "ListTapesResponse"
    "fixture/ListTapesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTapes)

responseDescribeSMBFileShares :: DescribeSMBFileSharesResponse -> TestTree
responseDescribeSMBFileShares =
  res
    "DescribeSMBFileSharesResponse"
    "fixture/DescribeSMBFileSharesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSMBFileShares)

responseUpdateNFSFileShare :: UpdateNFSFileShareResponse -> TestTree
responseUpdateNFSFileShare =
  res
    "UpdateNFSFileShareResponse"
    "fixture/UpdateNFSFileShareResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNFSFileShare)

responseDescribeGatewayInformation :: DescribeGatewayInformationResponse -> TestTree
responseDescribeGatewayInformation =
  res
    "DescribeGatewayInformationResponse"
    "fixture/DescribeGatewayInformationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGatewayInformation)

responseListVolumes :: ListVolumesResponse -> TestTree
responseListVolumes =
  res
    "ListVolumesResponse"
    "fixture/ListVolumesResponse.proto"
    defaultService
    (Proxy :: Proxy ListVolumes)

responseAddWorkingStorage :: AddWorkingStorageResponse -> TestTree
responseAddWorkingStorage =
  res
    "AddWorkingStorageResponse"
    "fixture/AddWorkingStorageResponse.proto"
    defaultService
    (Proxy :: Proxy AddWorkingStorage)

responseUpdateMaintenanceStartTime :: UpdateMaintenanceStartTimeResponse -> TestTree
responseUpdateMaintenanceStartTime =
  res
    "UpdateMaintenanceStartTimeResponse"
    "fixture/UpdateMaintenanceStartTimeResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMaintenanceStartTime)

responseActivateGateway :: ActivateGatewayResponse -> TestTree
responseActivateGateway =
  res
    "ActivateGatewayResponse"
    "fixture/ActivateGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy ActivateGateway)

responseListVolumeRecoveryPoints :: ListVolumeRecoveryPointsResponse -> TestTree
responseListVolumeRecoveryPoints =
  res
    "ListVolumeRecoveryPointsResponse"
    "fixture/ListVolumeRecoveryPointsResponse.proto"
    defaultService
    (Proxy :: Proxy ListVolumeRecoveryPoints)

responseDescribeAvailabilityMonitorTest :: DescribeAvailabilityMonitorTestResponse -> TestTree
responseDescribeAvailabilityMonitorTest =
  res
    "DescribeAvailabilityMonitorTestResponse"
    "fixture/DescribeAvailabilityMonitorTestResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAvailabilityMonitorTest)

responseUpdateSMBFileShare :: UpdateSMBFileShareResponse -> TestTree
responseUpdateSMBFileShare =
  res
    "UpdateSMBFileShareResponse"
    "fixture/UpdateSMBFileShareResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSMBFileShare)

responseDescribeTapes :: DescribeTapesResponse -> TestTree
responseDescribeTapes =
  res
    "DescribeTapesResponse"
    "fixture/DescribeTapesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTapes)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTagsFromResource)

responseDeleteTapeArchive :: DeleteTapeArchiveResponse -> TestTree
responseDeleteTapeArchive =
  res
    "DeleteTapeArchiveResponse"
    "fixture/DeleteTapeArchiveResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTapeArchive)

responseAttachVolume :: AttachVolumeResponse -> TestTree
responseAttachVolume =
  res
    "AttachVolumeResponse"
    "fixture/AttachVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy AttachVolume)

responseCreateSMBFileShare :: CreateSMBFileShareResponse -> TestTree
responseCreateSMBFileShare =
  res
    "CreateSMBFileShareResponse"
    "fixture/CreateSMBFileShareResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSMBFileShare)

responseUpdateSMBSecurityStrategy :: UpdateSMBSecurityStrategyResponse -> TestTree
responseUpdateSMBSecurityStrategy =
  res
    "UpdateSMBSecurityStrategyResponse"
    "fixture/UpdateSMBSecurityStrategyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSMBSecurityStrategy)

responseStartGateway :: StartGatewayResponse -> TestTree
responseStartGateway =
  res
    "StartGatewayResponse"
    "fixture/StartGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy StartGateway)

responseUpdateGatewaySoftwareNow :: UpdateGatewaySoftwareNowResponse -> TestTree
responseUpdateGatewaySoftwareNow =
  res
    "UpdateGatewaySoftwareNowResponse"
    "fixture/UpdateGatewaySoftwareNowResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGatewaySoftwareNow)

responseCancelRetrieval :: CancelRetrievalResponse -> TestTree
responseCancelRetrieval =
  res
    "CancelRetrievalResponse"
    "fixture/CancelRetrievalResponse.proto"
    defaultService
    (Proxy :: Proxy CancelRetrieval)

responseCreateTapePool :: CreateTapePoolResponse -> TestTree
responseCreateTapePool =
  res
    "CreateTapePoolResponse"
    "fixture/CreateTapePoolResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTapePool)

responseListTapePools :: ListTapePoolsResponse -> TestTree
responseListTapePools =
  res
    "ListTapePoolsResponse"
    "fixture/ListTapePoolsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTapePools)

responseDescribeBandwidthRateLimit :: DescribeBandwidthRateLimitResponse -> TestTree
responseDescribeBandwidthRateLimit =
  res
    "DescribeBandwidthRateLimitResponse"
    "fixture/DescribeBandwidthRateLimitResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBandwidthRateLimit)

responseCreateTapeWithBarcode :: CreateTapeWithBarcodeResponse -> TestTree
responseCreateTapeWithBarcode =
  res
    "CreateTapeWithBarcodeResponse"
    "fixture/CreateTapeWithBarcodeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTapeWithBarcode)

responseJoinDomain :: JoinDomainResponse -> TestTree
responseJoinDomain =
  res
    "JoinDomainResponse"
    "fixture/JoinDomainResponse.proto"
    defaultService
    (Proxy :: Proxy JoinDomain)

responseListVolumeInitiators :: ListVolumeInitiatorsResponse -> TestTree
responseListVolumeInitiators =
  res
    "ListVolumeInitiatorsResponse"
    "fixture/ListVolumeInitiatorsResponse.proto"
    defaultService
    (Proxy :: Proxy ListVolumeInitiators)

responseListFileShares :: ListFileSharesResponse -> TestTree
responseListFileShares =
  res
    "ListFileSharesResponse"
    "fixture/ListFileSharesResponse.proto"
    defaultService
    (Proxy :: Proxy ListFileShares)

responseCreateTapes :: CreateTapesResponse -> TestTree
responseCreateTapes =
  res
    "CreateTapesResponse"
    "fixture/CreateTapesResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTapes)

responseUpdateVTLDeviceType :: UpdateVTLDeviceTypeResponse -> TestTree
responseUpdateVTLDeviceType =
  res
    "UpdateVTLDeviceTypeResponse"
    "fixture/UpdateVTLDeviceTypeResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateVTLDeviceType)

responseCreateCachediSCSIVolume :: CreateCachediSCSIVolumeResponse -> TestTree
responseCreateCachediSCSIVolume =
  res
    "CreateCachediSCSIVolumeResponse"
    "fixture/CreateCachediSCSIVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCachediSCSIVolume)

responseSetLocalConsolePassword :: SetLocalConsolePasswordResponse -> TestTree
responseSetLocalConsolePassword =
  res
    "SetLocalConsolePasswordResponse"
    "fixture/SetLocalConsolePasswordResponse.proto"
    defaultService
    (Proxy :: Proxy SetLocalConsolePassword)

responseAssociateFileSystem :: AssociateFileSystemResponse -> TestTree
responseAssociateFileSystem =
  res
    "AssociateFileSystemResponse"
    "fixture/AssociateFileSystemResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateFileSystem)

responseDescribeChapCredentials :: DescribeChapCredentialsResponse -> TestTree
responseDescribeChapCredentials =
  res
    "DescribeChapCredentialsResponse"
    "fixture/DescribeChapCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeChapCredentials)

responseUpdateBandwidthRateLimitSchedule :: UpdateBandwidthRateLimitScheduleResponse -> TestTree
responseUpdateBandwidthRateLimitSchedule =
  res
    "UpdateBandwidthRateLimitScheduleResponse"
    "fixture/UpdateBandwidthRateLimitScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBandwidthRateLimitSchedule)

responseDisableGateway :: DisableGatewayResponse -> TestTree
responseDisableGateway =
  res
    "DisableGatewayResponse"
    "fixture/DisableGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DisableGateway)

responseDescribeSnapshotSchedule :: DescribeSnapshotScheduleResponse -> TestTree
responseDescribeSnapshotSchedule =
  res
    "DescribeSnapshotScheduleResponse"
    "fixture/DescribeSnapshotScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSnapshotSchedule)

responseDescribeTapeArchives :: DescribeTapeArchivesResponse -> TestTree
responseDescribeTapeArchives =
  res
    "DescribeTapeArchivesResponse"
    "fixture/DescribeTapeArchivesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTapeArchives)

responseCreateStorediSCSIVolume :: CreateStorediSCSIVolumeResponse -> TestTree
responseCreateStorediSCSIVolume =
  res
    "CreateStorediSCSIVolumeResponse"
    "fixture/CreateStorediSCSIVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStorediSCSIVolume)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy :: Proxy AddTagsToResource)

responseDeleteGateway :: DeleteGatewayResponse -> TestTree
responseDeleteGateway =
  res
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGateway)

responseCreateSnapshotFromVolumeRecoveryPoint :: CreateSnapshotFromVolumeRecoveryPointResponse -> TestTree
responseCreateSnapshotFromVolumeRecoveryPoint =
  res
    "CreateSnapshotFromVolumeRecoveryPointResponse"
    "fixture/CreateSnapshotFromVolumeRecoveryPointResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSnapshotFromVolumeRecoveryPoint)

responseDeleteBandwidthRateLimit :: DeleteBandwidthRateLimitResponse -> TestTree
responseDeleteBandwidthRateLimit =
  res
    "DeleteBandwidthRateLimitResponse"
    "fixture/DeleteBandwidthRateLimitResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBandwidthRateLimit)

responseDescribeTapeRecoveryPoints :: DescribeTapeRecoveryPointsResponse -> TestTree
responseDescribeTapeRecoveryPoints =
  res
    "DescribeTapeRecoveryPointsResponse"
    "fixture/DescribeTapeRecoveryPointsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTapeRecoveryPoints)

responseSetSMBGuestPassword :: SetSMBGuestPasswordResponse -> TestTree
responseSetSMBGuestPassword =
  res
    "SetSMBGuestPasswordResponse"
    "fixture/SetSMBGuestPasswordResponse.proto"
    defaultService
    (Proxy :: Proxy SetSMBGuestPassword)

responseUpdateBandwidthRateLimit :: UpdateBandwidthRateLimitResponse -> TestTree
responseUpdateBandwidthRateLimit =
  res
    "UpdateBandwidthRateLimitResponse"
    "fixture/UpdateBandwidthRateLimitResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBandwidthRateLimit)

responseRetrieveTapeRecoveryPoint :: RetrieveTapeRecoveryPointResponse -> TestTree
responseRetrieveTapeRecoveryPoint =
  res
    "RetrieveTapeRecoveryPointResponse"
    "fixture/RetrieveTapeRecoveryPointResponse.proto"
    defaultService
    (Proxy :: Proxy RetrieveTapeRecoveryPoint)

responseListGateways :: ListGatewaysResponse -> TestTree
responseListGateways =
  res
    "ListGatewaysResponse"
    "fixture/ListGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy ListGateways)

responseListLocalDisks :: ListLocalDisksResponse -> TestTree
responseListLocalDisks =
  res
    "ListLocalDisksResponse"
    "fixture/ListLocalDisksResponse.proto"
    defaultService
    (Proxy :: Proxy ListLocalDisks)

responseDeleteVolume :: DeleteVolumeResponse -> TestTree
responseDeleteVolume =
  res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVolume)

responseDeleteTape :: DeleteTapeResponse -> TestTree
responseDeleteTape =
  res
    "DeleteTapeResponse"
    "fixture/DeleteTapeResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTape)

responseResetCache :: ResetCacheResponse -> TestTree
responseResetCache =
  res
    "ResetCacheResponse"
    "fixture/ResetCacheResponse.proto"
    defaultService
    (Proxy :: Proxy ResetCache)

responseDescribeUploadBuffer :: DescribeUploadBufferResponse -> TestTree
responseDescribeUploadBuffer =
  res
    "DescribeUploadBufferResponse"
    "fixture/DescribeUploadBufferResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUploadBuffer)

responseDescribeNFSFileShares :: DescribeNFSFileSharesResponse -> TestTree
responseDescribeNFSFileShares =
  res
    "DescribeNFSFileSharesResponse"
    "fixture/DescribeNFSFileSharesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNFSFileShares)

responseDescribeStorediSCSIVolumes :: DescribeStorediSCSIVolumesResponse -> TestTree
responseDescribeStorediSCSIVolumes =
  res
    "DescribeStorediSCSIVolumesResponse"
    "fixture/DescribeStorediSCSIVolumesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStorediSCSIVolumes)

responseDeleteChapCredentials :: DeleteChapCredentialsResponse -> TestTree
responseDeleteChapCredentials =
  res
    "DeleteChapCredentialsResponse"
    "fixture/DeleteChapCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteChapCredentials)

responseListFileSystemAssociations :: ListFileSystemAssociationsResponse -> TestTree
responseListFileSystemAssociations =
  res
    "ListFileSystemAssociationsResponse"
    "fixture/ListFileSystemAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFileSystemAssociations)

responseUpdateFileSystemAssociation :: UpdateFileSystemAssociationResponse -> TestTree
responseUpdateFileSystemAssociation =
  res
    "UpdateFileSystemAssociationResponse"
    "fixture/UpdateFileSystemAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFileSystemAssociation)

responseUpdateChapCredentials :: UpdateChapCredentialsResponse -> TestTree
responseUpdateChapCredentials =
  res
    "UpdateChapCredentialsResponse"
    "fixture/UpdateChapCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateChapCredentials)

responseStartAvailabilityMonitorTest :: StartAvailabilityMonitorTestResponse -> TestTree
responseStartAvailabilityMonitorTest =
  res
    "StartAvailabilityMonitorTestResponse"
    "fixture/StartAvailabilityMonitorTestResponse.proto"
    defaultService
    (Proxy :: Proxy StartAvailabilityMonitorTest)

responseUpdateSnapshotSchedule :: UpdateSnapshotScheduleResponse -> TestTree
responseUpdateSnapshotSchedule =
  res
    "UpdateSnapshotScheduleResponse"
    "fixture/UpdateSnapshotScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSnapshotSchedule)

responseDescribeVTLDevices :: DescribeVTLDevicesResponse -> TestTree
responseDescribeVTLDevices =
  res
    "DescribeVTLDevicesResponse"
    "fixture/DescribeVTLDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVTLDevices)

responseDeleteSnapshotSchedule :: DeleteSnapshotScheduleResponse -> TestTree
responseDeleteSnapshotSchedule =
  res
    "DeleteSnapshotScheduleResponse"
    "fixture/DeleteSnapshotScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSnapshotSchedule)

responseListAutomaticTapeCreationPolicies :: ListAutomaticTapeCreationPoliciesResponse -> TestTree
responseListAutomaticTapeCreationPolicies =
  res
    "ListAutomaticTapeCreationPoliciesResponse"
    "fixture/ListAutomaticTapeCreationPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAutomaticTapeCreationPolicies)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSnapshot)

responseShutdownGateway :: ShutdownGatewayResponse -> TestTree
responseShutdownGateway =
  res
    "ShutdownGatewayResponse"
    "fixture/ShutdownGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy ShutdownGateway)

responseUpdateSMBFileShareVisibility :: UpdateSMBFileShareVisibilityResponse -> TestTree
responseUpdateSMBFileShareVisibility =
  res
    "UpdateSMBFileShareVisibilityResponse"
    "fixture/UpdateSMBFileShareVisibilityResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSMBFileShareVisibility)

responseDescribeBandwidthRateLimitSchedule :: DescribeBandwidthRateLimitScheduleResponse -> TestTree
responseDescribeBandwidthRateLimitSchedule =
  res
    "DescribeBandwidthRateLimitScheduleResponse"
    "fixture/DescribeBandwidthRateLimitScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBandwidthRateLimitSchedule)

responseAssignTapePool :: AssignTapePoolResponse -> TestTree
responseAssignTapePool =
  res
    "AssignTapePoolResponse"
    "fixture/AssignTapePoolResponse.proto"
    defaultService
    (Proxy :: Proxy AssignTapePool)

responseDescribeCachediSCSIVolumes :: DescribeCachediSCSIVolumesResponse -> TestTree
responseDescribeCachediSCSIVolumes =
  res
    "DescribeCachediSCSIVolumesResponse"
    "fixture/DescribeCachediSCSIVolumesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCachediSCSIVolumes)

responseNotifyWhenUploaded :: NotifyWhenUploadedResponse -> TestTree
responseNotifyWhenUploaded =
  res
    "NotifyWhenUploadedResponse"
    "fixture/NotifyWhenUploadedResponse.proto"
    defaultService
    (Proxy :: Proxy NotifyWhenUploaded)

responseDescribeMaintenanceStartTime :: DescribeMaintenanceStartTimeResponse -> TestTree
responseDescribeMaintenanceStartTime =
  res
    "DescribeMaintenanceStartTimeResponse"
    "fixture/DescribeMaintenanceStartTimeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMaintenanceStartTime)

responseUpdateGatewayInformation :: UpdateGatewayInformationResponse -> TestTree
responseUpdateGatewayInformation =
  res
    "UpdateGatewayInformationResponse"
    "fixture/UpdateGatewayInformationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGatewayInformation)

responseDeleteAutomaticTapeCreationPolicy :: DeleteAutomaticTapeCreationPolicyResponse -> TestTree
responseDeleteAutomaticTapeCreationPolicy =
  res
    "DeleteAutomaticTapeCreationPolicyResponse"
    "fixture/DeleteAutomaticTapeCreationPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAutomaticTapeCreationPolicy)

responseDescribeWorkingStorage :: DescribeWorkingStorageResponse -> TestTree
responseDescribeWorkingStorage =
  res
    "DescribeWorkingStorageResponse"
    "fixture/DescribeWorkingStorageResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkingStorage)

responseDeleteTapePool :: DeleteTapePoolResponse -> TestTree
responseDeleteTapePool =
  res
    "DeleteTapePoolResponse"
    "fixture/DeleteTapePoolResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTapePool)

responseUpdateAutomaticTapeCreationPolicy :: UpdateAutomaticTapeCreationPolicyResponse -> TestTree
responseUpdateAutomaticTapeCreationPolicy =
  res
    "UpdateAutomaticTapeCreationPolicyResponse"
    "fixture/UpdateAutomaticTapeCreationPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAutomaticTapeCreationPolicy)

responseAddCache :: AddCacheResponse -> TestTree
responseAddCache =
  res
    "AddCacheResponse"
    "fixture/AddCacheResponse.proto"
    defaultService
    (Proxy :: Proxy AddCache)

responseAddUploadBuffer :: AddUploadBufferResponse -> TestTree
responseAddUploadBuffer =
  res
    "AddUploadBufferResponse"
    "fixture/AddUploadBufferResponse.proto"
    defaultService
    (Proxy :: Proxy AddUploadBuffer)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDeleteFileShare :: DeleteFileShareResponse -> TestTree
responseDeleteFileShare =
  res
    "DeleteFileShareResponse"
    "fixture/DeleteFileShareResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFileShare)
