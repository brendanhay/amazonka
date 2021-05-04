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
--         , requestCreateNFSFileShare $
--             newCreateNFSFileShare
--
--         , requestRetrieveTapeArchive $
--             newRetrieveTapeArchive
--
--         , requestCancelArchival $
--             newCancelArchival
--
--         , requestDescribeSMBSettings $
--             newDescribeSMBSettings
--
--         , requestAddWorkingStorage $
--             newAddWorkingStorage
--
--         , requestListTapes $
--             newListTapes
--
--         , requestListVolumes $
--             newListVolumes
--
--         , requestDescribeSMBFileShares $
--             newDescribeSMBFileShares
--
--         , requestDescribeCache $
--             newDescribeCache
--
--         , requestUpdateMaintenanceStartTime $
--             newUpdateMaintenanceStartTime
--
--         , requestDescribeGatewayInformation $
--             newDescribeGatewayInformation
--
--         , requestActivateGateway $
--             newActivateGateway
--
--         , requestRefreshCache $
--             newRefreshCache
--
--         , requestUpdateNFSFileShare $
--             newUpdateNFSFileShare
--
--         , requestDescribeTapes $
--             newDescribeTapes
--
--         , requestListVolumeRecoveryPoints $
--             newListVolumeRecoveryPoints
--
--         , requestUpdateSMBFileShare $
--             newUpdateSMBFileShare
--
--         , requestDescribeAvailabilityMonitorTest $
--             newDescribeAvailabilityMonitorTest
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
--         , requestUpdateGatewaySoftwareNow $
--             newUpdateGatewaySoftwareNow
--
--         , requestStartGateway $
--             newStartGateway
--
--         , requestUpdateSMBSecurityStrategy $
--             newUpdateSMBSecurityStrategy
--
--         , requestCreateTapePool $
--             newCreateTapePool
--
--         , requestCancelRetrieval $
--             newCancelRetrieval
--
--         , requestDescribeBandwidthRateLimit $
--             newDescribeBandwidthRateLimit
--
--         , requestListTapePools $
--             newListTapePools
--
--         , requestJoinDomain $
--             newJoinDomain
--
--         , requestListFileShares $
--             newListFileShares
--
--         , requestListVolumeInitiators $
--             newListVolumeInitiators
--
--         , requestCreateTapeWithBarcode $
--             newCreateTapeWithBarcode
--
--         , requestSetLocalConsolePassword $
--             newSetLocalConsolePassword
--
--         , requestDescribeChapCredentials $
--             newDescribeChapCredentials
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
--         , requestDescribeSnapshotSchedule $
--             newDescribeSnapshotSchedule
--
--         , requestUpdateBandwidthRateLimitSchedule $
--             newUpdateBandwidthRateLimitSchedule
--
--         , requestDisableGateway $
--             newDisableGateway
--
--         , requestCreateStorediSCSIVolume $
--             newCreateStorediSCSIVolume
--
--         , requestDescribeTapeArchives $
--             newDescribeTapeArchives
--
--         , requestDeleteTape $
--             newDeleteTape
--
--         , requestResetCache $
--             newResetCache
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestSetSMBGuestPassword $
--             newSetSMBGuestPassword
--
--         , requestDeleteGateway $
--             newDeleteGateway
--
--         , requestCreateSnapshotFromVolumeRecoveryPoint $
--             newCreateSnapshotFromVolumeRecoveryPoint
--
--         , requestUpdateBandwidthRateLimit $
--             newUpdateBandwidthRateLimit
--
--         , requestDescribeTapeRecoveryPoints $
--             newDescribeTapeRecoveryPoints
--
--         , requestDeleteVolume $
--             newDeleteVolume
--
--         , requestListLocalDisks $
--             newListLocalDisks
--
--         , requestDeleteBandwidthRateLimit $
--             newDeleteBandwidthRateLimit
--
--         , requestRetrieveTapeRecoveryPoint $
--             newRetrieveTapeRecoveryPoint
--
--         , requestListGateways $
--             newListGateways
--
--         , requestDescribeNFSFileShares $
--             newDescribeNFSFileShares
--
--         , requestDescribeStorediSCSIVolumes $
--             newDescribeStorediSCSIVolumes
--
--         , requestDescribeUploadBuffer $
--             newDescribeUploadBuffer
--
--         , requestStartAvailabilityMonitorTest $
--             newStartAvailabilityMonitorTest
--
--         , requestDeleteChapCredentials $
--             newDeleteChapCredentials
--
--         , requestUpdateChapCredentials $
--             newUpdateChapCredentials
--
--         , requestDeleteSnapshotSchedule $
--             newDeleteSnapshotSchedule
--
--         , requestShutdownGateway $
--             newShutdownGateway
--
--         , requestDescribeBandwidthRateLimitSchedule $
--             newDescribeBandwidthRateLimitSchedule
--
--         , requestDescribeVTLDevices $
--             newDescribeVTLDevices
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestUpdateSnapshotSchedule $
--             newUpdateSnapshotSchedule
--
--         , requestUpdateSMBFileShareVisibility $
--             newUpdateSMBFileShareVisibility
--
--         , requestListAutomaticTapeCreationPolicies $
--             newListAutomaticTapeCreationPolicies
--
--         , requestAddCache $
--             newAddCache
--
--         , requestNotifyWhenUploaded $
--             newNotifyWhenUploaded
--
--         , requestDescribeCachediSCSIVolumes $
--             newDescribeCachediSCSIVolumes
--
--         , requestDeleteAutomaticTapeCreationPolicy $
--             newDeleteAutomaticTapeCreationPolicy
--
--         , requestUpdateAutomaticTapeCreationPolicy $
--             newUpdateAutomaticTapeCreationPolicy
--
--         , requestDescribeWorkingStorage $
--             newDescribeWorkingStorage
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
--         , requestDeleteTapePool $
--             newDeleteTapePool
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDeleteFileShare $
--             newDeleteFileShare
--
--         , requestAddUploadBuffer $
--             newAddUploadBuffer
--
--           ]

--     , testGroup "response"
--         [ responseDetachVolume $
--             newDetachVolumeResponse
--
--         , responseCreateNFSFileShare $
--             newCreateNFSFileShareResponse
--
--         , responseRetrieveTapeArchive $
--             newRetrieveTapeArchiveResponse
--
--         , responseCancelArchival $
--             newCancelArchivalResponse
--
--         , responseDescribeSMBSettings $
--             newDescribeSMBSettingsResponse
--
--         , responseAddWorkingStorage $
--             newAddWorkingStorageResponse
--
--         , responseListTapes $
--             newListTapesResponse
--
--         , responseListVolumes $
--             newListVolumesResponse
--
--         , responseDescribeSMBFileShares $
--             newDescribeSMBFileSharesResponse
--
--         , responseDescribeCache $
--             newDescribeCacheResponse
--
--         , responseUpdateMaintenanceStartTime $
--             newUpdateMaintenanceStartTimeResponse
--
--         , responseDescribeGatewayInformation $
--             newDescribeGatewayInformationResponse
--
--         , responseActivateGateway $
--             newActivateGatewayResponse
--
--         , responseRefreshCache $
--             newRefreshCacheResponse
--
--         , responseUpdateNFSFileShare $
--             newUpdateNFSFileShareResponse
--
--         , responseDescribeTapes $
--             newDescribeTapesResponse
--
--         , responseListVolumeRecoveryPoints $
--             newListVolumeRecoveryPointsResponse
--
--         , responseUpdateSMBFileShare $
--             newUpdateSMBFileShareResponse
--
--         , responseDescribeAvailabilityMonitorTest $
--             newDescribeAvailabilityMonitorTestResponse
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
--         , responseUpdateGatewaySoftwareNow $
--             newUpdateGatewaySoftwareNowResponse
--
--         , responseStartGateway $
--             newStartGatewayResponse
--
--         , responseUpdateSMBSecurityStrategy $
--             newUpdateSMBSecurityStrategyResponse
--
--         , responseCreateTapePool $
--             newCreateTapePoolResponse
--
--         , responseCancelRetrieval $
--             newCancelRetrievalResponse
--
--         , responseDescribeBandwidthRateLimit $
--             newDescribeBandwidthRateLimitResponse
--
--         , responseListTapePools $
--             newListTapePoolsResponse
--
--         , responseJoinDomain $
--             newJoinDomainResponse
--
--         , responseListFileShares $
--             newListFileSharesResponse
--
--         , responseListVolumeInitiators $
--             newListVolumeInitiatorsResponse
--
--         , responseCreateTapeWithBarcode $
--             newCreateTapeWithBarcodeResponse
--
--         , responseSetLocalConsolePassword $
--             newSetLocalConsolePasswordResponse
--
--         , responseDescribeChapCredentials $
--             newDescribeChapCredentialsResponse
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
--         , responseDescribeSnapshotSchedule $
--             newDescribeSnapshotScheduleResponse
--
--         , responseUpdateBandwidthRateLimitSchedule $
--             newUpdateBandwidthRateLimitScheduleResponse
--
--         , responseDisableGateway $
--             newDisableGatewayResponse
--
--         , responseCreateStorediSCSIVolume $
--             newCreateStorediSCSIVolumeResponse
--
--         , responseDescribeTapeArchives $
--             newDescribeTapeArchivesResponse
--
--         , responseDeleteTape $
--             newDeleteTapeResponse
--
--         , responseResetCache $
--             newResetCacheResponse
--
--         , responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseSetSMBGuestPassword $
--             newSetSMBGuestPasswordResponse
--
--         , responseDeleteGateway $
--             newDeleteGatewayResponse
--
--         , responseCreateSnapshotFromVolumeRecoveryPoint $
--             newCreateSnapshotFromVolumeRecoveryPointResponse
--
--         , responseUpdateBandwidthRateLimit $
--             newUpdateBandwidthRateLimitResponse
--
--         , responseDescribeTapeRecoveryPoints $
--             newDescribeTapeRecoveryPointsResponse
--
--         , responseDeleteVolume $
--             newDeleteVolumeResponse
--
--         , responseListLocalDisks $
--             newListLocalDisksResponse
--
--         , responseDeleteBandwidthRateLimit $
--             newDeleteBandwidthRateLimitResponse
--
--         , responseRetrieveTapeRecoveryPoint $
--             newRetrieveTapeRecoveryPointResponse
--
--         , responseListGateways $
--             newListGatewaysResponse
--
--         , responseDescribeNFSFileShares $
--             newDescribeNFSFileSharesResponse
--
--         , responseDescribeStorediSCSIVolumes $
--             newDescribeStorediSCSIVolumesResponse
--
--         , responseDescribeUploadBuffer $
--             newDescribeUploadBufferResponse
--
--         , responseStartAvailabilityMonitorTest $
--             newStartAvailabilityMonitorTestResponse
--
--         , responseDeleteChapCredentials $
--             newDeleteChapCredentialsResponse
--
--         , responseUpdateChapCredentials $
--             newUpdateChapCredentialsResponse
--
--         , responseDeleteSnapshotSchedule $
--             newDeleteSnapshotScheduleResponse
--
--         , responseShutdownGateway $
--             newShutdownGatewayResponse
--
--         , responseDescribeBandwidthRateLimitSchedule $
--             newDescribeBandwidthRateLimitScheduleResponse
--
--         , responseDescribeVTLDevices $
--             newDescribeVTLDevicesResponse
--
--         , responseCreateSnapshot $
--             newCreateSnapshotResponse
--
--         , responseUpdateSnapshotSchedule $
--             newUpdateSnapshotScheduleResponse
--
--         , responseUpdateSMBFileShareVisibility $
--             newUpdateSMBFileShareVisibilityResponse
--
--         , responseListAutomaticTapeCreationPolicies $
--             newListAutomaticTapeCreationPoliciesResponse
--
--         , responseAddCache $
--             newAddCacheResponse
--
--         , responseNotifyWhenUploaded $
--             newNotifyWhenUploadedResponse
--
--         , responseDescribeCachediSCSIVolumes $
--             newDescribeCachediSCSIVolumesResponse
--
--         , responseDeleteAutomaticTapeCreationPolicy $
--             newDeleteAutomaticTapeCreationPolicyResponse
--
--         , responseUpdateAutomaticTapeCreationPolicy $
--             newUpdateAutomaticTapeCreationPolicyResponse
--
--         , responseDescribeWorkingStorage $
--             newDescribeWorkingStorageResponse
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
--         , responseDeleteTapePool $
--             newDeleteTapePoolResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDeleteFileShare $
--             newDeleteFileShareResponse
--
--         , responseAddUploadBuffer $
--             newAddUploadBufferResponse
--
--           ]
--     ]

-- Requests

requestDetachVolume :: DetachVolume -> TestTree
requestDetachVolume =
  req
    "DetachVolume"
    "fixture/DetachVolume.yaml"

requestCreateNFSFileShare :: CreateNFSFileShare -> TestTree
requestCreateNFSFileShare =
  req
    "CreateNFSFileShare"
    "fixture/CreateNFSFileShare.yaml"

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

requestDescribeSMBSettings :: DescribeSMBSettings -> TestTree
requestDescribeSMBSettings =
  req
    "DescribeSMBSettings"
    "fixture/DescribeSMBSettings.yaml"

requestAddWorkingStorage :: AddWorkingStorage -> TestTree
requestAddWorkingStorage =
  req
    "AddWorkingStorage"
    "fixture/AddWorkingStorage.yaml"

requestListTapes :: ListTapes -> TestTree
requestListTapes =
  req
    "ListTapes"
    "fixture/ListTapes.yaml"

requestListVolumes :: ListVolumes -> TestTree
requestListVolumes =
  req
    "ListVolumes"
    "fixture/ListVolumes.yaml"

requestDescribeSMBFileShares :: DescribeSMBFileShares -> TestTree
requestDescribeSMBFileShares =
  req
    "DescribeSMBFileShares"
    "fixture/DescribeSMBFileShares.yaml"

requestDescribeCache :: DescribeCache -> TestTree
requestDescribeCache =
  req
    "DescribeCache"
    "fixture/DescribeCache.yaml"

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

requestActivateGateway :: ActivateGateway -> TestTree
requestActivateGateway =
  req
    "ActivateGateway"
    "fixture/ActivateGateway.yaml"

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

requestDescribeTapes :: DescribeTapes -> TestTree
requestDescribeTapes =
  req
    "DescribeTapes"
    "fixture/DescribeTapes.yaml"

requestListVolumeRecoveryPoints :: ListVolumeRecoveryPoints -> TestTree
requestListVolumeRecoveryPoints =
  req
    "ListVolumeRecoveryPoints"
    "fixture/ListVolumeRecoveryPoints.yaml"

requestUpdateSMBFileShare :: UpdateSMBFileShare -> TestTree
requestUpdateSMBFileShare =
  req
    "UpdateSMBFileShare"
    "fixture/UpdateSMBFileShare.yaml"

requestDescribeAvailabilityMonitorTest :: DescribeAvailabilityMonitorTest -> TestTree
requestDescribeAvailabilityMonitorTest =
  req
    "DescribeAvailabilityMonitorTest"
    "fixture/DescribeAvailabilityMonitorTest.yaml"

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

requestUpdateGatewaySoftwareNow :: UpdateGatewaySoftwareNow -> TestTree
requestUpdateGatewaySoftwareNow =
  req
    "UpdateGatewaySoftwareNow"
    "fixture/UpdateGatewaySoftwareNow.yaml"

requestStartGateway :: StartGateway -> TestTree
requestStartGateway =
  req
    "StartGateway"
    "fixture/StartGateway.yaml"

requestUpdateSMBSecurityStrategy :: UpdateSMBSecurityStrategy -> TestTree
requestUpdateSMBSecurityStrategy =
  req
    "UpdateSMBSecurityStrategy"
    "fixture/UpdateSMBSecurityStrategy.yaml"

requestCreateTapePool :: CreateTapePool -> TestTree
requestCreateTapePool =
  req
    "CreateTapePool"
    "fixture/CreateTapePool.yaml"

requestCancelRetrieval :: CancelRetrieval -> TestTree
requestCancelRetrieval =
  req
    "CancelRetrieval"
    "fixture/CancelRetrieval.yaml"

requestDescribeBandwidthRateLimit :: DescribeBandwidthRateLimit -> TestTree
requestDescribeBandwidthRateLimit =
  req
    "DescribeBandwidthRateLimit"
    "fixture/DescribeBandwidthRateLimit.yaml"

requestListTapePools :: ListTapePools -> TestTree
requestListTapePools =
  req
    "ListTapePools"
    "fixture/ListTapePools.yaml"

requestJoinDomain :: JoinDomain -> TestTree
requestJoinDomain =
  req
    "JoinDomain"
    "fixture/JoinDomain.yaml"

requestListFileShares :: ListFileShares -> TestTree
requestListFileShares =
  req
    "ListFileShares"
    "fixture/ListFileShares.yaml"

requestListVolumeInitiators :: ListVolumeInitiators -> TestTree
requestListVolumeInitiators =
  req
    "ListVolumeInitiators"
    "fixture/ListVolumeInitiators.yaml"

requestCreateTapeWithBarcode :: CreateTapeWithBarcode -> TestTree
requestCreateTapeWithBarcode =
  req
    "CreateTapeWithBarcode"
    "fixture/CreateTapeWithBarcode.yaml"

requestSetLocalConsolePassword :: SetLocalConsolePassword -> TestTree
requestSetLocalConsolePassword =
  req
    "SetLocalConsolePassword"
    "fixture/SetLocalConsolePassword.yaml"

requestDescribeChapCredentials :: DescribeChapCredentials -> TestTree
requestDescribeChapCredentials =
  req
    "DescribeChapCredentials"
    "fixture/DescribeChapCredentials.yaml"

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

requestDescribeSnapshotSchedule :: DescribeSnapshotSchedule -> TestTree
requestDescribeSnapshotSchedule =
  req
    "DescribeSnapshotSchedule"
    "fixture/DescribeSnapshotSchedule.yaml"

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

requestCreateStorediSCSIVolume :: CreateStorediSCSIVolume -> TestTree
requestCreateStorediSCSIVolume =
  req
    "CreateStorediSCSIVolume"
    "fixture/CreateStorediSCSIVolume.yaml"

requestDescribeTapeArchives :: DescribeTapeArchives -> TestTree
requestDescribeTapeArchives =
  req
    "DescribeTapeArchives"
    "fixture/DescribeTapeArchives.yaml"

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

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestSetSMBGuestPassword :: SetSMBGuestPassword -> TestTree
requestSetSMBGuestPassword =
  req
    "SetSMBGuestPassword"
    "fixture/SetSMBGuestPassword.yaml"

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

requestUpdateBandwidthRateLimit :: UpdateBandwidthRateLimit -> TestTree
requestUpdateBandwidthRateLimit =
  req
    "UpdateBandwidthRateLimit"
    "fixture/UpdateBandwidthRateLimit.yaml"

requestDescribeTapeRecoveryPoints :: DescribeTapeRecoveryPoints -> TestTree
requestDescribeTapeRecoveryPoints =
  req
    "DescribeTapeRecoveryPoints"
    "fixture/DescribeTapeRecoveryPoints.yaml"

requestDeleteVolume :: DeleteVolume -> TestTree
requestDeleteVolume =
  req
    "DeleteVolume"
    "fixture/DeleteVolume.yaml"

requestListLocalDisks :: ListLocalDisks -> TestTree
requestListLocalDisks =
  req
    "ListLocalDisks"
    "fixture/ListLocalDisks.yaml"

requestDeleteBandwidthRateLimit :: DeleteBandwidthRateLimit -> TestTree
requestDeleteBandwidthRateLimit =
  req
    "DeleteBandwidthRateLimit"
    "fixture/DeleteBandwidthRateLimit.yaml"

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

requestDescribeUploadBuffer :: DescribeUploadBuffer -> TestTree
requestDescribeUploadBuffer =
  req
    "DescribeUploadBuffer"
    "fixture/DescribeUploadBuffer.yaml"

requestStartAvailabilityMonitorTest :: StartAvailabilityMonitorTest -> TestTree
requestStartAvailabilityMonitorTest =
  req
    "StartAvailabilityMonitorTest"
    "fixture/StartAvailabilityMonitorTest.yaml"

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

requestDeleteSnapshotSchedule :: DeleteSnapshotSchedule -> TestTree
requestDeleteSnapshotSchedule =
  req
    "DeleteSnapshotSchedule"
    "fixture/DeleteSnapshotSchedule.yaml"

requestShutdownGateway :: ShutdownGateway -> TestTree
requestShutdownGateway =
  req
    "ShutdownGateway"
    "fixture/ShutdownGateway.yaml"

requestDescribeBandwidthRateLimitSchedule :: DescribeBandwidthRateLimitSchedule -> TestTree
requestDescribeBandwidthRateLimitSchedule =
  req
    "DescribeBandwidthRateLimitSchedule"
    "fixture/DescribeBandwidthRateLimitSchedule.yaml"

requestDescribeVTLDevices :: DescribeVTLDevices -> TestTree
requestDescribeVTLDevices =
  req
    "DescribeVTLDevices"
    "fixture/DescribeVTLDevices.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestUpdateSnapshotSchedule :: UpdateSnapshotSchedule -> TestTree
requestUpdateSnapshotSchedule =
  req
    "UpdateSnapshotSchedule"
    "fixture/UpdateSnapshotSchedule.yaml"

requestUpdateSMBFileShareVisibility :: UpdateSMBFileShareVisibility -> TestTree
requestUpdateSMBFileShareVisibility =
  req
    "UpdateSMBFileShareVisibility"
    "fixture/UpdateSMBFileShareVisibility.yaml"

requestListAutomaticTapeCreationPolicies :: ListAutomaticTapeCreationPolicies -> TestTree
requestListAutomaticTapeCreationPolicies =
  req
    "ListAutomaticTapeCreationPolicies"
    "fixture/ListAutomaticTapeCreationPolicies.yaml"

requestAddCache :: AddCache -> TestTree
requestAddCache =
  req
    "AddCache"
    "fixture/AddCache.yaml"

requestNotifyWhenUploaded :: NotifyWhenUploaded -> TestTree
requestNotifyWhenUploaded =
  req
    "NotifyWhenUploaded"
    "fixture/NotifyWhenUploaded.yaml"

requestDescribeCachediSCSIVolumes :: DescribeCachediSCSIVolumes -> TestTree
requestDescribeCachediSCSIVolumes =
  req
    "DescribeCachediSCSIVolumes"
    "fixture/DescribeCachediSCSIVolumes.yaml"

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

requestDescribeWorkingStorage :: DescribeWorkingStorage -> TestTree
requestDescribeWorkingStorage =
  req
    "DescribeWorkingStorage"
    "fixture/DescribeWorkingStorage.yaml"

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

requestDeleteTapePool :: DeleteTapePool -> TestTree
requestDeleteTapePool =
  req
    "DeleteTapePool"
    "fixture/DeleteTapePool.yaml"

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

requestAddUploadBuffer :: AddUploadBuffer -> TestTree
requestAddUploadBuffer =
  req
    "AddUploadBuffer"
    "fixture/AddUploadBuffer.yaml"

-- Responses

responseDetachVolume :: DetachVolumeResponse -> TestTree
responseDetachVolume =
  res
    "DetachVolumeResponse"
    "fixture/DetachVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy DetachVolume)

responseCreateNFSFileShare :: CreateNFSFileShareResponse -> TestTree
responseCreateNFSFileShare =
  res
    "CreateNFSFileShareResponse"
    "fixture/CreateNFSFileShareResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNFSFileShare)

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

responseDescribeSMBSettings :: DescribeSMBSettingsResponse -> TestTree
responseDescribeSMBSettings =
  res
    "DescribeSMBSettingsResponse"
    "fixture/DescribeSMBSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSMBSettings)

responseAddWorkingStorage :: AddWorkingStorageResponse -> TestTree
responseAddWorkingStorage =
  res
    "AddWorkingStorageResponse"
    "fixture/AddWorkingStorageResponse.proto"
    defaultService
    (Proxy :: Proxy AddWorkingStorage)

responseListTapes :: ListTapesResponse -> TestTree
responseListTapes =
  res
    "ListTapesResponse"
    "fixture/ListTapesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTapes)

responseListVolumes :: ListVolumesResponse -> TestTree
responseListVolumes =
  res
    "ListVolumesResponse"
    "fixture/ListVolumesResponse.proto"
    defaultService
    (Proxy :: Proxy ListVolumes)

responseDescribeSMBFileShares :: DescribeSMBFileSharesResponse -> TestTree
responseDescribeSMBFileShares =
  res
    "DescribeSMBFileSharesResponse"
    "fixture/DescribeSMBFileSharesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSMBFileShares)

responseDescribeCache :: DescribeCacheResponse -> TestTree
responseDescribeCache =
  res
    "DescribeCacheResponse"
    "fixture/DescribeCacheResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCache)

responseUpdateMaintenanceStartTime :: UpdateMaintenanceStartTimeResponse -> TestTree
responseUpdateMaintenanceStartTime =
  res
    "UpdateMaintenanceStartTimeResponse"
    "fixture/UpdateMaintenanceStartTimeResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMaintenanceStartTime)

responseDescribeGatewayInformation :: DescribeGatewayInformationResponse -> TestTree
responseDescribeGatewayInformation =
  res
    "DescribeGatewayInformationResponse"
    "fixture/DescribeGatewayInformationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGatewayInformation)

responseActivateGateway :: ActivateGatewayResponse -> TestTree
responseActivateGateway =
  res
    "ActivateGatewayResponse"
    "fixture/ActivateGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy ActivateGateway)

responseRefreshCache :: RefreshCacheResponse -> TestTree
responseRefreshCache =
  res
    "RefreshCacheResponse"
    "fixture/RefreshCacheResponse.proto"
    defaultService
    (Proxy :: Proxy RefreshCache)

responseUpdateNFSFileShare :: UpdateNFSFileShareResponse -> TestTree
responseUpdateNFSFileShare =
  res
    "UpdateNFSFileShareResponse"
    "fixture/UpdateNFSFileShareResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNFSFileShare)

responseDescribeTapes :: DescribeTapesResponse -> TestTree
responseDescribeTapes =
  res
    "DescribeTapesResponse"
    "fixture/DescribeTapesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTapes)

responseListVolumeRecoveryPoints :: ListVolumeRecoveryPointsResponse -> TestTree
responseListVolumeRecoveryPoints =
  res
    "ListVolumeRecoveryPointsResponse"
    "fixture/ListVolumeRecoveryPointsResponse.proto"
    defaultService
    (Proxy :: Proxy ListVolumeRecoveryPoints)

responseUpdateSMBFileShare :: UpdateSMBFileShareResponse -> TestTree
responseUpdateSMBFileShare =
  res
    "UpdateSMBFileShareResponse"
    "fixture/UpdateSMBFileShareResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSMBFileShare)

responseDescribeAvailabilityMonitorTest :: DescribeAvailabilityMonitorTestResponse -> TestTree
responseDescribeAvailabilityMonitorTest =
  res
    "DescribeAvailabilityMonitorTestResponse"
    "fixture/DescribeAvailabilityMonitorTestResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAvailabilityMonitorTest)

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

responseUpdateGatewaySoftwareNow :: UpdateGatewaySoftwareNowResponse -> TestTree
responseUpdateGatewaySoftwareNow =
  res
    "UpdateGatewaySoftwareNowResponse"
    "fixture/UpdateGatewaySoftwareNowResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGatewaySoftwareNow)

responseStartGateway :: StartGatewayResponse -> TestTree
responseStartGateway =
  res
    "StartGatewayResponse"
    "fixture/StartGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy StartGateway)

responseUpdateSMBSecurityStrategy :: UpdateSMBSecurityStrategyResponse -> TestTree
responseUpdateSMBSecurityStrategy =
  res
    "UpdateSMBSecurityStrategyResponse"
    "fixture/UpdateSMBSecurityStrategyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSMBSecurityStrategy)

responseCreateTapePool :: CreateTapePoolResponse -> TestTree
responseCreateTapePool =
  res
    "CreateTapePoolResponse"
    "fixture/CreateTapePoolResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTapePool)

responseCancelRetrieval :: CancelRetrievalResponse -> TestTree
responseCancelRetrieval =
  res
    "CancelRetrievalResponse"
    "fixture/CancelRetrievalResponse.proto"
    defaultService
    (Proxy :: Proxy CancelRetrieval)

responseDescribeBandwidthRateLimit :: DescribeBandwidthRateLimitResponse -> TestTree
responseDescribeBandwidthRateLimit =
  res
    "DescribeBandwidthRateLimitResponse"
    "fixture/DescribeBandwidthRateLimitResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBandwidthRateLimit)

responseListTapePools :: ListTapePoolsResponse -> TestTree
responseListTapePools =
  res
    "ListTapePoolsResponse"
    "fixture/ListTapePoolsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTapePools)

responseJoinDomain :: JoinDomainResponse -> TestTree
responseJoinDomain =
  res
    "JoinDomainResponse"
    "fixture/JoinDomainResponse.proto"
    defaultService
    (Proxy :: Proxy JoinDomain)

responseListFileShares :: ListFileSharesResponse -> TestTree
responseListFileShares =
  res
    "ListFileSharesResponse"
    "fixture/ListFileSharesResponse.proto"
    defaultService
    (Proxy :: Proxy ListFileShares)

responseListVolumeInitiators :: ListVolumeInitiatorsResponse -> TestTree
responseListVolumeInitiators =
  res
    "ListVolumeInitiatorsResponse"
    "fixture/ListVolumeInitiatorsResponse.proto"
    defaultService
    (Proxy :: Proxy ListVolumeInitiators)

responseCreateTapeWithBarcode :: CreateTapeWithBarcodeResponse -> TestTree
responseCreateTapeWithBarcode =
  res
    "CreateTapeWithBarcodeResponse"
    "fixture/CreateTapeWithBarcodeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTapeWithBarcode)

responseSetLocalConsolePassword :: SetLocalConsolePasswordResponse -> TestTree
responseSetLocalConsolePassword =
  res
    "SetLocalConsolePasswordResponse"
    "fixture/SetLocalConsolePasswordResponse.proto"
    defaultService
    (Proxy :: Proxy SetLocalConsolePassword)

responseDescribeChapCredentials :: DescribeChapCredentialsResponse -> TestTree
responseDescribeChapCredentials =
  res
    "DescribeChapCredentialsResponse"
    "fixture/DescribeChapCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeChapCredentials)

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

responseDescribeSnapshotSchedule :: DescribeSnapshotScheduleResponse -> TestTree
responseDescribeSnapshotSchedule =
  res
    "DescribeSnapshotScheduleResponse"
    "fixture/DescribeSnapshotScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSnapshotSchedule)

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

responseCreateStorediSCSIVolume :: CreateStorediSCSIVolumeResponse -> TestTree
responseCreateStorediSCSIVolume =
  res
    "CreateStorediSCSIVolumeResponse"
    "fixture/CreateStorediSCSIVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStorediSCSIVolume)

responseDescribeTapeArchives :: DescribeTapeArchivesResponse -> TestTree
responseDescribeTapeArchives =
  res
    "DescribeTapeArchivesResponse"
    "fixture/DescribeTapeArchivesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTapeArchives)

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

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy :: Proxy AddTagsToResource)

responseSetSMBGuestPassword :: SetSMBGuestPasswordResponse -> TestTree
responseSetSMBGuestPassword =
  res
    "SetSMBGuestPasswordResponse"
    "fixture/SetSMBGuestPasswordResponse.proto"
    defaultService
    (Proxy :: Proxy SetSMBGuestPassword)

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

responseUpdateBandwidthRateLimit :: UpdateBandwidthRateLimitResponse -> TestTree
responseUpdateBandwidthRateLimit =
  res
    "UpdateBandwidthRateLimitResponse"
    "fixture/UpdateBandwidthRateLimitResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBandwidthRateLimit)

responseDescribeTapeRecoveryPoints :: DescribeTapeRecoveryPointsResponse -> TestTree
responseDescribeTapeRecoveryPoints =
  res
    "DescribeTapeRecoveryPointsResponse"
    "fixture/DescribeTapeRecoveryPointsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTapeRecoveryPoints)

responseDeleteVolume :: DeleteVolumeResponse -> TestTree
responseDeleteVolume =
  res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVolume)

responseListLocalDisks :: ListLocalDisksResponse -> TestTree
responseListLocalDisks =
  res
    "ListLocalDisksResponse"
    "fixture/ListLocalDisksResponse.proto"
    defaultService
    (Proxy :: Proxy ListLocalDisks)

responseDeleteBandwidthRateLimit :: DeleteBandwidthRateLimitResponse -> TestTree
responseDeleteBandwidthRateLimit =
  res
    "DeleteBandwidthRateLimitResponse"
    "fixture/DeleteBandwidthRateLimitResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBandwidthRateLimit)

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

responseDescribeUploadBuffer :: DescribeUploadBufferResponse -> TestTree
responseDescribeUploadBuffer =
  res
    "DescribeUploadBufferResponse"
    "fixture/DescribeUploadBufferResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUploadBuffer)

responseStartAvailabilityMonitorTest :: StartAvailabilityMonitorTestResponse -> TestTree
responseStartAvailabilityMonitorTest =
  res
    "StartAvailabilityMonitorTestResponse"
    "fixture/StartAvailabilityMonitorTestResponse.proto"
    defaultService
    (Proxy :: Proxy StartAvailabilityMonitorTest)

responseDeleteChapCredentials :: DeleteChapCredentialsResponse -> TestTree
responseDeleteChapCredentials =
  res
    "DeleteChapCredentialsResponse"
    "fixture/DeleteChapCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteChapCredentials)

responseUpdateChapCredentials :: UpdateChapCredentialsResponse -> TestTree
responseUpdateChapCredentials =
  res
    "UpdateChapCredentialsResponse"
    "fixture/UpdateChapCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateChapCredentials)

responseDeleteSnapshotSchedule :: DeleteSnapshotScheduleResponse -> TestTree
responseDeleteSnapshotSchedule =
  res
    "DeleteSnapshotScheduleResponse"
    "fixture/DeleteSnapshotScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSnapshotSchedule)

responseShutdownGateway :: ShutdownGatewayResponse -> TestTree
responseShutdownGateway =
  res
    "ShutdownGatewayResponse"
    "fixture/ShutdownGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy ShutdownGateway)

responseDescribeBandwidthRateLimitSchedule :: DescribeBandwidthRateLimitScheduleResponse -> TestTree
responseDescribeBandwidthRateLimitSchedule =
  res
    "DescribeBandwidthRateLimitScheduleResponse"
    "fixture/DescribeBandwidthRateLimitScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBandwidthRateLimitSchedule)

responseDescribeVTLDevices :: DescribeVTLDevicesResponse -> TestTree
responseDescribeVTLDevices =
  res
    "DescribeVTLDevicesResponse"
    "fixture/DescribeVTLDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVTLDevices)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSnapshot)

responseUpdateSnapshotSchedule :: UpdateSnapshotScheduleResponse -> TestTree
responseUpdateSnapshotSchedule =
  res
    "UpdateSnapshotScheduleResponse"
    "fixture/UpdateSnapshotScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSnapshotSchedule)

responseUpdateSMBFileShareVisibility :: UpdateSMBFileShareVisibilityResponse -> TestTree
responseUpdateSMBFileShareVisibility =
  res
    "UpdateSMBFileShareVisibilityResponse"
    "fixture/UpdateSMBFileShareVisibilityResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSMBFileShareVisibility)

responseListAutomaticTapeCreationPolicies :: ListAutomaticTapeCreationPoliciesResponse -> TestTree
responseListAutomaticTapeCreationPolicies =
  res
    "ListAutomaticTapeCreationPoliciesResponse"
    "fixture/ListAutomaticTapeCreationPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAutomaticTapeCreationPolicies)

responseAddCache :: AddCacheResponse -> TestTree
responseAddCache =
  res
    "AddCacheResponse"
    "fixture/AddCacheResponse.proto"
    defaultService
    (Proxy :: Proxy AddCache)

responseNotifyWhenUploaded :: NotifyWhenUploadedResponse -> TestTree
responseNotifyWhenUploaded =
  res
    "NotifyWhenUploadedResponse"
    "fixture/NotifyWhenUploadedResponse.proto"
    defaultService
    (Proxy :: Proxy NotifyWhenUploaded)

responseDescribeCachediSCSIVolumes :: DescribeCachediSCSIVolumesResponse -> TestTree
responseDescribeCachediSCSIVolumes =
  res
    "DescribeCachediSCSIVolumesResponse"
    "fixture/DescribeCachediSCSIVolumesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCachediSCSIVolumes)

responseDeleteAutomaticTapeCreationPolicy :: DeleteAutomaticTapeCreationPolicyResponse -> TestTree
responseDeleteAutomaticTapeCreationPolicy =
  res
    "DeleteAutomaticTapeCreationPolicyResponse"
    "fixture/DeleteAutomaticTapeCreationPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAutomaticTapeCreationPolicy)

responseUpdateAutomaticTapeCreationPolicy :: UpdateAutomaticTapeCreationPolicyResponse -> TestTree
responseUpdateAutomaticTapeCreationPolicy =
  res
    "UpdateAutomaticTapeCreationPolicyResponse"
    "fixture/UpdateAutomaticTapeCreationPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAutomaticTapeCreationPolicy)

responseDescribeWorkingStorage :: DescribeWorkingStorageResponse -> TestTree
responseDescribeWorkingStorage =
  res
    "DescribeWorkingStorageResponse"
    "fixture/DescribeWorkingStorageResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkingStorage)

responseUpdateGatewayInformation :: UpdateGatewayInformationResponse -> TestTree
responseUpdateGatewayInformation =
  res
    "UpdateGatewayInformationResponse"
    "fixture/UpdateGatewayInformationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGatewayInformation)

responseDescribeMaintenanceStartTime :: DescribeMaintenanceStartTimeResponse -> TestTree
responseDescribeMaintenanceStartTime =
  res
    "DescribeMaintenanceStartTimeResponse"
    "fixture/DescribeMaintenanceStartTimeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMaintenanceStartTime)

responseAssignTapePool :: AssignTapePoolResponse -> TestTree
responseAssignTapePool =
  res
    "AssignTapePoolResponse"
    "fixture/AssignTapePoolResponse.proto"
    defaultService
    (Proxy :: Proxy AssignTapePool)

responseDeleteTapePool :: DeleteTapePoolResponse -> TestTree
responseDeleteTapePool =
  res
    "DeleteTapePoolResponse"
    "fixture/DeleteTapePoolResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTapePool)

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

responseAddUploadBuffer :: AddUploadBufferResponse -> TestTree
responseAddUploadBuffer =
  res
    "AddUploadBufferResponse"
    "fixture/AddUploadBufferResponse.proto"
    defaultService
    (Proxy :: Proxy AddUploadBuffer)
