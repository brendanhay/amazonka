{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.StorageGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestCancelArchival $
--             mkCancelArchival
--
--         , requestCreateStorediSCSIVolume $
--             mkCreateStorediSCSIVolume
--
--         , requestCreateNFSFileShare $
--             mkCreateNFSFileShare
--
--         , requestDetachVolume $
--             mkDetachVolume
--
--         , requestDescribeChapCredentials $
--             mkDescribeChapCredentials
--
--         , requestSetLocalConsolePassword $
--             mkSetLocalConsolePassword
--
--         , requestCreateTapes $
--             mkCreateTapes
--
--         , requestUpdateVTLDeviceType $
--             mkUpdateVTLDeviceType
--
--         , requestCreateCachediSCSIVolume $
--             mkCreateCachediSCSIVolume
--
--         , requestListFileShares $
--             mkListFileShares
--
--         , requestJoinDomain $
--             mkJoinDomain
--
--         , requestDeleteFileShare $
--             mkDeleteFileShare
--
--         , requestListVolumeInitiators $
--             mkListVolumeInitiators
--
--         , requestAddUploadBuffer $
--             mkAddUploadBuffer
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestNotifyWhenUploaded $
--             mkNotifyWhenUploaded
--
--         , requestListTapePools $
--             mkListTapePools
--
--         , requestDeleteTapePool $
--             mkDeleteTapePool
--
--         , requestUpdateGatewayInformation $
--             mkUpdateGatewayInformation
--
--         , requestDescribeMaintenanceStartTime $
--             mkDescribeMaintenanceStartTime
--
--         , requestAssignTapePool $
--             mkAssignTapePool
--
--         , requestDescribeWorkingStorage $
--             mkDescribeWorkingStorage
--
--         , requestDescribeCachediSCSIVolumes $
--             mkDescribeCachediSCSIVolumes
--
--         , requestAddCache $
--             mkAddCache
--
--         , requestCreateTapePool $
--             mkCreateTapePool
--
--         , requestStartGateway $
--             mkStartGateway
--
--         , requestShutdownGateway $
--             mkShutdownGateway
--
--         , requestListAutomaticTapeCreationPolicies $
--             mkListAutomaticTapeCreationPolicies
--
--         , requestUpdateGatewaySoftwareNow $
--             mkUpdateGatewaySoftwareNow
--
--         , requestRemoveTagsFromResource $
--             mkRemoveTagsFromResource
--
--         , requestCreateSMBFileShare $
--             mkCreateSMBFileShare
--
--         , requestDeleteChapCredentials $
--             mkDeleteChapCredentials
--
--         , requestUpdateChapCredentials $
--             mkUpdateChapCredentials
--
--         , requestAttachVolume $
--             mkAttachVolume
--
--         , requestDescribeAvailabilityMonitorTest $
--             mkDescribeAvailabilityMonitorTest
--
--         , requestDescribeUploadBuffer $
--             mkDescribeUploadBuffer
--
--         , requestDescribeTapes $
--             mkDescribeTapes
--
--         , requestDescribeStorediSCSIVolumes $
--             mkDescribeStorediSCSIVolumes
--
--         , requestSetSMBGuestPassword $
--             mkSetSMBGuestPassword
--
--         , requestCreateSnapshotFromVolumeRecoveryPoint $
--             mkCreateSnapshotFromVolumeRecoveryPoint
--
--         , requestRetrieveTapeRecoveryPoint $
--             mkRetrieveTapeRecoveryPoint
--
--         , requestAddTagsToResource $
--             mkAddTagsToResource
--
--         , requestDeleteGateway $
--             mkDeleteGateway
--
--         , requestUpdateMaintenanceStartTime $
--             mkUpdateMaintenanceStartTime
--
--         , requestDescribeGatewayInformation $
--             mkDescribeGatewayInformation
--
--         , requestRefreshCache $
--             mkRefreshCache
--
--         , requestUpdateNFSFileShare $
--             mkUpdateNFSFileShare
--
--         , requestRetrieveTapeArchive $
--             mkRetrieveTapeArchive
--
--         , requestDescribeTapeArchives $
--             mkDescribeTapeArchives
--
--         , requestUpdateBandwidthRateLimitSchedule $
--             mkUpdateBandwidthRateLimitSchedule
--
--         , requestDisableGateway $
--             mkDisableGateway
--
--         , requestDescribeSMBSettings $
--             mkDescribeSMBSettings
--
--         , requestDescribeSnapshotSchedule $
--             mkDescribeSnapshotSchedule
--
--         , requestCreateTapeWithBarcode $
--             mkCreateTapeWithBarcode
--
--         , requestDescribeBandwidthRateLimit $
--             mkDescribeBandwidthRateLimit
--
--         , requestDeleteAutomaticTapeCreationPolicy $
--             mkDeleteAutomaticTapeCreationPolicy
--
--         , requestUpdateAutomaticTapeCreationPolicy $
--             mkUpdateAutomaticTapeCreationPolicy
--
--         , requestUpdateSMBFileShareVisibility $
--             mkUpdateSMBFileShareVisibility
--
--         , requestDeleteSnapshotSchedule $
--             mkDeleteSnapshotSchedule
--
--         , requestUpdateSnapshotSchedule $
--             mkUpdateSnapshotSchedule
--
--         , requestDescribeBandwidthRateLimitSchedule $
--             mkDescribeBandwidthRateLimitSchedule
--
--         , requestCreateSnapshot $
--             mkCreateSnapshot
--
--         , requestUpdateSMBSecurityStrategy $
--             mkUpdateSMBSecurityStrategy
--
--         , requestCancelRetrieval $
--             mkCancelRetrieval
--
--         , requestDescribeVTLDevices $
--             mkDescribeVTLDevices
--
--         , requestStartAvailabilityMonitorTest $
--             mkStartAvailabilityMonitorTest
--
--         , requestDeleteTapeArchive $
--             mkDeleteTapeArchive
--
--         , requestUpdateSMBFileShare $
--             mkUpdateSMBFileShare
--
--         , requestDescribeNFSFileShares $
--             mkDescribeNFSFileShares
--
--         , requestListVolumeRecoveryPoints $
--             mkListVolumeRecoveryPoints
--
--         , requestListTapes $
--             mkListTapes
--
--         , requestResetCache $
--             mkResetCache
--
--         , requestDescribeSMBFileShares $
--             mkDescribeSMBFileShares
--
--         , requestListGateways $
--             mkListGateways
--
--         , requestDeleteTape $
--             mkDeleteTape
--
--         , requestListLocalDisks $
--             mkListLocalDisks
--
--         , requestListVolumes $
--             mkListVolumes
--
--         , requestUpdateBandwidthRateLimit $
--             mkUpdateBandwidthRateLimit
--
--         , requestAddWorkingStorage $
--             mkAddWorkingStorage
--
--         , requestDescribeTapeRecoveryPoints $
--             mkDescribeTapeRecoveryPoints
--
--         , requestDeleteBandwidthRateLimit $
--             mkDeleteBandwidthRateLimit
--
--         , requestActivateGateway $
--             mkActivateGateway
--
--         , requestDescribeCache $
--             mkDescribeCache
--
--         , requestDeleteVolume $
--             mkDeleteVolume
--
--           ]

--     , testGroup "response"
--         [ responseCancelArchival $
--             mkCancelArchivalResponse
--
--         , responseCreateStorediSCSIVolume $
--             mkCreateStorediSCSIVolumeResponse
--
--         , responseCreateNFSFileShare $
--             mkCreateNFSFileShareResponse
--
--         , responseDetachVolume $
--             mkDetachVolumeResponse
--
--         , responseDescribeChapCredentials $
--             mkDescribeChapCredentialsResponse
--
--         , responseSetLocalConsolePassword $
--             mkSetLocalConsolePasswordResponse
--
--         , responseCreateTapes $
--             mkCreateTapesResponse
--
--         , responseUpdateVTLDeviceType $
--             mkUpdateVTLDeviceTypeResponse
--
--         , responseCreateCachediSCSIVolume $
--             mkCreateCachediSCSIVolumeResponse
--
--         , responseListFileShares $
--             mkListFileSharesResponse
--
--         , responseJoinDomain $
--             mkJoinDomainResponse
--
--         , responseDeleteFileShare $
--             mkDeleteFileShareResponse
--
--         , responseListVolumeInitiators $
--             mkListVolumeInitiatorsResponse
--
--         , responseAddUploadBuffer $
--             mkAddUploadBufferResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseNotifyWhenUploaded $
--             mkNotifyWhenUploadedResponse
--
--         , responseListTapePools $
--             mkListTapePoolsResponse
--
--         , responseDeleteTapePool $
--             mkDeleteTapePoolResponse
--
--         , responseUpdateGatewayInformation $
--             mkUpdateGatewayInformationResponse
--
--         , responseDescribeMaintenanceStartTime $
--             mkDescribeMaintenanceStartTimeResponse
--
--         , responseAssignTapePool $
--             mkAssignTapePoolResponse
--
--         , responseDescribeWorkingStorage $
--             mkDescribeWorkingStorageResponse
--
--         , responseDescribeCachediSCSIVolumes $
--             mkDescribeCachediSCSIVolumesResponse
--
--         , responseAddCache $
--             mkAddCacheResponse
--
--         , responseCreateTapePool $
--             mkCreateTapePoolResponse
--
--         , responseStartGateway $
--             mkStartGatewayResponse
--
--         , responseShutdownGateway $
--             mkShutdownGatewayResponse
--
--         , responseListAutomaticTapeCreationPolicies $
--             mkListAutomaticTapeCreationPoliciesResponse
--
--         , responseUpdateGatewaySoftwareNow $
--             mkUpdateGatewaySoftwareNowResponse
--
--         , responseRemoveTagsFromResource $
--             mkRemoveTagsFromResourceResponse
--
--         , responseCreateSMBFileShare $
--             mkCreateSMBFileShareResponse
--
--         , responseDeleteChapCredentials $
--             mkDeleteChapCredentialsResponse
--
--         , responseUpdateChapCredentials $
--             mkUpdateChapCredentialsResponse
--
--         , responseAttachVolume $
--             mkAttachVolumeResponse
--
--         , responseDescribeAvailabilityMonitorTest $
--             mkDescribeAvailabilityMonitorTestResponse
--
--         , responseDescribeUploadBuffer $
--             mkDescribeUploadBufferResponse
--
--         , responseDescribeTapes $
--             mkDescribeTapesResponse
--
--         , responseDescribeStorediSCSIVolumes $
--             mkDescribeStorediSCSIVolumesResponse
--
--         , responseSetSMBGuestPassword $
--             mkSetSMBGuestPasswordResponse
--
--         , responseCreateSnapshotFromVolumeRecoveryPoint $
--             mkCreateSnapshotFromVolumeRecoveryPointResponse
--
--         , responseRetrieveTapeRecoveryPoint $
--             mkRetrieveTapeRecoveryPointResponse
--
--         , responseAddTagsToResource $
--             mkAddTagsToResourceResponse
--
--         , responseDeleteGateway $
--             mkDeleteGatewayResponse
--
--         , responseUpdateMaintenanceStartTime $
--             mkUpdateMaintenanceStartTimeResponse
--
--         , responseDescribeGatewayInformation $
--             mkDescribeGatewayInformationResponse
--
--         , responseRefreshCache $
--             mkRefreshCacheResponse
--
--         , responseUpdateNFSFileShare $
--             mkUpdateNFSFileShareResponse
--
--         , responseRetrieveTapeArchive $
--             mkRetrieveTapeArchiveResponse
--
--         , responseDescribeTapeArchives $
--             mkDescribeTapeArchivesResponse
--
--         , responseUpdateBandwidthRateLimitSchedule $
--             mkUpdateBandwidthRateLimitScheduleResponse
--
--         , responseDisableGateway $
--             mkDisableGatewayResponse
--
--         , responseDescribeSMBSettings $
--             mkDescribeSMBSettingsResponse
--
--         , responseDescribeSnapshotSchedule $
--             mkDescribeSnapshotScheduleResponse
--
--         , responseCreateTapeWithBarcode $
--             mkCreateTapeWithBarcodeResponse
--
--         , responseDescribeBandwidthRateLimit $
--             mkDescribeBandwidthRateLimitResponse
--
--         , responseDeleteAutomaticTapeCreationPolicy $
--             mkDeleteAutomaticTapeCreationPolicyResponse
--
--         , responseUpdateAutomaticTapeCreationPolicy $
--             mkUpdateAutomaticTapeCreationPolicyResponse
--
--         , responseUpdateSMBFileShareVisibility $
--             mkUpdateSMBFileShareVisibilityResponse
--
--         , responseDeleteSnapshotSchedule $
--             mkDeleteSnapshotScheduleResponse
--
--         , responseUpdateSnapshotSchedule $
--             mkUpdateSnapshotScheduleResponse
--
--         , responseDescribeBandwidthRateLimitSchedule $
--             mkDescribeBandwidthRateLimitScheduleResponse
--
--         , responseCreateSnapshot $
--             mkCreateSnapshotResponse
--
--         , responseUpdateSMBSecurityStrategy $
--             mkUpdateSMBSecurityStrategyResponse
--
--         , responseCancelRetrieval $
--             mkCancelRetrievalResponse
--
--         , responseDescribeVTLDevices $
--             mkDescribeVTLDevicesResponse
--
--         , responseStartAvailabilityMonitorTest $
--             mkStartAvailabilityMonitorTestResponse
--
--         , responseDeleteTapeArchive $
--             mkDeleteTapeArchiveResponse
--
--         , responseUpdateSMBFileShare $
--             mkUpdateSMBFileShareResponse
--
--         , responseDescribeNFSFileShares $
--             mkDescribeNFSFileSharesResponse
--
--         , responseListVolumeRecoveryPoints $
--             mkListVolumeRecoveryPointsResponse
--
--         , responseListTapes $
--             mkListTapesResponse
--
--         , responseResetCache $
--             mkResetCacheResponse
--
--         , responseDescribeSMBFileShares $
--             mkDescribeSMBFileSharesResponse
--
--         , responseListGateways $
--             mkListGatewaysResponse
--
--         , responseDeleteTape $
--             mkDeleteTapeResponse
--
--         , responseListLocalDisks $
--             mkListLocalDisksResponse
--
--         , responseListVolumes $
--             mkListVolumesResponse
--
--         , responseUpdateBandwidthRateLimit $
--             mkUpdateBandwidthRateLimitResponse
--
--         , responseAddWorkingStorage $
--             mkAddWorkingStorageResponse
--
--         , responseDescribeTapeRecoveryPoints $
--             mkDescribeTapeRecoveryPointsResponse
--
--         , responseDeleteBandwidthRateLimit $
--             mkDeleteBandwidthRateLimitResponse
--
--         , responseActivateGateway $
--             mkActivateGatewayResponse
--
--         , responseDescribeCache $
--             mkDescribeCacheResponse
--
--         , responseDeleteVolume $
--             mkDeleteVolumeResponse
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

requestCreateNFSFileShare :: CreateNFSFileShare -> TestTree
requestCreateNFSFileShare =
  req
    "CreateNFSFileShare"
    "fixture/CreateNFSFileShare.yaml"

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
    mkServiceConfig
    (Proxy :: Proxy CancelArchival)

responseCreateStorediSCSIVolume :: CreateStorediSCSIVolumeResponse -> TestTree
responseCreateStorediSCSIVolume =
  res
    "CreateStorediSCSIVolumeResponse"
    "fixture/CreateStorediSCSIVolumeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateStorediSCSIVolume)

responseCreateNFSFileShare :: CreateNFSFileShareResponse -> TestTree
responseCreateNFSFileShare =
  res
    "CreateNFSFileShareResponse"
    "fixture/CreateNFSFileShareResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateNFSFileShare)

responseDetachVolume :: DetachVolumeResponse -> TestTree
responseDetachVolume =
  res
    "DetachVolumeResponse"
    "fixture/DetachVolumeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachVolume)

responseDescribeChapCredentials :: DescribeChapCredentialsResponse -> TestTree
responseDescribeChapCredentials =
  res
    "DescribeChapCredentialsResponse"
    "fixture/DescribeChapCredentialsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeChapCredentials)

responseSetLocalConsolePassword :: SetLocalConsolePasswordResponse -> TestTree
responseSetLocalConsolePassword =
  res
    "SetLocalConsolePasswordResponse"
    "fixture/SetLocalConsolePasswordResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetLocalConsolePassword)

responseCreateTapes :: CreateTapesResponse -> TestTree
responseCreateTapes =
  res
    "CreateTapesResponse"
    "fixture/CreateTapesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTapes)

responseUpdateVTLDeviceType :: UpdateVTLDeviceTypeResponse -> TestTree
responseUpdateVTLDeviceType =
  res
    "UpdateVTLDeviceTypeResponse"
    "fixture/UpdateVTLDeviceTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateVTLDeviceType)

responseCreateCachediSCSIVolume :: CreateCachediSCSIVolumeResponse -> TestTree
responseCreateCachediSCSIVolume =
  res
    "CreateCachediSCSIVolumeResponse"
    "fixture/CreateCachediSCSIVolumeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCachediSCSIVolume)

responseListFileShares :: ListFileSharesResponse -> TestTree
responseListFileShares =
  res
    "ListFileSharesResponse"
    "fixture/ListFileSharesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListFileShares)

responseJoinDomain :: JoinDomainResponse -> TestTree
responseJoinDomain =
  res
    "JoinDomainResponse"
    "fixture/JoinDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy JoinDomain)

responseDeleteFileShare :: DeleteFileShareResponse -> TestTree
responseDeleteFileShare =
  res
    "DeleteFileShareResponse"
    "fixture/DeleteFileShareResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteFileShare)

responseListVolumeInitiators :: ListVolumeInitiatorsResponse -> TestTree
responseListVolumeInitiators =
  res
    "ListVolumeInitiatorsResponse"
    "fixture/ListVolumeInitiatorsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListVolumeInitiators)

responseAddUploadBuffer :: AddUploadBufferResponse -> TestTree
responseAddUploadBuffer =
  res
    "AddUploadBufferResponse"
    "fixture/AddUploadBufferResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddUploadBuffer)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseNotifyWhenUploaded :: NotifyWhenUploadedResponse -> TestTree
responseNotifyWhenUploaded =
  res
    "NotifyWhenUploadedResponse"
    "fixture/NotifyWhenUploadedResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy NotifyWhenUploaded)

responseListTapePools :: ListTapePoolsResponse -> TestTree
responseListTapePools =
  res
    "ListTapePoolsResponse"
    "fixture/ListTapePoolsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTapePools)

responseDeleteTapePool :: DeleteTapePoolResponse -> TestTree
responseDeleteTapePool =
  res
    "DeleteTapePoolResponse"
    "fixture/DeleteTapePoolResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTapePool)

responseUpdateGatewayInformation :: UpdateGatewayInformationResponse -> TestTree
responseUpdateGatewayInformation =
  res
    "UpdateGatewayInformationResponse"
    "fixture/UpdateGatewayInformationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateGatewayInformation)

responseDescribeMaintenanceStartTime :: DescribeMaintenanceStartTimeResponse -> TestTree
responseDescribeMaintenanceStartTime =
  res
    "DescribeMaintenanceStartTimeResponse"
    "fixture/DescribeMaintenanceStartTimeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeMaintenanceStartTime)

responseAssignTapePool :: AssignTapePoolResponse -> TestTree
responseAssignTapePool =
  res
    "AssignTapePoolResponse"
    "fixture/AssignTapePoolResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssignTapePool)

responseDescribeWorkingStorage :: DescribeWorkingStorageResponse -> TestTree
responseDescribeWorkingStorage =
  res
    "DescribeWorkingStorageResponse"
    "fixture/DescribeWorkingStorageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeWorkingStorage)

responseDescribeCachediSCSIVolumes :: DescribeCachediSCSIVolumesResponse -> TestTree
responseDescribeCachediSCSIVolumes =
  res
    "DescribeCachediSCSIVolumesResponse"
    "fixture/DescribeCachediSCSIVolumesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCachediSCSIVolumes)

responseAddCache :: AddCacheResponse -> TestTree
responseAddCache =
  res
    "AddCacheResponse"
    "fixture/AddCacheResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddCache)

responseCreateTapePool :: CreateTapePoolResponse -> TestTree
responseCreateTapePool =
  res
    "CreateTapePoolResponse"
    "fixture/CreateTapePoolResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTapePool)

responseStartGateway :: StartGatewayResponse -> TestTree
responseStartGateway =
  res
    "StartGatewayResponse"
    "fixture/StartGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartGateway)

responseShutdownGateway :: ShutdownGatewayResponse -> TestTree
responseShutdownGateway =
  res
    "ShutdownGatewayResponse"
    "fixture/ShutdownGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ShutdownGateway)

responseListAutomaticTapeCreationPolicies :: ListAutomaticTapeCreationPoliciesResponse -> TestTree
responseListAutomaticTapeCreationPolicies =
  res
    "ListAutomaticTapeCreationPoliciesResponse"
    "fixture/ListAutomaticTapeCreationPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAutomaticTapeCreationPolicies)

responseUpdateGatewaySoftwareNow :: UpdateGatewaySoftwareNowResponse -> TestTree
responseUpdateGatewaySoftwareNow =
  res
    "UpdateGatewaySoftwareNowResponse"
    "fixture/UpdateGatewaySoftwareNowResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateGatewaySoftwareNow)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveTagsFromResource)

responseCreateSMBFileShare :: CreateSMBFileShareResponse -> TestTree
responseCreateSMBFileShare =
  res
    "CreateSMBFileShareResponse"
    "fixture/CreateSMBFileShareResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSMBFileShare)

responseDeleteChapCredentials :: DeleteChapCredentialsResponse -> TestTree
responseDeleteChapCredentials =
  res
    "DeleteChapCredentialsResponse"
    "fixture/DeleteChapCredentialsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteChapCredentials)

responseUpdateChapCredentials :: UpdateChapCredentialsResponse -> TestTree
responseUpdateChapCredentials =
  res
    "UpdateChapCredentialsResponse"
    "fixture/UpdateChapCredentialsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateChapCredentials)

responseAttachVolume :: AttachVolumeResponse -> TestTree
responseAttachVolume =
  res
    "AttachVolumeResponse"
    "fixture/AttachVolumeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachVolume)

responseDescribeAvailabilityMonitorTest :: DescribeAvailabilityMonitorTestResponse -> TestTree
responseDescribeAvailabilityMonitorTest =
  res
    "DescribeAvailabilityMonitorTestResponse"
    "fixture/DescribeAvailabilityMonitorTestResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAvailabilityMonitorTest)

responseDescribeUploadBuffer :: DescribeUploadBufferResponse -> TestTree
responseDescribeUploadBuffer =
  res
    "DescribeUploadBufferResponse"
    "fixture/DescribeUploadBufferResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeUploadBuffer)

responseDescribeTapes :: DescribeTapesResponse -> TestTree
responseDescribeTapes =
  res
    "DescribeTapesResponse"
    "fixture/DescribeTapesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTapes)

responseDescribeStorediSCSIVolumes :: DescribeStorediSCSIVolumesResponse -> TestTree
responseDescribeStorediSCSIVolumes =
  res
    "DescribeStorediSCSIVolumesResponse"
    "fixture/DescribeStorediSCSIVolumesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStorediSCSIVolumes)

responseSetSMBGuestPassword :: SetSMBGuestPasswordResponse -> TestTree
responseSetSMBGuestPassword =
  res
    "SetSMBGuestPasswordResponse"
    "fixture/SetSMBGuestPasswordResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetSMBGuestPassword)

responseCreateSnapshotFromVolumeRecoveryPoint :: CreateSnapshotFromVolumeRecoveryPointResponse -> TestTree
responseCreateSnapshotFromVolumeRecoveryPoint =
  res
    "CreateSnapshotFromVolumeRecoveryPointResponse"
    "fixture/CreateSnapshotFromVolumeRecoveryPointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSnapshotFromVolumeRecoveryPoint)

responseRetrieveTapeRecoveryPoint :: RetrieveTapeRecoveryPointResponse -> TestTree
responseRetrieveTapeRecoveryPoint =
  res
    "RetrieveTapeRecoveryPointResponse"
    "fixture/RetrieveTapeRecoveryPointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RetrieveTapeRecoveryPoint)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddTagsToResource)

responseDeleteGateway :: DeleteGatewayResponse -> TestTree
responseDeleteGateway =
  res
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteGateway)

responseUpdateMaintenanceStartTime :: UpdateMaintenanceStartTimeResponse -> TestTree
responseUpdateMaintenanceStartTime =
  res
    "UpdateMaintenanceStartTimeResponse"
    "fixture/UpdateMaintenanceStartTimeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateMaintenanceStartTime)

responseDescribeGatewayInformation :: DescribeGatewayInformationResponse -> TestTree
responseDescribeGatewayInformation =
  res
    "DescribeGatewayInformationResponse"
    "fixture/DescribeGatewayInformationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeGatewayInformation)

responseRefreshCache :: RefreshCacheResponse -> TestTree
responseRefreshCache =
  res
    "RefreshCacheResponse"
    "fixture/RefreshCacheResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RefreshCache)

responseUpdateNFSFileShare :: UpdateNFSFileShareResponse -> TestTree
responseUpdateNFSFileShare =
  res
    "UpdateNFSFileShareResponse"
    "fixture/UpdateNFSFileShareResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateNFSFileShare)

responseRetrieveTapeArchive :: RetrieveTapeArchiveResponse -> TestTree
responseRetrieveTapeArchive =
  res
    "RetrieveTapeArchiveResponse"
    "fixture/RetrieveTapeArchiveResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RetrieveTapeArchive)

responseDescribeTapeArchives :: DescribeTapeArchivesResponse -> TestTree
responseDescribeTapeArchives =
  res
    "DescribeTapeArchivesResponse"
    "fixture/DescribeTapeArchivesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTapeArchives)

responseUpdateBandwidthRateLimitSchedule :: UpdateBandwidthRateLimitScheduleResponse -> TestTree
responseUpdateBandwidthRateLimitSchedule =
  res
    "UpdateBandwidthRateLimitScheduleResponse"
    "fixture/UpdateBandwidthRateLimitScheduleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateBandwidthRateLimitSchedule)

responseDisableGateway :: DisableGatewayResponse -> TestTree
responseDisableGateway =
  res
    "DisableGatewayResponse"
    "fixture/DisableGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableGateway)

responseDescribeSMBSettings :: DescribeSMBSettingsResponse -> TestTree
responseDescribeSMBSettings =
  res
    "DescribeSMBSettingsResponse"
    "fixture/DescribeSMBSettingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSMBSettings)

responseDescribeSnapshotSchedule :: DescribeSnapshotScheduleResponse -> TestTree
responseDescribeSnapshotSchedule =
  res
    "DescribeSnapshotScheduleResponse"
    "fixture/DescribeSnapshotScheduleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSnapshotSchedule)

responseCreateTapeWithBarcode :: CreateTapeWithBarcodeResponse -> TestTree
responseCreateTapeWithBarcode =
  res
    "CreateTapeWithBarcodeResponse"
    "fixture/CreateTapeWithBarcodeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTapeWithBarcode)

responseDescribeBandwidthRateLimit :: DescribeBandwidthRateLimitResponse -> TestTree
responseDescribeBandwidthRateLimit =
  res
    "DescribeBandwidthRateLimitResponse"
    "fixture/DescribeBandwidthRateLimitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeBandwidthRateLimit)

responseDeleteAutomaticTapeCreationPolicy :: DeleteAutomaticTapeCreationPolicyResponse -> TestTree
responseDeleteAutomaticTapeCreationPolicy =
  res
    "DeleteAutomaticTapeCreationPolicyResponse"
    "fixture/DeleteAutomaticTapeCreationPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAutomaticTapeCreationPolicy)

responseUpdateAutomaticTapeCreationPolicy :: UpdateAutomaticTapeCreationPolicyResponse -> TestTree
responseUpdateAutomaticTapeCreationPolicy =
  res
    "UpdateAutomaticTapeCreationPolicyResponse"
    "fixture/UpdateAutomaticTapeCreationPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateAutomaticTapeCreationPolicy)

responseUpdateSMBFileShareVisibility :: UpdateSMBFileShareVisibilityResponse -> TestTree
responseUpdateSMBFileShareVisibility =
  res
    "UpdateSMBFileShareVisibilityResponse"
    "fixture/UpdateSMBFileShareVisibilityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSMBFileShareVisibility)

responseDeleteSnapshotSchedule :: DeleteSnapshotScheduleResponse -> TestTree
responseDeleteSnapshotSchedule =
  res
    "DeleteSnapshotScheduleResponse"
    "fixture/DeleteSnapshotScheduleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSnapshotSchedule)

responseUpdateSnapshotSchedule :: UpdateSnapshotScheduleResponse -> TestTree
responseUpdateSnapshotSchedule =
  res
    "UpdateSnapshotScheduleResponse"
    "fixture/UpdateSnapshotScheduleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSnapshotSchedule)

responseDescribeBandwidthRateLimitSchedule :: DescribeBandwidthRateLimitScheduleResponse -> TestTree
responseDescribeBandwidthRateLimitSchedule =
  res
    "DescribeBandwidthRateLimitScheduleResponse"
    "fixture/DescribeBandwidthRateLimitScheduleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeBandwidthRateLimitSchedule)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSnapshot)

responseUpdateSMBSecurityStrategy :: UpdateSMBSecurityStrategyResponse -> TestTree
responseUpdateSMBSecurityStrategy =
  res
    "UpdateSMBSecurityStrategyResponse"
    "fixture/UpdateSMBSecurityStrategyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSMBSecurityStrategy)

responseCancelRetrieval :: CancelRetrievalResponse -> TestTree
responseCancelRetrieval =
  res
    "CancelRetrievalResponse"
    "fixture/CancelRetrievalResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelRetrieval)

responseDescribeVTLDevices :: DescribeVTLDevicesResponse -> TestTree
responseDescribeVTLDevices =
  res
    "DescribeVTLDevicesResponse"
    "fixture/DescribeVTLDevicesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVTLDevices)

responseStartAvailabilityMonitorTest :: StartAvailabilityMonitorTestResponse -> TestTree
responseStartAvailabilityMonitorTest =
  res
    "StartAvailabilityMonitorTestResponse"
    "fixture/StartAvailabilityMonitorTestResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartAvailabilityMonitorTest)

responseDeleteTapeArchive :: DeleteTapeArchiveResponse -> TestTree
responseDeleteTapeArchive =
  res
    "DeleteTapeArchiveResponse"
    "fixture/DeleteTapeArchiveResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTapeArchive)

responseUpdateSMBFileShare :: UpdateSMBFileShareResponse -> TestTree
responseUpdateSMBFileShare =
  res
    "UpdateSMBFileShareResponse"
    "fixture/UpdateSMBFileShareResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSMBFileShare)

responseDescribeNFSFileShares :: DescribeNFSFileSharesResponse -> TestTree
responseDescribeNFSFileShares =
  res
    "DescribeNFSFileSharesResponse"
    "fixture/DescribeNFSFileSharesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeNFSFileShares)

responseListVolumeRecoveryPoints :: ListVolumeRecoveryPointsResponse -> TestTree
responseListVolumeRecoveryPoints =
  res
    "ListVolumeRecoveryPointsResponse"
    "fixture/ListVolumeRecoveryPointsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListVolumeRecoveryPoints)

responseListTapes :: ListTapesResponse -> TestTree
responseListTapes =
  res
    "ListTapesResponse"
    "fixture/ListTapesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTapes)

responseResetCache :: ResetCacheResponse -> TestTree
responseResetCache =
  res
    "ResetCacheResponse"
    "fixture/ResetCacheResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResetCache)

responseDescribeSMBFileShares :: DescribeSMBFileSharesResponse -> TestTree
responseDescribeSMBFileShares =
  res
    "DescribeSMBFileSharesResponse"
    "fixture/DescribeSMBFileSharesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSMBFileShares)

responseListGateways :: ListGatewaysResponse -> TestTree
responseListGateways =
  res
    "ListGatewaysResponse"
    "fixture/ListGatewaysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListGateways)

responseDeleteTape :: DeleteTapeResponse -> TestTree
responseDeleteTape =
  res
    "DeleteTapeResponse"
    "fixture/DeleteTapeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTape)

responseListLocalDisks :: ListLocalDisksResponse -> TestTree
responseListLocalDisks =
  res
    "ListLocalDisksResponse"
    "fixture/ListLocalDisksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListLocalDisks)

responseListVolumes :: ListVolumesResponse -> TestTree
responseListVolumes =
  res
    "ListVolumesResponse"
    "fixture/ListVolumesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListVolumes)

responseUpdateBandwidthRateLimit :: UpdateBandwidthRateLimitResponse -> TestTree
responseUpdateBandwidthRateLimit =
  res
    "UpdateBandwidthRateLimitResponse"
    "fixture/UpdateBandwidthRateLimitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateBandwidthRateLimit)

responseAddWorkingStorage :: AddWorkingStorageResponse -> TestTree
responseAddWorkingStorage =
  res
    "AddWorkingStorageResponse"
    "fixture/AddWorkingStorageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddWorkingStorage)

responseDescribeTapeRecoveryPoints :: DescribeTapeRecoveryPointsResponse -> TestTree
responseDescribeTapeRecoveryPoints =
  res
    "DescribeTapeRecoveryPointsResponse"
    "fixture/DescribeTapeRecoveryPointsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTapeRecoveryPoints)

responseDeleteBandwidthRateLimit :: DeleteBandwidthRateLimitResponse -> TestTree
responseDeleteBandwidthRateLimit =
  res
    "DeleteBandwidthRateLimitResponse"
    "fixture/DeleteBandwidthRateLimitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBandwidthRateLimit)

responseActivateGateway :: ActivateGatewayResponse -> TestTree
responseActivateGateway =
  res
    "ActivateGatewayResponse"
    "fixture/ActivateGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ActivateGateway)

responseDescribeCache :: DescribeCacheResponse -> TestTree
responseDescribeCache =
  res
    "DescribeCacheResponse"
    "fixture/DescribeCacheResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCache)

responseDeleteVolume :: DeleteVolumeResponse -> TestTree
responseDeleteVolume =
  res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteVolume)
