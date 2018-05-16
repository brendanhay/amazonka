{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.StorageGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--             cancelArchival
--
--         , requestCreateStorediSCSIVolume $
--             createStorediSCSIVolume
--
--         , requestCreateNFSFileShare $
--             createNFSFileShare
--
--         , requestDescribeChapCredentials $
--             describeChapCredentials
--
--         , requestSetLocalConsolePassword $
--             setLocalConsolePassword
--
--         , requestCreateTapes $
--             createTapes
--
--         , requestUpdateVTLDeviceType $
--             updateVTLDeviceType
--
--         , requestCreateCachediSCSIVolume $
--             createCachediSCSIVolume
--
--         , requestListFileShares $
--             listFileShares
--
--         , requestDeleteFileShare $
--             deleteFileShare
--
--         , requestListVolumeInitiators $
--             listVolumeInitiators
--
--         , requestAddUploadBuffer $
--             addUploadBuffer
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestNotifyWhenUploaded $
--             notifyWhenUploaded
--
--         , requestUpdateGatewayInformation $
--             updateGatewayInformation
--
--         , requestDescribeMaintenanceStartTime $
--             describeMaintenanceStartTime
--
--         , requestDescribeWorkingStorage $
--             describeWorkingStorage
--
--         , requestDescribeCachediSCSIVolumes $
--             describeCachediSCSIVolumes
--
--         , requestAddCache $
--             addCache
--
--         , requestStartGateway $
--             startGateway
--
--         , requestShutdownGateway $
--             shutdownGateway
--
--         , requestUpdateGatewaySoftwareNow $
--             updateGatewaySoftwareNow
--
--         , requestRemoveTagsFromResource $
--             removeTagsFromResource
--
--         , requestDeleteChapCredentials $
--             deleteChapCredentials
--
--         , requestUpdateChapCredentials $
--             updateChapCredentials
--
--         , requestDescribeUploadBuffer $
--             describeUploadBuffer
--
--         , requestDescribeTapes $
--             describeTapes
--
--         , requestDescribeStorediSCSIVolumes $
--             describeStorediSCSIVolumes
--
--         , requestCreateSnapshotFromVolumeRecoveryPoint $
--             createSnapshotFromVolumeRecoveryPoint
--
--         , requestRetrieveTapeRecoveryPoint $
--             retrieveTapeRecoveryPoint
--
--         , requestAddTagsToResource $
--             addTagsToResource
--
--         , requestDeleteGateway $
--             deleteGateway
--
--         , requestUpdateMaintenanceStartTime $
--             updateMaintenanceStartTime
--
--         , requestDescribeGatewayInformation $
--             describeGatewayInformation
--
--         , requestRefreshCache $
--             refreshCache
--
--         , requestUpdateNFSFileShare $
--             updateNFSFileShare
--
--         , requestRetrieveTapeArchive $
--             retrieveTapeArchive
--
--         , requestDescribeTapeArchives $
--             describeTapeArchives
--
--         , requestDisableGateway $
--             disableGateway
--
--         , requestDescribeSnapshotSchedule $
--             describeSnapshotSchedule
--
--         , requestCreateTapeWithBarcode $
--             createTapeWithBarcode
--
--         , requestDescribeBandwidthRateLimit $
--             describeBandwidthRateLimit
--
--         , requestDeleteSnapshotSchedule $
--             deleteSnapshotSchedule
--
--         , requestUpdateSnapshotSchedule $
--             updateSnapshotSchedule
--
--         , requestCreateSnapshot $
--             createSnapshot
--
--         , requestCancelRetrieval $
--             cancelRetrieval
--
--         , requestDescribeVTLDevices $
--             describeVTLDevices
--
--         , requestDeleteTapeArchive $
--             deleteTapeArchive
--
--         , requestDescribeNFSFileShares $
--             describeNFSFileShares
--
--         , requestListVolumeRecoveryPoints $
--             listVolumeRecoveryPoints
--
--         , requestListTapes $
--             listTapes
--
--         , requestResetCache $
--             resetCache
--
--         , requestListGateways $
--             listGateways
--
--         , requestDeleteTape $
--             deleteTape
--
--         , requestListLocalDisks $
--             listLocalDisks
--
--         , requestListVolumes $
--             listVolumes
--
--         , requestUpdateBandwidthRateLimit $
--             updateBandwidthRateLimit
--
--         , requestAddWorkingStorage $
--             addWorkingStorage
--
--         , requestDescribeTapeRecoveryPoints $
--             describeTapeRecoveryPoints
--
--         , requestDeleteBandwidthRateLimit $
--             deleteBandwidthRateLimit
--
--         , requestActivateGateway $
--             activateGateway
--
--         , requestDescribeCache $
--             describeCache
--
--         , requestDeleteVolume $
--             deleteVolume
--
--           ]

--     , testGroup "response"
--         [ responseCancelArchival $
--             cancelArchivalResponse
--
--         , responseCreateStorediSCSIVolume $
--             createStorediSCSIVolumeResponse
--
--         , responseCreateNFSFileShare $
--             createNFSFileShareResponse
--
--         , responseDescribeChapCredentials $
--             describeChapCredentialsResponse
--
--         , responseSetLocalConsolePassword $
--             setLocalConsolePasswordResponse
--
--         , responseCreateTapes $
--             createTapesResponse
--
--         , responseUpdateVTLDeviceType $
--             updateVTLDeviceTypeResponse
--
--         , responseCreateCachediSCSIVolume $
--             createCachediSCSIVolumeResponse
--
--         , responseListFileShares $
--             listFileSharesResponse
--
--         , responseDeleteFileShare $
--             deleteFileShareResponse
--
--         , responseListVolumeInitiators $
--             listVolumeInitiatorsResponse
--
--         , responseAddUploadBuffer $
--             addUploadBufferResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseNotifyWhenUploaded $
--             notifyWhenUploadedResponse
--
--         , responseUpdateGatewayInformation $
--             updateGatewayInformationResponse
--
--         , responseDescribeMaintenanceStartTime $
--             describeMaintenanceStartTimeResponse
--
--         , responseDescribeWorkingStorage $
--             describeWorkingStorageResponse
--
--         , responseDescribeCachediSCSIVolumes $
--             describeCachediSCSIVolumesResponse
--
--         , responseAddCache $
--             addCacheResponse
--
--         , responseStartGateway $
--             startGatewayResponse
--
--         , responseShutdownGateway $
--             shutdownGatewayResponse
--
--         , responseUpdateGatewaySoftwareNow $
--             updateGatewaySoftwareNowResponse
--
--         , responseRemoveTagsFromResource $
--             removeTagsFromResourceResponse
--
--         , responseDeleteChapCredentials $
--             deleteChapCredentialsResponse
--
--         , responseUpdateChapCredentials $
--             updateChapCredentialsResponse
--
--         , responseDescribeUploadBuffer $
--             describeUploadBufferResponse
--
--         , responseDescribeTapes $
--             describeTapesResponse
--
--         , responseDescribeStorediSCSIVolumes $
--             describeStorediSCSIVolumesResponse
--
--         , responseCreateSnapshotFromVolumeRecoveryPoint $
--             createSnapshotFromVolumeRecoveryPointResponse
--
--         , responseRetrieveTapeRecoveryPoint $
--             retrieveTapeRecoveryPointResponse
--
--         , responseAddTagsToResource $
--             addTagsToResourceResponse
--
--         , responseDeleteGateway $
--             deleteGatewayResponse
--
--         , responseUpdateMaintenanceStartTime $
--             updateMaintenanceStartTimeResponse
--
--         , responseDescribeGatewayInformation $
--             describeGatewayInformationResponse
--
--         , responseRefreshCache $
--             refreshCacheResponse
--
--         , responseUpdateNFSFileShare $
--             updateNFSFileShareResponse
--
--         , responseRetrieveTapeArchive $
--             retrieveTapeArchiveResponse
--
--         , responseDescribeTapeArchives $
--             describeTapeArchivesResponse
--
--         , responseDisableGateway $
--             disableGatewayResponse
--
--         , responseDescribeSnapshotSchedule $
--             describeSnapshotScheduleResponse
--
--         , responseCreateTapeWithBarcode $
--             createTapeWithBarcodeResponse
--
--         , responseDescribeBandwidthRateLimit $
--             describeBandwidthRateLimitResponse
--
--         , responseDeleteSnapshotSchedule $
--             deleteSnapshotScheduleResponse
--
--         , responseUpdateSnapshotSchedule $
--             updateSnapshotScheduleResponse
--
--         , responseCreateSnapshot $
--             createSnapshotResponse
--
--         , responseCancelRetrieval $
--             cancelRetrievalResponse
--
--         , responseDescribeVTLDevices $
--             describeVTLDevicesResponse
--
--         , responseDeleteTapeArchive $
--             deleteTapeArchiveResponse
--
--         , responseDescribeNFSFileShares $
--             describeNFSFileSharesResponse
--
--         , responseListVolumeRecoveryPoints $
--             listVolumeRecoveryPointsResponse
--
--         , responseListTapes $
--             listTapesResponse
--
--         , responseResetCache $
--             resetCacheResponse
--
--         , responseListGateways $
--             listGatewaysResponse
--
--         , responseDeleteTape $
--             deleteTapeResponse
--
--         , responseListLocalDisks $
--             listLocalDisksResponse
--
--         , responseListVolumes $
--             listVolumesResponse
--
--         , responseUpdateBandwidthRateLimit $
--             updateBandwidthRateLimitResponse
--
--         , responseAddWorkingStorage $
--             addWorkingStorageResponse
--
--         , responseDescribeTapeRecoveryPoints $
--             describeTapeRecoveryPointsResponse
--
--         , responseDeleteBandwidthRateLimit $
--             deleteBandwidthRateLimitResponse
--
--         , responseActivateGateway $
--             activateGatewayResponse
--
--         , responseDescribeCache $
--             describeCacheResponse
--
--         , responseDeleteVolume $
--             deleteVolumeResponse
--
--           ]
--     ]

-- Requests

requestCancelArchival :: CancelArchival -> TestTree
requestCancelArchival = req
    "CancelArchival"
    "fixture/CancelArchival.yaml"

requestCreateStorediSCSIVolume :: CreateStorediSCSIVolume -> TestTree
requestCreateStorediSCSIVolume = req
    "CreateStorediSCSIVolume"
    "fixture/CreateStorediSCSIVolume.yaml"

requestCreateNFSFileShare :: CreateNFSFileShare -> TestTree
requestCreateNFSFileShare = req
    "CreateNFSFileShare"
    "fixture/CreateNFSFileShare.yaml"

requestDescribeChapCredentials :: DescribeChapCredentials -> TestTree
requestDescribeChapCredentials = req
    "DescribeChapCredentials"
    "fixture/DescribeChapCredentials.yaml"

requestSetLocalConsolePassword :: SetLocalConsolePassword -> TestTree
requestSetLocalConsolePassword = req
    "SetLocalConsolePassword"
    "fixture/SetLocalConsolePassword.yaml"

requestCreateTapes :: CreateTapes -> TestTree
requestCreateTapes = req
    "CreateTapes"
    "fixture/CreateTapes.yaml"

requestUpdateVTLDeviceType :: UpdateVTLDeviceType -> TestTree
requestUpdateVTLDeviceType = req
    "UpdateVTLDeviceType"
    "fixture/UpdateVTLDeviceType.yaml"

requestCreateCachediSCSIVolume :: CreateCachediSCSIVolume -> TestTree
requestCreateCachediSCSIVolume = req
    "CreateCachediSCSIVolume"
    "fixture/CreateCachediSCSIVolume.yaml"

requestListFileShares :: ListFileShares -> TestTree
requestListFileShares = req
    "ListFileShares"
    "fixture/ListFileShares.yaml"

requestDeleteFileShare :: DeleteFileShare -> TestTree
requestDeleteFileShare = req
    "DeleteFileShare"
    "fixture/DeleteFileShare.yaml"

requestListVolumeInitiators :: ListVolumeInitiators -> TestTree
requestListVolumeInitiators = req
    "ListVolumeInitiators"
    "fixture/ListVolumeInitiators.yaml"

requestAddUploadBuffer :: AddUploadBuffer -> TestTree
requestAddUploadBuffer = req
    "AddUploadBuffer"
    "fixture/AddUploadBuffer.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestNotifyWhenUploaded :: NotifyWhenUploaded -> TestTree
requestNotifyWhenUploaded = req
    "NotifyWhenUploaded"
    "fixture/NotifyWhenUploaded.yaml"

requestUpdateGatewayInformation :: UpdateGatewayInformation -> TestTree
requestUpdateGatewayInformation = req
    "UpdateGatewayInformation"
    "fixture/UpdateGatewayInformation.yaml"

requestDescribeMaintenanceStartTime :: DescribeMaintenanceStartTime -> TestTree
requestDescribeMaintenanceStartTime = req
    "DescribeMaintenanceStartTime"
    "fixture/DescribeMaintenanceStartTime.yaml"

requestDescribeWorkingStorage :: DescribeWorkingStorage -> TestTree
requestDescribeWorkingStorage = req
    "DescribeWorkingStorage"
    "fixture/DescribeWorkingStorage.yaml"

requestDescribeCachediSCSIVolumes :: DescribeCachediSCSIVolumes -> TestTree
requestDescribeCachediSCSIVolumes = req
    "DescribeCachediSCSIVolumes"
    "fixture/DescribeCachediSCSIVolumes.yaml"

requestAddCache :: AddCache -> TestTree
requestAddCache = req
    "AddCache"
    "fixture/AddCache.yaml"

requestStartGateway :: StartGateway -> TestTree
requestStartGateway = req
    "StartGateway"
    "fixture/StartGateway.yaml"

requestShutdownGateway :: ShutdownGateway -> TestTree
requestShutdownGateway = req
    "ShutdownGateway"
    "fixture/ShutdownGateway.yaml"

requestUpdateGatewaySoftwareNow :: UpdateGatewaySoftwareNow -> TestTree
requestUpdateGatewaySoftwareNow = req
    "UpdateGatewaySoftwareNow"
    "fixture/UpdateGatewaySoftwareNow.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource = req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestDeleteChapCredentials :: DeleteChapCredentials -> TestTree
requestDeleteChapCredentials = req
    "DeleteChapCredentials"
    "fixture/DeleteChapCredentials.yaml"

requestUpdateChapCredentials :: UpdateChapCredentials -> TestTree
requestUpdateChapCredentials = req
    "UpdateChapCredentials"
    "fixture/UpdateChapCredentials.yaml"

requestDescribeUploadBuffer :: DescribeUploadBuffer -> TestTree
requestDescribeUploadBuffer = req
    "DescribeUploadBuffer"
    "fixture/DescribeUploadBuffer.yaml"

requestDescribeTapes :: DescribeTapes -> TestTree
requestDescribeTapes = req
    "DescribeTapes"
    "fixture/DescribeTapes.yaml"

requestDescribeStorediSCSIVolumes :: DescribeStorediSCSIVolumes -> TestTree
requestDescribeStorediSCSIVolumes = req
    "DescribeStorediSCSIVolumes"
    "fixture/DescribeStorediSCSIVolumes.yaml"

requestCreateSnapshotFromVolumeRecoveryPoint :: CreateSnapshotFromVolumeRecoveryPoint -> TestTree
requestCreateSnapshotFromVolumeRecoveryPoint = req
    "CreateSnapshotFromVolumeRecoveryPoint"
    "fixture/CreateSnapshotFromVolumeRecoveryPoint.yaml"

requestRetrieveTapeRecoveryPoint :: RetrieveTapeRecoveryPoint -> TestTree
requestRetrieveTapeRecoveryPoint = req
    "RetrieveTapeRecoveryPoint"
    "fixture/RetrieveTapeRecoveryPoint.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource = req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestDeleteGateway :: DeleteGateway -> TestTree
requestDeleteGateway = req
    "DeleteGateway"
    "fixture/DeleteGateway.yaml"

requestUpdateMaintenanceStartTime :: UpdateMaintenanceStartTime -> TestTree
requestUpdateMaintenanceStartTime = req
    "UpdateMaintenanceStartTime"
    "fixture/UpdateMaintenanceStartTime.yaml"

requestDescribeGatewayInformation :: DescribeGatewayInformation -> TestTree
requestDescribeGatewayInformation = req
    "DescribeGatewayInformation"
    "fixture/DescribeGatewayInformation.yaml"

requestRefreshCache :: RefreshCache -> TestTree
requestRefreshCache = req
    "RefreshCache"
    "fixture/RefreshCache.yaml"

requestUpdateNFSFileShare :: UpdateNFSFileShare -> TestTree
requestUpdateNFSFileShare = req
    "UpdateNFSFileShare"
    "fixture/UpdateNFSFileShare.yaml"

requestRetrieveTapeArchive :: RetrieveTapeArchive -> TestTree
requestRetrieveTapeArchive = req
    "RetrieveTapeArchive"
    "fixture/RetrieveTapeArchive.yaml"

requestDescribeTapeArchives :: DescribeTapeArchives -> TestTree
requestDescribeTapeArchives = req
    "DescribeTapeArchives"
    "fixture/DescribeTapeArchives.yaml"

requestDisableGateway :: DisableGateway -> TestTree
requestDisableGateway = req
    "DisableGateway"
    "fixture/DisableGateway.yaml"

requestDescribeSnapshotSchedule :: DescribeSnapshotSchedule -> TestTree
requestDescribeSnapshotSchedule = req
    "DescribeSnapshotSchedule"
    "fixture/DescribeSnapshotSchedule.yaml"

requestCreateTapeWithBarcode :: CreateTapeWithBarcode -> TestTree
requestCreateTapeWithBarcode = req
    "CreateTapeWithBarcode"
    "fixture/CreateTapeWithBarcode.yaml"

requestDescribeBandwidthRateLimit :: DescribeBandwidthRateLimit -> TestTree
requestDescribeBandwidthRateLimit = req
    "DescribeBandwidthRateLimit"
    "fixture/DescribeBandwidthRateLimit.yaml"

requestDeleteSnapshotSchedule :: DeleteSnapshotSchedule -> TestTree
requestDeleteSnapshotSchedule = req
    "DeleteSnapshotSchedule"
    "fixture/DeleteSnapshotSchedule.yaml"

requestUpdateSnapshotSchedule :: UpdateSnapshotSchedule -> TestTree
requestUpdateSnapshotSchedule = req
    "UpdateSnapshotSchedule"
    "fixture/UpdateSnapshotSchedule.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot = req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestCancelRetrieval :: CancelRetrieval -> TestTree
requestCancelRetrieval = req
    "CancelRetrieval"
    "fixture/CancelRetrieval.yaml"

requestDescribeVTLDevices :: DescribeVTLDevices -> TestTree
requestDescribeVTLDevices = req
    "DescribeVTLDevices"
    "fixture/DescribeVTLDevices.yaml"

requestDeleteTapeArchive :: DeleteTapeArchive -> TestTree
requestDeleteTapeArchive = req
    "DeleteTapeArchive"
    "fixture/DeleteTapeArchive.yaml"

requestDescribeNFSFileShares :: DescribeNFSFileShares -> TestTree
requestDescribeNFSFileShares = req
    "DescribeNFSFileShares"
    "fixture/DescribeNFSFileShares.yaml"

requestListVolumeRecoveryPoints :: ListVolumeRecoveryPoints -> TestTree
requestListVolumeRecoveryPoints = req
    "ListVolumeRecoveryPoints"
    "fixture/ListVolumeRecoveryPoints.yaml"

requestListTapes :: ListTapes -> TestTree
requestListTapes = req
    "ListTapes"
    "fixture/ListTapes.yaml"

requestResetCache :: ResetCache -> TestTree
requestResetCache = req
    "ResetCache"
    "fixture/ResetCache.yaml"

requestListGateways :: ListGateways -> TestTree
requestListGateways = req
    "ListGateways"
    "fixture/ListGateways.yaml"

requestDeleteTape :: DeleteTape -> TestTree
requestDeleteTape = req
    "DeleteTape"
    "fixture/DeleteTape.yaml"

requestListLocalDisks :: ListLocalDisks -> TestTree
requestListLocalDisks = req
    "ListLocalDisks"
    "fixture/ListLocalDisks.yaml"

requestListVolumes :: ListVolumes -> TestTree
requestListVolumes = req
    "ListVolumes"
    "fixture/ListVolumes.yaml"

requestUpdateBandwidthRateLimit :: UpdateBandwidthRateLimit -> TestTree
requestUpdateBandwidthRateLimit = req
    "UpdateBandwidthRateLimit"
    "fixture/UpdateBandwidthRateLimit.yaml"

requestAddWorkingStorage :: AddWorkingStorage -> TestTree
requestAddWorkingStorage = req
    "AddWorkingStorage"
    "fixture/AddWorkingStorage.yaml"

requestDescribeTapeRecoveryPoints :: DescribeTapeRecoveryPoints -> TestTree
requestDescribeTapeRecoveryPoints = req
    "DescribeTapeRecoveryPoints"
    "fixture/DescribeTapeRecoveryPoints.yaml"

requestDeleteBandwidthRateLimit :: DeleteBandwidthRateLimit -> TestTree
requestDeleteBandwidthRateLimit = req
    "DeleteBandwidthRateLimit"
    "fixture/DeleteBandwidthRateLimit.yaml"

requestActivateGateway :: ActivateGateway -> TestTree
requestActivateGateway = req
    "ActivateGateway"
    "fixture/ActivateGateway.yaml"

requestDescribeCache :: DescribeCache -> TestTree
requestDescribeCache = req
    "DescribeCache"
    "fixture/DescribeCache.yaml"

requestDeleteVolume :: DeleteVolume -> TestTree
requestDeleteVolume = req
    "DeleteVolume"
    "fixture/DeleteVolume.yaml"

-- Responses

responseCancelArchival :: CancelArchivalResponse -> TestTree
responseCancelArchival = res
    "CancelArchivalResponse"
    "fixture/CancelArchivalResponse.proto"
    storageGateway
    (Proxy :: Proxy CancelArchival)

responseCreateStorediSCSIVolume :: CreateStorediSCSIVolumeResponse -> TestTree
responseCreateStorediSCSIVolume = res
    "CreateStorediSCSIVolumeResponse"
    "fixture/CreateStorediSCSIVolumeResponse.proto"
    storageGateway
    (Proxy :: Proxy CreateStorediSCSIVolume)

responseCreateNFSFileShare :: CreateNFSFileShareResponse -> TestTree
responseCreateNFSFileShare = res
    "CreateNFSFileShareResponse"
    "fixture/CreateNFSFileShareResponse.proto"
    storageGateway
    (Proxy :: Proxy CreateNFSFileShare)

responseDescribeChapCredentials :: DescribeChapCredentialsResponse -> TestTree
responseDescribeChapCredentials = res
    "DescribeChapCredentialsResponse"
    "fixture/DescribeChapCredentialsResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeChapCredentials)

responseSetLocalConsolePassword :: SetLocalConsolePasswordResponse -> TestTree
responseSetLocalConsolePassword = res
    "SetLocalConsolePasswordResponse"
    "fixture/SetLocalConsolePasswordResponse.proto"
    storageGateway
    (Proxy :: Proxy SetLocalConsolePassword)

responseCreateTapes :: CreateTapesResponse -> TestTree
responseCreateTapes = res
    "CreateTapesResponse"
    "fixture/CreateTapesResponse.proto"
    storageGateway
    (Proxy :: Proxy CreateTapes)

responseUpdateVTLDeviceType :: UpdateVTLDeviceTypeResponse -> TestTree
responseUpdateVTLDeviceType = res
    "UpdateVTLDeviceTypeResponse"
    "fixture/UpdateVTLDeviceTypeResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateVTLDeviceType)

responseCreateCachediSCSIVolume :: CreateCachediSCSIVolumeResponse -> TestTree
responseCreateCachediSCSIVolume = res
    "CreateCachediSCSIVolumeResponse"
    "fixture/CreateCachediSCSIVolumeResponse.proto"
    storageGateway
    (Proxy :: Proxy CreateCachediSCSIVolume)

responseListFileShares :: ListFileSharesResponse -> TestTree
responseListFileShares = res
    "ListFileSharesResponse"
    "fixture/ListFileSharesResponse.proto"
    storageGateway
    (Proxy :: Proxy ListFileShares)

responseDeleteFileShare :: DeleteFileShareResponse -> TestTree
responseDeleteFileShare = res
    "DeleteFileShareResponse"
    "fixture/DeleteFileShareResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteFileShare)

responseListVolumeInitiators :: ListVolumeInitiatorsResponse -> TestTree
responseListVolumeInitiators = res
    "ListVolumeInitiatorsResponse"
    "fixture/ListVolumeInitiatorsResponse.proto"
    storageGateway
    (Proxy :: Proxy ListVolumeInitiators)

responseAddUploadBuffer :: AddUploadBufferResponse -> TestTree
responseAddUploadBuffer = res
    "AddUploadBufferResponse"
    "fixture/AddUploadBufferResponse.proto"
    storageGateway
    (Proxy :: Proxy AddUploadBuffer)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    storageGateway
    (Proxy :: Proxy ListTagsForResource)

responseNotifyWhenUploaded :: NotifyWhenUploadedResponse -> TestTree
responseNotifyWhenUploaded = res
    "NotifyWhenUploadedResponse"
    "fixture/NotifyWhenUploadedResponse.proto"
    storageGateway
    (Proxy :: Proxy NotifyWhenUploaded)

responseUpdateGatewayInformation :: UpdateGatewayInformationResponse -> TestTree
responseUpdateGatewayInformation = res
    "UpdateGatewayInformationResponse"
    "fixture/UpdateGatewayInformationResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateGatewayInformation)

responseDescribeMaintenanceStartTime :: DescribeMaintenanceStartTimeResponse -> TestTree
responseDescribeMaintenanceStartTime = res
    "DescribeMaintenanceStartTimeResponse"
    "fixture/DescribeMaintenanceStartTimeResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeMaintenanceStartTime)

responseDescribeWorkingStorage :: DescribeWorkingStorageResponse -> TestTree
responseDescribeWorkingStorage = res
    "DescribeWorkingStorageResponse"
    "fixture/DescribeWorkingStorageResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeWorkingStorage)

responseDescribeCachediSCSIVolumes :: DescribeCachediSCSIVolumesResponse -> TestTree
responseDescribeCachediSCSIVolumes = res
    "DescribeCachediSCSIVolumesResponse"
    "fixture/DescribeCachediSCSIVolumesResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeCachediSCSIVolumes)

responseAddCache :: AddCacheResponse -> TestTree
responseAddCache = res
    "AddCacheResponse"
    "fixture/AddCacheResponse.proto"
    storageGateway
    (Proxy :: Proxy AddCache)

responseStartGateway :: StartGatewayResponse -> TestTree
responseStartGateway = res
    "StartGatewayResponse"
    "fixture/StartGatewayResponse.proto"
    storageGateway
    (Proxy :: Proxy StartGateway)

responseShutdownGateway :: ShutdownGatewayResponse -> TestTree
responseShutdownGateway = res
    "ShutdownGatewayResponse"
    "fixture/ShutdownGatewayResponse.proto"
    storageGateway
    (Proxy :: Proxy ShutdownGateway)

responseUpdateGatewaySoftwareNow :: UpdateGatewaySoftwareNowResponse -> TestTree
responseUpdateGatewaySoftwareNow = res
    "UpdateGatewaySoftwareNowResponse"
    "fixture/UpdateGatewaySoftwareNowResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateGatewaySoftwareNow)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource = res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    storageGateway
    (Proxy :: Proxy RemoveTagsFromResource)

responseDeleteChapCredentials :: DeleteChapCredentialsResponse -> TestTree
responseDeleteChapCredentials = res
    "DeleteChapCredentialsResponse"
    "fixture/DeleteChapCredentialsResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteChapCredentials)

responseUpdateChapCredentials :: UpdateChapCredentialsResponse -> TestTree
responseUpdateChapCredentials = res
    "UpdateChapCredentialsResponse"
    "fixture/UpdateChapCredentialsResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateChapCredentials)

responseDescribeUploadBuffer :: DescribeUploadBufferResponse -> TestTree
responseDescribeUploadBuffer = res
    "DescribeUploadBufferResponse"
    "fixture/DescribeUploadBufferResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeUploadBuffer)

responseDescribeTapes :: DescribeTapesResponse -> TestTree
responseDescribeTapes = res
    "DescribeTapesResponse"
    "fixture/DescribeTapesResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeTapes)

responseDescribeStorediSCSIVolumes :: DescribeStorediSCSIVolumesResponse -> TestTree
responseDescribeStorediSCSIVolumes = res
    "DescribeStorediSCSIVolumesResponse"
    "fixture/DescribeStorediSCSIVolumesResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeStorediSCSIVolumes)

responseCreateSnapshotFromVolumeRecoveryPoint :: CreateSnapshotFromVolumeRecoveryPointResponse -> TestTree
responseCreateSnapshotFromVolumeRecoveryPoint = res
    "CreateSnapshotFromVolumeRecoveryPointResponse"
    "fixture/CreateSnapshotFromVolumeRecoveryPointResponse.proto"
    storageGateway
    (Proxy :: Proxy CreateSnapshotFromVolumeRecoveryPoint)

responseRetrieveTapeRecoveryPoint :: RetrieveTapeRecoveryPointResponse -> TestTree
responseRetrieveTapeRecoveryPoint = res
    "RetrieveTapeRecoveryPointResponse"
    "fixture/RetrieveTapeRecoveryPointResponse.proto"
    storageGateway
    (Proxy :: Proxy RetrieveTapeRecoveryPoint)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource = res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    storageGateway
    (Proxy :: Proxy AddTagsToResource)

responseDeleteGateway :: DeleteGatewayResponse -> TestTree
responseDeleteGateway = res
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteGateway)

responseUpdateMaintenanceStartTime :: UpdateMaintenanceStartTimeResponse -> TestTree
responseUpdateMaintenanceStartTime = res
    "UpdateMaintenanceStartTimeResponse"
    "fixture/UpdateMaintenanceStartTimeResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateMaintenanceStartTime)

responseDescribeGatewayInformation :: DescribeGatewayInformationResponse -> TestTree
responseDescribeGatewayInformation = res
    "DescribeGatewayInformationResponse"
    "fixture/DescribeGatewayInformationResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeGatewayInformation)

responseRefreshCache :: RefreshCacheResponse -> TestTree
responseRefreshCache = res
    "RefreshCacheResponse"
    "fixture/RefreshCacheResponse.proto"
    storageGateway
    (Proxy :: Proxy RefreshCache)

responseUpdateNFSFileShare :: UpdateNFSFileShareResponse -> TestTree
responseUpdateNFSFileShare = res
    "UpdateNFSFileShareResponse"
    "fixture/UpdateNFSFileShareResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateNFSFileShare)

responseRetrieveTapeArchive :: RetrieveTapeArchiveResponse -> TestTree
responseRetrieveTapeArchive = res
    "RetrieveTapeArchiveResponse"
    "fixture/RetrieveTapeArchiveResponse.proto"
    storageGateway
    (Proxy :: Proxy RetrieveTapeArchive)

responseDescribeTapeArchives :: DescribeTapeArchivesResponse -> TestTree
responseDescribeTapeArchives = res
    "DescribeTapeArchivesResponse"
    "fixture/DescribeTapeArchivesResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeTapeArchives)

responseDisableGateway :: DisableGatewayResponse -> TestTree
responseDisableGateway = res
    "DisableGatewayResponse"
    "fixture/DisableGatewayResponse.proto"
    storageGateway
    (Proxy :: Proxy DisableGateway)

responseDescribeSnapshotSchedule :: DescribeSnapshotScheduleResponse -> TestTree
responseDescribeSnapshotSchedule = res
    "DescribeSnapshotScheduleResponse"
    "fixture/DescribeSnapshotScheduleResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeSnapshotSchedule)

responseCreateTapeWithBarcode :: CreateTapeWithBarcodeResponse -> TestTree
responseCreateTapeWithBarcode = res
    "CreateTapeWithBarcodeResponse"
    "fixture/CreateTapeWithBarcodeResponse.proto"
    storageGateway
    (Proxy :: Proxy CreateTapeWithBarcode)

responseDescribeBandwidthRateLimit :: DescribeBandwidthRateLimitResponse -> TestTree
responseDescribeBandwidthRateLimit = res
    "DescribeBandwidthRateLimitResponse"
    "fixture/DescribeBandwidthRateLimitResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeBandwidthRateLimit)

responseDeleteSnapshotSchedule :: DeleteSnapshotScheduleResponse -> TestTree
responseDeleteSnapshotSchedule = res
    "DeleteSnapshotScheduleResponse"
    "fixture/DeleteSnapshotScheduleResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteSnapshotSchedule)

responseUpdateSnapshotSchedule :: UpdateSnapshotScheduleResponse -> TestTree
responseUpdateSnapshotSchedule = res
    "UpdateSnapshotScheduleResponse"
    "fixture/UpdateSnapshotScheduleResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateSnapshotSchedule)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot = res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    storageGateway
    (Proxy :: Proxy CreateSnapshot)

responseCancelRetrieval :: CancelRetrievalResponse -> TestTree
responseCancelRetrieval = res
    "CancelRetrievalResponse"
    "fixture/CancelRetrievalResponse.proto"
    storageGateway
    (Proxy :: Proxy CancelRetrieval)

responseDescribeVTLDevices :: DescribeVTLDevicesResponse -> TestTree
responseDescribeVTLDevices = res
    "DescribeVTLDevicesResponse"
    "fixture/DescribeVTLDevicesResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeVTLDevices)

responseDeleteTapeArchive :: DeleteTapeArchiveResponse -> TestTree
responseDeleteTapeArchive = res
    "DeleteTapeArchiveResponse"
    "fixture/DeleteTapeArchiveResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteTapeArchive)

responseDescribeNFSFileShares :: DescribeNFSFileSharesResponse -> TestTree
responseDescribeNFSFileShares = res
    "DescribeNFSFileSharesResponse"
    "fixture/DescribeNFSFileSharesResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeNFSFileShares)

responseListVolumeRecoveryPoints :: ListVolumeRecoveryPointsResponse -> TestTree
responseListVolumeRecoveryPoints = res
    "ListVolumeRecoveryPointsResponse"
    "fixture/ListVolumeRecoveryPointsResponse.proto"
    storageGateway
    (Proxy :: Proxy ListVolumeRecoveryPoints)

responseListTapes :: ListTapesResponse -> TestTree
responseListTapes = res
    "ListTapesResponse"
    "fixture/ListTapesResponse.proto"
    storageGateway
    (Proxy :: Proxy ListTapes)

responseResetCache :: ResetCacheResponse -> TestTree
responseResetCache = res
    "ResetCacheResponse"
    "fixture/ResetCacheResponse.proto"
    storageGateway
    (Proxy :: Proxy ResetCache)

responseListGateways :: ListGatewaysResponse -> TestTree
responseListGateways = res
    "ListGatewaysResponse"
    "fixture/ListGatewaysResponse.proto"
    storageGateway
    (Proxy :: Proxy ListGateways)

responseDeleteTape :: DeleteTapeResponse -> TestTree
responseDeleteTape = res
    "DeleteTapeResponse"
    "fixture/DeleteTapeResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteTape)

responseListLocalDisks :: ListLocalDisksResponse -> TestTree
responseListLocalDisks = res
    "ListLocalDisksResponse"
    "fixture/ListLocalDisksResponse.proto"
    storageGateway
    (Proxy :: Proxy ListLocalDisks)

responseListVolumes :: ListVolumesResponse -> TestTree
responseListVolumes = res
    "ListVolumesResponse"
    "fixture/ListVolumesResponse.proto"
    storageGateway
    (Proxy :: Proxy ListVolumes)

responseUpdateBandwidthRateLimit :: UpdateBandwidthRateLimitResponse -> TestTree
responseUpdateBandwidthRateLimit = res
    "UpdateBandwidthRateLimitResponse"
    "fixture/UpdateBandwidthRateLimitResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateBandwidthRateLimit)

responseAddWorkingStorage :: AddWorkingStorageResponse -> TestTree
responseAddWorkingStorage = res
    "AddWorkingStorageResponse"
    "fixture/AddWorkingStorageResponse.proto"
    storageGateway
    (Proxy :: Proxy AddWorkingStorage)

responseDescribeTapeRecoveryPoints :: DescribeTapeRecoveryPointsResponse -> TestTree
responseDescribeTapeRecoveryPoints = res
    "DescribeTapeRecoveryPointsResponse"
    "fixture/DescribeTapeRecoveryPointsResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeTapeRecoveryPoints)

responseDeleteBandwidthRateLimit :: DeleteBandwidthRateLimitResponse -> TestTree
responseDeleteBandwidthRateLimit = res
    "DeleteBandwidthRateLimitResponse"
    "fixture/DeleteBandwidthRateLimitResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteBandwidthRateLimit)

responseActivateGateway :: ActivateGatewayResponse -> TestTree
responseActivateGateway = res
    "ActivateGatewayResponse"
    "fixture/ActivateGatewayResponse.proto"
    storageGateway
    (Proxy :: Proxy ActivateGateway)

responseDescribeCache :: DescribeCacheResponse -> TestTree
responseDescribeCache = res
    "DescribeCacheResponse"
    "fixture/DescribeCacheResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeCache)

responseDeleteVolume :: DeleteVolumeResponse -> TestTree
responseDeleteVolume = res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteVolume)
