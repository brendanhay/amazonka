{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.StorageGateway
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.StorageGateway where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.StorageGateway
import Test.AWS.StorageGateway.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testCancelArchival $
--             cancelArchival
--
--         , testCreateStorediSCSIVolume $
--             createStorediSCSIVolume
--
--         , testDescribeChapCredentials $
--             describeChapCredentials
--
--         , testSetLocalConsolePassword $
--             setLocalConsolePassword
--
--         , testCreateTapes $
--             createTapes
--
--         , testUpdateVTLDeviceType $
--             updateVTLDeviceType
--
--         , testCreateCachediSCSIVolume $
--             createCachediSCSIVolume
--
--         , testListVolumeInitiators $
--             listVolumeInitiators
--
--         , testAddUploadBuffer $
--             addUploadBuffer
--
--         , testListTagsForResource $
--             listTagsForResource
--
--         , testUpdateGatewayInformation $
--             updateGatewayInformation
--
--         , testDescribeMaintenanceStartTime $
--             describeMaintenanceStartTime
--
--         , testDescribeWorkingStorage $
--             describeWorkingStorage
--
--         , testDescribeCachediSCSIVolumes $
--             describeCachediSCSIVolumes
--
--         , testAddCache $
--             addCache
--
--         , testStartGateway $
--             startGateway
--
--         , testShutdownGateway $
--             shutdownGateway
--
--         , testUpdateGatewaySoftwareNow $
--             updateGatewaySoftwareNow
--
--         , testRemoveTagsFromResource $
--             removeTagsFromResource
--
--         , testDeleteChapCredentials $
--             deleteChapCredentials
--
--         , testUpdateChapCredentials $
--             updateChapCredentials
--
--         , testDescribeUploadBuffer $
--             describeUploadBuffer
--
--         , testDescribeTapes $
--             describeTapes
--
--         , testDescribeStorediSCSIVolumes $
--             describeStorediSCSIVolumes
--
--         , testCreateSnapshotFromVolumeRecoveryPoint $
--             createSnapshotFromVolumeRecoveryPoint
--
--         , testRetrieveTapeRecoveryPoint $
--             retrieveTapeRecoveryPoint
--
--         , testAddTagsToResource $
--             addTagsToResource
--
--         , testDeleteGateway $
--             deleteGateway
--
--         , testUpdateMaintenanceStartTime $
--             updateMaintenanceStartTime
--
--         , testDescribeGatewayInformation $
--             describeGatewayInformation
--
--         , testRetrieveTapeArchive $
--             retrieveTapeArchive
--
--         , testDescribeTapeArchives $
--             describeTapeArchives
--
--         , testDisableGateway $
--             disableGateway
--
--         , testDescribeSnapshotSchedule $
--             describeSnapshotSchedule
--
--         , testCreateTapeWithBarcode $
--             createTapeWithBarcode
--
--         , testDescribeBandwidthRateLimit $
--             describeBandwidthRateLimit
--
--         , testDeleteSnapshotSchedule $
--             deleteSnapshotSchedule
--
--         , testUpdateSnapshotSchedule $
--             updateSnapshotSchedule
--
--         , testCreateSnapshot $
--             createSnapshot
--
--         , testCancelRetrieval $
--             cancelRetrieval
--
--         , testDescribeVTLDevices $
--             describeVTLDevices
--
--         , testDeleteTapeArchive $
--             deleteTapeArchive
--
--         , testListVolumeRecoveryPoints $
--             listVolumeRecoveryPoints
--
--         , testResetCache $
--             resetCache
--
--         , testListGateways $
--             listGateways
--
--         , testDeleteTape $
--             deleteTape
--
--         , testListLocalDisks $
--             listLocalDisks
--
--         , testListVolumes $
--             listVolumes
--
--         , testUpdateBandwidthRateLimit $
--             updateBandwidthRateLimit
--
--         , testAddWorkingStorage $
--             addWorkingStorage
--
--         , testDescribeTapeRecoveryPoints $
--             describeTapeRecoveryPoints
--
--         , testDeleteBandwidthRateLimit $
--             deleteBandwidthRateLimit
--
--         , testActivateGateway $
--             activateGateway
--
--         , testDescribeCache $
--             describeCache
--
--         , testDeleteVolume $
--             deleteVolume
--
--           ]

--     , testGroup "response"
--         [ testCancelArchivalResponse $
--             cancelArchivalResponse
--
--         , testCreateStorediSCSIVolumeResponse $
--             createStorediSCSIVolumeResponse
--
--         , testDescribeChapCredentialsResponse $
--             describeChapCredentialsResponse
--
--         , testSetLocalConsolePasswordResponse $
--             setLocalConsolePasswordResponse
--
--         , testCreateTapesResponse $
--             createTapesResponse
--
--         , testUpdateVTLDeviceTypeResponse $
--             updateVTLDeviceTypeResponse
--
--         , testCreateCachediSCSIVolumeResponse $
--             createCachediSCSIVolumeResponse
--
--         , testListVolumeInitiatorsResponse $
--             listVolumeInitiatorsResponse
--
--         , testAddUploadBufferResponse $
--             addUploadBufferResponse
--
--         , testListTagsForResourceResponse $
--             listTagsForResourceResponse
--
--         , testUpdateGatewayInformationResponse $
--             updateGatewayInformationResponse
--
--         , testDescribeMaintenanceStartTimeResponse $
--             describeMaintenanceStartTimeResponse
--
--         , testDescribeWorkingStorageResponse $
--             describeWorkingStorageResponse
--
--         , testDescribeCachediSCSIVolumesResponse $
--             describeCachediSCSIVolumesResponse
--
--         , testAddCacheResponse $
--             addCacheResponse
--
--         , testStartGatewayResponse $
--             startGatewayResponse
--
--         , testShutdownGatewayResponse $
--             shutdownGatewayResponse
--
--         , testUpdateGatewaySoftwareNowResponse $
--             updateGatewaySoftwareNowResponse
--
--         , testRemoveTagsFromResourceResponse $
--             removeTagsFromResourceResponse
--
--         , testDeleteChapCredentialsResponse $
--             deleteChapCredentialsResponse
--
--         , testUpdateChapCredentialsResponse $
--             updateChapCredentialsResponse
--
--         , testDescribeUploadBufferResponse $
--             describeUploadBufferResponse
--
--         , testDescribeTapesResponse $
--             describeTapesResponse
--
--         , testDescribeStorediSCSIVolumesResponse $
--             describeStorediSCSIVolumesResponse
--
--         , testCreateSnapshotFromVolumeRecoveryPointResponse $
--             createSnapshotFromVolumeRecoveryPointResponse
--
--         , testRetrieveTapeRecoveryPointResponse $
--             retrieveTapeRecoveryPointResponse
--
--         , testAddTagsToResourceResponse $
--             addTagsToResourceResponse
--
--         , testDeleteGatewayResponse $
--             deleteGatewayResponse
--
--         , testUpdateMaintenanceStartTimeResponse $
--             updateMaintenanceStartTimeResponse
--
--         , testDescribeGatewayInformationResponse $
--             describeGatewayInformationResponse
--
--         , testRetrieveTapeArchiveResponse $
--             retrieveTapeArchiveResponse
--
--         , testDescribeTapeArchivesResponse $
--             describeTapeArchivesResponse
--
--         , testDisableGatewayResponse $
--             disableGatewayResponse
--
--         , testDescribeSnapshotScheduleResponse $
--             describeSnapshotScheduleResponse
--
--         , testCreateTapeWithBarcodeResponse $
--             createTapeWithBarcodeResponse
--
--         , testDescribeBandwidthRateLimitResponse $
--             describeBandwidthRateLimitResponse
--
--         , testDeleteSnapshotScheduleResponse $
--             deleteSnapshotScheduleResponse
--
--         , testUpdateSnapshotScheduleResponse $
--             updateSnapshotScheduleResponse
--
--         , testCreateSnapshotResponse $
--             createSnapshotResponse
--
--         , testCancelRetrievalResponse $
--             cancelRetrievalResponse
--
--         , testDescribeVTLDevicesResponse $
--             describeVTLDevicesResponse
--
--         , testDeleteTapeArchiveResponse $
--             deleteTapeArchiveResponse
--
--         , testListVolumeRecoveryPointsResponse $
--             listVolumeRecoveryPointsResponse
--
--         , testResetCacheResponse $
--             resetCacheResponse
--
--         , testListGatewaysResponse $
--             listGatewaysResponse
--
--         , testDeleteTapeResponse $
--             deleteTapeResponse
--
--         , testListLocalDisksResponse $
--             listLocalDisksResponse
--
--         , testListVolumesResponse $
--             listVolumesResponse
--
--         , testUpdateBandwidthRateLimitResponse $
--             updateBandwidthRateLimitResponse
--
--         , testAddWorkingStorageResponse $
--             addWorkingStorageResponse
--
--         , testDescribeTapeRecoveryPointsResponse $
--             describeTapeRecoveryPointsResponse
--
--         , testDeleteBandwidthRateLimitResponse $
--             deleteBandwidthRateLimitResponse
--
--         , testActivateGatewayResponse $
--             activateGatewayResponse
--
--         , testDescribeCacheResponse $
--             describeCacheResponse
--
--         , testDeleteVolumeResponse $
--             deleteVolumeResponse
--
--           ]
--     ]

-- Requests

testCancelArchival :: CancelArchival -> TestTree
testCancelArchival = req
    "CancelArchival"
    "fixture/CancelArchival.yaml"

testCreateStorediSCSIVolume :: CreateStorediSCSIVolume -> TestTree
testCreateStorediSCSIVolume = req
    "CreateStorediSCSIVolume"
    "fixture/CreateStorediSCSIVolume.yaml"

testDescribeChapCredentials :: DescribeChapCredentials -> TestTree
testDescribeChapCredentials = req
    "DescribeChapCredentials"
    "fixture/DescribeChapCredentials.yaml"

testSetLocalConsolePassword :: SetLocalConsolePassword -> TestTree
testSetLocalConsolePassword = req
    "SetLocalConsolePassword"
    "fixture/SetLocalConsolePassword.yaml"

testCreateTapes :: CreateTapes -> TestTree
testCreateTapes = req
    "CreateTapes"
    "fixture/CreateTapes.yaml"

testUpdateVTLDeviceType :: UpdateVTLDeviceType -> TestTree
testUpdateVTLDeviceType = req
    "UpdateVTLDeviceType"
    "fixture/UpdateVTLDeviceType.yaml"

testCreateCachediSCSIVolume :: CreateCachediSCSIVolume -> TestTree
testCreateCachediSCSIVolume = req
    "CreateCachediSCSIVolume"
    "fixture/CreateCachediSCSIVolume.yaml"

testListVolumeInitiators :: ListVolumeInitiators -> TestTree
testListVolumeInitiators = req
    "ListVolumeInitiators"
    "fixture/ListVolumeInitiators.yaml"

testAddUploadBuffer :: AddUploadBuffer -> TestTree
testAddUploadBuffer = req
    "AddUploadBuffer"
    "fixture/AddUploadBuffer.yaml"

testListTagsForResource :: ListTagsForResource -> TestTree
testListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

testUpdateGatewayInformation :: UpdateGatewayInformation -> TestTree
testUpdateGatewayInformation = req
    "UpdateGatewayInformation"
    "fixture/UpdateGatewayInformation.yaml"

testDescribeMaintenanceStartTime :: DescribeMaintenanceStartTime -> TestTree
testDescribeMaintenanceStartTime = req
    "DescribeMaintenanceStartTime"
    "fixture/DescribeMaintenanceStartTime.yaml"

testDescribeWorkingStorage :: DescribeWorkingStorage -> TestTree
testDescribeWorkingStorage = req
    "DescribeWorkingStorage"
    "fixture/DescribeWorkingStorage.yaml"

testDescribeCachediSCSIVolumes :: DescribeCachediSCSIVolumes -> TestTree
testDescribeCachediSCSIVolumes = req
    "DescribeCachediSCSIVolumes"
    "fixture/DescribeCachediSCSIVolumes.yaml"

testAddCache :: AddCache -> TestTree
testAddCache = req
    "AddCache"
    "fixture/AddCache.yaml"

testStartGateway :: StartGateway -> TestTree
testStartGateway = req
    "StartGateway"
    "fixture/StartGateway.yaml"

testShutdownGateway :: ShutdownGateway -> TestTree
testShutdownGateway = req
    "ShutdownGateway"
    "fixture/ShutdownGateway.yaml"

testUpdateGatewaySoftwareNow :: UpdateGatewaySoftwareNow -> TestTree
testUpdateGatewaySoftwareNow = req
    "UpdateGatewaySoftwareNow"
    "fixture/UpdateGatewaySoftwareNow.yaml"

testRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
testRemoveTagsFromResource = req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

testDeleteChapCredentials :: DeleteChapCredentials -> TestTree
testDeleteChapCredentials = req
    "DeleteChapCredentials"
    "fixture/DeleteChapCredentials.yaml"

testUpdateChapCredentials :: UpdateChapCredentials -> TestTree
testUpdateChapCredentials = req
    "UpdateChapCredentials"
    "fixture/UpdateChapCredentials.yaml"

testDescribeUploadBuffer :: DescribeUploadBuffer -> TestTree
testDescribeUploadBuffer = req
    "DescribeUploadBuffer"
    "fixture/DescribeUploadBuffer.yaml"

testDescribeTapes :: DescribeTapes -> TestTree
testDescribeTapes = req
    "DescribeTapes"
    "fixture/DescribeTapes.yaml"

testDescribeStorediSCSIVolumes :: DescribeStorediSCSIVolumes -> TestTree
testDescribeStorediSCSIVolumes = req
    "DescribeStorediSCSIVolumes"
    "fixture/DescribeStorediSCSIVolumes.yaml"

testCreateSnapshotFromVolumeRecoveryPoint :: CreateSnapshotFromVolumeRecoveryPoint -> TestTree
testCreateSnapshotFromVolumeRecoveryPoint = req
    "CreateSnapshotFromVolumeRecoveryPoint"
    "fixture/CreateSnapshotFromVolumeRecoveryPoint.yaml"

testRetrieveTapeRecoveryPoint :: RetrieveTapeRecoveryPoint -> TestTree
testRetrieveTapeRecoveryPoint = req
    "RetrieveTapeRecoveryPoint"
    "fixture/RetrieveTapeRecoveryPoint.yaml"

testAddTagsToResource :: AddTagsToResource -> TestTree
testAddTagsToResource = req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

testDeleteGateway :: DeleteGateway -> TestTree
testDeleteGateway = req
    "DeleteGateway"
    "fixture/DeleteGateway.yaml"

testUpdateMaintenanceStartTime :: UpdateMaintenanceStartTime -> TestTree
testUpdateMaintenanceStartTime = req
    "UpdateMaintenanceStartTime"
    "fixture/UpdateMaintenanceStartTime.yaml"

testDescribeGatewayInformation :: DescribeGatewayInformation -> TestTree
testDescribeGatewayInformation = req
    "DescribeGatewayInformation"
    "fixture/DescribeGatewayInformation.yaml"

testRetrieveTapeArchive :: RetrieveTapeArchive -> TestTree
testRetrieveTapeArchive = req
    "RetrieveTapeArchive"
    "fixture/RetrieveTapeArchive.yaml"

testDescribeTapeArchives :: DescribeTapeArchives -> TestTree
testDescribeTapeArchives = req
    "DescribeTapeArchives"
    "fixture/DescribeTapeArchives.yaml"

testDisableGateway :: DisableGateway -> TestTree
testDisableGateway = req
    "DisableGateway"
    "fixture/DisableGateway.yaml"

testDescribeSnapshotSchedule :: DescribeSnapshotSchedule -> TestTree
testDescribeSnapshotSchedule = req
    "DescribeSnapshotSchedule"
    "fixture/DescribeSnapshotSchedule.yaml"

testCreateTapeWithBarcode :: CreateTapeWithBarcode -> TestTree
testCreateTapeWithBarcode = req
    "CreateTapeWithBarcode"
    "fixture/CreateTapeWithBarcode.yaml"

testDescribeBandwidthRateLimit :: DescribeBandwidthRateLimit -> TestTree
testDescribeBandwidthRateLimit = req
    "DescribeBandwidthRateLimit"
    "fixture/DescribeBandwidthRateLimit.yaml"

testDeleteSnapshotSchedule :: DeleteSnapshotSchedule -> TestTree
testDeleteSnapshotSchedule = req
    "DeleteSnapshotSchedule"
    "fixture/DeleteSnapshotSchedule.yaml"

testUpdateSnapshotSchedule :: UpdateSnapshotSchedule -> TestTree
testUpdateSnapshotSchedule = req
    "UpdateSnapshotSchedule"
    "fixture/UpdateSnapshotSchedule.yaml"

testCreateSnapshot :: CreateSnapshot -> TestTree
testCreateSnapshot = req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

testCancelRetrieval :: CancelRetrieval -> TestTree
testCancelRetrieval = req
    "CancelRetrieval"
    "fixture/CancelRetrieval.yaml"

testDescribeVTLDevices :: DescribeVTLDevices -> TestTree
testDescribeVTLDevices = req
    "DescribeVTLDevices"
    "fixture/DescribeVTLDevices.yaml"

testDeleteTapeArchive :: DeleteTapeArchive -> TestTree
testDeleteTapeArchive = req
    "DeleteTapeArchive"
    "fixture/DeleteTapeArchive.yaml"

testListVolumeRecoveryPoints :: ListVolumeRecoveryPoints -> TestTree
testListVolumeRecoveryPoints = req
    "ListVolumeRecoveryPoints"
    "fixture/ListVolumeRecoveryPoints.yaml"

testResetCache :: ResetCache -> TestTree
testResetCache = req
    "ResetCache"
    "fixture/ResetCache.yaml"

testListGateways :: ListGateways -> TestTree
testListGateways = req
    "ListGateways"
    "fixture/ListGateways.yaml"

testDeleteTape :: DeleteTape -> TestTree
testDeleteTape = req
    "DeleteTape"
    "fixture/DeleteTape.yaml"

testListLocalDisks :: ListLocalDisks -> TestTree
testListLocalDisks = req
    "ListLocalDisks"
    "fixture/ListLocalDisks.yaml"

testListVolumes :: ListVolumes -> TestTree
testListVolumes = req
    "ListVolumes"
    "fixture/ListVolumes.yaml"

testUpdateBandwidthRateLimit :: UpdateBandwidthRateLimit -> TestTree
testUpdateBandwidthRateLimit = req
    "UpdateBandwidthRateLimit"
    "fixture/UpdateBandwidthRateLimit.yaml"

testAddWorkingStorage :: AddWorkingStorage -> TestTree
testAddWorkingStorage = req
    "AddWorkingStorage"
    "fixture/AddWorkingStorage.yaml"

testDescribeTapeRecoveryPoints :: DescribeTapeRecoveryPoints -> TestTree
testDescribeTapeRecoveryPoints = req
    "DescribeTapeRecoveryPoints"
    "fixture/DescribeTapeRecoveryPoints.yaml"

testDeleteBandwidthRateLimit :: DeleteBandwidthRateLimit -> TestTree
testDeleteBandwidthRateLimit = req
    "DeleteBandwidthRateLimit"
    "fixture/DeleteBandwidthRateLimit.yaml"

testActivateGateway :: ActivateGateway -> TestTree
testActivateGateway = req
    "ActivateGateway"
    "fixture/ActivateGateway.yaml"

testDescribeCache :: DescribeCache -> TestTree
testDescribeCache = req
    "DescribeCache"
    "fixture/DescribeCache.yaml"

testDeleteVolume :: DeleteVolume -> TestTree
testDeleteVolume = req
    "DeleteVolume"
    "fixture/DeleteVolume.yaml"

-- Responses

testCancelArchivalResponse :: CancelArchivalResponse -> TestTree
testCancelArchivalResponse = res
    "CancelArchivalResponse"
    "fixture/CancelArchivalResponse.proto"
    storageGateway
    (Proxy :: Proxy CancelArchival)

testCreateStorediSCSIVolumeResponse :: CreateStorediSCSIVolumeResponse -> TestTree
testCreateStorediSCSIVolumeResponse = res
    "CreateStorediSCSIVolumeResponse"
    "fixture/CreateStorediSCSIVolumeResponse.proto"
    storageGateway
    (Proxy :: Proxy CreateStorediSCSIVolume)

testDescribeChapCredentialsResponse :: DescribeChapCredentialsResponse -> TestTree
testDescribeChapCredentialsResponse = res
    "DescribeChapCredentialsResponse"
    "fixture/DescribeChapCredentialsResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeChapCredentials)

testSetLocalConsolePasswordResponse :: SetLocalConsolePasswordResponse -> TestTree
testSetLocalConsolePasswordResponse = res
    "SetLocalConsolePasswordResponse"
    "fixture/SetLocalConsolePasswordResponse.proto"
    storageGateway
    (Proxy :: Proxy SetLocalConsolePassword)

testCreateTapesResponse :: CreateTapesResponse -> TestTree
testCreateTapesResponse = res
    "CreateTapesResponse"
    "fixture/CreateTapesResponse.proto"
    storageGateway
    (Proxy :: Proxy CreateTapes)

testUpdateVTLDeviceTypeResponse :: UpdateVTLDeviceTypeResponse -> TestTree
testUpdateVTLDeviceTypeResponse = res
    "UpdateVTLDeviceTypeResponse"
    "fixture/UpdateVTLDeviceTypeResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateVTLDeviceType)

testCreateCachediSCSIVolumeResponse :: CreateCachediSCSIVolumeResponse -> TestTree
testCreateCachediSCSIVolumeResponse = res
    "CreateCachediSCSIVolumeResponse"
    "fixture/CreateCachediSCSIVolumeResponse.proto"
    storageGateway
    (Proxy :: Proxy CreateCachediSCSIVolume)

testListVolumeInitiatorsResponse :: ListVolumeInitiatorsResponse -> TestTree
testListVolumeInitiatorsResponse = res
    "ListVolumeInitiatorsResponse"
    "fixture/ListVolumeInitiatorsResponse.proto"
    storageGateway
    (Proxy :: Proxy ListVolumeInitiators)

testAddUploadBufferResponse :: AddUploadBufferResponse -> TestTree
testAddUploadBufferResponse = res
    "AddUploadBufferResponse"
    "fixture/AddUploadBufferResponse.proto"
    storageGateway
    (Proxy :: Proxy AddUploadBuffer)

testListTagsForResourceResponse :: ListTagsForResourceResponse -> TestTree
testListTagsForResourceResponse = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    storageGateway
    (Proxy :: Proxy ListTagsForResource)

testUpdateGatewayInformationResponse :: UpdateGatewayInformationResponse -> TestTree
testUpdateGatewayInformationResponse = res
    "UpdateGatewayInformationResponse"
    "fixture/UpdateGatewayInformationResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateGatewayInformation)

testDescribeMaintenanceStartTimeResponse :: DescribeMaintenanceStartTimeResponse -> TestTree
testDescribeMaintenanceStartTimeResponse = res
    "DescribeMaintenanceStartTimeResponse"
    "fixture/DescribeMaintenanceStartTimeResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeMaintenanceStartTime)

testDescribeWorkingStorageResponse :: DescribeWorkingStorageResponse -> TestTree
testDescribeWorkingStorageResponse = res
    "DescribeWorkingStorageResponse"
    "fixture/DescribeWorkingStorageResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeWorkingStorage)

testDescribeCachediSCSIVolumesResponse :: DescribeCachediSCSIVolumesResponse -> TestTree
testDescribeCachediSCSIVolumesResponse = res
    "DescribeCachediSCSIVolumesResponse"
    "fixture/DescribeCachediSCSIVolumesResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeCachediSCSIVolumes)

testAddCacheResponse :: AddCacheResponse -> TestTree
testAddCacheResponse = res
    "AddCacheResponse"
    "fixture/AddCacheResponse.proto"
    storageGateway
    (Proxy :: Proxy AddCache)

testStartGatewayResponse :: StartGatewayResponse -> TestTree
testStartGatewayResponse = res
    "StartGatewayResponse"
    "fixture/StartGatewayResponse.proto"
    storageGateway
    (Proxy :: Proxy StartGateway)

testShutdownGatewayResponse :: ShutdownGatewayResponse -> TestTree
testShutdownGatewayResponse = res
    "ShutdownGatewayResponse"
    "fixture/ShutdownGatewayResponse.proto"
    storageGateway
    (Proxy :: Proxy ShutdownGateway)

testUpdateGatewaySoftwareNowResponse :: UpdateGatewaySoftwareNowResponse -> TestTree
testUpdateGatewaySoftwareNowResponse = res
    "UpdateGatewaySoftwareNowResponse"
    "fixture/UpdateGatewaySoftwareNowResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateGatewaySoftwareNow)

testRemoveTagsFromResourceResponse :: RemoveTagsFromResourceResponse -> TestTree
testRemoveTagsFromResourceResponse = res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    storageGateway
    (Proxy :: Proxy RemoveTagsFromResource)

testDeleteChapCredentialsResponse :: DeleteChapCredentialsResponse -> TestTree
testDeleteChapCredentialsResponse = res
    "DeleteChapCredentialsResponse"
    "fixture/DeleteChapCredentialsResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteChapCredentials)

testUpdateChapCredentialsResponse :: UpdateChapCredentialsResponse -> TestTree
testUpdateChapCredentialsResponse = res
    "UpdateChapCredentialsResponse"
    "fixture/UpdateChapCredentialsResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateChapCredentials)

testDescribeUploadBufferResponse :: DescribeUploadBufferResponse -> TestTree
testDescribeUploadBufferResponse = res
    "DescribeUploadBufferResponse"
    "fixture/DescribeUploadBufferResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeUploadBuffer)

testDescribeTapesResponse :: DescribeTapesResponse -> TestTree
testDescribeTapesResponse = res
    "DescribeTapesResponse"
    "fixture/DescribeTapesResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeTapes)

testDescribeStorediSCSIVolumesResponse :: DescribeStorediSCSIVolumesResponse -> TestTree
testDescribeStorediSCSIVolumesResponse = res
    "DescribeStorediSCSIVolumesResponse"
    "fixture/DescribeStorediSCSIVolumesResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeStorediSCSIVolumes)

testCreateSnapshotFromVolumeRecoveryPointResponse :: CreateSnapshotFromVolumeRecoveryPointResponse -> TestTree
testCreateSnapshotFromVolumeRecoveryPointResponse = res
    "CreateSnapshotFromVolumeRecoveryPointResponse"
    "fixture/CreateSnapshotFromVolumeRecoveryPointResponse.proto"
    storageGateway
    (Proxy :: Proxy CreateSnapshotFromVolumeRecoveryPoint)

testRetrieveTapeRecoveryPointResponse :: RetrieveTapeRecoveryPointResponse -> TestTree
testRetrieveTapeRecoveryPointResponse = res
    "RetrieveTapeRecoveryPointResponse"
    "fixture/RetrieveTapeRecoveryPointResponse.proto"
    storageGateway
    (Proxy :: Proxy RetrieveTapeRecoveryPoint)

testAddTagsToResourceResponse :: AddTagsToResourceResponse -> TestTree
testAddTagsToResourceResponse = res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    storageGateway
    (Proxy :: Proxy AddTagsToResource)

testDeleteGatewayResponse :: DeleteGatewayResponse -> TestTree
testDeleteGatewayResponse = res
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteGateway)

testUpdateMaintenanceStartTimeResponse :: UpdateMaintenanceStartTimeResponse -> TestTree
testUpdateMaintenanceStartTimeResponse = res
    "UpdateMaintenanceStartTimeResponse"
    "fixture/UpdateMaintenanceStartTimeResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateMaintenanceStartTime)

testDescribeGatewayInformationResponse :: DescribeGatewayInformationResponse -> TestTree
testDescribeGatewayInformationResponse = res
    "DescribeGatewayInformationResponse"
    "fixture/DescribeGatewayInformationResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeGatewayInformation)

testRetrieveTapeArchiveResponse :: RetrieveTapeArchiveResponse -> TestTree
testRetrieveTapeArchiveResponse = res
    "RetrieveTapeArchiveResponse"
    "fixture/RetrieveTapeArchiveResponse.proto"
    storageGateway
    (Proxy :: Proxy RetrieveTapeArchive)

testDescribeTapeArchivesResponse :: DescribeTapeArchivesResponse -> TestTree
testDescribeTapeArchivesResponse = res
    "DescribeTapeArchivesResponse"
    "fixture/DescribeTapeArchivesResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeTapeArchives)

testDisableGatewayResponse :: DisableGatewayResponse -> TestTree
testDisableGatewayResponse = res
    "DisableGatewayResponse"
    "fixture/DisableGatewayResponse.proto"
    storageGateway
    (Proxy :: Proxy DisableGateway)

testDescribeSnapshotScheduleResponse :: DescribeSnapshotScheduleResponse -> TestTree
testDescribeSnapshotScheduleResponse = res
    "DescribeSnapshotScheduleResponse"
    "fixture/DescribeSnapshotScheduleResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeSnapshotSchedule)

testCreateTapeWithBarcodeResponse :: CreateTapeWithBarcodeResponse -> TestTree
testCreateTapeWithBarcodeResponse = res
    "CreateTapeWithBarcodeResponse"
    "fixture/CreateTapeWithBarcodeResponse.proto"
    storageGateway
    (Proxy :: Proxy CreateTapeWithBarcode)

testDescribeBandwidthRateLimitResponse :: DescribeBandwidthRateLimitResponse -> TestTree
testDescribeBandwidthRateLimitResponse = res
    "DescribeBandwidthRateLimitResponse"
    "fixture/DescribeBandwidthRateLimitResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeBandwidthRateLimit)

testDeleteSnapshotScheduleResponse :: DeleteSnapshotScheduleResponse -> TestTree
testDeleteSnapshotScheduleResponse = res
    "DeleteSnapshotScheduleResponse"
    "fixture/DeleteSnapshotScheduleResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteSnapshotSchedule)

testUpdateSnapshotScheduleResponse :: UpdateSnapshotScheduleResponse -> TestTree
testUpdateSnapshotScheduleResponse = res
    "UpdateSnapshotScheduleResponse"
    "fixture/UpdateSnapshotScheduleResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateSnapshotSchedule)

testCreateSnapshotResponse :: CreateSnapshotResponse -> TestTree
testCreateSnapshotResponse = res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    storageGateway
    (Proxy :: Proxy CreateSnapshot)

testCancelRetrievalResponse :: CancelRetrievalResponse -> TestTree
testCancelRetrievalResponse = res
    "CancelRetrievalResponse"
    "fixture/CancelRetrievalResponse.proto"
    storageGateway
    (Proxy :: Proxy CancelRetrieval)

testDescribeVTLDevicesResponse :: DescribeVTLDevicesResponse -> TestTree
testDescribeVTLDevicesResponse = res
    "DescribeVTLDevicesResponse"
    "fixture/DescribeVTLDevicesResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeVTLDevices)

testDeleteTapeArchiveResponse :: DeleteTapeArchiveResponse -> TestTree
testDeleteTapeArchiveResponse = res
    "DeleteTapeArchiveResponse"
    "fixture/DeleteTapeArchiveResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteTapeArchive)

testListVolumeRecoveryPointsResponse :: ListVolumeRecoveryPointsResponse -> TestTree
testListVolumeRecoveryPointsResponse = res
    "ListVolumeRecoveryPointsResponse"
    "fixture/ListVolumeRecoveryPointsResponse.proto"
    storageGateway
    (Proxy :: Proxy ListVolumeRecoveryPoints)

testResetCacheResponse :: ResetCacheResponse -> TestTree
testResetCacheResponse = res
    "ResetCacheResponse"
    "fixture/ResetCacheResponse.proto"
    storageGateway
    (Proxy :: Proxy ResetCache)

testListGatewaysResponse :: ListGatewaysResponse -> TestTree
testListGatewaysResponse = res
    "ListGatewaysResponse"
    "fixture/ListGatewaysResponse.proto"
    storageGateway
    (Proxy :: Proxy ListGateways)

testDeleteTapeResponse :: DeleteTapeResponse -> TestTree
testDeleteTapeResponse = res
    "DeleteTapeResponse"
    "fixture/DeleteTapeResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteTape)

testListLocalDisksResponse :: ListLocalDisksResponse -> TestTree
testListLocalDisksResponse = res
    "ListLocalDisksResponse"
    "fixture/ListLocalDisksResponse.proto"
    storageGateway
    (Proxy :: Proxy ListLocalDisks)

testListVolumesResponse :: ListVolumesResponse -> TestTree
testListVolumesResponse = res
    "ListVolumesResponse"
    "fixture/ListVolumesResponse.proto"
    storageGateway
    (Proxy :: Proxy ListVolumes)

testUpdateBandwidthRateLimitResponse :: UpdateBandwidthRateLimitResponse -> TestTree
testUpdateBandwidthRateLimitResponse = res
    "UpdateBandwidthRateLimitResponse"
    "fixture/UpdateBandwidthRateLimitResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateBandwidthRateLimit)

testAddWorkingStorageResponse :: AddWorkingStorageResponse -> TestTree
testAddWorkingStorageResponse = res
    "AddWorkingStorageResponse"
    "fixture/AddWorkingStorageResponse.proto"
    storageGateway
    (Proxy :: Proxy AddWorkingStorage)

testDescribeTapeRecoveryPointsResponse :: DescribeTapeRecoveryPointsResponse -> TestTree
testDescribeTapeRecoveryPointsResponse = res
    "DescribeTapeRecoveryPointsResponse"
    "fixture/DescribeTapeRecoveryPointsResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeTapeRecoveryPoints)

testDeleteBandwidthRateLimitResponse :: DeleteBandwidthRateLimitResponse -> TestTree
testDeleteBandwidthRateLimitResponse = res
    "DeleteBandwidthRateLimitResponse"
    "fixture/DeleteBandwidthRateLimitResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteBandwidthRateLimit)

testActivateGatewayResponse :: ActivateGatewayResponse -> TestTree
testActivateGatewayResponse = res
    "ActivateGatewayResponse"
    "fixture/ActivateGatewayResponse.proto"
    storageGateway
    (Proxy :: Proxy ActivateGateway)

testDescribeCacheResponse :: DescribeCacheResponse -> TestTree
testDescribeCacheResponse = res
    "DescribeCacheResponse"
    "fixture/DescribeCacheResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeCache)

testDeleteVolumeResponse :: DeleteVolumeResponse -> TestTree
testDeleteVolumeResponse = res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteVolume)
