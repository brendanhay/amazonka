{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.StorageGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
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
--         , testCreateTapes $
--             createTapes
--
--         , testCreateCachediSCSIVolume $
--             createCachediSCSIVolume
--
--         , testUpdateVTLDeviceType $
--             updateVTLDeviceType
--
--         , testDescribeChapCredentials $
--             describeChapCredentials
--
--         , testAddUploadBuffer $
--             addUploadBuffer
--
--         , testListVolumeInitiators $
--             listVolumeInitiators
--
--         , testDescribeWorkingStorage $
--             describeWorkingStorage
--
--         , testDescribeCachediSCSIVolumes $
--             describeCachediSCSIVolumes
--
--         , testUpdateGatewayInformation $
--             updateGatewayInformation
--
--         , testDescribeMaintenanceStartTime $
--             describeMaintenanceStartTime
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
--         , testDeleteChapCredentials $
--             deleteChapCredentials
--
--         , testUpdateChapCredentials $
--             updateChapCredentials
--
--         , testDescribeStorediSCSIVolumes $
--             describeStorediSCSIVolumes
--
--         , testDescribeTapes $
--             describeTapes
--
--         , testDescribeUploadBuffer $
--             describeUploadBuffer
--
--         , testCreateSnapshotFromVolumeRecoveryPoint $
--             createSnapshotFromVolumeRecoveryPoint
--
--         , testDescribeGatewayInformation $
--             describeGatewayInformation
--
--         , testRetrieveTapeRecoveryPoint $
--             retrieveTapeRecoveryPoint
--
--         , testUpdateMaintenanceStartTime $
--             updateMaintenanceStartTime
--
--         , testDeleteGateway $
--             deleteGateway
--
--         , testDisableGateway $
--             disableGateway
--
--         , testDescribeSnapshotSchedule $
--             describeSnapshotSchedule
--
--         , testDescribeTapeArchives $
--             describeTapeArchives
--
--         , testRetrieveTapeArchive $
--             retrieveTapeArchive
--
--         , testDescribeBandwidthRateLimit $
--             describeBandwidthRateLimit
--
--         , testDescribeVTLDevices $
--             describeVTLDevices
--
--         , testCreateSnapshot $
--             createSnapshot
--
--         , testUpdateSnapshotSchedule $
--             updateSnapshotSchedule
--
--         , testCancelRetrieval $
--             cancelRetrieval
--
--         , testDeleteSnapshotSchedule $
--             deleteSnapshotSchedule
--
--         , testDeleteTapeArchive $
--             deleteTapeArchive
--
--         , testListVolumeRecoveryPoints $
--             listVolumeRecoveryPoints
--
--         , testAddWorkingStorage $
--             addWorkingStorage
--
--         , testListGateways $
--             listGateways
--
--         , testListVolumes $
--             listVolumes
--
--         , testDescribeTapeRecoveryPoints $
--             describeTapeRecoveryPoints
--
--         , testDeleteVolume $
--             deleteVolume
--
--         , testResetCache $
--             resetCache
--
--         , testActivateGateway $
--             activateGateway
--
--         , testDescribeCache $
--             describeCache
--
--         , testUpdateBandwidthRateLimit $
--             updateBandwidthRateLimit
--
--         , testDeleteBandwidthRateLimit $
--             deleteBandwidthRateLimit
--
--         , testListLocalDisks $
--             listLocalDisks
--
--         , testDeleteTape $
--             deleteTape
--
--           ]

--     , testGroup "response"
--         [ testCancelArchivalResponse $
--             cancelArchivalResponse
--
--         , testCreateStorediSCSIVolumeResponse $
--             createStorediSCSIVolumeResponse
--
--         , testCreateTapesResponse $
--             createTapesResponse
--
--         , testCreateCachediSCSIVolumeResponse $
--             createCachediSCSIVolumeResponse
--
--         , testUpdateVTLDeviceTypeResponse $
--             updateVTLDeviceTypeResponse
--
--         , testDescribeChapCredentialsResponse $
--             describeChapCredentialsResponse
--
--         , testAddUploadBufferResponse $
--             addUploadBufferResponse
--
--         , testListVolumeInitiatorsResponse $
--             listVolumeInitiatorsResponse
--
--         , testDescribeWorkingStorageResponse $
--             describeWorkingStorageResponse
--
--         , testDescribeCachediSCSIVolumesResponse $
--             describeCachediSCSIVolumesResponse
--
--         , testUpdateGatewayInformationResponse $
--             updateGatewayInformationResponse
--
--         , testDescribeMaintenanceStartTimeResponse $
--             describeMaintenanceStartTimeResponse
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
--         , testDeleteChapCredentialsResponse $
--             deleteChapCredentialsResponse
--
--         , testUpdateChapCredentialsResponse $
--             updateChapCredentialsResponse
--
--         , testDescribeStorediSCSIVolumesResponse $
--             describeStorediSCSIVolumesResponse
--
--         , testDescribeTapesResponse $
--             describeTapesResponse
--
--         , testDescribeUploadBufferResponse $
--             describeUploadBufferResponse
--
--         , testCreateSnapshotFromVolumeRecoveryPointResponse $
--             createSnapshotFromVolumeRecoveryPointResponse
--
--         , testDescribeGatewayInformationResponse $
--             describeGatewayInformationResponse
--
--         , testRetrieveTapeRecoveryPointResponse $
--             retrieveTapeRecoveryPointResponse
--
--         , testUpdateMaintenanceStartTimeResponse $
--             updateMaintenanceStartTimeResponse
--
--         , testDeleteGatewayResponse $
--             deleteGatewayResponse
--
--         , testDisableGatewayResponse $
--             disableGatewayResponse
--
--         , testDescribeSnapshotScheduleResponse $
--             describeSnapshotScheduleResponse
--
--         , testDescribeTapeArchivesResponse $
--             describeTapeArchivesResponse
--
--         , testRetrieveTapeArchiveResponse $
--             retrieveTapeArchiveResponse
--
--         , testDescribeBandwidthRateLimitResponse $
--             describeBandwidthRateLimitResponse
--
--         , testDescribeVTLDevicesResponse $
--             describeVTLDevicesResponse
--
--         , testCreateSnapshotResponse $
--             createSnapshotResponse
--
--         , testUpdateSnapshotScheduleResponse $
--             updateSnapshotScheduleResponse
--
--         , testCancelRetrievalResponse $
--             cancelRetrievalResponse
--
--         , testDeleteSnapshotScheduleResponse $
--             deleteSnapshotScheduleResponse
--
--         , testDeleteTapeArchiveResponse $
--             deleteTapeArchiveResponse
--
--         , testListVolumeRecoveryPointsResponse $
--             listVolumeRecoveryPointsResponse
--
--         , testAddWorkingStorageResponse $
--             addWorkingStorageResponse
--
--         , testListGatewaysResponse $
--             listGatewaysResponse
--
--         , testListVolumesResponse $
--             listVolumesResponse
--
--         , testDescribeTapeRecoveryPointsResponse $
--             describeTapeRecoveryPointsResponse
--
--         , testDeleteVolumeResponse $
--             deleteVolumeResponse
--
--         , testResetCacheResponse $
--             resetCacheResponse
--
--         , testActivateGatewayResponse $
--             activateGatewayResponse
--
--         , testDescribeCacheResponse $
--             describeCacheResponse
--
--         , testUpdateBandwidthRateLimitResponse $
--             updateBandwidthRateLimitResponse
--
--         , testDeleteBandwidthRateLimitResponse $
--             deleteBandwidthRateLimitResponse
--
--         , testListLocalDisksResponse $
--             listLocalDisksResponse
--
--         , testDeleteTapeResponse $
--             deleteTapeResponse
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

testCreateTapes :: CreateTapes -> TestTree
testCreateTapes = req
    "CreateTapes"
    "fixture/CreateTapes.yaml"

testCreateCachediSCSIVolume :: CreateCachediSCSIVolume -> TestTree
testCreateCachediSCSIVolume = req
    "CreateCachediSCSIVolume"
    "fixture/CreateCachediSCSIVolume.yaml"

testUpdateVTLDeviceType :: UpdateVTLDeviceType -> TestTree
testUpdateVTLDeviceType = req
    "UpdateVTLDeviceType"
    "fixture/UpdateVTLDeviceType.yaml"

testDescribeChapCredentials :: DescribeChapCredentials -> TestTree
testDescribeChapCredentials = req
    "DescribeChapCredentials"
    "fixture/DescribeChapCredentials.yaml"

testAddUploadBuffer :: AddUploadBuffer -> TestTree
testAddUploadBuffer = req
    "AddUploadBuffer"
    "fixture/AddUploadBuffer.yaml"

testListVolumeInitiators :: ListVolumeInitiators -> TestTree
testListVolumeInitiators = req
    "ListVolumeInitiators"
    "fixture/ListVolumeInitiators.yaml"

testDescribeWorkingStorage :: DescribeWorkingStorage -> TestTree
testDescribeWorkingStorage = req
    "DescribeWorkingStorage"
    "fixture/DescribeWorkingStorage.yaml"

testDescribeCachediSCSIVolumes :: DescribeCachediSCSIVolumes -> TestTree
testDescribeCachediSCSIVolumes = req
    "DescribeCachediSCSIVolumes"
    "fixture/DescribeCachediSCSIVolumes.yaml"

testUpdateGatewayInformation :: UpdateGatewayInformation -> TestTree
testUpdateGatewayInformation = req
    "UpdateGatewayInformation"
    "fixture/UpdateGatewayInformation.yaml"

testDescribeMaintenanceStartTime :: DescribeMaintenanceStartTime -> TestTree
testDescribeMaintenanceStartTime = req
    "DescribeMaintenanceStartTime"
    "fixture/DescribeMaintenanceStartTime.yaml"

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

testDeleteChapCredentials :: DeleteChapCredentials -> TestTree
testDeleteChapCredentials = req
    "DeleteChapCredentials"
    "fixture/DeleteChapCredentials.yaml"

testUpdateChapCredentials :: UpdateChapCredentials -> TestTree
testUpdateChapCredentials = req
    "UpdateChapCredentials"
    "fixture/UpdateChapCredentials.yaml"

testDescribeStorediSCSIVolumes :: DescribeStorediSCSIVolumes -> TestTree
testDescribeStorediSCSIVolumes = req
    "DescribeStorediSCSIVolumes"
    "fixture/DescribeStorediSCSIVolumes.yaml"

testDescribeTapes :: DescribeTapes -> TestTree
testDescribeTapes = req
    "DescribeTapes"
    "fixture/DescribeTapes.yaml"

testDescribeUploadBuffer :: DescribeUploadBuffer -> TestTree
testDescribeUploadBuffer = req
    "DescribeUploadBuffer"
    "fixture/DescribeUploadBuffer.yaml"

testCreateSnapshotFromVolumeRecoveryPoint :: CreateSnapshotFromVolumeRecoveryPoint -> TestTree
testCreateSnapshotFromVolumeRecoveryPoint = req
    "CreateSnapshotFromVolumeRecoveryPoint"
    "fixture/CreateSnapshotFromVolumeRecoveryPoint.yaml"

testDescribeGatewayInformation :: DescribeGatewayInformation -> TestTree
testDescribeGatewayInformation = req
    "DescribeGatewayInformation"
    "fixture/DescribeGatewayInformation.yaml"

testRetrieveTapeRecoveryPoint :: RetrieveTapeRecoveryPoint -> TestTree
testRetrieveTapeRecoveryPoint = req
    "RetrieveTapeRecoveryPoint"
    "fixture/RetrieveTapeRecoveryPoint.yaml"

testUpdateMaintenanceStartTime :: UpdateMaintenanceStartTime -> TestTree
testUpdateMaintenanceStartTime = req
    "UpdateMaintenanceStartTime"
    "fixture/UpdateMaintenanceStartTime.yaml"

testDeleteGateway :: DeleteGateway -> TestTree
testDeleteGateway = req
    "DeleteGateway"
    "fixture/DeleteGateway.yaml"

testDisableGateway :: DisableGateway -> TestTree
testDisableGateway = req
    "DisableGateway"
    "fixture/DisableGateway.yaml"

testDescribeSnapshotSchedule :: DescribeSnapshotSchedule -> TestTree
testDescribeSnapshotSchedule = req
    "DescribeSnapshotSchedule"
    "fixture/DescribeSnapshotSchedule.yaml"

testDescribeTapeArchives :: DescribeTapeArchives -> TestTree
testDescribeTapeArchives = req
    "DescribeTapeArchives"
    "fixture/DescribeTapeArchives.yaml"

testRetrieveTapeArchive :: RetrieveTapeArchive -> TestTree
testRetrieveTapeArchive = req
    "RetrieveTapeArchive"
    "fixture/RetrieveTapeArchive.yaml"

testDescribeBandwidthRateLimit :: DescribeBandwidthRateLimit -> TestTree
testDescribeBandwidthRateLimit = req
    "DescribeBandwidthRateLimit"
    "fixture/DescribeBandwidthRateLimit.yaml"

testDescribeVTLDevices :: DescribeVTLDevices -> TestTree
testDescribeVTLDevices = req
    "DescribeVTLDevices"
    "fixture/DescribeVTLDevices.yaml"

testCreateSnapshot :: CreateSnapshot -> TestTree
testCreateSnapshot = req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

testUpdateSnapshotSchedule :: UpdateSnapshotSchedule -> TestTree
testUpdateSnapshotSchedule = req
    "UpdateSnapshotSchedule"
    "fixture/UpdateSnapshotSchedule.yaml"

testCancelRetrieval :: CancelRetrieval -> TestTree
testCancelRetrieval = req
    "CancelRetrieval"
    "fixture/CancelRetrieval.yaml"

testDeleteSnapshotSchedule :: DeleteSnapshotSchedule -> TestTree
testDeleteSnapshotSchedule = req
    "DeleteSnapshotSchedule"
    "fixture/DeleteSnapshotSchedule.yaml"

testDeleteTapeArchive :: DeleteTapeArchive -> TestTree
testDeleteTapeArchive = req
    "DeleteTapeArchive"
    "fixture/DeleteTapeArchive.yaml"

testListVolumeRecoveryPoints :: ListVolumeRecoveryPoints -> TestTree
testListVolumeRecoveryPoints = req
    "ListVolumeRecoveryPoints"
    "fixture/ListVolumeRecoveryPoints.yaml"

testAddWorkingStorage :: AddWorkingStorage -> TestTree
testAddWorkingStorage = req
    "AddWorkingStorage"
    "fixture/AddWorkingStorage.yaml"

testListGateways :: ListGateways -> TestTree
testListGateways = req
    "ListGateways"
    "fixture/ListGateways.yaml"

testListVolumes :: ListVolumes -> TestTree
testListVolumes = req
    "ListVolumes"
    "fixture/ListVolumes.yaml"

testDescribeTapeRecoveryPoints :: DescribeTapeRecoveryPoints -> TestTree
testDescribeTapeRecoveryPoints = req
    "DescribeTapeRecoveryPoints"
    "fixture/DescribeTapeRecoveryPoints.yaml"

testDeleteVolume :: DeleteVolume -> TestTree
testDeleteVolume = req
    "DeleteVolume"
    "fixture/DeleteVolume.yaml"

testResetCache :: ResetCache -> TestTree
testResetCache = req
    "ResetCache"
    "fixture/ResetCache.yaml"

testActivateGateway :: ActivateGateway -> TestTree
testActivateGateway = req
    "ActivateGateway"
    "fixture/ActivateGateway.yaml"

testDescribeCache :: DescribeCache -> TestTree
testDescribeCache = req
    "DescribeCache"
    "fixture/DescribeCache.yaml"

testUpdateBandwidthRateLimit :: UpdateBandwidthRateLimit -> TestTree
testUpdateBandwidthRateLimit = req
    "UpdateBandwidthRateLimit"
    "fixture/UpdateBandwidthRateLimit.yaml"

testDeleteBandwidthRateLimit :: DeleteBandwidthRateLimit -> TestTree
testDeleteBandwidthRateLimit = req
    "DeleteBandwidthRateLimit"
    "fixture/DeleteBandwidthRateLimit.yaml"

testListLocalDisks :: ListLocalDisks -> TestTree
testListLocalDisks = req
    "ListLocalDisks"
    "fixture/ListLocalDisks.yaml"

testDeleteTape :: DeleteTape -> TestTree
testDeleteTape = req
    "DeleteTape"
    "fixture/DeleteTape.yaml"

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

testCreateTapesResponse :: CreateTapesResponse -> TestTree
testCreateTapesResponse = res
    "CreateTapesResponse"
    "fixture/CreateTapesResponse.proto"
    storageGateway
    (Proxy :: Proxy CreateTapes)

testCreateCachediSCSIVolumeResponse :: CreateCachediSCSIVolumeResponse -> TestTree
testCreateCachediSCSIVolumeResponse = res
    "CreateCachediSCSIVolumeResponse"
    "fixture/CreateCachediSCSIVolumeResponse.proto"
    storageGateway
    (Proxy :: Proxy CreateCachediSCSIVolume)

testUpdateVTLDeviceTypeResponse :: UpdateVTLDeviceTypeResponse -> TestTree
testUpdateVTLDeviceTypeResponse = res
    "UpdateVTLDeviceTypeResponse"
    "fixture/UpdateVTLDeviceTypeResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateVTLDeviceType)

testDescribeChapCredentialsResponse :: DescribeChapCredentialsResponse -> TestTree
testDescribeChapCredentialsResponse = res
    "DescribeChapCredentialsResponse"
    "fixture/DescribeChapCredentialsResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeChapCredentials)

testAddUploadBufferResponse :: AddUploadBufferResponse -> TestTree
testAddUploadBufferResponse = res
    "AddUploadBufferResponse"
    "fixture/AddUploadBufferResponse.proto"
    storageGateway
    (Proxy :: Proxy AddUploadBuffer)

testListVolumeInitiatorsResponse :: ListVolumeInitiatorsResponse -> TestTree
testListVolumeInitiatorsResponse = res
    "ListVolumeInitiatorsResponse"
    "fixture/ListVolumeInitiatorsResponse.proto"
    storageGateway
    (Proxy :: Proxy ListVolumeInitiators)

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

testDescribeStorediSCSIVolumesResponse :: DescribeStorediSCSIVolumesResponse -> TestTree
testDescribeStorediSCSIVolumesResponse = res
    "DescribeStorediSCSIVolumesResponse"
    "fixture/DescribeStorediSCSIVolumesResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeStorediSCSIVolumes)

testDescribeTapesResponse :: DescribeTapesResponse -> TestTree
testDescribeTapesResponse = res
    "DescribeTapesResponse"
    "fixture/DescribeTapesResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeTapes)

testDescribeUploadBufferResponse :: DescribeUploadBufferResponse -> TestTree
testDescribeUploadBufferResponse = res
    "DescribeUploadBufferResponse"
    "fixture/DescribeUploadBufferResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeUploadBuffer)

testCreateSnapshotFromVolumeRecoveryPointResponse :: CreateSnapshotFromVolumeRecoveryPointResponse -> TestTree
testCreateSnapshotFromVolumeRecoveryPointResponse = res
    "CreateSnapshotFromVolumeRecoveryPointResponse"
    "fixture/CreateSnapshotFromVolumeRecoveryPointResponse.proto"
    storageGateway
    (Proxy :: Proxy CreateSnapshotFromVolumeRecoveryPoint)

testDescribeGatewayInformationResponse :: DescribeGatewayInformationResponse -> TestTree
testDescribeGatewayInformationResponse = res
    "DescribeGatewayInformationResponse"
    "fixture/DescribeGatewayInformationResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeGatewayInformation)

testRetrieveTapeRecoveryPointResponse :: RetrieveTapeRecoveryPointResponse -> TestTree
testRetrieveTapeRecoveryPointResponse = res
    "RetrieveTapeRecoveryPointResponse"
    "fixture/RetrieveTapeRecoveryPointResponse.proto"
    storageGateway
    (Proxy :: Proxy RetrieveTapeRecoveryPoint)

testUpdateMaintenanceStartTimeResponse :: UpdateMaintenanceStartTimeResponse -> TestTree
testUpdateMaintenanceStartTimeResponse = res
    "UpdateMaintenanceStartTimeResponse"
    "fixture/UpdateMaintenanceStartTimeResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateMaintenanceStartTime)

testDeleteGatewayResponse :: DeleteGatewayResponse -> TestTree
testDeleteGatewayResponse = res
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteGateway)

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

testDescribeTapeArchivesResponse :: DescribeTapeArchivesResponse -> TestTree
testDescribeTapeArchivesResponse = res
    "DescribeTapeArchivesResponse"
    "fixture/DescribeTapeArchivesResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeTapeArchives)

testRetrieveTapeArchiveResponse :: RetrieveTapeArchiveResponse -> TestTree
testRetrieveTapeArchiveResponse = res
    "RetrieveTapeArchiveResponse"
    "fixture/RetrieveTapeArchiveResponse.proto"
    storageGateway
    (Proxy :: Proxy RetrieveTapeArchive)

testDescribeBandwidthRateLimitResponse :: DescribeBandwidthRateLimitResponse -> TestTree
testDescribeBandwidthRateLimitResponse = res
    "DescribeBandwidthRateLimitResponse"
    "fixture/DescribeBandwidthRateLimitResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeBandwidthRateLimit)

testDescribeVTLDevicesResponse :: DescribeVTLDevicesResponse -> TestTree
testDescribeVTLDevicesResponse = res
    "DescribeVTLDevicesResponse"
    "fixture/DescribeVTLDevicesResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeVTLDevices)

testCreateSnapshotResponse :: CreateSnapshotResponse -> TestTree
testCreateSnapshotResponse = res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    storageGateway
    (Proxy :: Proxy CreateSnapshot)

testUpdateSnapshotScheduleResponse :: UpdateSnapshotScheduleResponse -> TestTree
testUpdateSnapshotScheduleResponse = res
    "UpdateSnapshotScheduleResponse"
    "fixture/UpdateSnapshotScheduleResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateSnapshotSchedule)

testCancelRetrievalResponse :: CancelRetrievalResponse -> TestTree
testCancelRetrievalResponse = res
    "CancelRetrievalResponse"
    "fixture/CancelRetrievalResponse.proto"
    storageGateway
    (Proxy :: Proxy CancelRetrieval)

testDeleteSnapshotScheduleResponse :: DeleteSnapshotScheduleResponse -> TestTree
testDeleteSnapshotScheduleResponse = res
    "DeleteSnapshotScheduleResponse"
    "fixture/DeleteSnapshotScheduleResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteSnapshotSchedule)

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

testAddWorkingStorageResponse :: AddWorkingStorageResponse -> TestTree
testAddWorkingStorageResponse = res
    "AddWorkingStorageResponse"
    "fixture/AddWorkingStorageResponse.proto"
    storageGateway
    (Proxy :: Proxy AddWorkingStorage)

testListGatewaysResponse :: ListGatewaysResponse -> TestTree
testListGatewaysResponse = res
    "ListGatewaysResponse"
    "fixture/ListGatewaysResponse.proto"
    storageGateway
    (Proxy :: Proxy ListGateways)

testListVolumesResponse :: ListVolumesResponse -> TestTree
testListVolumesResponse = res
    "ListVolumesResponse"
    "fixture/ListVolumesResponse.proto"
    storageGateway
    (Proxy :: Proxy ListVolumes)

testDescribeTapeRecoveryPointsResponse :: DescribeTapeRecoveryPointsResponse -> TestTree
testDescribeTapeRecoveryPointsResponse = res
    "DescribeTapeRecoveryPointsResponse"
    "fixture/DescribeTapeRecoveryPointsResponse.proto"
    storageGateway
    (Proxy :: Proxy DescribeTapeRecoveryPoints)

testDeleteVolumeResponse :: DeleteVolumeResponse -> TestTree
testDeleteVolumeResponse = res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteVolume)

testResetCacheResponse :: ResetCacheResponse -> TestTree
testResetCacheResponse = res
    "ResetCacheResponse"
    "fixture/ResetCacheResponse.proto"
    storageGateway
    (Proxy :: Proxy ResetCache)

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

testUpdateBandwidthRateLimitResponse :: UpdateBandwidthRateLimitResponse -> TestTree
testUpdateBandwidthRateLimitResponse = res
    "UpdateBandwidthRateLimitResponse"
    "fixture/UpdateBandwidthRateLimitResponse.proto"
    storageGateway
    (Proxy :: Proxy UpdateBandwidthRateLimit)

testDeleteBandwidthRateLimitResponse :: DeleteBandwidthRateLimitResponse -> TestTree
testDeleteBandwidthRateLimitResponse = res
    "DeleteBandwidthRateLimitResponse"
    "fixture/DeleteBandwidthRateLimitResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteBandwidthRateLimit)

testListLocalDisksResponse :: ListLocalDisksResponse -> TestTree
testListLocalDisksResponse = res
    "ListLocalDisksResponse"
    "fixture/ListLocalDisksResponse.proto"
    storageGateway
    (Proxy :: Proxy ListLocalDisks)

testDeleteTapeResponse :: DeleteTapeResponse -> TestTree
testDeleteTapeResponse = res
    "DeleteTapeResponse"
    "fixture/DeleteTapeResponse.proto"
    storageGateway
    (Proxy :: Proxy DeleteTape)
