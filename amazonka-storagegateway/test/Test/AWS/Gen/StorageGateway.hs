{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.AWS.Gen.StorageGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.StorageGateway where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.StorageGateway

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
testCancelArchival = undefined

testCreateStorediSCSIVolume :: CreateStorediSCSIVolume -> TestTree
testCreateStorediSCSIVolume = undefined

testCreateTapes :: CreateTapes -> TestTree
testCreateTapes = undefined

testCreateCachediSCSIVolume :: CreateCachediSCSIVolume -> TestTree
testCreateCachediSCSIVolume = undefined

testUpdateVTLDeviceType :: UpdateVTLDeviceType -> TestTree
testUpdateVTLDeviceType = undefined

testDescribeChapCredentials :: DescribeChapCredentials -> TestTree
testDescribeChapCredentials = undefined

testAddUploadBuffer :: AddUploadBuffer -> TestTree
testAddUploadBuffer = undefined

testListVolumeInitiators :: ListVolumeInitiators -> TestTree
testListVolumeInitiators = undefined

testDescribeWorkingStorage :: DescribeWorkingStorage -> TestTree
testDescribeWorkingStorage = undefined

testDescribeCachediSCSIVolumes :: DescribeCachediSCSIVolumes -> TestTree
testDescribeCachediSCSIVolumes = undefined

testUpdateGatewayInformation :: UpdateGatewayInformation -> TestTree
testUpdateGatewayInformation = undefined

testDescribeMaintenanceStartTime :: DescribeMaintenanceStartTime -> TestTree
testDescribeMaintenanceStartTime = undefined

testAddCache :: AddCache -> TestTree
testAddCache = undefined

testStartGateway :: StartGateway -> TestTree
testStartGateway = undefined

testShutdownGateway :: ShutdownGateway -> TestTree
testShutdownGateway = undefined

testUpdateGatewaySoftwareNow :: UpdateGatewaySoftwareNow -> TestTree
testUpdateGatewaySoftwareNow = undefined

testDeleteChapCredentials :: DeleteChapCredentials -> TestTree
testDeleteChapCredentials = undefined

testUpdateChapCredentials :: UpdateChapCredentials -> TestTree
testUpdateChapCredentials = undefined

testDescribeStorediSCSIVolumes :: DescribeStorediSCSIVolumes -> TestTree
testDescribeStorediSCSIVolumes = undefined

testDescribeTapes :: DescribeTapes -> TestTree
testDescribeTapes = undefined

testDescribeUploadBuffer :: DescribeUploadBuffer -> TestTree
testDescribeUploadBuffer = undefined

testCreateSnapshotFromVolumeRecoveryPoint :: CreateSnapshotFromVolumeRecoveryPoint -> TestTree
testCreateSnapshotFromVolumeRecoveryPoint = undefined

testDescribeGatewayInformation :: DescribeGatewayInformation -> TestTree
testDescribeGatewayInformation = undefined

testRetrieveTapeRecoveryPoint :: RetrieveTapeRecoveryPoint -> TestTree
testRetrieveTapeRecoveryPoint = undefined

testUpdateMaintenanceStartTime :: UpdateMaintenanceStartTime -> TestTree
testUpdateMaintenanceStartTime = undefined

testDeleteGateway :: DeleteGateway -> TestTree
testDeleteGateway = undefined

testDisableGateway :: DisableGateway -> TestTree
testDisableGateway = undefined

testDescribeSnapshotSchedule :: DescribeSnapshotSchedule -> TestTree
testDescribeSnapshotSchedule = undefined

testDescribeTapeArchives :: DescribeTapeArchives -> TestTree
testDescribeTapeArchives = undefined

testRetrieveTapeArchive :: RetrieveTapeArchive -> TestTree
testRetrieveTapeArchive = undefined

testDescribeBandwidthRateLimit :: DescribeBandwidthRateLimit -> TestTree
testDescribeBandwidthRateLimit = undefined

testDescribeVTLDevices :: DescribeVTLDevices -> TestTree
testDescribeVTLDevices = undefined

testCreateSnapshot :: CreateSnapshot -> TestTree
testCreateSnapshot = undefined

testUpdateSnapshotSchedule :: UpdateSnapshotSchedule -> TestTree
testUpdateSnapshotSchedule = undefined

testCancelRetrieval :: CancelRetrieval -> TestTree
testCancelRetrieval = undefined

testDeleteSnapshotSchedule :: DeleteSnapshotSchedule -> TestTree
testDeleteSnapshotSchedule = undefined

testDeleteTapeArchive :: DeleteTapeArchive -> TestTree
testDeleteTapeArchive = undefined

testListVolumeRecoveryPoints :: ListVolumeRecoveryPoints -> TestTree
testListVolumeRecoveryPoints = undefined

testAddWorkingStorage :: AddWorkingStorage -> TestTree
testAddWorkingStorage = undefined

testListGateways :: ListGateways -> TestTree
testListGateways = undefined

testListVolumes :: ListVolumes -> TestTree
testListVolumes = undefined

testDescribeTapeRecoveryPoints :: DescribeTapeRecoveryPoints -> TestTree
testDescribeTapeRecoveryPoints = undefined

testDeleteVolume :: DeleteVolume -> TestTree
testDeleteVolume = undefined

testResetCache :: ResetCache -> TestTree
testResetCache = undefined

testActivateGateway :: ActivateGateway -> TestTree
testActivateGateway = undefined

testDescribeCache :: DescribeCache -> TestTree
testDescribeCache = undefined

testUpdateBandwidthRateLimit :: UpdateBandwidthRateLimit -> TestTree
testUpdateBandwidthRateLimit = undefined

testDeleteBandwidthRateLimit :: DeleteBandwidthRateLimit -> TestTree
testDeleteBandwidthRateLimit = undefined

testListLocalDisks :: ListLocalDisks -> TestTree
testListLocalDisks = undefined

testDeleteTape :: DeleteTape -> TestTree
testDeleteTape = undefined

-- Responses

testCancelArchivalResponse :: CancelArchivalResponse -> TestTree
testCancelArchivalResponse = resp
    "CancelArchivalResponse"
    "fixture/CancelArchivalResponse"
    (Proxy :: Proxy CancelArchival)

testCreateStorediSCSIVolumeResponse :: CreateStorediSCSIVolumeResponse -> TestTree
testCreateStorediSCSIVolumeResponse = resp
    "CreateStorediSCSIVolumeResponse"
    "fixture/CreateStorediSCSIVolumeResponse"
    (Proxy :: Proxy CreateStorediSCSIVolume)

testCreateTapesResponse :: CreateTapesResponse -> TestTree
testCreateTapesResponse = resp
    "CreateTapesResponse"
    "fixture/CreateTapesResponse"
    (Proxy :: Proxy CreateTapes)

testCreateCachediSCSIVolumeResponse :: CreateCachediSCSIVolumeResponse -> TestTree
testCreateCachediSCSIVolumeResponse = resp
    "CreateCachediSCSIVolumeResponse"
    "fixture/CreateCachediSCSIVolumeResponse"
    (Proxy :: Proxy CreateCachediSCSIVolume)

testUpdateVTLDeviceTypeResponse :: UpdateVTLDeviceTypeResponse -> TestTree
testUpdateVTLDeviceTypeResponse = resp
    "UpdateVTLDeviceTypeResponse"
    "fixture/UpdateVTLDeviceTypeResponse"
    (Proxy :: Proxy UpdateVTLDeviceType)

testDescribeChapCredentialsResponse :: DescribeChapCredentialsResponse -> TestTree
testDescribeChapCredentialsResponse = resp
    "DescribeChapCredentialsResponse"
    "fixture/DescribeChapCredentialsResponse"
    (Proxy :: Proxy DescribeChapCredentials)

testAddUploadBufferResponse :: AddUploadBufferResponse -> TestTree
testAddUploadBufferResponse = resp
    "AddUploadBufferResponse"
    "fixture/AddUploadBufferResponse"
    (Proxy :: Proxy AddUploadBuffer)

testListVolumeInitiatorsResponse :: ListVolumeInitiatorsResponse -> TestTree
testListVolumeInitiatorsResponse = resp
    "ListVolumeInitiatorsResponse"
    "fixture/ListVolumeInitiatorsResponse"
    (Proxy :: Proxy ListVolumeInitiators)

testDescribeWorkingStorageResponse :: DescribeWorkingStorageResponse -> TestTree
testDescribeWorkingStorageResponse = resp
    "DescribeWorkingStorageResponse"
    "fixture/DescribeWorkingStorageResponse"
    (Proxy :: Proxy DescribeWorkingStorage)

testDescribeCachediSCSIVolumesResponse :: DescribeCachediSCSIVolumesResponse -> TestTree
testDescribeCachediSCSIVolumesResponse = resp
    "DescribeCachediSCSIVolumesResponse"
    "fixture/DescribeCachediSCSIVolumesResponse"
    (Proxy :: Proxy DescribeCachediSCSIVolumes)

testUpdateGatewayInformationResponse :: UpdateGatewayInformationResponse -> TestTree
testUpdateGatewayInformationResponse = resp
    "UpdateGatewayInformationResponse"
    "fixture/UpdateGatewayInformationResponse"
    (Proxy :: Proxy UpdateGatewayInformation)

testDescribeMaintenanceStartTimeResponse :: DescribeMaintenanceStartTimeResponse -> TestTree
testDescribeMaintenanceStartTimeResponse = resp
    "DescribeMaintenanceStartTimeResponse"
    "fixture/DescribeMaintenanceStartTimeResponse"
    (Proxy :: Proxy DescribeMaintenanceStartTime)

testAddCacheResponse :: AddCacheResponse -> TestTree
testAddCacheResponse = resp
    "AddCacheResponse"
    "fixture/AddCacheResponse"
    (Proxy :: Proxy AddCache)

testStartGatewayResponse :: StartGatewayResponse -> TestTree
testStartGatewayResponse = resp
    "StartGatewayResponse"
    "fixture/StartGatewayResponse"
    (Proxy :: Proxy StartGateway)

testShutdownGatewayResponse :: ShutdownGatewayResponse -> TestTree
testShutdownGatewayResponse = resp
    "ShutdownGatewayResponse"
    "fixture/ShutdownGatewayResponse"
    (Proxy :: Proxy ShutdownGateway)

testUpdateGatewaySoftwareNowResponse :: UpdateGatewaySoftwareNowResponse -> TestTree
testUpdateGatewaySoftwareNowResponse = resp
    "UpdateGatewaySoftwareNowResponse"
    "fixture/UpdateGatewaySoftwareNowResponse"
    (Proxy :: Proxy UpdateGatewaySoftwareNow)

testDeleteChapCredentialsResponse :: DeleteChapCredentialsResponse -> TestTree
testDeleteChapCredentialsResponse = resp
    "DeleteChapCredentialsResponse"
    "fixture/DeleteChapCredentialsResponse"
    (Proxy :: Proxy DeleteChapCredentials)

testUpdateChapCredentialsResponse :: UpdateChapCredentialsResponse -> TestTree
testUpdateChapCredentialsResponse = resp
    "UpdateChapCredentialsResponse"
    "fixture/UpdateChapCredentialsResponse"
    (Proxy :: Proxy UpdateChapCredentials)

testDescribeStorediSCSIVolumesResponse :: DescribeStorediSCSIVolumesResponse -> TestTree
testDescribeStorediSCSIVolumesResponse = resp
    "DescribeStorediSCSIVolumesResponse"
    "fixture/DescribeStorediSCSIVolumesResponse"
    (Proxy :: Proxy DescribeStorediSCSIVolumes)

testDescribeTapesResponse :: DescribeTapesResponse -> TestTree
testDescribeTapesResponse = resp
    "DescribeTapesResponse"
    "fixture/DescribeTapesResponse"
    (Proxy :: Proxy DescribeTapes)

testDescribeUploadBufferResponse :: DescribeUploadBufferResponse -> TestTree
testDescribeUploadBufferResponse = resp
    "DescribeUploadBufferResponse"
    "fixture/DescribeUploadBufferResponse"
    (Proxy :: Proxy DescribeUploadBuffer)

testCreateSnapshotFromVolumeRecoveryPointResponse :: CreateSnapshotFromVolumeRecoveryPointResponse -> TestTree
testCreateSnapshotFromVolumeRecoveryPointResponse = resp
    "CreateSnapshotFromVolumeRecoveryPointResponse"
    "fixture/CreateSnapshotFromVolumeRecoveryPointResponse"
    (Proxy :: Proxy CreateSnapshotFromVolumeRecoveryPoint)

testDescribeGatewayInformationResponse :: DescribeGatewayInformationResponse -> TestTree
testDescribeGatewayInformationResponse = resp
    "DescribeGatewayInformationResponse"
    "fixture/DescribeGatewayInformationResponse"
    (Proxy :: Proxy DescribeGatewayInformation)

testRetrieveTapeRecoveryPointResponse :: RetrieveTapeRecoveryPointResponse -> TestTree
testRetrieveTapeRecoveryPointResponse = resp
    "RetrieveTapeRecoveryPointResponse"
    "fixture/RetrieveTapeRecoveryPointResponse"
    (Proxy :: Proxy RetrieveTapeRecoveryPoint)

testUpdateMaintenanceStartTimeResponse :: UpdateMaintenanceStartTimeResponse -> TestTree
testUpdateMaintenanceStartTimeResponse = resp
    "UpdateMaintenanceStartTimeResponse"
    "fixture/UpdateMaintenanceStartTimeResponse"
    (Proxy :: Proxy UpdateMaintenanceStartTime)

testDeleteGatewayResponse :: DeleteGatewayResponse -> TestTree
testDeleteGatewayResponse = resp
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse"
    (Proxy :: Proxy DeleteGateway)

testDisableGatewayResponse :: DisableGatewayResponse -> TestTree
testDisableGatewayResponse = resp
    "DisableGatewayResponse"
    "fixture/DisableGatewayResponse"
    (Proxy :: Proxy DisableGateway)

testDescribeSnapshotScheduleResponse :: DescribeSnapshotScheduleResponse -> TestTree
testDescribeSnapshotScheduleResponse = resp
    "DescribeSnapshotScheduleResponse"
    "fixture/DescribeSnapshotScheduleResponse"
    (Proxy :: Proxy DescribeSnapshotSchedule)

testDescribeTapeArchivesResponse :: DescribeTapeArchivesResponse -> TestTree
testDescribeTapeArchivesResponse = resp
    "DescribeTapeArchivesResponse"
    "fixture/DescribeTapeArchivesResponse"
    (Proxy :: Proxy DescribeTapeArchives)

testRetrieveTapeArchiveResponse :: RetrieveTapeArchiveResponse -> TestTree
testRetrieveTapeArchiveResponse = resp
    "RetrieveTapeArchiveResponse"
    "fixture/RetrieveTapeArchiveResponse"
    (Proxy :: Proxy RetrieveTapeArchive)

testDescribeBandwidthRateLimitResponse :: DescribeBandwidthRateLimitResponse -> TestTree
testDescribeBandwidthRateLimitResponse = resp
    "DescribeBandwidthRateLimitResponse"
    "fixture/DescribeBandwidthRateLimitResponse"
    (Proxy :: Proxy DescribeBandwidthRateLimit)

testDescribeVTLDevicesResponse :: DescribeVTLDevicesResponse -> TestTree
testDescribeVTLDevicesResponse = resp
    "DescribeVTLDevicesResponse"
    "fixture/DescribeVTLDevicesResponse"
    (Proxy :: Proxy DescribeVTLDevices)

testCreateSnapshotResponse :: CreateSnapshotResponse -> TestTree
testCreateSnapshotResponse = resp
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse"
    (Proxy :: Proxy CreateSnapshot)

testUpdateSnapshotScheduleResponse :: UpdateSnapshotScheduleResponse -> TestTree
testUpdateSnapshotScheduleResponse = resp
    "UpdateSnapshotScheduleResponse"
    "fixture/UpdateSnapshotScheduleResponse"
    (Proxy :: Proxy UpdateSnapshotSchedule)

testCancelRetrievalResponse :: CancelRetrievalResponse -> TestTree
testCancelRetrievalResponse = resp
    "CancelRetrievalResponse"
    "fixture/CancelRetrievalResponse"
    (Proxy :: Proxy CancelRetrieval)

testDeleteSnapshotScheduleResponse :: DeleteSnapshotScheduleResponse -> TestTree
testDeleteSnapshotScheduleResponse = resp
    "DeleteSnapshotScheduleResponse"
    "fixture/DeleteSnapshotScheduleResponse"
    (Proxy :: Proxy DeleteSnapshotSchedule)

testDeleteTapeArchiveResponse :: DeleteTapeArchiveResponse -> TestTree
testDeleteTapeArchiveResponse = resp
    "DeleteTapeArchiveResponse"
    "fixture/DeleteTapeArchiveResponse"
    (Proxy :: Proxy DeleteTapeArchive)

testListVolumeRecoveryPointsResponse :: ListVolumeRecoveryPointsResponse -> TestTree
testListVolumeRecoveryPointsResponse = resp
    "ListVolumeRecoveryPointsResponse"
    "fixture/ListVolumeRecoveryPointsResponse"
    (Proxy :: Proxy ListVolumeRecoveryPoints)

testAddWorkingStorageResponse :: AddWorkingStorageResponse -> TestTree
testAddWorkingStorageResponse = resp
    "AddWorkingStorageResponse"
    "fixture/AddWorkingStorageResponse"
    (Proxy :: Proxy AddWorkingStorage)

testListGatewaysResponse :: ListGatewaysResponse -> TestTree
testListGatewaysResponse = resp
    "ListGatewaysResponse"
    "fixture/ListGatewaysResponse"
    (Proxy :: Proxy ListGateways)

testListVolumesResponse :: ListVolumesResponse -> TestTree
testListVolumesResponse = resp
    "ListVolumesResponse"
    "fixture/ListVolumesResponse"
    (Proxy :: Proxy ListVolumes)

testDescribeTapeRecoveryPointsResponse :: DescribeTapeRecoveryPointsResponse -> TestTree
testDescribeTapeRecoveryPointsResponse = resp
    "DescribeTapeRecoveryPointsResponse"
    "fixture/DescribeTapeRecoveryPointsResponse"
    (Proxy :: Proxy DescribeTapeRecoveryPoints)

testDeleteVolumeResponse :: DeleteVolumeResponse -> TestTree
testDeleteVolumeResponse = resp
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse"
    (Proxy :: Proxy DeleteVolume)

testResetCacheResponse :: ResetCacheResponse -> TestTree
testResetCacheResponse = resp
    "ResetCacheResponse"
    "fixture/ResetCacheResponse"
    (Proxy :: Proxy ResetCache)

testActivateGatewayResponse :: ActivateGatewayResponse -> TestTree
testActivateGatewayResponse = resp
    "ActivateGatewayResponse"
    "fixture/ActivateGatewayResponse"
    (Proxy :: Proxy ActivateGateway)

testDescribeCacheResponse :: DescribeCacheResponse -> TestTree
testDescribeCacheResponse = resp
    "DescribeCacheResponse"
    "fixture/DescribeCacheResponse"
    (Proxy :: Proxy DescribeCache)

testUpdateBandwidthRateLimitResponse :: UpdateBandwidthRateLimitResponse -> TestTree
testUpdateBandwidthRateLimitResponse = resp
    "UpdateBandwidthRateLimitResponse"
    "fixture/UpdateBandwidthRateLimitResponse"
    (Proxy :: Proxy UpdateBandwidthRateLimit)

testDeleteBandwidthRateLimitResponse :: DeleteBandwidthRateLimitResponse -> TestTree
testDeleteBandwidthRateLimitResponse = resp
    "DeleteBandwidthRateLimitResponse"
    "fixture/DeleteBandwidthRateLimitResponse"
    (Proxy :: Proxy DeleteBandwidthRateLimit)

testListLocalDisksResponse :: ListLocalDisksResponse -> TestTree
testListLocalDisksResponse = resp
    "ListLocalDisksResponse"
    "fixture/ListLocalDisksResponse"
    (Proxy :: Proxy ListLocalDisks)

testDeleteTapeResponse :: DeleteTapeResponse -> TestTree
testDeleteTapeResponse = resp
    "DeleteTapeResponse"
    "fixture/DeleteTapeResponse"
    (Proxy :: Proxy DeleteTape)

instance Out ActivateGateway
instance Out ActivateGatewayResponse
instance Out AddCache
instance Out AddCacheResponse
instance Out AddUploadBuffer
instance Out AddUploadBufferResponse
instance Out AddWorkingStorage
instance Out AddWorkingStorageResponse
instance Out CachediSCSIVolume
instance Out CancelArchival
instance Out CancelArchivalResponse
instance Out CancelRetrieval
instance Out CancelRetrievalResponse
instance Out ChapInfo
instance Out CreateCachediSCSIVolume
instance Out CreateCachediSCSIVolumeResponse
instance Out CreateSnapshot
instance Out CreateSnapshotFromVolumeRecoveryPoint
instance Out CreateSnapshotFromVolumeRecoveryPointResponse
instance Out CreateSnapshotResponse
instance Out CreateStorediSCSIVolume
instance Out CreateStorediSCSIVolumeResponse
instance Out CreateTapes
instance Out CreateTapesResponse
instance Out DeleteBandwidthRateLimit
instance Out DeleteBandwidthRateLimitResponse
instance Out DeleteChapCredentials
instance Out DeleteChapCredentialsResponse
instance Out DeleteGateway
instance Out DeleteGatewayResponse
instance Out DeleteSnapshotSchedule
instance Out DeleteSnapshotScheduleResponse
instance Out DeleteTape
instance Out DeleteTapeArchive
instance Out DeleteTapeArchiveResponse
instance Out DeleteTapeResponse
instance Out DeleteVolume
instance Out DeleteVolumeResponse
instance Out DescribeBandwidthRateLimit
instance Out DescribeBandwidthRateLimitResponse
instance Out DescribeCache
instance Out DescribeCacheResponse
instance Out DescribeCachediSCSIVolumes
instance Out DescribeCachediSCSIVolumesResponse
instance Out DescribeChapCredentials
instance Out DescribeChapCredentialsResponse
instance Out DescribeGatewayInformation
instance Out DescribeGatewayInformationResponse
instance Out DescribeMaintenanceStartTime
instance Out DescribeMaintenanceStartTimeResponse
instance Out DescribeSnapshotSchedule
instance Out DescribeSnapshotScheduleResponse
instance Out DescribeStorediSCSIVolumes
instance Out DescribeStorediSCSIVolumesResponse
instance Out DescribeTapeArchives
instance Out DescribeTapeArchivesResponse
instance Out DescribeTapeRecoveryPoints
instance Out DescribeTapeRecoveryPointsResponse
instance Out DescribeTapes
instance Out DescribeTapesResponse
instance Out DescribeUploadBuffer
instance Out DescribeUploadBufferResponse
instance Out DescribeVTLDevices
instance Out DescribeVTLDevicesResponse
instance Out DescribeWorkingStorage
instance Out DescribeWorkingStorageResponse
instance Out DeviceiSCSIAttributes
instance Out DisableGateway
instance Out DisableGatewayResponse
instance Out Disk
instance Out GatewayInfo
instance Out ListGateways
instance Out ListGatewaysResponse
instance Out ListLocalDisks
instance Out ListLocalDisksResponse
instance Out ListVolumeInitiators
instance Out ListVolumeInitiatorsResponse
instance Out ListVolumeRecoveryPoints
instance Out ListVolumeRecoveryPointsResponse
instance Out ListVolumes
instance Out ListVolumesResponse
instance Out NetworkInterface
instance Out ResetCache
instance Out ResetCacheResponse
instance Out RetrieveTapeArchive
instance Out RetrieveTapeArchiveResponse
instance Out RetrieveTapeRecoveryPoint
instance Out RetrieveTapeRecoveryPointResponse
instance Out ShutdownGateway
instance Out ShutdownGatewayResponse
instance Out StartGateway
instance Out StartGatewayResponse
instance Out StorediSCSIVolume
instance Out Tape
instance Out TapeArchive
instance Out TapeRecoveryPointInfo
instance Out UpdateBandwidthRateLimit
instance Out UpdateBandwidthRateLimitResponse
instance Out UpdateChapCredentials
instance Out UpdateChapCredentialsResponse
instance Out UpdateGatewayInformation
instance Out UpdateGatewayInformationResponse
instance Out UpdateGatewaySoftwareNow
instance Out UpdateGatewaySoftwareNowResponse
instance Out UpdateMaintenanceStartTime
instance Out UpdateMaintenanceStartTimeResponse
instance Out UpdateSnapshotSchedule
instance Out UpdateSnapshotScheduleResponse
instance Out UpdateVTLDeviceType
instance Out UpdateVTLDeviceTypeResponse
instance Out VTLDevice
instance Out VolumeInfo
instance Out VolumeRecoveryPointInfo
instance Out VolumeiSCSIAttributes
