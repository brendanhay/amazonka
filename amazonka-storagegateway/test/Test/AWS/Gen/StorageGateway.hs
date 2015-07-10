{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.StorageGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
    "fixture/CancelArchival"

testCreateStorediSCSIVolume :: CreateStorediSCSIVolume -> TestTree
testCreateStorediSCSIVolume = req
    "CreateStorediSCSIVolume"
    "fixture/CreateStorediSCSIVolume"

testCreateTapes :: CreateTapes -> TestTree
testCreateTapes = req
    "CreateTapes"
    "fixture/CreateTapes"

testCreateCachediSCSIVolume :: CreateCachediSCSIVolume -> TestTree
testCreateCachediSCSIVolume = req
    "CreateCachediSCSIVolume"
    "fixture/CreateCachediSCSIVolume"

testUpdateVTLDeviceType :: UpdateVTLDeviceType -> TestTree
testUpdateVTLDeviceType = req
    "UpdateVTLDeviceType"
    "fixture/UpdateVTLDeviceType"

testDescribeChapCredentials :: DescribeChapCredentials -> TestTree
testDescribeChapCredentials = req
    "DescribeChapCredentials"
    "fixture/DescribeChapCredentials"

testAddUploadBuffer :: AddUploadBuffer -> TestTree
testAddUploadBuffer = req
    "AddUploadBuffer"
    "fixture/AddUploadBuffer"

testListVolumeInitiators :: ListVolumeInitiators -> TestTree
testListVolumeInitiators = req
    "ListVolumeInitiators"
    "fixture/ListVolumeInitiators"

testDescribeWorkingStorage :: DescribeWorkingStorage -> TestTree
testDescribeWorkingStorage = req
    "DescribeWorkingStorage"
    "fixture/DescribeWorkingStorage"

testDescribeCachediSCSIVolumes :: DescribeCachediSCSIVolumes -> TestTree
testDescribeCachediSCSIVolumes = req
    "DescribeCachediSCSIVolumes"
    "fixture/DescribeCachediSCSIVolumes"

testUpdateGatewayInformation :: UpdateGatewayInformation -> TestTree
testUpdateGatewayInformation = req
    "UpdateGatewayInformation"
    "fixture/UpdateGatewayInformation"

testDescribeMaintenanceStartTime :: DescribeMaintenanceStartTime -> TestTree
testDescribeMaintenanceStartTime = req
    "DescribeMaintenanceStartTime"
    "fixture/DescribeMaintenanceStartTime"

testAddCache :: AddCache -> TestTree
testAddCache = req
    "AddCache"
    "fixture/AddCache"

testStartGateway :: StartGateway -> TestTree
testStartGateway = req
    "StartGateway"
    "fixture/StartGateway"

testShutdownGateway :: ShutdownGateway -> TestTree
testShutdownGateway = req
    "ShutdownGateway"
    "fixture/ShutdownGateway"

testUpdateGatewaySoftwareNow :: UpdateGatewaySoftwareNow -> TestTree
testUpdateGatewaySoftwareNow = req
    "UpdateGatewaySoftwareNow"
    "fixture/UpdateGatewaySoftwareNow"

testDeleteChapCredentials :: DeleteChapCredentials -> TestTree
testDeleteChapCredentials = req
    "DeleteChapCredentials"
    "fixture/DeleteChapCredentials"

testUpdateChapCredentials :: UpdateChapCredentials -> TestTree
testUpdateChapCredentials = req
    "UpdateChapCredentials"
    "fixture/UpdateChapCredentials"

testDescribeStorediSCSIVolumes :: DescribeStorediSCSIVolumes -> TestTree
testDescribeStorediSCSIVolumes = req
    "DescribeStorediSCSIVolumes"
    "fixture/DescribeStorediSCSIVolumes"

testDescribeTapes :: DescribeTapes -> TestTree
testDescribeTapes = req
    "DescribeTapes"
    "fixture/DescribeTapes"

testDescribeUploadBuffer :: DescribeUploadBuffer -> TestTree
testDescribeUploadBuffer = req
    "DescribeUploadBuffer"
    "fixture/DescribeUploadBuffer"

testCreateSnapshotFromVolumeRecoveryPoint :: CreateSnapshotFromVolumeRecoveryPoint -> TestTree
testCreateSnapshotFromVolumeRecoveryPoint = req
    "CreateSnapshotFromVolumeRecoveryPoint"
    "fixture/CreateSnapshotFromVolumeRecoveryPoint"

testDescribeGatewayInformation :: DescribeGatewayInformation -> TestTree
testDescribeGatewayInformation = req
    "DescribeGatewayInformation"
    "fixture/DescribeGatewayInformation"

testRetrieveTapeRecoveryPoint :: RetrieveTapeRecoveryPoint -> TestTree
testRetrieveTapeRecoveryPoint = req
    "RetrieveTapeRecoveryPoint"
    "fixture/RetrieveTapeRecoveryPoint"

testUpdateMaintenanceStartTime :: UpdateMaintenanceStartTime -> TestTree
testUpdateMaintenanceStartTime = req
    "UpdateMaintenanceStartTime"
    "fixture/UpdateMaintenanceStartTime"

testDeleteGateway :: DeleteGateway -> TestTree
testDeleteGateway = req
    "DeleteGateway"
    "fixture/DeleteGateway"

testDisableGateway :: DisableGateway -> TestTree
testDisableGateway = req
    "DisableGateway"
    "fixture/DisableGateway"

testDescribeSnapshotSchedule :: DescribeSnapshotSchedule -> TestTree
testDescribeSnapshotSchedule = req
    "DescribeSnapshotSchedule"
    "fixture/DescribeSnapshotSchedule"

testDescribeTapeArchives :: DescribeTapeArchives -> TestTree
testDescribeTapeArchives = req
    "DescribeTapeArchives"
    "fixture/DescribeTapeArchives"

testRetrieveTapeArchive :: RetrieveTapeArchive -> TestTree
testRetrieveTapeArchive = req
    "RetrieveTapeArchive"
    "fixture/RetrieveTapeArchive"

testDescribeBandwidthRateLimit :: DescribeBandwidthRateLimit -> TestTree
testDescribeBandwidthRateLimit = req
    "DescribeBandwidthRateLimit"
    "fixture/DescribeBandwidthRateLimit"

testDescribeVTLDevices :: DescribeVTLDevices -> TestTree
testDescribeVTLDevices = req
    "DescribeVTLDevices"
    "fixture/DescribeVTLDevices"

testCreateSnapshot :: CreateSnapshot -> TestTree
testCreateSnapshot = req
    "CreateSnapshot"
    "fixture/CreateSnapshot"

testUpdateSnapshotSchedule :: UpdateSnapshotSchedule -> TestTree
testUpdateSnapshotSchedule = req
    "UpdateSnapshotSchedule"
    "fixture/UpdateSnapshotSchedule"

testCancelRetrieval :: CancelRetrieval -> TestTree
testCancelRetrieval = req
    "CancelRetrieval"
    "fixture/CancelRetrieval"

testDeleteSnapshotSchedule :: DeleteSnapshotSchedule -> TestTree
testDeleteSnapshotSchedule = req
    "DeleteSnapshotSchedule"
    "fixture/DeleteSnapshotSchedule"

testDeleteTapeArchive :: DeleteTapeArchive -> TestTree
testDeleteTapeArchive = req
    "DeleteTapeArchive"
    "fixture/DeleteTapeArchive"

testListVolumeRecoveryPoints :: ListVolumeRecoveryPoints -> TestTree
testListVolumeRecoveryPoints = req
    "ListVolumeRecoveryPoints"
    "fixture/ListVolumeRecoveryPoints"

testAddWorkingStorage :: AddWorkingStorage -> TestTree
testAddWorkingStorage = req
    "AddWorkingStorage"
    "fixture/AddWorkingStorage"

testListGateways :: ListGateways -> TestTree
testListGateways = req
    "ListGateways"
    "fixture/ListGateways"

testListVolumes :: ListVolumes -> TestTree
testListVolumes = req
    "ListVolumes"
    "fixture/ListVolumes"

testDescribeTapeRecoveryPoints :: DescribeTapeRecoveryPoints -> TestTree
testDescribeTapeRecoveryPoints = req
    "DescribeTapeRecoveryPoints"
    "fixture/DescribeTapeRecoveryPoints"

testDeleteVolume :: DeleteVolume -> TestTree
testDeleteVolume = req
    "DeleteVolume"
    "fixture/DeleteVolume"

testResetCache :: ResetCache -> TestTree
testResetCache = req
    "ResetCache"
    "fixture/ResetCache"

testActivateGateway :: ActivateGateway -> TestTree
testActivateGateway = req
    "ActivateGateway"
    "fixture/ActivateGateway"

testDescribeCache :: DescribeCache -> TestTree
testDescribeCache = req
    "DescribeCache"
    "fixture/DescribeCache"

testUpdateBandwidthRateLimit :: UpdateBandwidthRateLimit -> TestTree
testUpdateBandwidthRateLimit = req
    "UpdateBandwidthRateLimit"
    "fixture/UpdateBandwidthRateLimit"

testDeleteBandwidthRateLimit :: DeleteBandwidthRateLimit -> TestTree
testDeleteBandwidthRateLimit = req
    "DeleteBandwidthRateLimit"
    "fixture/DeleteBandwidthRateLimit"

testListLocalDisks :: ListLocalDisks -> TestTree
testListLocalDisks = req
    "ListLocalDisks"
    "fixture/ListLocalDisks"

testDeleteTape :: DeleteTape -> TestTree
testDeleteTape = req
    "DeleteTape"
    "fixture/DeleteTape"

-- Responses

testCancelArchivalResponse :: CancelArchivalResponse -> TestTree
testCancelArchivalResponse = res
    "CancelArchivalResponse"
    "fixture/CancelArchivalResponse"
    (Proxy :: Proxy CancelArchival)

testCreateStorediSCSIVolumeResponse :: CreateStorediSCSIVolumeResponse -> TestTree
testCreateStorediSCSIVolumeResponse = res
    "CreateStorediSCSIVolumeResponse"
    "fixture/CreateStorediSCSIVolumeResponse"
    (Proxy :: Proxy CreateStorediSCSIVolume)

testCreateTapesResponse :: CreateTapesResponse -> TestTree
testCreateTapesResponse = res
    "CreateTapesResponse"
    "fixture/CreateTapesResponse"
    (Proxy :: Proxy CreateTapes)

testCreateCachediSCSIVolumeResponse :: CreateCachediSCSIVolumeResponse -> TestTree
testCreateCachediSCSIVolumeResponse = res
    "CreateCachediSCSIVolumeResponse"
    "fixture/CreateCachediSCSIVolumeResponse"
    (Proxy :: Proxy CreateCachediSCSIVolume)

testUpdateVTLDeviceTypeResponse :: UpdateVTLDeviceTypeResponse -> TestTree
testUpdateVTLDeviceTypeResponse = res
    "UpdateVTLDeviceTypeResponse"
    "fixture/UpdateVTLDeviceTypeResponse"
    (Proxy :: Proxy UpdateVTLDeviceType)

testDescribeChapCredentialsResponse :: DescribeChapCredentialsResponse -> TestTree
testDescribeChapCredentialsResponse = res
    "DescribeChapCredentialsResponse"
    "fixture/DescribeChapCredentialsResponse"
    (Proxy :: Proxy DescribeChapCredentials)

testAddUploadBufferResponse :: AddUploadBufferResponse -> TestTree
testAddUploadBufferResponse = res
    "AddUploadBufferResponse"
    "fixture/AddUploadBufferResponse"
    (Proxy :: Proxy AddUploadBuffer)

testListVolumeInitiatorsResponse :: ListVolumeInitiatorsResponse -> TestTree
testListVolumeInitiatorsResponse = res
    "ListVolumeInitiatorsResponse"
    "fixture/ListVolumeInitiatorsResponse"
    (Proxy :: Proxy ListVolumeInitiators)

testDescribeWorkingStorageResponse :: DescribeWorkingStorageResponse -> TestTree
testDescribeWorkingStorageResponse = res
    "DescribeWorkingStorageResponse"
    "fixture/DescribeWorkingStorageResponse"
    (Proxy :: Proxy DescribeWorkingStorage)

testDescribeCachediSCSIVolumesResponse :: DescribeCachediSCSIVolumesResponse -> TestTree
testDescribeCachediSCSIVolumesResponse = res
    "DescribeCachediSCSIVolumesResponse"
    "fixture/DescribeCachediSCSIVolumesResponse"
    (Proxy :: Proxy DescribeCachediSCSIVolumes)

testUpdateGatewayInformationResponse :: UpdateGatewayInformationResponse -> TestTree
testUpdateGatewayInformationResponse = res
    "UpdateGatewayInformationResponse"
    "fixture/UpdateGatewayInformationResponse"
    (Proxy :: Proxy UpdateGatewayInformation)

testDescribeMaintenanceStartTimeResponse :: DescribeMaintenanceStartTimeResponse -> TestTree
testDescribeMaintenanceStartTimeResponse = res
    "DescribeMaintenanceStartTimeResponse"
    "fixture/DescribeMaintenanceStartTimeResponse"
    (Proxy :: Proxy DescribeMaintenanceStartTime)

testAddCacheResponse :: AddCacheResponse -> TestTree
testAddCacheResponse = res
    "AddCacheResponse"
    "fixture/AddCacheResponse"
    (Proxy :: Proxy AddCache)

testStartGatewayResponse :: StartGatewayResponse -> TestTree
testStartGatewayResponse = res
    "StartGatewayResponse"
    "fixture/StartGatewayResponse"
    (Proxy :: Proxy StartGateway)

testShutdownGatewayResponse :: ShutdownGatewayResponse -> TestTree
testShutdownGatewayResponse = res
    "ShutdownGatewayResponse"
    "fixture/ShutdownGatewayResponse"
    (Proxy :: Proxy ShutdownGateway)

testUpdateGatewaySoftwareNowResponse :: UpdateGatewaySoftwareNowResponse -> TestTree
testUpdateGatewaySoftwareNowResponse = res
    "UpdateGatewaySoftwareNowResponse"
    "fixture/UpdateGatewaySoftwareNowResponse"
    (Proxy :: Proxy UpdateGatewaySoftwareNow)

testDeleteChapCredentialsResponse :: DeleteChapCredentialsResponse -> TestTree
testDeleteChapCredentialsResponse = res
    "DeleteChapCredentialsResponse"
    "fixture/DeleteChapCredentialsResponse"
    (Proxy :: Proxy DeleteChapCredentials)

testUpdateChapCredentialsResponse :: UpdateChapCredentialsResponse -> TestTree
testUpdateChapCredentialsResponse = res
    "UpdateChapCredentialsResponse"
    "fixture/UpdateChapCredentialsResponse"
    (Proxy :: Proxy UpdateChapCredentials)

testDescribeStorediSCSIVolumesResponse :: DescribeStorediSCSIVolumesResponse -> TestTree
testDescribeStorediSCSIVolumesResponse = res
    "DescribeStorediSCSIVolumesResponse"
    "fixture/DescribeStorediSCSIVolumesResponse"
    (Proxy :: Proxy DescribeStorediSCSIVolumes)

testDescribeTapesResponse :: DescribeTapesResponse -> TestTree
testDescribeTapesResponse = res
    "DescribeTapesResponse"
    "fixture/DescribeTapesResponse"
    (Proxy :: Proxy DescribeTapes)

testDescribeUploadBufferResponse :: DescribeUploadBufferResponse -> TestTree
testDescribeUploadBufferResponse = res
    "DescribeUploadBufferResponse"
    "fixture/DescribeUploadBufferResponse"
    (Proxy :: Proxy DescribeUploadBuffer)

testCreateSnapshotFromVolumeRecoveryPointResponse :: CreateSnapshotFromVolumeRecoveryPointResponse -> TestTree
testCreateSnapshotFromVolumeRecoveryPointResponse = res
    "CreateSnapshotFromVolumeRecoveryPointResponse"
    "fixture/CreateSnapshotFromVolumeRecoveryPointResponse"
    (Proxy :: Proxy CreateSnapshotFromVolumeRecoveryPoint)

testDescribeGatewayInformationResponse :: DescribeGatewayInformationResponse -> TestTree
testDescribeGatewayInformationResponse = res
    "DescribeGatewayInformationResponse"
    "fixture/DescribeGatewayInformationResponse"
    (Proxy :: Proxy DescribeGatewayInformation)

testRetrieveTapeRecoveryPointResponse :: RetrieveTapeRecoveryPointResponse -> TestTree
testRetrieveTapeRecoveryPointResponse = res
    "RetrieveTapeRecoveryPointResponse"
    "fixture/RetrieveTapeRecoveryPointResponse"
    (Proxy :: Proxy RetrieveTapeRecoveryPoint)

testUpdateMaintenanceStartTimeResponse :: UpdateMaintenanceStartTimeResponse -> TestTree
testUpdateMaintenanceStartTimeResponse = res
    "UpdateMaintenanceStartTimeResponse"
    "fixture/UpdateMaintenanceStartTimeResponse"
    (Proxy :: Proxy UpdateMaintenanceStartTime)

testDeleteGatewayResponse :: DeleteGatewayResponse -> TestTree
testDeleteGatewayResponse = res
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse"
    (Proxy :: Proxy DeleteGateway)

testDisableGatewayResponse :: DisableGatewayResponse -> TestTree
testDisableGatewayResponse = res
    "DisableGatewayResponse"
    "fixture/DisableGatewayResponse"
    (Proxy :: Proxy DisableGateway)

testDescribeSnapshotScheduleResponse :: DescribeSnapshotScheduleResponse -> TestTree
testDescribeSnapshotScheduleResponse = res
    "DescribeSnapshotScheduleResponse"
    "fixture/DescribeSnapshotScheduleResponse"
    (Proxy :: Proxy DescribeSnapshotSchedule)

testDescribeTapeArchivesResponse :: DescribeTapeArchivesResponse -> TestTree
testDescribeTapeArchivesResponse = res
    "DescribeTapeArchivesResponse"
    "fixture/DescribeTapeArchivesResponse"
    (Proxy :: Proxy DescribeTapeArchives)

testRetrieveTapeArchiveResponse :: RetrieveTapeArchiveResponse -> TestTree
testRetrieveTapeArchiveResponse = res
    "RetrieveTapeArchiveResponse"
    "fixture/RetrieveTapeArchiveResponse"
    (Proxy :: Proxy RetrieveTapeArchive)

testDescribeBandwidthRateLimitResponse :: DescribeBandwidthRateLimitResponse -> TestTree
testDescribeBandwidthRateLimitResponse = res
    "DescribeBandwidthRateLimitResponse"
    "fixture/DescribeBandwidthRateLimitResponse"
    (Proxy :: Proxy DescribeBandwidthRateLimit)

testDescribeVTLDevicesResponse :: DescribeVTLDevicesResponse -> TestTree
testDescribeVTLDevicesResponse = res
    "DescribeVTLDevicesResponse"
    "fixture/DescribeVTLDevicesResponse"
    (Proxy :: Proxy DescribeVTLDevices)

testCreateSnapshotResponse :: CreateSnapshotResponse -> TestTree
testCreateSnapshotResponse = res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse"
    (Proxy :: Proxy CreateSnapshot)

testUpdateSnapshotScheduleResponse :: UpdateSnapshotScheduleResponse -> TestTree
testUpdateSnapshotScheduleResponse = res
    "UpdateSnapshotScheduleResponse"
    "fixture/UpdateSnapshotScheduleResponse"
    (Proxy :: Proxy UpdateSnapshotSchedule)

testCancelRetrievalResponse :: CancelRetrievalResponse -> TestTree
testCancelRetrievalResponse = res
    "CancelRetrievalResponse"
    "fixture/CancelRetrievalResponse"
    (Proxy :: Proxy CancelRetrieval)

testDeleteSnapshotScheduleResponse :: DeleteSnapshotScheduleResponse -> TestTree
testDeleteSnapshotScheduleResponse = res
    "DeleteSnapshotScheduleResponse"
    "fixture/DeleteSnapshotScheduleResponse"
    (Proxy :: Proxy DeleteSnapshotSchedule)

testDeleteTapeArchiveResponse :: DeleteTapeArchiveResponse -> TestTree
testDeleteTapeArchiveResponse = res
    "DeleteTapeArchiveResponse"
    "fixture/DeleteTapeArchiveResponse"
    (Proxy :: Proxy DeleteTapeArchive)

testListVolumeRecoveryPointsResponse :: ListVolumeRecoveryPointsResponse -> TestTree
testListVolumeRecoveryPointsResponse = res
    "ListVolumeRecoveryPointsResponse"
    "fixture/ListVolumeRecoveryPointsResponse"
    (Proxy :: Proxy ListVolumeRecoveryPoints)

testAddWorkingStorageResponse :: AddWorkingStorageResponse -> TestTree
testAddWorkingStorageResponse = res
    "AddWorkingStorageResponse"
    "fixture/AddWorkingStorageResponse"
    (Proxy :: Proxy AddWorkingStorage)

testListGatewaysResponse :: ListGatewaysResponse -> TestTree
testListGatewaysResponse = res
    "ListGatewaysResponse"
    "fixture/ListGatewaysResponse"
    (Proxy :: Proxy ListGateways)

testListVolumesResponse :: ListVolumesResponse -> TestTree
testListVolumesResponse = res
    "ListVolumesResponse"
    "fixture/ListVolumesResponse"
    (Proxy :: Proxy ListVolumes)

testDescribeTapeRecoveryPointsResponse :: DescribeTapeRecoveryPointsResponse -> TestTree
testDescribeTapeRecoveryPointsResponse = res
    "DescribeTapeRecoveryPointsResponse"
    "fixture/DescribeTapeRecoveryPointsResponse"
    (Proxy :: Proxy DescribeTapeRecoveryPoints)

testDeleteVolumeResponse :: DeleteVolumeResponse -> TestTree
testDeleteVolumeResponse = res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse"
    (Proxy :: Proxy DeleteVolume)

testResetCacheResponse :: ResetCacheResponse -> TestTree
testResetCacheResponse = res
    "ResetCacheResponse"
    "fixture/ResetCacheResponse"
    (Proxy :: Proxy ResetCache)

testActivateGatewayResponse :: ActivateGatewayResponse -> TestTree
testActivateGatewayResponse = res
    "ActivateGatewayResponse"
    "fixture/ActivateGatewayResponse"
    (Proxy :: Proxy ActivateGateway)

testDescribeCacheResponse :: DescribeCacheResponse -> TestTree
testDescribeCacheResponse = res
    "DescribeCacheResponse"
    "fixture/DescribeCacheResponse"
    (Proxy :: Proxy DescribeCache)

testUpdateBandwidthRateLimitResponse :: UpdateBandwidthRateLimitResponse -> TestTree
testUpdateBandwidthRateLimitResponse = res
    "UpdateBandwidthRateLimitResponse"
    "fixture/UpdateBandwidthRateLimitResponse"
    (Proxy :: Proxy UpdateBandwidthRateLimit)

testDeleteBandwidthRateLimitResponse :: DeleteBandwidthRateLimitResponse -> TestTree
testDeleteBandwidthRateLimitResponse = res
    "DeleteBandwidthRateLimitResponse"
    "fixture/DeleteBandwidthRateLimitResponse"
    (Proxy :: Proxy DeleteBandwidthRateLimit)

testListLocalDisksResponse :: ListLocalDisksResponse -> TestTree
testListLocalDisksResponse = res
    "ListLocalDisksResponse"
    "fixture/ListLocalDisksResponse"
    (Proxy :: Proxy ListLocalDisks)

testDeleteTapeResponse :: DeleteTapeResponse -> TestTree
testDeleteTapeResponse = res
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
