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
    storageGateway
    (Proxy :: Proxy CancelArchival)

testCreateStorediSCSIVolumeResponse :: CreateStorediSCSIVolumeResponse -> TestTree
testCreateStorediSCSIVolumeResponse = res
    "CreateStorediSCSIVolumeResponse"
    "fixture/CreateStorediSCSIVolumeResponse"
    storageGateway
    (Proxy :: Proxy CreateStorediSCSIVolume)

testCreateTapesResponse :: CreateTapesResponse -> TestTree
testCreateTapesResponse = res
    "CreateTapesResponse"
    "fixture/CreateTapesResponse"
    storageGateway
    (Proxy :: Proxy CreateTapes)

testCreateCachediSCSIVolumeResponse :: CreateCachediSCSIVolumeResponse -> TestTree
testCreateCachediSCSIVolumeResponse = res
    "CreateCachediSCSIVolumeResponse"
    "fixture/CreateCachediSCSIVolumeResponse"
    storageGateway
    (Proxy :: Proxy CreateCachediSCSIVolume)

testUpdateVTLDeviceTypeResponse :: UpdateVTLDeviceTypeResponse -> TestTree
testUpdateVTLDeviceTypeResponse = res
    "UpdateVTLDeviceTypeResponse"
    "fixture/UpdateVTLDeviceTypeResponse"
    storageGateway
    (Proxy :: Proxy UpdateVTLDeviceType)

testDescribeChapCredentialsResponse :: DescribeChapCredentialsResponse -> TestTree
testDescribeChapCredentialsResponse = res
    "DescribeChapCredentialsResponse"
    "fixture/DescribeChapCredentialsResponse"
    storageGateway
    (Proxy :: Proxy DescribeChapCredentials)

testAddUploadBufferResponse :: AddUploadBufferResponse -> TestTree
testAddUploadBufferResponse = res
    "AddUploadBufferResponse"
    "fixture/AddUploadBufferResponse"
    storageGateway
    (Proxy :: Proxy AddUploadBuffer)

testListVolumeInitiatorsResponse :: ListVolumeInitiatorsResponse -> TestTree
testListVolumeInitiatorsResponse = res
    "ListVolumeInitiatorsResponse"
    "fixture/ListVolumeInitiatorsResponse"
    storageGateway
    (Proxy :: Proxy ListVolumeInitiators)

testDescribeWorkingStorageResponse :: DescribeWorkingStorageResponse -> TestTree
testDescribeWorkingStorageResponse = res
    "DescribeWorkingStorageResponse"
    "fixture/DescribeWorkingStorageResponse"
    storageGateway
    (Proxy :: Proxy DescribeWorkingStorage)

testDescribeCachediSCSIVolumesResponse :: DescribeCachediSCSIVolumesResponse -> TestTree
testDescribeCachediSCSIVolumesResponse = res
    "DescribeCachediSCSIVolumesResponse"
    "fixture/DescribeCachediSCSIVolumesResponse"
    storageGateway
    (Proxy :: Proxy DescribeCachediSCSIVolumes)

testUpdateGatewayInformationResponse :: UpdateGatewayInformationResponse -> TestTree
testUpdateGatewayInformationResponse = res
    "UpdateGatewayInformationResponse"
    "fixture/UpdateGatewayInformationResponse"
    storageGateway
    (Proxy :: Proxy UpdateGatewayInformation)

testDescribeMaintenanceStartTimeResponse :: DescribeMaintenanceStartTimeResponse -> TestTree
testDescribeMaintenanceStartTimeResponse = res
    "DescribeMaintenanceStartTimeResponse"
    "fixture/DescribeMaintenanceStartTimeResponse"
    storageGateway
    (Proxy :: Proxy DescribeMaintenanceStartTime)

testAddCacheResponse :: AddCacheResponse -> TestTree
testAddCacheResponse = res
    "AddCacheResponse"
    "fixture/AddCacheResponse"
    storageGateway
    (Proxy :: Proxy AddCache)

testStartGatewayResponse :: StartGatewayResponse -> TestTree
testStartGatewayResponse = res
    "StartGatewayResponse"
    "fixture/StartGatewayResponse"
    storageGateway
    (Proxy :: Proxy StartGateway)

testShutdownGatewayResponse :: ShutdownGatewayResponse -> TestTree
testShutdownGatewayResponse = res
    "ShutdownGatewayResponse"
    "fixture/ShutdownGatewayResponse"
    storageGateway
    (Proxy :: Proxy ShutdownGateway)

testUpdateGatewaySoftwareNowResponse :: UpdateGatewaySoftwareNowResponse -> TestTree
testUpdateGatewaySoftwareNowResponse = res
    "UpdateGatewaySoftwareNowResponse"
    "fixture/UpdateGatewaySoftwareNowResponse"
    storageGateway
    (Proxy :: Proxy UpdateGatewaySoftwareNow)

testDeleteChapCredentialsResponse :: DeleteChapCredentialsResponse -> TestTree
testDeleteChapCredentialsResponse = res
    "DeleteChapCredentialsResponse"
    "fixture/DeleteChapCredentialsResponse"
    storageGateway
    (Proxy :: Proxy DeleteChapCredentials)

testUpdateChapCredentialsResponse :: UpdateChapCredentialsResponse -> TestTree
testUpdateChapCredentialsResponse = res
    "UpdateChapCredentialsResponse"
    "fixture/UpdateChapCredentialsResponse"
    storageGateway
    (Proxy :: Proxy UpdateChapCredentials)

testDescribeStorediSCSIVolumesResponse :: DescribeStorediSCSIVolumesResponse -> TestTree
testDescribeStorediSCSIVolumesResponse = res
    "DescribeStorediSCSIVolumesResponse"
    "fixture/DescribeStorediSCSIVolumesResponse"
    storageGateway
    (Proxy :: Proxy DescribeStorediSCSIVolumes)

testDescribeTapesResponse :: DescribeTapesResponse -> TestTree
testDescribeTapesResponse = res
    "DescribeTapesResponse"
    "fixture/DescribeTapesResponse"
    storageGateway
    (Proxy :: Proxy DescribeTapes)

testDescribeUploadBufferResponse :: DescribeUploadBufferResponse -> TestTree
testDescribeUploadBufferResponse = res
    "DescribeUploadBufferResponse"
    "fixture/DescribeUploadBufferResponse"
    storageGateway
    (Proxy :: Proxy DescribeUploadBuffer)

testCreateSnapshotFromVolumeRecoveryPointResponse :: CreateSnapshotFromVolumeRecoveryPointResponse -> TestTree
testCreateSnapshotFromVolumeRecoveryPointResponse = res
    "CreateSnapshotFromVolumeRecoveryPointResponse"
    "fixture/CreateSnapshotFromVolumeRecoveryPointResponse"
    storageGateway
    (Proxy :: Proxy CreateSnapshotFromVolumeRecoveryPoint)

testDescribeGatewayInformationResponse :: DescribeGatewayInformationResponse -> TestTree
testDescribeGatewayInformationResponse = res
    "DescribeGatewayInformationResponse"
    "fixture/DescribeGatewayInformationResponse"
    storageGateway
    (Proxy :: Proxy DescribeGatewayInformation)

testRetrieveTapeRecoveryPointResponse :: RetrieveTapeRecoveryPointResponse -> TestTree
testRetrieveTapeRecoveryPointResponse = res
    "RetrieveTapeRecoveryPointResponse"
    "fixture/RetrieveTapeRecoveryPointResponse"
    storageGateway
    (Proxy :: Proxy RetrieveTapeRecoveryPoint)

testUpdateMaintenanceStartTimeResponse :: UpdateMaintenanceStartTimeResponse -> TestTree
testUpdateMaintenanceStartTimeResponse = res
    "UpdateMaintenanceStartTimeResponse"
    "fixture/UpdateMaintenanceStartTimeResponse"
    storageGateway
    (Proxy :: Proxy UpdateMaintenanceStartTime)

testDeleteGatewayResponse :: DeleteGatewayResponse -> TestTree
testDeleteGatewayResponse = res
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse"
    storageGateway
    (Proxy :: Proxy DeleteGateway)

testDisableGatewayResponse :: DisableGatewayResponse -> TestTree
testDisableGatewayResponse = res
    "DisableGatewayResponse"
    "fixture/DisableGatewayResponse"
    storageGateway
    (Proxy :: Proxy DisableGateway)

testDescribeSnapshotScheduleResponse :: DescribeSnapshotScheduleResponse -> TestTree
testDescribeSnapshotScheduleResponse = res
    "DescribeSnapshotScheduleResponse"
    "fixture/DescribeSnapshotScheduleResponse"
    storageGateway
    (Proxy :: Proxy DescribeSnapshotSchedule)

testDescribeTapeArchivesResponse :: DescribeTapeArchivesResponse -> TestTree
testDescribeTapeArchivesResponse = res
    "DescribeTapeArchivesResponse"
    "fixture/DescribeTapeArchivesResponse"
    storageGateway
    (Proxy :: Proxy DescribeTapeArchives)

testRetrieveTapeArchiveResponse :: RetrieveTapeArchiveResponse -> TestTree
testRetrieveTapeArchiveResponse = res
    "RetrieveTapeArchiveResponse"
    "fixture/RetrieveTapeArchiveResponse"
    storageGateway
    (Proxy :: Proxy RetrieveTapeArchive)

testDescribeBandwidthRateLimitResponse :: DescribeBandwidthRateLimitResponse -> TestTree
testDescribeBandwidthRateLimitResponse = res
    "DescribeBandwidthRateLimitResponse"
    "fixture/DescribeBandwidthRateLimitResponse"
    storageGateway
    (Proxy :: Proxy DescribeBandwidthRateLimit)

testDescribeVTLDevicesResponse :: DescribeVTLDevicesResponse -> TestTree
testDescribeVTLDevicesResponse = res
    "DescribeVTLDevicesResponse"
    "fixture/DescribeVTLDevicesResponse"
    storageGateway
    (Proxy :: Proxy DescribeVTLDevices)

testCreateSnapshotResponse :: CreateSnapshotResponse -> TestTree
testCreateSnapshotResponse = res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse"
    storageGateway
    (Proxy :: Proxy CreateSnapshot)

testUpdateSnapshotScheduleResponse :: UpdateSnapshotScheduleResponse -> TestTree
testUpdateSnapshotScheduleResponse = res
    "UpdateSnapshotScheduleResponse"
    "fixture/UpdateSnapshotScheduleResponse"
    storageGateway
    (Proxy :: Proxy UpdateSnapshotSchedule)

testCancelRetrievalResponse :: CancelRetrievalResponse -> TestTree
testCancelRetrievalResponse = res
    "CancelRetrievalResponse"
    "fixture/CancelRetrievalResponse"
    storageGateway
    (Proxy :: Proxy CancelRetrieval)

testDeleteSnapshotScheduleResponse :: DeleteSnapshotScheduleResponse -> TestTree
testDeleteSnapshotScheduleResponse = res
    "DeleteSnapshotScheduleResponse"
    "fixture/DeleteSnapshotScheduleResponse"
    storageGateway
    (Proxy :: Proxy DeleteSnapshotSchedule)

testDeleteTapeArchiveResponse :: DeleteTapeArchiveResponse -> TestTree
testDeleteTapeArchiveResponse = res
    "DeleteTapeArchiveResponse"
    "fixture/DeleteTapeArchiveResponse"
    storageGateway
    (Proxy :: Proxy DeleteTapeArchive)

testListVolumeRecoveryPointsResponse :: ListVolumeRecoveryPointsResponse -> TestTree
testListVolumeRecoveryPointsResponse = res
    "ListVolumeRecoveryPointsResponse"
    "fixture/ListVolumeRecoveryPointsResponse"
    storageGateway
    (Proxy :: Proxy ListVolumeRecoveryPoints)

testAddWorkingStorageResponse :: AddWorkingStorageResponse -> TestTree
testAddWorkingStorageResponse = res
    "AddWorkingStorageResponse"
    "fixture/AddWorkingStorageResponse"
    storageGateway
    (Proxy :: Proxy AddWorkingStorage)

testListGatewaysResponse :: ListGatewaysResponse -> TestTree
testListGatewaysResponse = res
    "ListGatewaysResponse"
    "fixture/ListGatewaysResponse"
    storageGateway
    (Proxy :: Proxy ListGateways)

testListVolumesResponse :: ListVolumesResponse -> TestTree
testListVolumesResponse = res
    "ListVolumesResponse"
    "fixture/ListVolumesResponse"
    storageGateway
    (Proxy :: Proxy ListVolumes)

testDescribeTapeRecoveryPointsResponse :: DescribeTapeRecoveryPointsResponse -> TestTree
testDescribeTapeRecoveryPointsResponse = res
    "DescribeTapeRecoveryPointsResponse"
    "fixture/DescribeTapeRecoveryPointsResponse"
    storageGateway
    (Proxy :: Proxy DescribeTapeRecoveryPoints)

testDeleteVolumeResponse :: DeleteVolumeResponse -> TestTree
testDeleteVolumeResponse = res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse"
    storageGateway
    (Proxy :: Proxy DeleteVolume)

testResetCacheResponse :: ResetCacheResponse -> TestTree
testResetCacheResponse = res
    "ResetCacheResponse"
    "fixture/ResetCacheResponse"
    storageGateway
    (Proxy :: Proxy ResetCache)

testActivateGatewayResponse :: ActivateGatewayResponse -> TestTree
testActivateGatewayResponse = res
    "ActivateGatewayResponse"
    "fixture/ActivateGatewayResponse"
    storageGateway
    (Proxy :: Proxy ActivateGateway)

testDescribeCacheResponse :: DescribeCacheResponse -> TestTree
testDescribeCacheResponse = res
    "DescribeCacheResponse"
    "fixture/DescribeCacheResponse"
    storageGateway
    (Proxy :: Proxy DescribeCache)

testUpdateBandwidthRateLimitResponse :: UpdateBandwidthRateLimitResponse -> TestTree
testUpdateBandwidthRateLimitResponse = res
    "UpdateBandwidthRateLimitResponse"
    "fixture/UpdateBandwidthRateLimitResponse"
    storageGateway
    (Proxy :: Proxy UpdateBandwidthRateLimit)

testDeleteBandwidthRateLimitResponse :: DeleteBandwidthRateLimitResponse -> TestTree
testDeleteBandwidthRateLimitResponse = res
    "DeleteBandwidthRateLimitResponse"
    "fixture/DeleteBandwidthRateLimitResponse"
    storageGateway
    (Proxy :: Proxy DeleteBandwidthRateLimit)

testListLocalDisksResponse :: ListLocalDisksResponse -> TestTree
testListLocalDisksResponse = res
    "ListLocalDisksResponse"
    "fixture/ListLocalDisksResponse"
    storageGateway
    (Proxy :: Proxy ListLocalDisks)

testDeleteTapeResponse :: DeleteTapeResponse -> TestTree
testDeleteTapeResponse = res
    "DeleteTapeResponse"
    "fixture/DeleteTapeResponse"
    storageGateway
    (Proxy :: Proxy DeleteTape)
