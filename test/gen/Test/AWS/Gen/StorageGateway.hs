-- Module      : Test.AWS.Gen.StorageGateway
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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

import           Data.Proxy
import           Network.AWS.StorageGateway
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ cancelArchivalTest $
--             cancelArchival
--
--         , createStorediSCSIVolumeTest $
--             createStorediSCSIVolume
--
--         , createTapesTest $
--             createTapes
--
--         , createCachediSCSIVolumeTest $
--             createCachediSCSIVolume
--
--         , updateVTLDeviceTypeTest $
--             updateVTLDeviceType
--
--         , describeChapCredentialsTest $
--             describeChapCredentials
--
--         , addUploadBufferTest $
--             addUploadBuffer
--
--         , listVolumeInitiatorsTest $
--             listVolumeInitiators
--
--         , describeWorkingStorageTest $
--             describeWorkingStorage
--
--         , describeCachediSCSIVolumesTest $
--             describeCachediSCSIVolumes
--
--         , updateGatewayInformationTest $
--             updateGatewayInformation
--
--         , describeMaintenanceStartTimeTest $
--             describeMaintenanceStartTime
--
--         , addCacheTest $
--             addCache
--
--         , startGatewayTest $
--             startGateway
--
--         , shutdownGatewayTest $
--             shutdownGateway
--
--         , updateGatewaySoftwareNowTest $
--             updateGatewaySoftwareNow
--
--         , deleteChapCredentialsTest $
--             deleteChapCredentials
--
--         , updateChapCredentialsTest $
--             updateChapCredentials
--
--         , describeStorediSCSIVolumesTest $
--             describeStorediSCSIVolumes
--
--         , describeTapesTest $
--             describeTapes
--
--         , describeUploadBufferTest $
--             describeUploadBuffer
--
--         , createSnapshotFromVolumeRecoveryPointTest $
--             createSnapshotFromVolumeRecoveryPoint
--
--         , describeGatewayInformationTest $
--             describeGatewayInformation
--
--         , retrieveTapeRecoveryPointTest $
--             retrieveTapeRecoveryPoint
--
--         , updateMaintenanceStartTimeTest $
--             updateMaintenanceStartTime
--
--         , deleteGatewayTest $
--             deleteGateway
--
--         , disableGatewayTest $
--             disableGateway
--
--         , describeSnapshotScheduleTest $
--             describeSnapshotSchedule
--
--         , describeTapeArchivesTest $
--             describeTapeArchives
--
--         , retrieveTapeArchiveTest $
--             retrieveTapeArchive
--
--         , describeBandwidthRateLimitTest $
--             describeBandwidthRateLimit
--
--         , describeVTLDevicesTest $
--             describeVTLDevices
--
--         , createSnapshotTest $
--             createSnapshot
--
--         , updateSnapshotScheduleTest $
--             updateSnapshotSchedule
--
--         , cancelRetrievalTest $
--             cancelRetrieval
--
--         , deleteSnapshotScheduleTest $
--             deleteSnapshotSchedule
--
--         , deleteTapeArchiveTest $
--             deleteTapeArchive
--
--         , listVolumeRecoveryPointsTest $
--             listVolumeRecoveryPoints
--
--         , addWorkingStorageTest $
--             addWorkingStorage
--
--         , listGatewaysTest $
--             listGateways
--
--         , listVolumesTest $
--             listVolumes
--
--         , describeTapeRecoveryPointsTest $
--             describeTapeRecoveryPoints
--
--         , deleteVolumeTest $
--             deleteVolume
--
--         , resetCacheTest $
--             resetCache
--
--         , activateGatewayTest $
--             activateGateway
--
--         , describeCacheTest $
--             describeCache
--
--         , updateBandwidthRateLimitTest $
--             updateBandwidthRateLimit
--
--         , deleteBandwidthRateLimitTest $
--             deleteBandwidthRateLimit
--
--         , listLocalDisksTest $
--             listLocalDisks
--
--         , deleteTapeTest $
--             deleteTape
--
--           ]

--     , testGroup "response"
--         [ cancelArchivalResponseTest $
--             cancelArchivalResponse
--
--         , createStorediSCSIVolumeResponseTest $
--             createStorediSCSIVolumeResponse
--
--         , createTapesResponseTest $
--             createTapesResponse
--
--         , createCachediSCSIVolumeResponseTest $
--             createCachediSCSIVolumeResponse
--
--         , updateVTLDeviceTypeResponseTest $
--             updateVTLDeviceTypeResponse
--
--         , describeChapCredentialsResponseTest $
--             describeChapCredentialsResponse
--
--         , addUploadBufferResponseTest $
--             addUploadBufferResponse
--
--         , listVolumeInitiatorsResponseTest $
--             listVolumeInitiatorsResponse
--
--         , describeWorkingStorageResponseTest $
--             describeWorkingStorageResponse
--
--         , describeCachediSCSIVolumesResponseTest $
--             describeCachediSCSIVolumesResponse
--
--         , updateGatewayInformationResponseTest $
--             updateGatewayInformationResponse
--
--         , describeMaintenanceStartTimeResponseTest $
--             describeMaintenanceStartTimeResponse
--
--         , addCacheResponseTest $
--             addCacheResponse
--
--         , startGatewayResponseTest $
--             startGatewayResponse
--
--         , shutdownGatewayResponseTest $
--             shutdownGatewayResponse
--
--         , updateGatewaySoftwareNowResponseTest $
--             updateGatewaySoftwareNowResponse
--
--         , deleteChapCredentialsResponseTest $
--             deleteChapCredentialsResponse
--
--         , updateChapCredentialsResponseTest $
--             updateChapCredentialsResponse
--
--         , describeStorediSCSIVolumesResponseTest $
--             describeStorediSCSIVolumesResponse
--
--         , describeTapesResponseTest $
--             describeTapesResponse
--
--         , describeUploadBufferResponseTest $
--             describeUploadBufferResponse
--
--         , createSnapshotFromVolumeRecoveryPointResponseTest $
--             createSnapshotFromVolumeRecoveryPointResponse
--
--         , describeGatewayInformationResponseTest $
--             describeGatewayInformationResponse
--
--         , retrieveTapeRecoveryPointResponseTest $
--             retrieveTapeRecoveryPointResponse
--
--         , updateMaintenanceStartTimeResponseTest $
--             updateMaintenanceStartTimeResponse
--
--         , deleteGatewayResponseTest $
--             deleteGatewayResponse
--
--         , disableGatewayResponseTest $
--             disableGatewayResponse
--
--         , describeSnapshotScheduleResponseTest $
--             describeSnapshotScheduleResponse
--
--         , describeTapeArchivesResponseTest $
--             describeTapeArchivesResponse
--
--         , retrieveTapeArchiveResponseTest $
--             retrieveTapeArchiveResponse
--
--         , describeBandwidthRateLimitResponseTest $
--             describeBandwidthRateLimitResponse
--
--         , describeVTLDevicesResponseTest $
--             describeVTLDevicesResponse
--
--         , createSnapshotResponseTest $
--             createSnapshotResponse
--
--         , updateSnapshotScheduleResponseTest $
--             updateSnapshotScheduleResponse
--
--         , cancelRetrievalResponseTest $
--             cancelRetrievalResponse
--
--         , deleteSnapshotScheduleResponseTest $
--             deleteSnapshotScheduleResponse
--
--         , deleteTapeArchiveResponseTest $
--             deleteTapeArchiveResponse
--
--         , listVolumeRecoveryPointsResponseTest $
--             listVolumeRecoveryPointsResponse
--
--         , addWorkingStorageResponseTest $
--             addWorkingStorageResponse
--
--         , listGatewaysResponseTest $
--             listGatewaysResponse
--
--         , listVolumesResponseTest $
--             listVolumesResponse
--
--         , describeTapeRecoveryPointsResponseTest $
--             describeTapeRecoveryPointsResponse
--
--         , deleteVolumeResponseTest $
--             deleteVolumeResponse
--
--         , resetCacheResponseTest $
--             resetCacheResponse
--
--         , activateGatewayResponseTest $
--             activateGatewayResponse
--
--         , describeCacheResponseTest $
--             describeCacheResponse
--
--         , updateBandwidthRateLimitResponseTest $
--             updateBandwidthRateLimitResponse
--
--         , deleteBandwidthRateLimitResponseTest $
--             deleteBandwidthRateLimitResponse
--
--         , listLocalDisksResponseTest $
--             listLocalDisksResponse
--
--         , deleteTapeResponseTest $
--             deleteTapeResponse
--
--           ]
--     ]

-- Requests

cancelArchivalTest :: CancelArchival -> TestTree
cancelArchivalTest = undefined

createStorediSCSIVolumeTest :: CreateStorediSCSIVolume -> TestTree
createStorediSCSIVolumeTest = undefined

createTapesTest :: CreateTapes -> TestTree
createTapesTest = undefined

createCachediSCSIVolumeTest :: CreateCachediSCSIVolume -> TestTree
createCachediSCSIVolumeTest = undefined

updateVTLDeviceTypeTest :: UpdateVTLDeviceType -> TestTree
updateVTLDeviceTypeTest = undefined

describeChapCredentialsTest :: DescribeChapCredentials -> TestTree
describeChapCredentialsTest = undefined

addUploadBufferTest :: AddUploadBuffer -> TestTree
addUploadBufferTest = undefined

listVolumeInitiatorsTest :: ListVolumeInitiators -> TestTree
listVolumeInitiatorsTest = undefined

describeWorkingStorageTest :: DescribeWorkingStorage -> TestTree
describeWorkingStorageTest = undefined

describeCachediSCSIVolumesTest :: DescribeCachediSCSIVolumes -> TestTree
describeCachediSCSIVolumesTest = undefined

updateGatewayInformationTest :: UpdateGatewayInformation -> TestTree
updateGatewayInformationTest = undefined

describeMaintenanceStartTimeTest :: DescribeMaintenanceStartTime -> TestTree
describeMaintenanceStartTimeTest = undefined

addCacheTest :: AddCache -> TestTree
addCacheTest = undefined

startGatewayTest :: StartGateway -> TestTree
startGatewayTest = undefined

shutdownGatewayTest :: ShutdownGateway -> TestTree
shutdownGatewayTest = undefined

updateGatewaySoftwareNowTest :: UpdateGatewaySoftwareNow -> TestTree
updateGatewaySoftwareNowTest = undefined

deleteChapCredentialsTest :: DeleteChapCredentials -> TestTree
deleteChapCredentialsTest = undefined

updateChapCredentialsTest :: UpdateChapCredentials -> TestTree
updateChapCredentialsTest = undefined

describeStorediSCSIVolumesTest :: DescribeStorediSCSIVolumes -> TestTree
describeStorediSCSIVolumesTest = undefined

describeTapesTest :: DescribeTapes -> TestTree
describeTapesTest = undefined

describeUploadBufferTest :: DescribeUploadBuffer -> TestTree
describeUploadBufferTest = undefined

createSnapshotFromVolumeRecoveryPointTest :: CreateSnapshotFromVolumeRecoveryPoint -> TestTree
createSnapshotFromVolumeRecoveryPointTest = undefined

describeGatewayInformationTest :: DescribeGatewayInformation -> TestTree
describeGatewayInformationTest = undefined

retrieveTapeRecoveryPointTest :: RetrieveTapeRecoveryPoint -> TestTree
retrieveTapeRecoveryPointTest = undefined

updateMaintenanceStartTimeTest :: UpdateMaintenanceStartTime -> TestTree
updateMaintenanceStartTimeTest = undefined

deleteGatewayTest :: DeleteGateway -> TestTree
deleteGatewayTest = undefined

disableGatewayTest :: DisableGateway -> TestTree
disableGatewayTest = undefined

describeSnapshotScheduleTest :: DescribeSnapshotSchedule -> TestTree
describeSnapshotScheduleTest = undefined

describeTapeArchivesTest :: DescribeTapeArchives -> TestTree
describeTapeArchivesTest = undefined

retrieveTapeArchiveTest :: RetrieveTapeArchive -> TestTree
retrieveTapeArchiveTest = undefined

describeBandwidthRateLimitTest :: DescribeBandwidthRateLimit -> TestTree
describeBandwidthRateLimitTest = undefined

describeVTLDevicesTest :: DescribeVTLDevices -> TestTree
describeVTLDevicesTest = undefined

createSnapshotTest :: CreateSnapshot -> TestTree
createSnapshotTest = undefined

updateSnapshotScheduleTest :: UpdateSnapshotSchedule -> TestTree
updateSnapshotScheduleTest = undefined

cancelRetrievalTest :: CancelRetrieval -> TestTree
cancelRetrievalTest = undefined

deleteSnapshotScheduleTest :: DeleteSnapshotSchedule -> TestTree
deleteSnapshotScheduleTest = undefined

deleteTapeArchiveTest :: DeleteTapeArchive -> TestTree
deleteTapeArchiveTest = undefined

listVolumeRecoveryPointsTest :: ListVolumeRecoveryPoints -> TestTree
listVolumeRecoveryPointsTest = undefined

addWorkingStorageTest :: AddWorkingStorage -> TestTree
addWorkingStorageTest = undefined

listGatewaysTest :: ListGateways -> TestTree
listGatewaysTest = undefined

listVolumesTest :: ListVolumes -> TestTree
listVolumesTest = undefined

describeTapeRecoveryPointsTest :: DescribeTapeRecoveryPoints -> TestTree
describeTapeRecoveryPointsTest = undefined

deleteVolumeTest :: DeleteVolume -> TestTree
deleteVolumeTest = undefined

resetCacheTest :: ResetCache -> TestTree
resetCacheTest = undefined

activateGatewayTest :: ActivateGateway -> TestTree
activateGatewayTest = undefined

describeCacheTest :: DescribeCache -> TestTree
describeCacheTest = undefined

updateBandwidthRateLimitTest :: UpdateBandwidthRateLimit -> TestTree
updateBandwidthRateLimitTest = undefined

deleteBandwidthRateLimitTest :: DeleteBandwidthRateLimit -> TestTree
deleteBandwidthRateLimitTest = undefined

listLocalDisksTest :: ListLocalDisks -> TestTree
listLocalDisksTest = undefined

deleteTapeTest :: DeleteTape -> TestTree
deleteTapeTest = undefined

-- Responses

cancelArchivalResponseTest :: CancelArchivalResponse -> TestTree
cancelArchivalResponseTest = resp
    "CancelArchival"
    "fixture/StorageGateway/CancelArchivalResponse"
    (Proxy :: Proxy CancelArchival)

createStorediSCSIVolumeResponseTest :: CreateStorediSCSIVolumeResponse -> TestTree
createStorediSCSIVolumeResponseTest = resp
    "CreateStorediSCSIVolume"
    "fixture/StorageGateway/CreateStorediSCSIVolumeResponse"
    (Proxy :: Proxy CreateStorediSCSIVolume)

createTapesResponseTest :: CreateTapesResponse -> TestTree
createTapesResponseTest = resp
    "CreateTapes"
    "fixture/StorageGateway/CreateTapesResponse"
    (Proxy :: Proxy CreateTapes)

createCachediSCSIVolumeResponseTest :: CreateCachediSCSIVolumeResponse -> TestTree
createCachediSCSIVolumeResponseTest = resp
    "CreateCachediSCSIVolume"
    "fixture/StorageGateway/CreateCachediSCSIVolumeResponse"
    (Proxy :: Proxy CreateCachediSCSIVolume)

updateVTLDeviceTypeResponseTest :: UpdateVTLDeviceTypeResponse -> TestTree
updateVTLDeviceTypeResponseTest = resp
    "UpdateVTLDeviceType"
    "fixture/StorageGateway/UpdateVTLDeviceTypeResponse"
    (Proxy :: Proxy UpdateVTLDeviceType)

describeChapCredentialsResponseTest :: DescribeChapCredentialsResponse -> TestTree
describeChapCredentialsResponseTest = resp
    "DescribeChapCredentials"
    "fixture/StorageGateway/DescribeChapCredentialsResponse"
    (Proxy :: Proxy DescribeChapCredentials)

addUploadBufferResponseTest :: AddUploadBufferResponse -> TestTree
addUploadBufferResponseTest = resp
    "AddUploadBuffer"
    "fixture/StorageGateway/AddUploadBufferResponse"
    (Proxy :: Proxy AddUploadBuffer)

listVolumeInitiatorsResponseTest :: ListVolumeInitiatorsResponse -> TestTree
listVolumeInitiatorsResponseTest = resp
    "ListVolumeInitiators"
    "fixture/StorageGateway/ListVolumeInitiatorsResponse"
    (Proxy :: Proxy ListVolumeInitiators)

describeWorkingStorageResponseTest :: DescribeWorkingStorageResponse -> TestTree
describeWorkingStorageResponseTest = resp
    "DescribeWorkingStorage"
    "fixture/StorageGateway/DescribeWorkingStorageResponse"
    (Proxy :: Proxy DescribeWorkingStorage)

describeCachediSCSIVolumesResponseTest :: DescribeCachediSCSIVolumesResponse -> TestTree
describeCachediSCSIVolumesResponseTest = resp
    "DescribeCachediSCSIVolumes"
    "fixture/StorageGateway/DescribeCachediSCSIVolumesResponse"
    (Proxy :: Proxy DescribeCachediSCSIVolumes)

updateGatewayInformationResponseTest :: UpdateGatewayInformationResponse -> TestTree
updateGatewayInformationResponseTest = resp
    "UpdateGatewayInformation"
    "fixture/StorageGateway/UpdateGatewayInformationResponse"
    (Proxy :: Proxy UpdateGatewayInformation)

describeMaintenanceStartTimeResponseTest :: DescribeMaintenanceStartTimeResponse -> TestTree
describeMaintenanceStartTimeResponseTest = resp
    "DescribeMaintenanceStartTime"
    "fixture/StorageGateway/DescribeMaintenanceStartTimeResponse"
    (Proxy :: Proxy DescribeMaintenanceStartTime)

addCacheResponseTest :: AddCacheResponse -> TestTree
addCacheResponseTest = resp
    "AddCache"
    "fixture/StorageGateway/AddCacheResponse"
    (Proxy :: Proxy AddCache)

startGatewayResponseTest :: StartGatewayResponse -> TestTree
startGatewayResponseTest = resp
    "StartGateway"
    "fixture/StorageGateway/StartGatewayResponse"
    (Proxy :: Proxy StartGateway)

shutdownGatewayResponseTest :: ShutdownGatewayResponse -> TestTree
shutdownGatewayResponseTest = resp
    "ShutdownGateway"
    "fixture/StorageGateway/ShutdownGatewayResponse"
    (Proxy :: Proxy ShutdownGateway)

updateGatewaySoftwareNowResponseTest :: UpdateGatewaySoftwareNowResponse -> TestTree
updateGatewaySoftwareNowResponseTest = resp
    "UpdateGatewaySoftwareNow"
    "fixture/StorageGateway/UpdateGatewaySoftwareNowResponse"
    (Proxy :: Proxy UpdateGatewaySoftwareNow)

deleteChapCredentialsResponseTest :: DeleteChapCredentialsResponse -> TestTree
deleteChapCredentialsResponseTest = resp
    "DeleteChapCredentials"
    "fixture/StorageGateway/DeleteChapCredentialsResponse"
    (Proxy :: Proxy DeleteChapCredentials)

updateChapCredentialsResponseTest :: UpdateChapCredentialsResponse -> TestTree
updateChapCredentialsResponseTest = resp
    "UpdateChapCredentials"
    "fixture/StorageGateway/UpdateChapCredentialsResponse"
    (Proxy :: Proxy UpdateChapCredentials)

describeStorediSCSIVolumesResponseTest :: DescribeStorediSCSIVolumesResponse -> TestTree
describeStorediSCSIVolumesResponseTest = resp
    "DescribeStorediSCSIVolumes"
    "fixture/StorageGateway/DescribeStorediSCSIVolumesResponse"
    (Proxy :: Proxy DescribeStorediSCSIVolumes)

describeTapesResponseTest :: DescribeTapesResponse -> TestTree
describeTapesResponseTest = resp
    "DescribeTapes"
    "fixture/StorageGateway/DescribeTapesResponse"
    (Proxy :: Proxy DescribeTapes)

describeUploadBufferResponseTest :: DescribeUploadBufferResponse -> TestTree
describeUploadBufferResponseTest = resp
    "DescribeUploadBuffer"
    "fixture/StorageGateway/DescribeUploadBufferResponse"
    (Proxy :: Proxy DescribeUploadBuffer)

createSnapshotFromVolumeRecoveryPointResponseTest :: CreateSnapshotFromVolumeRecoveryPointResponse -> TestTree
createSnapshotFromVolumeRecoveryPointResponseTest = resp
    "CreateSnapshotFromVolumeRecoveryPoint"
    "fixture/StorageGateway/CreateSnapshotFromVolumeRecoveryPointResponse"
    (Proxy :: Proxy CreateSnapshotFromVolumeRecoveryPoint)

describeGatewayInformationResponseTest :: DescribeGatewayInformationResponse -> TestTree
describeGatewayInformationResponseTest = resp
    "DescribeGatewayInformation"
    "fixture/StorageGateway/DescribeGatewayInformationResponse"
    (Proxy :: Proxy DescribeGatewayInformation)

retrieveTapeRecoveryPointResponseTest :: RetrieveTapeRecoveryPointResponse -> TestTree
retrieveTapeRecoveryPointResponseTest = resp
    "RetrieveTapeRecoveryPoint"
    "fixture/StorageGateway/RetrieveTapeRecoveryPointResponse"
    (Proxy :: Proxy RetrieveTapeRecoveryPoint)

updateMaintenanceStartTimeResponseTest :: UpdateMaintenanceStartTimeResponse -> TestTree
updateMaintenanceStartTimeResponseTest = resp
    "UpdateMaintenanceStartTime"
    "fixture/StorageGateway/UpdateMaintenanceStartTimeResponse"
    (Proxy :: Proxy UpdateMaintenanceStartTime)

deleteGatewayResponseTest :: DeleteGatewayResponse -> TestTree
deleteGatewayResponseTest = resp
    "DeleteGateway"
    "fixture/StorageGateway/DeleteGatewayResponse"
    (Proxy :: Proxy DeleteGateway)

disableGatewayResponseTest :: DisableGatewayResponse -> TestTree
disableGatewayResponseTest = resp
    "DisableGateway"
    "fixture/StorageGateway/DisableGatewayResponse"
    (Proxy :: Proxy DisableGateway)

describeSnapshotScheduleResponseTest :: DescribeSnapshotScheduleResponse -> TestTree
describeSnapshotScheduleResponseTest = resp
    "DescribeSnapshotSchedule"
    "fixture/StorageGateway/DescribeSnapshotScheduleResponse"
    (Proxy :: Proxy DescribeSnapshotSchedule)

describeTapeArchivesResponseTest :: DescribeTapeArchivesResponse -> TestTree
describeTapeArchivesResponseTest = resp
    "DescribeTapeArchives"
    "fixture/StorageGateway/DescribeTapeArchivesResponse"
    (Proxy :: Proxy DescribeTapeArchives)

retrieveTapeArchiveResponseTest :: RetrieveTapeArchiveResponse -> TestTree
retrieveTapeArchiveResponseTest = resp
    "RetrieveTapeArchive"
    "fixture/StorageGateway/RetrieveTapeArchiveResponse"
    (Proxy :: Proxy RetrieveTapeArchive)

describeBandwidthRateLimitResponseTest :: DescribeBandwidthRateLimitResponse -> TestTree
describeBandwidthRateLimitResponseTest = resp
    "DescribeBandwidthRateLimit"
    "fixture/StorageGateway/DescribeBandwidthRateLimitResponse"
    (Proxy :: Proxy DescribeBandwidthRateLimit)

describeVTLDevicesResponseTest :: DescribeVTLDevicesResponse -> TestTree
describeVTLDevicesResponseTest = resp
    "DescribeVTLDevices"
    "fixture/StorageGateway/DescribeVTLDevicesResponse"
    (Proxy :: Proxy DescribeVTLDevices)

createSnapshotResponseTest :: CreateSnapshotResponse -> TestTree
createSnapshotResponseTest = resp
    "CreateSnapshot"
    "fixture/StorageGateway/CreateSnapshotResponse"
    (Proxy :: Proxy CreateSnapshot)

updateSnapshotScheduleResponseTest :: UpdateSnapshotScheduleResponse -> TestTree
updateSnapshotScheduleResponseTest = resp
    "UpdateSnapshotSchedule"
    "fixture/StorageGateway/UpdateSnapshotScheduleResponse"
    (Proxy :: Proxy UpdateSnapshotSchedule)

cancelRetrievalResponseTest :: CancelRetrievalResponse -> TestTree
cancelRetrievalResponseTest = resp
    "CancelRetrieval"
    "fixture/StorageGateway/CancelRetrievalResponse"
    (Proxy :: Proxy CancelRetrieval)

deleteSnapshotScheduleResponseTest :: DeleteSnapshotScheduleResponse -> TestTree
deleteSnapshotScheduleResponseTest = resp
    "DeleteSnapshotSchedule"
    "fixture/StorageGateway/DeleteSnapshotScheduleResponse"
    (Proxy :: Proxy DeleteSnapshotSchedule)

deleteTapeArchiveResponseTest :: DeleteTapeArchiveResponse -> TestTree
deleteTapeArchiveResponseTest = resp
    "DeleteTapeArchive"
    "fixture/StorageGateway/DeleteTapeArchiveResponse"
    (Proxy :: Proxy DeleteTapeArchive)

listVolumeRecoveryPointsResponseTest :: ListVolumeRecoveryPointsResponse -> TestTree
listVolumeRecoveryPointsResponseTest = resp
    "ListVolumeRecoveryPoints"
    "fixture/StorageGateway/ListVolumeRecoveryPointsResponse"
    (Proxy :: Proxy ListVolumeRecoveryPoints)

addWorkingStorageResponseTest :: AddWorkingStorageResponse -> TestTree
addWorkingStorageResponseTest = resp
    "AddWorkingStorage"
    "fixture/StorageGateway/AddWorkingStorageResponse"
    (Proxy :: Proxy AddWorkingStorage)

listGatewaysResponseTest :: ListGatewaysResponse -> TestTree
listGatewaysResponseTest = resp
    "ListGateways"
    "fixture/StorageGateway/ListGatewaysResponse"
    (Proxy :: Proxy ListGateways)

listVolumesResponseTest :: ListVolumesResponse -> TestTree
listVolumesResponseTest = resp
    "ListVolumes"
    "fixture/StorageGateway/ListVolumesResponse"
    (Proxy :: Proxy ListVolumes)

describeTapeRecoveryPointsResponseTest :: DescribeTapeRecoveryPointsResponse -> TestTree
describeTapeRecoveryPointsResponseTest = resp
    "DescribeTapeRecoveryPoints"
    "fixture/StorageGateway/DescribeTapeRecoveryPointsResponse"
    (Proxy :: Proxy DescribeTapeRecoveryPoints)

deleteVolumeResponseTest :: DeleteVolumeResponse -> TestTree
deleteVolumeResponseTest = resp
    "DeleteVolume"
    "fixture/StorageGateway/DeleteVolumeResponse"
    (Proxy :: Proxy DeleteVolume)

resetCacheResponseTest :: ResetCacheResponse -> TestTree
resetCacheResponseTest = resp
    "ResetCache"
    "fixture/StorageGateway/ResetCacheResponse"
    (Proxy :: Proxy ResetCache)

activateGatewayResponseTest :: ActivateGatewayResponse -> TestTree
activateGatewayResponseTest = resp
    "ActivateGateway"
    "fixture/StorageGateway/ActivateGatewayResponse"
    (Proxy :: Proxy ActivateGateway)

describeCacheResponseTest :: DescribeCacheResponse -> TestTree
describeCacheResponseTest = resp
    "DescribeCache"
    "fixture/StorageGateway/DescribeCacheResponse"
    (Proxy :: Proxy DescribeCache)

updateBandwidthRateLimitResponseTest :: UpdateBandwidthRateLimitResponse -> TestTree
updateBandwidthRateLimitResponseTest = resp
    "UpdateBandwidthRateLimit"
    "fixture/StorageGateway/UpdateBandwidthRateLimitResponse"
    (Proxy :: Proxy UpdateBandwidthRateLimit)

deleteBandwidthRateLimitResponseTest :: DeleteBandwidthRateLimitResponse -> TestTree
deleteBandwidthRateLimitResponseTest = resp
    "DeleteBandwidthRateLimit"
    "fixture/StorageGateway/DeleteBandwidthRateLimitResponse"
    (Proxy :: Proxy DeleteBandwidthRateLimit)

listLocalDisksResponseTest :: ListLocalDisksResponse -> TestTree
listLocalDisksResponseTest = resp
    "ListLocalDisks"
    "fixture/StorageGateway/ListLocalDisksResponse"
    (Proxy :: Proxy ListLocalDisks)

deleteTapeResponseTest :: DeleteTapeResponse -> TestTree
deleteTapeResponseTest = resp
    "DeleteTape"
    "fixture/StorageGateway/DeleteTapeResponse"
    (Proxy :: Proxy DeleteTape)
