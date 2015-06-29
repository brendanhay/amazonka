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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.StorageGateway

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ activateGatewayTest $
--             activateGateway
--
--         , addCacheTest $
--             addCache
--
--         , addUploadBufferTest $
--             addUploadBuffer
--
--         , addWorkingStorageTest $
--             addWorkingStorage
--
--         , cancelArchivalTest $
--             cancelArchival
--
--         , cancelRetrievalTest $
--             cancelRetrieval
--
--         , createCachediSCSIVolumeTest $
--             createCachediSCSIVolume
--
--         , createSnapshotTest $
--             createSnapshot
--
--         , createSnapshotFromVolumeRecoveryPointTest $
--             createSnapshotFromVolumeRecoveryPoint
--
--         , createStorediSCSIVolumeTest $
--             createStorediSCSIVolume
--
--         , createTapesTest $
--             createTapes
--
--         , deleteBandwidthRateLimitTest $
--             deleteBandwidthRateLimit
--
--         , deleteChapCredentialsTest $
--             deleteChapCredentials
--
--         , deleteGatewayTest $
--             deleteGateway
--
--         , deleteSnapshotScheduleTest $
--             deleteSnapshotSchedule
--
--         , deleteTapeTest $
--             deleteTape
--
--         , deleteTapeArchiveTest $
--             deleteTapeArchive
--
--         , deleteVolumeTest $
--             deleteVolume
--
--         , describeBandwidthRateLimitTest $
--             describeBandwidthRateLimit
--
--         , describeCacheTest $
--             describeCache
--
--         , describeCachediSCSIVolumesTest $
--             describeCachediSCSIVolumes
--
--         , describeChapCredentialsTest $
--             describeChapCredentials
--
--         , describeGatewayInformationTest $
--             describeGatewayInformation
--
--         , describeMaintenanceStartTimeTest $
--             describeMaintenanceStartTime
--
--         , describeSnapshotScheduleTest $
--             describeSnapshotSchedule
--
--         , describeStorediSCSIVolumesTest $
--             describeStorediSCSIVolumes
--
--         , describeTapeArchivesTest $
--             describeTapeArchives
--
--         , describeTapeRecoveryPointsTest $
--             describeTapeRecoveryPoints
--
--         , describeTapesTest $
--             describeTapes
--
--         , describeUploadBufferTest $
--             describeUploadBuffer
--
--         , describeVTLDevicesTest $
--             describeVTLDevices
--
--         , describeWorkingStorageTest $
--             describeWorkingStorage
--
--         , disableGatewayTest $
--             disableGateway
--
--         , listGatewaysTest $
--             listGateways
--
--         , listLocalDisksTest $
--             listLocalDisks
--
--         , listVolumeInitiatorsTest $
--             listVolumeInitiators
--
--         , listVolumeRecoveryPointsTest $
--             listVolumeRecoveryPoints
--
--         , listVolumesTest $
--             listVolumes
--
--         , resetCacheTest $
--             resetCache
--
--         , retrieveTapeArchiveTest $
--             retrieveTapeArchive
--
--         , retrieveTapeRecoveryPointTest $
--             retrieveTapeRecoveryPoint
--
--         , shutdownGatewayTest $
--             shutdownGateway
--
--         , startGatewayTest $
--             startGateway
--
--         , updateBandwidthRateLimitTest $
--             updateBandwidthRateLimit
--
--         , updateChapCredentialsTest $
--             updateChapCredentials
--
--         , updateGatewayInformationTest $
--             updateGatewayInformation
--
--         , updateGatewaySoftwareNowTest $
--             updateGatewaySoftwareNow
--
--         , updateMaintenanceStartTimeTest $
--             updateMaintenanceStartTime
--
--         , updateSnapshotScheduleTest $
--             updateSnapshotSchedule
--
--         , updateVTLDeviceTypeTest $
--             updateVTLDeviceType
--
--           ]

--     , testGroup "response"
--         [ activateGatewayResponseTest $
--             activateGatewayResponse
--
--         , addCacheResponseTest $
--             addCacheResponse
--
--         , addUploadBufferResponseTest $
--             addUploadBufferResponse
--
--         , addWorkingStorageResponseTest $
--             addWorkingStorageResponse
--
--         , cancelArchivalResponseTest $
--             cancelArchivalResponse
--
--         , cancelRetrievalResponseTest $
--             cancelRetrievalResponse
--
--         , createCachediSCSIVolumeResponseTest $
--             createCachediSCSIVolumeResponse
--
--         , createSnapshotResponseTest $
--             createSnapshotResponse
--
--         , createSnapshotFromVolumeRecoveryPointResponseTest $
--             createSnapshotFromVolumeRecoveryPointResponse
--
--         , createStorediSCSIVolumeResponseTest $
--             createStorediSCSIVolumeResponse
--
--         , createTapesResponseTest $
--             createTapesResponse
--
--         , deleteBandwidthRateLimitResponseTest $
--             deleteBandwidthRateLimitResponse
--
--         , deleteChapCredentialsResponseTest $
--             deleteChapCredentialsResponse
--
--         , deleteGatewayResponseTest $
--             deleteGatewayResponse
--
--         , deleteSnapshotScheduleResponseTest $
--             deleteSnapshotScheduleResponse
--
--         , deleteTapeResponseTest $
--             deleteTapeResponse
--
--         , deleteTapeArchiveResponseTest $
--             deleteTapeArchiveResponse
--
--         , deleteVolumeResponseTest $
--             deleteVolumeResponse
--
--         , describeBandwidthRateLimitResponseTest $
--             describeBandwidthRateLimitResponse
--
--         , describeCacheResponseTest $
--             describeCacheResponse
--
--         , describeCachediSCSIVolumesResponseTest $
--             describeCachediSCSIVolumesResponse
--
--         , describeChapCredentialsResponseTest $
--             describeChapCredentialsResponse
--
--         , describeGatewayInformationResponseTest $
--             describeGatewayInformationResponse
--
--         , describeMaintenanceStartTimeResponseTest $
--             describeMaintenanceStartTimeResponse
--
--         , describeSnapshotScheduleResponseTest $
--             describeSnapshotScheduleResponse
--
--         , describeStorediSCSIVolumesResponseTest $
--             describeStorediSCSIVolumesResponse
--
--         , describeTapeArchivesResponseTest $
--             describeTapeArchivesResponse
--
--         , describeTapeRecoveryPointsResponseTest $
--             describeTapeRecoveryPointsResponse
--
--         , describeTapesResponseTest $
--             describeTapesResponse
--
--         , describeUploadBufferResponseTest $
--             describeUploadBufferResponse
--
--         , describeVTLDevicesResponseTest $
--             describeVTLDevicesResponse
--
--         , describeWorkingStorageResponseTest $
--             describeWorkingStorageResponse
--
--         , disableGatewayResponseTest $
--             disableGatewayResponse
--
--         , listGatewaysResponseTest $
--             listGatewaysResponse
--
--         , listLocalDisksResponseTest $
--             listLocalDisksResponse
--
--         , listVolumeInitiatorsResponseTest $
--             listVolumeInitiatorsResponse
--
--         , listVolumeRecoveryPointsResponseTest $
--             listVolumeRecoveryPointsResponse
--
--         , listVolumesResponseTest $
--             listVolumesResponse
--
--         , resetCacheResponseTest $
--             resetCacheResponse
--
--         , retrieveTapeArchiveResponseTest $
--             retrieveTapeArchiveResponse
--
--         , retrieveTapeRecoveryPointResponseTest $
--             retrieveTapeRecoveryPointResponse
--
--         , shutdownGatewayResponseTest $
--             shutdownGatewayResponse
--
--         , startGatewayResponseTest $
--             startGatewayResponse
--
--         , updateBandwidthRateLimitResponseTest $
--             updateBandwidthRateLimitResponse
--
--         , updateChapCredentialsResponseTest $
--             updateChapCredentialsResponse
--
--         , updateGatewayInformationResponseTest $
--             updateGatewayInformationResponse
--
--         , updateGatewaySoftwareNowResponseTest $
--             updateGatewaySoftwareNowResponse
--
--         , updateMaintenanceStartTimeResponseTest $
--             updateMaintenanceStartTimeResponse
--
--         , updateSnapshotScheduleResponseTest $
--             updateSnapshotScheduleResponse
--
--         , updateVTLDeviceTypeResponseTest $
--             updateVTLDeviceTypeResponse
--
--           ]
--     ]

-- Requests

activateGatewayTest :: ActivateGateway -> TestTree
activateGatewayTest = undefined

addCacheTest :: AddCache -> TestTree
addCacheTest = undefined

addUploadBufferTest :: AddUploadBuffer -> TestTree
addUploadBufferTest = undefined

addWorkingStorageTest :: AddWorkingStorage -> TestTree
addWorkingStorageTest = undefined

cancelArchivalTest :: CancelArchival -> TestTree
cancelArchivalTest = undefined

cancelRetrievalTest :: CancelRetrieval -> TestTree
cancelRetrievalTest = undefined

createCachediSCSIVolumeTest :: CreateCachediSCSIVolume -> TestTree
createCachediSCSIVolumeTest = undefined

createSnapshotTest :: CreateSnapshot -> TestTree
createSnapshotTest = undefined

createSnapshotFromVolumeRecoveryPointTest :: CreateSnapshotFromVolumeRecoveryPoint -> TestTree
createSnapshotFromVolumeRecoveryPointTest = undefined

createStorediSCSIVolumeTest :: CreateStorediSCSIVolume -> TestTree
createStorediSCSIVolumeTest = undefined

createTapesTest :: CreateTapes -> TestTree
createTapesTest = undefined

deleteBandwidthRateLimitTest :: DeleteBandwidthRateLimit -> TestTree
deleteBandwidthRateLimitTest = undefined

deleteChapCredentialsTest :: DeleteChapCredentials -> TestTree
deleteChapCredentialsTest = undefined

deleteGatewayTest :: DeleteGateway -> TestTree
deleteGatewayTest = undefined

deleteSnapshotScheduleTest :: DeleteSnapshotSchedule -> TestTree
deleteSnapshotScheduleTest = undefined

deleteTapeTest :: DeleteTape -> TestTree
deleteTapeTest = undefined

deleteTapeArchiveTest :: DeleteTapeArchive -> TestTree
deleteTapeArchiveTest = undefined

deleteVolumeTest :: DeleteVolume -> TestTree
deleteVolumeTest = undefined

describeBandwidthRateLimitTest :: DescribeBandwidthRateLimit -> TestTree
describeBandwidthRateLimitTest = undefined

describeCacheTest :: DescribeCache -> TestTree
describeCacheTest = undefined

describeCachediSCSIVolumesTest :: DescribeCachediSCSIVolumes -> TestTree
describeCachediSCSIVolumesTest = undefined

describeChapCredentialsTest :: DescribeChapCredentials -> TestTree
describeChapCredentialsTest = undefined

describeGatewayInformationTest :: DescribeGatewayInformation -> TestTree
describeGatewayInformationTest = undefined

describeMaintenanceStartTimeTest :: DescribeMaintenanceStartTime -> TestTree
describeMaintenanceStartTimeTest = undefined

describeSnapshotScheduleTest :: DescribeSnapshotSchedule -> TestTree
describeSnapshotScheduleTest = undefined

describeStorediSCSIVolumesTest :: DescribeStorediSCSIVolumes -> TestTree
describeStorediSCSIVolumesTest = undefined

describeTapeArchivesTest :: DescribeTapeArchives -> TestTree
describeTapeArchivesTest = undefined

describeTapeRecoveryPointsTest :: DescribeTapeRecoveryPoints -> TestTree
describeTapeRecoveryPointsTest = undefined

describeTapesTest :: DescribeTapes -> TestTree
describeTapesTest = undefined

describeUploadBufferTest :: DescribeUploadBuffer -> TestTree
describeUploadBufferTest = undefined

describeVTLDevicesTest :: DescribeVTLDevices -> TestTree
describeVTLDevicesTest = undefined

describeWorkingStorageTest :: DescribeWorkingStorage -> TestTree
describeWorkingStorageTest = undefined

disableGatewayTest :: DisableGateway -> TestTree
disableGatewayTest = undefined

listGatewaysTest :: ListGateways -> TestTree
listGatewaysTest = undefined

listLocalDisksTest :: ListLocalDisks -> TestTree
listLocalDisksTest = undefined

listVolumeInitiatorsTest :: ListVolumeInitiators -> TestTree
listVolumeInitiatorsTest = undefined

listVolumeRecoveryPointsTest :: ListVolumeRecoveryPoints -> TestTree
listVolumeRecoveryPointsTest = undefined

listVolumesTest :: ListVolumes -> TestTree
listVolumesTest = undefined

resetCacheTest :: ResetCache -> TestTree
resetCacheTest = undefined

retrieveTapeArchiveTest :: RetrieveTapeArchive -> TestTree
retrieveTapeArchiveTest = undefined

retrieveTapeRecoveryPointTest :: RetrieveTapeRecoveryPoint -> TestTree
retrieveTapeRecoveryPointTest = undefined

shutdownGatewayTest :: ShutdownGateway -> TestTree
shutdownGatewayTest = undefined

startGatewayTest :: StartGateway -> TestTree
startGatewayTest = undefined

updateBandwidthRateLimitTest :: UpdateBandwidthRateLimit -> TestTree
updateBandwidthRateLimitTest = undefined

updateChapCredentialsTest :: UpdateChapCredentials -> TestTree
updateChapCredentialsTest = undefined

updateGatewayInformationTest :: UpdateGatewayInformation -> TestTree
updateGatewayInformationTest = undefined

updateGatewaySoftwareNowTest :: UpdateGatewaySoftwareNow -> TestTree
updateGatewaySoftwareNowTest = undefined

updateMaintenanceStartTimeTest :: UpdateMaintenanceStartTime -> TestTree
updateMaintenanceStartTimeTest = undefined

updateSnapshotScheduleTest :: UpdateSnapshotSchedule -> TestTree
updateSnapshotScheduleTest = undefined

updateVTLDeviceTypeTest :: UpdateVTLDeviceType -> TestTree
updateVTLDeviceTypeTest = undefined

-- Responses

activateGatewayResponseTest :: ActivateGatewayResponse -> TestTree
activateGatewayResponseTest = resp
    "activateGatewayResponse"
    "fixture/ActivateGatewayResponse"
    (Proxy :: Proxy ActivateGateway)

addCacheResponseTest :: AddCacheResponse -> TestTree
addCacheResponseTest = resp
    "addCacheResponse"
    "fixture/AddCacheResponse"
    (Proxy :: Proxy AddCache)

addUploadBufferResponseTest :: AddUploadBufferResponse -> TestTree
addUploadBufferResponseTest = resp
    "addUploadBufferResponse"
    "fixture/AddUploadBufferResponse"
    (Proxy :: Proxy AddUploadBuffer)

addWorkingStorageResponseTest :: AddWorkingStorageResponse -> TestTree
addWorkingStorageResponseTest = resp
    "addWorkingStorageResponse"
    "fixture/AddWorkingStorageResponse"
    (Proxy :: Proxy AddWorkingStorage)

cancelArchivalResponseTest :: CancelArchivalResponse -> TestTree
cancelArchivalResponseTest = resp
    "cancelArchivalResponse"
    "fixture/CancelArchivalResponse"
    (Proxy :: Proxy CancelArchival)

cancelRetrievalResponseTest :: CancelRetrievalResponse -> TestTree
cancelRetrievalResponseTest = resp
    "cancelRetrievalResponse"
    "fixture/CancelRetrievalResponse"
    (Proxy :: Proxy CancelRetrieval)

createCachediSCSIVolumeResponseTest :: CreateCachediSCSIVolumeResponse -> TestTree
createCachediSCSIVolumeResponseTest = resp
    "createCachediSCSIVolumeResponse"
    "fixture/CreateCachediSCSIVolumeResponse"
    (Proxy :: Proxy CreateCachediSCSIVolume)

createSnapshotResponseTest :: CreateSnapshotResponse -> TestTree
createSnapshotResponseTest = resp
    "createSnapshotResponse"
    "fixture/CreateSnapshotResponse"
    (Proxy :: Proxy CreateSnapshot)

createSnapshotFromVolumeRecoveryPointResponseTest :: CreateSnapshotFromVolumeRecoveryPointResponse -> TestTree
createSnapshotFromVolumeRecoveryPointResponseTest = resp
    "createSnapshotFromVolumeRecoveryPointResponse"
    "fixture/CreateSnapshotFromVolumeRecoveryPointResponse"
    (Proxy :: Proxy CreateSnapshotFromVolumeRecoveryPoint)

createStorediSCSIVolumeResponseTest :: CreateStorediSCSIVolumeResponse -> TestTree
createStorediSCSIVolumeResponseTest = resp
    "createStorediSCSIVolumeResponse"
    "fixture/CreateStorediSCSIVolumeResponse"
    (Proxy :: Proxy CreateStorediSCSIVolume)

createTapesResponseTest :: CreateTapesResponse -> TestTree
createTapesResponseTest = resp
    "createTapesResponse"
    "fixture/CreateTapesResponse"
    (Proxy :: Proxy CreateTapes)

deleteBandwidthRateLimitResponseTest :: DeleteBandwidthRateLimitResponse -> TestTree
deleteBandwidthRateLimitResponseTest = resp
    "deleteBandwidthRateLimitResponse"
    "fixture/DeleteBandwidthRateLimitResponse"
    (Proxy :: Proxy DeleteBandwidthRateLimit)

deleteChapCredentialsResponseTest :: DeleteChapCredentialsResponse -> TestTree
deleteChapCredentialsResponseTest = resp
    "deleteChapCredentialsResponse"
    "fixture/DeleteChapCredentialsResponse"
    (Proxy :: Proxy DeleteChapCredentials)

deleteGatewayResponseTest :: DeleteGatewayResponse -> TestTree
deleteGatewayResponseTest = resp
    "deleteGatewayResponse"
    "fixture/DeleteGatewayResponse"
    (Proxy :: Proxy DeleteGateway)

deleteSnapshotScheduleResponseTest :: DeleteSnapshotScheduleResponse -> TestTree
deleteSnapshotScheduleResponseTest = resp
    "deleteSnapshotScheduleResponse"
    "fixture/DeleteSnapshotScheduleResponse"
    (Proxy :: Proxy DeleteSnapshotSchedule)

deleteTapeResponseTest :: DeleteTapeResponse -> TestTree
deleteTapeResponseTest = resp
    "deleteTapeResponse"
    "fixture/DeleteTapeResponse"
    (Proxy :: Proxy DeleteTape)

deleteTapeArchiveResponseTest :: DeleteTapeArchiveResponse -> TestTree
deleteTapeArchiveResponseTest = resp
    "deleteTapeArchiveResponse"
    "fixture/DeleteTapeArchiveResponse"
    (Proxy :: Proxy DeleteTapeArchive)

deleteVolumeResponseTest :: DeleteVolumeResponse -> TestTree
deleteVolumeResponseTest = resp
    "deleteVolumeResponse"
    "fixture/DeleteVolumeResponse"
    (Proxy :: Proxy DeleteVolume)

describeBandwidthRateLimitResponseTest :: DescribeBandwidthRateLimitResponse -> TestTree
describeBandwidthRateLimitResponseTest = resp
    "describeBandwidthRateLimitResponse"
    "fixture/DescribeBandwidthRateLimitResponse"
    (Proxy :: Proxy DescribeBandwidthRateLimit)

describeCacheResponseTest :: DescribeCacheResponse -> TestTree
describeCacheResponseTest = resp
    "describeCacheResponse"
    "fixture/DescribeCacheResponse"
    (Proxy :: Proxy DescribeCache)

describeCachediSCSIVolumesResponseTest :: DescribeCachediSCSIVolumesResponse -> TestTree
describeCachediSCSIVolumesResponseTest = resp
    "describeCachediSCSIVolumesResponse"
    "fixture/DescribeCachediSCSIVolumesResponse"
    (Proxy :: Proxy DescribeCachediSCSIVolumes)

describeChapCredentialsResponseTest :: DescribeChapCredentialsResponse -> TestTree
describeChapCredentialsResponseTest = resp
    "describeChapCredentialsResponse"
    "fixture/DescribeChapCredentialsResponse"
    (Proxy :: Proxy DescribeChapCredentials)

describeGatewayInformationResponseTest :: DescribeGatewayInformationResponse -> TestTree
describeGatewayInformationResponseTest = resp
    "describeGatewayInformationResponse"
    "fixture/DescribeGatewayInformationResponse"
    (Proxy :: Proxy DescribeGatewayInformation)

describeMaintenanceStartTimeResponseTest :: DescribeMaintenanceStartTimeResponse -> TestTree
describeMaintenanceStartTimeResponseTest = resp
    "describeMaintenanceStartTimeResponse"
    "fixture/DescribeMaintenanceStartTimeResponse"
    (Proxy :: Proxy DescribeMaintenanceStartTime)

describeSnapshotScheduleResponseTest :: DescribeSnapshotScheduleResponse -> TestTree
describeSnapshotScheduleResponseTest = resp
    "describeSnapshotScheduleResponse"
    "fixture/DescribeSnapshotScheduleResponse"
    (Proxy :: Proxy DescribeSnapshotSchedule)

describeStorediSCSIVolumesResponseTest :: DescribeStorediSCSIVolumesResponse -> TestTree
describeStorediSCSIVolumesResponseTest = resp
    "describeStorediSCSIVolumesResponse"
    "fixture/DescribeStorediSCSIVolumesResponse"
    (Proxy :: Proxy DescribeStorediSCSIVolumes)

describeTapeArchivesResponseTest :: DescribeTapeArchivesResponse -> TestTree
describeTapeArchivesResponseTest = resp
    "describeTapeArchivesResponse"
    "fixture/DescribeTapeArchivesResponse"
    (Proxy :: Proxy DescribeTapeArchives)

describeTapeRecoveryPointsResponseTest :: DescribeTapeRecoveryPointsResponse -> TestTree
describeTapeRecoveryPointsResponseTest = resp
    "describeTapeRecoveryPointsResponse"
    "fixture/DescribeTapeRecoveryPointsResponse"
    (Proxy :: Proxy DescribeTapeRecoveryPoints)

describeTapesResponseTest :: DescribeTapesResponse -> TestTree
describeTapesResponseTest = resp
    "describeTapesResponse"
    "fixture/DescribeTapesResponse"
    (Proxy :: Proxy DescribeTapes)

describeUploadBufferResponseTest :: DescribeUploadBufferResponse -> TestTree
describeUploadBufferResponseTest = resp
    "describeUploadBufferResponse"
    "fixture/DescribeUploadBufferResponse"
    (Proxy :: Proxy DescribeUploadBuffer)

describeVTLDevicesResponseTest :: DescribeVTLDevicesResponse -> TestTree
describeVTLDevicesResponseTest = resp
    "describeVTLDevicesResponse"
    "fixture/DescribeVTLDevicesResponse"
    (Proxy :: Proxy DescribeVTLDevices)

describeWorkingStorageResponseTest :: DescribeWorkingStorageResponse -> TestTree
describeWorkingStorageResponseTest = resp
    "describeWorkingStorageResponse"
    "fixture/DescribeWorkingStorageResponse"
    (Proxy :: Proxy DescribeWorkingStorage)

disableGatewayResponseTest :: DisableGatewayResponse -> TestTree
disableGatewayResponseTest = resp
    "disableGatewayResponse"
    "fixture/DisableGatewayResponse"
    (Proxy :: Proxy DisableGateway)

listGatewaysResponseTest :: ListGatewaysResponse -> TestTree
listGatewaysResponseTest = resp
    "listGatewaysResponse"
    "fixture/ListGatewaysResponse"
    (Proxy :: Proxy ListGateways)

listLocalDisksResponseTest :: ListLocalDisksResponse -> TestTree
listLocalDisksResponseTest = resp
    "listLocalDisksResponse"
    "fixture/ListLocalDisksResponse"
    (Proxy :: Proxy ListLocalDisks)

listVolumeInitiatorsResponseTest :: ListVolumeInitiatorsResponse -> TestTree
listVolumeInitiatorsResponseTest = resp
    "listVolumeInitiatorsResponse"
    "fixture/ListVolumeInitiatorsResponse"
    (Proxy :: Proxy ListVolumeInitiators)

listVolumeRecoveryPointsResponseTest :: ListVolumeRecoveryPointsResponse -> TestTree
listVolumeRecoveryPointsResponseTest = resp
    "listVolumeRecoveryPointsResponse"
    "fixture/ListVolumeRecoveryPointsResponse"
    (Proxy :: Proxy ListVolumeRecoveryPoints)

listVolumesResponseTest :: ListVolumesResponse -> TestTree
listVolumesResponseTest = resp
    "listVolumesResponse"
    "fixture/ListVolumesResponse"
    (Proxy :: Proxy ListVolumes)

resetCacheResponseTest :: ResetCacheResponse -> TestTree
resetCacheResponseTest = resp
    "resetCacheResponse"
    "fixture/ResetCacheResponse"
    (Proxy :: Proxy ResetCache)

retrieveTapeArchiveResponseTest :: RetrieveTapeArchiveResponse -> TestTree
retrieveTapeArchiveResponseTest = resp
    "retrieveTapeArchiveResponse"
    "fixture/RetrieveTapeArchiveResponse"
    (Proxy :: Proxy RetrieveTapeArchive)

retrieveTapeRecoveryPointResponseTest :: RetrieveTapeRecoveryPointResponse -> TestTree
retrieveTapeRecoveryPointResponseTest = resp
    "retrieveTapeRecoveryPointResponse"
    "fixture/RetrieveTapeRecoveryPointResponse"
    (Proxy :: Proxy RetrieveTapeRecoveryPoint)

shutdownGatewayResponseTest :: ShutdownGatewayResponse -> TestTree
shutdownGatewayResponseTest = resp
    "shutdownGatewayResponse"
    "fixture/ShutdownGatewayResponse"
    (Proxy :: Proxy ShutdownGateway)

startGatewayResponseTest :: StartGatewayResponse -> TestTree
startGatewayResponseTest = resp
    "startGatewayResponse"
    "fixture/StartGatewayResponse"
    (Proxy :: Proxy StartGateway)

updateBandwidthRateLimitResponseTest :: UpdateBandwidthRateLimitResponse -> TestTree
updateBandwidthRateLimitResponseTest = resp
    "updateBandwidthRateLimitResponse"
    "fixture/UpdateBandwidthRateLimitResponse"
    (Proxy :: Proxy UpdateBandwidthRateLimit)

updateChapCredentialsResponseTest :: UpdateChapCredentialsResponse -> TestTree
updateChapCredentialsResponseTest = resp
    "updateChapCredentialsResponse"
    "fixture/UpdateChapCredentialsResponse"
    (Proxy :: Proxy UpdateChapCredentials)

updateGatewayInformationResponseTest :: UpdateGatewayInformationResponse -> TestTree
updateGatewayInformationResponseTest = resp
    "updateGatewayInformationResponse"
    "fixture/UpdateGatewayInformationResponse"
    (Proxy :: Proxy UpdateGatewayInformation)

updateGatewaySoftwareNowResponseTest :: UpdateGatewaySoftwareNowResponse -> TestTree
updateGatewaySoftwareNowResponseTest = resp
    "updateGatewaySoftwareNowResponse"
    "fixture/UpdateGatewaySoftwareNowResponse"
    (Proxy :: Proxy UpdateGatewaySoftwareNow)

updateMaintenanceStartTimeResponseTest :: UpdateMaintenanceStartTimeResponse -> TestTree
updateMaintenanceStartTimeResponseTest = resp
    "updateMaintenanceStartTimeResponse"
    "fixture/UpdateMaintenanceStartTimeResponse"
    (Proxy :: Proxy UpdateMaintenanceStartTime)

updateSnapshotScheduleResponseTest :: UpdateSnapshotScheduleResponse -> TestTree
updateSnapshotScheduleResponseTest = resp
    "updateSnapshotScheduleResponse"
    "fixture/UpdateSnapshotScheduleResponse"
    (Proxy :: Proxy UpdateSnapshotSchedule)

updateVTLDeviceTypeResponseTest :: UpdateVTLDeviceTypeResponse -> TestTree
updateVTLDeviceTypeResponseTest = resp
    "updateVTLDeviceTypeResponse"
    "fixture/UpdateVTLDeviceTypeResponse"
    (Proxy :: Proxy UpdateVTLDeviceType)
