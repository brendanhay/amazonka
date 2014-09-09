{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.Monadic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Storage Gateway is a service that connects an on-premises software
-- appliance with cloud-based storage to provide seamless and secure
-- integration between your on-premises IT environment and AWS's storage
-- infrastructure.
--
-- This module is provided for convenience. It offers an alternative to the
-- common idiom of supplying required fields to an operations's smart constructor,
-- using the operation's lenses to modify additional fields, and then sending
-- or paginating the request.
--
-- As an example: using "Network.AWS.StorageGateway" with the smart constructor and
-- basic lens syntax, before explicitly calling 'send':
--
-- @
-- import Control.Monad.Trans.AWS
-- import Network.AWS.StorageGateway
--
-- send $ (mkOperationName w x)
--      & onLensField1 .~ y
--      & onLensField2 .~ z
-- @
--
-- Versus using "Network.AWS.StorageGateway.Monadic" with the 'State' operator variants from
-- "Control.Lens.Setter" such as '.=' to modify any additional request
-- parameters before sending:
--
-- @
-- import Network.AWS.StorageGateway.Monadic
--
-- operationName w x $ do
--     onLensField1 .= y
--     onLensField2 .= z
-- @
--
module Network.AWS.StorageGateway.V2013_06_30.Monadic
    (
    -- * ActivateGateway
    -- $ActivateGateway
      activateGateway
    , activateGatewayCatch

    -- * AddCache
    -- $AddCache
    , addCache
    , addCacheCatch

    -- * AddUploadBuffer
    -- $AddUploadBuffer
    , addUploadBuffer
    , addUploadBufferCatch

    -- * AddWorkingStorage
    -- $AddWorkingStorage
    , addWorkingStorage
    , addWorkingStorageCatch

    -- * CancelArchival
    -- $CancelArchival
    , cancelArchival
    , cancelArchivalCatch

    -- * CancelRetrieval
    -- $CancelRetrieval
    , cancelRetrieval
    , cancelRetrievalCatch

    -- * CreateCachediSCSIVolume
    -- $CreateCachediSCSIVolume
    , createCachediSCSIVolume
    , createCachediSCSIVolumeCatch

    -- * CreateSnapshot
    -- $CreateSnapshot
    , createSnapshot
    , createSnapshotCatch

    -- * CreateSnapshotFromVolumeRecoveryPoint
    -- $CreateSnapshotFromVolumeRecoveryPoint
    , createSnapshotFromVolumeRecoveryPoint
    , createSnapshotFromVolumeRecoveryPointCatch

    -- * CreateStorediSCSIVolume
    -- $CreateStorediSCSIVolume
    , createStorediSCSIVolume
    , createStorediSCSIVolumeCatch

    -- * CreateTapes
    -- $CreateTapes
    , createTapes
    , createTapesCatch

    -- * DeleteBandwidthRateLimit
    -- $DeleteBandwidthRateLimit
    , deleteBandwidthRateLimit
    , deleteBandwidthRateLimitCatch

    -- * DeleteChapCredentials
    -- $DeleteChapCredentials
    , deleteChapCredentials
    , deleteChapCredentialsCatch

    -- * DeleteGateway
    -- $DeleteGateway
    , deleteGateway
    , deleteGatewayCatch

    -- * DeleteSnapshotSchedule
    -- $DeleteSnapshotSchedule
    , deleteSnapshotSchedule
    , deleteSnapshotScheduleCatch

    -- * DeleteTape
    -- $DeleteTape
    , deleteTape
    , deleteTapeCatch

    -- * DeleteTapeArchive
    -- $DeleteTapeArchive
    , deleteTapeArchive
    , deleteTapeArchiveCatch

    -- * DeleteVolume
    -- $DeleteVolume
    , deleteVolume
    , deleteVolumeCatch

    -- * DescribeBandwidthRateLimit
    -- $DescribeBandwidthRateLimit
    , describeBandwidthRateLimit
    , describeBandwidthRateLimitCatch

    -- * DescribeCache
    -- $DescribeCache
    , describeCache
    , describeCacheCatch

    -- * DescribeCachediSCSIVolumes
    -- $DescribeCachediSCSIVolumes
    , describeCachediSCSIVolumes
    , describeCachediSCSIVolumesCatch

    -- * DescribeChapCredentials
    -- $DescribeChapCredentials
    , describeChapCredentials
    , describeChapCredentialsCatch

    -- * DescribeGatewayInformation
    -- $DescribeGatewayInformation
    , describeGatewayInformation
    , describeGatewayInformationCatch

    -- * DescribeMaintenanceStartTime
    -- $DescribeMaintenanceStartTime
    , describeMaintenanceStartTime
    , describeMaintenanceStartTimeCatch

    -- * DescribeSnapshotSchedule
    -- $DescribeSnapshotSchedule
    , describeSnapshotSchedule
    , describeSnapshotScheduleCatch

    -- * DescribeStorediSCSIVolumes
    -- $DescribeStorediSCSIVolumes
    , describeStorediSCSIVolumes
    , describeStorediSCSIVolumesCatch

    -- * DescribeTapeArchives
    -- $DescribeTapeArchives
    , describeTapeArchives
    , describeTapeArchivesCatch

    -- * DescribeTapeRecoveryPoints
    -- $DescribeTapeRecoveryPoints
    , describeTapeRecoveryPoints
    , describeTapeRecoveryPointsCatch

    -- * DescribeTapes
    -- $DescribeTapes
    , describeTapes
    , describeTapesCatch

    -- * DescribeUploadBuffer
    -- $DescribeUploadBuffer
    , describeUploadBuffer
    , describeUploadBufferCatch

    -- * DescribeVTLDevices
    -- $DescribeVTLDevices
    , describeVTLDevices
    , describeVTLDevicesCatch

    -- * DescribeWorkingStorage
    -- $DescribeWorkingStorage
    , describeWorkingStorage
    , describeWorkingStorageCatch

    -- * DisableGateway
    -- $DisableGateway
    , disableGateway
    , disableGatewayCatch

    -- * ListGateways
    -- $ListGateways
    , listGateways
    , listGatewaysCatch

    -- * ListLocalDisks
    -- $ListLocalDisks
    , listLocalDisks
    , listLocalDisksCatch

    -- * ListVolumeRecoveryPoints
    -- $ListVolumeRecoveryPoints
    , listVolumeRecoveryPoints
    , listVolumeRecoveryPointsCatch

    -- * ListVolumes
    -- $ListVolumes
    , listVolumes
    , listVolumesCatch

    -- * RetrieveTapeArchive
    -- $RetrieveTapeArchive
    , retrieveTapeArchive
    , retrieveTapeArchiveCatch

    -- * RetrieveTapeRecoveryPoint
    -- $RetrieveTapeRecoveryPoint
    , retrieveTapeRecoveryPoint
    , retrieveTapeRecoveryPointCatch

    -- * ShutdownGateway
    -- $ShutdownGateway
    , shutdownGateway
    , shutdownGatewayCatch

    -- * StartGateway
    -- $StartGateway
    , startGateway
    , startGatewayCatch

    -- * UpdateBandwidthRateLimit
    -- $UpdateBandwidthRateLimit
    , updateBandwidthRateLimit
    , updateBandwidthRateLimitCatch

    -- * UpdateChapCredentials
    -- $UpdateChapCredentials
    , updateChapCredentials
    , updateChapCredentialsCatch

    -- * UpdateGatewayInformation
    -- $UpdateGatewayInformation
    , updateGatewayInformation
    , updateGatewayInformationCatch

    -- * UpdateGatewaySoftwareNow
    -- $UpdateGatewaySoftwareNow
    , updateGatewaySoftwareNow
    , updateGatewaySoftwareNowCatch

    -- * UpdateMaintenanceStartTime
    -- $UpdateMaintenanceStartTime
    , updateMaintenanceStartTime
    , updateMaintenanceStartTimeCatch

    -- * UpdateSnapshotSchedule
    -- $UpdateSnapshotSchedule
    , updateSnapshotSchedule
    , updateSnapshotScheduleCatch

    -- * Re-exported
    , module Network.AWS.StorageGateway.V2013_06_30

    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.StorageGateway.V2013_06_30

type ServiceEr = Er StorageGateway


-- $ActivateGateway
-- This operation activates the gateway you previously deployed on your host.
-- For more information, see Downloading and Deploying AWS Storage Gateway VM.
-- In the activation process you specify information such as the region you
-- want to use for storing snapshots, the time zone for scheduled snapshots
-- and the gateway schedule window, an activation key, and a name for your
-- gateway. The activation process also associates your gateway with your
-- account (see UpdateGatewayInformation). You must power on the gateway VM
-- before you can activate your gateway. Example Request The following example
-- shows a request that activates a gateway. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.ActivateGateway { "ActivationKey":
-- "29AV1-3OFV9-VVIUB-NKT0I-LRO6V", "GatewayName": "mygateway",
-- "GatewayTimezone": "GMT-12:00", "GatewayRegion": "us-east-1",
-- "GatewayType": "STORED" } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 80 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.ActivateGateway'

activateGateway :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'agActivationKey'
    -> Text -- ^ 'agGatewayName'
    -> Text -- ^ 'agGatewayTimezone'
    -> Text -- ^ 'agGatewayRegion'
    -> State ActivateGateway a
    -> m ActivateGatewayResponse
activateGateway p1 p2 p3 p4 s =
    send $ (mkActivateGateway p1 p2 p3 p4) &~ s

activateGatewayCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'agActivationKey'
    -> Text -- ^ 'agGatewayName'
    -> Text -- ^ 'agGatewayTimezone'
    -> Text -- ^ 'agGatewayRegion'
    -> State ActivateGateway a
    -> m (Either ServiceEr ActivateGatewayResponse)
activateGatewayCatch p1 p2 p3 p4 s =
    sendCatch $ (mkActivateGateway p1 p2 p3 p4) &~ s

-- $AddCache
-- This operation configures one or more gateway local disks as cache for a
-- cached-volume gateway. This operation is supported only for the
-- gateway-cached volume architecture (see Storage Gateway Concepts). In the
-- request, you specify the gateway Amazon Resource Name (ARN) to which you
-- want to add cache, and one or more disk IDs that you want to configure as
-- cache. Example Request The following example shows a request that activates
-- a gateway. POST / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com
-- Content-Type: application/x-amz-json-1.1 Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20120425/us-east-1/storagegateway/aws4_request,
-- SignedHeaders=content-type;host;x-amz-date;x-amz-target,
-- Signature=9cd5a3584d1d67d57e61f120f35102d6b3649066abdd4bf4bbcf05bd9f2f8fe2
-- x-amz-date: 20120425T120000Z x-amz-target: StorageGateway_20120630.AddCache
-- { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway"
-- "DiskIds": [ "pci-0000:03:00.0-scsi-0:0:0:0",
-- "pci-0000:03:00.0-scsi-0:0:1:0" ] } HTTP/1.1 200 OK x-amzn-RequestId:
-- gur28r2rqlgb8vvs0mq17hlgij1q8glle1qeu3kpgg6f0kstauu0 Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-Type: application/x-amz-json-1.1 Content-length: 85 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.AddCache'

addCache :: ( MonadCatch m
            , MonadResource m
            , MonadError AWS.Error m
            , MonadReader Env m
            )
    => Text -- ^ 'acGatewayARN'
    -> [Text] -- ^ 'acDiskIds'
    -> State AddCache a
    -> m AddCacheResponse
addCache p1 p2 s =
    send $ (mkAddCache p1 p2) &~ s

addCacheCatch :: ( MonadCatch m
                 , MonadResource m
                 , MonadReader Env m
                 )
    => Text -- ^ 'acGatewayARN'
    -> [Text] -- ^ 'acDiskIds'
    -> State AddCache a
    -> m (Either ServiceEr AddCacheResponse)
addCacheCatch p1 p2 s =
    sendCatch $ (mkAddCache p1 p2) &~ s

-- $AddUploadBuffer
-- This operation configures one or more gateway local disks as upload buffer
-- for a specified gateway. This operation is supported for both the
-- gateway-stored and gateway-cached volume architectures. In the request, you
-- specify the gateway Amazon Resource Name (ARN) to which you want to add
-- upload buffer, and one or more disk IDs that you want to configure as
-- upload buffer.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.AddUploadBuffer'

addUploadBuffer :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'aubGatewayARN'
    -> [Text] -- ^ 'aubDiskIds'
    -> State AddUploadBuffer a
    -> m AddUploadBufferResponse
addUploadBuffer p1 p2 s =
    send $ (mkAddUploadBuffer p1 p2) &~ s

addUploadBufferCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'aubGatewayARN'
    -> [Text] -- ^ 'aubDiskIds'
    -> State AddUploadBuffer a
    -> m (Either ServiceEr AddUploadBufferResponse)
addUploadBufferCatch p1 p2 s =
    sendCatch $ (mkAddUploadBuffer p1 p2) &~ s

-- $AddWorkingStorage
-- This operation configures one or more gateway local disks as working
-- storage for a gateway. This operation is supported only for the
-- gateway-stored volume architecture. Working storage is also referred to as
-- upload buffer. You can also use the AddUploadBuffer operation to add upload
-- buffer to a stored-volume gateway. In the request, you specify the gateway
-- Amazon Resource Name (ARN) to which you want to add working storage, and
-- one or more disk IDs that you want to configure as working storage. Example
-- Request The following example shows a request that specifies that two local
-- disks of a gateway are to be configured as working storage. POST / HTTP/1.1
-- Host: storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.AddWorkingStorage { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway"
-- "DiskIds": ["pci-0000:03:00.0-scsi-0:0:0:0",
-- "pci-0000:04:00.0-scsi-1:0:0:0"] } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 80 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.AddWorkingStorage'

addWorkingStorage :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => Text -- ^ 'awsGatewayARN'
    -> [Text] -- ^ 'awsDiskIds'
    -> State AddWorkingStorage a
    -> m AddWorkingStorageResponse
addWorkingStorage p1 p2 s =
    send $ (mkAddWorkingStorage p1 p2) &~ s

addWorkingStorageCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => Text -- ^ 'awsGatewayARN'
    -> [Text] -- ^ 'awsDiskIds'
    -> State AddWorkingStorage a
    -> m (Either ServiceEr AddWorkingStorageResponse)
addWorkingStorageCatch p1 p2 s =
    sendCatch $ (mkAddWorkingStorage p1 p2) &~ s

-- $CancelArchival
-- See: 'Network.AWS.StorageGateway.V2013_06_30.CancelArchival'

cancelArchival :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'caGatewayARN'
    -> Text -- ^ 'caTapeARN'
    -> State CancelArchival a
    -> m CancelArchivalResponse
cancelArchival p1 p2 s =
    send $ (mkCancelArchival p1 p2) &~ s

cancelArchivalCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'caGatewayARN'
    -> Text -- ^ 'caTapeARN'
    -> State CancelArchival a
    -> m (Either ServiceEr CancelArchivalResponse)
cancelArchivalCatch p1 p2 s =
    sendCatch $ (mkCancelArchival p1 p2) &~ s

-- $CancelRetrieval
-- See: 'Network.AWS.StorageGateway.V2013_06_30.CancelRetrieval'

cancelRetrieval :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'crGatewayARN'
    -> Text -- ^ 'crTapeARN'
    -> State CancelRetrieval a
    -> m CancelRetrievalResponse
cancelRetrieval p1 p2 s =
    send $ (mkCancelRetrieval p1 p2) &~ s

cancelRetrievalCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'crGatewayARN'
    -> Text -- ^ 'crTapeARN'
    -> State CancelRetrieval a
    -> m (Either ServiceEr CancelRetrievalResponse)
cancelRetrievalCatch p1 p2 s =
    sendCatch $ (mkCancelRetrieval p1 p2) &~ s

-- $CreateCachediSCSIVolume
-- This operation creates a cached volume on a specified cached gateway. This
-- operation is supported only for the gateway-cached volume architecture.
-- Cache storage must be allocated to the gateway before you can create a
-- cached volume. Use the AddCache operation to add cache storage to a
-- gateway. In the request, you must specify the gateway, size of the volume
-- in bytes, the iSCSI target name, an IP address on which to expose the
-- target, and a unique client token. In response, AWS Storage Gateway creates
-- the volume and returns information about it such as the volume Amazon
-- Resource Name (ARN), its size, and the iSCSI target ARN that initiators can
-- use to connect to the volume target. Example Request The following example
-- shows a request that specifies that a local disk of a gateway be configured
-- as a cached volume. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com Content-Type:
-- application/x-amz-json-1.1 Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20120425/us-east-1/storagegateway/aws4_request,
-- SignedHeaders=content-type;host;x-amz-date;x-amz-target,
-- Signature=9cd5a3584d1d67d57e61f120f35102d6b3649066abdd4bf4bbcf05bd9f2f8fe2
-- x-amz-date: 20120912T120000Z x-amz-target:
-- StorageGateway_20120630.CreateCachediSCSIVolume { "ClientToken":
-- "cachedvol112233", "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "NetworkInterfaceId": "10.1.1.1", "TargetName": "myvolume",
-- "VolumeSizeInBytes": 536870912000 } HTTP/1.1 200 OK x-amzn-RequestId:
-- gur28r2rqlgb8vvs0mq17hlgij1q8glle1qeu3kpgg6f0kstauu0 Date: Wed, 12 Sep 2012
-- 12:00:02 GMT Content-Type: application/x-amz-json-1.1 Content-length: 263 {
-- "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume",
-- "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB"
-- }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.CreateCachediSCSIVolume'

createCachediSCSIVolume :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => Text -- ^ 'ccscsivGatewayARN'
    -> Integer -- ^ 'ccscsivVolumeSizeInBytes'
    -> Text -- ^ 'ccscsivTargetName'
    -> Text -- ^ 'ccscsivNetworkInterfaceId'
    -> Text -- ^ 'ccscsivClientToken'
    -> State CreateCachediSCSIVolume a
    -> m CreateCachediSCSIVolumeResponse
createCachediSCSIVolume p1 p2 p4 p5 p6 s =
    send $ (mkCreateCachediSCSIVolume p1 p2 p4 p5 p6) &~ s

createCachediSCSIVolumeCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => Text -- ^ 'ccscsivGatewayARN'
    -> Integer -- ^ 'ccscsivVolumeSizeInBytes'
    -> Text -- ^ 'ccscsivTargetName'
    -> Text -- ^ 'ccscsivNetworkInterfaceId'
    -> Text -- ^ 'ccscsivClientToken'
    -> State CreateCachediSCSIVolume a
    -> m (Either ServiceEr CreateCachediSCSIVolumeResponse)
createCachediSCSIVolumeCatch p1 p2 p4 p5 p6 s =
    sendCatch $ (mkCreateCachediSCSIVolume p1 p2 p4 p5 p6) &~ s

-- $CreateSnapshot
-- This operation initiates a snapshot of a volume. AWS Storage Gateway
-- provides the ability to back up point-in-time snapshots of your data to
-- Amazon Simple Storage (S3) for durable off-site recovery, as well as import
-- the data to an Amazon Elastic Block Store (EBS) volume in Amazon Elastic
-- Compute Cloud (EC2). You can take snapshots of your gateway volume on a
-- scheduled or ad-hoc basis. This API enables you to take ad-hoc snapshot.
-- For more information, see Working With Snapshots in the AWS Storage Gateway
-- Console. In the CreateSnapshot request you identify the volume by providing
-- its Amazon Resource Name (ARN). You must also provide description for the
-- snapshot. When AWS Storage Gateway takes the snapshot of specified volume,
-- the snapshot and description appears in the AWS Storage Gateway Console. In
-- response, AWS Storage Gateway returns you a snapshot ID. You can use this
-- snapshot ID to check the snapshot progress or later use it when you want to
-- create a volume from a snapshot. To list or delete a snapshot, you must use
-- the Amazon EC2 API. For more information, . Example Request The following
-- example sends a CreateSnapshot request to take snapshot of the specified an
-- example volume. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.CreateSnapshot { "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB",
-- "SnapshotDescription": "snapshot description" } HTTP/1.1 200 OK
-- x-amzn-RequestId: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Date: Wed, 25 Apr 2012 12:00:02 GMT Content-type:
-- application/x-amz-json-1.1 Content-length: 128 { "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB",
-- "SnapshotId": "snap-78e22663" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.CreateSnapshot'

createSnapshot :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'csVolumeARN'
    -> Text -- ^ 'csSnapshotDescription'
    -> State CreateSnapshot a
    -> m CreateSnapshotResponse
createSnapshot p1 p2 s =
    send $ (mkCreateSnapshot p1 p2) &~ s

createSnapshotCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'csVolumeARN'
    -> Text -- ^ 'csSnapshotDescription'
    -> State CreateSnapshot a
    -> m (Either ServiceEr CreateSnapshotResponse)
createSnapshotCatch p1 p2 s =
    sendCatch $ (mkCreateSnapshot p1 p2) &~ s

-- $CreateSnapshotFromVolumeRecoveryPoint
-- This operation initiates a snapshot of a gateway from a volume recovery
-- point. This operation is supported only for the gateway-cached volume
-- architecture (see ). A volume recovery point is a point in time at which
-- all data of the volume is consistent and from which you can create a
-- snapshot. To get a list of volume recovery point for gateway-cached
-- volumes, use ListVolumeRecoveryPoints. In the
-- CreateSnapshotFromVolumeRecoveryPoint request, you identify the volume by
-- providing its Amazon Resource Name (ARN). You must also provide a
-- description for the snapshot. When AWS Storage Gateway takes a snapshot of
-- the specified volume, the snapshot and its description appear in the AWS
-- Storage Gateway console. In response, AWS Storage Gateway returns you a
-- snapshot ID. You can use this snapshot ID to check the snapshot progress or
-- later use it when you want to create a volume from a snapshot. To list or
-- delete a snapshot, you must use the Amazon EC2 API. For more information,
-- in Amazon Elastic Compute Cloud API Reference. Example Request The
-- following example sends a CreateSnapshotFromVolumeRecoveryPoint request to
-- take snapshot of the specified an example volume. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com Content-Type:
-- application/x-amz-json-1.1 Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20120425/us-east-1/storagegateway/aws4_request,
-- SignedHeaders=content-type;host;x-amz-date;x-amz-target,
-- Signature=9cd5a3584d1d67d57e61f120f35102d6b3649066abdd4bf4bbcf05bd9f2f8fe2
-- x-amz-date: 20120912T120000Z x-amz-target:
-- StorageGateway_20120630.CreateSnapshotFromVolumeRecoveryPoint {
-- "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB",
-- "SnapshotDescription": "snapshot description" } HTTP/1.1 200 OK
-- x-amzn-RequestId: gur28r2rqlgb8vvs0mq17hlgij1q8glle1qeu3kpgg6f0kstauu0
-- Date: Wed, 12 Sep 2012 12:00:02 GMT Content-Type:
-- application/x-amz-json-1.1 Content-length: 137 { "SnapshotId":
-- "snap-78e22663", "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB",
-- "VolumeRecoveryPointTime": "2012-06-30T10:10:10.000Z" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.CreateSnapshotFromVolumeRecoveryPoint'

createSnapshotFromVolumeRecoveryPoint :: ( MonadCatch m
                                         , MonadResource m
                                         , MonadError AWS.Error m
                                         , MonadReader Env m
                                         )
    => Text -- ^ 'csfvrpVolumeARN'
    -> Text -- ^ 'csfvrpSnapshotDescription'
    -> State CreateSnapshotFromVolumeRecoveryPoint a
    -> m CreateSnapshotFromVolumeRecoveryPointResponse
createSnapshotFromVolumeRecoveryPoint p1 p2 s =
    send $ (mkCreateSnapshotFromVolumeRecoveryPoint p1 p2) &~ s

createSnapshotFromVolumeRecoveryPointCatch :: ( MonadCatch m
                                              , MonadResource m
                                              , MonadReader Env m
                                              )
    => Text -- ^ 'csfvrpVolumeARN'
    -> Text -- ^ 'csfvrpSnapshotDescription'
    -> State CreateSnapshotFromVolumeRecoveryPoint a
    -> m (Either ServiceEr CreateSnapshotFromVolumeRecoveryPointResponse)
createSnapshotFromVolumeRecoveryPointCatch p1 p2 s =
    sendCatch $ (mkCreateSnapshotFromVolumeRecoveryPoint p1 p2) &~ s

-- $CreateStorediSCSIVolume
-- This operation creates a volume on a specified gateway. This operation is
-- supported only for the gateway-cached volume architecture. The size of the
-- volume to create is inferred from the disk size. You can choose to preserve
-- existing data on the disk, create volume from an existing snapshot, or
-- create an empty volume. If you choose to create an empty gateway volume,
-- then any existing data on the disk is erased. In the request you must
-- specify the gateway and the disk information on which you are creating the
-- volume. In response, AWS Storage Gateway creates the volume and returns
-- volume information such as the volume Amazon Resource Name (ARN), its size,
-- and the iSCSI target ARN that initiators can use to connect to the volume
-- target. Example Request The following example shows a request that
-- specifies that a local disk of a gateway be configured as a volume. POST /
-- HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com x-amz-Date:
-- 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.CreateStorediSCSIVolume { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "DiskId": "pci-0000:03:00.0-scsi-0:0:0:0", "PreserveExistingData": true,
-- "TargetName": "myvolume", "NetworkInterfaceId": "10.1.1.1" } HTTP/1.1 200
-- OK x-amzn-RequestId: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Date: Wed, 25 Apr 2012 12:00:02 GMT Content-type:
-- application/x-amz-json-1.1 Content-length: 215 { "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB",
-- "VolumeSizeInBytes": 1099511627776, "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume"
-- }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.CreateStorediSCSIVolume'

createStorediSCSIVolume :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => Text -- ^ 'csscsivGatewayARN'
    -> Text -- ^ 'csscsivDiskId'
    -> Bool -- ^ 'csscsivPreserveExistingData'
    -> Text -- ^ 'csscsivTargetName'
    -> Text -- ^ 'csscsivNetworkInterfaceId'
    -> State CreateStorediSCSIVolume a
    -> m CreateStorediSCSIVolumeResponse
createStorediSCSIVolume p1 p2 p4 p5 p6 s =
    send $ (mkCreateStorediSCSIVolume p1 p2 p4 p5 p6) &~ s

createStorediSCSIVolumeCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => Text -- ^ 'csscsivGatewayARN'
    -> Text -- ^ 'csscsivDiskId'
    -> Bool -- ^ 'csscsivPreserveExistingData'
    -> Text -- ^ 'csscsivTargetName'
    -> Text -- ^ 'csscsivNetworkInterfaceId'
    -> State CreateStorediSCSIVolume a
    -> m (Either ServiceEr CreateStorediSCSIVolumeResponse)
createStorediSCSIVolumeCatch p1 p2 p4 p5 p6 s =
    sendCatch $ (mkCreateStorediSCSIVolume p1 p2 p4 p5 p6) &~ s

-- $CreateTapes
-- See: 'Network.AWS.StorageGateway.V2013_06_30.CreateTapes'

createTapes :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => Text -- ^ 'ctGatewayARN'
    -> Integer -- ^ 'ctTapeSizeInBytes'
    -> Text -- ^ 'ctClientToken'
    -> Integer -- ^ 'ctNumTapesToCreate'
    -> Text -- ^ 'ctTapeBarcodePrefix'
    -> State CreateTapes a
    -> m CreateTapesResponse
createTapes p1 p2 p3 p4 p5 s =
    send $ (mkCreateTapes p1 p2 p3 p4 p5) &~ s

createTapesCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => Text -- ^ 'ctGatewayARN'
    -> Integer -- ^ 'ctTapeSizeInBytes'
    -> Text -- ^ 'ctClientToken'
    -> Integer -- ^ 'ctNumTapesToCreate'
    -> Text -- ^ 'ctTapeBarcodePrefix'
    -> State CreateTapes a
    -> m (Either ServiceEr CreateTapesResponse)
createTapesCatch p1 p2 p3 p4 p5 s =
    sendCatch $ (mkCreateTapes p1 p2 p3 p4 p5) &~ s

-- $DeleteBandwidthRateLimit
-- This operation deletes the bandwidth rate limits of a gateway. You can
-- delete either the upload and download bandwidth rate limit, or you can
-- delete both. If you delete only one of the limits, the other limit remains
-- unchanged. To specify which gateway to work with, use the Amazon Resource
-- Name (ARN) of the gateway in your request. Example Request The following
-- example shows a request that deletes both of the bandwidth rate limits of a
-- gateway. POST / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com
-- x-amz-Date: 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DeleteBandwidthRateLimit { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "BandwidthType: "All" } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 80 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DeleteBandwidthRateLimit'

deleteBandwidthRateLimit :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => Text -- ^ 'dbrlGatewayARN'
    -> Text -- ^ 'dbrlBandwidthType'
    -> State DeleteBandwidthRateLimit a
    -> m DeleteBandwidthRateLimitResponse
deleteBandwidthRateLimit p1 p2 s =
    send $ (mkDeleteBandwidthRateLimit p1 p2) &~ s

deleteBandwidthRateLimitCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'dbrlGatewayARN'
    -> Text -- ^ 'dbrlBandwidthType'
    -> State DeleteBandwidthRateLimit a
    -> m (Either ServiceEr DeleteBandwidthRateLimitResponse)
deleteBandwidthRateLimitCatch p1 p2 s =
    sendCatch $ (mkDeleteBandwidthRateLimit p1 p2) &~ s

-- $DeleteChapCredentials
-- This operation deletes Challenge-Handshake Authentication Protocol (CHAP)
-- credentials for a specified iSCSI target and initiator pair. Example
-- Request The following example shows a request that deletes the CHAP
-- credentials for an iSCSI target myvolume. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DeleteChapCredentials { "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume",
-- "InitiatorName":
-- "iqn.1991-05.com.microsoft:computername.domain.example.com" } HTTP/1.1 200
-- OK x-amzn-RequestId: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Date: Wed, 25 Apr 2012 12:00:02 GMT Content-type:
-- application/x-amz-json-1.1 Content-length: 161 { "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume",
-- "InitiatorName":
-- "iqn.1991-05.com.microsoft:computername.domain.example.com" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DeleteChapCredentials'

deleteChapCredentials :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'dccTargetARN'
    -> Text -- ^ 'dccInitiatorName'
    -> State DeleteChapCredentials a
    -> m DeleteChapCredentialsResponse
deleteChapCredentials p1 p2 s =
    send $ (mkDeleteChapCredentials p1 p2) &~ s

deleteChapCredentialsCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'dccTargetARN'
    -> Text -- ^ 'dccInitiatorName'
    -> State DeleteChapCredentials a
    -> m (Either ServiceEr DeleteChapCredentialsResponse)
deleteChapCredentialsCatch p1 p2 s =
    sendCatch $ (mkDeleteChapCredentials p1 p2) &~ s

-- $DeleteGateway
-- This operation deletes a gateway. To specify which gateway to delete, use
-- the Amazon Resource Name (ARN) of the gateway in your request. The
-- operation deletes the gateway; however, it does not delete the gateway
-- virtual machine (VM) from your host computer. After you delete a gateway,
-- you cannot reactivate it. Completed snapshots of the gateway volumes are
-- not deleted upon deleting the gateway, however, pending snapshots will not
-- complete. After you delete a gateway, your next step is to remove it from
-- your environment. You no longer pay software charges after the gateway is
-- deleted; however, your existing Amazon EBS snapshots persist and you will
-- continue to be billed for these snapshots. You can choose to remove all
-- remaining Amazon EBS snapshots by canceling your Amazon EC2 subscription.
-- If you prefer not to cancel your Amazon EC2 subscription, you can delete
-- your snapshots using the Amazon EC2 console. For more information, see the
-- AWS Storage Gateway Detail Page. Example Request The following example
-- shows a request that deactivates a gateway. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DeleteGateway { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 80 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DeleteGateway'

deleteGateway :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => Text -- ^ 'dgGatewayARN'
    -> State DeleteGateway a
    -> m DeleteGatewayResponse
deleteGateway p1 s =
    send $ (mkDeleteGateway p1) &~ s

deleteGatewayCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => Text -- ^ 'dgGatewayARN'
    -> State DeleteGateway a
    -> m (Either ServiceEr DeleteGatewayResponse)
deleteGatewayCatch p1 s =
    sendCatch $ (mkDeleteGateway p1) &~ s

-- $DeleteSnapshotSchedule
-- This operation deletes a snapshot of a volume. You can take snapshots of
-- your gateway volumes on a scheduled or ad-hoc basis. This API enables you
-- to delete a snapshot schedule for a volume. For more information, see
-- Working with Snapshots. In the DeleteSnapshotSchedule request, you identify
-- the volume by providing its Amazon Resource Name (ARN). To list or delete a
-- snapshot, you must use the Amazon EC2 API. in Amazon Elastic Compute Cloud
-- API Reference. Example Request The following example... POST / HTTP/1.1
-- Host: storagegateway.us-east-1.amazonaws.com Content-Type:
-- application/x-amz-json-1.1 Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20120425/us-east-1/storagegateway/aws4_request,
-- SignedHeaders=content-type;host;x-amz-date;x-amz-target,
-- Signature=9cd5a3584d1d67d57e61f120f35102d6b3649066abdd4bf4bbcf05bd9f2f8fe2
-- x-amz-date: 20120912T120000Z x-amz-target:
-- StorageGateway_20120630.DeleteSnapshotSchedule { "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB"
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- gur28r2rqlgb8vvs0mq17hlgij1q8glle1qeu3kpgg6f0kstauu0 Date: Wed, 12 Sep 2012
-- 12:00:02 GMT Content-Type: application/x-amz-json-1.1 Content-length: 137 {
-- "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB"
-- }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DeleteSnapshotSchedule'

deleteSnapshotSchedule :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'dssVolumeARN'
    -> State DeleteSnapshotSchedule a
    -> m DeleteSnapshotScheduleResponse
deleteSnapshotSchedule p1 s =
    send $ (mkDeleteSnapshotSchedule p1) &~ s

deleteSnapshotScheduleCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'dssVolumeARN'
    -> State DeleteSnapshotSchedule a
    -> m (Either ServiceEr DeleteSnapshotScheduleResponse)
deleteSnapshotScheduleCatch p1 s =
    sendCatch $ (mkDeleteSnapshotSchedule p1) &~ s

-- $DeleteTape
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DeleteTape'

deleteTape :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              )
    => Text -- ^ 'dtGatewayARN'
    -> Text -- ^ 'dtTapeARN'
    -> State DeleteTape a
    -> m DeleteTapeResponse
deleteTape p1 p2 s =
    send $ (mkDeleteTape p1 p2) &~ s

deleteTapeCatch :: ( MonadCatch m
                   , MonadResource m
                   , MonadReader Env m
                   )
    => Text -- ^ 'dtGatewayARN'
    -> Text -- ^ 'dtTapeARN'
    -> State DeleteTape a
    -> m (Either ServiceEr DeleteTapeResponse)
deleteTapeCatch p1 p2 s =
    sendCatch $ (mkDeleteTape p1 p2) &~ s

-- $DeleteTapeArchive
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DeleteTapeArchive'

deleteTapeArchive :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => Text -- ^ 'dtaTapeARN'
    -> State DeleteTapeArchive a
    -> m DeleteTapeArchiveResponse
deleteTapeArchive p1 s =
    send $ (mkDeleteTapeArchive p1) &~ s

deleteTapeArchiveCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => Text -- ^ 'dtaTapeARN'
    -> State DeleteTapeArchive a
    -> m (Either ServiceEr DeleteTapeArchiveResponse)
deleteTapeArchiveCatch p1 s =
    sendCatch $ (mkDeleteTapeArchive p1) &~ s

-- $DeleteVolume
-- This operation delete the specified gateway volume that you previously
-- created using the CreateStorediSCSIVolume API. For gateway-stored volumes,
-- the local disk that was configured as the storage volume is not deleted.
-- You can reuse the local disk to create another storage volume. Before you
-- delete a gateway volume, make sure there are no iSCSI connections to the
-- volume you are deleting. You should also make sure there is no snapshot in
-- progress. You can use the Amazon Elastic Compute Cloud (Amazon EC2) API to
-- query snapshots on the volume you are deleting and check the snapshot
-- status. For more information, go to DescribeSnapshots in the Amazon Elastic
-- Compute Cloud API Reference. In the request, you must provide the Amazon
-- Resource Name (ARN) of the storage volume you want to delete. Example
-- Request The following example shows a request that deletes a volume. POST /
-- HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com x-amz-Date:
-- 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DeleteVolume { "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB"
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 99 {
-- "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB"
-- }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DeleteVolume'

deleteVolume :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'dvVolumeARN'
    -> State DeleteVolume a
    -> m DeleteVolumeResponse
deleteVolume p1 s =
    send $ (mkDeleteVolume p1) &~ s

deleteVolumeCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'dvVolumeARN'
    -> State DeleteVolume a
    -> m (Either ServiceEr DeleteVolumeResponse)
deleteVolumeCatch p1 s =
    sendCatch $ (mkDeleteVolume p1) &~ s

-- $DescribeBandwidthRateLimit
-- This operation returns the bandwidth rate limits of a gateway. By default,
-- these limits are not set, which means no bandwidth rate limiting is in
-- effect. This operation only returns a value for a bandwidth rate limit only
-- if the limit is set. If no limits are set for the gateway, then this
-- operation returns only the gateway ARN in the response body. To specify
-- which gateway to describe, use the Amazon Resource Name (ARN) of the
-- gateway in your request. Example Request The following example shows a
-- request that returns the bandwidth throttle properties of a gateway. POST /
-- HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com x-amz-Date:
-- 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DescribeBandwidthRateLimit { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygate way" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 169 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "AverageUploadRateLimitInBitsPerSec": 102400,
-- "AverageDownloadRateLimitInBitsPerSec": 51200 }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DescribeBandwidthRateLimit'

describeBandwidthRateLimit :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              )
    => Text -- ^ 'dbrl1GatewayARN'
    -> State DescribeBandwidthRateLimit a
    -> m DescribeBandwidthRateLimitResponse
describeBandwidthRateLimit p1 s =
    send $ (mkDescribeBandwidthRateLimit p1) &~ s

describeBandwidthRateLimitCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env m
                                   )
    => Text -- ^ 'dbrl1GatewayARN'
    -> State DescribeBandwidthRateLimit a
    -> m (Either ServiceEr DescribeBandwidthRateLimitResponse)
describeBandwidthRateLimitCatch p1 s =
    sendCatch $ (mkDescribeBandwidthRateLimit p1) &~ s

-- $DescribeCache
-- This operation returns information about the cache of a gateway. This
-- operation is supported only for the gateway-cached volume architecture. The
-- response includes disk IDs that are configured as cache, and it includes
-- the amount of cache allocated and used. Example Request The following
-- example shows a request to obtain a description of a gateway's working
-- storage. POST / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com
-- Content-Type: application/x-amz-json-1.1 Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20120425/us-east-1/storagegateway/aws4_request,
-- SignedHeaders=content-type;host;x-amz-date;x-amz-target,
-- Signature=9cd5a3584d1d67d57e61f120f35102d6b3649066abdd4bf4bbcf05bd9f2f8fe2
-- x-amz-date: 20120912T120000Z x-amz-target:
-- StorageGateway_20120630.DescribeCache {
-- "GatewayARN":"arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway"
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- gur28r2rqlgb8vvs0mq17hlgij1q8glle1qeu3kpgg6f0kstauu0 Date: Wed, 12 Sep 2012
-- 12:00:02 GMT Content-Type: application/x-amz-json-1.1 Content-length: 271 {
-- "CacheAllocationInBytes": 2199023255552, "CacheDirtyPercentage": 0.07,
-- "CacheHitPercentage": 99.68, "CacheMissPercentage": 0.32,
-- "CacheUsedPercentage": 0.07, "DiskIds": [ "pci-0000:03:00.0-scsi-0:0:0:0",
-- "pci-0000:04:00.0-scsi-0:1:0:0" ], "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DescribeCache'

describeCache :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => Text -- ^ 'dcGatewayARN'
    -> State DescribeCache a
    -> m DescribeCacheResponse
describeCache p1 s =
    send $ (mkDescribeCache p1) &~ s

describeCacheCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => Text -- ^ 'dcGatewayARN'
    -> State DescribeCache a
    -> m (Either ServiceEr DescribeCacheResponse)
describeCacheCatch p1 s =
    sendCatch $ (mkDescribeCache p1) &~ s

-- $DescribeCachediSCSIVolumes
-- This operation returns a description of the gateway volumes specified in
-- the request. This operation is supported only for the gateway-cached volume
-- architecture. The list of gateway volumes in the request must be from one
-- gateway. In the response Amazon Storage Gateway returns volume information
-- sorted by volume Amazon Resource Name (ARN). Example Request The following
-- example shows a request that returns a description of a volume. POST /
-- HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com Content-Type:
-- application/x-amz-json-1.1 Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20120425/us-east-1/storagegateway/aws4_request,
-- SignedHeaders=content-type;host;x-amz-date;x-amz-target,
-- Signature=9cd5a3584d1d67d57e61f120f35102d6b3649066abdd4bf4bbcf05bd9f2f8fe2
-- x-amz-date: 20120912T120000Z x-amz-target:
-- StorageGateway_20120630.DescribeCachediSCSIVolumes { "VolumeARNs":
-- ["arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB"]
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- gur28r2rqlgb8vvs0mq17hlgij1q8glle1qeu3kpgg6f0kstauu0 Date: Wed, 12 Sep 2012
-- 12:00:02 GMT Content-Type: application/x-amz-json-1.1 Content-length: 664 {
-- "CachediSCSIVolumes": [ { "VolumeiSCSIAttributes": { "ChapEnabled": true,
-- "LunNumber": 0, "NetworkInterfaceId": "10.243.43.207",
-- "NetworkInterfacePort": 3260, "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume"
-- }, "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB",
-- "VolumeDiskId": "pci-0000:03:00.0-scsi-0:0:0:0", "VolumeId":
-- "vol-1122AABB", "VolumeSizeInBytes": 1099511627776, "VolumeStatus":
-- "AVAILABLE", "VolumeType": "CACHED iSCSI" } ] }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DescribeCachediSCSIVolumes'

describeCachediSCSIVolumes :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              )
    => [Text] -- ^ 'dcscsivVolumeARNs'
    -> State DescribeCachediSCSIVolumes a
    -> m DescribeCachediSCSIVolumesResponse
describeCachediSCSIVolumes p1 s =
    send $ (mkDescribeCachediSCSIVolumes p1) &~ s

describeCachediSCSIVolumesCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env m
                                   )
    => [Text] -- ^ 'dcscsivVolumeARNs'
    -> State DescribeCachediSCSIVolumes a
    -> m (Either ServiceEr DescribeCachediSCSIVolumesResponse)
describeCachediSCSIVolumesCatch p1 s =
    sendCatch $ (mkDescribeCachediSCSIVolumes p1) &~ s

-- $DescribeChapCredentials
-- This operation returns an array of Challenge-Handshake Authentication
-- Protocol (CHAP) credentials information for a specified iSCSI target, one
-- for each target-initiator pair. Example Request The following example shows
-- a request that returns the CHAP credentials of an iSCSI target. POST /
-- HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com x-amz-Date:
-- 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DescribeChapCredentials { "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume"
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 235 {
-- "ChapCredentials": { "TargetName": "iqn.1997-05.com.amazon:myvolume",
-- "SecretToAuthenticateInitiator": "111111111111", "InitiatorName":
-- "iqn.1991-05.com.microsoft:computername.domain.example.com",
-- "SecretToAuthenticateTarget": "222222222222" } }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DescribeChapCredentials'

describeChapCredentials :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => Text -- ^ 'dcc1TargetARN'
    -> State DescribeChapCredentials a
    -> m DescribeChapCredentialsResponse
describeChapCredentials p1 s =
    send $ (mkDescribeChapCredentials p1) &~ s

describeChapCredentialsCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => Text -- ^ 'dcc1TargetARN'
    -> State DescribeChapCredentials a
    -> m (Either ServiceEr DescribeChapCredentialsResponse)
describeChapCredentialsCatch p1 s =
    sendCatch $ (mkDescribeChapCredentials p1) &~ s

-- $DescribeGatewayInformation
-- This operation returns metadata about a gateway such as its name, network
-- interfaces, configured time zone, and the state (whether the gateway is
-- running or not). To specify which gateway to describe, use the Amazon
-- Resource Name (ARN) of the gateway in your request. Example Request The
-- following example shows a request for describing a gateway. POST / HTTP/1.1
-- Host: storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DescribeGatewayInformation { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 227 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "GatewayId": "sgw-AABB1122", "GatewayNetworkInterfaces": [ {"Ipv4Address":
-- "10.35.69.216"} ], "GatewayState": "STATE_RUNNING", "GatewayTimezone":
-- "GMT-8:00" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DescribeGatewayInformation'

describeGatewayInformation :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              )
    => Text -- ^ 'dgiGatewayARN'
    -> State DescribeGatewayInformation a
    -> m DescribeGatewayInformationResponse
describeGatewayInformation p1 s =
    send $ (mkDescribeGatewayInformation p1) &~ s

describeGatewayInformationCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env m
                                   )
    => Text -- ^ 'dgiGatewayARN'
    -> State DescribeGatewayInformation a
    -> m (Either ServiceEr DescribeGatewayInformationResponse)
describeGatewayInformationCatch p1 s =
    sendCatch $ (mkDescribeGatewayInformation p1) &~ s

-- $DescribeMaintenanceStartTime
-- This operation returns your gateway's weekly maintenance start time
-- including the day and time of the week. Note that values are in terms of
-- the gateway's time zone. Example Request The following example shows a
-- request that describes a gateway's maintenance window. POST / HTTP/1.1
-- Host: storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DescribeMaintenanceStartTime { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 136 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "HourOfDay": 15, "MinuteOfHour": 35, "DayOfWeek": 2, "Timezone": "GMT+7:00"
-- }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DescribeMaintenanceStartTime'

describeMaintenanceStartTime :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env m
                                )
    => Text -- ^ 'dmstGatewayARN'
    -> State DescribeMaintenanceStartTime a
    -> m DescribeMaintenanceStartTimeResponse
describeMaintenanceStartTime p1 s =
    send $ (mkDescribeMaintenanceStartTime p1) &~ s

describeMaintenanceStartTimeCatch :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadReader Env m
                                     )
    => Text -- ^ 'dmstGatewayARN'
    -> State DescribeMaintenanceStartTime a
    -> m (Either ServiceEr DescribeMaintenanceStartTimeResponse)
describeMaintenanceStartTimeCatch p1 s =
    sendCatch $ (mkDescribeMaintenanceStartTime p1) &~ s

-- $DescribeSnapshotSchedule
-- This operation describes the snapshot schedule for the specified gateway
-- volume. The snapshot schedule information includes intervals at which
-- snapshots are automatically initiated on the volume. Example Request The
-- following example shows a request that retrieves the snapshot schedule for
-- a volume. POST / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com
-- x-amz-Date: 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DescribeSnapshotSchedule { "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB"
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 211 {
-- "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB",
-- "StartAt": 6, "RecurrenceInHours": 24, "Description":
-- "sgw-AABB1122:vol-AABB1122:Schedule", "Timezone": "GMT+7:00" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DescribeSnapshotSchedule'

describeSnapshotSchedule :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => Text -- ^ 'dss1VolumeARN'
    -> State DescribeSnapshotSchedule a
    -> m DescribeSnapshotScheduleResponse
describeSnapshotSchedule p1 s =
    send $ (mkDescribeSnapshotSchedule p1) &~ s

describeSnapshotScheduleCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'dss1VolumeARN'
    -> State DescribeSnapshotSchedule a
    -> m (Either ServiceEr DescribeSnapshotScheduleResponse)
describeSnapshotScheduleCatch p1 s =
    sendCatch $ (mkDescribeSnapshotSchedule p1) &~ s

-- $DescribeStorediSCSIVolumes
-- This operation returns description of the gateway volumes specified in the
-- request. The list of gateway volumes in the request must be from one
-- gateway. In the response Amazon Storage Gateway returns volume information
-- sorted by volume ARNs. Example Request The following example shows a
-- request that returns a description of a volume. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DescribeStorediSCSIVolumes { "VolumeARNs":
-- ["arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB"]
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 507 {
-- "StorediSCSIVolumes": [ { "VolumeiSCSIAttributes": { "ChapEnabled": true,
-- "NetworkInterfaceId": "10.243.43.207", "NetworkInterfacePort": 3260,
-- "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume"
-- }, "PreservedExistingData": false, "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/myg
-- ateway/volume/vol-1122AABB", "VolumeDiskId":
-- "pci-0000:03:00.0-scsi-0:0:0:0", "VolumeId": "vol-1122AABB",
-- "VolumeProgress": 23.7, "VolumeSizeInBytes": 1099511627776, "VolumeStatus":
-- "BOOTSTRAPPING" } ] }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DescribeStorediSCSIVolumes'

describeStorediSCSIVolumes :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              )
    => [Text] -- ^ 'dsscsivVolumeARNs'
    -> State DescribeStorediSCSIVolumes a
    -> m DescribeStorediSCSIVolumesResponse
describeStorediSCSIVolumes p1 s =
    send $ (mkDescribeStorediSCSIVolumes p1) &~ s

describeStorediSCSIVolumesCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env m
                                   )
    => [Text] -- ^ 'dsscsivVolumeARNs'
    -> State DescribeStorediSCSIVolumes a
    -> m (Either ServiceEr DescribeStorediSCSIVolumesResponse)
describeStorediSCSIVolumesCatch p1 s =
    sendCatch $ (mkDescribeStorediSCSIVolumes p1) &~ s

-- $DescribeTapeArchives
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DescribeTapeArchives'

describeTapeArchives :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env (ResumableSource m)
                        )
    => State DescribeTapeArchives a
    -> ResumableSource m DescribeTapeArchivesResponse
describeTapeArchives s =
    paginate (mkDescribeTapeArchives &~ s)

describeTapeArchivesCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env (ResumableSource m)
                             )
    => State DescribeTapeArchives a
    -> ResumableSource m (Either ServiceEr DescribeTapeArchivesResponse)
describeTapeArchivesCatch s =
    paginateCatch (mkDescribeTapeArchives &~ s)

-- $DescribeTapeRecoveryPoints
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DescribeTapeRecoveryPoints'

describeTapeRecoveryPoints :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env (ResumableSource m)
                              )
    => Text -- ^ 'dtrpGatewayARN'
    -> State DescribeTapeRecoveryPoints a
    -> ResumableSource m DescribeTapeRecoveryPointsResponse
describeTapeRecoveryPoints p1 s =
    paginate $ (mkDescribeTapeRecoveryPoints p1) &~ s

describeTapeRecoveryPointsCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env (ResumableSource m)
                                   )
    => Text -- ^ 'dtrpGatewayARN'
    -> State DescribeTapeRecoveryPoints a
    -> ResumableSource m (Either ServiceEr DescribeTapeRecoveryPointsResponse)
describeTapeRecoveryPointsCatch p1 s =
    paginateCatch $ (mkDescribeTapeRecoveryPoints p1) &~ s

-- $DescribeTapes
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DescribeTapes'

describeTapes :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env (ResumableSource m)
                 )
    => Text -- ^ 'dt1GatewayARN'
    -> State DescribeTapes a
    -> ResumableSource m DescribeTapesResponse
describeTapes p1 s =
    paginate $ (mkDescribeTapes p1) &~ s

describeTapesCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env (ResumableSource m)
                      )
    => Text -- ^ 'dt1GatewayARN'
    -> State DescribeTapes a
    -> ResumableSource m (Either ServiceEr DescribeTapesResponse)
describeTapesCatch p1 s =
    paginateCatch $ (mkDescribeTapes p1) &~ s

-- $DescribeUploadBuffer
-- This operation returns information about the upload buffer of a gateway.
-- This operation is supported for both the gateway-stored and gateway-cached
-- volume architectures. The response includes disk IDs that are configured as
-- upload buffer space, and it includes the amount of upload buffer space
-- allocated and used. Example Request The following example shows a request
-- to obtain a description of a gateway's working storage. POST / HTTP/1.1
-- Host: storagegateway.us-east-1.amazonaws.com Content-Type:
-- application/x-amz-json-1.1 Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20120425/us-east-1/storagegateway/aws4_request,
-- SignedHeaders=content-type;host;x-amz-date;x-amz-target,
-- Signature=9cd5a3584d1d67d57e61f120f35102d6b3649066abdd4bf4bbcf05bd9f2f8fe2
-- x-amz-date: 20120912T120000Z x-amz-target:
-- StorageGateway_20120630.DescribeUploadBuffer {
-- "GatewayARN":"arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway"
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- gur28r2rqlgb8vvs0mq17hlgij1q8glle1qeu3kpgg6f0kstauu0 Date: Wed, 12 Sep 2012
-- 12:00:02 GMT Content-Type: application/x-amz-json-1.1 Content-length: 271 {
-- "DiskIds": [ "pci-0000:03:00.0-scsi-0:0:0:0",
-- "pci-0000:04:00.0-scsi-0:1:0:0" ], "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "UploadBufferAllocatedInBytes": 161061273600, "UploadBufferUsedInBytes": 0
-- }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DescribeUploadBuffer'

describeUploadBuffer :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => Text -- ^ 'dubGatewayARN'
    -> State DescribeUploadBuffer a
    -> m DescribeUploadBufferResponse
describeUploadBuffer p1 s =
    send $ (mkDescribeUploadBuffer p1) &~ s

describeUploadBufferCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => Text -- ^ 'dubGatewayARN'
    -> State DescribeUploadBuffer a
    -> m (Either ServiceEr DescribeUploadBufferResponse)
describeUploadBufferCatch p1 s =
    sendCatch $ (mkDescribeUploadBuffer p1) &~ s

-- $DescribeVTLDevices
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DescribeVTLDevices'

describeVTLDevices :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env (ResumableSource m)
                      )
    => Text -- ^ 'dvtldGatewayARN'
    -> State DescribeVTLDevices a
    -> ResumableSource m DescribeVTLDevicesResponse
describeVTLDevices p1 s =
    paginate $ (mkDescribeVTLDevices p1) &~ s

describeVTLDevicesCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env (ResumableSource m)
                           )
    => Text -- ^ 'dvtldGatewayARN'
    -> State DescribeVTLDevices a
    -> ResumableSource m (Either ServiceEr DescribeVTLDevicesResponse)
describeVTLDevicesCatch p1 s =
    paginateCatch $ (mkDescribeVTLDevices p1) &~ s

-- $DescribeWorkingStorage
-- This operation returns information about the working storage of a gateway.
-- This operation is supported only for the gateway-stored volume
-- architecture. Working storage is also referred to as upload buffer. You can
-- also use the DescribeUploadBuffer operation to add upload buffer to a
-- stored-volume gateway. The response includes disk IDs that are configured
-- as working storage, and it includes the amount of working storage allocated
-- and used. Example Request The following example shows a request to obtain a
-- description of a gateway's working storage. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DescribeWorkingStorage {
-- "GatewayARN":"arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway"
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 241 {
-- "DiskIds": ["pci-0000:03:00.0-scsi-0:0:0:0",
-- "pci-0000:03:00.0-scsi-0:0:1:0"], "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "WorkingStorageAllocatedInBytes": 2199023255552,
-- "WorkingStorageUsedInBytes": 789207040 }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DescribeWorkingStorage'

describeWorkingStorage :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'dwsGatewayARN'
    -> State DescribeWorkingStorage a
    -> m DescribeWorkingStorageResponse
describeWorkingStorage p1 s =
    send $ (mkDescribeWorkingStorage p1) &~ s

describeWorkingStorageCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'dwsGatewayARN'
    -> State DescribeWorkingStorage a
    -> m (Either ServiceEr DescribeWorkingStorageResponse)
describeWorkingStorageCatch p1 s =
    sendCatch $ (mkDescribeWorkingStorage p1) &~ s

-- $DisableGateway
-- See: 'Network.AWS.StorageGateway.V2013_06_30.DisableGateway'

disableGateway :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'dg1GatewayARN'
    -> State DisableGateway a
    -> m DisableGatewayResponse
disableGateway p1 s =
    send $ (mkDisableGateway p1) &~ s

disableGatewayCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'dg1GatewayARN'
    -> State DisableGateway a
    -> m (Either ServiceEr DisableGatewayResponse)
disableGatewayCatch p1 s =
    sendCatch $ (mkDisableGateway p1) &~ s

-- $ListGateways
-- This operation lists gateways owned by an AWS account in a region specified
-- in the request. The returned list is ordered by gateway Amazon Resource
-- Name (ARN). By default, the operation returns a maximum of 100 gateways.
-- This operation supports pagination that allows you to optionally reduce the
-- number of gateways returned in a response. If you have more gateways than
-- are returned in a response-that is, the response returns only a truncated
-- list of your gateways-the response contains a marker that you can specify
-- in your next request to fetch the next page of gateways. List Gateways The
-- following example does not specify any criteria for the returned list. Note
-- that the request body is "{}". The response returns gateways (or up to the
-- first 100) in the specified region owned by the AWS account. POST /
-- HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com x-amz-Date:
-- 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.ListGateways HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 178 {
-- "GatewayList": [ { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway2" } ] }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.ListGateways'

listGateways :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env (ResumableSource m)
                )
    => State ListGateways a
    -> ResumableSource m ListGatewaysResponse
listGateways s =
    paginate (mkListGateways &~ s)

listGatewaysCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env (ResumableSource m)
                     )
    => State ListGateways a
    -> ResumableSource m (Either ServiceEr ListGatewaysResponse)
listGatewaysCatch s =
    paginateCatch (mkListGateways &~ s)

-- $ListLocalDisks
-- This operation returns a list of the local disks of a gateway. To specify
-- which gateway to describe you use the Amazon Resource Name (ARN) of the
-- gateway in the body of the request. The request returns all disks,
-- specifying which are configured as working storage, stored volume or not
-- configured at all. Example Request The following example shows a request
-- that returns information about a gateway's local disks. POST / HTTP/1.1
-- Host: storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.ListLocalDisks { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 398 {
-- "Disks": [ { "DiskAllocationType": "UPLOAD_BUFFER", "DiskId":
-- "pci-0000:03:00.0-scsi-0:0:0:0", "DiskNode": "SCSI(0:0)", "DiskPath":
-- "/dev/sda", "DiskSizeInBytes": 1099511627776 }, { "DiskAllocationType":
-- "STORED_iSCSI_VOLUME", "DiskAllocationResource": "", "DiskId":
-- "pci-0000:03:00.0-scsi-0:0:1:0", "DiskNode": "SCSI(0:1)", "DiskPath":
-- "/dev/sdb", "DiskSizeInBytes": 1099511627776 } ], "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.ListLocalDisks'

listLocalDisks :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'lldGatewayARN'
    -> State ListLocalDisks a
    -> m ListLocalDisksResponse
listLocalDisks p1 s =
    send $ (mkListLocalDisks p1) &~ s

listLocalDisksCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'lldGatewayARN'
    -> State ListLocalDisks a
    -> m (Either ServiceEr ListLocalDisksResponse)
listLocalDisksCatch p1 s =
    sendCatch $ (mkListLocalDisks p1) &~ s

-- $ListVolumeRecoveryPoints
-- This operation lists the recovery points for a specified gateway. This
-- operation is supported only for the gateway-cached volume architecture.
-- Each gateway-cached volume has one recovery point. A volume recovery point
-- is a point in time at which all data of the volume is consistent and from
-- which you can create a snapshot. To create a snapshot from a volume
-- recovery point use the CreateSnapshotFromVolumeRecoveryPoint operation.
-- Example Request The following example sends a ListVolumeRecoveryPoints
-- request to take a snapshot of the specified example volume. POST / HTTP/1.1
-- Host: storagegateway.us-east-1.amazonaws.com Content-Type:
-- application/x-amz-json-1.1 Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20120425/us-east-1/storagegateway/aws4_request,
-- SignedHeaders=content-type;host;x-amz-date;x-amz-target,
-- Signature=9cd5a3584d1d67d57e61f120f35102d6b3649066abdd4bf4bbcf05bd9f2f8fe2
-- x-amz-date: 20120912T120000Z x-amz-target:
-- StorageGateway_20120630.ListVolumeRecoveryPoints { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- gur28r2rqlgb8vvs0mq17hlgij1q8glle1qeu3kpgg6f0kstauu0 Date: Wed, 12 Sep 2012
-- 12:00:02 GMT Content-Type: application/x-amz-json-1.1 Content-length: 137 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "VolumeRecoveryPointInfos": [ { "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB",
-- "VolumeRecoveryPointTime": "2012-09-04T21:08:44.627Z", "VolumeSizeInBytes":
-- 536870912000, "VolumeUsageInBytes": 6694048 } ] }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.ListVolumeRecoveryPoints'

listVolumeRecoveryPoints :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => Text -- ^ 'lvrpGatewayARN'
    -> State ListVolumeRecoveryPoints a
    -> m ListVolumeRecoveryPointsResponse
listVolumeRecoveryPoints p1 s =
    send $ (mkListVolumeRecoveryPoints p1) &~ s

listVolumeRecoveryPointsCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'lvrpGatewayARN'
    -> State ListVolumeRecoveryPoints a
    -> m (Either ServiceEr ListVolumeRecoveryPointsResponse)
listVolumeRecoveryPointsCatch p1 s =
    sendCatch $ (mkListVolumeRecoveryPoints p1) &~ s

-- $ListVolumes
-- This operation lists the iSCSI stored volumes of a gateway. Results are
-- sorted by volume ARN. The response includes only the volume ARNs. If you
-- want additional volume information, use the DescribeStorediSCSIVolumes API.
-- The operation supports pagination. By default, the operation returns a
-- maximum of up to 100 volumes. You can optionally specify the Limit field in
-- the body to limit the number of volumes in the response. If the number of
-- volumes returned in the response is truncated, the response includes a
-- Marker field. You can use this Marker value in your subsequent request to
-- retrieve the next set of volumes. Example Request The List iSCSI Volumes
-- request in this example does not specify a limit or marker field in the
-- response body. The response returns the volumes (up to the first 100) of
-- the gateway. POST / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com
-- x-amz-Date: 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.ListVolumes { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 346 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "VolumeInfos": [ { "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB",
-- "VolumeType": "STORED" }, { "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-3344CCDD",
-- "VolumeType": "STORED" }, ] }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.ListVolumes'

listVolumes :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env (ResumableSource m)
               )
    => Text -- ^ 'lvGatewayARN'
    -> State ListVolumes a
    -> ResumableSource m ListVolumesResponse
listVolumes p1 s =
    paginate $ (mkListVolumes p1) &~ s

listVolumesCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env (ResumableSource m)
                    )
    => Text -- ^ 'lvGatewayARN'
    -> State ListVolumes a
    -> ResumableSource m (Either ServiceEr ListVolumesResponse)
listVolumesCatch p1 s =
    paginateCatch $ (mkListVolumes p1) &~ s

-- $RetrieveTapeArchive
-- See: 'Network.AWS.StorageGateway.V2013_06_30.RetrieveTapeArchive'

retrieveTapeArchive :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => Text -- ^ 'rtaTapeARN'
    -> Text -- ^ 'rtaGatewayARN'
    -> State RetrieveTapeArchive a
    -> m RetrieveTapeArchiveResponse
retrieveTapeArchive p1 p2 s =
    send $ (mkRetrieveTapeArchive p1 p2) &~ s

retrieveTapeArchiveCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => Text -- ^ 'rtaTapeARN'
    -> Text -- ^ 'rtaGatewayARN'
    -> State RetrieveTapeArchive a
    -> m (Either ServiceEr RetrieveTapeArchiveResponse)
retrieveTapeArchiveCatch p1 p2 s =
    sendCatch $ (mkRetrieveTapeArchive p1 p2) &~ s

-- $RetrieveTapeRecoveryPoint
-- See: 'Network.AWS.StorageGateway.V2013_06_30.RetrieveTapeRecoveryPoint'

retrieveTapeRecoveryPoint :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => Text -- ^ 'rtrpTapeARN'
    -> Text -- ^ 'rtrpGatewayARN'
    -> State RetrieveTapeRecoveryPoint a
    -> m RetrieveTapeRecoveryPointResponse
retrieveTapeRecoveryPoint p1 p2 s =
    send $ (mkRetrieveTapeRecoveryPoint p1 p2) &~ s

retrieveTapeRecoveryPointCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'rtrpTapeARN'
    -> Text -- ^ 'rtrpGatewayARN'
    -> State RetrieveTapeRecoveryPoint a
    -> m (Either ServiceEr RetrieveTapeRecoveryPointResponse)
retrieveTapeRecoveryPointCatch p1 p2 s =
    sendCatch $ (mkRetrieveTapeRecoveryPoint p1 p2) &~ s

-- $ShutdownGateway
-- This operation shuts down a gateway. To specify which gateway to shut down,
-- use the Amazon Resource Name (ARN) of the gateway in the body of your
-- request. The operation shuts down the gateway service component running in
-- the storage gateway's virtual machine (VM) and not the VM. If you want to
-- shut down the VM, it is recommended that you first shut down the gateway
-- component in the VM to avoid unpredictable conditions. After the gateway is
-- shutdown, you cannot call any other API except StartGateway,
-- DescribeGatewayInformation, and ListGateways. For more information, see
-- ActivateGateway. Your applications cannot read from or write to the
-- gateway's storage volumes, and there are no snapshots taken. When you make
-- a shutdown request, you will get a 200 OK success response immediately.
-- However, it might take some time for the gateway to shut down. You can call
-- the DescribeGatewayInformation API to check the status. For more
-- information, see ActivateGateway. If do not intend to use the gateway
-- again, you must delete the gateway (using DeleteGateway) to no longer pay
-- software charges associated with the gateway. Example Request The following
-- example shows a request that shuts down a gateway. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.ShutdownGateway { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 80 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.ShutdownGateway'

shutdownGateway :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'sgGatewayARN'
    -> State ShutdownGateway a
    -> m ShutdownGatewayResponse
shutdownGateway p1 s =
    send $ (mkShutdownGateway p1) &~ s

shutdownGatewayCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'sgGatewayARN'
    -> State ShutdownGateway a
    -> m (Either ServiceEr ShutdownGatewayResponse)
shutdownGatewayCatch p1 s =
    sendCatch $ (mkShutdownGateway p1) &~ s

-- $StartGateway
-- This operation starts a gateway that you previously shut down (see
-- ShutdownGateway). After the gateway starts, you can then make other API
-- calls, your applications can read from or write to the gateway's storage
-- volumes and you will be able to take snapshot backups. When you make a
-- request, you will get a 200 OK success response immediately. However, it
-- might take some time for the gateway to be ready. You should call
-- DescribeGatewayInformation and check the status before making any
-- additional API calls. For more information, see ActivateGateway. To specify
-- which gateway to start, use the Amazon Resource Name (ARN) of the gateway
-- in your request. Example Request The following example shows a request that
-- starts a gateway. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.StartGateway { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 80 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.StartGateway'

startGateway :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'sg1GatewayARN'
    -> State StartGateway a
    -> m StartGatewayResponse
startGateway p1 s =
    send $ (mkStartGateway p1) &~ s

startGatewayCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'sg1GatewayARN'
    -> State StartGateway a
    -> m (Either ServiceEr StartGatewayResponse)
startGatewayCatch p1 s =
    sendCatch $ (mkStartGateway p1) &~ s

-- $UpdateBandwidthRateLimit
-- This operation updates the bandwidth rate limits of a gateway. You can
-- update both the upload and download bandwidth rate limit or specify only
-- one of the two. If you don't set a bandwidth rate limit, the existing rate
-- limit remains. By default, a gateway's bandwidth rate limits are not set.
-- If you don't set any limit, the gateway does not have any limitations on
-- its bandwidth usage and could potentially use the maximum available
-- bandwidth. To specify which gateway to update, use the Amazon Resource Name
-- (ARN) of the gateway in your request. Example Request The following example
-- shows a request that returns the bandwidth throttle properties of a
-- gateway. POST / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com
-- x-amz-Date: 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.UpdateBandwidthRateLimit { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "AverageUploadRateLimitInBitsPerSec": 51200,
-- "AverageDownloadRateLimitInBitsPerSec": 102400 } HTTP/1.1 200 OK
-- x-amzn-RequestId: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Date: Wed, 25 Apr 2012 12:00:02 GMT Content-type:
-- application/x-amz-json-1.1 Content-length: 80 { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.UpdateBandwidthRateLimit'

updateBandwidthRateLimit :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => Text -- ^ 'ubrlGatewayARN'
    -> State UpdateBandwidthRateLimit a
    -> m UpdateBandwidthRateLimitResponse
updateBandwidthRateLimit p1 s =
    send $ (mkUpdateBandwidthRateLimit p1) &~ s

updateBandwidthRateLimitCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'ubrlGatewayARN'
    -> State UpdateBandwidthRateLimit a
    -> m (Either ServiceEr UpdateBandwidthRateLimitResponse)
updateBandwidthRateLimitCatch p1 s =
    sendCatch $ (mkUpdateBandwidthRateLimit p1) &~ s

-- $UpdateChapCredentials
-- This operation updates the Challenge-Handshake Authentication Protocol
-- (CHAP) credentials for a specified iSCSI target. By default, a gateway does
-- not have CHAP enabled; however, for added security, you might use it. When
-- you update CHAP credentials, all existing connections on the target are
-- closed and initiators must reconnect with the new credentials. Example
-- Request The following example shows a request that updates CHAP credentials
-- for an iSCSI target. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.UpdateChapCredentials { "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume",
-- "SecretToAuthenticateInitiator": "111111111111", "InitiatorName":
-- "iqn.1991-05.com.microsoft:computername.domain.example.com",
-- "SecretToAuthenticateTarget": "222222222222" } HTTP/1.1 200 OK
-- x-amzn-RequestId: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Date: Wed, 25 Apr 2012 12:00:02 GMT Content-type:
-- application/x-amz-json-1.1 Content-length: 161 { "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume",
-- "InitiatorName":
-- "iqn.1991-05.com.microsoft:computername.domain.example.com" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.UpdateChapCredentials'

updateChapCredentials :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'uccTargetARN'
    -> Text -- ^ 'uccSecretToAuthenticateInitiator'
    -> Text -- ^ 'uccInitiatorName'
    -> State UpdateChapCredentials a
    -> m UpdateChapCredentialsResponse
updateChapCredentials p1 p2 p3 s =
    send $ (mkUpdateChapCredentials p1 p2 p3) &~ s

updateChapCredentialsCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'uccTargetARN'
    -> Text -- ^ 'uccSecretToAuthenticateInitiator'
    -> Text -- ^ 'uccInitiatorName'
    -> State UpdateChapCredentials a
    -> m (Either ServiceEr UpdateChapCredentialsResponse)
updateChapCredentialsCatch p1 p2 p3 s =
    sendCatch $ (mkUpdateChapCredentials p1 p2 p3) &~ s

-- $UpdateGatewayInformation
-- This operation updates a gateway's metadata, which includes the gateway's
-- name and time zone. To specify which gateway to update, use the Amazon
-- Resource Name (ARN) of the gateway in your request. Example Request The
-- following example shows a request that updates the name of a gateway. POST
-- / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com x-amz-Date:
-- 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.UpdateGatewayInformation { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "GatewayName" "mygateway2" } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 81 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway2" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.UpdateGatewayInformation'

updateGatewayInformation :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => Text -- ^ 'ugiGatewayARN'
    -> State UpdateGatewayInformation a
    -> m UpdateGatewayInformationResponse
updateGatewayInformation p1 s =
    send $ (mkUpdateGatewayInformation p1) &~ s

updateGatewayInformationCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'ugiGatewayARN'
    -> State UpdateGatewayInformation a
    -> m (Either ServiceEr UpdateGatewayInformationResponse)
updateGatewayInformationCatch p1 s =
    sendCatch $ (mkUpdateGatewayInformation p1) &~ s

-- $UpdateGatewaySoftwareNow
-- This operation updates the gateway virtual machine (VM) software. The
-- request immediately triggers the software update. Before sending this
-- request, you should make sure all your applications have finished writing
-- to your gateway's storage volumes in order to avoid data loss. During the
-- update, applications cannot use the gateway's storage volumes. --> When you
-- make this request, you get a 200 OK success response immediately. However,
-- it might take some time for the update to complete. You can call
-- DescribeGatewayInformation to verify the gateway is in the STATE_RUNNING
-- state. A software update forces a system restart of your gateway. You can
-- minimize the chance of any disruption to your applications by increasing
-- your iSCSI Initiators' timeouts. For more information about increasing
-- iSCSI Initiator timeouts for Windows and Linux, see Customizing Your
-- Windows iSCSI Settings and Customizing Your Linux iSCSI Settings,
-- respectively. Example Request The following example shows a request that
-- initiates a gateway VM update. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.UpdateGatewaySoftwareNow { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 80 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.UpdateGatewaySoftwareNow'

updateGatewaySoftwareNow :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => Text -- ^ 'ugsnGatewayARN'
    -> State UpdateGatewaySoftwareNow a
    -> m UpdateGatewaySoftwareNowResponse
updateGatewaySoftwareNow p1 s =
    send $ (mkUpdateGatewaySoftwareNow p1) &~ s

updateGatewaySoftwareNowCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'ugsnGatewayARN'
    -> State UpdateGatewaySoftwareNow a
    -> m (Either ServiceEr UpdateGatewaySoftwareNowResponse)
updateGatewaySoftwareNowCatch p1 s =
    sendCatch $ (mkUpdateGatewaySoftwareNow p1) &~ s

-- $UpdateMaintenanceStartTime
-- This operation updates a gateway's weekly maintenance start time
-- information, including day and time of the week. The maintenance time is
-- the time in your gateway's time zone. Example Request The following example
-- shows a request that updates the maintenance start time of mygateway. POST
-- / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com x-amz-Date:
-- 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.UpdateMaintenanceStartTime { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "HourOfDay": 0, "MinuteOfHour": 30, "DayOfWeek": 2 } HTTP/1.1 200 OK
-- x-amzn-RequestId: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Date: Wed, 25 Apr 2012 12:00:02 GMT Content-type:
-- application/x-amz-json-1.1 Content-length: 80 { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.UpdateMaintenanceStartTime'

updateMaintenanceStartTime :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              )
    => Text -- ^ 'umstGatewayARN'
    -> Integer -- ^ 'umstHourOfDay'
    -> Integer -- ^ 'umstMinuteOfHour'
    -> Integer -- ^ 'umstDayOfWeek'
    -> State UpdateMaintenanceStartTime a
    -> m UpdateMaintenanceStartTimeResponse
updateMaintenanceStartTime p1 p2 p3 p4 s =
    send $ (mkUpdateMaintenanceStartTime p1 p2 p3 p4) &~ s

updateMaintenanceStartTimeCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env m
                                   )
    => Text -- ^ 'umstGatewayARN'
    -> Integer -- ^ 'umstHourOfDay'
    -> Integer -- ^ 'umstMinuteOfHour'
    -> Integer -- ^ 'umstDayOfWeek'
    -> State UpdateMaintenanceStartTime a
    -> m (Either ServiceEr UpdateMaintenanceStartTimeResponse)
updateMaintenanceStartTimeCatch p1 p2 p3 p4 s =
    sendCatch $ (mkUpdateMaintenanceStartTime p1 p2 p3 p4) &~ s

-- $UpdateSnapshotSchedule
-- This operation updates a snapshot schedule configured for a gateway volume.
-- The default snapshot schedule for volume is once every 24 hours, starting
-- at the creation time of the volume. You can use this API to change the
-- shapshot schedule configured for the volume. In the request you must
-- identify the gateway volume whose snapshot schedule you want to update, and
-- the schedule information, including when you want the snapshot to begin on
-- a day and the frequency (in hours) of snapshots. Example Request The
-- following example shows a request that updates a snapshot schedule. POST /
-- HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com x-amz-Date:
-- 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.UpdateSnapshotSchedule { "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB",
-- "StartAt": 0, "RecurrenceInHours": 1, "Description": "hourly snapshot" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 99 {
-- "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB"
-- }.
--
-- See: 'Network.AWS.StorageGateway.V2013_06_30.UpdateSnapshotSchedule'

updateSnapshotSchedule :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'ussVolumeARN'
    -> Integer -- ^ 'ussStartAt'
    -> Integer -- ^ 'ussRecurrenceInHours'
    -> State UpdateSnapshotSchedule a
    -> m UpdateSnapshotScheduleResponse
updateSnapshotSchedule p1 p2 p3 s =
    send $ (mkUpdateSnapshotSchedule p1 p2 p3) &~ s

updateSnapshotScheduleCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'ussVolumeARN'
    -> Integer -- ^ 'ussStartAt'
    -> Integer -- ^ 'ussRecurrenceInHours'
    -> State UpdateSnapshotSchedule a
    -> m (Either ServiceEr UpdateSnapshotScheduleResponse)
updateSnapshotScheduleCatch p1 p2 p3 s =
    sendCatch $ (mkUpdateSnapshotSchedule p1 p2 p3) &~ s
