{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.CreateStorediSCSIVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation creates a volume on a specified gateway. This operation is
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
module Network.AWS.StorageGateway.V2013_06_30.CreateStorediSCSIVolume
    (
    -- * Request
      CreateStorediSCSIVolume
    -- ** Request constructor
    , mkCreateStorediSCSIVolume
    -- ** Request lenses
    , csscsivGatewayARN
    , csscsivDiskId
    , csscsivSnapshotId
    , csscsivPreserveExistingData
    , csscsivTargetName
    , csscsivNetworkInterfaceId

    -- * Response
    , CreateStorediSCSIVolumeResponse
    -- ** Response constructor
    , mkCreateStorediSCSIVolumeResponse
    -- ** Response lenses
    , csscsivrVolumeARN
    , csscsivrVolumeSizeInBytes
    , csscsivrTargetARN
    ) where

import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | A JSON object containing one or more of the following fields:
-- CreateStorediSCSIVolumeInput$DiskId
-- CreateStorediSCSIVolumeInput$NetworkInterfaceId
-- CreateStorediSCSIVolumeInput$PreserveExistingData
-- CreateStorediSCSIVolumeInput$SnapshotId
-- CreateStorediSCSIVolumeInput$TargetName.
data CreateStorediSCSIVolume = CreateStorediSCSIVolume
    { _csscsivGatewayARN :: Text
    , _csscsivDiskId :: Text
    , _csscsivSnapshotId :: Maybe Text
    , _csscsivPreserveExistingData :: Bool
    , _csscsivTargetName :: Text
    , _csscsivNetworkInterfaceId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateStorediSCSIVolume' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Text@
--
-- * @DiskId ::@ @Text@
--
-- * @SnapshotId ::@ @Maybe Text@
--
-- * @PreserveExistingData ::@ @Bool@
--
-- * @TargetName ::@ @Text@
--
-- * @NetworkInterfaceId ::@ @Text@
--
mkCreateStorediSCSIVolume :: Text -- ^ 'csscsivGatewayARN'
                          -> Text -- ^ 'csscsivDiskId'
                          -> Bool -- ^ 'csscsivPreserveExistingData'
                          -> Text -- ^ 'csscsivTargetName'
                          -> Text -- ^ 'csscsivNetworkInterfaceId'
                          -> CreateStorediSCSIVolume
mkCreateStorediSCSIVolume p1 p2 p4 p5 p6 = CreateStorediSCSIVolume
    { _csscsivGatewayARN = p1
    , _csscsivDiskId = p2
    , _csscsivSnapshotId = Nothing
    , _csscsivPreserveExistingData = p4
    , _csscsivTargetName = p5
    , _csscsivNetworkInterfaceId = p6
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
csscsivGatewayARN :: Lens' CreateStorediSCSIVolume Text
csscsivGatewayARN =
    lens _csscsivGatewayARN (\s a -> s { _csscsivGatewayARN = a })

-- | The unique identifier for the gateway local disk that is configured as a
-- stored volume. Use ListLocalDisks to list disk IDs for a gateway.
csscsivDiskId :: Lens' CreateStorediSCSIVolume Text
csscsivDiskId = lens _csscsivDiskId (\s a -> s { _csscsivDiskId = a })

-- | The snapshot ID (e.g. "snap-1122aabb") of the snapshot to restore as the
-- new stored volume. Specify this field if you want to create the iSCSI
-- storage volume from a snapshot otherwise do not include this field. To list
-- snapshots for your account use DescribeSnapshots in the Amazon Elastic
-- Compute Cloud API Reference.
csscsivSnapshotId :: Lens' CreateStorediSCSIVolume (Maybe Text)
csscsivSnapshotId =
    lens _csscsivSnapshotId (\s a -> s { _csscsivSnapshotId = a })

-- | Specify this field as true if you want to preserve the data on the local
-- disk. Otherwise, specifying this field as false creates an empty volume.
-- Valid Values: true, false.
csscsivPreserveExistingData :: Lens' CreateStorediSCSIVolume Bool
csscsivPreserveExistingData =
    lens _csscsivPreserveExistingData
         (\s a -> s { _csscsivPreserveExistingData = a })

-- | The name of the iSCSI target used by initiators to connect to the target
-- and as a suffix for the target ARN. For example, specifying TargetName as
-- myvolume results in the target ARN of
-- arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume.
-- The target name must be unique across all volumes of a gateway.
csscsivTargetName :: Lens' CreateStorediSCSIVolume Text
csscsivTargetName =
    lens _csscsivTargetName (\s a -> s { _csscsivTargetName = a })

-- | The network interface of the gateway on which to expose the iSCSI target.
-- Only IPv4 addresses are accepted. Use DescribeGatewayInformation to get a
-- list of the network interfaces available on a gateway. Valid Values: A
-- valid IP address.
csscsivNetworkInterfaceId :: Lens' CreateStorediSCSIVolume Text
csscsivNetworkInterfaceId =
    lens _csscsivNetworkInterfaceId
         (\s a -> s { _csscsivNetworkInterfaceId = a })

instance ToPath CreateStorediSCSIVolume

instance ToQuery CreateStorediSCSIVolume

instance ToHeaders CreateStorediSCSIVolume

instance ToJSON CreateStorediSCSIVolume

-- | A JSON object containing the following fields:.
data CreateStorediSCSIVolumeResponse = CreateStorediSCSIVolumeResponse
    { _csscsivrVolumeARN :: Maybe Text
    , _csscsivrVolumeSizeInBytes :: Maybe Integer
    , _csscsivrTargetARN :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateStorediSCSIVolumeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeARN ::@ @Maybe Text@
--
-- * @VolumeSizeInBytes ::@ @Maybe Integer@
--
-- * @TargetARN ::@ @Maybe Text@
--
mkCreateStorediSCSIVolumeResponse :: CreateStorediSCSIVolumeResponse
mkCreateStorediSCSIVolumeResponse = CreateStorediSCSIVolumeResponse
    { _csscsivrVolumeARN = Nothing
    , _csscsivrVolumeSizeInBytes = Nothing
    , _csscsivrTargetARN = Nothing
    }

-- | The Amazon Resource Name (ARN) of the configured volume.
csscsivrVolumeARN :: Lens' CreateStorediSCSIVolumeResponse (Maybe Text)
csscsivrVolumeARN =
    lens _csscsivrVolumeARN (\s a -> s { _csscsivrVolumeARN = a })

-- | The size of the volume in bytes.
csscsivrVolumeSizeInBytes :: Lens' CreateStorediSCSIVolumeResponse (Maybe Integer)
csscsivrVolumeSizeInBytes =
    lens _csscsivrVolumeSizeInBytes
         (\s a -> s { _csscsivrVolumeSizeInBytes = a })

-- | he Amazon Resource Name (ARN) of the volume target that includes the iSCSI
-- name that initiators can use to connect to the target.
csscsivrTargetARN :: Lens' CreateStorediSCSIVolumeResponse (Maybe Text)
csscsivrTargetARN =
    lens _csscsivrTargetARN (\s a -> s { _csscsivrTargetARN = a })

instance FromJSON CreateStorediSCSIVolumeResponse

instance AWSRequest CreateStorediSCSIVolume where
    type Sv CreateStorediSCSIVolume = StorageGateway
    type Rs CreateStorediSCSIVolume = CreateStorediSCSIVolumeResponse

    request = get
    response _ = jsonResponse
