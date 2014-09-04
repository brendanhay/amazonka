{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.CreateCachediSCSIVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation creates a cached volume on a specified cached gateway. This
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
module Network.AWS.StorageGateway.V2013_06_30.CreateCachediSCSIVolume
    (
    -- * Request
      CreateCachediSCSIVolume
    -- ** Request constructor
    , mkCreateCachediSCSIVolumeInput
    -- ** Request lenses
    , ccscsiviGatewayARN
    , ccscsiviVolumeSizeInBytes
    , ccscsiviSnapshotId
    , ccscsiviTargetName
    , ccscsiviNetworkInterfaceId
    , ccscsiviClientToken

    -- * Response
    , CreateCachediSCSIVolumeResponse
    -- ** Response lenses
    , ccscsivoVolumeARN
    , ccscsivoTargetARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateCachediSCSIVolume' request.
mkCreateCachediSCSIVolumeInput :: Text -- ^ 'ccscsiviGatewayARN'
                               -> Integer -- ^ 'ccscsiviVolumeSizeInBytes'
                               -> Text -- ^ 'ccscsiviTargetName'
                               -> Text -- ^ 'ccscsiviNetworkInterfaceId'
                               -> Text -- ^ 'ccscsiviClientToken'
                               -> CreateCachediSCSIVolume
mkCreateCachediSCSIVolumeInput p1 p2 p3 p4 p5 = CreateCachediSCSIVolume
    { _ccscsiviGatewayARN = p1
    , _ccscsiviVolumeSizeInBytes = p2
    , _ccscsiviSnapshotId = Nothing
    , _ccscsiviTargetName = p4
    , _ccscsiviNetworkInterfaceId = p5
    , _ccscsiviClientToken = p6
    }
{-# INLINE mkCreateCachediSCSIVolumeInput #-}

data CreateCachediSCSIVolume = CreateCachediSCSIVolume
    { _ccscsiviGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _ccscsiviVolumeSizeInBytes :: Integer
    , _ccscsiviSnapshotId :: Maybe Text
    , _ccscsiviTargetName :: Text
    , _ccscsiviNetworkInterfaceId :: Text
    , _ccscsiviClientToken :: Text
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
ccscsiviGatewayARN :: Lens' CreateCachediSCSIVolume (Text)
ccscsiviGatewayARN = lens _ccscsiviGatewayARN (\s a -> s { _ccscsiviGatewayARN = a })
{-# INLINE ccscsiviGatewayARN #-}

ccscsiviVolumeSizeInBytes :: Lens' CreateCachediSCSIVolume (Integer)
ccscsiviVolumeSizeInBytes = lens _ccscsiviVolumeSizeInBytes (\s a -> s { _ccscsiviVolumeSizeInBytes = a })
{-# INLINE ccscsiviVolumeSizeInBytes #-}

ccscsiviSnapshotId :: Lens' CreateCachediSCSIVolume (Maybe Text)
ccscsiviSnapshotId = lens _ccscsiviSnapshotId (\s a -> s { _ccscsiviSnapshotId = a })
{-# INLINE ccscsiviSnapshotId #-}

ccscsiviTargetName :: Lens' CreateCachediSCSIVolume (Text)
ccscsiviTargetName = lens _ccscsiviTargetName (\s a -> s { _ccscsiviTargetName = a })
{-# INLINE ccscsiviTargetName #-}

ccscsiviNetworkInterfaceId :: Lens' CreateCachediSCSIVolume (Text)
ccscsiviNetworkInterfaceId = lens _ccscsiviNetworkInterfaceId (\s a -> s { _ccscsiviNetworkInterfaceId = a })
{-# INLINE ccscsiviNetworkInterfaceId #-}

ccscsiviClientToken :: Lens' CreateCachediSCSIVolume (Text)
ccscsiviClientToken = lens _ccscsiviClientToken (\s a -> s { _ccscsiviClientToken = a })
{-# INLINE ccscsiviClientToken #-}

instance ToPath CreateCachediSCSIVolume

instance ToQuery CreateCachediSCSIVolume

instance ToHeaders CreateCachediSCSIVolume

instance ToJSON CreateCachediSCSIVolume

data CreateCachediSCSIVolumeResponse = CreateCachediSCSIVolumeResponse
    { _ccscsivoVolumeARN :: Maybe Text
    , _ccscsivoTargetARN :: Maybe Text
    } deriving (Show, Generic)

ccscsivoVolumeARN :: Lens' CreateCachediSCSIVolumeResponse (Maybe Text)
ccscsivoVolumeARN = lens _ccscsivoVolumeARN (\s a -> s { _ccscsivoVolumeARN = a })
{-# INLINE ccscsivoVolumeARN #-}

ccscsivoTargetARN :: Lens' CreateCachediSCSIVolumeResponse (Maybe Text)
ccscsivoTargetARN = lens _ccscsivoTargetARN (\s a -> s { _ccscsivoTargetARN = a })
{-# INLINE ccscsivoTargetARN #-}

instance FromJSON CreateCachediSCSIVolumeResponse

instance AWSRequest CreateCachediSCSIVolume where
    type Sv CreateCachediSCSIVolume = StorageGateway
    type Rs CreateCachediSCSIVolume = CreateCachediSCSIVolumeResponse

    request = get
    response _ = jsonResponse
