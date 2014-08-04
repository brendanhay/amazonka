{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.StorageGateway.V2013_06_30.CreateCachediSCSIVolume where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateCachediSCSIVolume' request.
createCachediSCSIVolume :: Text -- ^ '_ccscsiviClientToken'
                        -> Text -- ^ '_ccscsiviGatewayARN'
                        -> Integer -- ^ '_ccscsiviVolumeSizeInBytes'
                        -> Text -- ^ '_ccscsiviNetworkInterfaceId'
                        -> Text -- ^ '_ccscsiviTargetName'
                        -> CreateCachediSCSIVolume
createCachediSCSIVolume p1 p2 p3 p4 p5 = CreateCachediSCSIVolume
    { _ccscsiviClientToken = p1
    , _ccscsiviGatewayARN = p2
    , _ccscsiviVolumeSizeInBytes = p3
    , _ccscsiviNetworkInterfaceId = p4
    , _ccscsiviTargetName = p5
    , _ccscsiviSnapshotId = Nothing
    }

data CreateCachediSCSIVolume = CreateCachediSCSIVolume
    { _ccscsiviClientToken :: Text
    , _ccscsiviGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _ccscsiviVolumeSizeInBytes :: Integer
    , _ccscsiviNetworkInterfaceId :: Text
    , _ccscsiviTargetName :: Text
    , _ccscsiviSnapshotId :: Maybe Text
    } deriving (Generic)

makeLenses ''CreateCachediSCSIVolume

instance ToPath CreateCachediSCSIVolume

instance ToQuery CreateCachediSCSIVolume

instance ToHeaders CreateCachediSCSIVolume

instance ToJSON CreateCachediSCSIVolume

data CreateCachediSCSIVolumeResponse = CreateCachediSCSIVolumeResponse
    { _ccscsivoTargetARN :: Maybe Text
    , _ccscsivoVolumeARN :: Maybe Text
    } deriving (Generic)

makeLenses ''CreateCachediSCSIVolumeResponse

instance FromJSON CreateCachediSCSIVolumeResponse

instance AWSRequest CreateCachediSCSIVolume where
    type Sv CreateCachediSCSIVolume = StorageGateway
    type Rs CreateCachediSCSIVolume = CreateCachediSCSIVolumeResponse

    request = get
    response _ = jsonResponse
