{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.ListVolumeRecoveryPoints
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation lists the recovery points for a specified gateway. This
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
module Network.AWS.StorageGateway.V2013_06_30.ListVolumeRecoveryPoints where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data ListVolumeRecoveryPoints = ListVolumeRecoveryPoints
    { _lvrpiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

makeLenses ''ListVolumeRecoveryPoints

instance ToPath ListVolumeRecoveryPoints

instance ToQuery ListVolumeRecoveryPoints

instance ToHeaders ListVolumeRecoveryPoints

instance ToJSON ListVolumeRecoveryPoints

data ListVolumeRecoveryPointsResponse = ListVolumeRecoveryPointsResponse
    { _lvrpoGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _lvrpoVolumeRecoveryPointInfos :: [VolumeRecoveryPointInfo]
    } deriving (Show, Generic)

makeLenses ''ListVolumeRecoveryPointsResponse

instance FromJSON ListVolumeRecoveryPointsResponse

instance AWSRequest ListVolumeRecoveryPoints where
    type Sv ListVolumeRecoveryPoints = StorageGateway
    type Rs ListVolumeRecoveryPoints = ListVolumeRecoveryPointsResponse

    request = get
    response _ = jsonResponse
