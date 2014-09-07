{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.CreateSnapshotFromVolumeRecoveryPoint
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation initiates a snapshot of a gateway from a volume recovery
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
module Network.AWS.StorageGateway.V2013_06_30.CreateSnapshotFromVolumeRecoveryPoint
    (
    -- * Request
      CreateSnapshotFromVolumeRecoveryPoint
    -- ** Request constructor
    , mkCreateSnapshotFromVolumeRecoveryPoint
    -- ** Request lenses
    , csfvrpVolumeARN
    , csfvrpSnapshotDescription

    -- * Response
    , CreateSnapshotFromVolumeRecoveryPointResponse
    -- ** Response lenses
    , csfvrprsSnapshotId
    , csfvrprsVolumeARN
    , csfvrprsVolumeRecoveryPointTime
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data CreateSnapshotFromVolumeRecoveryPoint = CreateSnapshotFromVolumeRecoveryPoint
    { _csfvrpVolumeARN :: Text
    , _csfvrpSnapshotDescription :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateSnapshotFromVolumeRecoveryPoint' request.
mkCreateSnapshotFromVolumeRecoveryPoint :: Text -- ^ 'csfvrpVolumeARN'
                                        -> Text -- ^ 'csfvrpSnapshotDescription'
                                        -> CreateSnapshotFromVolumeRecoveryPoint
mkCreateSnapshotFromVolumeRecoveryPoint p1 p2 = CreateSnapshotFromVolumeRecoveryPoint
    { _csfvrpVolumeARN = p1
    , _csfvrpSnapshotDescription = p2
    }

csfvrpVolumeARN :: Lens' CreateSnapshotFromVolumeRecoveryPoint Text
csfvrpVolumeARN = lens _csfvrpVolumeARN (\s a -> s { _csfvrpVolumeARN = a })

csfvrpSnapshotDescription :: Lens' CreateSnapshotFromVolumeRecoveryPoint Text
csfvrpSnapshotDescription =
    lens _csfvrpSnapshotDescription
         (\s a -> s { _csfvrpSnapshotDescription = a })

instance ToPath CreateSnapshotFromVolumeRecoveryPoint

instance ToQuery CreateSnapshotFromVolumeRecoveryPoint

instance ToHeaders CreateSnapshotFromVolumeRecoveryPoint

instance ToJSON CreateSnapshotFromVolumeRecoveryPoint

data CreateSnapshotFromVolumeRecoveryPointResponse = CreateSnapshotFromVolumeRecoveryPointResponse
    { _csfvrprsSnapshotId :: Maybe Text
    , _csfvrprsVolumeARN :: Maybe Text
    , _csfvrprsVolumeRecoveryPointTime :: Maybe Text
    } deriving (Show, Generic)

csfvrprsSnapshotId :: Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Maybe Text)
csfvrprsSnapshotId =
    lens _csfvrprsSnapshotId (\s a -> s { _csfvrprsSnapshotId = a })

csfvrprsVolumeARN :: Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Maybe Text)
csfvrprsVolumeARN =
    lens _csfvrprsVolumeARN (\s a -> s { _csfvrprsVolumeARN = a })

csfvrprsVolumeRecoveryPointTime :: Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Maybe Text)
csfvrprsVolumeRecoveryPointTime =
    lens _csfvrprsVolumeRecoveryPointTime
         (\s a -> s { _csfvrprsVolumeRecoveryPointTime = a })

instance FromJSON CreateSnapshotFromVolumeRecoveryPointResponse

instance AWSRequest CreateSnapshotFromVolumeRecoveryPoint where
    type Sv CreateSnapshotFromVolumeRecoveryPoint = StorageGateway
    type Rs CreateSnapshotFromVolumeRecoveryPoint = CreateSnapshotFromVolumeRecoveryPointResponse

    request = get
    response _ = jsonResponse
