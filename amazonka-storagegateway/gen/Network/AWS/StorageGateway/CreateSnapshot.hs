{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation initiates a snapshot of a volume. AWS Storage Gateway
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
module Network.AWS.StorageGateway
    (
    -- * Request
      CreateSnapshot
    -- ** Request constructor
    , mkCreateSnapshot
    -- ** Request lenses
    , csVolumeARN
    , csSnapshotDescription

    -- * Response
    , CreateSnapshotResponse
    -- ** Response constructor
    , mkCreateSnapshotResponse
    -- ** Response lenses
    , csrVolumeARN
    , csrSnapshotId
    ) where

import Network.AWS.StorageGateway.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | A JSON object containing one or more of the following fields:
-- CreateSnapshotInput$SnapshotDescription CreateSnapshotInput$VolumeARN.
data CreateSnapshot = CreateSnapshot
    { _csVolumeARN :: Text
    , _csSnapshotDescription :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateSnapshot' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeARN ::@ @Text@
--
-- * @SnapshotDescription ::@ @Text@
--
mkCreateSnapshot :: Text -- ^ 'csVolumeARN'
                 -> Text -- ^ 'csSnapshotDescription'
                 -> CreateSnapshot
mkCreateSnapshot p1 p2 = CreateSnapshot
    { _csVolumeARN = p1
    , _csSnapshotDescription = p2
    }

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes operation
-- to return a list of gateway volumes.
csVolumeARN :: Lens' CreateSnapshot Text
csVolumeARN = lens _csVolumeARN (\s a -> s { _csVolumeARN = a })

-- | Textual description of the snapshot that appears in the Amazon EC2 console,
-- Elastic Block Store snapshots panel in the Description field, and in the
-- AWS Storage Gateway snapshot Details pane, Description field.
csSnapshotDescription :: Lens' CreateSnapshot Text
csSnapshotDescription =
    lens _csSnapshotDescription (\s a -> s { _csSnapshotDescription = a })

instance ToPath CreateSnapshot

instance ToQuery CreateSnapshot

instance ToHeaders CreateSnapshot

instance ToJSON CreateSnapshot

-- | A JSON object containing the following fields:.
data CreateSnapshotResponse = CreateSnapshotResponse
    { _csrVolumeARN :: Maybe Text
    , _csrSnapshotId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateSnapshotResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeARN ::@ @Maybe Text@
--
-- * @SnapshotId ::@ @Maybe Text@
--
mkCreateSnapshotResponse :: CreateSnapshotResponse
mkCreateSnapshotResponse = CreateSnapshotResponse
    { _csrVolumeARN = Nothing
    , _csrSnapshotId = Nothing
    }

-- | The Amazon Resource Name (ARN) of the volume of which the snapshot was
-- taken.
csrVolumeARN :: Lens' CreateSnapshotResponse (Maybe Text)
csrVolumeARN = lens _csrVolumeARN (\s a -> s { _csrVolumeARN = a })

-- | The snapshot ID that is used to refer to the snapshot in future operations
-- such as describing snapshots (Amazon Elastic Compute Cloud API
-- DescribeSnapshots) or creating a volume from a snapshot
-- (CreateStorediSCSIVolume).
csrSnapshotId :: Lens' CreateSnapshotResponse (Maybe Text)
csrSnapshotId = lens _csrSnapshotId (\s a -> s { _csrSnapshotId = a })

instance FromJSON CreateSnapshotResponse

instance AWSRequest CreateSnapshot where
    type Sv CreateSnapshot = StorageGateway
    type Rs CreateSnapshot = CreateSnapshotResponse

    request = get
    response _ = jsonResponse
