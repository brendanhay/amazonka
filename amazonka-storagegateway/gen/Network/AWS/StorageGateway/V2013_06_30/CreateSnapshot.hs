{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.CreateSnapshot
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
module Network.AWS.StorageGateway.V2013_06_30.CreateSnapshot where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data CreateSnapshot = CreateSnapshot
    { _csiSnapshotDescription :: Text
      -- ^ Textual description of the snapshot that appears in the Amazon
      -- EC2 console, Elastic Block Store snapshots panel in the
      -- Description field, and in the AWS Storage Gateway snapshot
      -- Details pane, Description field.
    , _csiVolumeARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
      -- operation to return a list of gateway volumes.
    } deriving (Show, Generic)

makeLenses ''CreateSnapshot

instance ToPath CreateSnapshot

instance ToQuery CreateSnapshot

instance ToHeaders CreateSnapshot

instance ToJSON CreateSnapshot

data CreateSnapshotResponse = CreateSnapshotResponse
    { _csoSnapshotId :: Maybe Text
      -- ^ The snapshot ID that is used to refer to the snapshot in future
      -- operations such as describing snapshots (Amazon Elastic Compute
      -- Cloud API DescribeSnapshots) or creating a volume from a snapshot
      -- (CreateStorediSCSIVolume).
    , _csoVolumeARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the volume of which the
      -- snapshot was taken.
    } deriving (Show, Generic)

makeLenses ''CreateSnapshotResponse

instance FromJSON CreateSnapshotResponse

instance AWSRequest CreateSnapshot where
    type Sv CreateSnapshot = StorageGateway
    type Rs CreateSnapshot = CreateSnapshotResponse

    request = get
    response _ = jsonResponse
