{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.CreateSnapshot
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

-- | This operation initiates a snapshot of a volume.
--
-- AWS Storage Gateway provides the ability to back up point-in-time
-- snapshots of your data to Amazon Simple Storage (S3) for durable
-- off-site recovery, as well as import the data to an Amazon Elastic Block
-- Store (EBS) volume in Amazon Elastic Compute Cloud (EC2). You can take
-- snapshots of your gateway volume on a scheduled or ad-hoc basis. This
-- API enables you to take ad-hoc snapshot. For more information, see
-- <http://docs.aws.amazon.com/storagegateway/latest/userguide/WorkingWithSnapshots.html Working With Snapshots in the AWS Storage Gateway Console>.
--
-- In the CreateSnapshot request you identify the volume by providing its
-- Amazon Resource Name (ARN). You must also provide description for the
-- snapshot. When AWS Storage Gateway takes the snapshot of specified
-- volume, the snapshot and description appears in the AWS Storage Gateway
-- Console. In response, AWS Storage Gateway returns you a snapshot ID. You
-- can use this snapshot ID to check the snapshot progress or later use it
-- when you want to create a volume from a snapshot.
--
-- To list or delete a snapshot, you must use the Amazon EC2 API. For more
-- information, see DescribeSnapshots or DeleteSnapshot in the
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_Operations.html EC2 API reference>.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_CreateSnapshot.html>
module Network.AWS.StorageGateway.CreateSnapshot
    (
    -- * Request
      CreateSnapshot
    -- ** Request constructor
    , createSnapshot
    -- ** Request lenses
    , csVolumeARN
    , csSnapshotDescription

    -- * Response
    , CreateSnapshotResponse
    -- ** Response constructor
    , createSnapshotResponse
    -- ** Response lenses
    , csrVolumeARN
    , csrSnapshotId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'createSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csVolumeARN'
--
-- * 'csSnapshotDescription'
data CreateSnapshot = CreateSnapshot'{_csVolumeARN :: Text, _csSnapshotDescription :: Text} deriving (Eq, Read, Show)

-- | 'CreateSnapshot' smart constructor.
createSnapshot :: Text -> Text -> CreateSnapshot
createSnapshot pVolumeARN pSnapshotDescription = CreateSnapshot'{_csVolumeARN = pVolumeARN, _csSnapshotDescription = pSnapshotDescription};

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
csVolumeARN :: Lens' CreateSnapshot Text
csVolumeARN = lens _csVolumeARN (\ s a -> s{_csVolumeARN = a});

-- | Textual description of the snapshot that appears in the Amazon EC2
-- console, Elastic Block Store snapshots panel in the __Description__
-- field, and in the AWS Storage Gateway snapshot __Details__ pane,
-- __Description__ field
csSnapshotDescription :: Lens' CreateSnapshot Text
csSnapshotDescription = lens _csSnapshotDescription (\ s a -> s{_csSnapshotDescription = a});

instance AWSRequest CreateSnapshot where
        type Sv CreateSnapshot = StorageGateway
        type Rs CreateSnapshot = CreateSnapshotResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateSnapshotResponse' <$>
                   (x .?> "VolumeARN") <*> (x .?> "SnapshotId"))

instance ToHeaders CreateSnapshot where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.CreateSnapshot" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateSnapshot where
        toJSON CreateSnapshot'{..}
          = object
              ["VolumeARN" .= _csVolumeARN,
               "SnapshotDescription" .= _csSnapshotDescription]

instance ToPath CreateSnapshot where
        toPath = const "/"

instance ToQuery CreateSnapshot where
        toQuery = const mempty

-- | /See:/ 'createSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrVolumeARN'
--
-- * 'csrSnapshotId'
data CreateSnapshotResponse = CreateSnapshotResponse'{_csrVolumeARN :: Maybe Text, _csrSnapshotId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CreateSnapshotResponse' smart constructor.
createSnapshotResponse :: CreateSnapshotResponse
createSnapshotResponse = CreateSnapshotResponse'{_csrVolumeARN = Nothing, _csrSnapshotId = Nothing};

-- | The Amazon Resource Name (ARN) of the volume of which the snapshot was
-- taken.
csrVolumeARN :: Lens' CreateSnapshotResponse (Maybe Text)
csrVolumeARN = lens _csrVolumeARN (\ s a -> s{_csrVolumeARN = a});

-- | The snapshot ID that is used to refer to the snapshot in future
-- operations such as describing snapshots (Amazon Elastic Compute Cloud
-- API @DescribeSnapshots@) or creating a volume from a snapshot
-- (CreateStorediSCSIVolume).
csrSnapshotId :: Lens' CreateSnapshotResponse (Maybe Text)
csrSnapshotId = lens _csrSnapshotId (\ s a -> s{_csrSnapshotId = a});
