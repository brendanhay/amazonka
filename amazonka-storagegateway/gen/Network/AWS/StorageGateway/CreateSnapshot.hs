{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a snapshot of a volume.
--
--
-- AWS Storage Gateway provides the ability to back up point-in-time snapshots of your data to Amazon Simple Storage (S3) for durable off-site recovery, as well as import the data to an Amazon Elastic Block Store (EBS) volume in Amazon Elastic Compute Cloud (EC2). You can take snapshots of your gateway volume on a scheduled or ad-hoc basis. This API enables you to take ad-hoc snapshot. For more information, see <http://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#SchedulingSnapshot Editing a Snapshot Schedule> .
--
-- In the CreateSnapshot request you identify the volume by providing its Amazon Resource Name (ARN). You must also provide description for the snapshot. When AWS Storage Gateway takes the snapshot of specified volume, the snapshot and description appears in the AWS Storage Gateway Console. In response, AWS Storage Gateway returns you a snapshot ID. You can use this snapshot ID to check the snapshot progress or later use it when you want to create a volume from a snapshot. This operation is only supported in stored and cached volume gateway type.
--
-- /Important:/ Volume and snapshot IDs are changing to a longer length ID format. For more information, see the important note on the <http://docs.aws.amazon.com/storagegateway/latest/APIReference/Welcome.html Welcome> page.
--
module Network.AWS.StorageGateway.CreateSnapshot
    (
    -- * Creating a Request
      createSnapshot
    , CreateSnapshot
    -- * Request Lenses
    , csVolumeARN
    , csSnapshotDescription

    -- * Destructuring the Response
    , createSnapshotResponse
    , CreateSnapshotResponse
    -- * Response Lenses
    , csrsVolumeARN
    , csrsSnapshotId
    , csrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | A JSON object containing one or more of the following fields:
--
--
--     * 'CreateSnapshotInput$SnapshotDescription'
--
--     * 'CreateSnapshotInput$VolumeARN'
--
--
--
--
-- /See:/ 'createSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { _csVolumeARN           :: !Text
  , _csSnapshotDescription :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csVolumeARN' - The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
--
-- * 'csSnapshotDescription' - Textual description of the snapshot that appears in the Amazon EC2 console, Elastic Block Store snapshots panel in the __Description__ field, and in the AWS Storage Gateway snapshot __Details__ pane, __Description__ field
createSnapshot
    :: Text -- ^ 'csVolumeARN'
    -> Text -- ^ 'csSnapshotDescription'
    -> CreateSnapshot
createSnapshot pVolumeARN_ pSnapshotDescription_ =
  CreateSnapshot'
    {_csVolumeARN = pVolumeARN_, _csSnapshotDescription = pSnapshotDescription_}


-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
csVolumeARN :: Lens' CreateSnapshot Text
csVolumeARN = lens _csVolumeARN (\ s a -> s{_csVolumeARN = a})

-- | Textual description of the snapshot that appears in the Amazon EC2 console, Elastic Block Store snapshots panel in the __Description__ field, and in the AWS Storage Gateway snapshot __Details__ pane, __Description__ field
csSnapshotDescription :: Lens' CreateSnapshot Text
csSnapshotDescription = lens _csSnapshotDescription (\ s a -> s{_csSnapshotDescription = a})

instance AWSRequest CreateSnapshot where
        type Rs CreateSnapshot = CreateSnapshotResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 CreateSnapshotResponse' <$>
                   (x .?> "VolumeARN") <*> (x .?> "SnapshotId") <*>
                     (pure (fromEnum s)))

instance Hashable CreateSnapshot where

instance NFData CreateSnapshot where

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
              (catMaybes
                 [Just ("VolumeARN" .= _csVolumeARN),
                  Just
                    ("SnapshotDescription" .= _csSnapshotDescription)])

instance ToPath CreateSnapshot where
        toPath = const "/"

instance ToQuery CreateSnapshot where
        toQuery = const mempty

-- | A JSON object containing the following fields:
--
--
--
-- /See:/ 'createSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
  { _csrsVolumeARN      :: !(Maybe Text)
  , _csrsSnapshotId     :: !(Maybe Text)
  , _csrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsVolumeARN' - The Amazon Resource Name (ARN) of the volume of which the snapshot was taken.
--
-- * 'csrsSnapshotId' - The snapshot ID that is used to refer to the snapshot in future operations such as describing snapshots (Amazon Elastic Compute Cloud API @DescribeSnapshots@ ) or creating a volume from a snapshot ('CreateStorediSCSIVolume' ).
--
-- * 'csrsResponseStatus' - -- | The response status code.
createSnapshotResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> CreateSnapshotResponse
createSnapshotResponse pResponseStatus_ =
  CreateSnapshotResponse'
    { _csrsVolumeARN = Nothing
    , _csrsSnapshotId = Nothing
    , _csrsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) of the volume of which the snapshot was taken.
csrsVolumeARN :: Lens' CreateSnapshotResponse (Maybe Text)
csrsVolumeARN = lens _csrsVolumeARN (\ s a -> s{_csrsVolumeARN = a})

-- | The snapshot ID that is used to refer to the snapshot in future operations such as describing snapshots (Amazon Elastic Compute Cloud API @DescribeSnapshots@ ) or creating a volume from a snapshot ('CreateStorediSCSIVolume' ).
csrsSnapshotId :: Lens' CreateSnapshotResponse (Maybe Text)
csrsSnapshotId = lens _csrsSnapshotId (\ s a -> s{_csrsSnapshotId = a})

-- | -- | The response status code.
csrsResponseStatus :: Lens' CreateSnapshotResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a})

instance NFData CreateSnapshotResponse where
