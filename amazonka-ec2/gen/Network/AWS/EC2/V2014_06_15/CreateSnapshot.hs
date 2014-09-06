{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a snapshot of an Amazon EBS volume and stores it in Amazon S3. You
-- can use snapshots for backups, to make copies of Amazon EBS volumes, and to
-- save data before shutting down an instance. When a snapshot is created, any
-- AWS Marketplace product codes that are associated with the source volume
-- are propagated to the snapshot. You can take a snapshot of an attached
-- volume that is in use. However, snapshots only capture data that has been
-- written to your Amazon EBS volume at the time the snapshot command is
-- issued; this may exclude any data that has been cached by any applications
-- or the operating system. If you can pause any file systems on the volume
-- long enough to take a snapshot, your snapshot should be complete. However,
-- if you cannot pause all file writes to the volume, you should unmount the
-- volume from within the instance, issue the snapshot command, and then
-- remount the volume to ensure a consistent and complete snapshot. You may
-- remount and use your volume while the snapshot status is pending. To create
-- a snapshot for Amazon EBS volumes that serve as root devices, you should
-- stop the instance before taking the snapshot. Snapshots that are taken from
-- encrypted volumes are automatically encrypted. Volumes that are created
-- from encrypted snapshots are also automatically encrypted. Your encrypted
-- volumes and any associated snapshots always remain protected. For more
-- information, see Amazon Elastic Block Store and Amazon EBS Encryption in
-- the Amazon Elastic Compute Cloud User Guide. Example This example creates a
-- snapshot of the volume with the ID vol-1a2b3c4d.
-- https://ec2.amazonaws.com/?Action=CreateSnapshot &amp;VolumeId=vol-1a2b3c4d
-- &amp;Description=Daily+Backup &amp;AUTHPARAMS &lt;CreateSnapshotResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;snapshotId&gt;snap-1a2b3c4d&lt;/snapshotId&gt;
-- &lt;volumeId&gt;vol-1a2b3c4d&lt;/volumeId&gt;
-- &lt;status&gt;pending&lt;/status&gt;
-- &lt;startTime&gt;YYYY-MM-DDTHH:MM:SS.000Z&lt;/startTime&gt;
-- &lt;progress&gt;60%&lt;/progress&gt;
-- &lt;ownerId&gt;111122223333&lt;/ownerId&gt;
-- &lt;volumeSize&gt;30&lt;/volumeSize&gt; &lt;description&gt;Daily
-- Backup&lt;/description&gt; &lt;/CreateSnapshotResponse&gt;.
module Network.AWS.EC2.V2014_06_15.CreateSnapshot
    (
    -- * Request
      CreateSnapshot
    -- ** Request constructor
    , mkCreateSnapshot
    -- ** Request lenses
    , cs1VolumeId
    , cs1Description

    -- * Response
    , CreateSnapshotResponse
    -- ** Response lenses
    , csrsrsSnapshotId
    , csrsrsVolumeId
    , csrsrsState
    , csrsrsStartTime
    , csrsrsProgress
    , csrsrsOwnerId
    , csrsrsDescription
    , csrsrsVolumeSize
    , csrsrsOwnerAlias
    , csrsrsEncrypted
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data CreateSnapshot = CreateSnapshot
    { _cs1VolumeId :: Text
    , _cs1Description :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateSnapshot' request.
mkCreateSnapshot :: Text -- ^ 'cs1VolumeId'
                 -> CreateSnapshot
mkCreateSnapshot p1 = CreateSnapshot
    { _cs1VolumeId = p1
    , _cs1Description = Nothing
    }
{-# INLINE mkCreateSnapshot #-}

-- | The ID of the Amazon EBS volume.
cs1VolumeId :: Lens' CreateSnapshot Text
cs1VolumeId = lens _cs1VolumeId (\s a -> s { _cs1VolumeId = a })
{-# INLINE cs1VolumeId #-}

-- | A description for the snapshot.
cs1Description :: Lens' CreateSnapshot (Maybe Text)
cs1Description = lens _cs1Description (\s a -> s { _cs1Description = a })
{-# INLINE cs1Description #-}

instance ToQuery CreateSnapshot where
    toQuery = genericQuery def

-- | 
data CreateSnapshotResponse = CreateSnapshotResponse
    { _csrsrsSnapshotId :: Maybe Text
    , _csrsrsVolumeId :: Maybe Text
    , _csrsrsState :: Maybe SnapshotState
    , _csrsrsStartTime :: Maybe ISO8601
    , _csrsrsProgress :: Maybe Text
    , _csrsrsOwnerId :: Maybe Text
    , _csrsrsDescription :: Maybe Text
    , _csrsrsVolumeSize :: Maybe Integer
    , _csrsrsOwnerAlias :: Maybe Text
    , _csrsrsEncrypted :: Maybe Bool
    } deriving (Show, Generic)

-- | The ID of the snapshot.
csrsrsSnapshotId :: Lens' CreateSnapshotResponse (Maybe Text)
csrsrsSnapshotId =
    lens _csrsrsSnapshotId (\s a -> s { _csrsrsSnapshotId = a })
{-# INLINE csrsrsSnapshotId #-}

-- | The ID of the volume.
csrsrsVolumeId :: Lens' CreateSnapshotResponse (Maybe Text)
csrsrsVolumeId = lens _csrsrsVolumeId (\s a -> s { _csrsrsVolumeId = a })
{-# INLINE csrsrsVolumeId #-}

-- | The snapshot state.
csrsrsState :: Lens' CreateSnapshotResponse (Maybe SnapshotState)
csrsrsState = lens _csrsrsState (\s a -> s { _csrsrsState = a })
{-# INLINE csrsrsState #-}

-- | The time stamp when the snapshot was initiated.
csrsrsStartTime :: Lens' CreateSnapshotResponse (Maybe ISO8601)
csrsrsStartTime = lens _csrsrsStartTime (\s a -> s { _csrsrsStartTime = a })
{-# INLINE csrsrsStartTime #-}

-- | The progress of the snapshot, as a percentage.
csrsrsProgress :: Lens' CreateSnapshotResponse (Maybe Text)
csrsrsProgress = lens _csrsrsProgress (\s a -> s { _csrsrsProgress = a })
{-# INLINE csrsrsProgress #-}

-- | The AWS account ID of the Amazon EBS snapshot owner.
csrsrsOwnerId :: Lens' CreateSnapshotResponse (Maybe Text)
csrsrsOwnerId = lens _csrsrsOwnerId (\s a -> s { _csrsrsOwnerId = a })
{-# INLINE csrsrsOwnerId #-}

-- | The description for the snapshot.
csrsrsDescription :: Lens' CreateSnapshotResponse (Maybe Text)
csrsrsDescription =
    lens _csrsrsDescription (\s a -> s { _csrsrsDescription = a })
{-# INLINE csrsrsDescription #-}

-- | The size of the volume, in GiB.
csrsrsVolumeSize :: Lens' CreateSnapshotResponse (Maybe Integer)
csrsrsVolumeSize =
    lens _csrsrsVolumeSize (\s a -> s { _csrsrsVolumeSize = a })
{-# INLINE csrsrsVolumeSize #-}

-- | The AWS account alias (for example, amazon, self) or AWS account ID that
-- owns the snapshot.
csrsrsOwnerAlias :: Lens' CreateSnapshotResponse (Maybe Text)
csrsrsOwnerAlias =
    lens _csrsrsOwnerAlias (\s a -> s { _csrsrsOwnerAlias = a })
{-# INLINE csrsrsOwnerAlias #-}

-- | Indicates whether the snapshot is encrypted.
csrsrsEncrypted :: Lens' CreateSnapshotResponse (Maybe Bool)
csrsrsEncrypted = lens _csrsrsEncrypted (\s a -> s { _csrsrsEncrypted = a })
{-# INLINE csrsrsEncrypted #-}

instance FromXML CreateSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateSnapshot where
    type Sv CreateSnapshot = EC2
    type Rs CreateSnapshot = CreateSnapshotResponse

    request = post "CreateSnapshot"
    response _ = xmlResponse
