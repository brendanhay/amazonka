{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateSnapshot
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
module Network.AWS.EC2.CreateSnapshot
    (
    -- * Request
      CreateSnapshot
    -- ** Request constructor
    , createSnapshot
    -- ** Request lenses
    , cs1VolumeId
    , cs1Description

    -- * Response
    , CreateSnapshotResponse
    -- ** Response constructor
    , createSnapshotResponse
    -- ** Response lenses
    , csrrSnapshotId
    , csrrVolumeId
    , csrrState
    , csrrStartTime
    , csrrProgress
    , csrrOwnerId
    , csrrDescription
    , csrrVolumeSize
    , csrrOwnerAlias
    , csrrEncrypted
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data CreateSnapshot = CreateSnapshot
    { _cs1VolumeId :: Text
    , _cs1Description :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateSnapshot' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeId ::@ @Text@
--
-- * @Description ::@ @Maybe Text@
--
createSnapshot :: Text -- ^ 'cs1VolumeId'
               -> CreateSnapshot
createSnapshot p1 = CreateSnapshot
    { _cs1VolumeId = p1
    , _cs1Description = Nothing
    }

-- | The ID of the Amazon EBS volume.
cs1VolumeId :: Lens' CreateSnapshot Text
cs1VolumeId = lens _cs1VolumeId (\s a -> s { _cs1VolumeId = a })

-- | A description for the snapshot.
cs1Description :: Lens' CreateSnapshot (Maybe Text)
cs1Description = lens _cs1Description (\s a -> s { _cs1Description = a })

instance ToQuery CreateSnapshot where
    toQuery = genericQuery def

data CreateSnapshotResponse = CreateSnapshotResponse
    { _csrrSnapshotId :: Maybe Text
    , _csrrVolumeId :: Maybe Text
    , _csrrState :: Maybe SnapshotState
    , _csrrStartTime :: Maybe ISO8601
    , _csrrProgress :: Maybe Text
    , _csrrOwnerId :: Maybe Text
    , _csrrDescription :: Maybe Text
    , _csrrVolumeSize :: Maybe Integer
    , _csrrOwnerAlias :: Maybe Text
    , _csrrEncrypted :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateSnapshotResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SnapshotId ::@ @Maybe Text@
--
-- * @VolumeId ::@ @Maybe Text@
--
-- * @State ::@ @Maybe SnapshotState@
--
-- * @StartTime ::@ @Maybe ISO8601@
--
-- * @Progress ::@ @Maybe Text@
--
-- * @OwnerId ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @VolumeSize ::@ @Maybe Integer@
--
-- * @OwnerAlias ::@ @Maybe Text@
--
-- * @Encrypted ::@ @Maybe Bool@
--
createSnapshotResponse :: CreateSnapshotResponse
createSnapshotResponse = CreateSnapshotResponse
    { _csrrSnapshotId = Nothing
    , _csrrVolumeId = Nothing
    , _csrrState = Nothing
    , _csrrStartTime = Nothing
    , _csrrProgress = Nothing
    , _csrrOwnerId = Nothing
    , _csrrDescription = Nothing
    , _csrrVolumeSize = Nothing
    , _csrrOwnerAlias = Nothing
    , _csrrEncrypted = Nothing
    }

-- | The ID of the snapshot.
csrrSnapshotId :: Lens' CreateSnapshotResponse (Maybe Text)
csrrSnapshotId = lens _csrrSnapshotId (\s a -> s { _csrrSnapshotId = a })

-- | The ID of the volume.
csrrVolumeId :: Lens' CreateSnapshotResponse (Maybe Text)
csrrVolumeId = lens _csrrVolumeId (\s a -> s { _csrrVolumeId = a })

-- | The snapshot state.
csrrState :: Lens' CreateSnapshotResponse (Maybe SnapshotState)
csrrState = lens _csrrState (\s a -> s { _csrrState = a })

-- | The time stamp when the snapshot was initiated.
csrrStartTime :: Lens' CreateSnapshotResponse (Maybe ISO8601)
csrrStartTime = lens _csrrStartTime (\s a -> s { _csrrStartTime = a })

-- | The progress of the snapshot, as a percentage.
csrrProgress :: Lens' CreateSnapshotResponse (Maybe Text)
csrrProgress = lens _csrrProgress (\s a -> s { _csrrProgress = a })

-- | The AWS account ID of the Amazon EBS snapshot owner.
csrrOwnerId :: Lens' CreateSnapshotResponse (Maybe Text)
csrrOwnerId = lens _csrrOwnerId (\s a -> s { _csrrOwnerId = a })

-- | The description for the snapshot.
csrrDescription :: Lens' CreateSnapshotResponse (Maybe Text)
csrrDescription = lens _csrrDescription (\s a -> s { _csrrDescription = a })

-- | The size of the volume, in GiB.
csrrVolumeSize :: Lens' CreateSnapshotResponse (Maybe Integer)
csrrVolumeSize = lens _csrrVolumeSize (\s a -> s { _csrrVolumeSize = a })

-- | The AWS account alias (for example, amazon, self) or AWS account ID that
-- owns the snapshot.
csrrOwnerAlias :: Lens' CreateSnapshotResponse (Maybe Text)
csrrOwnerAlias = lens _csrrOwnerAlias (\s a -> s { _csrrOwnerAlias = a })

-- | Indicates whether the snapshot is encrypted.
csrrEncrypted :: Lens' CreateSnapshotResponse (Maybe Bool)
csrrEncrypted = lens _csrrEncrypted (\s a -> s { _csrrEncrypted = a })

instance FromXML CreateSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateSnapshot where
    type Sv CreateSnapshot = EC2
    type Rs CreateSnapshot = CreateSnapshotResponse

    request = post "CreateSnapshot"
    response _ = xmlResponse
