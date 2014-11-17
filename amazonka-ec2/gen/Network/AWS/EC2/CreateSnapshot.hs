{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
-- the Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSnapshot.html>
module Network.AWS.EC2.CreateSnapshot
    (
    -- * Request
      CreateSnapshot
    -- ** Request constructor
    , createSnapshot
    -- ** Request lenses
    , cs2Description
    , cs2DryRun
    , cs2VolumeId

    -- * Response
    , CreateSnapshotResponse
    -- ** Response constructor
    , createSnapshotResponse
    -- ** Response lenses
    , csr1Description
    , csr1Encrypted
    , csr1OwnerAlias
    , csr1OwnerId
    , csr1Progress
    , csr1SnapshotId
    , csr1StartTime
    , csr1State
    , csr1Tags
    , csr1VolumeId
    , csr1VolumeSize
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateSnapshot = CreateSnapshot
    { _cs2Description :: Maybe Text
    , _cs2DryRun      :: Maybe Bool
    , _cs2VolumeId    :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateSnapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cs2Description' @::@ 'Maybe' 'Text'
--
-- * 'cs2DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'cs2VolumeId' @::@ 'Text'
--
createSnapshot :: Text -- ^ 'cs2VolumeId'
               -> CreateSnapshot
createSnapshot p1 = CreateSnapshot
    { _cs2VolumeId    = p1
    , _cs2DryRun      = Nothing
    , _cs2Description = Nothing
    }

-- | A description for the snapshot.
cs2Description :: Lens' CreateSnapshot (Maybe Text)
cs2Description = lens _cs2Description (\s a -> s { _cs2Description = a })

cs2DryRun :: Lens' CreateSnapshot (Maybe Bool)
cs2DryRun = lens _cs2DryRun (\s a -> s { _cs2DryRun = a })

-- | The ID of the Amazon EBS volume.
cs2VolumeId :: Lens' CreateSnapshot Text
cs2VolumeId = lens _cs2VolumeId (\s a -> s { _cs2VolumeId = a })

data CreateSnapshotResponse = CreateSnapshotResponse
    { _csr1Description :: Maybe Text
    , _csr1Encrypted   :: Maybe Bool
    , _csr1OwnerAlias  :: Maybe Text
    , _csr1OwnerId     :: Maybe Text
    , _csr1Progress    :: Maybe Text
    , _csr1SnapshotId  :: Maybe Text
    , _csr1StartTime   :: Maybe RFC822
    , _csr1State       :: Maybe Text
    , _csr1Tags        :: [Tag]
    , _csr1VolumeId    :: Maybe Text
    , _csr1VolumeSize  :: Maybe Int
    } deriving (Eq, Show, Generic)

-- | 'CreateSnapshotResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csr1Description' @::@ 'Maybe' 'Text'
--
-- * 'csr1Encrypted' @::@ 'Maybe' 'Bool'
--
-- * 'csr1OwnerAlias' @::@ 'Maybe' 'Text'
--
-- * 'csr1OwnerId' @::@ 'Maybe' 'Text'
--
-- * 'csr1Progress' @::@ 'Maybe' 'Text'
--
-- * 'csr1SnapshotId' @::@ 'Maybe' 'Text'
--
-- * 'csr1StartTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'csr1State' @::@ 'Maybe' 'Text'
--
-- * 'csr1Tags' @::@ ['Tag']
--
-- * 'csr1VolumeId' @::@ 'Maybe' 'Text'
--
-- * 'csr1VolumeSize' @::@ 'Maybe' 'Int'
--
createSnapshotResponse :: CreateSnapshotResponse
createSnapshotResponse = CreateSnapshotResponse
    { _csr1SnapshotId  = Nothing
    , _csr1VolumeId    = Nothing
    , _csr1State       = Nothing
    , _csr1StartTime   = Nothing
    , _csr1Progress    = Nothing
    , _csr1OwnerId     = Nothing
    , _csr1Description = Nothing
    , _csr1VolumeSize  = Nothing
    , _csr1OwnerAlias  = Nothing
    , _csr1Tags        = mempty
    , _csr1Encrypted   = Nothing
    }

-- | The description for the snapshot.
csr1Description :: Lens' CreateSnapshotResponse (Maybe Text)
csr1Description = lens _csr1Description (\s a -> s { _csr1Description = a })

-- | Indicates whether the snapshot is encrypted.
csr1Encrypted :: Lens' CreateSnapshotResponse (Maybe Bool)
csr1Encrypted = lens _csr1Encrypted (\s a -> s { _csr1Encrypted = a })

-- | The AWS account alias (for example, amazon, self) or AWS account ID that
-- owns the snapshot.
csr1OwnerAlias :: Lens' CreateSnapshotResponse (Maybe Text)
csr1OwnerAlias = lens _csr1OwnerAlias (\s a -> s { _csr1OwnerAlias = a })

-- | The AWS account ID of the Amazon EBS snapshot owner.
csr1OwnerId :: Lens' CreateSnapshotResponse (Maybe Text)
csr1OwnerId = lens _csr1OwnerId (\s a -> s { _csr1OwnerId = a })

-- | The progress of the snapshot, as a percentage.
csr1Progress :: Lens' CreateSnapshotResponse (Maybe Text)
csr1Progress = lens _csr1Progress (\s a -> s { _csr1Progress = a })

-- | The ID of the snapshot.
csr1SnapshotId :: Lens' CreateSnapshotResponse (Maybe Text)
csr1SnapshotId = lens _csr1SnapshotId (\s a -> s { _csr1SnapshotId = a })

-- | The time stamp when the snapshot was initiated.
csr1StartTime :: Lens' CreateSnapshotResponse (Maybe UTCTime)
csr1StartTime = lens _csr1StartTime (\s a -> s { _csr1StartTime = a })
    . mapping _Time

-- | The snapshot state.
csr1State :: Lens' CreateSnapshotResponse (Maybe Text)
csr1State = lens _csr1State (\s a -> s { _csr1State = a })

-- | Any tags assigned to the snapshot.
csr1Tags :: Lens' CreateSnapshotResponse [Tag]
csr1Tags = lens _csr1Tags (\s a -> s { _csr1Tags = a })

-- | The ID of the volume.
csr1VolumeId :: Lens' CreateSnapshotResponse (Maybe Text)
csr1VolumeId = lens _csr1VolumeId (\s a -> s { _csr1VolumeId = a })

-- | The size of the volume, in GiB.
csr1VolumeSize :: Lens' CreateSnapshotResponse (Maybe Int)
csr1VolumeSize = lens _csr1VolumeSize (\s a -> s { _csr1VolumeSize = a })

instance ToPath CreateSnapshot where
    toPath = const "/"

instance ToQuery CreateSnapshot

instance ToHeaders CreateSnapshot

instance AWSRequest CreateSnapshot where
    type Sv CreateSnapshot = EC2
    type Rs CreateSnapshot = CreateSnapshotResponse

    request  = post "CreateSnapshot"
    response = xmlResponse

instance FromXML CreateSnapshotResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateSnapshotResponse"
