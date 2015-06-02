{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a snapshot of an EBS volume and stores it in Amazon S3. You can use
-- snapshots for backups, to make copies of EBS volumes, and to save data before
-- shutting down an instance.
--
-- When a snapshot is created, any AWS Marketplace product codes that are
-- associated with the source volume are propagated to the snapshot.
--
-- You can take a snapshot of an attached volume that is in use. However,
-- snapshots only capture data that has been written to your EBS volume at the
-- time the snapshot command is issued; this may exclude any data that has been
-- cached by any applications or the operating system. If you can pause any file
-- systems on the volume long enough to take a snapshot, your snapshot should be
-- complete. However, if you cannot pause all file writes to the volume, you
-- should unmount the volume from within the instance, issue the snapshot
-- command, and then remount the volume to ensure a consistent and complete
-- snapshot. You may remount and use your volume while the snapshot status is 'pending'.
--
-- To create a snapshot for EBS volumes that serve as root devices, you should
-- stop the instance before taking the snapshot.
--
-- Snapshots that are taken from encrypted volumes are automatically encrypted.
-- Volumes that are created from encrypted snapshots are also automatically
-- encrypted. Your encrypted volumes and any associated snapshots always remain
-- protected.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AmazonEBS.html Amazon Elastic Block Store> and <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBSEncryption> in the /Amazon Elastic Compute Cloud User Guide/.
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
    , csr1KmsKeyId
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
    } deriving (Eq, Ord, Read, Show)

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

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
cs2DryRun :: Lens' CreateSnapshot (Maybe Bool)
cs2DryRun = lens _cs2DryRun (\s a -> s { _cs2DryRun = a })

-- | The ID of the EBS volume.
cs2VolumeId :: Lens' CreateSnapshot Text
cs2VolumeId = lens _cs2VolumeId (\s a -> s { _cs2VolumeId = a })

data CreateSnapshotResponse = CreateSnapshotResponse
    { _csr1Description :: Text
    , _csr1Encrypted   :: Bool
    , _csr1KmsKeyId    :: Maybe Text
    , _csr1OwnerAlias  :: Maybe Text
    , _csr1OwnerId     :: Text
    , _csr1Progress    :: Text
    , _csr1SnapshotId  :: Text
    , _csr1StartTime   :: ISO8601
    , _csr1State       :: SnapshotState
    , _csr1Tags        :: List "item" Tag
    , _csr1VolumeId    :: Text
    , _csr1VolumeSize  :: Int
    } deriving (Eq, Read, Show)

-- | 'CreateSnapshotResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csr1Description' @::@ 'Text'
--
-- * 'csr1Encrypted' @::@ 'Bool'
--
-- * 'csr1KmsKeyId' @::@ 'Maybe' 'Text'
--
-- * 'csr1OwnerAlias' @::@ 'Maybe' 'Text'
--
-- * 'csr1OwnerId' @::@ 'Text'
--
-- * 'csr1Progress' @::@ 'Text'
--
-- * 'csr1SnapshotId' @::@ 'Text'
--
-- * 'csr1StartTime' @::@ 'UTCTime'
--
-- * 'csr1State' @::@ 'SnapshotState'
--
-- * 'csr1Tags' @::@ ['Tag']
--
-- * 'csr1VolumeId' @::@ 'Text'
--
-- * 'csr1VolumeSize' @::@ 'Int'
--
createSnapshotResponse :: Text -- ^ 'csr1SnapshotId'
                       -> Text -- ^ 'csr1VolumeId'
                       -> SnapshotState -- ^ 'csr1State'
                       -> UTCTime -- ^ 'csr1StartTime'
                       -> Text -- ^ 'csr1Progress'
                       -> Text -- ^ 'csr1OwnerId'
                       -> Text -- ^ 'csr1Description'
                       -> Int -- ^ 'csr1VolumeSize'
                       -> Bool -- ^ 'csr1Encrypted'
                       -> CreateSnapshotResponse
createSnapshotResponse p1 p2 p3 p4 p5 p6 p7 p8 p9 = CreateSnapshotResponse
    { _csr1SnapshotId  = p1
    , _csr1VolumeId    = p2
    , _csr1State       = p3
    , _csr1StartTime   = withIso _Time (const id) p4
    , _csr1Progress    = p5
    , _csr1OwnerId     = p6
    , _csr1Description = p7
    , _csr1VolumeSize  = p8
    , _csr1Encrypted   = p9
    , _csr1OwnerAlias  = Nothing
    , _csr1Tags        = mempty
    , _csr1KmsKeyId    = Nothing
    }

-- | The description for the snapshot.
csr1Description :: Lens' CreateSnapshotResponse Text
csr1Description = lens _csr1Description (\s a -> s { _csr1Description = a })

-- | Indicates whether the snapshot is encrypted.
csr1Encrypted :: Lens' CreateSnapshotResponse Bool
csr1Encrypted = lens _csr1Encrypted (\s a -> s { _csr1Encrypted = a })

-- | The full ARN of the AWS Key Management Service (KMS) master key that was used
-- to protect the volume encryption key for the parent volume.
csr1KmsKeyId :: Lens' CreateSnapshotResponse (Maybe Text)
csr1KmsKeyId = lens _csr1KmsKeyId (\s a -> s { _csr1KmsKeyId = a })

-- | The AWS account alias (for example, 'amazon', 'self') or AWS account ID that owns
-- the snapshot.
csr1OwnerAlias :: Lens' CreateSnapshotResponse (Maybe Text)
csr1OwnerAlias = lens _csr1OwnerAlias (\s a -> s { _csr1OwnerAlias = a })

-- | The AWS account ID of the EBS snapshot owner.
csr1OwnerId :: Lens' CreateSnapshotResponse Text
csr1OwnerId = lens _csr1OwnerId (\s a -> s { _csr1OwnerId = a })

-- | The progress of the snapshot, as a percentage.
csr1Progress :: Lens' CreateSnapshotResponse Text
csr1Progress = lens _csr1Progress (\s a -> s { _csr1Progress = a })

-- | The ID of the snapshot.
csr1SnapshotId :: Lens' CreateSnapshotResponse Text
csr1SnapshotId = lens _csr1SnapshotId (\s a -> s { _csr1SnapshotId = a })

-- | The time stamp when the snapshot was initiated.
csr1StartTime :: Lens' CreateSnapshotResponse UTCTime
csr1StartTime = lens _csr1StartTime (\s a -> s { _csr1StartTime = a }) . _Time

-- | The snapshot state.
csr1State :: Lens' CreateSnapshotResponse SnapshotState
csr1State = lens _csr1State (\s a -> s { _csr1State = a })

-- | Any tags assigned to the snapshot.
csr1Tags :: Lens' CreateSnapshotResponse [Tag]
csr1Tags = lens _csr1Tags (\s a -> s { _csr1Tags = a }) . _List

-- | The ID of the volume.
csr1VolumeId :: Lens' CreateSnapshotResponse Text
csr1VolumeId = lens _csr1VolumeId (\s a -> s { _csr1VolumeId = a })

-- | The size of the volume, in GiB.
csr1VolumeSize :: Lens' CreateSnapshotResponse Int
csr1VolumeSize = lens _csr1VolumeSize (\s a -> s { _csr1VolumeSize = a })

instance ToPath CreateSnapshot where
    toPath = const "/"

instance ToQuery CreateSnapshot where
    toQuery CreateSnapshot{..} = mconcat
        [ "Description" =? _cs2Description
        , "DryRun"      =? _cs2DryRun
        , "VolumeId"    =? _cs2VolumeId
        ]

instance ToHeaders CreateSnapshot

instance AWSRequest CreateSnapshot where
    type Sv CreateSnapshot = EC2
    type Rs CreateSnapshot = CreateSnapshotResponse

    request  = post "CreateSnapshot"
    response = xmlResponse

instance FromXML CreateSnapshotResponse where
    parseXML x = CreateSnapshotResponse
        <$> x .@  "description"
        <*> x .@  "encrypted"
        <*> x .@? "kmsKeyId"
        <*> x .@? "ownerAlias"
        <*> x .@  "ownerId"
        <*> x .@  "progress"
        <*> x .@  "snapshotId"
        <*> x .@  "startTime"
        <*> x .@  "status"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@  "volumeId"
        <*> x .@  "volumeSize"
