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
    , createSnapshot
    -- ** Request lenses
    , cstVolumeId
    , cstDescription

    -- * Response
    , CreateSnapshotResponse
    -- ** Response lenses
    , svEncrypted
    , svStartTime
    , svVolumeSize
    , svState
    , svSnapshotId
    , svVolumeId
    , svProgress
    , svOwnerId
    , svDescription
    , svOwnerAlias
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateSnapshot' request.
createSnapshot :: Text -- ^ 'cstVolumeId'
               -> CreateSnapshot
createSnapshot p1 = CreateSnapshot
    { _cstVolumeId = p1
    , _cstDescription = Nothing
    }

data CreateSnapshot = CreateSnapshot
    { _cstVolumeId :: Text
      -- ^ The ID of the Amazon EBS volume.
    , _cstDescription :: Maybe Text
      -- ^ A description for the snapshot.
    } deriving (Show, Generic)

-- | The ID of the Amazon EBS volume.
cstVolumeId
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateSnapshot
    -> f CreateSnapshot
cstVolumeId f x =
    (\y -> x { _cstVolumeId = y })
       <$> f (_cstVolumeId x)
{-# INLINE cstVolumeId #-}

-- | A description for the snapshot.
cstDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateSnapshot
    -> f CreateSnapshot
cstDescription f x =
    (\y -> x { _cstDescription = y })
       <$> f (_cstDescription x)
{-# INLINE cstDescription #-}

instance ToQuery CreateSnapshot where
    toQuery = genericQuery def

data CreateSnapshotResponse = CreateSnapshotResponse
    { _svEncrypted :: Maybe Bool
      -- ^ Indicates whether the snapshot is encrypted.
    , _svStartTime :: Maybe ISO8601
      -- ^ The time stamp when the snapshot was initiated.
    , _svVolumeSize :: Maybe Integer
      -- ^ The size of the volume, in GiB.
    , _svState :: Maybe SnapshotState
      -- ^ The snapshot state.
    , _svSnapshotId :: Maybe Text
      -- ^ The ID of the snapshot.
    , _svVolumeId :: Maybe Text
      -- ^ The ID of the volume.
    , _svProgress :: Maybe Text
      -- ^ The progress of the snapshot, as a percentage.
    , _svOwnerId :: Maybe Text
      -- ^ The AWS account ID of the Amazon EBS snapshot owner.
    , _svDescription :: Maybe Text
      -- ^ The description for the snapshot.
    , _svOwnerAlias :: Maybe Text
      -- ^ The AWS account alias (for example, amazon, self) or AWS account
      -- ID that owns the snapshot.
    } deriving (Show, Generic)

-- | Indicates whether the snapshot is encrypted.
svEncrypted
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CreateSnapshotResponse
    -> f CreateSnapshotResponse
svEncrypted f x =
    (\y -> x { _svEncrypted = y })
       <$> f (_svEncrypted x)
{-# INLINE svEncrypted #-}

-- | The time stamp when the snapshot was initiated.
svStartTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> CreateSnapshotResponse
    -> f CreateSnapshotResponse
svStartTime f x =
    (\y -> x { _svStartTime = y })
       <$> f (_svStartTime x)
{-# INLINE svStartTime #-}

-- | The size of the volume, in GiB.
svVolumeSize
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> CreateSnapshotResponse
    -> f CreateSnapshotResponse
svVolumeSize f x =
    (\y -> x { _svVolumeSize = y })
       <$> f (_svVolumeSize x)
{-# INLINE svVolumeSize #-}

-- | The snapshot state.
svState
    :: Functor f
    => (Maybe SnapshotState
    -> f (Maybe SnapshotState))
    -> CreateSnapshotResponse
    -> f CreateSnapshotResponse
svState f x =
    (\y -> x { _svState = y })
       <$> f (_svState x)
{-# INLINE svState #-}

-- | The ID of the snapshot.
svSnapshotId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateSnapshotResponse
    -> f CreateSnapshotResponse
svSnapshotId f x =
    (\y -> x { _svSnapshotId = y })
       <$> f (_svSnapshotId x)
{-# INLINE svSnapshotId #-}

-- | The ID of the volume.
svVolumeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateSnapshotResponse
    -> f CreateSnapshotResponse
svVolumeId f x =
    (\y -> x { _svVolumeId = y })
       <$> f (_svVolumeId x)
{-# INLINE svVolumeId #-}

-- | The progress of the snapshot, as a percentage.
svProgress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateSnapshotResponse
    -> f CreateSnapshotResponse
svProgress f x =
    (\y -> x { _svProgress = y })
       <$> f (_svProgress x)
{-# INLINE svProgress #-}

-- | The AWS account ID of the Amazon EBS snapshot owner.
svOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateSnapshotResponse
    -> f CreateSnapshotResponse
svOwnerId f x =
    (\y -> x { _svOwnerId = y })
       <$> f (_svOwnerId x)
{-# INLINE svOwnerId #-}

-- | The description for the snapshot.
svDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateSnapshotResponse
    -> f CreateSnapshotResponse
svDescription f x =
    (\y -> x { _svDescription = y })
       <$> f (_svDescription x)
{-# INLINE svDescription #-}

-- | The AWS account alias (for example, amazon, self) or AWS account ID that
-- owns the snapshot.
svOwnerAlias
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateSnapshotResponse
    -> f CreateSnapshotResponse
svOwnerAlias f x =
    (\y -> x { _svOwnerAlias = y })
       <$> f (_svOwnerAlias x)
{-# INLINE svOwnerAlias #-}

instance FromXML CreateSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateSnapshot where
    type Sv CreateSnapshot = EC2
    type Rs CreateSnapshot = CreateSnapshotResponse

    request = post "CreateSnapshot"
    response _ = xmlResponse
