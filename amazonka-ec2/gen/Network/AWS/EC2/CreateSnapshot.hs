{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
    , Snapshot
    -- ** Response constructor
    , snapshot
    -- ** Response lenses
    , sDescription
    , sEncrypted
    , sOwnerAlias
    , sOwnerId
    , sProgress
    , sSnapshotId
    , sStartTime
    , sState
    , sTags
    , sVolumeId
    , sVolumeSize
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

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
instance ToQuery CreateSnapshot

instance ToPath CreateSnapshot where
    toPath = const "/"

instance AWSRequest CreateSnapshot where
    type Sv CreateSnapshot = EC2
    type Rs CreateSnapshot = Snapshot

    request  = post "CreateSnapshot"
    response = xmlResponse $ const decodeCursor
