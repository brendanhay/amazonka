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

-- Module      : Network.AWS.EC2.DeleteSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified snapshot.
--
-- When you make periodic snapshots of a volume, the snapshots are incremental,
-- and only the blocks on the device that have changed since your last snapshot
-- are saved in the new snapshot. When you delete a snapshot, only the data not
-- needed for any other snapshot is removed. So regardless of which prior
-- snapshots have been deleted, all active snapshots will have access to all the
-- information needed to restore the volume.
--
-- You cannot delete a snapshot of the root device of an Amazon EBS volume used
-- by a registered AMI. You must first de-register the AMI before you can delete
-- the snapshot.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-deleting-snapshot.html Deleting an Amazon EBS Snapshot> in the /AmazonElastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSnapshot.html>
module Network.AWS.EC2.DeleteSnapshot
    (
    -- * Request
      DeleteSnapshot
    -- ** Request constructor
    , deleteSnapshot
    -- ** Request lenses
    , ds3DryRun
    , ds3SnapshotId

    -- * Response
    , DeleteSnapshotResponse
    -- ** Response constructor
    , deleteSnapshotResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteSnapshot = DeleteSnapshot
    { _ds3DryRun     :: Maybe Bool
    , _ds3SnapshotId :: Text
    } deriving (Eq, Ord, Show)

-- | 'DeleteSnapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ds3DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ds3SnapshotId' @::@ 'Text'
--
deleteSnapshot :: Text -- ^ 'ds3SnapshotId'
               -> DeleteSnapshot
deleteSnapshot p1 = DeleteSnapshot
    { _ds3SnapshotId = p1
    , _ds3DryRun     = Nothing
    }

ds3DryRun :: Lens' DeleteSnapshot (Maybe Bool)
ds3DryRun = lens _ds3DryRun (\s a -> s { _ds3DryRun = a })

-- | The ID of the Amazon EBS snapshot.
ds3SnapshotId :: Lens' DeleteSnapshot Text
ds3SnapshotId = lens _ds3SnapshotId (\s a -> s { _ds3SnapshotId = a })

data DeleteSnapshotResponse = DeleteSnapshotResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteSnapshotResponse' constructor.
deleteSnapshotResponse :: DeleteSnapshotResponse
deleteSnapshotResponse = DeleteSnapshotResponse

instance ToPath DeleteSnapshot where
    toPath = const "/"

instance ToQuery DeleteSnapshot where
    toQuery DeleteSnapshot{..} = mconcat
        [ "dryRun"     =? _ds3DryRun
        , "SnapshotId" =? _ds3SnapshotId
        ]

instance ToHeaders DeleteSnapshot

instance AWSRequest DeleteSnapshot where
    type Sv DeleteSnapshot = EC2
    type Rs DeleteSnapshot = DeleteSnapshotResponse

    request  = post "DeleteSnapshot"
    response = nullResponse DeleteSnapshotResponse
