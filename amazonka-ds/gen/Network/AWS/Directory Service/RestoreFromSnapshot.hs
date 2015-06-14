{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Directory Service.RestoreFromSnapshot
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

-- | Restores a directory using an existing directory snapshot.
--
-- When you restore a directory from a snapshot, any changes made to the
-- directory after the snapshot date are overwritten.
--
-- This action returns as soon as the restore operation is initiated. You
-- can monitor the progress of the restore operation by calling the
-- DescribeDirectories operation with the directory identifier. When the
-- __DirectoryDescription.Stage__ value changes to @Active@, the restore
-- operation is complete.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_RestoreFromSnapshot.html>
module Network.AWS.Directory Service.RestoreFromSnapshot
    (
    -- * Request
      RestoreFromSnapshot
    -- ** Request constructor
    , restoreFromSnapshot
    -- ** Request lenses
    , rfsSnapshotId

    -- * Response
    , RestoreFromSnapshotResponse
    -- ** Response constructor
    , restoreFromSnapshotResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Directory Service.Types

-- | /See:/ 'restoreFromSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rfsSnapshotId'
newtype RestoreFromSnapshot = RestoreFromSnapshot'{_rfsSnapshotId :: Text} deriving (Eq, Read, Show)

-- | 'RestoreFromSnapshot' smart constructor.
restoreFromSnapshot :: Text -> RestoreFromSnapshot
restoreFromSnapshot pSnapshotId = RestoreFromSnapshot'{_rfsSnapshotId = pSnapshotId};

-- | The identifier of the snapshot to restore from.
rfsSnapshotId :: Lens' RestoreFromSnapshot Text
rfsSnapshotId = lens _rfsSnapshotId (\ s a -> s{_rfsSnapshotId = a});

instance AWSRequest RestoreFromSnapshot where
        type Sv RestoreFromSnapshot = Directory Service
        type Rs RestoreFromSnapshot =
             RestoreFromSnapshotResponse
        request = postJSON
        response = receiveNull RestoreFromSnapshotResponse'

instance ToHeaders RestoreFromSnapshot where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.RestoreFromSnapshot" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RestoreFromSnapshot where
        toJSON RestoreFromSnapshot'{..}
          = object ["SnapshotId" .= _rfsSnapshotId]

instance ToPath RestoreFromSnapshot where
        toPath = const "/"

instance ToQuery RestoreFromSnapshot where
        toQuery = const mempty

-- | /See:/ 'restoreFromSnapshotResponse' smart constructor.
data RestoreFromSnapshotResponse = RestoreFromSnapshotResponse' deriving (Eq, Read, Show)

-- | 'RestoreFromSnapshotResponse' smart constructor.
restoreFromSnapshotResponse :: RestoreFromSnapshotResponse
restoreFromSnapshotResponse = RestoreFromSnapshotResponse';
