{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.RestoreFromSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a directory using an existing directory snapshot.
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
-- /See:/ <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_RestoreFromSnapshot.html AWS API Reference> for RestoreFromSnapshot.
module Network.AWS.DirectoryService.RestoreFromSnapshot
    (
    -- * Creating a Request
      RestoreFromSnapshot
    , restoreFromSnapshot
    -- * Request Lenses
    , rfsSnapshotId

    -- * Destructuring the Response
    , RestoreFromSnapshotResponse
    , restoreFromSnapshotResponse
    -- * Response Lenses
    , rfsrsStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | An object representing the inputs for the RestoreFromSnapshot operation.
--
-- /See:/ 'restoreFromSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rfsSnapshotId'
newtype RestoreFromSnapshot = RestoreFromSnapshot'
    { _rfsSnapshotId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RestoreFromSnapshot' smart constructor.
restoreFromSnapshot :: Text -> RestoreFromSnapshot
restoreFromSnapshot pSnapshotId_ =
    RestoreFromSnapshot'
    { _rfsSnapshotId = pSnapshotId_
    }

-- | The identifier of the snapshot to restore from.
rfsSnapshotId :: Lens' RestoreFromSnapshot Text
rfsSnapshotId = lens _rfsSnapshotId (\ s a -> s{_rfsSnapshotId = a});

instance AWSRequest RestoreFromSnapshot where
        type Sv RestoreFromSnapshot = DirectoryService
        type Rs RestoreFromSnapshot =
             RestoreFromSnapshotResponse
        request = postJSON
        response
          = receiveEmpty
              (\ s h x ->
                 RestoreFromSnapshotResponse' <$> (pure (fromEnum s)))

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

-- | Contains the results of the RestoreFromSnapshot operation.
--
-- /See:/ 'restoreFromSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rfsrsStatus'
newtype RestoreFromSnapshotResponse = RestoreFromSnapshotResponse'
    { _rfsrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RestoreFromSnapshotResponse' smart constructor.
restoreFromSnapshotResponse :: Int -> RestoreFromSnapshotResponse
restoreFromSnapshotResponse pStatus_ =
    RestoreFromSnapshotResponse'
    { _rfsrsStatus = pStatus_
    }

-- | Undocumented member.
rfsrsStatus :: Lens' RestoreFromSnapshotResponse Int
rfsrsStatus = lens _rfsrsStatus (\ s a -> s{_rfsrsStatus = a});
