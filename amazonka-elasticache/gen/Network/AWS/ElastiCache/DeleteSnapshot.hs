{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.DeleteSnapshot
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

-- | The /DeleteSnapshot/ action deletes an existing snapshot. When you
-- receive a successful response from this action, ElastiCache immediately
-- begins deleting the snapshot; you cannot cancel or revert this action.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DeleteSnapshot.html>
module Network.AWS.ElastiCache.DeleteSnapshot
    (
    -- * Request
      DeleteSnapshot
    -- ** Request constructor
    , deleteSnapshot
    -- ** Request lenses
    , delSnapshotName

    -- * Response
    , DeleteSnapshotResponse
    -- ** Response constructor
    , deleteSnapshotResponse
    -- ** Response lenses
    , dsrSnapshot
    , dsrStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DeleteSnapshot/ action.
--
-- /See:/ 'deleteSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delSnapshotName'
newtype DeleteSnapshot = DeleteSnapshot'
    { _delSnapshotName :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteSnapshot' smart constructor.
deleteSnapshot :: Text -> DeleteSnapshot
deleteSnapshot pSnapshotName =
    DeleteSnapshot'
    { _delSnapshotName = pSnapshotName
    }

-- | The name of the snapshot to be deleted.
delSnapshotName :: Lens' DeleteSnapshot Text
delSnapshotName = lens _delSnapshotName (\ s a -> s{_delSnapshotName = a});

instance AWSRequest DeleteSnapshot where
        type Sv DeleteSnapshot = ElastiCache
        type Rs DeleteSnapshot = DeleteSnapshotResponse
        request = post
        response
          = receiveXMLWrapper "DeleteSnapshotResult"
              (\ s h x ->
                 DeleteSnapshotResponse' <$>
                   (x .@? "Snapshot") <*> (pure (fromEnum s)))

instance ToHeaders DeleteSnapshot where
        toHeaders = const mempty

instance ToPath DeleteSnapshot where
        toPath = const "/"

instance ToQuery DeleteSnapshot where
        toQuery DeleteSnapshot'{..}
          = mconcat
              ["Action" =: ("DeleteSnapshot" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "SnapshotName" =: _delSnapshotName]

-- | /See:/ 'deleteSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrSnapshot'
--
-- * 'dsrStatus'
data DeleteSnapshotResponse = DeleteSnapshotResponse'
    { _dsrSnapshot :: !(Maybe Snapshot)
    , _dsrStatus   :: !Int
    } deriving (Eq,Read,Show)

-- | 'DeleteSnapshotResponse' smart constructor.
deleteSnapshotResponse :: Int -> DeleteSnapshotResponse
deleteSnapshotResponse pStatus =
    DeleteSnapshotResponse'
    { _dsrSnapshot = Nothing
    , _dsrStatus = pStatus
    }

-- | FIXME: Undocumented member.
dsrSnapshot :: Lens' DeleteSnapshotResponse (Maybe Snapshot)
dsrSnapshot = lens _dsrSnapshot (\ s a -> s{_dsrSnapshot = a});

-- | FIXME: Undocumented member.
dsrStatus :: Lens' DeleteSnapshotResponse Int
dsrStatus = lens _dsrStatus (\ s a -> s{_dsrStatus = a});
