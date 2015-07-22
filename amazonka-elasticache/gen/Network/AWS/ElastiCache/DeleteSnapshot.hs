{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /DeleteSnapshot/ action deletes an existing snapshot. When you
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
    , drqSnapshotName

    -- * Response
    , DeleteSnapshotResponse
    -- ** Response constructor
    , deleteSnapshotResponse
    -- ** Response lenses
    , dsrsSnapshot
    , dsrsStatus
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
-- * 'drqSnapshotName'
newtype DeleteSnapshot = DeleteSnapshot'
    { _drqSnapshotName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSnapshot' smart constructor.
deleteSnapshot :: Text -> DeleteSnapshot
deleteSnapshot pSnapshotName_ =
    DeleteSnapshot'
    { _drqSnapshotName = pSnapshotName_
    }

-- | The name of the snapshot to be deleted.
drqSnapshotName :: Lens' DeleteSnapshot Text
drqSnapshotName = lens _drqSnapshotName (\ s a -> s{_drqSnapshotName = a});

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
               "SnapshotName" =: _drqSnapshotName]

-- | /See:/ 'deleteSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrsSnapshot'
--
-- * 'dsrsStatus'
data DeleteSnapshotResponse = DeleteSnapshotResponse'
    { _dsrsSnapshot :: !(Maybe Snapshot)
    , _dsrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSnapshotResponse' smart constructor.
deleteSnapshotResponse :: Int -> DeleteSnapshotResponse
deleteSnapshotResponse pStatus_ =
    DeleteSnapshotResponse'
    { _dsrsSnapshot = Nothing
    , _dsrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
dsrsSnapshot :: Lens' DeleteSnapshotResponse (Maybe Snapshot)
dsrsSnapshot = lens _dsrsSnapshot (\ s a -> s{_dsrsSnapshot = a});

-- | FIXME: Undocumented member.
dsrsStatus :: Lens' DeleteSnapshotResponse Int
dsrsStatus = lens _dsrsStatus (\ s a -> s{_dsrsStatus = a});
