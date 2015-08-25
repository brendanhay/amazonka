{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The /DeleteSnapshot/ action deletes an existing snapshot. When you
-- receive a successful response from this action, ElastiCache immediately
-- begins deleting the snapshot; you cannot cancel or revert this action.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DeleteSnapshot.html AWS API Reference> for DeleteSnapshot.
module Network.AWS.ElastiCache.DeleteSnapshot
    (
    -- * Creating a Request
      deleteSnapshot
    , DeleteSnapshot
    -- * Request Lenses
    , dSnapshotName

    -- * Destructuring the Response
    , deleteSnapshotResponse
    , DeleteSnapshotResponse
    -- * Response Lenses
    , dsrsSnapshot
    , dsrsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.ElastiCache.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DeleteSnapshot/ action.
--
-- /See:/ 'deleteSnapshot' smart constructor.
newtype DeleteSnapshot = DeleteSnapshot'
    { _dSnapshotName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dSnapshotName'
deleteSnapshot
    :: Text -- ^ 'dSnapshotName'
    -> DeleteSnapshot
deleteSnapshot pSnapshotName_ =
    DeleteSnapshot'
    { _dSnapshotName = pSnapshotName_
    }

-- | The name of the snapshot to be deleted.
dSnapshotName :: Lens' DeleteSnapshot Text
dSnapshotName = lens _dSnapshotName (\ s a -> s{_dSnapshotName = a});

instance AWSRequest DeleteSnapshot where
        type Rs DeleteSnapshot = DeleteSnapshotResponse
        request = postQuery elastiCache
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
               "SnapshotName" =: _dSnapshotName]

-- | /See:/ 'deleteSnapshotResponse' smart constructor.
data DeleteSnapshotResponse = DeleteSnapshotResponse'
    { _dsrsSnapshot :: !(Maybe Snapshot)
    , _dsrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsSnapshot'
--
-- * 'dsrsStatus'
deleteSnapshotResponse
    :: Int -- ^ 'dsrsStatus'
    -> DeleteSnapshotResponse
deleteSnapshotResponse pStatus_ =
    DeleteSnapshotResponse'
    { _dsrsSnapshot = Nothing
    , _dsrsStatus = pStatus_
    }

-- | Undocumented member.
dsrsSnapshot :: Lens' DeleteSnapshotResponse (Maybe Snapshot)
dsrsSnapshot = lens _dsrsSnapshot (\ s a -> s{_dsrsSnapshot = a});

-- | The response status code.
dsrsStatus :: Lens' DeleteSnapshotResponse Int
dsrsStatus = lens _dsrsStatus (\ s a -> s{_dsrsStatus = a});
