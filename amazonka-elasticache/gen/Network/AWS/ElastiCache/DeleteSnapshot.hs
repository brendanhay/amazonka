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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The /DeleteSnapshot/ action deletes an existing snapshot. When you receive a successful response from this action, ElastiCache immediately begins deleting the snapshot; you cannot cancel or revert this action.
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
    , dsrsResponseStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.ElastiCache.Types.Product
import           Network.AWS.Lens
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

instance Hashable DeleteSnapshot

instance NFData DeleteSnapshot

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
    { _dsrsSnapshot       :: !(Maybe Snapshot)
    , _dsrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsSnapshot'
--
-- * 'dsrsResponseStatus'
deleteSnapshotResponse
    :: Int -- ^ 'dsrsResponseStatus'
    -> DeleteSnapshotResponse
deleteSnapshotResponse pResponseStatus_ =
    DeleteSnapshotResponse'
    { _dsrsSnapshot = Nothing
    , _dsrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dsrsSnapshot :: Lens' DeleteSnapshotResponse (Maybe Snapshot)
dsrsSnapshot = lens _dsrsSnapshot (\ s a -> s{_dsrsSnapshot = a});

-- | The response status code.
dsrsResponseStatus :: Lens' DeleteSnapshotResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\ s a -> s{_dsrsResponseStatus = a});

instance NFData DeleteSnapshotResponse
