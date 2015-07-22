{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeleteSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a directory snapshot.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DeleteSnapshot.html>
module Network.AWS.DirectoryService.DeleteSnapshot
    (
    -- * Request
      DeleteSnapshot
    -- ** Request constructor
    , deleteSnapshot
    -- ** Request lenses
    , dsrqSnapshotId

    -- * Response
    , DeleteSnapshotResponse
    -- ** Response constructor
    , deleteSnapshotResponse
    -- ** Response lenses
    , dsrsSnapshotId
    , dsrsStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the DeleteSnapshot operation.
--
-- /See:/ 'deleteSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrqSnapshotId'
newtype DeleteSnapshot = DeleteSnapshot'
    { _dsrqSnapshotId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSnapshot' smart constructor.
deleteSnapshot :: Text -> DeleteSnapshot
deleteSnapshot pSnapshotId =
    DeleteSnapshot'
    { _dsrqSnapshotId = pSnapshotId
    }

-- | The identifier of the directory snapshot to be deleted.
dsrqSnapshotId :: Lens' DeleteSnapshot Text
dsrqSnapshotId = lens _dsrqSnapshotId (\ s a -> s{_dsrqSnapshotId = a});

instance AWSRequest DeleteSnapshot where
        type Sv DeleteSnapshot = DirectoryService
        type Rs DeleteSnapshot = DeleteSnapshotResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteSnapshotResponse' <$>
                   (x .?> "SnapshotId") <*> (pure (fromEnum s)))

instance ToHeaders DeleteSnapshot where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.DeleteSnapshot" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteSnapshot where
        toJSON DeleteSnapshot'{..}
          = object ["SnapshotId" .= _dsrqSnapshotId]

instance ToPath DeleteSnapshot where
        toPath = const "/"

instance ToQuery DeleteSnapshot where
        toQuery = const mempty

-- | Contains the results of the DeleteSnapshot operation.
--
-- /See:/ 'deleteSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrsSnapshotId'
--
-- * 'dsrsStatus'
data DeleteSnapshotResponse = DeleteSnapshotResponse'
    { _dsrsSnapshotId :: !(Maybe Text)
    , _dsrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSnapshotResponse' smart constructor.
deleteSnapshotResponse :: Int -> DeleteSnapshotResponse
deleteSnapshotResponse pStatus =
    DeleteSnapshotResponse'
    { _dsrsSnapshotId = Nothing
    , _dsrsStatus = pStatus
    }

-- | The identifier of the directory snapshot that was deleted.
dsrsSnapshotId :: Lens' DeleteSnapshotResponse (Maybe Text)
dsrsSnapshotId = lens _dsrsSnapshotId (\ s a -> s{_dsrsSnapshotId = a});

-- | FIXME: Undocumented member.
dsrsStatus :: Lens' DeleteSnapshotResponse Int
dsrsStatus = lens _dsrsStatus (\ s a -> s{_dsrsStatus = a});
