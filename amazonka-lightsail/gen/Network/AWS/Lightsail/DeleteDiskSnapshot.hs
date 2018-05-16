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
-- Module      : Network.AWS.Lightsail.DeleteDiskSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified disk snapshot.
--
--
-- When you make periodic snapshots of a disk, the snapshots are incremental, and only the blocks on the device that have changed since your last snapshot are saved in the new snapshot. When you delete a snapshot, only the data not needed for any other snapshot is removed. So regardless of which prior snapshots have been deleted, all active snapshots will have access to all the information needed to restore the disk.
--
module Network.AWS.Lightsail.DeleteDiskSnapshot
    (
    -- * Creating a Request
      deleteDiskSnapshot
    , DeleteDiskSnapshot
    -- * Request Lenses
    , ddsDiskSnapshotName

    -- * Destructuring the Response
    , deleteDiskSnapshotResponse
    , DeleteDiskSnapshotResponse
    -- * Response Lenses
    , ddsrsOperations
    , ddsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDiskSnapshot' smart constructor.
newtype DeleteDiskSnapshot = DeleteDiskSnapshot'
  { _ddsDiskSnapshotName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDiskSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsDiskSnapshotName' - The name of the disk snapshot you want to delete (e.g., @my-disk-snapshot@ ).
deleteDiskSnapshot
    :: Text -- ^ 'ddsDiskSnapshotName'
    -> DeleteDiskSnapshot
deleteDiskSnapshot pDiskSnapshotName_ =
  DeleteDiskSnapshot' {_ddsDiskSnapshotName = pDiskSnapshotName_}


-- | The name of the disk snapshot you want to delete (e.g., @my-disk-snapshot@ ).
ddsDiskSnapshotName :: Lens' DeleteDiskSnapshot Text
ddsDiskSnapshotName = lens _ddsDiskSnapshotName (\ s a -> s{_ddsDiskSnapshotName = a})

instance AWSRequest DeleteDiskSnapshot where
        type Rs DeleteDiskSnapshot =
             DeleteDiskSnapshotResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 DeleteDiskSnapshotResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DeleteDiskSnapshot where

instance NFData DeleteDiskSnapshot where

instance ToHeaders DeleteDiskSnapshot where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.DeleteDiskSnapshot" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDiskSnapshot where
        toJSON DeleteDiskSnapshot'{..}
          = object
              (catMaybes
                 [Just ("diskSnapshotName" .= _ddsDiskSnapshotName)])

instance ToPath DeleteDiskSnapshot where
        toPath = const "/"

instance ToQuery DeleteDiskSnapshot where
        toQuery = const mempty

-- | /See:/ 'deleteDiskSnapshotResponse' smart constructor.
data DeleteDiskSnapshotResponse = DeleteDiskSnapshotResponse'
  { _ddsrsOperations     :: !(Maybe [Operation])
  , _ddsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDiskSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsrsOperations' - An object describing the API operations.
--
-- * 'ddsrsResponseStatus' - -- | The response status code.
deleteDiskSnapshotResponse
    :: Int -- ^ 'ddsrsResponseStatus'
    -> DeleteDiskSnapshotResponse
deleteDiskSnapshotResponse pResponseStatus_ =
  DeleteDiskSnapshotResponse'
    {_ddsrsOperations = Nothing, _ddsrsResponseStatus = pResponseStatus_}


-- | An object describing the API operations.
ddsrsOperations :: Lens' DeleteDiskSnapshotResponse [Operation]
ddsrsOperations = lens _ddsrsOperations (\ s a -> s{_ddsrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
ddsrsResponseStatus :: Lens' DeleteDiskSnapshotResponse Int
ddsrsResponseStatus = lens _ddsrsResponseStatus (\ s a -> s{_ddsrsResponseStatus = a})

instance NFData DeleteDiskSnapshotResponse where
