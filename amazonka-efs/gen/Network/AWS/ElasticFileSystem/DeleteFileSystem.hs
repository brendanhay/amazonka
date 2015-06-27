{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElasticFileSystem.DeleteFileSystem
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

-- | Deletes a file system, permanently severing access to its contents. Upon
-- return, the file system no longer exists and you will not be able to
-- access any contents of the deleted file system.
--
-- You cannot delete a file system that is in use. That is, if the file
-- system has any mount targets, you must first delete them. For more
-- information, see DescribeMountTargets and DeleteMountTarget.
--
-- The @DeleteFileSystem@ call returns while the file system state is still
-- \"deleting\". You can check the file system deletion status by calling
-- the DescribeFileSystems API, which returns a list of file systems in
-- your account. If you pass file system ID or creation token for the
-- deleted file system, the DescribeFileSystems will return a 404
-- \"FileSystemNotFound\" error.
--
-- This operation requires permission for the
-- @elasticfilesystem:DeleteFileSystem@ action.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DeleteFileSystem.html>
module Network.AWS.ElasticFileSystem.DeleteFileSystem
    (
    -- * Request
      DeleteFileSystem
    -- ** Request constructor
    , deleteFileSystem
    -- ** Request lenses
    , dFileSystemId

    -- * Response
    , DeleteFileSystemResponse
    -- ** Response constructor
    , deleteFileSystemResponse
    ) where

import           Network.AWS.ElasticFileSystem.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteFileSystem' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dFileSystemId'
newtype DeleteFileSystem = DeleteFileSystem'
    { _dFileSystemId :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteFileSystem' smart constructor.
deleteFileSystem :: Text -> DeleteFileSystem
deleteFileSystem pFileSystemId =
    DeleteFileSystem'
    { _dFileSystemId = pFileSystemId
    }

-- | The ID of the file system you want to delete.
dFileSystemId :: Lens' DeleteFileSystem Text
dFileSystemId = lens _dFileSystemId (\ s a -> s{_dFileSystemId = a});

instance AWSRequest DeleteFileSystem where
        type Sv DeleteFileSystem = ElasticFileSystem
        type Rs DeleteFileSystem = DeleteFileSystemResponse
        request = delete
        response = receiveNull DeleteFileSystemResponse'

instance ToHeaders DeleteFileSystem where
        toHeaders = const mempty

instance ToPath DeleteFileSystem where
        toPath DeleteFileSystem'{..}
          = mconcat
              ["/2015-02-01/file-systems/", toText _dFileSystemId]

instance ToQuery DeleteFileSystem where
        toQuery = const mempty

-- | /See:/ 'deleteFileSystemResponse' smart constructor.
data DeleteFileSystemResponse =
    DeleteFileSystemResponse'
    deriving (Eq,Read,Show)

-- | 'DeleteFileSystemResponse' smart constructor.
deleteFileSystemResponse :: DeleteFileSystemResponse
deleteFileSystemResponse = DeleteFileSystemResponse'
