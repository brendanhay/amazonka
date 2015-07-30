{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DeleteFileSystem
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a file system, permanently severing access to its contents. Upon
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
module Network.AWS.EFS.DeleteFileSystem
    (
    -- * Request
      DeleteFileSystem
    -- ** Request constructor
    , deleteFileSystem
    -- ** Request lenses
    , delFileSystemId

    -- * Response
    , DeleteFileSystemResponse
    -- ** Response constructor
    , deleteFileSystemResponse
    ) where

import           Network.AWS.EFS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteFileSystem' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delFileSystemId'
newtype DeleteFileSystem = DeleteFileSystem'
    { _delFileSystemId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteFileSystem' smart constructor.
deleteFileSystem :: Text -> DeleteFileSystem
deleteFileSystem pFileSystemId_ =
    DeleteFileSystem'
    { _delFileSystemId = pFileSystemId_
    }

-- | The ID of the file system you want to delete.
delFileSystemId :: Lens' DeleteFileSystem Text
delFileSystemId = lens _delFileSystemId (\ s a -> s{_delFileSystemId = a});

instance AWSRequest DeleteFileSystem where
        type Sv DeleteFileSystem = EFS
        type Rs DeleteFileSystem = DeleteFileSystemResponse
        request = delete
        response = receiveNull DeleteFileSystemResponse'

instance ToHeaders DeleteFileSystem where
        toHeaders = const mempty

instance ToPath DeleteFileSystem where
        toPath DeleteFileSystem'{..}
          = mconcat
              ["/2015-02-01/file-systems/", toBS _delFileSystemId]

instance ToQuery DeleteFileSystem where
        toQuery = const mempty

-- | /See:/ 'deleteFileSystemResponse' smart constructor.
data DeleteFileSystemResponse =
    DeleteFileSystemResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteFileSystemResponse' smart constructor.
deleteFileSystemResponse :: DeleteFileSystemResponse
deleteFileSystemResponse = DeleteFileSystemResponse'
