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
-- Module      : Network.AWS.EFS.DeleteFileSystem
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a file system, permanently severing access to its contents. Upon return, the file system no longer exists and you can't access any contents of the deleted file system.
--
--
-- You can't delete a file system that is in use. That is, if the file system has any mount targets, you must first delete them. For more information, see 'DescribeMountTargets' and 'DeleteMountTarget' .
--
-- This operation requires permissions for the @elasticfilesystem:DeleteFileSystem@ action.
--
module Network.AWS.EFS.DeleteFileSystem
    (
    -- * Creating a Request
      deleteFileSystem
    , DeleteFileSystem
    -- * Request Lenses
    , delFileSystemId

    -- * Destructuring the Response
    , deleteFileSystemResponse
    , DeleteFileSystemResponse
    ) where

import Network.AWS.EFS.Types
import Network.AWS.EFS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'deleteFileSystem' smart constructor.
newtype DeleteFileSystem = DeleteFileSystem'
  { _delFileSystemId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFileSystem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delFileSystemId' - ID of the file system you want to delete.
deleteFileSystem
    :: Text -- ^ 'delFileSystemId'
    -> DeleteFileSystem
deleteFileSystem pFileSystemId_ =
  DeleteFileSystem' {_delFileSystemId = pFileSystemId_}


-- | ID of the file system you want to delete.
delFileSystemId :: Lens' DeleteFileSystem Text
delFileSystemId = lens _delFileSystemId (\ s a -> s{_delFileSystemId = a})

instance AWSRequest DeleteFileSystem where
        type Rs DeleteFileSystem = DeleteFileSystemResponse
        request = delete efs
        response = receiveNull DeleteFileSystemResponse'

instance Hashable DeleteFileSystem where

instance NFData DeleteFileSystem where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFileSystemResponse' with the minimum fields required to make a request.
--
deleteFileSystemResponse
    :: DeleteFileSystemResponse
deleteFileSystemResponse = DeleteFileSystemResponse'


instance NFData DeleteFileSystemResponse where
