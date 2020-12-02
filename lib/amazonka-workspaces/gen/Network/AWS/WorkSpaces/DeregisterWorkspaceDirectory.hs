{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DeregisterWorkspaceDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified directory. This operation is asynchronous and returns before the WorkSpace directory is deregistered. If any WorkSpaces are registered to this directory, you must remove them before you can deregister the directory.
module Network.AWS.WorkSpaces.DeregisterWorkspaceDirectory
  ( -- * Creating a Request
    deregisterWorkspaceDirectory,
    DeregisterWorkspaceDirectory,

    -- * Request Lenses
    dwdDirectoryId,

    -- * Destructuring the Response
    deregisterWorkspaceDirectoryResponse,
    DeregisterWorkspaceDirectoryResponse,

    -- * Response Lenses
    drsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'deregisterWorkspaceDirectory' smart constructor.
newtype DeregisterWorkspaceDirectory = DeregisterWorkspaceDirectory'
  { _dwdDirectoryId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeregisterWorkspaceDirectory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwdDirectoryId' - The identifier of the directory. If any WorkSpaces are registered to this directory, you must remove them before you deregister the directory, or you will receive an OperationNotSupportedException error.
deregisterWorkspaceDirectory ::
  -- | 'dwdDirectoryId'
  Text ->
  DeregisterWorkspaceDirectory
deregisterWorkspaceDirectory pDirectoryId_ =
  DeregisterWorkspaceDirectory' {_dwdDirectoryId = pDirectoryId_}

-- | The identifier of the directory. If any WorkSpaces are registered to this directory, you must remove them before you deregister the directory, or you will receive an OperationNotSupportedException error.
dwdDirectoryId :: Lens' DeregisterWorkspaceDirectory Text
dwdDirectoryId = lens _dwdDirectoryId (\s a -> s {_dwdDirectoryId = a})

instance AWSRequest DeregisterWorkspaceDirectory where
  type
    Rs DeregisterWorkspaceDirectory =
      DeregisterWorkspaceDirectoryResponse
  request = postJSON workSpaces
  response =
    receiveEmpty
      ( \s h x ->
          DeregisterWorkspaceDirectoryResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeregisterWorkspaceDirectory

instance NFData DeregisterWorkspaceDirectory

instance ToHeaders DeregisterWorkspaceDirectory where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkspacesService.DeregisterWorkspaceDirectory" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeregisterWorkspaceDirectory where
  toJSON DeregisterWorkspaceDirectory' {..} =
    object (catMaybes [Just ("DirectoryId" .= _dwdDirectoryId)])

instance ToPath DeregisterWorkspaceDirectory where
  toPath = const "/"

instance ToQuery DeregisterWorkspaceDirectory where
  toQuery = const mempty

-- | /See:/ 'deregisterWorkspaceDirectoryResponse' smart constructor.
newtype DeregisterWorkspaceDirectoryResponse = DeregisterWorkspaceDirectoryResponse'
  { _drsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeregisterWorkspaceDirectoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deregisterWorkspaceDirectoryResponse ::
  -- | 'drsResponseStatus'
  Int ->
  DeregisterWorkspaceDirectoryResponse
deregisterWorkspaceDirectoryResponse pResponseStatus_ =
  DeregisterWorkspaceDirectoryResponse'
    { _drsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
drsResponseStatus :: Lens' DeregisterWorkspaceDirectoryResponse Int
drsResponseStatus = lens _drsResponseStatus (\s a -> s {_drsResponseStatus = a})

instance NFData DeregisterWorkspaceDirectoryResponse
