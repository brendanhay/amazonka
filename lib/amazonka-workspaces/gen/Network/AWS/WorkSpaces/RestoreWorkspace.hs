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
-- Module      : Network.AWS.WorkSpaces.RestoreWorkspace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores the specified WorkSpace to its last known healthy state.
--
--
-- You cannot restore a WorkSpace unless its state is @AVAILABLE@ , @ERROR@ , @UNHEALTHY@ , or @STOPPED@ .
--
-- Restoring a WorkSpace is a potentially destructive action that can result in the loss of data. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/restore-workspace.html Restore a WorkSpace> .
--
-- This operation is asynchronous and returns before the WorkSpace is completely restored.
module Network.AWS.WorkSpaces.RestoreWorkspace
  ( -- * Creating a Request
    restoreWorkspace,
    RestoreWorkspace,

    -- * Request Lenses
    rwWorkspaceId,

    -- * Destructuring the Response
    restoreWorkspaceResponse,
    RestoreWorkspaceResponse,

    -- * Response Lenses
    resrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'restoreWorkspace' smart constructor.
newtype RestoreWorkspace = RestoreWorkspace'
  { _rwWorkspaceId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RestoreWorkspace' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rwWorkspaceId' - The identifier of the WorkSpace.
restoreWorkspace ::
  -- | 'rwWorkspaceId'
  Text ->
  RestoreWorkspace
restoreWorkspace pWorkspaceId_ =
  RestoreWorkspace' {_rwWorkspaceId = pWorkspaceId_}

-- | The identifier of the WorkSpace.
rwWorkspaceId :: Lens' RestoreWorkspace Text
rwWorkspaceId = lens _rwWorkspaceId (\s a -> s {_rwWorkspaceId = a})

instance AWSRequest RestoreWorkspace where
  type Rs RestoreWorkspace = RestoreWorkspaceResponse
  request = postJSON workSpaces
  response =
    receiveEmpty
      (\s h x -> RestoreWorkspaceResponse' <$> (pure (fromEnum s)))

instance Hashable RestoreWorkspace

instance NFData RestoreWorkspace

instance ToHeaders RestoreWorkspace where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkspacesService.RestoreWorkspace" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RestoreWorkspace where
  toJSON RestoreWorkspace' {..} =
    object (catMaybes [Just ("WorkspaceId" .= _rwWorkspaceId)])

instance ToPath RestoreWorkspace where
  toPath = const "/"

instance ToQuery RestoreWorkspace where
  toQuery = const mempty

-- | /See:/ 'restoreWorkspaceResponse' smart constructor.
newtype RestoreWorkspaceResponse = RestoreWorkspaceResponse'
  { _resrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RestoreWorkspaceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'resrsResponseStatus' - -- | The response status code.
restoreWorkspaceResponse ::
  -- | 'resrsResponseStatus'
  Int ->
  RestoreWorkspaceResponse
restoreWorkspaceResponse pResponseStatus_ =
  RestoreWorkspaceResponse'
    { _resrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
resrsResponseStatus :: Lens' RestoreWorkspaceResponse Int
resrsResponseStatus = lens _resrsResponseStatus (\s a -> s {_resrsResponseStatus = a})

instance NFData RestoreWorkspaceResponse
