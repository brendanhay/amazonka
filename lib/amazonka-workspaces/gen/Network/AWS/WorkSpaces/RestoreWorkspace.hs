{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- You cannot restore a WorkSpace unless its state is @AVAILABLE@ , @ERROR@ , @UNHEALTHY@ , or @STOPPED@ .
-- Restoring a WorkSpace is a potentially destructive action that can result in the loss of data. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/restore-workspace.html Restore a WorkSpace> .
-- This operation is asynchronous and returns before the WorkSpace is completely restored.
module Network.AWS.WorkSpaces.RestoreWorkspace
  ( -- * Creating a request
    RestoreWorkspace (..),
    mkRestoreWorkspace,

    -- ** Request lenses
    rwWorkspaceId,

    -- * Destructuring the response
    RestoreWorkspaceResponse (..),
    mkRestoreWorkspaceResponse,

    -- ** Response lenses
    rwrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkRestoreWorkspace' smart constructor.
newtype RestoreWorkspace = RestoreWorkspace'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreWorkspace' with the minimum fields required to make a request.
--
-- * 'workspaceId' - The identifier of the WorkSpace.
mkRestoreWorkspace ::
  -- | 'workspaceId'
  Lude.Text ->
  RestoreWorkspace
mkRestoreWorkspace pWorkspaceId_ =
  RestoreWorkspace' {workspaceId = pWorkspaceId_}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwWorkspaceId :: Lens.Lens' RestoreWorkspace Lude.Text
rwWorkspaceId = Lens.lens (workspaceId :: RestoreWorkspace -> Lude.Text) (\s a -> s {workspaceId = a} :: RestoreWorkspace)
{-# DEPRECATED rwWorkspaceId "Use generic-lens or generic-optics with 'workspaceId' instead." #-}

instance Lude.AWSRequest RestoreWorkspace where
  type Rs RestoreWorkspace = RestoreWorkspaceResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RestoreWorkspaceResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RestoreWorkspace where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.RestoreWorkspace" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RestoreWorkspace where
  toJSON RestoreWorkspace' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("WorkspaceId" Lude..= workspaceId)])

instance Lude.ToPath RestoreWorkspace where
  toPath = Lude.const "/"

instance Lude.ToQuery RestoreWorkspace where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRestoreWorkspaceResponse' smart constructor.
newtype RestoreWorkspaceResponse = RestoreWorkspaceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreWorkspaceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRestoreWorkspaceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RestoreWorkspaceResponse
mkRestoreWorkspaceResponse pResponseStatus_ =
  RestoreWorkspaceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrsResponseStatus :: Lens.Lens' RestoreWorkspaceResponse Lude.Int
rwrsResponseStatus = Lens.lens (responseStatus :: RestoreWorkspaceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RestoreWorkspaceResponse)
{-# DEPRECATED rwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
