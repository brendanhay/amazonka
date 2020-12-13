{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.CreateWorkspaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more WorkSpaces.
--
-- This operation is asynchronous and returns before the WorkSpaces are created.
module Network.AWS.WorkSpaces.CreateWorkspaces
  ( -- * Creating a request
    CreateWorkspaces (..),
    mkCreateWorkspaces,

    -- ** Request lenses
    cwWorkspaces,

    -- * Destructuring the response
    CreateWorkspacesResponse (..),
    mkCreateWorkspacesResponse,

    -- ** Response lenses
    cwrsFailedRequests,
    cwrsPendingRequests,
    cwrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkCreateWorkspaces' smart constructor.
newtype CreateWorkspaces = CreateWorkspaces'
  { -- | The WorkSpaces to create. You can specify up to 25 WorkSpaces.
    workspaces :: Lude.NonEmpty WorkspaceRequest
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateWorkspaces' with the minimum fields required to make a request.
--
-- * 'workspaces' - The WorkSpaces to create. You can specify up to 25 WorkSpaces.
mkCreateWorkspaces ::
  -- | 'workspaces'
  Lude.NonEmpty WorkspaceRequest ->
  CreateWorkspaces
mkCreateWorkspaces pWorkspaces_ =
  CreateWorkspaces' {workspaces = pWorkspaces_}

-- | The WorkSpaces to create. You can specify up to 25 WorkSpaces.
--
-- /Note:/ Consider using 'workspaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwWorkspaces :: Lens.Lens' CreateWorkspaces (Lude.NonEmpty WorkspaceRequest)
cwWorkspaces = Lens.lens (workspaces :: CreateWorkspaces -> Lude.NonEmpty WorkspaceRequest) (\s a -> s {workspaces = a} :: CreateWorkspaces)
{-# DEPRECATED cwWorkspaces "Use generic-lens or generic-optics with 'workspaces' instead." #-}

instance Lude.AWSRequest CreateWorkspaces where
  type Rs CreateWorkspaces = CreateWorkspacesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateWorkspacesResponse'
            Lude.<$> (x Lude..?> "FailedRequests" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "PendingRequests" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateWorkspaces where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.CreateWorkspaces" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateWorkspaces where
  toJSON CreateWorkspaces' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Workspaces" Lude..= workspaces)])

instance Lude.ToPath CreateWorkspaces where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateWorkspaces where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateWorkspacesResponse' smart constructor.
data CreateWorkspacesResponse = CreateWorkspacesResponse'
  { -- | Information about the WorkSpaces that could not be created.
    failedRequests :: Lude.Maybe [FailedCreateWorkspaceRequest],
    -- | Information about the WorkSpaces that were created.
    --
    -- Because this operation is asynchronous, the identifier returned is not immediately available for use with other operations. For example, if you call 'DescribeWorkspaces' before the WorkSpace is created, the information returned can be incomplete.
    pendingRequests :: Lude.Maybe [Workspace],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateWorkspacesResponse' with the minimum fields required to make a request.
--
-- * 'failedRequests' - Information about the WorkSpaces that could not be created.
-- * 'pendingRequests' - Information about the WorkSpaces that were created.
--
-- Because this operation is asynchronous, the identifier returned is not immediately available for use with other operations. For example, if you call 'DescribeWorkspaces' before the WorkSpace is created, the information returned can be incomplete.
-- * 'responseStatus' - The response status code.
mkCreateWorkspacesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateWorkspacesResponse
mkCreateWorkspacesResponse pResponseStatus_ =
  CreateWorkspacesResponse'
    { failedRequests = Lude.Nothing,
      pendingRequests = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the WorkSpaces that could not be created.
--
-- /Note:/ Consider using 'failedRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwrsFailedRequests :: Lens.Lens' CreateWorkspacesResponse (Lude.Maybe [FailedCreateWorkspaceRequest])
cwrsFailedRequests = Lens.lens (failedRequests :: CreateWorkspacesResponse -> Lude.Maybe [FailedCreateWorkspaceRequest]) (\s a -> s {failedRequests = a} :: CreateWorkspacesResponse)
{-# DEPRECATED cwrsFailedRequests "Use generic-lens or generic-optics with 'failedRequests' instead." #-}

-- | Information about the WorkSpaces that were created.
--
-- Because this operation is asynchronous, the identifier returned is not immediately available for use with other operations. For example, if you call 'DescribeWorkspaces' before the WorkSpace is created, the information returned can be incomplete.
--
-- /Note:/ Consider using 'pendingRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwrsPendingRequests :: Lens.Lens' CreateWorkspacesResponse (Lude.Maybe [Workspace])
cwrsPendingRequests = Lens.lens (pendingRequests :: CreateWorkspacesResponse -> Lude.Maybe [Workspace]) (\s a -> s {pendingRequests = a} :: CreateWorkspacesResponse)
{-# DEPRECATED cwrsPendingRequests "Use generic-lens or generic-optics with 'pendingRequests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwrsResponseStatus :: Lens.Lens' CreateWorkspacesResponse Lude.Int
cwrsResponseStatus = Lens.lens (responseStatus :: CreateWorkspacesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateWorkspacesResponse)
{-# DEPRECATED cwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
