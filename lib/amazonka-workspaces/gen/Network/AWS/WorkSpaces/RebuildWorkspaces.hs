{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.RebuildWorkspaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rebuilds the specified WorkSpace.
--
-- You cannot rebuild a WorkSpace unless its state is @AVAILABLE@ , @ERROR@ , @UNHEALTHY@ , @STOPPED@ , or @REBOOTING@ .
-- Rebuilding a WorkSpace is a potentially destructive action that can result in the loss of data. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/reset-workspace.html Rebuild a WorkSpace> .
-- This operation is asynchronous and returns before the WorkSpaces have been completely rebuilt.
module Network.AWS.WorkSpaces.RebuildWorkspaces
  ( -- * Creating a request
    RebuildWorkspaces (..),
    mkRebuildWorkspaces,

    -- ** Request lenses
    rwRebuildWorkspaceRequests,

    -- * Destructuring the response
    RebuildWorkspacesResponse (..),
    mkRebuildWorkspacesResponse,

    -- ** Response lenses
    rwrsFailedRequests,
    rwrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkRebuildWorkspaces' smart constructor.
newtype RebuildWorkspaces = RebuildWorkspaces'
  { rebuildWorkspaceRequests ::
      Lude.NonEmpty RebuildRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebuildWorkspaces' with the minimum fields required to make a request.
--
-- * 'rebuildWorkspaceRequests' - The WorkSpace to rebuild. You can specify a single WorkSpace.
mkRebuildWorkspaces ::
  -- | 'rebuildWorkspaceRequests'
  Lude.NonEmpty RebuildRequest ->
  RebuildWorkspaces
mkRebuildWorkspaces pRebuildWorkspaceRequests_ =
  RebuildWorkspaces'
    { rebuildWorkspaceRequests =
        pRebuildWorkspaceRequests_
    }

-- | The WorkSpace to rebuild. You can specify a single WorkSpace.
--
-- /Note:/ Consider using 'rebuildWorkspaceRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwRebuildWorkspaceRequests :: Lens.Lens' RebuildWorkspaces (Lude.NonEmpty RebuildRequest)
rwRebuildWorkspaceRequests = Lens.lens (rebuildWorkspaceRequests :: RebuildWorkspaces -> Lude.NonEmpty RebuildRequest) (\s a -> s {rebuildWorkspaceRequests = a} :: RebuildWorkspaces)
{-# DEPRECATED rwRebuildWorkspaceRequests "Use generic-lens or generic-optics with 'rebuildWorkspaceRequests' instead." #-}

instance Lude.AWSRequest RebuildWorkspaces where
  type Rs RebuildWorkspaces = RebuildWorkspacesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          RebuildWorkspacesResponse'
            Lude.<$> (x Lude..?> "FailedRequests" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RebuildWorkspaces where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.RebuildWorkspaces" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RebuildWorkspaces where
  toJSON RebuildWorkspaces' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("RebuildWorkspaceRequests" Lude..= rebuildWorkspaceRequests)
          ]
      )

instance Lude.ToPath RebuildWorkspaces where
  toPath = Lude.const "/"

instance Lude.ToQuery RebuildWorkspaces where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRebuildWorkspacesResponse' smart constructor.
data RebuildWorkspacesResponse = RebuildWorkspacesResponse'
  { failedRequests ::
      Lude.Maybe
        [FailedWorkspaceChangeRequest],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebuildWorkspacesResponse' with the minimum fields required to make a request.
--
-- * 'failedRequests' - Information about the WorkSpace that could not be rebuilt.
-- * 'responseStatus' - The response status code.
mkRebuildWorkspacesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RebuildWorkspacesResponse
mkRebuildWorkspacesResponse pResponseStatus_ =
  RebuildWorkspacesResponse'
    { failedRequests = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the WorkSpace that could not be rebuilt.
--
-- /Note:/ Consider using 'failedRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrsFailedRequests :: Lens.Lens' RebuildWorkspacesResponse (Lude.Maybe [FailedWorkspaceChangeRequest])
rwrsFailedRequests = Lens.lens (failedRequests :: RebuildWorkspacesResponse -> Lude.Maybe [FailedWorkspaceChangeRequest]) (\s a -> s {failedRequests = a} :: RebuildWorkspacesResponse)
{-# DEPRECATED rwrsFailedRequests "Use generic-lens or generic-optics with 'failedRequests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrsResponseStatus :: Lens.Lens' RebuildWorkspacesResponse Lude.Int
rwrsResponseStatus = Lens.lens (responseStatus :: RebuildWorkspacesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RebuildWorkspacesResponse)
{-# DEPRECATED rwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
