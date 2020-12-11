{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.StartWorkspaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified WorkSpaces.
--
-- You cannot start a WorkSpace unless it has a running mode of @AutoStop@ and a state of @STOPPED@ .
module Network.AWS.WorkSpaces.StartWorkspaces
  ( -- * Creating a request
    StartWorkspaces (..),
    mkStartWorkspaces,

    -- ** Request lenses
    swStartWorkspaceRequests,

    -- * Destructuring the response
    StartWorkspacesResponse (..),
    mkStartWorkspacesResponse,

    -- ** Response lenses
    swrsFailedRequests,
    swrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkStartWorkspaces' smart constructor.
newtype StartWorkspaces = StartWorkspaces'
  { startWorkspaceRequests ::
      Lude.NonEmpty StartRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartWorkspaces' with the minimum fields required to make a request.
--
-- * 'startWorkspaceRequests' - The WorkSpaces to start. You can specify up to 25 WorkSpaces.
mkStartWorkspaces ::
  -- | 'startWorkspaceRequests'
  Lude.NonEmpty StartRequest ->
  StartWorkspaces
mkStartWorkspaces pStartWorkspaceRequests_ =
  StartWorkspaces'
    { startWorkspaceRequests =
        pStartWorkspaceRequests_
    }

-- | The WorkSpaces to start. You can specify up to 25 WorkSpaces.
--
-- /Note:/ Consider using 'startWorkspaceRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swStartWorkspaceRequests :: Lens.Lens' StartWorkspaces (Lude.NonEmpty StartRequest)
swStartWorkspaceRequests = Lens.lens (startWorkspaceRequests :: StartWorkspaces -> Lude.NonEmpty StartRequest) (\s a -> s {startWorkspaceRequests = a} :: StartWorkspaces)
{-# DEPRECATED swStartWorkspaceRequests "Use generic-lens or generic-optics with 'startWorkspaceRequests' instead." #-}

instance Lude.AWSRequest StartWorkspaces where
  type Rs StartWorkspaces = StartWorkspacesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartWorkspacesResponse'
            Lude.<$> (x Lude..?> "FailedRequests" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartWorkspaces where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.StartWorkspaces" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartWorkspaces where
  toJSON StartWorkspaces' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("StartWorkspaceRequests" Lude..= startWorkspaceRequests)
          ]
      )

instance Lude.ToPath StartWorkspaces where
  toPath = Lude.const "/"

instance Lude.ToQuery StartWorkspaces where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartWorkspacesResponse' smart constructor.
data StartWorkspacesResponse = StartWorkspacesResponse'
  { failedRequests ::
      Lude.Maybe [FailedWorkspaceChangeRequest],
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

-- | Creates a value of 'StartWorkspacesResponse' with the minimum fields required to make a request.
--
-- * 'failedRequests' - Information about the WorkSpaces that could not be started.
-- * 'responseStatus' - The response status code.
mkStartWorkspacesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartWorkspacesResponse
mkStartWorkspacesResponse pResponseStatus_ =
  StartWorkspacesResponse'
    { failedRequests = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the WorkSpaces that could not be started.
--
-- /Note:/ Consider using 'failedRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrsFailedRequests :: Lens.Lens' StartWorkspacesResponse (Lude.Maybe [FailedWorkspaceChangeRequest])
swrsFailedRequests = Lens.lens (failedRequests :: StartWorkspacesResponse -> Lude.Maybe [FailedWorkspaceChangeRequest]) (\s a -> s {failedRequests = a} :: StartWorkspacesResponse)
{-# DEPRECATED swrsFailedRequests "Use generic-lens or generic-optics with 'failedRequests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrsResponseStatus :: Lens.Lens' StartWorkspacesResponse Lude.Int
swrsResponseStatus = Lens.lens (responseStatus :: StartWorkspacesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartWorkspacesResponse)
{-# DEPRECATED swrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
