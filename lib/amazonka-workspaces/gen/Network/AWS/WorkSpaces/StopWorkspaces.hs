{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.StopWorkspaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified WorkSpaces.
--
-- You cannot stop a WorkSpace unless it has a running mode of @AutoStop@ and a state of @AVAILABLE@ , @IMPAIRED@ , @UNHEALTHY@ , or @ERROR@ .
module Network.AWS.WorkSpaces.StopWorkspaces
  ( -- * Creating a request
    StopWorkspaces (..),
    mkStopWorkspaces,

    -- ** Request lenses
    swStopWorkspaceRequests,

    -- * Destructuring the response
    StopWorkspacesResponse (..),
    mkStopWorkspacesResponse,

    -- ** Response lenses
    srsFailedRequests,
    srsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkStopWorkspaces' smart constructor.
newtype StopWorkspaces = StopWorkspaces'
  { stopWorkspaceRequests ::
      Lude.NonEmpty StopRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopWorkspaces' with the minimum fields required to make a request.
--
-- * 'stopWorkspaceRequests' - The WorkSpaces to stop. You can specify up to 25 WorkSpaces.
mkStopWorkspaces ::
  -- | 'stopWorkspaceRequests'
  Lude.NonEmpty StopRequest ->
  StopWorkspaces
mkStopWorkspaces pStopWorkspaceRequests_ =
  StopWorkspaces' {stopWorkspaceRequests = pStopWorkspaceRequests_}

-- | The WorkSpaces to stop. You can specify up to 25 WorkSpaces.
--
-- /Note:/ Consider using 'stopWorkspaceRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swStopWorkspaceRequests :: Lens.Lens' StopWorkspaces (Lude.NonEmpty StopRequest)
swStopWorkspaceRequests = Lens.lens (stopWorkspaceRequests :: StopWorkspaces -> Lude.NonEmpty StopRequest) (\s a -> s {stopWorkspaceRequests = a} :: StopWorkspaces)
{-# DEPRECATED swStopWorkspaceRequests "Use generic-lens or generic-optics with 'stopWorkspaceRequests' instead." #-}

instance Lude.AWSRequest StopWorkspaces where
  type Rs StopWorkspaces = StopWorkspacesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopWorkspacesResponse'
            Lude.<$> (x Lude..?> "FailedRequests" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopWorkspaces where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.StopWorkspaces" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopWorkspaces where
  toJSON StopWorkspaces' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("StopWorkspaceRequests" Lude..= stopWorkspaceRequests)
          ]
      )

instance Lude.ToPath StopWorkspaces where
  toPath = Lude.const "/"

instance Lude.ToQuery StopWorkspaces where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopWorkspacesResponse' smart constructor.
data StopWorkspacesResponse = StopWorkspacesResponse'
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

-- | Creates a value of 'StopWorkspacesResponse' with the minimum fields required to make a request.
--
-- * 'failedRequests' - Information about the WorkSpaces that could not be stopped.
-- * 'responseStatus' - The response status code.
mkStopWorkspacesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopWorkspacesResponse
mkStopWorkspacesResponse pResponseStatus_ =
  StopWorkspacesResponse'
    { failedRequests = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the WorkSpaces that could not be stopped.
--
-- /Note:/ Consider using 'failedRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsFailedRequests :: Lens.Lens' StopWorkspacesResponse (Lude.Maybe [FailedWorkspaceChangeRequest])
srsFailedRequests = Lens.lens (failedRequests :: StopWorkspacesResponse -> Lude.Maybe [FailedWorkspaceChangeRequest]) (\s a -> s {failedRequests = a} :: StopWorkspacesResponse)
{-# DEPRECATED srsFailedRequests "Use generic-lens or generic-optics with 'failedRequests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopWorkspacesResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StopWorkspacesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopWorkspacesResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
