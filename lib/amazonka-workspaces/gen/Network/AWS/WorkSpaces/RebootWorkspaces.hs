{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.RebootWorkspaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots the specified WorkSpaces.
--
-- You cannot reboot a WorkSpace unless its state is @AVAILABLE@ or @UNHEALTHY@ .
-- This operation is asynchronous and returns before the WorkSpaces have rebooted.
module Network.AWS.WorkSpaces.RebootWorkspaces
  ( -- * Creating a request
    RebootWorkspaces (..),
    mkRebootWorkspaces,

    -- ** Request lenses
    rwRebootWorkspaceRequests,

    -- * Destructuring the response
    RebootWorkspacesResponse (..),
    mkRebootWorkspacesResponse,

    -- ** Response lenses
    rrsFailedRequests,
    rrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkRebootWorkspaces' smart constructor.
newtype RebootWorkspaces = RebootWorkspaces'
  { rebootWorkspaceRequests ::
      Lude.NonEmpty RebootRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebootWorkspaces' with the minimum fields required to make a request.
--
-- * 'rebootWorkspaceRequests' - The WorkSpaces to reboot. You can specify up to 25 WorkSpaces.
mkRebootWorkspaces ::
  -- | 'rebootWorkspaceRequests'
  Lude.NonEmpty RebootRequest ->
  RebootWorkspaces
mkRebootWorkspaces pRebootWorkspaceRequests_ =
  RebootWorkspaces'
    { rebootWorkspaceRequests =
        pRebootWorkspaceRequests_
    }

-- | The WorkSpaces to reboot. You can specify up to 25 WorkSpaces.
--
-- /Note:/ Consider using 'rebootWorkspaceRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwRebootWorkspaceRequests :: Lens.Lens' RebootWorkspaces (Lude.NonEmpty RebootRequest)
rwRebootWorkspaceRequests = Lens.lens (rebootWorkspaceRequests :: RebootWorkspaces -> Lude.NonEmpty RebootRequest) (\s a -> s {rebootWorkspaceRequests = a} :: RebootWorkspaces)
{-# DEPRECATED rwRebootWorkspaceRequests "Use generic-lens or generic-optics with 'rebootWorkspaceRequests' instead." #-}

instance Lude.AWSRequest RebootWorkspaces where
  type Rs RebootWorkspaces = RebootWorkspacesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          RebootWorkspacesResponse'
            Lude.<$> (x Lude..?> "FailedRequests" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RebootWorkspaces where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.RebootWorkspaces" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RebootWorkspaces where
  toJSON RebootWorkspaces' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("RebootWorkspaceRequests" Lude..= rebootWorkspaceRequests)
          ]
      )

instance Lude.ToPath RebootWorkspaces where
  toPath = Lude.const "/"

instance Lude.ToQuery RebootWorkspaces where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRebootWorkspacesResponse' smart constructor.
data RebootWorkspacesResponse = RebootWorkspacesResponse'
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

-- | Creates a value of 'RebootWorkspacesResponse' with the minimum fields required to make a request.
--
-- * 'failedRequests' - Information about the WorkSpaces that could not be rebooted.
-- * 'responseStatus' - The response status code.
mkRebootWorkspacesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RebootWorkspacesResponse
mkRebootWorkspacesResponse pResponseStatus_ =
  RebootWorkspacesResponse'
    { failedRequests = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the WorkSpaces that could not be rebooted.
--
-- /Note:/ Consider using 'failedRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsFailedRequests :: Lens.Lens' RebootWorkspacesResponse (Lude.Maybe [FailedWorkspaceChangeRequest])
rrsFailedRequests = Lens.lens (failedRequests :: RebootWorkspacesResponse -> Lude.Maybe [FailedWorkspaceChangeRequest]) (\s a -> s {failedRequests = a} :: RebootWorkspacesResponse)
{-# DEPRECATED rrsFailedRequests "Use generic-lens or generic-optics with 'failedRequests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RebootWorkspacesResponse Lude.Int
rrsResponseStatus = Lens.lens (responseStatus :: RebootWorkspacesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RebootWorkspacesResponse)
{-# DEPRECATED rrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
