{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.TerminateWorkspaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified WorkSpaces.
--
-- /Important:/ Terminating a WorkSpace is a permanent action and cannot be undone. The user's data is destroyed. If you need to archive any user data, contact AWS Support before terminating the WorkSpace.
-- You can terminate a WorkSpace that is in any state except @SUSPENDED@ .
-- This operation is asynchronous and returns before the WorkSpaces have been completely terminated. After a WorkSpace is terminated, the @TERMINATED@ state is returned only briefly before the WorkSpace directory metadata is cleaned up, so this state is rarely returned. To confirm that a WorkSpace is terminated, check for the WorkSpace ID by using <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaces.html DescribeWorkSpaces> . If the WorkSpace ID isn't returned, then the WorkSpace has been successfully terminated.
module Network.AWS.WorkSpaces.TerminateWorkspaces
  ( -- * Creating a request
    TerminateWorkspaces (..),
    mkTerminateWorkspaces,

    -- ** Request lenses
    twTerminateWorkspaceRequests,

    -- * Destructuring the response
    TerminateWorkspacesResponse (..),
    mkTerminateWorkspacesResponse,

    -- ** Response lenses
    twrsFailedRequests,
    twrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkTerminateWorkspaces' smart constructor.
newtype TerminateWorkspaces = TerminateWorkspaces'
  { -- | The WorkSpaces to terminate. You can specify up to 25 WorkSpaces.
    terminateWorkspaceRequests :: Lude.NonEmpty TerminateRequest
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminateWorkspaces' with the minimum fields required to make a request.
--
-- * 'terminateWorkspaceRequests' - The WorkSpaces to terminate. You can specify up to 25 WorkSpaces.
mkTerminateWorkspaces ::
  -- | 'terminateWorkspaceRequests'
  Lude.NonEmpty TerminateRequest ->
  TerminateWorkspaces
mkTerminateWorkspaces pTerminateWorkspaceRequests_ =
  TerminateWorkspaces'
    { terminateWorkspaceRequests =
        pTerminateWorkspaceRequests_
    }

-- | The WorkSpaces to terminate. You can specify up to 25 WorkSpaces.
--
-- /Note:/ Consider using 'terminateWorkspaceRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twTerminateWorkspaceRequests :: Lens.Lens' TerminateWorkspaces (Lude.NonEmpty TerminateRequest)
twTerminateWorkspaceRequests = Lens.lens (terminateWorkspaceRequests :: TerminateWorkspaces -> Lude.NonEmpty TerminateRequest) (\s a -> s {terminateWorkspaceRequests = a} :: TerminateWorkspaces)
{-# DEPRECATED twTerminateWorkspaceRequests "Use generic-lens or generic-optics with 'terminateWorkspaceRequests' instead." #-}

instance Lude.AWSRequest TerminateWorkspaces where
  type Rs TerminateWorkspaces = TerminateWorkspacesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          TerminateWorkspacesResponse'
            Lude.<$> (x Lude..?> "FailedRequests" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TerminateWorkspaces where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.TerminateWorkspaces" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TerminateWorkspaces where
  toJSON TerminateWorkspaces' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("TerminateWorkspaceRequests" Lude..= terminateWorkspaceRequests)
          ]
      )

instance Lude.ToPath TerminateWorkspaces where
  toPath = Lude.const "/"

instance Lude.ToQuery TerminateWorkspaces where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTerminateWorkspacesResponse' smart constructor.
data TerminateWorkspacesResponse = TerminateWorkspacesResponse'
  { -- | Information about the WorkSpaces that could not be terminated.
    failedRequests :: Lude.Maybe [FailedWorkspaceChangeRequest],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminateWorkspacesResponse' with the minimum fields required to make a request.
--
-- * 'failedRequests' - Information about the WorkSpaces that could not be terminated.
-- * 'responseStatus' - The response status code.
mkTerminateWorkspacesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TerminateWorkspacesResponse
mkTerminateWorkspacesResponse pResponseStatus_ =
  TerminateWorkspacesResponse'
    { failedRequests = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the WorkSpaces that could not be terminated.
--
-- /Note:/ Consider using 'failedRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twrsFailedRequests :: Lens.Lens' TerminateWorkspacesResponse (Lude.Maybe [FailedWorkspaceChangeRequest])
twrsFailedRequests = Lens.lens (failedRequests :: TerminateWorkspacesResponse -> Lude.Maybe [FailedWorkspaceChangeRequest]) (\s a -> s {failedRequests = a} :: TerminateWorkspacesResponse)
{-# DEPRECATED twrsFailedRequests "Use generic-lens or generic-optics with 'failedRequests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twrsResponseStatus :: Lens.Lens' TerminateWorkspacesResponse Lude.Int
twrsResponseStatus = Lens.lens (responseStatus :: TerminateWorkspacesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TerminateWorkspacesResponse)
{-# DEPRECATED twrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
