{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspacesConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the connection status of the specified WorkSpaces.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeWorkspacesConnectionStatus
  ( -- * Creating a request
    DescribeWorkspacesConnectionStatus (..),
    mkDescribeWorkspacesConnectionStatus,

    -- ** Request lenses
    dwcsWorkspaceIds,
    dwcsNextToken,

    -- * Destructuring the response
    DescribeWorkspacesConnectionStatusResponse (..),
    mkDescribeWorkspacesConnectionStatusResponse,

    -- ** Response lenses
    dwcsrsNextToken,
    dwcsrsWorkspacesConnectionStatus,
    dwcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDescribeWorkspacesConnectionStatus' smart constructor.
data DescribeWorkspacesConnectionStatus = DescribeWorkspacesConnectionStatus'
  { -- | The identifiers of the WorkSpaces. You can specify up to 25 WorkSpaces.
    workspaceIds :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkspacesConnectionStatus' with the minimum fields required to make a request.
--
-- * 'workspaceIds' - The identifiers of the WorkSpaces. You can specify up to 25 WorkSpaces.
-- * 'nextToken' - If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
mkDescribeWorkspacesConnectionStatus ::
  DescribeWorkspacesConnectionStatus
mkDescribeWorkspacesConnectionStatus =
  DescribeWorkspacesConnectionStatus'
    { workspaceIds = Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | The identifiers of the WorkSpaces. You can specify up to 25 WorkSpaces.
--
-- /Note:/ Consider using 'workspaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcsWorkspaceIds :: Lens.Lens' DescribeWorkspacesConnectionStatus (Lude.Maybe (Lude.NonEmpty Lude.Text))
dwcsWorkspaceIds = Lens.lens (workspaceIds :: DescribeWorkspacesConnectionStatus -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {workspaceIds = a} :: DescribeWorkspacesConnectionStatus)
{-# DEPRECATED dwcsWorkspaceIds "Use generic-lens or generic-optics with 'workspaceIds' instead." #-}

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcsNextToken :: Lens.Lens' DescribeWorkspacesConnectionStatus (Lude.Maybe Lude.Text)
dwcsNextToken = Lens.lens (nextToken :: DescribeWorkspacesConnectionStatus -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeWorkspacesConnectionStatus)
{-# DEPRECATED dwcsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager DescribeWorkspacesConnectionStatus where
  page rq rs
    | Page.stop (rs Lens.^. dwcsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dwcsrsWorkspacesConnectionStatus) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dwcsNextToken Lens..~ rs Lens.^. dwcsrsNextToken

instance Lude.AWSRequest DescribeWorkspacesConnectionStatus where
  type
    Rs DescribeWorkspacesConnectionStatus =
      DescribeWorkspacesConnectionStatusResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeWorkspacesConnectionStatusResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "WorkspacesConnectionStatus" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeWorkspacesConnectionStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "WorkspacesService.DescribeWorkspacesConnectionStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeWorkspacesConnectionStatus where
  toJSON DescribeWorkspacesConnectionStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("WorkspaceIds" Lude..=) Lude.<$> workspaceIds,
            ("NextToken" Lude..=) Lude.<$> nextToken
          ]
      )

instance Lude.ToPath DescribeWorkspacesConnectionStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeWorkspacesConnectionStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeWorkspacesConnectionStatusResponse' smart constructor.
data DescribeWorkspacesConnectionStatusResponse = DescribeWorkspacesConnectionStatusResponse'
  { -- | The token to use to retrieve the next set of results, or null if no more results are available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the connection status of the WorkSpace.
    workspacesConnectionStatus :: Lude.Maybe [WorkspaceConnectionStatus],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkspacesConnectionStatusResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next set of results, or null if no more results are available.
-- * 'workspacesConnectionStatus' - Information about the connection status of the WorkSpace.
-- * 'responseStatus' - The response status code.
mkDescribeWorkspacesConnectionStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeWorkspacesConnectionStatusResponse
mkDescribeWorkspacesConnectionStatusResponse pResponseStatus_ =
  DescribeWorkspacesConnectionStatusResponse'
    { nextToken =
        Lude.Nothing,
      workspacesConnectionStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcsrsNextToken :: Lens.Lens' DescribeWorkspacesConnectionStatusResponse (Lude.Maybe Lude.Text)
dwcsrsNextToken = Lens.lens (nextToken :: DescribeWorkspacesConnectionStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeWorkspacesConnectionStatusResponse)
{-# DEPRECATED dwcsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the connection status of the WorkSpace.
--
-- /Note:/ Consider using 'workspacesConnectionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcsrsWorkspacesConnectionStatus :: Lens.Lens' DescribeWorkspacesConnectionStatusResponse (Lude.Maybe [WorkspaceConnectionStatus])
dwcsrsWorkspacesConnectionStatus = Lens.lens (workspacesConnectionStatus :: DescribeWorkspacesConnectionStatusResponse -> Lude.Maybe [WorkspaceConnectionStatus]) (\s a -> s {workspacesConnectionStatus = a} :: DescribeWorkspacesConnectionStatusResponse)
{-# DEPRECATED dwcsrsWorkspacesConnectionStatus "Use generic-lens or generic-optics with 'workspacesConnectionStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcsrsResponseStatus :: Lens.Lens' DescribeWorkspacesConnectionStatusResponse Lude.Int
dwcsrsResponseStatus = Lens.lens (responseStatus :: DescribeWorkspacesConnectionStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeWorkspacesConnectionStatusResponse)
{-# DEPRECATED dwcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
