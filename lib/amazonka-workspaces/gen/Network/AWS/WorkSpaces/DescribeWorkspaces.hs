{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified WorkSpaces.
--
-- You can filter the results by using the bundle identifier, directory identifier, or owner, but you can specify only one filter at a time.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeWorkspaces
  ( -- * Creating a request
    DescribeWorkspaces (..),
    mkDescribeWorkspaces,

    -- ** Request lenses
    dwDirectoryId,
    dwWorkspaceIds,
    dwUserName,
    dwBundleId,
    dwNextToken,
    dwLimit,

    -- * Destructuring the response
    DescribeWorkspacesResponse (..),
    mkDescribeWorkspacesResponse,

    -- ** Response lenses
    dwrsNextToken,
    dwrsWorkspaces,
    dwrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDescribeWorkspaces' smart constructor.
data DescribeWorkspaces = DescribeWorkspaces'
  { directoryId ::
      Lude.Maybe Lude.Text,
    workspaceIds :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    userName :: Lude.Maybe Lude.Text,
    bundleId :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkspaces' with the minimum fields required to make a request.
--
-- * 'bundleId' - The identifier of the bundle. All WorkSpaces that are created from this bundle are retrieved. You cannot combine this parameter with any other filter.
-- * 'directoryId' - The identifier of the directory. In addition, you can optionally specify a specific directory user (see @UserName@ ). You cannot combine this parameter with any other filter.
-- * 'limit' - The maximum number of items to return.
-- * 'nextToken' - If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
-- * 'userName' - The name of the directory user. You must specify this parameter with @DirectoryId@ .
-- * 'workspaceIds' - The identifiers of the WorkSpaces. You cannot combine this parameter with any other filter.
--
-- Because the 'CreateWorkspaces' operation is asynchronous, the identifier it returns is not immediately available. If you immediately call 'DescribeWorkspaces' with this identifier, no information is returned.
mkDescribeWorkspaces ::
  DescribeWorkspaces
mkDescribeWorkspaces =
  DescribeWorkspaces'
    { directoryId = Lude.Nothing,
      workspaceIds = Lude.Nothing,
      userName = Lude.Nothing,
      bundleId = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The identifier of the directory. In addition, you can optionally specify a specific directory user (see @UserName@ ). You cannot combine this parameter with any other filter.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwDirectoryId :: Lens.Lens' DescribeWorkspaces (Lude.Maybe Lude.Text)
dwDirectoryId = Lens.lens (directoryId :: DescribeWorkspaces -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: DescribeWorkspaces)
{-# DEPRECATED dwDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The identifiers of the WorkSpaces. You cannot combine this parameter with any other filter.
--
-- Because the 'CreateWorkspaces' operation is asynchronous, the identifier it returns is not immediately available. If you immediately call 'DescribeWorkspaces' with this identifier, no information is returned.
--
-- /Note:/ Consider using 'workspaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwWorkspaceIds :: Lens.Lens' DescribeWorkspaces (Lude.Maybe (Lude.NonEmpty Lude.Text))
dwWorkspaceIds = Lens.lens (workspaceIds :: DescribeWorkspaces -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {workspaceIds = a} :: DescribeWorkspaces)
{-# DEPRECATED dwWorkspaceIds "Use generic-lens or generic-optics with 'workspaceIds' instead." #-}

-- | The name of the directory user. You must specify this parameter with @DirectoryId@ .
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwUserName :: Lens.Lens' DescribeWorkspaces (Lude.Maybe Lude.Text)
dwUserName = Lens.lens (userName :: DescribeWorkspaces -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: DescribeWorkspaces)
{-# DEPRECATED dwUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The identifier of the bundle. All WorkSpaces that are created from this bundle are retrieved. You cannot combine this parameter with any other filter.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwBundleId :: Lens.Lens' DescribeWorkspaces (Lude.Maybe Lude.Text)
dwBundleId = Lens.lens (bundleId :: DescribeWorkspaces -> Lude.Maybe Lude.Text) (\s a -> s {bundleId = a} :: DescribeWorkspaces)
{-# DEPRECATED dwBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwNextToken :: Lens.Lens' DescribeWorkspaces (Lude.Maybe Lude.Text)
dwNextToken = Lens.lens (nextToken :: DescribeWorkspaces -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeWorkspaces)
{-# DEPRECATED dwNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwLimit :: Lens.Lens' DescribeWorkspaces (Lude.Maybe Lude.Natural)
dwLimit = Lens.lens (limit :: DescribeWorkspaces -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeWorkspaces)
{-# DEPRECATED dwLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager DescribeWorkspaces where
  page rq rs
    | Page.stop (rs Lens.^. dwrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dwrsWorkspaces) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dwNextToken Lens..~ rs Lens.^. dwrsNextToken

instance Lude.AWSRequest DescribeWorkspaces where
  type Rs DescribeWorkspaces = DescribeWorkspacesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeWorkspacesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Workspaces" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeWorkspaces where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.DescribeWorkspaces" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeWorkspaces where
  toJSON DescribeWorkspaces' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DirectoryId" Lude..=) Lude.<$> directoryId,
            ("WorkspaceIds" Lude..=) Lude.<$> workspaceIds,
            ("UserName" Lude..=) Lude.<$> userName,
            ("BundleId" Lude..=) Lude.<$> bundleId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeWorkspaces where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeWorkspaces where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeWorkspacesResponse' smart constructor.
data DescribeWorkspacesResponse = DescribeWorkspacesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    workspaces :: Lude.Maybe [Workspace],
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

-- | Creates a value of 'DescribeWorkspacesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next set of results, or null if no more results are available.
-- * 'responseStatus' - The response status code.
-- * 'workspaces' - Information about the WorkSpaces.
--
-- Because 'CreateWorkspaces' is an asynchronous operation, some of the returned information could be incomplete.
mkDescribeWorkspacesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeWorkspacesResponse
mkDescribeWorkspacesResponse pResponseStatus_ =
  DescribeWorkspacesResponse'
    { nextToken = Lude.Nothing,
      workspaces = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrsNextToken :: Lens.Lens' DescribeWorkspacesResponse (Lude.Maybe Lude.Text)
dwrsNextToken = Lens.lens (nextToken :: DescribeWorkspacesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeWorkspacesResponse)
{-# DEPRECATED dwrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the WorkSpaces.
--
-- Because 'CreateWorkspaces' is an asynchronous operation, some of the returned information could be incomplete.
--
-- /Note:/ Consider using 'workspaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrsWorkspaces :: Lens.Lens' DescribeWorkspacesResponse (Lude.Maybe [Workspace])
dwrsWorkspaces = Lens.lens (workspaces :: DescribeWorkspacesResponse -> Lude.Maybe [Workspace]) (\s a -> s {workspaces = a} :: DescribeWorkspacesResponse)
{-# DEPRECATED dwrsWorkspaces "Use generic-lens or generic-optics with 'workspaces' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrsResponseStatus :: Lens.Lens' DescribeWorkspacesResponse Lude.Int
dwrsResponseStatus = Lens.lens (responseStatus :: DescribeWorkspacesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeWorkspacesResponse)
{-# DEPRECATED dwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
