{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available directories that are registered with Amazon WorkSpaces.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
  ( -- * Creating a request
    DescribeWorkspaceDirectories (..),
    mkDescribeWorkspaceDirectories,

    -- ** Request lenses
    dwdNextToken,
    dwdDirectoryIds,
    dwdLimit,

    -- * Destructuring the response
    DescribeWorkspaceDirectoriesResponse (..),
    mkDescribeWorkspaceDirectoriesResponse,

    -- ** Response lenses
    dwdrsDirectories,
    dwdrsNextToken,
    dwdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDescribeWorkspaceDirectories' smart constructor.
data DescribeWorkspaceDirectories = DescribeWorkspaceDirectories'
  { nextToken ::
      Lude.Maybe Lude.Text,
    directoryIds ::
      Lude.Maybe
        (Lude.NonEmpty Lude.Text),
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

-- | Creates a value of 'DescribeWorkspaceDirectories' with the minimum fields required to make a request.
--
-- * 'directoryIds' - The identifiers of the directories. If the value is null, all directories are retrieved.
-- * 'limit' - The maximum number of directories to return.
-- * 'nextToken' - If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
mkDescribeWorkspaceDirectories ::
  DescribeWorkspaceDirectories
mkDescribeWorkspaceDirectories =
  DescribeWorkspaceDirectories'
    { nextToken = Lude.Nothing,
      directoryIds = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwdNextToken :: Lens.Lens' DescribeWorkspaceDirectories (Lude.Maybe Lude.Text)
dwdNextToken = Lens.lens (nextToken :: DescribeWorkspaceDirectories -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeWorkspaceDirectories)
{-# DEPRECATED dwdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The identifiers of the directories. If the value is null, all directories are retrieved.
--
-- /Note:/ Consider using 'directoryIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwdDirectoryIds :: Lens.Lens' DescribeWorkspaceDirectories (Lude.Maybe (Lude.NonEmpty Lude.Text))
dwdDirectoryIds = Lens.lens (directoryIds :: DescribeWorkspaceDirectories -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {directoryIds = a} :: DescribeWorkspaceDirectories)
{-# DEPRECATED dwdDirectoryIds "Use generic-lens or generic-optics with 'directoryIds' instead." #-}

-- | The maximum number of directories to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwdLimit :: Lens.Lens' DescribeWorkspaceDirectories (Lude.Maybe Lude.Natural)
dwdLimit = Lens.lens (limit :: DescribeWorkspaceDirectories -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeWorkspaceDirectories)
{-# DEPRECATED dwdLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager DescribeWorkspaceDirectories where
  page rq rs
    | Page.stop (rs Lens.^. dwdrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dwdrsDirectories) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dwdNextToken Lens..~ rs Lens.^. dwdrsNextToken

instance Lude.AWSRequest DescribeWorkspaceDirectories where
  type
    Rs DescribeWorkspaceDirectories =
      DescribeWorkspaceDirectoriesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeWorkspaceDirectoriesResponse'
            Lude.<$> (x Lude..?> "Directories" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeWorkspaceDirectories where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "WorkspacesService.DescribeWorkspaceDirectories" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeWorkspaceDirectories where
  toJSON DescribeWorkspaceDirectories' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("DirectoryIds" Lude..=) Lude.<$> directoryIds,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeWorkspaceDirectories where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeWorkspaceDirectories where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeWorkspaceDirectoriesResponse' smart constructor.
data DescribeWorkspaceDirectoriesResponse = DescribeWorkspaceDirectoriesResponse'
  { directories ::
      Lude.Maybe
        [WorkspaceDirectory],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkspaceDirectoriesResponse' with the minimum fields required to make a request.
--
-- * 'directories' - Information about the directories.
-- * 'nextToken' - The token to use to retrieve the next set of results, or null if no more results are available.
-- * 'responseStatus' - The response status code.
mkDescribeWorkspaceDirectoriesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeWorkspaceDirectoriesResponse
mkDescribeWorkspaceDirectoriesResponse pResponseStatus_ =
  DescribeWorkspaceDirectoriesResponse'
    { directories = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the directories.
--
-- /Note:/ Consider using 'directories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwdrsDirectories :: Lens.Lens' DescribeWorkspaceDirectoriesResponse (Lude.Maybe [WorkspaceDirectory])
dwdrsDirectories = Lens.lens (directories :: DescribeWorkspaceDirectoriesResponse -> Lude.Maybe [WorkspaceDirectory]) (\s a -> s {directories = a} :: DescribeWorkspaceDirectoriesResponse)
{-# DEPRECATED dwdrsDirectories "Use generic-lens or generic-optics with 'directories' instead." #-}

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwdrsNextToken :: Lens.Lens' DescribeWorkspaceDirectoriesResponse (Lude.Maybe Lude.Text)
dwdrsNextToken = Lens.lens (nextToken :: DescribeWorkspaceDirectoriesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeWorkspaceDirectoriesResponse)
{-# DEPRECATED dwdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwdrsResponseStatus :: Lens.Lens' DescribeWorkspaceDirectoriesResponse Lude.Int
dwdrsResponseStatus = Lens.lens (responseStatus :: DescribeWorkspaceDirectoriesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeWorkspaceDirectoriesResponse)
{-# DEPRECATED dwdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
