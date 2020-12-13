{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeRootFolders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current user's special folders; the @RootFolder@ and the @RecycleBin@ . @RootFolder@ is the root of user's files and folders and @RecycleBin@ is the root of recycled items. This is not a valid action for SigV4 (administrative API) clients.
--
-- This action requires an authentication token. To get an authentication token, register an application with Amazon WorkDocs. For more information, see <https://docs.aws.amazon.com/workdocs/latest/developerguide/wd-auth-user.html Authentication and Access Control for User Applications> in the /Amazon WorkDocs Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeRootFolders
  ( -- * Creating a request
    DescribeRootFolders (..),
    mkDescribeRootFolders,

    -- ** Request lenses
    drfAuthenticationToken,
    drfMarker,
    drfLimit,

    -- * Destructuring the response
    DescribeRootFoldersResponse (..),
    mkDescribeRootFoldersResponse,

    -- ** Response lenses
    drfrsFolders,
    drfrsMarker,
    drfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkDescribeRootFolders' smart constructor.
data DescribeRootFolders = DescribeRootFolders'
  { -- | Amazon WorkDocs authentication token.
    authenticationToken :: Lude.Sensitive Lude.Text,
    -- | The marker for the next set of results. (You received this marker from a previous call.)
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRootFolders' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token.
-- * 'marker' - The marker for the next set of results. (You received this marker from a previous call.)
-- * 'limit' - The maximum number of items to return.
mkDescribeRootFolders ::
  -- | 'authenticationToken'
  Lude.Sensitive Lude.Text ->
  DescribeRootFolders
mkDescribeRootFolders pAuthenticationToken_ =
  DescribeRootFolders'
    { authenticationToken = pAuthenticationToken_,
      marker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | Amazon WorkDocs authentication token.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfAuthenticationToken :: Lens.Lens' DescribeRootFolders (Lude.Sensitive Lude.Text)
drfAuthenticationToken = Lens.lens (authenticationToken :: DescribeRootFolders -> Lude.Sensitive Lude.Text) (\s a -> s {authenticationToken = a} :: DescribeRootFolders)
{-# DEPRECATED drfAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfMarker :: Lens.Lens' DescribeRootFolders (Lude.Maybe Lude.Text)
drfMarker = Lens.lens (marker :: DescribeRootFolders -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeRootFolders)
{-# DEPRECATED drfMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfLimit :: Lens.Lens' DescribeRootFolders (Lude.Maybe Lude.Natural)
drfLimit = Lens.lens (limit :: DescribeRootFolders -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeRootFolders)
{-# DEPRECATED drfLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager DescribeRootFolders where
  page rq rs
    | Page.stop (rs Lens.^. drfrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. drfrsFolders) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& drfMarker Lens..~ rs Lens.^. drfrsMarker

instance Lude.AWSRequest DescribeRootFolders where
  type Rs DescribeRootFolders = DescribeRootFoldersResponse
  request = Req.get workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeRootFoldersResponse'
            Lude.<$> (x Lude..?> "Folders" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRootFolders where
  toHeaders DescribeRootFolders' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath DescribeRootFolders where
  toPath = Lude.const "/api/v1/me/root"

instance Lude.ToQuery DescribeRootFolders where
  toQuery DescribeRootFolders' {..} =
    Lude.mconcat ["marker" Lude.=: marker, "limit" Lude.=: limit]

-- | /See:/ 'mkDescribeRootFoldersResponse' smart constructor.
data DescribeRootFoldersResponse = DescribeRootFoldersResponse'
  { -- | The user's special folders.
    folders :: Lude.Maybe [FolderMetadata],
    -- | The marker for the next set of results.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRootFoldersResponse' with the minimum fields required to make a request.
--
-- * 'folders' - The user's special folders.
-- * 'marker' - The marker for the next set of results.
-- * 'responseStatus' - The response status code.
mkDescribeRootFoldersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRootFoldersResponse
mkDescribeRootFoldersResponse pResponseStatus_ =
  DescribeRootFoldersResponse'
    { folders = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The user's special folders.
--
-- /Note:/ Consider using 'folders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsFolders :: Lens.Lens' DescribeRootFoldersResponse (Lude.Maybe [FolderMetadata])
drfrsFolders = Lens.lens (folders :: DescribeRootFoldersResponse -> Lude.Maybe [FolderMetadata]) (\s a -> s {folders = a} :: DescribeRootFoldersResponse)
{-# DEPRECATED drfrsFolders "Use generic-lens or generic-optics with 'folders' instead." #-}

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsMarker :: Lens.Lens' DescribeRootFoldersResponse (Lude.Maybe Lude.Text)
drfrsMarker = Lens.lens (marker :: DescribeRootFoldersResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeRootFoldersResponse)
{-# DEPRECATED drfrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsResponseStatus :: Lens.Lens' DescribeRootFoldersResponse Lude.Int
drfrsResponseStatus = Lens.lens (responseStatus :: DescribeRootFoldersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRootFoldersResponse)
{-# DEPRECATED drfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
