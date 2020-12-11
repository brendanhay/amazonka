{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListSharedProjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of projects that are shared with other AWS accounts or users.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListSharedProjects
  ( -- * Creating a request
    ListSharedProjects (..),
    mkListSharedProjects,

    -- ** Request lenses
    lspSortOrder,
    lspNextToken,
    lspMaxResults,
    lspSortBy,

    -- * Destructuring the response
    ListSharedProjectsResponse (..),
    mkListSharedProjectsResponse,

    -- ** Response lenses
    lsprsNextToken,
    lsprsProjects,
    lsprsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSharedProjects' smart constructor.
data ListSharedProjects = ListSharedProjects'
  { sortOrder ::
      Lude.Maybe SortOrderType,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    sortBy :: Lude.Maybe SharedResourceSortByType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSharedProjects' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of paginated shared build projects returned per response. Use @nextToken@ to iterate pages in the list of returned @Project@ objects. The default value is 100.
-- * 'nextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
-- * 'sortBy' - The criterion to be used to list build projects shared with the current AWS account or user. Valid values include:
--
--
--     * @ARN@ : List based on the ARN.
--
--
--     * @MODIFIED_TIME@ : List based on when information about the shared project was last changed.
--
--
-- * 'sortOrder' - The order in which to list shared build projects. Valid values include:
--
--
--     * @ASCENDING@ : List in ascending order.
--
--
--     * @DESCENDING@ : List in descending order.
mkListSharedProjects ::
  ListSharedProjects
mkListSharedProjects =
  ListSharedProjects'
    { sortOrder = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | The order in which to list shared build projects. Valid values include:
--
--
--     * @ASCENDING@ : List in ascending order.
--
--
--     * @DESCENDING@ : List in descending order.
--
--
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspSortOrder :: Lens.Lens' ListSharedProjects (Lude.Maybe SortOrderType)
lspSortOrder = Lens.lens (sortOrder :: ListSharedProjects -> Lude.Maybe SortOrderType) (\s a -> s {sortOrder = a} :: ListSharedProjects)
{-# DEPRECATED lspSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspNextToken :: Lens.Lens' ListSharedProjects (Lude.Maybe Lude.Text)
lspNextToken = Lens.lens (nextToken :: ListSharedProjects -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSharedProjects)
{-# DEPRECATED lspNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of paginated shared build projects returned per response. Use @nextToken@ to iterate pages in the list of returned @Project@ objects. The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspMaxResults :: Lens.Lens' ListSharedProjects (Lude.Maybe Lude.Natural)
lspMaxResults = Lens.lens (maxResults :: ListSharedProjects -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListSharedProjects)
{-# DEPRECATED lspMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The criterion to be used to list build projects shared with the current AWS account or user. Valid values include:
--
--
--     * @ARN@ : List based on the ARN.
--
--
--     * @MODIFIED_TIME@ : List based on when information about the shared project was last changed.
--
--
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspSortBy :: Lens.Lens' ListSharedProjects (Lude.Maybe SharedResourceSortByType)
lspSortBy = Lens.lens (sortBy :: ListSharedProjects -> Lude.Maybe SharedResourceSortByType) (\s a -> s {sortBy = a} :: ListSharedProjects)
{-# DEPRECATED lspSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListSharedProjects where
  page rq rs
    | Page.stop (rs Lens.^. lsprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsprsProjects) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lspNextToken Lens..~ rs Lens.^. lsprsNextToken

instance Lude.AWSRequest ListSharedProjects where
  type Rs ListSharedProjects = ListSharedProjectsResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSharedProjectsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "projects")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSharedProjects where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.ListSharedProjects" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSharedProjects where
  toJSON ListSharedProjects' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sortOrder" Lude..=) Lude.<$> sortOrder,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            ("sortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListSharedProjects where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSharedProjects where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListSharedProjectsResponse' smart constructor.
data ListSharedProjectsResponse = ListSharedProjectsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    projects ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
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

-- | Creates a value of 'ListSharedProjectsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
-- * 'projects' - The list of ARNs for the build projects shared with the current AWS account or user.
-- * 'responseStatus' - The response status code.
mkListSharedProjectsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSharedProjectsResponse
mkListSharedProjectsResponse pResponseStatus_ =
  ListSharedProjectsResponse'
    { nextToken = Lude.Nothing,
      projects = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprsNextToken :: Lens.Lens' ListSharedProjectsResponse (Lude.Maybe Lude.Text)
lsprsNextToken = Lens.lens (nextToken :: ListSharedProjectsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSharedProjectsResponse)
{-# DEPRECATED lsprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of ARNs for the build projects shared with the current AWS account or user.
--
-- /Note:/ Consider using 'projects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprsProjects :: Lens.Lens' ListSharedProjectsResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
lsprsProjects = Lens.lens (projects :: ListSharedProjectsResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {projects = a} :: ListSharedProjectsResponse)
{-# DEPRECATED lsprsProjects "Use generic-lens or generic-optics with 'projects' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprsResponseStatus :: Lens.Lens' ListSharedProjectsResponse Lude.Int
lsprsResponseStatus = Lens.lens (responseStatus :: ListSharedProjectsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSharedProjectsResponse)
{-# DEPRECATED lsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
