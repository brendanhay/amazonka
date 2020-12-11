{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListBuildBatchesForProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the identifiers of the build batches for a specific project.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListBuildBatchesForProject
  ( -- * Creating a request
    ListBuildBatchesForProject (..),
    mkListBuildBatchesForProject,

    -- ** Request lenses
    lbbfpSortOrder,
    lbbfpNextToken,
    lbbfpProjectName,
    lbbfpFilter,
    lbbfpMaxResults,

    -- * Destructuring the response
    ListBuildBatchesForProjectResponse (..),
    mkListBuildBatchesForProjectResponse,

    -- ** Response lenses
    lbbfprsIds,
    lbbfprsNextToken,
    lbbfprsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListBuildBatchesForProject' smart constructor.
data ListBuildBatchesForProject = ListBuildBatchesForProject'
  { sortOrder ::
      Lude.Maybe SortOrderType,
    nextToken :: Lude.Maybe Lude.Text,
    projectName :: Lude.Maybe Lude.Text,
    filter :: Lude.Maybe BuildBatchFilter,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBuildBatchesForProject' with the minimum fields required to make a request.
--
-- * 'filter' - A @BuildBatchFilter@ object that specifies the filters for the search.
-- * 'maxResults' - The maximum number of results to return.
-- * 'nextToken' - The @nextToken@ value returned from a previous call to @ListBuildBatchesForProject@ . This specifies the next item to return. To return the beginning of the list, exclude this parameter.
-- * 'projectName' - The name of the project.
-- * 'sortOrder' - Specifies the sort order of the returned items. Valid values include:
--
--
--     * @ASCENDING@ : List the batch build identifiers in ascending order by identifier.
--
--
--     * @DESCENDING@ : List the batch build identifiers in descending order by identifier.
mkListBuildBatchesForProject ::
  ListBuildBatchesForProject
mkListBuildBatchesForProject =
  ListBuildBatchesForProject'
    { sortOrder = Lude.Nothing,
      nextToken = Lude.Nothing,
      projectName = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Specifies the sort order of the returned items. Valid values include:
--
--
--     * @ASCENDING@ : List the batch build identifiers in ascending order by identifier.
--
--
--     * @DESCENDING@ : List the batch build identifiers in descending order by identifier.
--
--
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbfpSortOrder :: Lens.Lens' ListBuildBatchesForProject (Lude.Maybe SortOrderType)
lbbfpSortOrder = Lens.lens (sortOrder :: ListBuildBatchesForProject -> Lude.Maybe SortOrderType) (\s a -> s {sortOrder = a} :: ListBuildBatchesForProject)
{-# DEPRECATED lbbfpSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The @nextToken@ value returned from a previous call to @ListBuildBatchesForProject@ . This specifies the next item to return. To return the beginning of the list, exclude this parameter.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbfpNextToken :: Lens.Lens' ListBuildBatchesForProject (Lude.Maybe Lude.Text)
lbbfpNextToken = Lens.lens (nextToken :: ListBuildBatchesForProject -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBuildBatchesForProject)
{-# DEPRECATED lbbfpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the project.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbfpProjectName :: Lens.Lens' ListBuildBatchesForProject (Lude.Maybe Lude.Text)
lbbfpProjectName = Lens.lens (projectName :: ListBuildBatchesForProject -> Lude.Maybe Lude.Text) (\s a -> s {projectName = a} :: ListBuildBatchesForProject)
{-# DEPRECATED lbbfpProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

-- | A @BuildBatchFilter@ object that specifies the filters for the search.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbfpFilter :: Lens.Lens' ListBuildBatchesForProject (Lude.Maybe BuildBatchFilter)
lbbfpFilter = Lens.lens (filter :: ListBuildBatchesForProject -> Lude.Maybe BuildBatchFilter) (\s a -> s {filter = a} :: ListBuildBatchesForProject)
{-# DEPRECATED lbbfpFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbfpMaxResults :: Lens.Lens' ListBuildBatchesForProject (Lude.Maybe Lude.Natural)
lbbfpMaxResults = Lens.lens (maxResults :: ListBuildBatchesForProject -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListBuildBatchesForProject)
{-# DEPRECATED lbbfpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListBuildBatchesForProject where
  page rq rs
    | Page.stop (rs Lens.^. lbbfprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lbbfprsIds) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lbbfpNextToken Lens..~ rs Lens.^. lbbfprsNextToken

instance Lude.AWSRequest ListBuildBatchesForProject where
  type
    Rs ListBuildBatchesForProject =
      ListBuildBatchesForProjectResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListBuildBatchesForProjectResponse'
            Lude.<$> (x Lude..?> "ids" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBuildBatchesForProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeBuild_20161006.ListBuildBatchesForProject" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListBuildBatchesForProject where
  toJSON ListBuildBatchesForProject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sortOrder" Lude..=) Lude.<$> sortOrder,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("projectName" Lude..=) Lude.<$> projectName,
            ("filter" Lude..=) Lude.<$> filter,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListBuildBatchesForProject where
  toPath = Lude.const "/"

instance Lude.ToQuery ListBuildBatchesForProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListBuildBatchesForProjectResponse' smart constructor.
data ListBuildBatchesForProjectResponse = ListBuildBatchesForProjectResponse'
  { ids ::
      Lude.Maybe
        [Lude.Text],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListBuildBatchesForProjectResponse' with the minimum fields required to make a request.
--
-- * 'ids' - An array of strings that contains the batch build identifiers.
-- * 'nextToken' - If there are more items to return, this contains a token that is passed to a subsequent call to @ListBuildBatchesForProject@ to retrieve the next set of items.
-- * 'responseStatus' - The response status code.
mkListBuildBatchesForProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBuildBatchesForProjectResponse
mkListBuildBatchesForProjectResponse pResponseStatus_ =
  ListBuildBatchesForProjectResponse'
    { ids = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of strings that contains the batch build identifiers.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbfprsIds :: Lens.Lens' ListBuildBatchesForProjectResponse (Lude.Maybe [Lude.Text])
lbbfprsIds = Lens.lens (ids :: ListBuildBatchesForProjectResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {ids = a} :: ListBuildBatchesForProjectResponse)
{-# DEPRECATED lbbfprsIds "Use generic-lens or generic-optics with 'ids' instead." #-}

-- | If there are more items to return, this contains a token that is passed to a subsequent call to @ListBuildBatchesForProject@ to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbfprsNextToken :: Lens.Lens' ListBuildBatchesForProjectResponse (Lude.Maybe Lude.Text)
lbbfprsNextToken = Lens.lens (nextToken :: ListBuildBatchesForProjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBuildBatchesForProjectResponse)
{-# DEPRECATED lbbfprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbfprsResponseStatus :: Lens.Lens' ListBuildBatchesForProjectResponse Lude.Int
lbbfprsResponseStatus = Lens.lens (responseStatus :: ListBuildBatchesForProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBuildBatchesForProjectResponse)
{-# DEPRECATED lbbfprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
