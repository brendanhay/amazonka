{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListModelPackages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the model packages that have been created.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListModelPackages
  ( -- * Creating a request
    ListModelPackages (..),
    mkListModelPackages,

    -- ** Request lenses
    lmpNameContains,
    lmpCreationTimeAfter,
    lmpNextToken,
    lmpSortOrder,
    lmpCreationTimeBefore,
    lmpMaxResults,
    lmpSortBy,

    -- * Destructuring the response
    ListModelPackagesResponse (..),
    mkListModelPackagesResponse,

    -- ** Response lenses
    lmprsModelPackageSummaryList,
    lmprsNextToken,
    lmprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListModelPackages' smart constructor.
data ListModelPackages = ListModelPackages'
  { -- | A string in the model package name. This filter returns only model packages whose name contains the specified string.
    nameContains :: Lude.Maybe Lude.Text,
    -- | A filter that returns only model packages created after the specified time (timestamp).
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | If the response to a previous @ListModelPackages@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of model packages, use the token in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The sort order for the results. The default is @Ascending@ .
    sortOrder :: Lude.Maybe SortOrder,
    -- | A filter that returns only model packages created before the specified time (timestamp).
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | The maximum number of model packages to return in the response.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The parameter by which to sort the results. The default is @CreationTime@ .
    sortBy :: Lude.Maybe ModelPackageSortBy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListModelPackages' with the minimum fields required to make a request.
--
-- * 'nameContains' - A string in the model package name. This filter returns only model packages whose name contains the specified string.
-- * 'creationTimeAfter' - A filter that returns only model packages created after the specified time (timestamp).
-- * 'nextToken' - If the response to a previous @ListModelPackages@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of model packages, use the token in the next request.
-- * 'sortOrder' - The sort order for the results. The default is @Ascending@ .
-- * 'creationTimeBefore' - A filter that returns only model packages created before the specified time (timestamp).
-- * 'maxResults' - The maximum number of model packages to return in the response.
-- * 'sortBy' - The parameter by which to sort the results. The default is @CreationTime@ .
mkListModelPackages ::
  ListModelPackages
mkListModelPackages =
  ListModelPackages'
    { nameContains = Lude.Nothing,
      creationTimeAfter = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      creationTimeBefore = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A string in the model package name. This filter returns only model packages whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpNameContains :: Lens.Lens' ListModelPackages (Lude.Maybe Lude.Text)
lmpNameContains = Lens.lens (nameContains :: ListModelPackages -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListModelPackages)
{-# DEPRECATED lmpNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A filter that returns only model packages created after the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpCreationTimeAfter :: Lens.Lens' ListModelPackages (Lude.Maybe Lude.Timestamp)
lmpCreationTimeAfter = Lens.lens (creationTimeAfter :: ListModelPackages -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListModelPackages)
{-# DEPRECATED lmpCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the response to a previous @ListModelPackages@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of model packages, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpNextToken :: Lens.Lens' ListModelPackages (Lude.Maybe Lude.Text)
lmpNextToken = Lens.lens (nextToken :: ListModelPackages -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListModelPackages)
{-# DEPRECATED lmpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for the results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpSortOrder :: Lens.Lens' ListModelPackages (Lude.Maybe SortOrder)
lmpSortOrder = Lens.lens (sortOrder :: ListModelPackages -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListModelPackages)
{-# DEPRECATED lmpSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only model packages created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpCreationTimeBefore :: Lens.Lens' ListModelPackages (Lude.Maybe Lude.Timestamp)
lmpCreationTimeBefore = Lens.lens (creationTimeBefore :: ListModelPackages -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListModelPackages)
{-# DEPRECATED lmpCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | The maximum number of model packages to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpMaxResults :: Lens.Lens' ListModelPackages (Lude.Maybe Lude.Natural)
lmpMaxResults = Lens.lens (maxResults :: ListModelPackages -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListModelPackages)
{-# DEPRECATED lmpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The parameter by which to sort the results. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpSortBy :: Lens.Lens' ListModelPackages (Lude.Maybe ModelPackageSortBy)
lmpSortBy = Lens.lens (sortBy :: ListModelPackages -> Lude.Maybe ModelPackageSortBy) (\s a -> s {sortBy = a} :: ListModelPackages)
{-# DEPRECATED lmpSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListModelPackages where
  page rq rs
    | Page.stop (rs Lens.^. lmprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lmprsModelPackageSummaryList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lmpNextToken Lens..~ rs Lens.^. lmprsNextToken

instance Lude.AWSRequest ListModelPackages where
  type Rs ListModelPackages = ListModelPackagesResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListModelPackagesResponse'
            Lude.<$> (x Lude..?> "ModelPackageSummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListModelPackages where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListModelPackages" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListModelPackages where
  toJSON ListModelPackages' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NameContains" Lude..=) Lude.<$> nameContains,
            ("CreationTimeAfter" Lude..=) Lude.<$> creationTimeAfter,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("CreationTimeBefore" Lude..=) Lude.<$> creationTimeBefore,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListModelPackages where
  toPath = Lude.const "/"

instance Lude.ToQuery ListModelPackages where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListModelPackagesResponse' smart constructor.
data ListModelPackagesResponse = ListModelPackagesResponse'
  { -- | An array of @ModelPackageSummary@ objects, each of which lists a model package.
    modelPackageSummaryList :: [ModelPackageSummary],
    -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of model packages, use it in the subsequent request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListModelPackagesResponse' with the minimum fields required to make a request.
--
-- * 'modelPackageSummaryList' - An array of @ModelPackageSummary@ objects, each of which lists a model package.
-- * 'nextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of model packages, use it in the subsequent request.
-- * 'responseStatus' - The response status code.
mkListModelPackagesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListModelPackagesResponse
mkListModelPackagesResponse pResponseStatus_ =
  ListModelPackagesResponse'
    { modelPackageSummaryList = Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @ModelPackageSummary@ objects, each of which lists a model package.
--
-- /Note:/ Consider using 'modelPackageSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmprsModelPackageSummaryList :: Lens.Lens' ListModelPackagesResponse [ModelPackageSummary]
lmprsModelPackageSummaryList = Lens.lens (modelPackageSummaryList :: ListModelPackagesResponse -> [ModelPackageSummary]) (\s a -> s {modelPackageSummaryList = a} :: ListModelPackagesResponse)
{-# DEPRECATED lmprsModelPackageSummaryList "Use generic-lens or generic-optics with 'modelPackageSummaryList' instead." #-}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of model packages, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmprsNextToken :: Lens.Lens' ListModelPackagesResponse (Lude.Maybe Lude.Text)
lmprsNextToken = Lens.lens (nextToken :: ListModelPackagesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListModelPackagesResponse)
{-# DEPRECATED lmprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmprsResponseStatus :: Lens.Lens' ListModelPackagesResponse Lude.Int
lmprsResponseStatus = Lens.lens (responseStatus :: ListModelPackagesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListModelPackagesResponse)
{-# DEPRECATED lmprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
