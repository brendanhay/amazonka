{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListModels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists models created with the 'CreateModel' API.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListModels
  ( -- * Creating a request
    ListModels (..),
    mkListModels,

    -- ** Request lenses
    lmNameContains,
    lmCreationTimeAfter,
    lmNextToken,
    lmSortOrder,
    lmCreationTimeBefore,
    lmMaxResults,
    lmSortBy,

    -- * Destructuring the response
    ListModelsResponse (..),
    mkListModelsResponse,

    -- ** Response lenses
    lmrsNextToken,
    lmrsModels,
    lmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListModels' smart constructor.
data ListModels = ListModels'
  { -- | A string in the training job name. This filter returns only models in the training job whose name contains the specified string.
    nameContains :: Lude.Maybe Lude.Text,
    -- | A filter that returns only models with a creation time greater than or equal to the specified time (timestamp).
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | If the response to a previous @ListModels@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of models, use the token in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The sort order for results. The default is @Descending@ .
    sortOrder :: Lude.Maybe OrderKey,
    -- | A filter that returns only models created before the specified time (timestamp).
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | The maximum number of models to return in the response.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | Sorts the list of results. The default is @CreationTime@ .
    sortBy :: Lude.Maybe ModelSortKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListModels' with the minimum fields required to make a request.
--
-- * 'nameContains' - A string in the training job name. This filter returns only models in the training job whose name contains the specified string.
-- * 'creationTimeAfter' - A filter that returns only models with a creation time greater than or equal to the specified time (timestamp).
-- * 'nextToken' - If the response to a previous @ListModels@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of models, use the token in the next request.
-- * 'sortOrder' - The sort order for results. The default is @Descending@ .
-- * 'creationTimeBefore' - A filter that returns only models created before the specified time (timestamp).
-- * 'maxResults' - The maximum number of models to return in the response.
-- * 'sortBy' - Sorts the list of results. The default is @CreationTime@ .
mkListModels ::
  ListModels
mkListModels =
  ListModels'
    { nameContains = Lude.Nothing,
      creationTimeAfter = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      creationTimeBefore = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A string in the training job name. This filter returns only models in the training job whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmNameContains :: Lens.Lens' ListModels (Lude.Maybe Lude.Text)
lmNameContains = Lens.lens (nameContains :: ListModels -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListModels)
{-# DEPRECATED lmNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A filter that returns only models with a creation time greater than or equal to the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmCreationTimeAfter :: Lens.Lens' ListModels (Lude.Maybe Lude.Timestamp)
lmCreationTimeAfter = Lens.lens (creationTimeAfter :: ListModels -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListModels)
{-# DEPRECATED lmCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the response to a previous @ListModels@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of models, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmNextToken :: Lens.Lens' ListModels (Lude.Maybe Lude.Text)
lmNextToken = Lens.lens (nextToken :: ListModels -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListModels)
{-# DEPRECATED lmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for results. The default is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmSortOrder :: Lens.Lens' ListModels (Lude.Maybe OrderKey)
lmSortOrder = Lens.lens (sortOrder :: ListModels -> Lude.Maybe OrderKey) (\s a -> s {sortOrder = a} :: ListModels)
{-# DEPRECATED lmSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only models created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmCreationTimeBefore :: Lens.Lens' ListModels (Lude.Maybe Lude.Timestamp)
lmCreationTimeBefore = Lens.lens (creationTimeBefore :: ListModels -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListModels)
{-# DEPRECATED lmCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | The maximum number of models to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmMaxResults :: Lens.Lens' ListModels (Lude.Maybe Lude.Natural)
lmMaxResults = Lens.lens (maxResults :: ListModels -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListModels)
{-# DEPRECATED lmMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Sorts the list of results. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmSortBy :: Lens.Lens' ListModels (Lude.Maybe ModelSortKey)
lmSortBy = Lens.lens (sortBy :: ListModels -> Lude.Maybe ModelSortKey) (\s a -> s {sortBy = a} :: ListModels)
{-# DEPRECATED lmSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListModels where
  page rq rs
    | Page.stop (rs Lens.^. lmrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lmrsModels) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lmNextToken Lens..~ rs Lens.^. lmrsNextToken

instance Lude.AWSRequest ListModels where
  type Rs ListModels = ListModelsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListModelsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Models" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListModels where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListModels" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListModels where
  toJSON ListModels' {..} =
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

instance Lude.ToPath ListModels where
  toPath = Lude.const "/"

instance Lude.ToQuery ListModels where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListModelsResponse' smart constructor.
data ListModelsResponse = ListModelsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of models, use it in the subsequent request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An array of @ModelSummary@ objects, each of which lists a model.
    models :: [ModelSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListModelsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of models, use it in the subsequent request.
-- * 'models' - An array of @ModelSummary@ objects, each of which lists a model.
-- * 'responseStatus' - The response status code.
mkListModelsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListModelsResponse
mkListModelsResponse pResponseStatus_ =
  ListModelsResponse'
    { nextToken = Lude.Nothing,
      models = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of models, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrsNextToken :: Lens.Lens' ListModelsResponse (Lude.Maybe Lude.Text)
lmrsNextToken = Lens.lens (nextToken :: ListModelsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListModelsResponse)
{-# DEPRECATED lmrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of @ModelSummary@ objects, each of which lists a model.
--
-- /Note:/ Consider using 'models' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrsModels :: Lens.Lens' ListModelsResponse [ModelSummary]
lmrsModels = Lens.lens (models :: ListModelsResponse -> [ModelSummary]) (\s a -> s {models = a} :: ListModelsResponse)
{-# DEPRECATED lmrsModels "Use generic-lens or generic-optics with 'models' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrsResponseStatus :: Lens.Lens' ListModelsResponse Lude.Int
lmrsResponseStatus = Lens.lens (responseStatus :: ListModelsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListModelsResponse)
{-# DEPRECATED lmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
