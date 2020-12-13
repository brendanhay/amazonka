{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListFlowDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the flow definitions in your account.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListFlowDefinitions
  ( -- * Creating a request
    ListFlowDefinitions (..),
    mkListFlowDefinitions,

    -- ** Request lenses
    lfdCreationTimeAfter,
    lfdNextToken,
    lfdSortOrder,
    lfdCreationTimeBefore,
    lfdMaxResults,

    -- * Destructuring the response
    ListFlowDefinitionsResponse (..),
    mkListFlowDefinitionsResponse,

    -- ** Response lenses
    lfdrsFlowDefinitionSummaries,
    lfdrsNextToken,
    lfdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListFlowDefinitions' smart constructor.
data ListFlowDefinitions = ListFlowDefinitions'
  { -- | A filter that returns only flow definitions with a creation time greater than or equal to the specified timestamp.
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | A token to resume pagination.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An optional value that specifies whether you want the results sorted in @Ascending@ or @Descending@ order.
    sortOrder :: Lude.Maybe SortOrder,
    -- | A filter that returns only flow definitions that were created before the specified timestamp.
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | The total number of items to return. If the total number of available items is more than the value specified in @MaxResults@ , then a @NextToken@ will be provided in the output that you can use to resume pagination.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFlowDefinitions' with the minimum fields required to make a request.
--
-- * 'creationTimeAfter' - A filter that returns only flow definitions with a creation time greater than or equal to the specified timestamp.
-- * 'nextToken' - A token to resume pagination.
-- * 'sortOrder' - An optional value that specifies whether you want the results sorted in @Ascending@ or @Descending@ order.
-- * 'creationTimeBefore' - A filter that returns only flow definitions that were created before the specified timestamp.
-- * 'maxResults' - The total number of items to return. If the total number of available items is more than the value specified in @MaxResults@ , then a @NextToken@ will be provided in the output that you can use to resume pagination.
mkListFlowDefinitions ::
  ListFlowDefinitions
mkListFlowDefinitions =
  ListFlowDefinitions'
    { creationTimeAfter = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      creationTimeBefore = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A filter that returns only flow definitions with a creation time greater than or equal to the specified timestamp.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdCreationTimeAfter :: Lens.Lens' ListFlowDefinitions (Lude.Maybe Lude.Timestamp)
lfdCreationTimeAfter = Lens.lens (creationTimeAfter :: ListFlowDefinitions -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListFlowDefinitions)
{-# DEPRECATED lfdCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | A token to resume pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdNextToken :: Lens.Lens' ListFlowDefinitions (Lude.Maybe Lude.Text)
lfdNextToken = Lens.lens (nextToken :: ListFlowDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFlowDefinitions)
{-# DEPRECATED lfdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An optional value that specifies whether you want the results sorted in @Ascending@ or @Descending@ order.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdSortOrder :: Lens.Lens' ListFlowDefinitions (Lude.Maybe SortOrder)
lfdSortOrder = Lens.lens (sortOrder :: ListFlowDefinitions -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListFlowDefinitions)
{-# DEPRECATED lfdSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only flow definitions that were created before the specified timestamp.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdCreationTimeBefore :: Lens.Lens' ListFlowDefinitions (Lude.Maybe Lude.Timestamp)
lfdCreationTimeBefore = Lens.lens (creationTimeBefore :: ListFlowDefinitions -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListFlowDefinitions)
{-# DEPRECATED lfdCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | The total number of items to return. If the total number of available items is more than the value specified in @MaxResults@ , then a @NextToken@ will be provided in the output that you can use to resume pagination.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdMaxResults :: Lens.Lens' ListFlowDefinitions (Lude.Maybe Lude.Natural)
lfdMaxResults = Lens.lens (maxResults :: ListFlowDefinitions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListFlowDefinitions)
{-# DEPRECATED lfdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListFlowDefinitions where
  page rq rs
    | Page.stop (rs Lens.^. lfdrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lfdrsFlowDefinitionSummaries) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lfdNextToken Lens..~ rs Lens.^. lfdrsNextToken

instance Lude.AWSRequest ListFlowDefinitions where
  type Rs ListFlowDefinitions = ListFlowDefinitionsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListFlowDefinitionsResponse'
            Lude.<$> (x Lude..?> "FlowDefinitionSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListFlowDefinitions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListFlowDefinitions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListFlowDefinitions where
  toJSON ListFlowDefinitions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CreationTimeAfter" Lude..=) Lude.<$> creationTimeAfter,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("CreationTimeBefore" Lude..=) Lude.<$> creationTimeBefore,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListFlowDefinitions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListFlowDefinitions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListFlowDefinitionsResponse' smart constructor.
data ListFlowDefinitionsResponse = ListFlowDefinitionsResponse'
  { -- | An array of objects describing the flow definitions.
    flowDefinitionSummaries :: [FlowDefinitionSummary],
    -- | A token to resume pagination.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFlowDefinitionsResponse' with the minimum fields required to make a request.
--
-- * 'flowDefinitionSummaries' - An array of objects describing the flow definitions.
-- * 'nextToken' - A token to resume pagination.
-- * 'responseStatus' - The response status code.
mkListFlowDefinitionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListFlowDefinitionsResponse
mkListFlowDefinitionsResponse pResponseStatus_ =
  ListFlowDefinitionsResponse'
    { flowDefinitionSummaries =
        Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects describing the flow definitions.
--
-- /Note:/ Consider using 'flowDefinitionSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdrsFlowDefinitionSummaries :: Lens.Lens' ListFlowDefinitionsResponse [FlowDefinitionSummary]
lfdrsFlowDefinitionSummaries = Lens.lens (flowDefinitionSummaries :: ListFlowDefinitionsResponse -> [FlowDefinitionSummary]) (\s a -> s {flowDefinitionSummaries = a} :: ListFlowDefinitionsResponse)
{-# DEPRECATED lfdrsFlowDefinitionSummaries "Use generic-lens or generic-optics with 'flowDefinitionSummaries' instead." #-}

-- | A token to resume pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdrsNextToken :: Lens.Lens' ListFlowDefinitionsResponse (Lude.Maybe Lude.Text)
lfdrsNextToken = Lens.lens (nextToken :: ListFlowDefinitionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFlowDefinitionsResponse)
{-# DEPRECATED lfdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdrsResponseStatus :: Lens.Lens' ListFlowDefinitionsResponse Lude.Int
lfdrsResponseStatus = Lens.lens (responseStatus :: ListFlowDefinitionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListFlowDefinitionsResponse)
{-# DEPRECATED lfdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
