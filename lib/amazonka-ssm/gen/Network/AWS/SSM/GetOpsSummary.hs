{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetOpsSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- View a summary of OpsItems based on specified filters and aggregators.
--
-- This operation returns paginated results.
module Network.AWS.SSM.GetOpsSummary
  ( -- * Creating a request
    GetOpsSummary (..),
    mkGetOpsSummary,

    -- ** Request lenses
    gosAggregators,
    gosSyncName,
    gosFilters,
    gosResultAttributes,
    gosNextToken,
    gosMaxResults,

    -- * Destructuring the response
    GetOpsSummaryResponse (..),
    mkGetOpsSummaryResponse,

    -- ** Response lenses
    gosrsEntities,
    gosrsNextToken,
    gosrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetOpsSummary' smart constructor.
data GetOpsSummary = GetOpsSummary'
  { -- | Optional aggregators that return counts of OpsItems based on one or more expressions.
    aggregators :: Lude.Maybe (Lude.NonEmpty OpsAggregator),
    -- | Specify the name of a resource data sync to get.
    syncName :: Lude.Maybe Lude.Text,
    -- | Optional filters used to scope down the returned OpsItems.
    filters :: Lude.Maybe (Lude.NonEmpty OpsFilter),
    -- | The OpsItem data type to return.
    resultAttributes :: Lude.Maybe (Lude.NonEmpty OpsResultAttribute),
    -- | A token to start the list. Use this token to get the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOpsSummary' with the minimum fields required to make a request.
--
-- * 'aggregators' - Optional aggregators that return counts of OpsItems based on one or more expressions.
-- * 'syncName' - Specify the name of a resource data sync to get.
-- * 'filters' - Optional filters used to scope down the returned OpsItems.
-- * 'resultAttributes' - The OpsItem data type to return.
-- * 'nextToken' - A token to start the list. Use this token to get the next set of results.
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
mkGetOpsSummary ::
  GetOpsSummary
mkGetOpsSummary =
  GetOpsSummary'
    { aggregators = Lude.Nothing,
      syncName = Lude.Nothing,
      filters = Lude.Nothing,
      resultAttributes = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Optional aggregators that return counts of OpsItems based on one or more expressions.
--
-- /Note:/ Consider using 'aggregators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosAggregators :: Lens.Lens' GetOpsSummary (Lude.Maybe (Lude.NonEmpty OpsAggregator))
gosAggregators = Lens.lens (aggregators :: GetOpsSummary -> Lude.Maybe (Lude.NonEmpty OpsAggregator)) (\s a -> s {aggregators = a} :: GetOpsSummary)
{-# DEPRECATED gosAggregators "Use generic-lens or generic-optics with 'aggregators' instead." #-}

-- | Specify the name of a resource data sync to get.
--
-- /Note:/ Consider using 'syncName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosSyncName :: Lens.Lens' GetOpsSummary (Lude.Maybe Lude.Text)
gosSyncName = Lens.lens (syncName :: GetOpsSummary -> Lude.Maybe Lude.Text) (\s a -> s {syncName = a} :: GetOpsSummary)
{-# DEPRECATED gosSyncName "Use generic-lens or generic-optics with 'syncName' instead." #-}

-- | Optional filters used to scope down the returned OpsItems.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosFilters :: Lens.Lens' GetOpsSummary (Lude.Maybe (Lude.NonEmpty OpsFilter))
gosFilters = Lens.lens (filters :: GetOpsSummary -> Lude.Maybe (Lude.NonEmpty OpsFilter)) (\s a -> s {filters = a} :: GetOpsSummary)
{-# DEPRECATED gosFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The OpsItem data type to return.
--
-- /Note:/ Consider using 'resultAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosResultAttributes :: Lens.Lens' GetOpsSummary (Lude.Maybe (Lude.NonEmpty OpsResultAttribute))
gosResultAttributes = Lens.lens (resultAttributes :: GetOpsSummary -> Lude.Maybe (Lude.NonEmpty OpsResultAttribute)) (\s a -> s {resultAttributes = a} :: GetOpsSummary)
{-# DEPRECATED gosResultAttributes "Use generic-lens or generic-optics with 'resultAttributes' instead." #-}

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosNextToken :: Lens.Lens' GetOpsSummary (Lude.Maybe Lude.Text)
gosNextToken = Lens.lens (nextToken :: GetOpsSummary -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetOpsSummary)
{-# DEPRECATED gosNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosMaxResults :: Lens.Lens' GetOpsSummary (Lude.Maybe Lude.Natural)
gosMaxResults = Lens.lens (maxResults :: GetOpsSummary -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetOpsSummary)
{-# DEPRECATED gosMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetOpsSummary where
  page rq rs
    | Page.stop (rs Lens.^. gosrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gosrsEntities) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gosNextToken Lens..~ rs Lens.^. gosrsNextToken

instance Lude.AWSRequest GetOpsSummary where
  type Rs GetOpsSummary = GetOpsSummaryResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetOpsSummaryResponse'
            Lude.<$> (x Lude..?> "Entities" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetOpsSummary where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetOpsSummary" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetOpsSummary where
  toJSON GetOpsSummary' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Aggregators" Lude..=) Lude.<$> aggregators,
            ("SyncName" Lude..=) Lude.<$> syncName,
            ("Filters" Lude..=) Lude.<$> filters,
            ("ResultAttributes" Lude..=) Lude.<$> resultAttributes,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetOpsSummary where
  toPath = Lude.const "/"

instance Lude.ToQuery GetOpsSummary where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetOpsSummaryResponse' smart constructor.
data GetOpsSummaryResponse = GetOpsSummaryResponse'
  { -- | The list of aggregated and filtered OpsItems.
    entities :: Lude.Maybe [OpsEntity],
    -- | The token for the next set of items to return. Use this token to get the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOpsSummaryResponse' with the minimum fields required to make a request.
--
-- * 'entities' - The list of aggregated and filtered OpsItems.
-- * 'nextToken' - The token for the next set of items to return. Use this token to get the next set of results.
-- * 'responseStatus' - The response status code.
mkGetOpsSummaryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetOpsSummaryResponse
mkGetOpsSummaryResponse pResponseStatus_ =
  GetOpsSummaryResponse'
    { entities = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of aggregated and filtered OpsItems.
--
-- /Note:/ Consider using 'entities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosrsEntities :: Lens.Lens' GetOpsSummaryResponse (Lude.Maybe [OpsEntity])
gosrsEntities = Lens.lens (entities :: GetOpsSummaryResponse -> Lude.Maybe [OpsEntity]) (\s a -> s {entities = a} :: GetOpsSummaryResponse)
{-# DEPRECATED gosrsEntities "Use generic-lens or generic-optics with 'entities' instead." #-}

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosrsNextToken :: Lens.Lens' GetOpsSummaryResponse (Lude.Maybe Lude.Text)
gosrsNextToken = Lens.lens (nextToken :: GetOpsSummaryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetOpsSummaryResponse)
{-# DEPRECATED gosrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosrsResponseStatus :: Lens.Lens' GetOpsSummaryResponse Lude.Int
gosrsResponseStatus = Lens.lens (responseStatus :: GetOpsSummaryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetOpsSummaryResponse)
{-# DEPRECATED gosrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
