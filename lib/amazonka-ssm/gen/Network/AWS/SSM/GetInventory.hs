{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetInventory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Query inventory information.
--
-- This operation returns paginated results.
module Network.AWS.SSM.GetInventory
  ( -- * Creating a request
    GetInventory (..),
    mkGetInventory,

    -- ** Request lenses
    giAggregators,
    giFilters,
    giResultAttributes,
    giNextToken,
    giMaxResults,

    -- * Destructuring the response
    GetInventoryResponse (..),
    mkGetInventoryResponse,

    -- ** Response lenses
    girsEntities,
    girsNextToken,
    girsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetInventory' smart constructor.
data GetInventory = GetInventory'
  { -- | Returns counts of inventory types based on one or more expressions. For example, if you aggregate by using an expression that uses the @AWS:InstanceInformation.PlatformType@ type, you can see a count of how many Windows and Linux instances exist in your inventoried fleet.
    aggregators :: Lude.Maybe (Lude.NonEmpty InventoryAggregator),
    -- | One or more filters. Use a filter to return a more specific list of results.
    filters :: Lude.Maybe (Lude.NonEmpty InventoryFilter),
    -- | The list of inventory item types to return.
    resultAttributes :: Lude.Maybe (Lude.NonEmpty ResultAttribute),
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInventory' with the minimum fields required to make a request.
--
-- * 'aggregators' - Returns counts of inventory types based on one or more expressions. For example, if you aggregate by using an expression that uses the @AWS:InstanceInformation.PlatformType@ type, you can see a count of how many Windows and Linux instances exist in your inventoried fleet.
-- * 'filters' - One or more filters. Use a filter to return a more specific list of results.
-- * 'resultAttributes' - The list of inventory item types to return.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
mkGetInventory ::
  GetInventory
mkGetInventory =
  GetInventory'
    { aggregators = Lude.Nothing,
      filters = Lude.Nothing,
      resultAttributes = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Returns counts of inventory types based on one or more expressions. For example, if you aggregate by using an expression that uses the @AWS:InstanceInformation.PlatformType@ type, you can see a count of how many Windows and Linux instances exist in your inventoried fleet.
--
-- /Note:/ Consider using 'aggregators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giAggregators :: Lens.Lens' GetInventory (Lude.Maybe (Lude.NonEmpty InventoryAggregator))
giAggregators = Lens.lens (aggregators :: GetInventory -> Lude.Maybe (Lude.NonEmpty InventoryAggregator)) (\s a -> s {aggregators = a} :: GetInventory)
{-# DEPRECATED giAggregators "Use generic-lens or generic-optics with 'aggregators' instead." #-}

-- | One or more filters. Use a filter to return a more specific list of results.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giFilters :: Lens.Lens' GetInventory (Lude.Maybe (Lude.NonEmpty InventoryFilter))
giFilters = Lens.lens (filters :: GetInventory -> Lude.Maybe (Lude.NonEmpty InventoryFilter)) (\s a -> s {filters = a} :: GetInventory)
{-# DEPRECATED giFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The list of inventory item types to return.
--
-- /Note:/ Consider using 'resultAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giResultAttributes :: Lens.Lens' GetInventory (Lude.Maybe (Lude.NonEmpty ResultAttribute))
giResultAttributes = Lens.lens (resultAttributes :: GetInventory -> Lude.Maybe (Lude.NonEmpty ResultAttribute)) (\s a -> s {resultAttributes = a} :: GetInventory)
{-# DEPRECATED giResultAttributes "Use generic-lens or generic-optics with 'resultAttributes' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giNextToken :: Lens.Lens' GetInventory (Lude.Maybe Lude.Text)
giNextToken = Lens.lens (nextToken :: GetInventory -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetInventory)
{-# DEPRECATED giNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giMaxResults :: Lens.Lens' GetInventory (Lude.Maybe Lude.Natural)
giMaxResults = Lens.lens (maxResults :: GetInventory -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetInventory)
{-# DEPRECATED giMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetInventory where
  page rq rs
    | Page.stop (rs Lens.^. girsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. girsEntities) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& giNextToken Lens..~ rs Lens.^. girsNextToken

instance Lude.AWSRequest GetInventory where
  type Rs GetInventory = GetInventoryResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInventoryResponse'
            Lude.<$> (x Lude..?> "Entities" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInventory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetInventory" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetInventory where
  toJSON GetInventory' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Aggregators" Lude..=) Lude.<$> aggregators,
            ("Filters" Lude..=) Lude.<$> filters,
            ("ResultAttributes" Lude..=) Lude.<$> resultAttributes,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetInventory where
  toPath = Lude.const "/"

instance Lude.ToQuery GetInventory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInventoryResponse' smart constructor.
data GetInventoryResponse = GetInventoryResponse'
  { -- | Collection of inventory entities such as a collection of instance inventory.
    entities :: Lude.Maybe [InventoryResultEntity],
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInventoryResponse' with the minimum fields required to make a request.
--
-- * 'entities' - Collection of inventory entities such as a collection of instance inventory.
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
mkGetInventoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInventoryResponse
mkGetInventoryResponse pResponseStatus_ =
  GetInventoryResponse'
    { entities = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Collection of inventory entities such as a collection of instance inventory.
--
-- /Note:/ Consider using 'entities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsEntities :: Lens.Lens' GetInventoryResponse (Lude.Maybe [InventoryResultEntity])
girsEntities = Lens.lens (entities :: GetInventoryResponse -> Lude.Maybe [InventoryResultEntity]) (\s a -> s {entities = a} :: GetInventoryResponse)
{-# DEPRECATED girsEntities "Use generic-lens or generic-optics with 'entities' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsNextToken :: Lens.Lens' GetInventoryResponse (Lude.Maybe Lude.Text)
girsNextToken = Lens.lens (nextToken :: GetInventoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetInventoryResponse)
{-# DEPRECATED girsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsResponseStatus :: Lens.Lens' GetInventoryResponse Lude.Int
girsResponseStatus = Lens.lens (responseStatus :: GetInventoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInventoryResponse)
{-# DEPRECATED girsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
