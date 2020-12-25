{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeInventoryDeletions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a specific delete inventory operation.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInventoryDeletions
  ( -- * Creating a request
    DescribeInventoryDeletions (..),
    mkDescribeInventoryDeletions,

    -- ** Request lenses
    didDeletionId,
    didMaxResults,
    didNextToken,

    -- * Destructuring the response
    DescribeInventoryDeletionsResponse (..),
    mkDescribeInventoryDeletionsResponse,

    -- ** Response lenses
    didrrsInventoryDeletions,
    didrrsNextToken,
    didrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeInventoryDeletions' smart constructor.
data DescribeInventoryDeletions = DescribeInventoryDeletions'
  { -- | Specify the delete inventory ID for which you want information. This ID was returned by the @DeleteInventory@ action.
    deletionId :: Core.Maybe Types.UUID,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | A token to start the list. Use this token to get the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInventoryDeletions' value with any optional fields omitted.
mkDescribeInventoryDeletions ::
  DescribeInventoryDeletions
mkDescribeInventoryDeletions =
  DescribeInventoryDeletions'
    { deletionId = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Specify the delete inventory ID for which you want information. This ID was returned by the @DeleteInventory@ action.
--
-- /Note:/ Consider using 'deletionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didDeletionId :: Lens.Lens' DescribeInventoryDeletions (Core.Maybe Types.UUID)
didDeletionId = Lens.field @"deletionId"
{-# DEPRECATED didDeletionId "Use generic-lens or generic-optics with 'deletionId' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didMaxResults :: Lens.Lens' DescribeInventoryDeletions (Core.Maybe Core.Natural)
didMaxResults = Lens.field @"maxResults"
{-# DEPRECATED didMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didNextToken :: Lens.Lens' DescribeInventoryDeletions (Core.Maybe Types.NextToken)
didNextToken = Lens.field @"nextToken"
{-# DEPRECATED didNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeInventoryDeletions where
  toJSON DescribeInventoryDeletions {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeletionId" Core..=) Core.<$> deletionId,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeInventoryDeletions where
  type
    Rs DescribeInventoryDeletions =
      DescribeInventoryDeletionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.DescribeInventoryDeletions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInventoryDeletionsResponse'
            Core.<$> (x Core..:? "InventoryDeletions")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeInventoryDeletions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"inventoryDeletions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeInventoryDeletionsResponse' smart constructor.
data DescribeInventoryDeletionsResponse = DescribeInventoryDeletionsResponse'
  { -- | A list of status items for deleted inventory.
    inventoryDeletions :: Core.Maybe [Types.InventoryDeletionStatusItem],
    -- | The token for the next set of items to return. Use this token to get the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeInventoryDeletionsResponse' value with any optional fields omitted.
mkDescribeInventoryDeletionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeInventoryDeletionsResponse
mkDescribeInventoryDeletionsResponse responseStatus =
  DescribeInventoryDeletionsResponse'
    { inventoryDeletions =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of status items for deleted inventory.
--
-- /Note:/ Consider using 'inventoryDeletions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrrsInventoryDeletions :: Lens.Lens' DescribeInventoryDeletionsResponse (Core.Maybe [Types.InventoryDeletionStatusItem])
didrrsInventoryDeletions = Lens.field @"inventoryDeletions"
{-# DEPRECATED didrrsInventoryDeletions "Use generic-lens or generic-optics with 'inventoryDeletions' instead." #-}

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrrsNextToken :: Lens.Lens' DescribeInventoryDeletionsResponse (Core.Maybe Types.NextToken)
didrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED didrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrrsResponseStatus :: Lens.Lens' DescribeInventoryDeletionsResponse Core.Int
didrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED didrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
