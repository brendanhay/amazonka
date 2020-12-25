{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribePatchBaselines
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the patch baselines in your AWS account.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribePatchBaselines
  ( -- * Creating a request
    DescribePatchBaselines (..),
    mkDescribePatchBaselines,

    -- ** Request lenses
    dpbFilters,
    dpbMaxResults,
    dpbNextToken,

    -- * Destructuring the response
    DescribePatchBaselinesResponse (..),
    mkDescribePatchBaselinesResponse,

    -- ** Response lenses
    dpbrfrsBaselineIdentities,
    dpbrfrsNextToken,
    dpbrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribePatchBaselines' smart constructor.
data DescribePatchBaselines = DescribePatchBaselines'
  { -- | Each element in the array is a structure containing:
    --
    -- Key: (string, "NAME_PREFIX" or "OWNER")
    -- Value: (array of strings, exactly 1 entry, between 1 and 255 characters)
    filters :: Core.Maybe [Types.PatchOrchestratorFilter],
    -- | The maximum number of patch baselines to return (per page).
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePatchBaselines' value with any optional fields omitted.
mkDescribePatchBaselines ::
  DescribePatchBaselines
mkDescribePatchBaselines =
  DescribePatchBaselines'
    { filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Each element in the array is a structure containing:
--
-- Key: (string, "NAME_PREFIX" or "OWNER")
-- Value: (array of strings, exactly 1 entry, between 1 and 255 characters)
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbFilters :: Lens.Lens' DescribePatchBaselines (Core.Maybe [Types.PatchOrchestratorFilter])
dpbFilters = Lens.field @"filters"
{-# DEPRECATED dpbFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of patch baselines to return (per page).
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbMaxResults :: Lens.Lens' DescribePatchBaselines (Core.Maybe Core.Natural)
dpbMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dpbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbNextToken :: Lens.Lens' DescribePatchBaselines (Core.Maybe Types.NextToken)
dpbNextToken = Lens.field @"nextToken"
{-# DEPRECATED dpbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribePatchBaselines where
  toJSON DescribePatchBaselines {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribePatchBaselines where
  type Rs DescribePatchBaselines = DescribePatchBaselinesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.DescribePatchBaselines")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePatchBaselinesResponse'
            Core.<$> (x Core..:? "BaselineIdentities")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribePatchBaselines where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"baselineIdentities" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribePatchBaselinesResponse' smart constructor.
data DescribePatchBaselinesResponse = DescribePatchBaselinesResponse'
  { -- | An array of PatchBaselineIdentity elements.
    baselineIdentities :: Core.Maybe [Types.PatchBaselineIdentity],
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePatchBaselinesResponse' value with any optional fields omitted.
mkDescribePatchBaselinesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribePatchBaselinesResponse
mkDescribePatchBaselinesResponse responseStatus =
  DescribePatchBaselinesResponse'
    { baselineIdentities =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of PatchBaselineIdentity elements.
--
-- /Note:/ Consider using 'baselineIdentities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbrfrsBaselineIdentities :: Lens.Lens' DescribePatchBaselinesResponse (Core.Maybe [Types.PatchBaselineIdentity])
dpbrfrsBaselineIdentities = Lens.field @"baselineIdentities"
{-# DEPRECATED dpbrfrsBaselineIdentities "Use generic-lens or generic-optics with 'baselineIdentities' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbrfrsNextToken :: Lens.Lens' DescribePatchBaselinesResponse (Core.Maybe Types.NextToken)
dpbrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dpbrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbrfrsResponseStatus :: Lens.Lens' DescribePatchBaselinesResponse Core.Int
dpbrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpbrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
