{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConformancePacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of one or more conformance packs.
module Network.AWS.Config.DescribeConformancePacks
  ( -- * Creating a request
    DescribeConformancePacks (..),
    mkDescribeConformancePacks,

    -- ** Request lenses
    dcpConformancePackNames,
    dcpLimit,
    dcpNextToken,

    -- * Destructuring the response
    DescribeConformancePacksResponse (..),
    mkDescribeConformancePacksResponse,

    -- ** Response lenses
    dcprrsConformancePackDetails,
    dcprrsNextToken,
    dcprrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeConformancePacks' smart constructor.
data DescribeConformancePacks = DescribeConformancePacks'
  { -- | Comma-separated list of conformance pack names for which you want details. If you do not specify any names, AWS Config returns details for all your conformance packs.
    conformancePackNames :: Core.Maybe [Types.ConformancePackName],
    -- | The maximum number of conformance packs returned on each page.
    limit :: Core.Maybe Core.Natural,
    -- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConformancePacks' value with any optional fields omitted.
mkDescribeConformancePacks ::
  DescribeConformancePacks
mkDescribeConformancePacks =
  DescribeConformancePacks'
    { conformancePackNames = Core.Nothing,
      limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Comma-separated list of conformance pack names for which you want details. If you do not specify any names, AWS Config returns details for all your conformance packs.
--
-- /Note:/ Consider using 'conformancePackNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpConformancePackNames :: Lens.Lens' DescribeConformancePacks (Core.Maybe [Types.ConformancePackName])
dcpConformancePackNames = Lens.field @"conformancePackNames"
{-# DEPRECATED dcpConformancePackNames "Use generic-lens or generic-optics with 'conformancePackNames' instead." #-}

-- | The maximum number of conformance packs returned on each page.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpLimit :: Lens.Lens' DescribeConformancePacks (Core.Maybe Core.Natural)
dcpLimit = Lens.field @"limit"
{-# DEPRECATED dcpLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpNextToken :: Lens.Lens' DescribeConformancePacks (Core.Maybe Types.NextToken)
dcpNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeConformancePacks where
  toJSON DescribeConformancePacks {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConformancePackNames" Core..=) Core.<$> conformancePackNames,
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeConformancePacks where
  type Rs DescribeConformancePacks = DescribeConformancePacksResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StarlingDoveService.DescribeConformancePacks")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConformancePacksResponse'
            Core.<$> (x Core..:? "ConformancePackDetails")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeConformancePacksResponse' smart constructor.
data DescribeConformancePacksResponse = DescribeConformancePacksResponse'
  { -- | Returns a list of @ConformancePackDetail@ objects.
    conformancePackDetails :: Core.Maybe [Types.ConformancePackDetail],
    -- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeConformancePacksResponse' value with any optional fields omitted.
mkDescribeConformancePacksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeConformancePacksResponse
mkDescribeConformancePacksResponse responseStatus =
  DescribeConformancePacksResponse'
    { conformancePackDetails =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Returns a list of @ConformancePackDetail@ objects.
--
-- /Note:/ Consider using 'conformancePackDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsConformancePackDetails :: Lens.Lens' DescribeConformancePacksResponse (Core.Maybe [Types.ConformancePackDetail])
dcprrsConformancePackDetails = Lens.field @"conformancePackDetails"
{-# DEPRECATED dcprrsConformancePackDetails "Use generic-lens or generic-optics with 'conformancePackDetails' instead." #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsNextToken :: Lens.Lens' DescribeConformancePacksResponse (Core.Maybe Types.NextToken)
dcprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsResponseStatus :: Lens.Lens' DescribeConformancePacksResponse Core.Int
dcprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
