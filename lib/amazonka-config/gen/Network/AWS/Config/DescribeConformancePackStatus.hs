{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConformancePackStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides one or more conformance packs deployment status.
module Network.AWS.Config.DescribeConformancePackStatus
  ( -- * Creating a request
    DescribeConformancePackStatus (..),
    mkDescribeConformancePackStatus,

    -- ** Request lenses
    dcpsConformancePackNames,
    dcpsLimit,
    dcpsNextToken,

    -- * Destructuring the response
    DescribeConformancePackStatusResponse (..),
    mkDescribeConformancePackStatusResponse,

    -- ** Response lenses
    dcpsrrsConformancePackStatusDetails,
    dcpsrrsNextToken,
    dcpsrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeConformancePackStatus' smart constructor.
data DescribeConformancePackStatus = DescribeConformancePackStatus'
  { -- | Comma-separated list of conformance pack names.
    conformancePackNames :: Core.Maybe [Types.ConformancePackName],
    -- | The maximum number of conformance packs status returned on each page.
    limit :: Core.Maybe Core.Natural,
    -- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConformancePackStatus' value with any optional fields omitted.
mkDescribeConformancePackStatus ::
  DescribeConformancePackStatus
mkDescribeConformancePackStatus =
  DescribeConformancePackStatus'
    { conformancePackNames =
        Core.Nothing,
      limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Comma-separated list of conformance pack names.
--
-- /Note:/ Consider using 'conformancePackNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsConformancePackNames :: Lens.Lens' DescribeConformancePackStatus (Core.Maybe [Types.ConformancePackName])
dcpsConformancePackNames = Lens.field @"conformancePackNames"
{-# DEPRECATED dcpsConformancePackNames "Use generic-lens or generic-optics with 'conformancePackNames' instead." #-}

-- | The maximum number of conformance packs status returned on each page.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsLimit :: Lens.Lens' DescribeConformancePackStatus (Core.Maybe Core.Natural)
dcpsLimit = Lens.field @"limit"
{-# DEPRECATED dcpsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsNextToken :: Lens.Lens' DescribeConformancePackStatus (Core.Maybe Types.NextToken)
dcpsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcpsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeConformancePackStatus where
  toJSON DescribeConformancePackStatus {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConformancePackNames" Core..=) Core.<$> conformancePackNames,
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeConformancePackStatus where
  type
    Rs DescribeConformancePackStatus =
      DescribeConformancePackStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.DescribeConformancePackStatus"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConformancePackStatusResponse'
            Core.<$> (x Core..:? "ConformancePackStatusDetails")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeConformancePackStatusResponse' smart constructor.
data DescribeConformancePackStatusResponse = DescribeConformancePackStatusResponse'
  { -- | A list of @ConformancePackStatusDetail@ objects.
    conformancePackStatusDetails :: Core.Maybe [Types.ConformancePackStatusDetail],
    -- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeConformancePackStatusResponse' value with any optional fields omitted.
mkDescribeConformancePackStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeConformancePackStatusResponse
mkDescribeConformancePackStatusResponse responseStatus =
  DescribeConformancePackStatusResponse'
    { conformancePackStatusDetails =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of @ConformancePackStatusDetail@ objects.
--
-- /Note:/ Consider using 'conformancePackStatusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrrsConformancePackStatusDetails :: Lens.Lens' DescribeConformancePackStatusResponse (Core.Maybe [Types.ConformancePackStatusDetail])
dcpsrrsConformancePackStatusDetails = Lens.field @"conformancePackStatusDetails"
{-# DEPRECATED dcpsrrsConformancePackStatusDetails "Use generic-lens or generic-optics with 'conformancePackStatusDetails' instead." #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrrsNextToken :: Lens.Lens' DescribeConformancePackStatusResponse (Core.Maybe Types.NextToken)
dcpsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcpsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrrsResponseStatus :: Lens.Lens' DescribeConformancePackStatusResponse Core.Int
dcpsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcpsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
