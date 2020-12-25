{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DescribeContinuousExports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists exports as specified by ID. All continuous exports associated with your user account can be listed if you call @DescribeContinuousExports@ as is without passing any parameters.
--
-- This operation returns paginated results.
module Network.AWS.Discovery.DescribeContinuousExports
  ( -- * Creating a request
    DescribeContinuousExports (..),
    mkDescribeContinuousExports,

    -- ** Request lenses
    dceExportIds,
    dceMaxResults,
    dceNextToken,

    -- * Destructuring the response
    DescribeContinuousExportsResponse (..),
    mkDescribeContinuousExportsResponse,

    -- ** Response lenses
    dcerrsDescriptions,
    dcerrsNextToken,
    dcerrsResponseStatus,
  )
where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeContinuousExports' smart constructor.
data DescribeContinuousExports = DescribeContinuousExports'
  { -- | The unique IDs assigned to the exports.
    exportIds :: Core.Maybe [Types.ConfigurationsExportId],
    -- | A number between 1 and 100 specifying the maximum number of continuous export descriptions returned.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token from the previous call to @DescribeExportTasks@ .
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeContinuousExports' value with any optional fields omitted.
mkDescribeContinuousExports ::
  DescribeContinuousExports
mkDescribeContinuousExports =
  DescribeContinuousExports'
    { exportIds = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The unique IDs assigned to the exports.
--
-- /Note:/ Consider using 'exportIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceExportIds :: Lens.Lens' DescribeContinuousExports (Core.Maybe [Types.ConfigurationsExportId])
dceExportIds = Lens.field @"exportIds"
{-# DEPRECATED dceExportIds "Use generic-lens or generic-optics with 'exportIds' instead." #-}

-- | A number between 1 and 100 specifying the maximum number of continuous export descriptions returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceMaxResults :: Lens.Lens' DescribeContinuousExports (Core.Maybe Core.Natural)
dceMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dceMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token from the previous call to @DescribeExportTasks@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceNextToken :: Lens.Lens' DescribeContinuousExports (Core.Maybe Types.NextToken)
dceNextToken = Lens.field @"nextToken"
{-# DEPRECATED dceNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeContinuousExports where
  toJSON DescribeContinuousExports {..} =
    Core.object
      ( Core.catMaybes
          [ ("exportIds" Core..=) Core.<$> exportIds,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeContinuousExports where
  type
    Rs DescribeContinuousExports =
      DescribeContinuousExportsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSPoseidonService_V2015_11_01.DescribeContinuousExports"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContinuousExportsResponse'
            Core.<$> (x Core..:? "descriptions")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeContinuousExports where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"descriptions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeContinuousExportsResponse' smart constructor.
data DescribeContinuousExportsResponse = DescribeContinuousExportsResponse'
  { -- | A list of continuous export descriptions.
    descriptions :: Core.Maybe [Types.ContinuousExportDescription],
    -- | The token from the previous call to @DescribeExportTasks@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeContinuousExportsResponse' value with any optional fields omitted.
mkDescribeContinuousExportsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeContinuousExportsResponse
mkDescribeContinuousExportsResponse responseStatus =
  DescribeContinuousExportsResponse'
    { descriptions = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of continuous export descriptions.
--
-- /Note:/ Consider using 'descriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcerrsDescriptions :: Lens.Lens' DescribeContinuousExportsResponse (Core.Maybe [Types.ContinuousExportDescription])
dcerrsDescriptions = Lens.field @"descriptions"
{-# DEPRECATED dcerrsDescriptions "Use generic-lens or generic-optics with 'descriptions' instead." #-}

-- | The token from the previous call to @DescribeExportTasks@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcerrsNextToken :: Lens.Lens' DescribeContinuousExportsResponse (Core.Maybe Types.NextToken)
dcerrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcerrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcerrsResponseStatus :: Lens.Lens' DescribeContinuousExportsResponse Core.Int
dcerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
