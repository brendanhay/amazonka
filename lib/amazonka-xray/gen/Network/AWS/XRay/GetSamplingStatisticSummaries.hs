{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetSamplingStatisticSummaries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about recent sampling results for all sampling rules.
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetSamplingStatisticSummaries
  ( -- * Creating a request
    GetSamplingStatisticSummaries (..),
    mkGetSamplingStatisticSummaries,

    -- ** Request lenses
    gsssNextToken,

    -- * Destructuring the response
    GetSamplingStatisticSummariesResponse (..),
    mkGetSamplingStatisticSummariesResponse,

    -- ** Response lenses
    gsssrrsNextToken,
    gsssrrsSamplingStatisticSummaries,
    gsssrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkGetSamplingStatisticSummaries' smart constructor.
newtype GetSamplingStatisticSummaries = GetSamplingStatisticSummaries'
  { -- | Pagination token.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSamplingStatisticSummaries' value with any optional fields omitted.
mkGetSamplingStatisticSummaries ::
  GetSamplingStatisticSummaries
mkGetSamplingStatisticSummaries =
  GetSamplingStatisticSummaries' {nextToken = Core.Nothing}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsssNextToken :: Lens.Lens' GetSamplingStatisticSummaries (Core.Maybe Types.String)
gsssNextToken = Lens.field @"nextToken"
{-# DEPRECATED gsssNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetSamplingStatisticSummaries where
  toJSON GetSamplingStatisticSummaries {..} =
    Core.object
      (Core.catMaybes [("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetSamplingStatisticSummaries where
  type
    Rs GetSamplingStatisticSummaries =
      GetSamplingStatisticSummariesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/SamplingStatisticSummaries",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSamplingStatisticSummariesResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "SamplingStatisticSummaries")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetSamplingStatisticSummaries where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"samplingStatisticSummaries" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetSamplingStatisticSummariesResponse' smart constructor.
data GetSamplingStatisticSummariesResponse = GetSamplingStatisticSummariesResponse'
  { -- | Pagination token.
    nextToken :: Core.Maybe Types.String,
    -- | Information about the number of requests instrumented for each sampling rule.
    samplingStatisticSummaries :: Core.Maybe [Types.SamplingStatisticSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetSamplingStatisticSummariesResponse' value with any optional fields omitted.
mkGetSamplingStatisticSummariesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSamplingStatisticSummariesResponse
mkGetSamplingStatisticSummariesResponse responseStatus =
  GetSamplingStatisticSummariesResponse'
    { nextToken = Core.Nothing,
      samplingStatisticSummaries = Core.Nothing,
      responseStatus
    }

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsssrrsNextToken :: Lens.Lens' GetSamplingStatisticSummariesResponse (Core.Maybe Types.String)
gsssrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gsssrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the number of requests instrumented for each sampling rule.
--
-- /Note:/ Consider using 'samplingStatisticSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsssrrsSamplingStatisticSummaries :: Lens.Lens' GetSamplingStatisticSummariesResponse (Core.Maybe [Types.SamplingStatisticSummary])
gsssrrsSamplingStatisticSummaries = Lens.field @"samplingStatisticSummaries"
{-# DEPRECATED gsssrrsSamplingStatisticSummaries "Use generic-lens or generic-optics with 'samplingStatisticSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsssrrsResponseStatus :: Lens.Lens' GetSamplingStatisticSummariesResponse Core.Int
gsssrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsssrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
