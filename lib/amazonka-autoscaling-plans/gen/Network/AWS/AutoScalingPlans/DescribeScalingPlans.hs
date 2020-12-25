{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.DescribeScalingPlans
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your scaling plans.
--
-- This operation returns paginated results.
module Network.AWS.AutoScalingPlans.DescribeScalingPlans
  ( -- * Creating a request
    DescribeScalingPlans (..),
    mkDescribeScalingPlans,

    -- ** Request lenses
    dApplicationSources,
    dMaxResults,
    dNextToken,
    dScalingPlanNames,
    dScalingPlanVersion,

    -- * Destructuring the response
    DescribeScalingPlansResponse (..),
    mkDescribeScalingPlansResponse,

    -- ** Response lenses
    drsNextToken,
    drsScalingPlans,
    drsResponseStatus,
  )
where

import qualified Network.AWS.AutoScalingPlans.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeScalingPlans' smart constructor.
data DescribeScalingPlans = DescribeScalingPlans'
  { -- | The sources for the applications (up to 10). If you specify scaling plan names, you cannot specify application sources.
    applicationSources :: Core.Maybe [Types.ApplicationSource],
    -- | The maximum number of scalable resources to return. This value can be between 1 and 50. The default value is 50.
    maxResults :: Core.Maybe Core.Int,
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The names of the scaling plans (up to 10). If you specify application sources, you cannot specify scaling plan names.
    scalingPlanNames :: Core.Maybe [Types.ScalingPlanName],
    -- | The version number of the scaling plan. If you specify a scaling plan version, you must also specify a scaling plan name.
    scalingPlanVersion :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeScalingPlans' value with any optional fields omitted.
mkDescribeScalingPlans ::
  DescribeScalingPlans
mkDescribeScalingPlans =
  DescribeScalingPlans'
    { applicationSources = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      scalingPlanNames = Core.Nothing,
      scalingPlanVersion = Core.Nothing
    }

-- | The sources for the applications (up to 10). If you specify scaling plan names, you cannot specify application sources.
--
-- /Note:/ Consider using 'applicationSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dApplicationSources :: Lens.Lens' DescribeScalingPlans (Core.Maybe [Types.ApplicationSource])
dApplicationSources = Lens.field @"applicationSources"
{-# DEPRECATED dApplicationSources "Use generic-lens or generic-optics with 'applicationSources' instead." #-}

-- | The maximum number of scalable resources to return. This value can be between 1 and 50. The default value is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxResults :: Lens.Lens' DescribeScalingPlans (Core.Maybe Core.Int)
dMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeScalingPlans (Core.Maybe Types.NextToken)
dNextToken = Lens.field @"nextToken"
{-# DEPRECATED dNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The names of the scaling plans (up to 10). If you specify application sources, you cannot specify scaling plan names.
--
-- /Note:/ Consider using 'scalingPlanNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScalingPlanNames :: Lens.Lens' DescribeScalingPlans (Core.Maybe [Types.ScalingPlanName])
dScalingPlanNames = Lens.field @"scalingPlanNames"
{-# DEPRECATED dScalingPlanNames "Use generic-lens or generic-optics with 'scalingPlanNames' instead." #-}

-- | The version number of the scaling plan. If you specify a scaling plan version, you must also specify a scaling plan name.
--
-- /Note:/ Consider using 'scalingPlanVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScalingPlanVersion :: Lens.Lens' DescribeScalingPlans (Core.Maybe Core.Integer)
dScalingPlanVersion = Lens.field @"scalingPlanVersion"
{-# DEPRECATED dScalingPlanVersion "Use generic-lens or generic-optics with 'scalingPlanVersion' instead." #-}

instance Core.FromJSON DescribeScalingPlans where
  toJSON DescribeScalingPlans {..} =
    Core.object
      ( Core.catMaybes
          [ ("ApplicationSources" Core..=) Core.<$> applicationSources,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("ScalingPlanNames" Core..=) Core.<$> scalingPlanNames,
            ("ScalingPlanVersion" Core..=) Core.<$> scalingPlanVersion
          ]
      )

instance Core.AWSRequest DescribeScalingPlans where
  type Rs DescribeScalingPlans = DescribeScalingPlansResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AnyScaleScalingPlannerFrontendService.DescribeScalingPlans"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeScalingPlansResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "ScalingPlans")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeScalingPlans where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"scalingPlans" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeScalingPlansResponse' smart constructor.
data DescribeScalingPlansResponse = DescribeScalingPlansResponse'
  { -- | The token required to get the next set of results. This value is @null@ if there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Information about the scaling plans.
    scalingPlans :: Core.Maybe [Types.ScalingPlan],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeScalingPlansResponse' value with any optional fields omitted.
mkDescribeScalingPlansResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeScalingPlansResponse
mkDescribeScalingPlansResponse responseStatus =
  DescribeScalingPlansResponse'
    { nextToken = Core.Nothing,
      scalingPlans = Core.Nothing,
      responseStatus
    }

-- | The token required to get the next set of results. This value is @null@ if there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeScalingPlansResponse (Core.Maybe Types.NextToken)
drsNextToken = Lens.field @"nextToken"
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the scaling plans.
--
-- /Note:/ Consider using 'scalingPlans' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsScalingPlans :: Lens.Lens' DescribeScalingPlansResponse (Core.Maybe [Types.ScalingPlan])
drsScalingPlans = Lens.field @"scalingPlans"
{-# DEPRECATED drsScalingPlans "Use generic-lens or generic-optics with 'scalingPlans' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeScalingPlansResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
