{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListSteps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of steps for the cluster in reverse order unless you specify @stepIds@ with the request of filter by @StepStates@ . You can specify a maximum of ten @stepIDs@ .
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListSteps
  ( -- * Creating a request
    ListSteps (..),
    mkListSteps,

    -- ** Request lenses
    lsClusterId,
    lsMarker,
    lsStepIds,
    lsStepStates,

    -- * Destructuring the response
    ListStepsResponse (..),
    mkListStepsResponse,

    -- ** Response lenses
    lsrrsMarker,
    lsrrsSteps,
    lsrrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input determines which steps to list.
--
-- /See:/ 'mkListSteps' smart constructor.
data ListSteps = ListSteps'
  { -- | The identifier of the cluster for which to list the steps.
    clusterId :: Types.ClusterId,
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Core.Maybe Types.Marker,
    -- | The filter to limit the step list based on the identifier of the steps. You can specify a maximum of ten Step IDs. The character constraint applies to the overall length of the array.
    stepIds :: Core.Maybe [Types.XmlString],
    -- | The filter to limit the step list based on certain states.
    stepStates :: Core.Maybe [Types.StepState]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSteps' value with any optional fields omitted.
mkListSteps ::
  -- | 'clusterId'
  Types.ClusterId ->
  ListSteps
mkListSteps clusterId =
  ListSteps'
    { clusterId,
      marker = Core.Nothing,
      stepIds = Core.Nothing,
      stepStates = Core.Nothing
    }

-- | The identifier of the cluster for which to list the steps.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsClusterId :: Lens.Lens' ListSteps Types.ClusterId
lsClusterId = Lens.field @"clusterId"
{-# DEPRECATED lsClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMarker :: Lens.Lens' ListSteps (Core.Maybe Types.Marker)
lsMarker = Lens.field @"marker"
{-# DEPRECATED lsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The filter to limit the step list based on the identifier of the steps. You can specify a maximum of ten Step IDs. The character constraint applies to the overall length of the array.
--
-- /Note:/ Consider using 'stepIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsStepIds :: Lens.Lens' ListSteps (Core.Maybe [Types.XmlString])
lsStepIds = Lens.field @"stepIds"
{-# DEPRECATED lsStepIds "Use generic-lens or generic-optics with 'stepIds' instead." #-}

-- | The filter to limit the step list based on certain states.
--
-- /Note:/ Consider using 'stepStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsStepStates :: Lens.Lens' ListSteps (Core.Maybe [Types.StepState])
lsStepStates = Lens.field @"stepStates"
{-# DEPRECATED lsStepStates "Use generic-lens or generic-optics with 'stepStates' instead." #-}

instance Core.FromJSON ListSteps where
  toJSON ListSteps {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterId" Core..= clusterId),
            ("Marker" Core..=) Core.<$> marker,
            ("StepIds" Core..=) Core.<$> stepIds,
            ("StepStates" Core..=) Core.<$> stepStates
          ]
      )

instance Core.AWSRequest ListSteps where
  type Rs ListSteps = ListStepsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "ElasticMapReduce.ListSteps")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStepsResponse'
            Core.<$> (x Core..:? "Marker")
            Core.<*> (x Core..:? "Steps")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListSteps where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"steps" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | This output contains the list of steps returned in reverse order. This means that the last step is the first element in the list.
--
-- /See:/ 'mkListStepsResponse' smart constructor.
data ListStepsResponse = ListStepsResponse'
  { -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Core.Maybe Types.Marker,
    -- | The filtered list of steps for the cluster.
    steps :: Core.Maybe [Types.StepSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListStepsResponse' value with any optional fields omitted.
mkListStepsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListStepsResponse
mkListStepsResponse responseStatus =
  ListStepsResponse'
    { marker = Core.Nothing,
      steps = Core.Nothing,
      responseStatus
    }

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsMarker :: Lens.Lens' ListStepsResponse (Core.Maybe Types.Marker)
lsrrsMarker = Lens.field @"marker"
{-# DEPRECATED lsrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The filtered list of steps for the cluster.
--
-- /Note:/ Consider using 'steps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsSteps :: Lens.Lens' ListStepsResponse (Core.Maybe [Types.StepSummary])
lsrrsSteps = Lens.field @"steps"
{-# DEPRECATED lsrrsSteps "Use generic-lens or generic-optics with 'steps' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsResponseStatus :: Lens.Lens' ListStepsResponse Core.Int
lsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
