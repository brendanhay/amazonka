{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListSteps (..)
    , mkListSteps
    -- ** Request lenses
    , lsClusterId
    , lsMarker
    , lsStepIds
    , lsStepStates

    -- * Destructuring the response
    , ListStepsResponse (..)
    , mkListStepsResponse
    -- ** Response lenses
    , lsrrsMarker
    , lsrrsSteps
    , lsrrsResponseStatus
    ) where

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
  { clusterId :: Types.ClusterId
    -- ^ The identifier of the cluster for which to list the steps.
  , marker :: Core.Maybe Types.Marker
    -- ^ The pagination token that indicates the next set of results to retrieve.
  , stepIds :: Core.Maybe [Types.XmlString]
    -- ^ The filter to limit the step list based on the identifier of the steps. You can specify a maximum of ten Step IDs. The character constraint applies to the overall length of the array.
  , stepStates :: Core.Maybe [Types.StepState]
    -- ^ The filter to limit the step list based on certain states.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSteps' value with any optional fields omitted.
mkListSteps
    :: Types.ClusterId -- ^ 'clusterId'
    -> ListSteps
mkListSteps clusterId
  = ListSteps'{clusterId, marker = Core.Nothing,
               stepIds = Core.Nothing, stepStates = Core.Nothing}

-- | The identifier of the cluster for which to list the steps.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsClusterId :: Lens.Lens' ListSteps Types.ClusterId
lsClusterId = Lens.field @"clusterId"
{-# INLINEABLE lsClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMarker :: Lens.Lens' ListSteps (Core.Maybe Types.Marker)
lsMarker = Lens.field @"marker"
{-# INLINEABLE lsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The filter to limit the step list based on the identifier of the steps. You can specify a maximum of ten Step IDs. The character constraint applies to the overall length of the array.
--
-- /Note:/ Consider using 'stepIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsStepIds :: Lens.Lens' ListSteps (Core.Maybe [Types.XmlString])
lsStepIds = Lens.field @"stepIds"
{-# INLINEABLE lsStepIds #-}
{-# DEPRECATED stepIds "Use generic-lens or generic-optics with 'stepIds' instead"  #-}

-- | The filter to limit the step list based on certain states.
--
-- /Note:/ Consider using 'stepStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsStepStates :: Lens.Lens' ListSteps (Core.Maybe [Types.StepState])
lsStepStates = Lens.field @"stepStates"
{-# INLINEABLE lsStepStates #-}
{-# DEPRECATED stepStates "Use generic-lens or generic-optics with 'stepStates' instead"  #-}

instance Core.ToQuery ListSteps where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListSteps where
        toHeaders ListSteps{..}
          = Core.pure ("X-Amz-Target", "ElasticMapReduce.ListSteps") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListSteps where
        toJSON ListSteps{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClusterId" Core..= clusterId),
                  ("Marker" Core..=) Core.<$> marker,
                  ("StepIds" Core..=) Core.<$> stepIds,
                  ("StepStates" Core..=) Core.<$> stepStates])

instance Core.AWSRequest ListSteps where
        type Rs ListSteps = ListStepsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListStepsResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*> x Core..:? "Steps" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListSteps where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"steps" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | This output contains the list of steps returned in reverse order. This means that the last step is the first element in the list.
--
-- /See:/ 'mkListStepsResponse' smart constructor.
data ListStepsResponse = ListStepsResponse'
  { marker :: Core.Maybe Types.Marker
    -- ^ The pagination token that indicates the next set of results to retrieve.
  , steps :: Core.Maybe [Types.StepSummary]
    -- ^ The filtered list of steps for the cluster.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListStepsResponse' value with any optional fields omitted.
mkListStepsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListStepsResponse
mkListStepsResponse responseStatus
  = ListStepsResponse'{marker = Core.Nothing, steps = Core.Nothing,
                       responseStatus}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsMarker :: Lens.Lens' ListStepsResponse (Core.Maybe Types.Marker)
lsrrsMarker = Lens.field @"marker"
{-# INLINEABLE lsrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The filtered list of steps for the cluster.
--
-- /Note:/ Consider using 'steps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsSteps :: Lens.Lens' ListStepsResponse (Core.Maybe [Types.StepSummary])
lsrrsSteps = Lens.field @"steps"
{-# INLINEABLE lsrrsSteps #-}
{-# DEPRECATED steps "Use generic-lens or generic-optics with 'steps' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsResponseStatus :: Lens.Lens' ListStepsResponse Core.Int
lsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
