{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.DeleteScalingPlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified scaling plan.
--
-- Deleting a scaling plan deletes the underlying 'ScalingInstruction' for all of the scalable resources that are covered by the plan.
-- If the plan has launched resources or has scaling activities in progress, you must delete those resources separately.
module Network.AWS.AutoScalingPlans.DeleteScalingPlan
    (
    -- * Creating a request
      DeleteScalingPlan (..)
    , mkDeleteScalingPlan
    -- ** Request lenses
    , dspScalingPlanName
    , dspScalingPlanVersion

    -- * Destructuring the response
    , DeleteScalingPlanResponse (..)
    , mkDeleteScalingPlanResponse
    -- ** Response lenses
    , dsprrsResponseStatus
    ) where

import qualified Network.AWS.AutoScalingPlans.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteScalingPlan' smart constructor.
data DeleteScalingPlan = DeleteScalingPlan'
  { scalingPlanName :: Types.ScalingPlanName
    -- ^ The name of the scaling plan.
  , scalingPlanVersion :: Core.Integer
    -- ^ The version number of the scaling plan.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteScalingPlan' value with any optional fields omitted.
mkDeleteScalingPlan
    :: Types.ScalingPlanName -- ^ 'scalingPlanName'
    -> Core.Integer -- ^ 'scalingPlanVersion'
    -> DeleteScalingPlan
mkDeleteScalingPlan scalingPlanName scalingPlanVersion
  = DeleteScalingPlan'{scalingPlanName, scalingPlanVersion}

-- | The name of the scaling plan.
--
-- /Note:/ Consider using 'scalingPlanName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspScalingPlanName :: Lens.Lens' DeleteScalingPlan Types.ScalingPlanName
dspScalingPlanName = Lens.field @"scalingPlanName"
{-# INLINEABLE dspScalingPlanName #-}
{-# DEPRECATED scalingPlanName "Use generic-lens or generic-optics with 'scalingPlanName' instead"  #-}

-- | The version number of the scaling plan.
--
-- /Note:/ Consider using 'scalingPlanVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspScalingPlanVersion :: Lens.Lens' DeleteScalingPlan Core.Integer
dspScalingPlanVersion = Lens.field @"scalingPlanVersion"
{-# INLINEABLE dspScalingPlanVersion #-}
{-# DEPRECATED scalingPlanVersion "Use generic-lens or generic-optics with 'scalingPlanVersion' instead"  #-}

instance Core.ToQuery DeleteScalingPlan where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteScalingPlan where
        toHeaders DeleteScalingPlan{..}
          = Core.pure
              ("X-Amz-Target",
               "AnyScaleScalingPlannerFrontendService.DeleteScalingPlan")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteScalingPlan where
        toJSON DeleteScalingPlan{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ScalingPlanName" Core..= scalingPlanName),
                  Core.Just ("ScalingPlanVersion" Core..= scalingPlanVersion)])

instance Core.AWSRequest DeleteScalingPlan where
        type Rs DeleteScalingPlan = DeleteScalingPlanResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteScalingPlanResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteScalingPlanResponse' smart constructor.
newtype DeleteScalingPlanResponse = DeleteScalingPlanResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteScalingPlanResponse' value with any optional fields omitted.
mkDeleteScalingPlanResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteScalingPlanResponse
mkDeleteScalingPlanResponse responseStatus
  = DeleteScalingPlanResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsResponseStatus :: Lens.Lens' DeleteScalingPlanResponse Core.Int
dsprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
