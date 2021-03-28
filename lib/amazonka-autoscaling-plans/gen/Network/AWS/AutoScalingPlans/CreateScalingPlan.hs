{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.CreateScalingPlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a scaling plan.
module Network.AWS.AutoScalingPlans.CreateScalingPlan
    (
    -- * Creating a request
      CreateScalingPlan (..)
    , mkCreateScalingPlan
    -- ** Request lenses
    , cspScalingPlanName
    , cspApplicationSource
    , cspScalingInstructions

    -- * Destructuring the response
    , CreateScalingPlanResponse (..)
    , mkCreateScalingPlanResponse
    -- ** Response lenses
    , csprrsScalingPlanVersion
    , csprrsResponseStatus
    ) where

import qualified Network.AWS.AutoScalingPlans.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateScalingPlan' smart constructor.
data CreateScalingPlan = CreateScalingPlan'
  { scalingPlanName :: Types.ScalingPlanName
    -- ^ The name of the scaling plan. Names cannot contain vertical bars, colons, or forward slashes.
  , applicationSource :: Types.ApplicationSource
    -- ^ A CloudFormation stack or set of tags. You can create one scaling plan per application source.
  , scalingInstructions :: [Types.ScalingInstruction]
    -- ^ The scaling instructions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateScalingPlan' value with any optional fields omitted.
mkCreateScalingPlan
    :: Types.ScalingPlanName -- ^ 'scalingPlanName'
    -> Types.ApplicationSource -- ^ 'applicationSource'
    -> CreateScalingPlan
mkCreateScalingPlan scalingPlanName applicationSource
  = CreateScalingPlan'{scalingPlanName, applicationSource,
                       scalingInstructions = Core.mempty}

-- | The name of the scaling plan. Names cannot contain vertical bars, colons, or forward slashes.
--
-- /Note:/ Consider using 'scalingPlanName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspScalingPlanName :: Lens.Lens' CreateScalingPlan Types.ScalingPlanName
cspScalingPlanName = Lens.field @"scalingPlanName"
{-# INLINEABLE cspScalingPlanName #-}
{-# DEPRECATED scalingPlanName "Use generic-lens or generic-optics with 'scalingPlanName' instead"  #-}

-- | A CloudFormation stack or set of tags. You can create one scaling plan per application source.
--
-- /Note:/ Consider using 'applicationSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspApplicationSource :: Lens.Lens' CreateScalingPlan Types.ApplicationSource
cspApplicationSource = Lens.field @"applicationSource"
{-# INLINEABLE cspApplicationSource #-}
{-# DEPRECATED applicationSource "Use generic-lens or generic-optics with 'applicationSource' instead"  #-}

-- | The scaling instructions.
--
-- /Note:/ Consider using 'scalingInstructions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspScalingInstructions :: Lens.Lens' CreateScalingPlan [Types.ScalingInstruction]
cspScalingInstructions = Lens.field @"scalingInstructions"
{-# INLINEABLE cspScalingInstructions #-}
{-# DEPRECATED scalingInstructions "Use generic-lens or generic-optics with 'scalingInstructions' instead"  #-}

instance Core.ToQuery CreateScalingPlan where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateScalingPlan where
        toHeaders CreateScalingPlan{..}
          = Core.pure
              ("X-Amz-Target",
               "AnyScaleScalingPlannerFrontendService.CreateScalingPlan")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateScalingPlan where
        toJSON CreateScalingPlan{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ScalingPlanName" Core..= scalingPlanName),
                  Core.Just ("ApplicationSource" Core..= applicationSource),
                  Core.Just ("ScalingInstructions" Core..= scalingInstructions)])

instance Core.AWSRequest CreateScalingPlan where
        type Rs CreateScalingPlan = CreateScalingPlanResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateScalingPlanResponse' Core.<$>
                   (x Core..: "ScalingPlanVersion") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateScalingPlanResponse' smart constructor.
data CreateScalingPlanResponse = CreateScalingPlanResponse'
  { scalingPlanVersion :: Core.Integer
    -- ^ The version number of the scaling plan. This value is always 1.
--
-- Currently, you cannot specify multiple scaling plan versions.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateScalingPlanResponse' value with any optional fields omitted.
mkCreateScalingPlanResponse
    :: Core.Integer -- ^ 'scalingPlanVersion'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateScalingPlanResponse
mkCreateScalingPlanResponse scalingPlanVersion responseStatus
  = CreateScalingPlanResponse'{scalingPlanVersion, responseStatus}

-- | The version number of the scaling plan. This value is always 1.
--
-- Currently, you cannot specify multiple scaling plan versions.
--
-- /Note:/ Consider using 'scalingPlanVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csprrsScalingPlanVersion :: Lens.Lens' CreateScalingPlanResponse Core.Integer
csprrsScalingPlanVersion = Lens.field @"scalingPlanVersion"
{-# INLINEABLE csprrsScalingPlanVersion #-}
{-# DEPRECATED scalingPlanVersion "Use generic-lens or generic-optics with 'scalingPlanVersion' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csprrsResponseStatus :: Lens.Lens' CreateScalingPlanResponse Core.Int
csprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
