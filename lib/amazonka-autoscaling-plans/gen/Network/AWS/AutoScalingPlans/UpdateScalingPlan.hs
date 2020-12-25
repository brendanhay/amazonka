{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.UpdateScalingPlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified scaling plan.
--
-- You cannot update a scaling plan if it is in the process of being created, updated, or deleted.
module Network.AWS.AutoScalingPlans.UpdateScalingPlan
  ( -- * Creating a request
    UpdateScalingPlan (..),
    mkUpdateScalingPlan,

    -- ** Request lenses
    uspScalingPlanName,
    uspScalingPlanVersion,
    uspApplicationSource,
    uspScalingInstructions,

    -- * Destructuring the response
    UpdateScalingPlanResponse (..),
    mkUpdateScalingPlanResponse,

    -- ** Response lenses
    usprrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScalingPlans.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateScalingPlan' smart constructor.
data UpdateScalingPlan = UpdateScalingPlan'
  { -- | The name of the scaling plan.
    scalingPlanName :: Types.ScalingPlanName,
    -- | The version number of the scaling plan.
    scalingPlanVersion :: Core.Integer,
    -- | A CloudFormation stack or set of tags.
    applicationSource :: Core.Maybe Types.ApplicationSource,
    -- | The scaling instructions.
    scalingInstructions :: Core.Maybe [Types.ScalingInstruction]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateScalingPlan' value with any optional fields omitted.
mkUpdateScalingPlan ::
  -- | 'scalingPlanName'
  Types.ScalingPlanName ->
  -- | 'scalingPlanVersion'
  Core.Integer ->
  UpdateScalingPlan
mkUpdateScalingPlan scalingPlanName scalingPlanVersion =
  UpdateScalingPlan'
    { scalingPlanName,
      scalingPlanVersion,
      applicationSource = Core.Nothing,
      scalingInstructions = Core.Nothing
    }

-- | The name of the scaling plan.
--
-- /Note:/ Consider using 'scalingPlanName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspScalingPlanName :: Lens.Lens' UpdateScalingPlan Types.ScalingPlanName
uspScalingPlanName = Lens.field @"scalingPlanName"
{-# DEPRECATED uspScalingPlanName "Use generic-lens or generic-optics with 'scalingPlanName' instead." #-}

-- | The version number of the scaling plan.
--
-- /Note:/ Consider using 'scalingPlanVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspScalingPlanVersion :: Lens.Lens' UpdateScalingPlan Core.Integer
uspScalingPlanVersion = Lens.field @"scalingPlanVersion"
{-# DEPRECATED uspScalingPlanVersion "Use generic-lens or generic-optics with 'scalingPlanVersion' instead." #-}

-- | A CloudFormation stack or set of tags.
--
-- /Note:/ Consider using 'applicationSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspApplicationSource :: Lens.Lens' UpdateScalingPlan (Core.Maybe Types.ApplicationSource)
uspApplicationSource = Lens.field @"applicationSource"
{-# DEPRECATED uspApplicationSource "Use generic-lens or generic-optics with 'applicationSource' instead." #-}

-- | The scaling instructions.
--
-- /Note:/ Consider using 'scalingInstructions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspScalingInstructions :: Lens.Lens' UpdateScalingPlan (Core.Maybe [Types.ScalingInstruction])
uspScalingInstructions = Lens.field @"scalingInstructions"
{-# DEPRECATED uspScalingInstructions "Use generic-lens or generic-optics with 'scalingInstructions' instead." #-}

instance Core.FromJSON UpdateScalingPlan where
  toJSON UpdateScalingPlan {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ScalingPlanName" Core..= scalingPlanName),
            Core.Just ("ScalingPlanVersion" Core..= scalingPlanVersion),
            ("ApplicationSource" Core..=) Core.<$> applicationSource,
            ("ScalingInstructions" Core..=) Core.<$> scalingInstructions
          ]
      )

instance Core.AWSRequest UpdateScalingPlan where
  type Rs UpdateScalingPlan = UpdateScalingPlanResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AnyScaleScalingPlannerFrontendService.UpdateScalingPlan"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateScalingPlanResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateScalingPlanResponse' smart constructor.
newtype UpdateScalingPlanResponse = UpdateScalingPlanResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateScalingPlanResponse' value with any optional fields omitted.
mkUpdateScalingPlanResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateScalingPlanResponse
mkUpdateScalingPlanResponse responseStatus =
  UpdateScalingPlanResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprrsResponseStatus :: Lens.Lens' UpdateScalingPlanResponse Core.Int
usprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
