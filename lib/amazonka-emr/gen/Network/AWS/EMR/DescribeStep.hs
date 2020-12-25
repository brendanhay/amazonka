{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.DescribeStep
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides more detail about the cluster step.
module Network.AWS.EMR.DescribeStep
  ( -- * Creating a request
    DescribeStep (..),
    mkDescribeStep,

    -- ** Request lenses
    dsClusterId,
    dsStepId,

    -- * Destructuring the response
    DescribeStepResponse (..),
    mkDescribeStepResponse,

    -- ** Response lenses
    dsrrsStep,
    dsrrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input determines which step to describe.
--
-- /See:/ 'mkDescribeStep' smart constructor.
data DescribeStep = DescribeStep'
  { -- | The identifier of the cluster with steps to describe.
    clusterId :: Types.ClusterId,
    -- | The identifier of the step to describe.
    stepId :: Types.StepId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStep' value with any optional fields omitted.
mkDescribeStep ::
  -- | 'clusterId'
  Types.ClusterId ->
  -- | 'stepId'
  Types.StepId ->
  DescribeStep
mkDescribeStep clusterId stepId = DescribeStep' {clusterId, stepId}

-- | The identifier of the cluster with steps to describe.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsClusterId :: Lens.Lens' DescribeStep Types.ClusterId
dsClusterId = Lens.field @"clusterId"
{-# DEPRECATED dsClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The identifier of the step to describe.
--
-- /Note:/ Consider using 'stepId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStepId :: Lens.Lens' DescribeStep Types.StepId
dsStepId = Lens.field @"stepId"
{-# DEPRECATED dsStepId "Use generic-lens or generic-optics with 'stepId' instead." #-}

instance Core.FromJSON DescribeStep where
  toJSON DescribeStep {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterId" Core..= clusterId),
            Core.Just ("StepId" Core..= stepId)
          ]
      )

instance Core.AWSRequest DescribeStep where
  type Rs DescribeStep = DescribeStepResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "ElasticMapReduce.DescribeStep")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStepResponse'
            Core.<$> (x Core..:? "Step") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | This output contains the description of the cluster step.
--
-- /See:/ 'mkDescribeStepResponse' smart constructor.
data DescribeStepResponse = DescribeStepResponse'
  { -- | The step details for the requested step identifier.
    step :: Core.Maybe Types.Step,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeStepResponse' value with any optional fields omitted.
mkDescribeStepResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeStepResponse
mkDescribeStepResponse responseStatus =
  DescribeStepResponse' {step = Core.Nothing, responseStatus}

-- | The step details for the requested step identifier.
--
-- /Note:/ Consider using 'step' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsStep :: Lens.Lens' DescribeStepResponse (Core.Maybe Types.Step)
dsrrsStep = Lens.field @"step"
{-# DEPRECATED dsrrsStep "Use generic-lens or generic-optics with 'step' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeStepResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
