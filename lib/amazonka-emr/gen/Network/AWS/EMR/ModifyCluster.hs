{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ModifyCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the number of steps that can be executed concurrently for the cluster specified using ClusterID.
module Network.AWS.EMR.ModifyCluster
  ( -- * Creating a request
    ModifyCluster (..),
    mkModifyCluster,

    -- ** Request lenses
    mcClusterId,
    mcStepConcurrencyLevel,

    -- * Destructuring the response
    ModifyClusterResponse (..),
    mkModifyClusterResponse,

    -- ** Response lenses
    mcrrsStepConcurrencyLevel,
    mcrrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyCluster' smart constructor.
data ModifyCluster = ModifyCluster'
  { -- | The unique identifier of the cluster.
    clusterId :: Types.ClusterId,
    -- | The number of steps that can be executed concurrently. You can specify a maximum of 256 steps.
    stepConcurrencyLevel :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyCluster' value with any optional fields omitted.
mkModifyCluster ::
  -- | 'clusterId'
  Types.ClusterId ->
  ModifyCluster
mkModifyCluster clusterId =
  ModifyCluster' {clusterId, stepConcurrencyLevel = Core.Nothing}

-- | The unique identifier of the cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcClusterId :: Lens.Lens' ModifyCluster Types.ClusterId
mcClusterId = Lens.field @"clusterId"
{-# DEPRECATED mcClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The number of steps that can be executed concurrently. You can specify a maximum of 256 steps.
--
-- /Note:/ Consider using 'stepConcurrencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcStepConcurrencyLevel :: Lens.Lens' ModifyCluster (Core.Maybe Core.Int)
mcStepConcurrencyLevel = Lens.field @"stepConcurrencyLevel"
{-# DEPRECATED mcStepConcurrencyLevel "Use generic-lens or generic-optics with 'stepConcurrencyLevel' instead." #-}

instance Core.FromJSON ModifyCluster where
  toJSON ModifyCluster {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterId" Core..= clusterId),
            ("StepConcurrencyLevel" Core..=) Core.<$> stepConcurrencyLevel
          ]
      )

instance Core.AWSRequest ModifyCluster where
  type Rs ModifyCluster = ModifyClusterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "ElasticMapReduce.ModifyCluster")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyClusterResponse'
            Core.<$> (x Core..:? "StepConcurrencyLevel")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyClusterResponse' smart constructor.
data ModifyClusterResponse = ModifyClusterResponse'
  { -- | The number of steps that can be executed concurrently.
    stepConcurrencyLevel :: Core.Maybe Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyClusterResponse' value with any optional fields omitted.
mkModifyClusterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyClusterResponse
mkModifyClusterResponse responseStatus =
  ModifyClusterResponse'
    { stepConcurrencyLevel = Core.Nothing,
      responseStatus
    }

-- | The number of steps that can be executed concurrently.
--
-- /Note:/ Consider using 'stepConcurrencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrrsStepConcurrencyLevel :: Lens.Lens' ModifyClusterResponse (Core.Maybe Core.Int)
mcrrsStepConcurrencyLevel = Lens.field @"stepConcurrencyLevel"
{-# DEPRECATED mcrrsStepConcurrencyLevel "Use generic-lens or generic-optics with 'stepConcurrencyLevel' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrrsResponseStatus :: Lens.Lens' ModifyClusterResponse Core.Int
mcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
