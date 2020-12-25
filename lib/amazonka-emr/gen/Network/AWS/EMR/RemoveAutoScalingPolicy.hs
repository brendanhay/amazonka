{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.RemoveAutoScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an automatic scaling policy from a specified instance group within an EMR cluster.
module Network.AWS.EMR.RemoveAutoScalingPolicy
  ( -- * Creating a request
    RemoveAutoScalingPolicy (..),
    mkRemoveAutoScalingPolicy,

    -- ** Request lenses
    raspClusterId,
    raspInstanceGroupId,

    -- * Destructuring the response
    RemoveAutoScalingPolicyResponse (..),
    mkRemoveAutoScalingPolicyResponse,

    -- ** Response lenses
    rasprrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveAutoScalingPolicy' smart constructor.
data RemoveAutoScalingPolicy = RemoveAutoScalingPolicy'
  { -- | Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
    clusterId :: Types.ClusterId,
    -- | Specifies the ID of the instance group to which the scaling policy is applied.
    instanceGroupId :: Types.InstanceGroupId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveAutoScalingPolicy' value with any optional fields omitted.
mkRemoveAutoScalingPolicy ::
  -- | 'clusterId'
  Types.ClusterId ->
  -- | 'instanceGroupId'
  Types.InstanceGroupId ->
  RemoveAutoScalingPolicy
mkRemoveAutoScalingPolicy clusterId instanceGroupId =
  RemoveAutoScalingPolicy' {clusterId, instanceGroupId}

-- | Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raspClusterId :: Lens.Lens' RemoveAutoScalingPolicy Types.ClusterId
raspClusterId = Lens.field @"clusterId"
{-# DEPRECATED raspClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | Specifies the ID of the instance group to which the scaling policy is applied.
--
-- /Note:/ Consider using 'instanceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raspInstanceGroupId :: Lens.Lens' RemoveAutoScalingPolicy Types.InstanceGroupId
raspInstanceGroupId = Lens.field @"instanceGroupId"
{-# DEPRECATED raspInstanceGroupId "Use generic-lens or generic-optics with 'instanceGroupId' instead." #-}

instance Core.FromJSON RemoveAutoScalingPolicy where
  toJSON RemoveAutoScalingPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterId" Core..= clusterId),
            Core.Just ("InstanceGroupId" Core..= instanceGroupId)
          ]
      )

instance Core.AWSRequest RemoveAutoScalingPolicy where
  type Rs RemoveAutoScalingPolicy = RemoveAutoScalingPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "ElasticMapReduce.RemoveAutoScalingPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveAutoScalingPolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRemoveAutoScalingPolicyResponse' smart constructor.
newtype RemoveAutoScalingPolicyResponse = RemoveAutoScalingPolicyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveAutoScalingPolicyResponse' value with any optional fields omitted.
mkRemoveAutoScalingPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RemoveAutoScalingPolicyResponse
mkRemoveAutoScalingPolicyResponse responseStatus =
  RemoveAutoScalingPolicyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasprrsResponseStatus :: Lens.Lens' RemoveAutoScalingPolicyResponse Core.Int
rasprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rasprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
