{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.PutAutoScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric.
module Network.AWS.EMR.PutAutoScalingPolicy
  ( -- * Creating a request
    PutAutoScalingPolicy (..),
    mkPutAutoScalingPolicy,

    -- ** Request lenses
    paspClusterId,
    paspInstanceGroupId,
    paspAutoScalingPolicy,

    -- * Destructuring the response
    PutAutoScalingPolicyResponse (..),
    mkPutAutoScalingPolicyResponse,

    -- ** Response lenses
    pasprrsAutoScalingPolicy,
    pasprrsClusterArn,
    pasprrsClusterId,
    pasprrsInstanceGroupId,
    pasprrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutAutoScalingPolicy' smart constructor.
data PutAutoScalingPolicy = PutAutoScalingPolicy'
  { -- | Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
    clusterId :: Types.ClusterId,
    -- | Specifies the ID of the instance group to which the automatic scaling policy is applied.
    instanceGroupId :: Types.InstanceGroupId,
    -- | Specifies the definition of the automatic scaling policy.
    autoScalingPolicy :: Types.AutoScalingPolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutAutoScalingPolicy' value with any optional fields omitted.
mkPutAutoScalingPolicy ::
  -- | 'clusterId'
  Types.ClusterId ->
  -- | 'instanceGroupId'
  Types.InstanceGroupId ->
  -- | 'autoScalingPolicy'
  Types.AutoScalingPolicy ->
  PutAutoScalingPolicy
mkPutAutoScalingPolicy clusterId instanceGroupId autoScalingPolicy =
  PutAutoScalingPolicy'
    { clusterId,
      instanceGroupId,
      autoScalingPolicy
    }

-- | Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paspClusterId :: Lens.Lens' PutAutoScalingPolicy Types.ClusterId
paspClusterId = Lens.field @"clusterId"
{-# DEPRECATED paspClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | Specifies the ID of the instance group to which the automatic scaling policy is applied.
--
-- /Note:/ Consider using 'instanceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paspInstanceGroupId :: Lens.Lens' PutAutoScalingPolicy Types.InstanceGroupId
paspInstanceGroupId = Lens.field @"instanceGroupId"
{-# DEPRECATED paspInstanceGroupId "Use generic-lens or generic-optics with 'instanceGroupId' instead." #-}

-- | Specifies the definition of the automatic scaling policy.
--
-- /Note:/ Consider using 'autoScalingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paspAutoScalingPolicy :: Lens.Lens' PutAutoScalingPolicy Types.AutoScalingPolicy
paspAutoScalingPolicy = Lens.field @"autoScalingPolicy"
{-# DEPRECATED paspAutoScalingPolicy "Use generic-lens or generic-optics with 'autoScalingPolicy' instead." #-}

instance Core.FromJSON PutAutoScalingPolicy where
  toJSON PutAutoScalingPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterId" Core..= clusterId),
            Core.Just ("InstanceGroupId" Core..= instanceGroupId),
            Core.Just ("AutoScalingPolicy" Core..= autoScalingPolicy)
          ]
      )

instance Core.AWSRequest PutAutoScalingPolicy where
  type Rs PutAutoScalingPolicy = PutAutoScalingPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "ElasticMapReduce.PutAutoScalingPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAutoScalingPolicyResponse'
            Core.<$> (x Core..:? "AutoScalingPolicy")
            Core.<*> (x Core..:? "ClusterArn")
            Core.<*> (x Core..:? "ClusterId")
            Core.<*> (x Core..:? "InstanceGroupId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutAutoScalingPolicyResponse' smart constructor.
data PutAutoScalingPolicyResponse = PutAutoScalingPolicyResponse'
  { -- | The automatic scaling policy definition.
    autoScalingPolicy :: Core.Maybe Types.AutoScalingPolicyDescription,
    -- | The Amazon Resource Name of the cluster.
    clusterArn :: Core.Maybe Types.ClusterArn,
    -- | Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
    clusterId :: Core.Maybe Types.ClusterId,
    -- | Specifies the ID of the instance group to which the scaling policy is applied.
    instanceGroupId :: Core.Maybe Types.InstanceGroupId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutAutoScalingPolicyResponse' value with any optional fields omitted.
mkPutAutoScalingPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutAutoScalingPolicyResponse
mkPutAutoScalingPolicyResponse responseStatus =
  PutAutoScalingPolicyResponse'
    { autoScalingPolicy = Core.Nothing,
      clusterArn = Core.Nothing,
      clusterId = Core.Nothing,
      instanceGroupId = Core.Nothing,
      responseStatus
    }

-- | The automatic scaling policy definition.
--
-- /Note:/ Consider using 'autoScalingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasprrsAutoScalingPolicy :: Lens.Lens' PutAutoScalingPolicyResponse (Core.Maybe Types.AutoScalingPolicyDescription)
pasprrsAutoScalingPolicy = Lens.field @"autoScalingPolicy"
{-# DEPRECATED pasprrsAutoScalingPolicy "Use generic-lens or generic-optics with 'autoScalingPolicy' instead." #-}

-- | The Amazon Resource Name of the cluster.
--
-- /Note:/ Consider using 'clusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasprrsClusterArn :: Lens.Lens' PutAutoScalingPolicyResponse (Core.Maybe Types.ClusterArn)
pasprrsClusterArn = Lens.field @"clusterArn"
{-# DEPRECATED pasprrsClusterArn "Use generic-lens or generic-optics with 'clusterArn' instead." #-}

-- | Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasprrsClusterId :: Lens.Lens' PutAutoScalingPolicyResponse (Core.Maybe Types.ClusterId)
pasprrsClusterId = Lens.field @"clusterId"
{-# DEPRECATED pasprrsClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | Specifies the ID of the instance group to which the scaling policy is applied.
--
-- /Note:/ Consider using 'instanceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasprrsInstanceGroupId :: Lens.Lens' PutAutoScalingPolicyResponse (Core.Maybe Types.InstanceGroupId)
pasprrsInstanceGroupId = Lens.field @"instanceGroupId"
{-# DEPRECATED pasprrsInstanceGroupId "Use generic-lens or generic-optics with 'instanceGroupId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasprrsResponseStatus :: Lens.Lens' PutAutoScalingPolicyResponse Core.Int
pasprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pasprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
