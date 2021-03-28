{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.LoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.LoadBalancer
  ( LoadBalancer (..)
  -- * Smart constructor
  , mkLoadBalancer
  -- * Lenses
  , lbContainerName
  , lbContainerPort
  , lbLoadBalancerName
  , lbTargetGroupArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The load balancer configuration to use with a service or task set.
--
-- For specific notes and restrictions regarding the use of load balancers with services and task sets, see the CreateService and CreateTaskSet actions.
--
-- /See:/ 'mkLoadBalancer' smart constructor.
data LoadBalancer = LoadBalancer'
  { containerName :: Core.Maybe Core.Text
    -- ^ The name of the container (as it appears in a container definition) to associate with the load balancer.
  , containerPort :: Core.Maybe Core.Int
    -- ^ The port on the container to associate with the load balancer. This port must correspond to a @containerPort@ in the task definition the tasks in the service are using. For tasks that use the EC2 launch type, the container instance they are launched on must allow ingress traffic on the @hostPort@ of the port mapping.
  , loadBalancerName :: Core.Maybe Core.Text
    -- ^ The name of the load balancer to associate with the Amazon ECS service or task set.
--
-- A load balancer name is only specified when using a Classic Load Balancer. If you are using an Application Load Balancer or a Network Load Balancer the load balancer name parameter should be omitted.
  , targetGroupArn :: Core.Maybe Core.Text
    -- ^ The full Amazon Resource Name (ARN) of the Elastic Load Balancing target group or groups associated with a service or task set.
--
-- A target group ARN is only specified when using an Application Load Balancer or Network Load Balancer. If you are using a Classic Load Balancer the target group ARN should be omitted.
-- For services using the @ECS@ deployment controller, you can specify one or multiple target groups. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/register-multiple-targetgroups.html Registering Multiple Target Groups with a Service> in the /Amazon Elastic Container Service Developer Guide/ .
-- For services using the @CODE_DEPLOY@ deployment controller, you are required to define two target groups for the load balancer. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-bluegreen.html Blue/Green Deployment with CodeDeploy> in the /Amazon Elastic Container Service Developer Guide/ .
-- /Important:/ If your service's task definition uses the @awsvpc@ network mode (which is required for the Fargate launch type), you must choose @ip@ as the target type, not @instance@ , when creating your target groups because tasks that use the @awsvpc@ network mode are associated with an elastic network interface, not an Amazon EC2 instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoadBalancer' value with any optional fields omitted.
mkLoadBalancer
    :: LoadBalancer
mkLoadBalancer
  = LoadBalancer'{containerName = Core.Nothing,
                  containerPort = Core.Nothing, loadBalancerName = Core.Nothing,
                  targetGroupArn = Core.Nothing}

-- | The name of the container (as it appears in a container definition) to associate with the load balancer.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbContainerName :: Lens.Lens' LoadBalancer (Core.Maybe Core.Text)
lbContainerName = Lens.field @"containerName"
{-# INLINEABLE lbContainerName #-}
{-# DEPRECATED containerName "Use generic-lens or generic-optics with 'containerName' instead"  #-}

-- | The port on the container to associate with the load balancer. This port must correspond to a @containerPort@ in the task definition the tasks in the service are using. For tasks that use the EC2 launch type, the container instance they are launched on must allow ingress traffic on the @hostPort@ of the port mapping.
--
-- /Note:/ Consider using 'containerPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbContainerPort :: Lens.Lens' LoadBalancer (Core.Maybe Core.Int)
lbContainerPort = Lens.field @"containerPort"
{-# INLINEABLE lbContainerPort #-}
{-# DEPRECATED containerPort "Use generic-lens or generic-optics with 'containerPort' instead"  #-}

-- | The name of the load balancer to associate with the Amazon ECS service or task set.
--
-- A load balancer name is only specified when using a Classic Load Balancer. If you are using an Application Load Balancer or a Network Load Balancer the load balancer name parameter should be omitted.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbLoadBalancerName :: Lens.Lens' LoadBalancer (Core.Maybe Core.Text)
lbLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE lbLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The full Amazon Resource Name (ARN) of the Elastic Load Balancing target group or groups associated with a service or task set.
--
-- A target group ARN is only specified when using an Application Load Balancer or Network Load Balancer. If you are using a Classic Load Balancer the target group ARN should be omitted.
-- For services using the @ECS@ deployment controller, you can specify one or multiple target groups. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/register-multiple-targetgroups.html Registering Multiple Target Groups with a Service> in the /Amazon Elastic Container Service Developer Guide/ .
-- For services using the @CODE_DEPLOY@ deployment controller, you are required to define two target groups for the load balancer. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-bluegreen.html Blue/Green Deployment with CodeDeploy> in the /Amazon Elastic Container Service Developer Guide/ .
-- /Important:/ If your service's task definition uses the @awsvpc@ network mode (which is required for the Fargate launch type), you must choose @ip@ as the target type, not @instance@ , when creating your target groups because tasks that use the @awsvpc@ network mode are associated with an elastic network interface, not an Amazon EC2 instance.
--
-- /Note:/ Consider using 'targetGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbTargetGroupArn :: Lens.Lens' LoadBalancer (Core.Maybe Core.Text)
lbTargetGroupArn = Lens.field @"targetGroupArn"
{-# INLINEABLE lbTargetGroupArn #-}
{-# DEPRECATED targetGroupArn "Use generic-lens or generic-optics with 'targetGroupArn' instead"  #-}

instance Core.FromJSON LoadBalancer where
        toJSON LoadBalancer{..}
          = Core.object
              (Core.catMaybes
                 [("containerName" Core..=) Core.<$> containerName,
                  ("containerPort" Core..=) Core.<$> containerPort,
                  ("loadBalancerName" Core..=) Core.<$> loadBalancerName,
                  ("targetGroupArn" Core..=) Core.<$> targetGroupArn])

instance Core.FromJSON LoadBalancer where
        parseJSON
          = Core.withObject "LoadBalancer" Core.$
              \ x ->
                LoadBalancer' Core.<$>
                  (x Core..:? "containerName") Core.<*> x Core..:? "containerPort"
                    Core.<*> x Core..:? "loadBalancerName"
                    Core.<*> x Core..:? "targetGroupArn"
