{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.LoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.LoadBalancer
  ( LoadBalancer (..),

    -- * Smart constructor
    mkLoadBalancer,

    -- * Lenses
    lbLoadBalancerName,
    lbContainerName,
    lbTargetGroupARN,
    lbContainerPort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The load balancer configuration to use with a service or task set.
--
-- For specific notes and restrictions regarding the use of load balancers with services and task sets, see the CreateService and CreateTaskSet actions.
--
-- /See:/ 'mkLoadBalancer' smart constructor.
data LoadBalancer = LoadBalancer'
  { -- | The name of the load balancer to associate with the Amazon ECS service or task set.
    --
    -- A load balancer name is only specified when using a Classic Load Balancer. If you are using an Application Load Balancer or a Network Load Balancer the load balancer name parameter should be omitted.
    loadBalancerName :: Lude.Maybe Lude.Text,
    -- | The name of the container (as it appears in a container definition) to associate with the load balancer.
    containerName :: Lude.Maybe Lude.Text,
    -- | The full Amazon Resource Name (ARN) of the Elastic Load Balancing target group or groups associated with a service or task set.
    --
    -- A target group ARN is only specified when using an Application Load Balancer or Network Load Balancer. If you are using a Classic Load Balancer the target group ARN should be omitted.
    -- For services using the @ECS@ deployment controller, you can specify one or multiple target groups. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/register-multiple-targetgroups.html Registering Multiple Target Groups with a Service> in the /Amazon Elastic Container Service Developer Guide/ .
    -- For services using the @CODE_DEPLOY@ deployment controller, you are required to define two target groups for the load balancer. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-bluegreen.html Blue/Green Deployment with CodeDeploy> in the /Amazon Elastic Container Service Developer Guide/ .
    -- /Important:/ If your service's task definition uses the @awsvpc@ network mode (which is required for the Fargate launch type), you must choose @ip@ as the target type, not @instance@ , when creating your target groups because tasks that use the @awsvpc@ network mode are associated with an elastic network interface, not an Amazon EC2 instance.
    targetGroupARN :: Lude.Maybe Lude.Text,
    -- | The port on the container to associate with the load balancer. This port must correspond to a @containerPort@ in the task definition the tasks in the service are using. For tasks that use the EC2 launch type, the container instance they are launched on must allow ingress traffic on the @hostPort@ of the port mapping.
    containerPort :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBalancer' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer to associate with the Amazon ECS service or task set.
--
-- A load balancer name is only specified when using a Classic Load Balancer. If you are using an Application Load Balancer or a Network Load Balancer the load balancer name parameter should be omitted.
-- * 'containerName' - The name of the container (as it appears in a container definition) to associate with the load balancer.
-- * 'targetGroupARN' - The full Amazon Resource Name (ARN) of the Elastic Load Balancing target group or groups associated with a service or task set.
--
-- A target group ARN is only specified when using an Application Load Balancer or Network Load Balancer. If you are using a Classic Load Balancer the target group ARN should be omitted.
-- For services using the @ECS@ deployment controller, you can specify one or multiple target groups. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/register-multiple-targetgroups.html Registering Multiple Target Groups with a Service> in the /Amazon Elastic Container Service Developer Guide/ .
-- For services using the @CODE_DEPLOY@ deployment controller, you are required to define two target groups for the load balancer. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-bluegreen.html Blue/Green Deployment with CodeDeploy> in the /Amazon Elastic Container Service Developer Guide/ .
-- /Important:/ If your service's task definition uses the @awsvpc@ network mode (which is required for the Fargate launch type), you must choose @ip@ as the target type, not @instance@ , when creating your target groups because tasks that use the @awsvpc@ network mode are associated with an elastic network interface, not an Amazon EC2 instance.
-- * 'containerPort' - The port on the container to associate with the load balancer. This port must correspond to a @containerPort@ in the task definition the tasks in the service are using. For tasks that use the EC2 launch type, the container instance they are launched on must allow ingress traffic on the @hostPort@ of the port mapping.
mkLoadBalancer ::
  LoadBalancer
mkLoadBalancer =
  LoadBalancer'
    { loadBalancerName = Lude.Nothing,
      containerName = Lude.Nothing,
      targetGroupARN = Lude.Nothing,
      containerPort = Lude.Nothing
    }

-- | The name of the load balancer to associate with the Amazon ECS service or task set.
--
-- A load balancer name is only specified when using a Classic Load Balancer. If you are using an Application Load Balancer or a Network Load Balancer the load balancer name parameter should be omitted.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbLoadBalancerName :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.Text)
lbLoadBalancerName = Lens.lens (loadBalancerName :: LoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {loadBalancerName = a} :: LoadBalancer)
{-# DEPRECATED lbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The name of the container (as it appears in a container definition) to associate with the load balancer.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbContainerName :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.Text)
lbContainerName = Lens.lens (containerName :: LoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {containerName = a} :: LoadBalancer)
{-# DEPRECATED lbContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

-- | The full Amazon Resource Name (ARN) of the Elastic Load Balancing target group or groups associated with a service or task set.
--
-- A target group ARN is only specified when using an Application Load Balancer or Network Load Balancer. If you are using a Classic Load Balancer the target group ARN should be omitted.
-- For services using the @ECS@ deployment controller, you can specify one or multiple target groups. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/register-multiple-targetgroups.html Registering Multiple Target Groups with a Service> in the /Amazon Elastic Container Service Developer Guide/ .
-- For services using the @CODE_DEPLOY@ deployment controller, you are required to define two target groups for the load balancer. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-bluegreen.html Blue/Green Deployment with CodeDeploy> in the /Amazon Elastic Container Service Developer Guide/ .
-- /Important:/ If your service's task definition uses the @awsvpc@ network mode (which is required for the Fargate launch type), you must choose @ip@ as the target type, not @instance@ , when creating your target groups because tasks that use the @awsvpc@ network mode are associated with an elastic network interface, not an Amazon EC2 instance.
--
-- /Note:/ Consider using 'targetGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbTargetGroupARN :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.Text)
lbTargetGroupARN = Lens.lens (targetGroupARN :: LoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {targetGroupARN = a} :: LoadBalancer)
{-# DEPRECATED lbTargetGroupARN "Use generic-lens or generic-optics with 'targetGroupARN' instead." #-}

-- | The port on the container to associate with the load balancer. This port must correspond to a @containerPort@ in the task definition the tasks in the service are using. For tasks that use the EC2 launch type, the container instance they are launched on must allow ingress traffic on the @hostPort@ of the port mapping.
--
-- /Note:/ Consider using 'containerPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbContainerPort :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.Int)
lbContainerPort = Lens.lens (containerPort :: LoadBalancer -> Lude.Maybe Lude.Int) (\s a -> s {containerPort = a} :: LoadBalancer)
{-# DEPRECATED lbContainerPort "Use generic-lens or generic-optics with 'containerPort' instead." #-}

instance Lude.FromJSON LoadBalancer where
  parseJSON =
    Lude.withObject
      "LoadBalancer"
      ( \x ->
          LoadBalancer'
            Lude.<$> (x Lude..:? "loadBalancerName")
            Lude.<*> (x Lude..:? "containerName")
            Lude.<*> (x Lude..:? "targetGroupArn")
            Lude.<*> (x Lude..:? "containerPort")
      )

instance Lude.ToJSON LoadBalancer where
  toJSON LoadBalancer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("loadBalancerName" Lude..=) Lude.<$> loadBalancerName,
            ("containerName" Lude..=) Lude.<$> containerName,
            ("targetGroupArn" Lude..=) Lude.<$> targetGroupARN,
            ("containerPort" Lude..=) Lude.<$> containerPort
          ]
      )
