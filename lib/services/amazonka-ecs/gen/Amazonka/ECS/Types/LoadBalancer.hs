{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECS.Types.LoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.LoadBalancer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The load balancer configuration to use with a service or task set.
--
-- For specific notes and restrictions regarding the use of load balancers
-- with services and task sets, see the CreateService and CreateTaskSet
-- actions.
--
-- /See:/ 'newLoadBalancer' smart constructor.
data LoadBalancer = LoadBalancer'
  { -- | The name of the load balancer to associate with the Amazon ECS service
    -- or task set.
    --
    -- A load balancer name is only specified when using a Classic Load
    -- Balancer. If you are using an Application Load Balancer or a Network
    -- Load Balancer the load balancer name parameter should be omitted.
    loadBalancerName :: Prelude.Maybe Prelude.Text,
    -- | The name of the container (as it appears in a container definition) to
    -- associate with the load balancer.
    containerName :: Prelude.Maybe Prelude.Text,
    -- | The full Amazon Resource Name (ARN) of the Elastic Load Balancing target
    -- group or groups associated with a service or task set.
    --
    -- A target group ARN is only specified when using an Application Load
    -- Balancer or Network Load Balancer. If you are using a Classic Load
    -- Balancer the target group ARN should be omitted.
    --
    -- For services using the @ECS@ deployment controller, you can specify one
    -- or multiple target groups. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/register-multiple-targetgroups.html Registering Multiple Target Groups with a Service>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- For services using the @CODE_DEPLOY@ deployment controller, you are
    -- required to define two target groups for the load balancer. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-bluegreen.html Blue\/Green Deployment with CodeDeploy>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- If your service\'s task definition uses the @awsvpc@ network mode (which
    -- is required for the Fargate launch type), you must choose @ip@ as the
    -- target type, not @instance@, when creating your target groups because
    -- tasks that use the @awsvpc@ network mode are associated with an elastic
    -- network interface, not an Amazon EC2 instance.
    targetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The port on the container to associate with the load balancer. This port
    -- must correspond to a @containerPort@ in the task definition the tasks in
    -- the service are using. For tasks that use the EC2 launch type, the
    -- container instance they are launched on must allow ingress traffic on
    -- the @hostPort@ of the port mapping.
    containerPort :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'loadBalancer_loadBalancerName' - The name of the load balancer to associate with the Amazon ECS service
-- or task set.
--
-- A load balancer name is only specified when using a Classic Load
-- Balancer. If you are using an Application Load Balancer or a Network
-- Load Balancer the load balancer name parameter should be omitted.
--
-- 'containerName', 'loadBalancer_containerName' - The name of the container (as it appears in a container definition) to
-- associate with the load balancer.
--
-- 'targetGroupArn', 'loadBalancer_targetGroupArn' - The full Amazon Resource Name (ARN) of the Elastic Load Balancing target
-- group or groups associated with a service or task set.
--
-- A target group ARN is only specified when using an Application Load
-- Balancer or Network Load Balancer. If you are using a Classic Load
-- Balancer the target group ARN should be omitted.
--
-- For services using the @ECS@ deployment controller, you can specify one
-- or multiple target groups. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/register-multiple-targetgroups.html Registering Multiple Target Groups with a Service>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- For services using the @CODE_DEPLOY@ deployment controller, you are
-- required to define two target groups for the load balancer. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-bluegreen.html Blue\/Green Deployment with CodeDeploy>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If your service\'s task definition uses the @awsvpc@ network mode (which
-- is required for the Fargate launch type), you must choose @ip@ as the
-- target type, not @instance@, when creating your target groups because
-- tasks that use the @awsvpc@ network mode are associated with an elastic
-- network interface, not an Amazon EC2 instance.
--
-- 'containerPort', 'loadBalancer_containerPort' - The port on the container to associate with the load balancer. This port
-- must correspond to a @containerPort@ in the task definition the tasks in
-- the service are using. For tasks that use the EC2 launch type, the
-- container instance they are launched on must allow ingress traffic on
-- the @hostPort@ of the port mapping.
newLoadBalancer ::
  LoadBalancer
newLoadBalancer =
  LoadBalancer'
    { loadBalancerName = Prelude.Nothing,
      containerName = Prelude.Nothing,
      targetGroupArn = Prelude.Nothing,
      containerPort = Prelude.Nothing
    }

-- | The name of the load balancer to associate with the Amazon ECS service
-- or task set.
--
-- A load balancer name is only specified when using a Classic Load
-- Balancer. If you are using an Application Load Balancer or a Network
-- Load Balancer the load balancer name parameter should be omitted.
loadBalancer_loadBalancerName :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.Text)
loadBalancer_loadBalancerName = Lens.lens (\LoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@LoadBalancer' {} a -> s {loadBalancerName = a} :: LoadBalancer)

-- | The name of the container (as it appears in a container definition) to
-- associate with the load balancer.
loadBalancer_containerName :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.Text)
loadBalancer_containerName = Lens.lens (\LoadBalancer' {containerName} -> containerName) (\s@LoadBalancer' {} a -> s {containerName = a} :: LoadBalancer)

-- | The full Amazon Resource Name (ARN) of the Elastic Load Balancing target
-- group or groups associated with a service or task set.
--
-- A target group ARN is only specified when using an Application Load
-- Balancer or Network Load Balancer. If you are using a Classic Load
-- Balancer the target group ARN should be omitted.
--
-- For services using the @ECS@ deployment controller, you can specify one
-- or multiple target groups. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/register-multiple-targetgroups.html Registering Multiple Target Groups with a Service>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- For services using the @CODE_DEPLOY@ deployment controller, you are
-- required to define two target groups for the load balancer. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-bluegreen.html Blue\/Green Deployment with CodeDeploy>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If your service\'s task definition uses the @awsvpc@ network mode (which
-- is required for the Fargate launch type), you must choose @ip@ as the
-- target type, not @instance@, when creating your target groups because
-- tasks that use the @awsvpc@ network mode are associated with an elastic
-- network interface, not an Amazon EC2 instance.
loadBalancer_targetGroupArn :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.Text)
loadBalancer_targetGroupArn = Lens.lens (\LoadBalancer' {targetGroupArn} -> targetGroupArn) (\s@LoadBalancer' {} a -> s {targetGroupArn = a} :: LoadBalancer)

-- | The port on the container to associate with the load balancer. This port
-- must correspond to a @containerPort@ in the task definition the tasks in
-- the service are using. For tasks that use the EC2 launch type, the
-- container instance they are launched on must allow ingress traffic on
-- the @hostPort@ of the port mapping.
loadBalancer_containerPort :: Lens.Lens' LoadBalancer (Prelude.Maybe Prelude.Int)
loadBalancer_containerPort = Lens.lens (\LoadBalancer' {containerPort} -> containerPort) (\s@LoadBalancer' {} a -> s {containerPort = a} :: LoadBalancer)

instance Core.FromJSON LoadBalancer where
  parseJSON =
    Core.withObject
      "LoadBalancer"
      ( \x ->
          LoadBalancer'
            Prelude.<$> (x Core..:? "loadBalancerName")
            Prelude.<*> (x Core..:? "containerName")
            Prelude.<*> (x Core..:? "targetGroupArn")
            Prelude.<*> (x Core..:? "containerPort")
      )

instance Prelude.Hashable LoadBalancer where
  hashWithSalt _salt LoadBalancer' {..} =
    _salt `Prelude.hashWithSalt` loadBalancerName
      `Prelude.hashWithSalt` containerName
      `Prelude.hashWithSalt` targetGroupArn
      `Prelude.hashWithSalt` containerPort

instance Prelude.NFData LoadBalancer where
  rnf LoadBalancer' {..} =
    Prelude.rnf loadBalancerName
      `Prelude.seq` Prelude.rnf containerName
      `Prelude.seq` Prelude.rnf targetGroupArn
      `Prelude.seq` Prelude.rnf containerPort

instance Core.ToJSON LoadBalancer where
  toJSON LoadBalancer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("loadBalancerName" Core..=)
              Prelude.<$> loadBalancerName,
            ("containerName" Core..=) Prelude.<$> containerName,
            ("targetGroupArn" Core..=)
              Prelude.<$> targetGroupArn,
            ("containerPort" Core..=) Prelude.<$> containerPort
          ]
      )
