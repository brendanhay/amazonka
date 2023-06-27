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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsServiceLoadBalancersDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsServiceLoadBalancersDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a load balancer that the service uses.
--
-- /See:/ 'newAwsEcsServiceLoadBalancersDetails' smart constructor.
data AwsEcsServiceLoadBalancersDetails = AwsEcsServiceLoadBalancersDetails'
  { -- | The name of the container to associate with the load balancer.
    containerName :: Prelude.Maybe Prelude.Text,
    -- | The port on the container to associate with the load balancer. This port
    -- must correspond to a @containerPort@ in the task definition the tasks in
    -- the service are using. For tasks that use the EC2 launch type, the
    -- container instance they are launched on must allow ingress traffic on
    -- the @hostPort@ of the port mapping.
    containerPort :: Prelude.Maybe Prelude.Int,
    -- | The name of the load balancer to associate with the Amazon ECS service
    -- or task set.
    --
    -- Only specified when using a Classic Load Balancer. For an Application
    -- Load Balancer or a Network Load Balancer, the load balancer name is
    -- omitted.
    loadBalancerName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Elastic Load Balancing target group or groups associated
    -- with a service or task set.
    --
    -- Only specified when using an Application Load Balancer or a Network Load
    -- Balancer. For a Classic Load Balancer, the target group ARN is omitted.
    targetGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsServiceLoadBalancersDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'awsEcsServiceLoadBalancersDetails_containerName' - The name of the container to associate with the load balancer.
--
-- 'containerPort', 'awsEcsServiceLoadBalancersDetails_containerPort' - The port on the container to associate with the load balancer. This port
-- must correspond to a @containerPort@ in the task definition the tasks in
-- the service are using. For tasks that use the EC2 launch type, the
-- container instance they are launched on must allow ingress traffic on
-- the @hostPort@ of the port mapping.
--
-- 'loadBalancerName', 'awsEcsServiceLoadBalancersDetails_loadBalancerName' - The name of the load balancer to associate with the Amazon ECS service
-- or task set.
--
-- Only specified when using a Classic Load Balancer. For an Application
-- Load Balancer or a Network Load Balancer, the load balancer name is
-- omitted.
--
-- 'targetGroupArn', 'awsEcsServiceLoadBalancersDetails_targetGroupArn' - The ARN of the Elastic Load Balancing target group or groups associated
-- with a service or task set.
--
-- Only specified when using an Application Load Balancer or a Network Load
-- Balancer. For a Classic Load Balancer, the target group ARN is omitted.
newAwsEcsServiceLoadBalancersDetails ::
  AwsEcsServiceLoadBalancersDetails
newAwsEcsServiceLoadBalancersDetails =
  AwsEcsServiceLoadBalancersDetails'
    { containerName =
        Prelude.Nothing,
      containerPort = Prelude.Nothing,
      loadBalancerName = Prelude.Nothing,
      targetGroupArn = Prelude.Nothing
    }

-- | The name of the container to associate with the load balancer.
awsEcsServiceLoadBalancersDetails_containerName :: Lens.Lens' AwsEcsServiceLoadBalancersDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceLoadBalancersDetails_containerName = Lens.lens (\AwsEcsServiceLoadBalancersDetails' {containerName} -> containerName) (\s@AwsEcsServiceLoadBalancersDetails' {} a -> s {containerName = a} :: AwsEcsServiceLoadBalancersDetails)

-- | The port on the container to associate with the load balancer. This port
-- must correspond to a @containerPort@ in the task definition the tasks in
-- the service are using. For tasks that use the EC2 launch type, the
-- container instance they are launched on must allow ingress traffic on
-- the @hostPort@ of the port mapping.
awsEcsServiceLoadBalancersDetails_containerPort :: Lens.Lens' AwsEcsServiceLoadBalancersDetails (Prelude.Maybe Prelude.Int)
awsEcsServiceLoadBalancersDetails_containerPort = Lens.lens (\AwsEcsServiceLoadBalancersDetails' {containerPort} -> containerPort) (\s@AwsEcsServiceLoadBalancersDetails' {} a -> s {containerPort = a} :: AwsEcsServiceLoadBalancersDetails)

-- | The name of the load balancer to associate with the Amazon ECS service
-- or task set.
--
-- Only specified when using a Classic Load Balancer. For an Application
-- Load Balancer or a Network Load Balancer, the load balancer name is
-- omitted.
awsEcsServiceLoadBalancersDetails_loadBalancerName :: Lens.Lens' AwsEcsServiceLoadBalancersDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceLoadBalancersDetails_loadBalancerName = Lens.lens (\AwsEcsServiceLoadBalancersDetails' {loadBalancerName} -> loadBalancerName) (\s@AwsEcsServiceLoadBalancersDetails' {} a -> s {loadBalancerName = a} :: AwsEcsServiceLoadBalancersDetails)

-- | The ARN of the Elastic Load Balancing target group or groups associated
-- with a service or task set.
--
-- Only specified when using an Application Load Balancer or a Network Load
-- Balancer. For a Classic Load Balancer, the target group ARN is omitted.
awsEcsServiceLoadBalancersDetails_targetGroupArn :: Lens.Lens' AwsEcsServiceLoadBalancersDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceLoadBalancersDetails_targetGroupArn = Lens.lens (\AwsEcsServiceLoadBalancersDetails' {targetGroupArn} -> targetGroupArn) (\s@AwsEcsServiceLoadBalancersDetails' {} a -> s {targetGroupArn = a} :: AwsEcsServiceLoadBalancersDetails)

instance
  Data.FromJSON
    AwsEcsServiceLoadBalancersDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsServiceLoadBalancersDetails"
      ( \x ->
          AwsEcsServiceLoadBalancersDetails'
            Prelude.<$> (x Data..:? "ContainerName")
            Prelude.<*> (x Data..:? "ContainerPort")
            Prelude.<*> (x Data..:? "LoadBalancerName")
            Prelude.<*> (x Data..:? "TargetGroupArn")
      )

instance
  Prelude.Hashable
    AwsEcsServiceLoadBalancersDetails
  where
  hashWithSalt
    _salt
    AwsEcsServiceLoadBalancersDetails' {..} =
      _salt
        `Prelude.hashWithSalt` containerName
        `Prelude.hashWithSalt` containerPort
        `Prelude.hashWithSalt` loadBalancerName
        `Prelude.hashWithSalt` targetGroupArn

instance
  Prelude.NFData
    AwsEcsServiceLoadBalancersDetails
  where
  rnf AwsEcsServiceLoadBalancersDetails' {..} =
    Prelude.rnf containerName
      `Prelude.seq` Prelude.rnf containerPort
      `Prelude.seq` Prelude.rnf loadBalancerName
      `Prelude.seq` Prelude.rnf targetGroupArn

instance
  Data.ToJSON
    AwsEcsServiceLoadBalancersDetails
  where
  toJSON AwsEcsServiceLoadBalancersDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContainerName" Data..=) Prelude.<$> containerName,
            ("ContainerPort" Data..=) Prelude.<$> containerPort,
            ("LoadBalancerName" Data..=)
              Prelude.<$> loadBalancerName,
            ("TargetGroupArn" Data..=)
              Prelude.<$> targetGroupArn
          ]
      )
