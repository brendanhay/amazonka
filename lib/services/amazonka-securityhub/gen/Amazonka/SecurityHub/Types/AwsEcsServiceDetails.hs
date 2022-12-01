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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsServiceDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsServiceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcsServiceCapacityProviderStrategyDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceDeploymentConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceDeploymentControllerDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceLoadBalancersDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceNetworkConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsServicePlacementConstraintsDetails
import Amazonka.SecurityHub.Types.AwsEcsServicePlacementStrategiesDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceServiceRegistriesDetails

-- | Provides details about a service within an ECS cluster.
--
-- /See:/ 'newAwsEcsServiceDetails' smart constructor.
data AwsEcsServiceDetails = AwsEcsServiceDetails'
  { -- | After a task starts, the amount of time in seconds that the Amazon ECS
    -- service scheduler ignores unhealthy Elastic Load Balancing target health
    -- checks.
    healthCheckGracePeriodSeconds :: Prelude.Maybe Prelude.Int,
    -- | The name of the service.
    name :: Prelude.Maybe Prelude.Text,
    -- | Deployment parameters for the service. Includes the number of tasks that
    -- run and the order in which to start and stop tasks.
    deploymentConfiguration :: Prelude.Maybe AwsEcsServiceDeploymentConfigurationDetails,
    -- | The scheduling strategy to use for the service.
    --
    -- The @REPLICA@ scheduling strategy places and maintains the desired
    -- number of tasks across the cluster. By default, the service scheduler
    -- spreads tasks across Availability Zones. Task placement strategies and
    -- constraints are used to customize task placement decisions.
    --
    -- The @DAEMON@ scheduling strategy deploys exactly one task on each active
    -- container instance that meets all of the task placement constraints that
    -- are specified in the cluster. The service scheduler also evaluates the
    -- task placement constraints for running tasks and stops tasks that do not
    -- meet the placement constraints.
    --
    -- Valid values: @REPLICA@ | @DAEMON@
    schedulingStrategy :: Prelude.Maybe Prelude.Text,
    -- | Information about the service discovery registries to assign to the
    -- service.
    serviceRegistries :: Prelude.Maybe [AwsEcsServiceServiceRegistriesDetails],
    -- | The ARN of the cluster that hosts the service.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | Information about how tasks for the service are placed.
    placementStrategies :: Prelude.Maybe [AwsEcsServicePlacementStrategiesDetails],
    -- | The task definition to use for tasks in the service.
    taskDefinition :: Prelude.Maybe Prelude.Text,
    -- | For tasks that use the @awsvpc@ networking mode, the VPC subnet and
    -- security group configuration.
    networkConfiguration :: Prelude.Maybe AwsEcsServiceNetworkConfigurationDetails,
    -- | The number of instantiations of the task definition to run on the
    -- service.
    desiredCount :: Prelude.Maybe Prelude.Int,
    -- | Whether the execute command functionality is enabled for the service.
    enableExecuteCommand :: Prelude.Maybe Prelude.Bool,
    -- | The capacity provider strategy that the service uses.
    capacityProviderStrategy :: Prelude.Maybe [AwsEcsServiceCapacityProviderStrategyDetails],
    -- | The placement constraints for the tasks in the service.
    placementConstraints :: Prelude.Maybe [AwsEcsServicePlacementConstraintsDetails],
    -- | Indicates whether to propagate the tags from the task definition to the
    -- task or from the service to the task. If no value is provided, then tags
    -- are not propagated.
    --
    -- Valid values: @TASK_DEFINITION@ | @SERVICE@
    propagateTags :: Prelude.Maybe Prelude.Text,
    -- | Contains the deployment controller type that the service uses.
    deploymentController :: Prelude.Maybe AwsEcsServiceDeploymentControllerDetails,
    -- | Information about the load balancers that the service uses.
    loadBalancers :: Prelude.Maybe [AwsEcsServiceLoadBalancersDetails],
    -- | The launch type that the service uses.
    --
    -- Valid values: @EC2@ | @FARGATE@ | @EXTERNAL@
    launchType :: Prelude.Maybe Prelude.Text,
    -- | The platform version on which to run the service. Only specified for
    -- tasks that are hosted on Fargate. If a platform version is not
    -- specified, the @LATEST@ platform version is used by default.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role that is associated with the service. The role
    -- allows the Amazon ECS container agent to register container instances
    -- with an Elastic Load Balancing load balancer.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The name of the service.
    --
    -- The name can contain up to 255 characters. It can use letters, numbers,
    -- underscores, and hyphens.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | Whether to enable Amazon ECS managed tags for the tasks in the service.
    enableEcsManagedTags :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the service.
    serviceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsServiceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthCheckGracePeriodSeconds', 'awsEcsServiceDetails_healthCheckGracePeriodSeconds' - After a task starts, the amount of time in seconds that the Amazon ECS
-- service scheduler ignores unhealthy Elastic Load Balancing target health
-- checks.
--
-- 'name', 'awsEcsServiceDetails_name' - The name of the service.
--
-- 'deploymentConfiguration', 'awsEcsServiceDetails_deploymentConfiguration' - Deployment parameters for the service. Includes the number of tasks that
-- run and the order in which to start and stop tasks.
--
-- 'schedulingStrategy', 'awsEcsServiceDetails_schedulingStrategy' - The scheduling strategy to use for the service.
--
-- The @REPLICA@ scheduling strategy places and maintains the desired
-- number of tasks across the cluster. By default, the service scheduler
-- spreads tasks across Availability Zones. Task placement strategies and
-- constraints are used to customize task placement decisions.
--
-- The @DAEMON@ scheduling strategy deploys exactly one task on each active
-- container instance that meets all of the task placement constraints that
-- are specified in the cluster. The service scheduler also evaluates the
-- task placement constraints for running tasks and stops tasks that do not
-- meet the placement constraints.
--
-- Valid values: @REPLICA@ | @DAEMON@
--
-- 'serviceRegistries', 'awsEcsServiceDetails_serviceRegistries' - Information about the service discovery registries to assign to the
-- service.
--
-- 'cluster', 'awsEcsServiceDetails_cluster' - The ARN of the cluster that hosts the service.
--
-- 'placementStrategies', 'awsEcsServiceDetails_placementStrategies' - Information about how tasks for the service are placed.
--
-- 'taskDefinition', 'awsEcsServiceDetails_taskDefinition' - The task definition to use for tasks in the service.
--
-- 'networkConfiguration', 'awsEcsServiceDetails_networkConfiguration' - For tasks that use the @awsvpc@ networking mode, the VPC subnet and
-- security group configuration.
--
-- 'desiredCount', 'awsEcsServiceDetails_desiredCount' - The number of instantiations of the task definition to run on the
-- service.
--
-- 'enableExecuteCommand', 'awsEcsServiceDetails_enableExecuteCommand' - Whether the execute command functionality is enabled for the service.
--
-- 'capacityProviderStrategy', 'awsEcsServiceDetails_capacityProviderStrategy' - The capacity provider strategy that the service uses.
--
-- 'placementConstraints', 'awsEcsServiceDetails_placementConstraints' - The placement constraints for the tasks in the service.
--
-- 'propagateTags', 'awsEcsServiceDetails_propagateTags' - Indicates whether to propagate the tags from the task definition to the
-- task or from the service to the task. If no value is provided, then tags
-- are not propagated.
--
-- Valid values: @TASK_DEFINITION@ | @SERVICE@
--
-- 'deploymentController', 'awsEcsServiceDetails_deploymentController' - Contains the deployment controller type that the service uses.
--
-- 'loadBalancers', 'awsEcsServiceDetails_loadBalancers' - Information about the load balancers that the service uses.
--
-- 'launchType', 'awsEcsServiceDetails_launchType' - The launch type that the service uses.
--
-- Valid values: @EC2@ | @FARGATE@ | @EXTERNAL@
--
-- 'platformVersion', 'awsEcsServiceDetails_platformVersion' - The platform version on which to run the service. Only specified for
-- tasks that are hosted on Fargate. If a platform version is not
-- specified, the @LATEST@ platform version is used by default.
--
-- 'role'', 'awsEcsServiceDetails_role' - The ARN of the IAM role that is associated with the service. The role
-- allows the Amazon ECS container agent to register container instances
-- with an Elastic Load Balancing load balancer.
--
-- 'serviceName', 'awsEcsServiceDetails_serviceName' - The name of the service.
--
-- The name can contain up to 255 characters. It can use letters, numbers,
-- underscores, and hyphens.
--
-- 'enableEcsManagedTags', 'awsEcsServiceDetails_enableEcsManagedTags' - Whether to enable Amazon ECS managed tags for the tasks in the service.
--
-- 'serviceArn', 'awsEcsServiceDetails_serviceArn' - The ARN of the service.
newAwsEcsServiceDetails ::
  AwsEcsServiceDetails
newAwsEcsServiceDetails =
  AwsEcsServiceDetails'
    { healthCheckGracePeriodSeconds =
        Prelude.Nothing,
      name = Prelude.Nothing,
      deploymentConfiguration = Prelude.Nothing,
      schedulingStrategy = Prelude.Nothing,
      serviceRegistries = Prelude.Nothing,
      cluster = Prelude.Nothing,
      placementStrategies = Prelude.Nothing,
      taskDefinition = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      desiredCount = Prelude.Nothing,
      enableExecuteCommand = Prelude.Nothing,
      capacityProviderStrategy = Prelude.Nothing,
      placementConstraints = Prelude.Nothing,
      propagateTags = Prelude.Nothing,
      deploymentController = Prelude.Nothing,
      loadBalancers = Prelude.Nothing,
      launchType = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      role' = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      enableEcsManagedTags = Prelude.Nothing,
      serviceArn = Prelude.Nothing
    }

-- | After a task starts, the amount of time in seconds that the Amazon ECS
-- service scheduler ignores unhealthy Elastic Load Balancing target health
-- checks.
awsEcsServiceDetails_healthCheckGracePeriodSeconds :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Int)
awsEcsServiceDetails_healthCheckGracePeriodSeconds = Lens.lens (\AwsEcsServiceDetails' {healthCheckGracePeriodSeconds} -> healthCheckGracePeriodSeconds) (\s@AwsEcsServiceDetails' {} a -> s {healthCheckGracePeriodSeconds = a} :: AwsEcsServiceDetails)

-- | The name of the service.
awsEcsServiceDetails_name :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_name = Lens.lens (\AwsEcsServiceDetails' {name} -> name) (\s@AwsEcsServiceDetails' {} a -> s {name = a} :: AwsEcsServiceDetails)

-- | Deployment parameters for the service. Includes the number of tasks that
-- run and the order in which to start and stop tasks.
awsEcsServiceDetails_deploymentConfiguration :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe AwsEcsServiceDeploymentConfigurationDetails)
awsEcsServiceDetails_deploymentConfiguration = Lens.lens (\AwsEcsServiceDetails' {deploymentConfiguration} -> deploymentConfiguration) (\s@AwsEcsServiceDetails' {} a -> s {deploymentConfiguration = a} :: AwsEcsServiceDetails)

-- | The scheduling strategy to use for the service.
--
-- The @REPLICA@ scheduling strategy places and maintains the desired
-- number of tasks across the cluster. By default, the service scheduler
-- spreads tasks across Availability Zones. Task placement strategies and
-- constraints are used to customize task placement decisions.
--
-- The @DAEMON@ scheduling strategy deploys exactly one task on each active
-- container instance that meets all of the task placement constraints that
-- are specified in the cluster. The service scheduler also evaluates the
-- task placement constraints for running tasks and stops tasks that do not
-- meet the placement constraints.
--
-- Valid values: @REPLICA@ | @DAEMON@
awsEcsServiceDetails_schedulingStrategy :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_schedulingStrategy = Lens.lens (\AwsEcsServiceDetails' {schedulingStrategy} -> schedulingStrategy) (\s@AwsEcsServiceDetails' {} a -> s {schedulingStrategy = a} :: AwsEcsServiceDetails)

-- | Information about the service discovery registries to assign to the
-- service.
awsEcsServiceDetails_serviceRegistries :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe [AwsEcsServiceServiceRegistriesDetails])
awsEcsServiceDetails_serviceRegistries = Lens.lens (\AwsEcsServiceDetails' {serviceRegistries} -> serviceRegistries) (\s@AwsEcsServiceDetails' {} a -> s {serviceRegistries = a} :: AwsEcsServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the cluster that hosts the service.
awsEcsServiceDetails_cluster :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_cluster = Lens.lens (\AwsEcsServiceDetails' {cluster} -> cluster) (\s@AwsEcsServiceDetails' {} a -> s {cluster = a} :: AwsEcsServiceDetails)

-- | Information about how tasks for the service are placed.
awsEcsServiceDetails_placementStrategies :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe [AwsEcsServicePlacementStrategiesDetails])
awsEcsServiceDetails_placementStrategies = Lens.lens (\AwsEcsServiceDetails' {placementStrategies} -> placementStrategies) (\s@AwsEcsServiceDetails' {} a -> s {placementStrategies = a} :: AwsEcsServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The task definition to use for tasks in the service.
awsEcsServiceDetails_taskDefinition :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_taskDefinition = Lens.lens (\AwsEcsServiceDetails' {taskDefinition} -> taskDefinition) (\s@AwsEcsServiceDetails' {} a -> s {taskDefinition = a} :: AwsEcsServiceDetails)

-- | For tasks that use the @awsvpc@ networking mode, the VPC subnet and
-- security group configuration.
awsEcsServiceDetails_networkConfiguration :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe AwsEcsServiceNetworkConfigurationDetails)
awsEcsServiceDetails_networkConfiguration = Lens.lens (\AwsEcsServiceDetails' {networkConfiguration} -> networkConfiguration) (\s@AwsEcsServiceDetails' {} a -> s {networkConfiguration = a} :: AwsEcsServiceDetails)

-- | The number of instantiations of the task definition to run on the
-- service.
awsEcsServiceDetails_desiredCount :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Int)
awsEcsServiceDetails_desiredCount = Lens.lens (\AwsEcsServiceDetails' {desiredCount} -> desiredCount) (\s@AwsEcsServiceDetails' {} a -> s {desiredCount = a} :: AwsEcsServiceDetails)

-- | Whether the execute command functionality is enabled for the service.
awsEcsServiceDetails_enableExecuteCommand :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Bool)
awsEcsServiceDetails_enableExecuteCommand = Lens.lens (\AwsEcsServiceDetails' {enableExecuteCommand} -> enableExecuteCommand) (\s@AwsEcsServiceDetails' {} a -> s {enableExecuteCommand = a} :: AwsEcsServiceDetails)

-- | The capacity provider strategy that the service uses.
awsEcsServiceDetails_capacityProviderStrategy :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe [AwsEcsServiceCapacityProviderStrategyDetails])
awsEcsServiceDetails_capacityProviderStrategy = Lens.lens (\AwsEcsServiceDetails' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@AwsEcsServiceDetails' {} a -> s {capacityProviderStrategy = a} :: AwsEcsServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The placement constraints for the tasks in the service.
awsEcsServiceDetails_placementConstraints :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe [AwsEcsServicePlacementConstraintsDetails])
awsEcsServiceDetails_placementConstraints = Lens.lens (\AwsEcsServiceDetails' {placementConstraints} -> placementConstraints) (\s@AwsEcsServiceDetails' {} a -> s {placementConstraints = a} :: AwsEcsServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether to propagate the tags from the task definition to the
-- task or from the service to the task. If no value is provided, then tags
-- are not propagated.
--
-- Valid values: @TASK_DEFINITION@ | @SERVICE@
awsEcsServiceDetails_propagateTags :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_propagateTags = Lens.lens (\AwsEcsServiceDetails' {propagateTags} -> propagateTags) (\s@AwsEcsServiceDetails' {} a -> s {propagateTags = a} :: AwsEcsServiceDetails)

-- | Contains the deployment controller type that the service uses.
awsEcsServiceDetails_deploymentController :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe AwsEcsServiceDeploymentControllerDetails)
awsEcsServiceDetails_deploymentController = Lens.lens (\AwsEcsServiceDetails' {deploymentController} -> deploymentController) (\s@AwsEcsServiceDetails' {} a -> s {deploymentController = a} :: AwsEcsServiceDetails)

-- | Information about the load balancers that the service uses.
awsEcsServiceDetails_loadBalancers :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe [AwsEcsServiceLoadBalancersDetails])
awsEcsServiceDetails_loadBalancers = Lens.lens (\AwsEcsServiceDetails' {loadBalancers} -> loadBalancers) (\s@AwsEcsServiceDetails' {} a -> s {loadBalancers = a} :: AwsEcsServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The launch type that the service uses.
--
-- Valid values: @EC2@ | @FARGATE@ | @EXTERNAL@
awsEcsServiceDetails_launchType :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_launchType = Lens.lens (\AwsEcsServiceDetails' {launchType} -> launchType) (\s@AwsEcsServiceDetails' {} a -> s {launchType = a} :: AwsEcsServiceDetails)

-- | The platform version on which to run the service. Only specified for
-- tasks that are hosted on Fargate. If a platform version is not
-- specified, the @LATEST@ platform version is used by default.
awsEcsServiceDetails_platformVersion :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_platformVersion = Lens.lens (\AwsEcsServiceDetails' {platformVersion} -> platformVersion) (\s@AwsEcsServiceDetails' {} a -> s {platformVersion = a} :: AwsEcsServiceDetails)

-- | The ARN of the IAM role that is associated with the service. The role
-- allows the Amazon ECS container agent to register container instances
-- with an Elastic Load Balancing load balancer.
awsEcsServiceDetails_role :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_role = Lens.lens (\AwsEcsServiceDetails' {role'} -> role') (\s@AwsEcsServiceDetails' {} a -> s {role' = a} :: AwsEcsServiceDetails)

-- | The name of the service.
--
-- The name can contain up to 255 characters. It can use letters, numbers,
-- underscores, and hyphens.
awsEcsServiceDetails_serviceName :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_serviceName = Lens.lens (\AwsEcsServiceDetails' {serviceName} -> serviceName) (\s@AwsEcsServiceDetails' {} a -> s {serviceName = a} :: AwsEcsServiceDetails)

-- | Whether to enable Amazon ECS managed tags for the tasks in the service.
awsEcsServiceDetails_enableEcsManagedTags :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Bool)
awsEcsServiceDetails_enableEcsManagedTags = Lens.lens (\AwsEcsServiceDetails' {enableEcsManagedTags} -> enableEcsManagedTags) (\s@AwsEcsServiceDetails' {} a -> s {enableEcsManagedTags = a} :: AwsEcsServiceDetails)

-- | The ARN of the service.
awsEcsServiceDetails_serviceArn :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_serviceArn = Lens.lens (\AwsEcsServiceDetails' {serviceArn} -> serviceArn) (\s@AwsEcsServiceDetails' {} a -> s {serviceArn = a} :: AwsEcsServiceDetails)

instance Core.FromJSON AwsEcsServiceDetails where
  parseJSON =
    Core.withObject
      "AwsEcsServiceDetails"
      ( \x ->
          AwsEcsServiceDetails'
            Prelude.<$> (x Core..:? "HealthCheckGracePeriodSeconds")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "DeploymentConfiguration")
            Prelude.<*> (x Core..:? "SchedulingStrategy")
            Prelude.<*> ( x Core..:? "ServiceRegistries"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Cluster")
            Prelude.<*> ( x Core..:? "PlacementStrategies"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "TaskDefinition")
            Prelude.<*> (x Core..:? "NetworkConfiguration")
            Prelude.<*> (x Core..:? "DesiredCount")
            Prelude.<*> (x Core..:? "EnableExecuteCommand")
            Prelude.<*> ( x Core..:? "CapacityProviderStrategy"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "PlacementConstraints"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "PropagateTags")
            Prelude.<*> (x Core..:? "DeploymentController")
            Prelude.<*> (x Core..:? "LoadBalancers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LaunchType")
            Prelude.<*> (x Core..:? "PlatformVersion")
            Prelude.<*> (x Core..:? "Role")
            Prelude.<*> (x Core..:? "ServiceName")
            Prelude.<*> (x Core..:? "EnableEcsManagedTags")
            Prelude.<*> (x Core..:? "ServiceArn")
      )

instance Prelude.Hashable AwsEcsServiceDetails where
  hashWithSalt _salt AwsEcsServiceDetails' {..} =
    _salt
      `Prelude.hashWithSalt` healthCheckGracePeriodSeconds
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` deploymentConfiguration
      `Prelude.hashWithSalt` schedulingStrategy
      `Prelude.hashWithSalt` serviceRegistries
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` placementStrategies
      `Prelude.hashWithSalt` taskDefinition
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` desiredCount
      `Prelude.hashWithSalt` enableExecuteCommand
      `Prelude.hashWithSalt` capacityProviderStrategy
      `Prelude.hashWithSalt` placementConstraints
      `Prelude.hashWithSalt` propagateTags
      `Prelude.hashWithSalt` deploymentController
      `Prelude.hashWithSalt` loadBalancers
      `Prelude.hashWithSalt` launchType
      `Prelude.hashWithSalt` platformVersion
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` enableEcsManagedTags
      `Prelude.hashWithSalt` serviceArn

instance Prelude.NFData AwsEcsServiceDetails where
  rnf AwsEcsServiceDetails' {..} =
    Prelude.rnf healthCheckGracePeriodSeconds
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf deploymentConfiguration
      `Prelude.seq` Prelude.rnf schedulingStrategy
      `Prelude.seq` Prelude.rnf serviceRegistries
      `Prelude.seq` Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf placementStrategies
      `Prelude.seq` Prelude.rnf taskDefinition
      `Prelude.seq` Prelude.rnf networkConfiguration
      `Prelude.seq` Prelude.rnf desiredCount
      `Prelude.seq` Prelude.rnf enableExecuteCommand
      `Prelude.seq` Prelude.rnf capacityProviderStrategy
      `Prelude.seq` Prelude.rnf placementConstraints
      `Prelude.seq` Prelude.rnf propagateTags
      `Prelude.seq` Prelude.rnf deploymentController
      `Prelude.seq` Prelude.rnf loadBalancers
      `Prelude.seq` Prelude.rnf launchType
      `Prelude.seq` Prelude.rnf platformVersion
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf
        enableEcsManagedTags
      `Prelude.seq` Prelude.rnf serviceArn

instance Core.ToJSON AwsEcsServiceDetails where
  toJSON AwsEcsServiceDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("HealthCheckGracePeriodSeconds" Core..=)
              Prelude.<$> healthCheckGracePeriodSeconds,
            ("Name" Core..=) Prelude.<$> name,
            ("DeploymentConfiguration" Core..=)
              Prelude.<$> deploymentConfiguration,
            ("SchedulingStrategy" Core..=)
              Prelude.<$> schedulingStrategy,
            ("ServiceRegistries" Core..=)
              Prelude.<$> serviceRegistries,
            ("Cluster" Core..=) Prelude.<$> cluster,
            ("PlacementStrategies" Core..=)
              Prelude.<$> placementStrategies,
            ("TaskDefinition" Core..=)
              Prelude.<$> taskDefinition,
            ("NetworkConfiguration" Core..=)
              Prelude.<$> networkConfiguration,
            ("DesiredCount" Core..=) Prelude.<$> desiredCount,
            ("EnableExecuteCommand" Core..=)
              Prelude.<$> enableExecuteCommand,
            ("CapacityProviderStrategy" Core..=)
              Prelude.<$> capacityProviderStrategy,
            ("PlacementConstraints" Core..=)
              Prelude.<$> placementConstraints,
            ("PropagateTags" Core..=) Prelude.<$> propagateTags,
            ("DeploymentController" Core..=)
              Prelude.<$> deploymentController,
            ("LoadBalancers" Core..=) Prelude.<$> loadBalancers,
            ("LaunchType" Core..=) Prelude.<$> launchType,
            ("PlatformVersion" Core..=)
              Prelude.<$> platformVersion,
            ("Role" Core..=) Prelude.<$> role',
            ("ServiceName" Core..=) Prelude.<$> serviceName,
            ("EnableEcsManagedTags" Core..=)
              Prelude.<$> enableEcsManagedTags,
            ("ServiceArn" Core..=) Prelude.<$> serviceArn
          ]
      )
