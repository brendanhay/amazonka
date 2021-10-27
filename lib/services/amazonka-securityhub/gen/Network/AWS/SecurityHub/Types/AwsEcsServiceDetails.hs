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
-- Module      : Network.AWS.SecurityHub.Types.AwsEcsServiceDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsEcsServiceDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsEcsServiceCapacityProviderStrategyDetails
import Network.AWS.SecurityHub.Types.AwsEcsServiceDeploymentConfigurationDetails
import Network.AWS.SecurityHub.Types.AwsEcsServiceDeploymentControllerDetails
import Network.AWS.SecurityHub.Types.AwsEcsServiceLoadBalancersDetails
import Network.AWS.SecurityHub.Types.AwsEcsServiceNetworkConfigurationDetails
import Network.AWS.SecurityHub.Types.AwsEcsServicePlacementConstraintsDetails
import Network.AWS.SecurityHub.Types.AwsEcsServicePlacementStrategiesDetails
import Network.AWS.SecurityHub.Types.AwsEcsServiceServiceRegistriesDetails

-- | Provides details about a service within an ECS cluster.
--
-- /See:/ 'newAwsEcsServiceDetails' smart constructor.
data AwsEcsServiceDetails = AwsEcsServiceDetails'
  { -- | Information about how tasks for the service are placed.
    placementStrategies :: Prelude.Maybe [AwsEcsServicePlacementStrategiesDetails],
    -- | The ARN of the cluster that hosts the service.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to propagate the tags from the task definition to the
    -- task or from the service to the task. If no value is provided, then tags
    -- are not propagated.
    --
    -- Valid values: @TASK_DEFINITION@ | @SERVICE@
    propagateTags :: Prelude.Maybe Prelude.Text,
    -- | The platform version on which to run the service. Only specified for
    -- tasks that are hosted on Fargate. If a platform version is not
    -- specified, the @LATEST@ platform version is used by default.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | Whether to enable Amazon ECS managed tags for the tasks in the service.
    enableEcsManagedTags :: Prelude.Maybe Prelude.Bool,
    -- | The number of instantiations of the task definition to run on the
    -- service.
    desiredCount :: Prelude.Maybe Prelude.Int,
    -- | Information about the load balancers that the service uses.
    loadBalancers :: Prelude.Maybe [AwsEcsServiceLoadBalancersDetails],
    -- | The ARN of the IAM role that is associated with the service. The role
    -- allows the Amazon ECS container agent to register container instances
    -- with an Elastic Load Balancing load balancer.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The name of the service.
    name :: Prelude.Maybe Prelude.Text,
    -- | The placement constraints for the tasks in the service.
    placementConstraints :: Prelude.Maybe [AwsEcsServicePlacementConstraintsDetails],
    -- | The name of the service.
    --
    -- The name can contain up to 255 characters. It can use letters, numbers,
    -- underscores, and hyphens.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | Contains the deployment controller type that the service uses.
    deploymentController :: Prelude.Maybe AwsEcsServiceDeploymentControllerDetails,
    -- | The launch type that the service uses.
    --
    -- Valid values: @EC2@ | @FARGATE@ | @EXTERNAL@
    launchType :: Prelude.Maybe Prelude.Text,
    -- | The task definition to use for tasks in the service.
    taskDefinition :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the service.
    serviceArn :: Prelude.Maybe Prelude.Text,
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
    -- | The capacity provider strategy that the service uses.
    capacityProviderStrategy :: Prelude.Maybe [AwsEcsServiceCapacityProviderStrategyDetails],
    -- | Information about the service discovery registries to assign to the
    -- service.
    serviceRegistries :: Prelude.Maybe [AwsEcsServiceServiceRegistriesDetails],
    -- | After a task starts, the amount of time in seconds that the Amazon ECS
    -- service scheduler ignores unhealthy Elastic Load Balancing target health
    -- checks.
    healthCheckGracePeriodSeconds :: Prelude.Maybe Prelude.Int,
    -- | For tasks that use the @awsvpc@ networking mode, the VPC subnet and
    -- security group configuration.
    networkConfiguration :: Prelude.Maybe AwsEcsServiceNetworkConfigurationDetails,
    -- | Deployment parameters for the service. Includes the number of tasks that
    -- run and the order in which to start and stop tasks.
    deploymentConfiguration :: Prelude.Maybe AwsEcsServiceDeploymentConfigurationDetails,
    -- | Whether the execute command functionality is enabled for the service.
    enableExecuteCommand :: Prelude.Maybe Prelude.Bool
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
-- 'placementStrategies', 'awsEcsServiceDetails_placementStrategies' - Information about how tasks for the service are placed.
--
-- 'cluster', 'awsEcsServiceDetails_cluster' - The ARN of the cluster that hosts the service.
--
-- 'propagateTags', 'awsEcsServiceDetails_propagateTags' - Indicates whether to propagate the tags from the task definition to the
-- task or from the service to the task. If no value is provided, then tags
-- are not propagated.
--
-- Valid values: @TASK_DEFINITION@ | @SERVICE@
--
-- 'platformVersion', 'awsEcsServiceDetails_platformVersion' - The platform version on which to run the service. Only specified for
-- tasks that are hosted on Fargate. If a platform version is not
-- specified, the @LATEST@ platform version is used by default.
--
-- 'enableEcsManagedTags', 'awsEcsServiceDetails_enableEcsManagedTags' - Whether to enable Amazon ECS managed tags for the tasks in the service.
--
-- 'desiredCount', 'awsEcsServiceDetails_desiredCount' - The number of instantiations of the task definition to run on the
-- service.
--
-- 'loadBalancers', 'awsEcsServiceDetails_loadBalancers' - Information about the load balancers that the service uses.
--
-- 'role'', 'awsEcsServiceDetails_role' - The ARN of the IAM role that is associated with the service. The role
-- allows the Amazon ECS container agent to register container instances
-- with an Elastic Load Balancing load balancer.
--
-- 'name', 'awsEcsServiceDetails_name' - The name of the service.
--
-- 'placementConstraints', 'awsEcsServiceDetails_placementConstraints' - The placement constraints for the tasks in the service.
--
-- 'serviceName', 'awsEcsServiceDetails_serviceName' - The name of the service.
--
-- The name can contain up to 255 characters. It can use letters, numbers,
-- underscores, and hyphens.
--
-- 'deploymentController', 'awsEcsServiceDetails_deploymentController' - Contains the deployment controller type that the service uses.
--
-- 'launchType', 'awsEcsServiceDetails_launchType' - The launch type that the service uses.
--
-- Valid values: @EC2@ | @FARGATE@ | @EXTERNAL@
--
-- 'taskDefinition', 'awsEcsServiceDetails_taskDefinition' - The task definition to use for tasks in the service.
--
-- 'serviceArn', 'awsEcsServiceDetails_serviceArn' - The ARN of the service.
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
-- 'capacityProviderStrategy', 'awsEcsServiceDetails_capacityProviderStrategy' - The capacity provider strategy that the service uses.
--
-- 'serviceRegistries', 'awsEcsServiceDetails_serviceRegistries' - Information about the service discovery registries to assign to the
-- service.
--
-- 'healthCheckGracePeriodSeconds', 'awsEcsServiceDetails_healthCheckGracePeriodSeconds' - After a task starts, the amount of time in seconds that the Amazon ECS
-- service scheduler ignores unhealthy Elastic Load Balancing target health
-- checks.
--
-- 'networkConfiguration', 'awsEcsServiceDetails_networkConfiguration' - For tasks that use the @awsvpc@ networking mode, the VPC subnet and
-- security group configuration.
--
-- 'deploymentConfiguration', 'awsEcsServiceDetails_deploymentConfiguration' - Deployment parameters for the service. Includes the number of tasks that
-- run and the order in which to start and stop tasks.
--
-- 'enableExecuteCommand', 'awsEcsServiceDetails_enableExecuteCommand' - Whether the execute command functionality is enabled for the service.
newAwsEcsServiceDetails ::
  AwsEcsServiceDetails
newAwsEcsServiceDetails =
  AwsEcsServiceDetails'
    { placementStrategies =
        Prelude.Nothing,
      cluster = Prelude.Nothing,
      propagateTags = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      enableEcsManagedTags = Prelude.Nothing,
      desiredCount = Prelude.Nothing,
      loadBalancers = Prelude.Nothing,
      role' = Prelude.Nothing,
      name = Prelude.Nothing,
      placementConstraints = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      deploymentController = Prelude.Nothing,
      launchType = Prelude.Nothing,
      taskDefinition = Prelude.Nothing,
      serviceArn = Prelude.Nothing,
      schedulingStrategy = Prelude.Nothing,
      capacityProviderStrategy = Prelude.Nothing,
      serviceRegistries = Prelude.Nothing,
      healthCheckGracePeriodSeconds = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      deploymentConfiguration = Prelude.Nothing,
      enableExecuteCommand = Prelude.Nothing
    }

-- | Information about how tasks for the service are placed.
awsEcsServiceDetails_placementStrategies :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe [AwsEcsServicePlacementStrategiesDetails])
awsEcsServiceDetails_placementStrategies = Lens.lens (\AwsEcsServiceDetails' {placementStrategies} -> placementStrategies) (\s@AwsEcsServiceDetails' {} a -> s {placementStrategies = a} :: AwsEcsServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the cluster that hosts the service.
awsEcsServiceDetails_cluster :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_cluster = Lens.lens (\AwsEcsServiceDetails' {cluster} -> cluster) (\s@AwsEcsServiceDetails' {} a -> s {cluster = a} :: AwsEcsServiceDetails)

-- | Indicates whether to propagate the tags from the task definition to the
-- task or from the service to the task. If no value is provided, then tags
-- are not propagated.
--
-- Valid values: @TASK_DEFINITION@ | @SERVICE@
awsEcsServiceDetails_propagateTags :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_propagateTags = Lens.lens (\AwsEcsServiceDetails' {propagateTags} -> propagateTags) (\s@AwsEcsServiceDetails' {} a -> s {propagateTags = a} :: AwsEcsServiceDetails)

-- | The platform version on which to run the service. Only specified for
-- tasks that are hosted on Fargate. If a platform version is not
-- specified, the @LATEST@ platform version is used by default.
awsEcsServiceDetails_platformVersion :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_platformVersion = Lens.lens (\AwsEcsServiceDetails' {platformVersion} -> platformVersion) (\s@AwsEcsServiceDetails' {} a -> s {platformVersion = a} :: AwsEcsServiceDetails)

-- | Whether to enable Amazon ECS managed tags for the tasks in the service.
awsEcsServiceDetails_enableEcsManagedTags :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Bool)
awsEcsServiceDetails_enableEcsManagedTags = Lens.lens (\AwsEcsServiceDetails' {enableEcsManagedTags} -> enableEcsManagedTags) (\s@AwsEcsServiceDetails' {} a -> s {enableEcsManagedTags = a} :: AwsEcsServiceDetails)

-- | The number of instantiations of the task definition to run on the
-- service.
awsEcsServiceDetails_desiredCount :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Int)
awsEcsServiceDetails_desiredCount = Lens.lens (\AwsEcsServiceDetails' {desiredCount} -> desiredCount) (\s@AwsEcsServiceDetails' {} a -> s {desiredCount = a} :: AwsEcsServiceDetails)

-- | Information about the load balancers that the service uses.
awsEcsServiceDetails_loadBalancers :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe [AwsEcsServiceLoadBalancersDetails])
awsEcsServiceDetails_loadBalancers = Lens.lens (\AwsEcsServiceDetails' {loadBalancers} -> loadBalancers) (\s@AwsEcsServiceDetails' {} a -> s {loadBalancers = a} :: AwsEcsServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the IAM role that is associated with the service. The role
-- allows the Amazon ECS container agent to register container instances
-- with an Elastic Load Balancing load balancer.
awsEcsServiceDetails_role :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_role = Lens.lens (\AwsEcsServiceDetails' {role'} -> role') (\s@AwsEcsServiceDetails' {} a -> s {role' = a} :: AwsEcsServiceDetails)

-- | The name of the service.
awsEcsServiceDetails_name :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_name = Lens.lens (\AwsEcsServiceDetails' {name} -> name) (\s@AwsEcsServiceDetails' {} a -> s {name = a} :: AwsEcsServiceDetails)

-- | The placement constraints for the tasks in the service.
awsEcsServiceDetails_placementConstraints :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe [AwsEcsServicePlacementConstraintsDetails])
awsEcsServiceDetails_placementConstraints = Lens.lens (\AwsEcsServiceDetails' {placementConstraints} -> placementConstraints) (\s@AwsEcsServiceDetails' {} a -> s {placementConstraints = a} :: AwsEcsServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the service.
--
-- The name can contain up to 255 characters. It can use letters, numbers,
-- underscores, and hyphens.
awsEcsServiceDetails_serviceName :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_serviceName = Lens.lens (\AwsEcsServiceDetails' {serviceName} -> serviceName) (\s@AwsEcsServiceDetails' {} a -> s {serviceName = a} :: AwsEcsServiceDetails)

-- | Contains the deployment controller type that the service uses.
awsEcsServiceDetails_deploymentController :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe AwsEcsServiceDeploymentControllerDetails)
awsEcsServiceDetails_deploymentController = Lens.lens (\AwsEcsServiceDetails' {deploymentController} -> deploymentController) (\s@AwsEcsServiceDetails' {} a -> s {deploymentController = a} :: AwsEcsServiceDetails)

-- | The launch type that the service uses.
--
-- Valid values: @EC2@ | @FARGATE@ | @EXTERNAL@
awsEcsServiceDetails_launchType :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_launchType = Lens.lens (\AwsEcsServiceDetails' {launchType} -> launchType) (\s@AwsEcsServiceDetails' {} a -> s {launchType = a} :: AwsEcsServiceDetails)

-- | The task definition to use for tasks in the service.
awsEcsServiceDetails_taskDefinition :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_taskDefinition = Lens.lens (\AwsEcsServiceDetails' {taskDefinition} -> taskDefinition) (\s@AwsEcsServiceDetails' {} a -> s {taskDefinition = a} :: AwsEcsServiceDetails)

-- | The ARN of the service.
awsEcsServiceDetails_serviceArn :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_serviceArn = Lens.lens (\AwsEcsServiceDetails' {serviceArn} -> serviceArn) (\s@AwsEcsServiceDetails' {} a -> s {serviceArn = a} :: AwsEcsServiceDetails)

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

-- | The capacity provider strategy that the service uses.
awsEcsServiceDetails_capacityProviderStrategy :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe [AwsEcsServiceCapacityProviderStrategyDetails])
awsEcsServiceDetails_capacityProviderStrategy = Lens.lens (\AwsEcsServiceDetails' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@AwsEcsServiceDetails' {} a -> s {capacityProviderStrategy = a} :: AwsEcsServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | Information about the service discovery registries to assign to the
-- service.
awsEcsServiceDetails_serviceRegistries :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe [AwsEcsServiceServiceRegistriesDetails])
awsEcsServiceDetails_serviceRegistries = Lens.lens (\AwsEcsServiceDetails' {serviceRegistries} -> serviceRegistries) (\s@AwsEcsServiceDetails' {} a -> s {serviceRegistries = a} :: AwsEcsServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | After a task starts, the amount of time in seconds that the Amazon ECS
-- service scheduler ignores unhealthy Elastic Load Balancing target health
-- checks.
awsEcsServiceDetails_healthCheckGracePeriodSeconds :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Int)
awsEcsServiceDetails_healthCheckGracePeriodSeconds = Lens.lens (\AwsEcsServiceDetails' {healthCheckGracePeriodSeconds} -> healthCheckGracePeriodSeconds) (\s@AwsEcsServiceDetails' {} a -> s {healthCheckGracePeriodSeconds = a} :: AwsEcsServiceDetails)

-- | For tasks that use the @awsvpc@ networking mode, the VPC subnet and
-- security group configuration.
awsEcsServiceDetails_networkConfiguration :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe AwsEcsServiceNetworkConfigurationDetails)
awsEcsServiceDetails_networkConfiguration = Lens.lens (\AwsEcsServiceDetails' {networkConfiguration} -> networkConfiguration) (\s@AwsEcsServiceDetails' {} a -> s {networkConfiguration = a} :: AwsEcsServiceDetails)

-- | Deployment parameters for the service. Includes the number of tasks that
-- run and the order in which to start and stop tasks.
awsEcsServiceDetails_deploymentConfiguration :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe AwsEcsServiceDeploymentConfigurationDetails)
awsEcsServiceDetails_deploymentConfiguration = Lens.lens (\AwsEcsServiceDetails' {deploymentConfiguration} -> deploymentConfiguration) (\s@AwsEcsServiceDetails' {} a -> s {deploymentConfiguration = a} :: AwsEcsServiceDetails)

-- | Whether the execute command functionality is enabled for the service.
awsEcsServiceDetails_enableExecuteCommand :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Bool)
awsEcsServiceDetails_enableExecuteCommand = Lens.lens (\AwsEcsServiceDetails' {enableExecuteCommand} -> enableExecuteCommand) (\s@AwsEcsServiceDetails' {} a -> s {enableExecuteCommand = a} :: AwsEcsServiceDetails)

instance Core.FromJSON AwsEcsServiceDetails where
  parseJSON =
    Core.withObject
      "AwsEcsServiceDetails"
      ( \x ->
          AwsEcsServiceDetails'
            Prelude.<$> ( x Core..:? "PlacementStrategies"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Cluster")
            Prelude.<*> (x Core..:? "PropagateTags")
            Prelude.<*> (x Core..:? "PlatformVersion")
            Prelude.<*> (x Core..:? "EnableEcsManagedTags")
            Prelude.<*> (x Core..:? "DesiredCount")
            Prelude.<*> (x Core..:? "LoadBalancers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Role")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> ( x Core..:? "PlacementConstraints"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ServiceName")
            Prelude.<*> (x Core..:? "DeploymentController")
            Prelude.<*> (x Core..:? "LaunchType")
            Prelude.<*> (x Core..:? "TaskDefinition")
            Prelude.<*> (x Core..:? "ServiceArn")
            Prelude.<*> (x Core..:? "SchedulingStrategy")
            Prelude.<*> ( x Core..:? "CapacityProviderStrategy"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "ServiceRegistries"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "HealthCheckGracePeriodSeconds")
            Prelude.<*> (x Core..:? "NetworkConfiguration")
            Prelude.<*> (x Core..:? "DeploymentConfiguration")
            Prelude.<*> (x Core..:? "EnableExecuteCommand")
      )

instance Prelude.Hashable AwsEcsServiceDetails

instance Prelude.NFData AwsEcsServiceDetails

instance Core.ToJSON AwsEcsServiceDetails where
  toJSON AwsEcsServiceDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PlacementStrategies" Core..=)
              Prelude.<$> placementStrategies,
            ("Cluster" Core..=) Prelude.<$> cluster,
            ("PropagateTags" Core..=) Prelude.<$> propagateTags,
            ("PlatformVersion" Core..=)
              Prelude.<$> platformVersion,
            ("EnableEcsManagedTags" Core..=)
              Prelude.<$> enableEcsManagedTags,
            ("DesiredCount" Core..=) Prelude.<$> desiredCount,
            ("LoadBalancers" Core..=) Prelude.<$> loadBalancers,
            ("Role" Core..=) Prelude.<$> role',
            ("Name" Core..=) Prelude.<$> name,
            ("PlacementConstraints" Core..=)
              Prelude.<$> placementConstraints,
            ("ServiceName" Core..=) Prelude.<$> serviceName,
            ("DeploymentController" Core..=)
              Prelude.<$> deploymentController,
            ("LaunchType" Core..=) Prelude.<$> launchType,
            ("TaskDefinition" Core..=)
              Prelude.<$> taskDefinition,
            ("ServiceArn" Core..=) Prelude.<$> serviceArn,
            ("SchedulingStrategy" Core..=)
              Prelude.<$> schedulingStrategy,
            ("CapacityProviderStrategy" Core..=)
              Prelude.<$> capacityProviderStrategy,
            ("ServiceRegistries" Core..=)
              Prelude.<$> serviceRegistries,
            ("HealthCheckGracePeriodSeconds" Core..=)
              Prelude.<$> healthCheckGracePeriodSeconds,
            ("NetworkConfiguration" Core..=)
              Prelude.<$> networkConfiguration,
            ("DeploymentConfiguration" Core..=)
              Prelude.<$> deploymentConfiguration,
            ("EnableExecuteCommand" Core..=)
              Prelude.<$> enableExecuteCommand
          ]
      )
