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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsServiceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | The capacity provider strategy that the service uses.
    capacityProviderStrategy :: Prelude.Maybe [AwsEcsServiceCapacityProviderStrategyDetails],
    -- | The ARN of the cluster that hosts the service.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | Deployment parameters for the service. Includes the number of tasks that
    -- run and the order in which to start and stop tasks.
    deploymentConfiguration :: Prelude.Maybe AwsEcsServiceDeploymentConfigurationDetails,
    -- | Contains the deployment controller type that the service uses.
    deploymentController :: Prelude.Maybe AwsEcsServiceDeploymentControllerDetails,
    -- | The number of instantiations of the task definition to run on the
    -- service.
    desiredCount :: Prelude.Maybe Prelude.Int,
    -- | Whether to enable Amazon ECS managed tags for the tasks in the service.
    enableEcsManagedTags :: Prelude.Maybe Prelude.Bool,
    -- | Whether the execute command functionality is enabled for the service.
    enableExecuteCommand :: Prelude.Maybe Prelude.Bool,
    -- | After a task starts, the amount of time in seconds that the Amazon ECS
    -- service scheduler ignores unhealthy Elastic Load Balancing target health
    -- checks.
    healthCheckGracePeriodSeconds :: Prelude.Maybe Prelude.Int,
    -- | The launch type that the service uses.
    --
    -- Valid values: @EC2@ | @FARGATE@ | @EXTERNAL@
    launchType :: Prelude.Maybe Prelude.Text,
    -- | Information about the load balancers that the service uses.
    loadBalancers :: Prelude.Maybe [AwsEcsServiceLoadBalancersDetails],
    -- | The name of the service.
    name :: Prelude.Maybe Prelude.Text,
    -- | For tasks that use the @awsvpc@ networking mode, the VPC subnet and
    -- security group configuration.
    networkConfiguration :: Prelude.Maybe AwsEcsServiceNetworkConfigurationDetails,
    -- | The placement constraints for the tasks in the service.
    placementConstraints :: Prelude.Maybe [AwsEcsServicePlacementConstraintsDetails],
    -- | Information about how tasks for the service are placed.
    placementStrategies :: Prelude.Maybe [AwsEcsServicePlacementStrategiesDetails],
    -- | The platform version on which to run the service. Only specified for
    -- tasks that are hosted on Fargate. If a platform version is not
    -- specified, the @LATEST@ platform version is used by default.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to propagate the tags from the task definition to the
    -- task or from the service to the task. If no value is provided, then tags
    -- are not propagated.
    --
    -- Valid values: @TASK_DEFINITION@ | @SERVICE@
    propagateTags :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role that is associated with the service. The role
    -- allows the Amazon ECS container agent to register container instances
    -- with an Elastic Load Balancing load balancer.
    role' :: Prelude.Maybe Prelude.Text,
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
    -- | The ARN of the service.
    serviceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the service.
    --
    -- The name can contain up to 255 characters. It can use letters, numbers,
    -- underscores, and hyphens.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | Information about the service discovery registries to assign to the
    -- service.
    serviceRegistries :: Prelude.Maybe [AwsEcsServiceServiceRegistriesDetails],
    -- | The task definition to use for tasks in the service.
    taskDefinition :: Prelude.Maybe Prelude.Text
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
-- 'capacityProviderStrategy', 'awsEcsServiceDetails_capacityProviderStrategy' - The capacity provider strategy that the service uses.
--
-- 'cluster', 'awsEcsServiceDetails_cluster' - The ARN of the cluster that hosts the service.
--
-- 'deploymentConfiguration', 'awsEcsServiceDetails_deploymentConfiguration' - Deployment parameters for the service. Includes the number of tasks that
-- run and the order in which to start and stop tasks.
--
-- 'deploymentController', 'awsEcsServiceDetails_deploymentController' - Contains the deployment controller type that the service uses.
--
-- 'desiredCount', 'awsEcsServiceDetails_desiredCount' - The number of instantiations of the task definition to run on the
-- service.
--
-- 'enableEcsManagedTags', 'awsEcsServiceDetails_enableEcsManagedTags' - Whether to enable Amazon ECS managed tags for the tasks in the service.
--
-- 'enableExecuteCommand', 'awsEcsServiceDetails_enableExecuteCommand' - Whether the execute command functionality is enabled for the service.
--
-- 'healthCheckGracePeriodSeconds', 'awsEcsServiceDetails_healthCheckGracePeriodSeconds' - After a task starts, the amount of time in seconds that the Amazon ECS
-- service scheduler ignores unhealthy Elastic Load Balancing target health
-- checks.
--
-- 'launchType', 'awsEcsServiceDetails_launchType' - The launch type that the service uses.
--
-- Valid values: @EC2@ | @FARGATE@ | @EXTERNAL@
--
-- 'loadBalancers', 'awsEcsServiceDetails_loadBalancers' - Information about the load balancers that the service uses.
--
-- 'name', 'awsEcsServiceDetails_name' - The name of the service.
--
-- 'networkConfiguration', 'awsEcsServiceDetails_networkConfiguration' - For tasks that use the @awsvpc@ networking mode, the VPC subnet and
-- security group configuration.
--
-- 'placementConstraints', 'awsEcsServiceDetails_placementConstraints' - The placement constraints for the tasks in the service.
--
-- 'placementStrategies', 'awsEcsServiceDetails_placementStrategies' - Information about how tasks for the service are placed.
--
-- 'platformVersion', 'awsEcsServiceDetails_platformVersion' - The platform version on which to run the service. Only specified for
-- tasks that are hosted on Fargate. If a platform version is not
-- specified, the @LATEST@ platform version is used by default.
--
-- 'propagateTags', 'awsEcsServiceDetails_propagateTags' - Indicates whether to propagate the tags from the task definition to the
-- task or from the service to the task. If no value is provided, then tags
-- are not propagated.
--
-- Valid values: @TASK_DEFINITION@ | @SERVICE@
--
-- 'role'', 'awsEcsServiceDetails_role' - The ARN of the IAM role that is associated with the service. The role
-- allows the Amazon ECS container agent to register container instances
-- with an Elastic Load Balancing load balancer.
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
-- 'serviceArn', 'awsEcsServiceDetails_serviceArn' - The ARN of the service.
--
-- 'serviceName', 'awsEcsServiceDetails_serviceName' - The name of the service.
--
-- The name can contain up to 255 characters. It can use letters, numbers,
-- underscores, and hyphens.
--
-- 'serviceRegistries', 'awsEcsServiceDetails_serviceRegistries' - Information about the service discovery registries to assign to the
-- service.
--
-- 'taskDefinition', 'awsEcsServiceDetails_taskDefinition' - The task definition to use for tasks in the service.
newAwsEcsServiceDetails ::
  AwsEcsServiceDetails
newAwsEcsServiceDetails =
  AwsEcsServiceDetails'
    { capacityProviderStrategy =
        Prelude.Nothing,
      cluster = Prelude.Nothing,
      deploymentConfiguration = Prelude.Nothing,
      deploymentController = Prelude.Nothing,
      desiredCount = Prelude.Nothing,
      enableEcsManagedTags = Prelude.Nothing,
      enableExecuteCommand = Prelude.Nothing,
      healthCheckGracePeriodSeconds = Prelude.Nothing,
      launchType = Prelude.Nothing,
      loadBalancers = Prelude.Nothing,
      name = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      placementConstraints = Prelude.Nothing,
      placementStrategies = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      propagateTags = Prelude.Nothing,
      role' = Prelude.Nothing,
      schedulingStrategy = Prelude.Nothing,
      serviceArn = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      serviceRegistries = Prelude.Nothing,
      taskDefinition = Prelude.Nothing
    }

-- | The capacity provider strategy that the service uses.
awsEcsServiceDetails_capacityProviderStrategy :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe [AwsEcsServiceCapacityProviderStrategyDetails])
awsEcsServiceDetails_capacityProviderStrategy = Lens.lens (\AwsEcsServiceDetails' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@AwsEcsServiceDetails' {} a -> s {capacityProviderStrategy = a} :: AwsEcsServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the cluster that hosts the service.
awsEcsServiceDetails_cluster :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_cluster = Lens.lens (\AwsEcsServiceDetails' {cluster} -> cluster) (\s@AwsEcsServiceDetails' {} a -> s {cluster = a} :: AwsEcsServiceDetails)

-- | Deployment parameters for the service. Includes the number of tasks that
-- run and the order in which to start and stop tasks.
awsEcsServiceDetails_deploymentConfiguration :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe AwsEcsServiceDeploymentConfigurationDetails)
awsEcsServiceDetails_deploymentConfiguration = Lens.lens (\AwsEcsServiceDetails' {deploymentConfiguration} -> deploymentConfiguration) (\s@AwsEcsServiceDetails' {} a -> s {deploymentConfiguration = a} :: AwsEcsServiceDetails)

-- | Contains the deployment controller type that the service uses.
awsEcsServiceDetails_deploymentController :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe AwsEcsServiceDeploymentControllerDetails)
awsEcsServiceDetails_deploymentController = Lens.lens (\AwsEcsServiceDetails' {deploymentController} -> deploymentController) (\s@AwsEcsServiceDetails' {} a -> s {deploymentController = a} :: AwsEcsServiceDetails)

-- | The number of instantiations of the task definition to run on the
-- service.
awsEcsServiceDetails_desiredCount :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Int)
awsEcsServiceDetails_desiredCount = Lens.lens (\AwsEcsServiceDetails' {desiredCount} -> desiredCount) (\s@AwsEcsServiceDetails' {} a -> s {desiredCount = a} :: AwsEcsServiceDetails)

-- | Whether to enable Amazon ECS managed tags for the tasks in the service.
awsEcsServiceDetails_enableEcsManagedTags :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Bool)
awsEcsServiceDetails_enableEcsManagedTags = Lens.lens (\AwsEcsServiceDetails' {enableEcsManagedTags} -> enableEcsManagedTags) (\s@AwsEcsServiceDetails' {} a -> s {enableEcsManagedTags = a} :: AwsEcsServiceDetails)

-- | Whether the execute command functionality is enabled for the service.
awsEcsServiceDetails_enableExecuteCommand :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Bool)
awsEcsServiceDetails_enableExecuteCommand = Lens.lens (\AwsEcsServiceDetails' {enableExecuteCommand} -> enableExecuteCommand) (\s@AwsEcsServiceDetails' {} a -> s {enableExecuteCommand = a} :: AwsEcsServiceDetails)

-- | After a task starts, the amount of time in seconds that the Amazon ECS
-- service scheduler ignores unhealthy Elastic Load Balancing target health
-- checks.
awsEcsServiceDetails_healthCheckGracePeriodSeconds :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Int)
awsEcsServiceDetails_healthCheckGracePeriodSeconds = Lens.lens (\AwsEcsServiceDetails' {healthCheckGracePeriodSeconds} -> healthCheckGracePeriodSeconds) (\s@AwsEcsServiceDetails' {} a -> s {healthCheckGracePeriodSeconds = a} :: AwsEcsServiceDetails)

-- | The launch type that the service uses.
--
-- Valid values: @EC2@ | @FARGATE@ | @EXTERNAL@
awsEcsServiceDetails_launchType :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_launchType = Lens.lens (\AwsEcsServiceDetails' {launchType} -> launchType) (\s@AwsEcsServiceDetails' {} a -> s {launchType = a} :: AwsEcsServiceDetails)

-- | Information about the load balancers that the service uses.
awsEcsServiceDetails_loadBalancers :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe [AwsEcsServiceLoadBalancersDetails])
awsEcsServiceDetails_loadBalancers = Lens.lens (\AwsEcsServiceDetails' {loadBalancers} -> loadBalancers) (\s@AwsEcsServiceDetails' {} a -> s {loadBalancers = a} :: AwsEcsServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the service.
awsEcsServiceDetails_name :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_name = Lens.lens (\AwsEcsServiceDetails' {name} -> name) (\s@AwsEcsServiceDetails' {} a -> s {name = a} :: AwsEcsServiceDetails)

-- | For tasks that use the @awsvpc@ networking mode, the VPC subnet and
-- security group configuration.
awsEcsServiceDetails_networkConfiguration :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe AwsEcsServiceNetworkConfigurationDetails)
awsEcsServiceDetails_networkConfiguration = Lens.lens (\AwsEcsServiceDetails' {networkConfiguration} -> networkConfiguration) (\s@AwsEcsServiceDetails' {} a -> s {networkConfiguration = a} :: AwsEcsServiceDetails)

-- | The placement constraints for the tasks in the service.
awsEcsServiceDetails_placementConstraints :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe [AwsEcsServicePlacementConstraintsDetails])
awsEcsServiceDetails_placementConstraints = Lens.lens (\AwsEcsServiceDetails' {placementConstraints} -> placementConstraints) (\s@AwsEcsServiceDetails' {} a -> s {placementConstraints = a} :: AwsEcsServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | Information about how tasks for the service are placed.
awsEcsServiceDetails_placementStrategies :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe [AwsEcsServicePlacementStrategiesDetails])
awsEcsServiceDetails_placementStrategies = Lens.lens (\AwsEcsServiceDetails' {placementStrategies} -> placementStrategies) (\s@AwsEcsServiceDetails' {} a -> s {placementStrategies = a} :: AwsEcsServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The platform version on which to run the service. Only specified for
-- tasks that are hosted on Fargate. If a platform version is not
-- specified, the @LATEST@ platform version is used by default.
awsEcsServiceDetails_platformVersion :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_platformVersion = Lens.lens (\AwsEcsServiceDetails' {platformVersion} -> platformVersion) (\s@AwsEcsServiceDetails' {} a -> s {platformVersion = a} :: AwsEcsServiceDetails)

-- | Indicates whether to propagate the tags from the task definition to the
-- task or from the service to the task. If no value is provided, then tags
-- are not propagated.
--
-- Valid values: @TASK_DEFINITION@ | @SERVICE@
awsEcsServiceDetails_propagateTags :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_propagateTags = Lens.lens (\AwsEcsServiceDetails' {propagateTags} -> propagateTags) (\s@AwsEcsServiceDetails' {} a -> s {propagateTags = a} :: AwsEcsServiceDetails)

-- | The ARN of the IAM role that is associated with the service. The role
-- allows the Amazon ECS container agent to register container instances
-- with an Elastic Load Balancing load balancer.
awsEcsServiceDetails_role :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_role = Lens.lens (\AwsEcsServiceDetails' {role'} -> role') (\s@AwsEcsServiceDetails' {} a -> s {role' = a} :: AwsEcsServiceDetails)

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

-- | The ARN of the service.
awsEcsServiceDetails_serviceArn :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_serviceArn = Lens.lens (\AwsEcsServiceDetails' {serviceArn} -> serviceArn) (\s@AwsEcsServiceDetails' {} a -> s {serviceArn = a} :: AwsEcsServiceDetails)

-- | The name of the service.
--
-- The name can contain up to 255 characters. It can use letters, numbers,
-- underscores, and hyphens.
awsEcsServiceDetails_serviceName :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_serviceName = Lens.lens (\AwsEcsServiceDetails' {serviceName} -> serviceName) (\s@AwsEcsServiceDetails' {} a -> s {serviceName = a} :: AwsEcsServiceDetails)

-- | Information about the service discovery registries to assign to the
-- service.
awsEcsServiceDetails_serviceRegistries :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe [AwsEcsServiceServiceRegistriesDetails])
awsEcsServiceDetails_serviceRegistries = Lens.lens (\AwsEcsServiceDetails' {serviceRegistries} -> serviceRegistries) (\s@AwsEcsServiceDetails' {} a -> s {serviceRegistries = a} :: AwsEcsServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The task definition to use for tasks in the service.
awsEcsServiceDetails_taskDefinition :: Lens.Lens' AwsEcsServiceDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceDetails_taskDefinition = Lens.lens (\AwsEcsServiceDetails' {taskDefinition} -> taskDefinition) (\s@AwsEcsServiceDetails' {} a -> s {taskDefinition = a} :: AwsEcsServiceDetails)

instance Data.FromJSON AwsEcsServiceDetails where
  parseJSON =
    Data.withObject
      "AwsEcsServiceDetails"
      ( \x ->
          AwsEcsServiceDetails'
            Prelude.<$> ( x
                            Data..:? "CapacityProviderStrategy"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Cluster")
            Prelude.<*> (x Data..:? "DeploymentConfiguration")
            Prelude.<*> (x Data..:? "DeploymentController")
            Prelude.<*> (x Data..:? "DesiredCount")
            Prelude.<*> (x Data..:? "EnableEcsManagedTags")
            Prelude.<*> (x Data..:? "EnableExecuteCommand")
            Prelude.<*> (x Data..:? "HealthCheckGracePeriodSeconds")
            Prelude.<*> (x Data..:? "LaunchType")
            Prelude.<*> (x Data..:? "LoadBalancers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "NetworkConfiguration")
            Prelude.<*> ( x
                            Data..:? "PlacementConstraints"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "PlacementStrategies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PlatformVersion")
            Prelude.<*> (x Data..:? "PropagateTags")
            Prelude.<*> (x Data..:? "Role")
            Prelude.<*> (x Data..:? "SchedulingStrategy")
            Prelude.<*> (x Data..:? "ServiceArn")
            Prelude.<*> (x Data..:? "ServiceName")
            Prelude.<*> ( x
                            Data..:? "ServiceRegistries"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "TaskDefinition")
      )

instance Prelude.Hashable AwsEcsServiceDetails where
  hashWithSalt _salt AwsEcsServiceDetails' {..} =
    _salt
      `Prelude.hashWithSalt` capacityProviderStrategy
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` deploymentConfiguration
      `Prelude.hashWithSalt` deploymentController
      `Prelude.hashWithSalt` desiredCount
      `Prelude.hashWithSalt` enableEcsManagedTags
      `Prelude.hashWithSalt` enableExecuteCommand
      `Prelude.hashWithSalt` healthCheckGracePeriodSeconds
      `Prelude.hashWithSalt` launchType
      `Prelude.hashWithSalt` loadBalancers
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` placementConstraints
      `Prelude.hashWithSalt` placementStrategies
      `Prelude.hashWithSalt` platformVersion
      `Prelude.hashWithSalt` propagateTags
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` schedulingStrategy
      `Prelude.hashWithSalt` serviceArn
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` serviceRegistries
      `Prelude.hashWithSalt` taskDefinition

instance Prelude.NFData AwsEcsServiceDetails where
  rnf AwsEcsServiceDetails' {..} =
    Prelude.rnf capacityProviderStrategy
      `Prelude.seq` Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf deploymentConfiguration
      `Prelude.seq` Prelude.rnf deploymentController
      `Prelude.seq` Prelude.rnf desiredCount
      `Prelude.seq` Prelude.rnf enableEcsManagedTags
      `Prelude.seq` Prelude.rnf enableExecuteCommand
      `Prelude.seq` Prelude.rnf healthCheckGracePeriodSeconds
      `Prelude.seq` Prelude.rnf launchType
      `Prelude.seq` Prelude.rnf loadBalancers
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf networkConfiguration
      `Prelude.seq` Prelude.rnf placementConstraints
      `Prelude.seq` Prelude.rnf placementStrategies
      `Prelude.seq` Prelude.rnf platformVersion
      `Prelude.seq` Prelude.rnf propagateTags
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf schedulingStrategy
      `Prelude.seq` Prelude.rnf serviceArn
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf
        serviceRegistries
      `Prelude.seq` Prelude.rnf taskDefinition

instance Data.ToJSON AwsEcsServiceDetails where
  toJSON AwsEcsServiceDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CapacityProviderStrategy" Data..=)
              Prelude.<$> capacityProviderStrategy,
            ("Cluster" Data..=) Prelude.<$> cluster,
            ("DeploymentConfiguration" Data..=)
              Prelude.<$> deploymentConfiguration,
            ("DeploymentController" Data..=)
              Prelude.<$> deploymentController,
            ("DesiredCount" Data..=) Prelude.<$> desiredCount,
            ("EnableEcsManagedTags" Data..=)
              Prelude.<$> enableEcsManagedTags,
            ("EnableExecuteCommand" Data..=)
              Prelude.<$> enableExecuteCommand,
            ("HealthCheckGracePeriodSeconds" Data..=)
              Prelude.<$> healthCheckGracePeriodSeconds,
            ("LaunchType" Data..=) Prelude.<$> launchType,
            ("LoadBalancers" Data..=) Prelude.<$> loadBalancers,
            ("Name" Data..=) Prelude.<$> name,
            ("NetworkConfiguration" Data..=)
              Prelude.<$> networkConfiguration,
            ("PlacementConstraints" Data..=)
              Prelude.<$> placementConstraints,
            ("PlacementStrategies" Data..=)
              Prelude.<$> placementStrategies,
            ("PlatformVersion" Data..=)
              Prelude.<$> platformVersion,
            ("PropagateTags" Data..=) Prelude.<$> propagateTags,
            ("Role" Data..=) Prelude.<$> role',
            ("SchedulingStrategy" Data..=)
              Prelude.<$> schedulingStrategy,
            ("ServiceArn" Data..=) Prelude.<$> serviceArn,
            ("ServiceName" Data..=) Prelude.<$> serviceName,
            ("ServiceRegistries" Data..=)
              Prelude.<$> serviceRegistries,
            ("TaskDefinition" Data..=)
              Prelude.<$> taskDefinition
          ]
      )
