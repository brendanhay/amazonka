{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECS.CreateService
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs and maintains your desired number of tasks from a specified task
-- definition. If the number of tasks running in a service drops below the
-- @desiredCount@, Amazon ECS runs another copy of the task in the
-- specified cluster. To update an existing service, see the UpdateService
-- action.
--
-- In addition to maintaining the desired count of tasks in your service,
-- you can optionally run your service behind one or more load balancers.
-- The load balancers distribute traffic across the tasks that are
-- associated with the service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-load-balancing.html Service load balancing>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- Tasks for services that don\'t use a load balancer are considered
-- healthy if they\'re in the @RUNNING@ state. Tasks for services that use
-- a load balancer are considered healthy if they\'re in the @RUNNING@
-- state and are reported as healthy by the load balancer.
--
-- There are two service scheduler strategies available:
--
-- -   @REPLICA@ - The replica scheduling strategy places and maintains
--     your desired number of tasks across your cluster. By default, the
--     service scheduler spreads tasks across Availability Zones. You can
--     use task placement strategies and constraints to customize task
--     placement decisions. For more information, see
--     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Service scheduler concepts>
--     in the /Amazon Elastic Container Service Developer Guide/.
--
-- -   @DAEMON@ - The daemon scheduling strategy deploys exactly one task
--     on each active container instance that meets all of the task
--     placement constraints that you specify in your cluster. The service
--     scheduler also evaluates the task placement constraints for running
--     tasks. It also stops tasks that don\'t meet the placement
--     constraints. When using this strategy, you don\'t need to specify a
--     desired number of tasks, a task placement strategy, or use Service
--     Auto Scaling policies. For more information, see
--     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Service scheduler concepts>
--     in the /Amazon Elastic Container Service Developer Guide/.
--
-- You can optionally specify a deployment configuration for your service.
-- The deployment is initiated by changing properties. For example, the
-- deployment might be initiated by the task definition or by your desired
-- count of a service. This is done with an UpdateService operation. The
-- default value for a replica service for @minimumHealthyPercent@ is 100%.
-- The default value for a daemon service for @minimumHealthyPercent@ is
-- 0%.
--
-- If a service uses the @ECS@ deployment controller, the minimum healthy
-- percent represents a lower limit on the number of tasks in a service
-- that must remain in the @RUNNING@ state during a deployment.
-- Specifically, it represents it as a percentage of your desired number of
-- tasks (rounded up to the nearest integer). This happens when any of your
-- container instances are in the @DRAINING@ state if the service contains
-- tasks using the EC2 launch type. Using this parameter, you can deploy
-- without using additional cluster capacity. For example, if you set your
-- service to have desired number of four tasks and a minimum healthy
-- percent of 50%, the scheduler might stop two existing tasks to free up
-- cluster capacity before starting two new tasks. If they\'re in the
-- @RUNNING@ state, tasks for services that don\'t use a load balancer are
-- considered healthy . If they\'re in the @RUNNING@ state and reported as
-- healthy by the load balancer, tasks for services that /do/ use a load
-- balancer are considered healthy . The default value for minimum healthy
-- percent is 100%.
--
-- If a service uses the @ECS@ deployment controller, the __maximum
-- percent__ parameter represents an upper limit on the number of tasks in
-- a service that are allowed in the @RUNNING@ or @PENDING@ state during a
-- deployment. Specifically, it represents it as a percentage of the
-- desired number of tasks (rounded down to the nearest integer). This
-- happens when any of your container instances are in the @DRAINING@ state
-- if the service contains tasks using the EC2 launch type. Using this
-- parameter, you can define the deployment batch size. For example, if
-- your service has a desired number of four tasks and a maximum percent
-- value of 200%, the scheduler may start four new tasks before stopping
-- the four older tasks (provided that the cluster resources required to do
-- this are available). The default value for maximum percent is 200%.
--
-- If a service uses either the @CODE_DEPLOY@ or @EXTERNAL@ deployment
-- controller types and tasks that use the EC2 launch type, the __minimum
-- healthy percent__ and __maximum percent__ values are used only to define
-- the lower and upper limit on the number of the tasks in the service that
-- remain in the @RUNNING@ state. This is while the container instances are
-- in the @DRAINING@ state. If the tasks in the service use the Fargate
-- launch type, the minimum healthy percent and maximum percent values
-- aren\'t used. This is the case even if they\'re currently visible when
-- describing your service.
--
-- When creating a service that uses the @EXTERNAL@ deployment controller,
-- you can specify only parameters that aren\'t controlled at the task set
-- level. The only required parameter is the service name. You control your
-- services using the CreateTaskSet operation. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS deployment types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- When the service scheduler launches new tasks, it determines task
-- placement. For information about task placement and task placement
-- strategies, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement.html Amazon ECS task placement>
-- in the /Amazon Elastic Container Service Developer Guide/.
module Amazonka.ECS.CreateService
  ( -- * Creating a Request
    CreateService (..),
    newCreateService,

    -- * Request Lenses
    createService_capacityProviderStrategy,
    createService_clientToken,
    createService_cluster,
    createService_deploymentConfiguration,
    createService_deploymentController,
    createService_desiredCount,
    createService_enableECSManagedTags,
    createService_enableExecuteCommand,
    createService_healthCheckGracePeriodSeconds,
    createService_launchType,
    createService_loadBalancers,
    createService_networkConfiguration,
    createService_placementConstraints,
    createService_placementStrategy,
    createService_platformVersion,
    createService_propagateTags,
    createService_role,
    createService_schedulingStrategy,
    createService_serviceConnectConfiguration,
    createService_serviceRegistries,
    createService_tags,
    createService_taskDefinition,
    createService_serviceName,

    -- * Destructuring the Response
    CreateServiceResponse (..),
    newCreateServiceResponse,

    -- * Response Lenses
    createServiceResponse_service,
    createServiceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateService' smart constructor.
data CreateService = CreateService'
  { -- | The capacity provider strategy to use for the service.
    --
    -- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter
    -- must be omitted. If no @capacityProviderStrategy@ or @launchType@ is
    -- specified, the @defaultCapacityProviderStrategy@ for the cluster is
    -- used.
    --
    -- A capacity provider strategy may contain a maximum of 6 capacity
    -- providers.
    capacityProviderStrategy :: Prelude.Maybe [CapacityProviderStrategyItem],
    -- | An identifier that you provide to ensure the idempotency of the request.
    -- It must be unique and is case sensitive. Up to 32 ASCII characters are
    -- allowed.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- you run your service on. If you do not specify a cluster, the default
    -- cluster is assumed.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | Optional deployment parameters that control how many tasks run during
    -- the deployment and the ordering of stopping and starting tasks.
    deploymentConfiguration :: Prelude.Maybe DeploymentConfiguration,
    -- | The deployment controller to use for the service. If no deployment
    -- controller is specified, the default value of @ECS@ is used.
    deploymentController :: Prelude.Maybe DeploymentController,
    -- | The number of instantiations of the specified task definition to place
    -- and keep running on your cluster.
    --
    -- This is required if @schedulingStrategy@ is @REPLICA@ or isn\'t
    -- specified. If @schedulingStrategy@ is @DAEMON@ then this isn\'t
    -- required.
    desiredCount :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether to turn on Amazon ECS managed tags for the tasks
    -- within the service. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging your Amazon ECS resources>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    enableECSManagedTags :: Prelude.Maybe Prelude.Bool,
    -- | Determines whether the execute command functionality is enabled for the
    -- service. If @true@, this enables execute command functionality on all
    -- containers in the service tasks.
    enableExecuteCommand :: Prelude.Maybe Prelude.Bool,
    -- | The period of time, in seconds, that the Amazon ECS service scheduler
    -- ignores unhealthy Elastic Load Balancing target health checks after a
    -- task has first started. This is only used when your service is
    -- configured to use a load balancer. If your service has a load balancer
    -- defined and you don\'t specify a health check grace period value, the
    -- default value of @0@ is used.
    --
    -- If you do not use an Elastic Load Balancing, we recommend that you use
    -- the @startPeriod@ in the task definition health check parameters. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_HealthCheck.html Health check>.
    --
    -- If your service\'s tasks take a while to start and respond to Elastic
    -- Load Balancing health checks, you can specify a health check grace
    -- period of up to 2,147,483,647 seconds (about 69 years). During that
    -- time, the Amazon ECS service scheduler ignores health check status. This
    -- grace period can prevent the service scheduler from marking tasks as
    -- unhealthy and stopping them before they have time to come up.
    healthCheckGracePeriodSeconds :: Prelude.Maybe Prelude.Int,
    -- | The infrastructure that you run your service on. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- The @FARGATE@ launch type runs your tasks on Fargate On-Demand
    -- infrastructure.
    --
    -- Fargate Spot infrastructure is available for use but a capacity provider
    -- strategy must be used. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/userguide/fargate-capacity-providers.html Fargate capacity providers>
    -- in the /Amazon ECS User Guide for Fargate/.
    --
    -- The @EC2@ launch type runs your tasks on Amazon EC2 instances registered
    -- to your cluster.
    --
    -- The @EXTERNAL@ launch type runs your tasks on your on-premises server or
    -- virtual machine (VM) capacity registered to your cluster.
    --
    -- A service can use either a launch type or a capacity provider strategy.
    -- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter
    -- must be omitted.
    launchType :: Prelude.Maybe LaunchType,
    -- | A load balancer object representing the load balancers to use with your
    -- service. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-load-balancing.html Service load balancing>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- If the service uses the rolling update (@ECS@) deployment controller and
    -- using either an Application Load Balancer or Network Load Balancer, you
    -- must specify one or more target group ARNs to attach to the service. The
    -- service-linked role is required for services that use multiple target
    -- groups. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using service-linked roles for Amazon ECS>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- If the service uses the @CODE_DEPLOY@ deployment controller, the service
    -- is required to use either an Application Load Balancer or Network Load
    -- Balancer. When creating an CodeDeploy deployment group, you specify two
    -- target groups (referred to as a @targetGroupPair@). During a deployment,
    -- CodeDeploy determines which task set in your service has the status
    -- @PRIMARY@, and it associates one target group with it. Then, it also
    -- associates the other target group with the replacement task set. The
    -- load balancer can also have up to two listeners: a required listener for
    -- production traffic and an optional listener that you can use to perform
    -- validation tests with Lambda functions before routing production traffic
    -- to it.
    --
    -- If you use the @CODE_DEPLOY@ deployment controller, these values can be
    -- changed when updating the service.
    --
    -- For Application Load Balancers and Network Load Balancers, this object
    -- must contain the load balancer target group ARN, the container name, and
    -- the container port to access from the load balancer. The container name
    -- must be as it appears in a container definition. The load balancer name
    -- parameter must be omitted. When a task from this service is placed on a
    -- container instance, the container instance and port combination is
    -- registered as a target in the target group that\'s specified here.
    --
    -- For Classic Load Balancers, this object must contain the load balancer
    -- name, the container name , and the container port to access from the
    -- load balancer. The container name must be as it appears in a container
    -- definition. The target group ARN parameter must be omitted. When a task
    -- from this service is placed on a container instance, the container
    -- instance is registered with the load balancer that\'s specified here.
    --
    -- Services with tasks that use the @awsvpc@ network mode (for example,
    -- those with the Fargate launch type) only support Application Load
    -- Balancers and Network Load Balancers. Classic Load Balancers aren\'t
    -- supported. Also, when you create any target groups for these services,
    -- you must choose @ip@ as the target type, not @instance@. This is because
    -- tasks that use the @awsvpc@ network mode are associated with an elastic
    -- network interface, not an Amazon EC2 instance.
    loadBalancers :: Prelude.Maybe [LoadBalancer],
    -- | The network configuration for the service. This parameter is required
    -- for task definitions that use the @awsvpc@ network mode to receive their
    -- own elastic network interface, and it isn\'t supported for other network
    -- modes. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task networking>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | An array of placement constraint objects to use for tasks in your
    -- service. You can specify a maximum of 10 constraints for each task. This
    -- limit includes constraints in the task definition and those specified at
    -- runtime.
    placementConstraints :: Prelude.Maybe [PlacementConstraint],
    -- | The placement strategy objects to use for tasks in your service. You can
    -- specify a maximum of 5 strategy rules for each service.
    placementStrategy :: Prelude.Maybe [PlacementStrategy],
    -- | The platform version that your tasks in the service are running on. A
    -- platform version is specified only for tasks using the Fargate launch
    -- type. If one isn\'t specified, the @LATEST@ platform version is used.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate platform versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to propagate the tags from the task definition to the
    -- task. If no value is specified, the tags aren\'t propagated. Tags can
    -- only be propagated to the task during task creation. To add tags to a
    -- task after task creation, use the TagResource API action.
    propagateTags :: Prelude.Maybe PropagateTags,
    -- | The name or full Amazon Resource Name (ARN) of the IAM role that allows
    -- Amazon ECS to make calls to your load balancer on your behalf. This
    -- parameter is only permitted if you are using a load balancer with your
    -- service and your task definition doesn\'t use the @awsvpc@ network mode.
    -- If you specify the @role@ parameter, you must also specify a load
    -- balancer object with the @loadBalancers@ parameter.
    --
    -- If your account has already created the Amazon ECS service-linked role,
    -- that role is used for your service unless you specify a role here. The
    -- service-linked role is required if your task definition uses the
    -- @awsvpc@ network mode or if the service is configured to use service
    -- discovery, an external deployment controller, multiple target groups, or
    -- Elastic Inference accelerators in which case you don\'t specify a role
    -- here. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using service-linked roles for Amazon ECS>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- If your specified role has a path other than @\/@, then you must either
    -- specify the full role ARN (this is recommended) or prefix the role name
    -- with the path. For example, if a role with the name @bar@ has a path of
    -- @\/foo\/@ then you would specify @\/foo\/bar@ as the role name. For more
    -- information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-friendly-names Friendly names and paths>
    -- in the /IAM User Guide/.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The scheduling strategy to use for the service. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services>.
    --
    -- There are two service scheduler strategies available:
    --
    -- -   @REPLICA@-The replica scheduling strategy places and maintains the
    --     desired number of tasks across your cluster. By default, the service
    --     scheduler spreads tasks across Availability Zones. You can use task
    --     placement strategies and constraints to customize task placement
    --     decisions. This scheduler strategy is required if the service uses
    --     the @CODE_DEPLOY@ or @EXTERNAL@ deployment controller types.
    --
    -- -   @DAEMON@-The daemon scheduling strategy deploys exactly one task on
    --     each active container instance that meets all of the task placement
    --     constraints that you specify in your cluster. The service scheduler
    --     also evaluates the task placement constraints for running tasks and
    --     will stop tasks that don\'t meet the placement constraints. When
    --     you\'re using this strategy, you don\'t need to specify a desired
    --     number of tasks, a task placement strategy, or use Service Auto
    --     Scaling policies.
    --
    --     Tasks using the Fargate launch type or the @CODE_DEPLOY@ or
    --     @EXTERNAL@ deployment controller types don\'t support the @DAEMON@
    --     scheduling strategy.
    schedulingStrategy :: Prelude.Maybe SchedulingStrategy,
    -- | The configuration for this service to discover and connect to services,
    -- and be discovered by, and connected from, other services within a
    -- namespace.
    --
    -- Tasks that run in a namespace can use short names to connect to services
    -- in the namespace. Tasks can connect to services across all of the
    -- clusters in the namespace. Tasks connect through a managed proxy
    -- container that collects logs and metrics for increased visibility. Only
    -- the tasks that Amazon ECS services create are supported with Service
    -- Connect. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    serviceConnectConfiguration :: Prelude.Maybe ServiceConnectConfiguration,
    -- | The details of the service discovery registry to associate with this
    -- service. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service discovery>.
    --
    -- Each service may be associated with one service registry. Multiple
    -- service registries for each service isn\'t supported.
    serviceRegistries :: Prelude.Maybe [ServiceRegistry],
    -- | The metadata that you apply to the service to help you categorize and
    -- organize them. Each tag consists of a key and an optional value, both of
    -- which you define. When a service is deleted, the tags are deleted as
    -- well.
    --
    -- The following basic restrictions apply to tags:
    --
    -- -   Maximum number of tags per resource - 50
    --
    -- -   For each resource, each tag key must be unique, and each tag key can
    --     have only one value.
    --
    -- -   Maximum key length - 128 Unicode characters in UTF-8
    --
    -- -   Maximum value length - 256 Unicode characters in UTF-8
    --
    -- -   If your tagging schema is used across multiple services and
    --     resources, remember that other services may have restrictions on
    --     allowed characters. Generally allowed characters are: letters,
    --     numbers, and spaces representable in UTF-8, and the following
    --     characters: + - = . _ : \/ \@.
    --
    -- -   Tag keys and values are case-sensitive.
    --
    -- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
    --     such as a prefix for either keys or values as it is reserved for
    --     Amazon Web Services use. You cannot edit or delete tag keys or
    --     values with this prefix. Tags with this prefix do not count against
    --     your tags per resource limit.
    tags :: Prelude.Maybe [Tag],
    -- | The @family@ and @revision@ (@family:revision@) or full ARN of the task
    -- definition to run in your service. If a @revision@ isn\'t specified, the
    -- latest @ACTIVE@ revision is used.
    --
    -- A task definition must be specified if the service uses either the @ECS@
    -- or @CODE_DEPLOY@ deployment controllers.
    taskDefinition :: Prelude.Maybe Prelude.Text,
    -- | The name of your service. Up to 255 letters (uppercase and lowercase),
    -- numbers, underscores, and hyphens are allowed. Service names must be
    -- unique within a cluster, but you can have similarly named services in
    -- multiple clusters within a Region or across multiple Regions.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityProviderStrategy', 'createService_capacityProviderStrategy' - The capacity provider strategy to use for the service.
--
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter
-- must be omitted. If no @capacityProviderStrategy@ or @launchType@ is
-- specified, the @defaultCapacityProviderStrategy@ for the cluster is
-- used.
--
-- A capacity provider strategy may contain a maximum of 6 capacity
-- providers.
--
-- 'clientToken', 'createService_clientToken' - An identifier that you provide to ensure the idempotency of the request.
-- It must be unique and is case sensitive. Up to 32 ASCII characters are
-- allowed.
--
-- 'cluster', 'createService_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- you run your service on. If you do not specify a cluster, the default
-- cluster is assumed.
--
-- 'deploymentConfiguration', 'createService_deploymentConfiguration' - Optional deployment parameters that control how many tasks run during
-- the deployment and the ordering of stopping and starting tasks.
--
-- 'deploymentController', 'createService_deploymentController' - The deployment controller to use for the service. If no deployment
-- controller is specified, the default value of @ECS@ is used.
--
-- 'desiredCount', 'createService_desiredCount' - The number of instantiations of the specified task definition to place
-- and keep running on your cluster.
--
-- This is required if @schedulingStrategy@ is @REPLICA@ or isn\'t
-- specified. If @schedulingStrategy@ is @DAEMON@ then this isn\'t
-- required.
--
-- 'enableECSManagedTags', 'createService_enableECSManagedTags' - Specifies whether to turn on Amazon ECS managed tags for the tasks
-- within the service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging your Amazon ECS resources>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'enableExecuteCommand', 'createService_enableExecuteCommand' - Determines whether the execute command functionality is enabled for the
-- service. If @true@, this enables execute command functionality on all
-- containers in the service tasks.
--
-- 'healthCheckGracePeriodSeconds', 'createService_healthCheckGracePeriodSeconds' - The period of time, in seconds, that the Amazon ECS service scheduler
-- ignores unhealthy Elastic Load Balancing target health checks after a
-- task has first started. This is only used when your service is
-- configured to use a load balancer. If your service has a load balancer
-- defined and you don\'t specify a health check grace period value, the
-- default value of @0@ is used.
--
-- If you do not use an Elastic Load Balancing, we recommend that you use
-- the @startPeriod@ in the task definition health check parameters. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_HealthCheck.html Health check>.
--
-- If your service\'s tasks take a while to start and respond to Elastic
-- Load Balancing health checks, you can specify a health check grace
-- period of up to 2,147,483,647 seconds (about 69 years). During that
-- time, the Amazon ECS service scheduler ignores health check status. This
-- grace period can prevent the service scheduler from marking tasks as
-- unhealthy and stopping them before they have time to come up.
--
-- 'launchType', 'createService_launchType' - The infrastructure that you run your service on. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- The @FARGATE@ launch type runs your tasks on Fargate On-Demand
-- infrastructure.
--
-- Fargate Spot infrastructure is available for use but a capacity provider
-- strategy must be used. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/userguide/fargate-capacity-providers.html Fargate capacity providers>
-- in the /Amazon ECS User Guide for Fargate/.
--
-- The @EC2@ launch type runs your tasks on Amazon EC2 instances registered
-- to your cluster.
--
-- The @EXTERNAL@ launch type runs your tasks on your on-premises server or
-- virtual machine (VM) capacity registered to your cluster.
--
-- A service can use either a launch type or a capacity provider strategy.
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter
-- must be omitted.
--
-- 'loadBalancers', 'createService_loadBalancers' - A load balancer object representing the load balancers to use with your
-- service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-load-balancing.html Service load balancing>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If the service uses the rolling update (@ECS@) deployment controller and
-- using either an Application Load Balancer or Network Load Balancer, you
-- must specify one or more target group ARNs to attach to the service. The
-- service-linked role is required for services that use multiple target
-- groups. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using service-linked roles for Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If the service uses the @CODE_DEPLOY@ deployment controller, the service
-- is required to use either an Application Load Balancer or Network Load
-- Balancer. When creating an CodeDeploy deployment group, you specify two
-- target groups (referred to as a @targetGroupPair@). During a deployment,
-- CodeDeploy determines which task set in your service has the status
-- @PRIMARY@, and it associates one target group with it. Then, it also
-- associates the other target group with the replacement task set. The
-- load balancer can also have up to two listeners: a required listener for
-- production traffic and an optional listener that you can use to perform
-- validation tests with Lambda functions before routing production traffic
-- to it.
--
-- If you use the @CODE_DEPLOY@ deployment controller, these values can be
-- changed when updating the service.
--
-- For Application Load Balancers and Network Load Balancers, this object
-- must contain the load balancer target group ARN, the container name, and
-- the container port to access from the load balancer. The container name
-- must be as it appears in a container definition. The load balancer name
-- parameter must be omitted. When a task from this service is placed on a
-- container instance, the container instance and port combination is
-- registered as a target in the target group that\'s specified here.
--
-- For Classic Load Balancers, this object must contain the load balancer
-- name, the container name , and the container port to access from the
-- load balancer. The container name must be as it appears in a container
-- definition. The target group ARN parameter must be omitted. When a task
-- from this service is placed on a container instance, the container
-- instance is registered with the load balancer that\'s specified here.
--
-- Services with tasks that use the @awsvpc@ network mode (for example,
-- those with the Fargate launch type) only support Application Load
-- Balancers and Network Load Balancers. Classic Load Balancers aren\'t
-- supported. Also, when you create any target groups for these services,
-- you must choose @ip@ as the target type, not @instance@. This is because
-- tasks that use the @awsvpc@ network mode are associated with an elastic
-- network interface, not an Amazon EC2 instance.
--
-- 'networkConfiguration', 'createService_networkConfiguration' - The network configuration for the service. This parameter is required
-- for task definitions that use the @awsvpc@ network mode to receive their
-- own elastic network interface, and it isn\'t supported for other network
-- modes. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task networking>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'placementConstraints', 'createService_placementConstraints' - An array of placement constraint objects to use for tasks in your
-- service. You can specify a maximum of 10 constraints for each task. This
-- limit includes constraints in the task definition and those specified at
-- runtime.
--
-- 'placementStrategy', 'createService_placementStrategy' - The placement strategy objects to use for tasks in your service. You can
-- specify a maximum of 5 strategy rules for each service.
--
-- 'platformVersion', 'createService_platformVersion' - The platform version that your tasks in the service are running on. A
-- platform version is specified only for tasks using the Fargate launch
-- type. If one isn\'t specified, the @LATEST@ platform version is used.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate platform versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'propagateTags', 'createService_propagateTags' - Specifies whether to propagate the tags from the task definition to the
-- task. If no value is specified, the tags aren\'t propagated. Tags can
-- only be propagated to the task during task creation. To add tags to a
-- task after task creation, use the TagResource API action.
--
-- 'role'', 'createService_role' - The name or full Amazon Resource Name (ARN) of the IAM role that allows
-- Amazon ECS to make calls to your load balancer on your behalf. This
-- parameter is only permitted if you are using a load balancer with your
-- service and your task definition doesn\'t use the @awsvpc@ network mode.
-- If you specify the @role@ parameter, you must also specify a load
-- balancer object with the @loadBalancers@ parameter.
--
-- If your account has already created the Amazon ECS service-linked role,
-- that role is used for your service unless you specify a role here. The
-- service-linked role is required if your task definition uses the
-- @awsvpc@ network mode or if the service is configured to use service
-- discovery, an external deployment controller, multiple target groups, or
-- Elastic Inference accelerators in which case you don\'t specify a role
-- here. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using service-linked roles for Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If your specified role has a path other than @\/@, then you must either
-- specify the full role ARN (this is recommended) or prefix the role name
-- with the path. For example, if a role with the name @bar@ has a path of
-- @\/foo\/@ then you would specify @\/foo\/bar@ as the role name. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-friendly-names Friendly names and paths>
-- in the /IAM User Guide/.
--
-- 'schedulingStrategy', 'createService_schedulingStrategy' - The scheduling strategy to use for the service. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services>.
--
-- There are two service scheduler strategies available:
--
-- -   @REPLICA@-The replica scheduling strategy places and maintains the
--     desired number of tasks across your cluster. By default, the service
--     scheduler spreads tasks across Availability Zones. You can use task
--     placement strategies and constraints to customize task placement
--     decisions. This scheduler strategy is required if the service uses
--     the @CODE_DEPLOY@ or @EXTERNAL@ deployment controller types.
--
-- -   @DAEMON@-The daemon scheduling strategy deploys exactly one task on
--     each active container instance that meets all of the task placement
--     constraints that you specify in your cluster. The service scheduler
--     also evaluates the task placement constraints for running tasks and
--     will stop tasks that don\'t meet the placement constraints. When
--     you\'re using this strategy, you don\'t need to specify a desired
--     number of tasks, a task placement strategy, or use Service Auto
--     Scaling policies.
--
--     Tasks using the Fargate launch type or the @CODE_DEPLOY@ or
--     @EXTERNAL@ deployment controller types don\'t support the @DAEMON@
--     scheduling strategy.
--
-- 'serviceConnectConfiguration', 'createService_serviceConnectConfiguration' - The configuration for this service to discover and connect to services,
-- and be discovered by, and connected from, other services within a
-- namespace.
--
-- Tasks that run in a namespace can use short names to connect to services
-- in the namespace. Tasks can connect to services across all of the
-- clusters in the namespace. Tasks connect through a managed proxy
-- container that collects logs and metrics for increased visibility. Only
-- the tasks that Amazon ECS services create are supported with Service
-- Connect. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'serviceRegistries', 'createService_serviceRegistries' - The details of the service discovery registry to associate with this
-- service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service discovery>.
--
-- Each service may be associated with one service registry. Multiple
-- service registries for each service isn\'t supported.
--
-- 'tags', 'createService_tags' - The metadata that you apply to the service to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define. When a service is deleted, the tags are deleted as
-- well.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8
--
-- -   Maximum value length - 256 Unicode characters in UTF-8
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case-sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for either keys or values as it is reserved for
--     Amazon Web Services use. You cannot edit or delete tag keys or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per resource limit.
--
-- 'taskDefinition', 'createService_taskDefinition' - The @family@ and @revision@ (@family:revision@) or full ARN of the task
-- definition to run in your service. If a @revision@ isn\'t specified, the
-- latest @ACTIVE@ revision is used.
--
-- A task definition must be specified if the service uses either the @ECS@
-- or @CODE_DEPLOY@ deployment controllers.
--
-- 'serviceName', 'createService_serviceName' - The name of your service. Up to 255 letters (uppercase and lowercase),
-- numbers, underscores, and hyphens are allowed. Service names must be
-- unique within a cluster, but you can have similarly named services in
-- multiple clusters within a Region or across multiple Regions.
newCreateService ::
  -- | 'serviceName'
  Prelude.Text ->
  CreateService
newCreateService pServiceName_ =
  CreateService'
    { capacityProviderStrategy =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      cluster = Prelude.Nothing,
      deploymentConfiguration = Prelude.Nothing,
      deploymentController = Prelude.Nothing,
      desiredCount = Prelude.Nothing,
      enableECSManagedTags = Prelude.Nothing,
      enableExecuteCommand = Prelude.Nothing,
      healthCheckGracePeriodSeconds = Prelude.Nothing,
      launchType = Prelude.Nothing,
      loadBalancers = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      placementConstraints = Prelude.Nothing,
      placementStrategy = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      propagateTags = Prelude.Nothing,
      role' = Prelude.Nothing,
      schedulingStrategy = Prelude.Nothing,
      serviceConnectConfiguration = Prelude.Nothing,
      serviceRegistries = Prelude.Nothing,
      tags = Prelude.Nothing,
      taskDefinition = Prelude.Nothing,
      serviceName = pServiceName_
    }

-- | The capacity provider strategy to use for the service.
--
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter
-- must be omitted. If no @capacityProviderStrategy@ or @launchType@ is
-- specified, the @defaultCapacityProviderStrategy@ for the cluster is
-- used.
--
-- A capacity provider strategy may contain a maximum of 6 capacity
-- providers.
createService_capacityProviderStrategy :: Lens.Lens' CreateService (Prelude.Maybe [CapacityProviderStrategyItem])
createService_capacityProviderStrategy = Lens.lens (\CreateService' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@CreateService' {} a -> s {capacityProviderStrategy = a} :: CreateService) Prelude.. Lens.mapping Lens.coerced

-- | An identifier that you provide to ensure the idempotency of the request.
-- It must be unique and is case sensitive. Up to 32 ASCII characters are
-- allowed.
createService_clientToken :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_clientToken = Lens.lens (\CreateService' {clientToken} -> clientToken) (\s@CreateService' {} a -> s {clientToken = a} :: CreateService)

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- you run your service on. If you do not specify a cluster, the default
-- cluster is assumed.
createService_cluster :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_cluster = Lens.lens (\CreateService' {cluster} -> cluster) (\s@CreateService' {} a -> s {cluster = a} :: CreateService)

-- | Optional deployment parameters that control how many tasks run during
-- the deployment and the ordering of stopping and starting tasks.
createService_deploymentConfiguration :: Lens.Lens' CreateService (Prelude.Maybe DeploymentConfiguration)
createService_deploymentConfiguration = Lens.lens (\CreateService' {deploymentConfiguration} -> deploymentConfiguration) (\s@CreateService' {} a -> s {deploymentConfiguration = a} :: CreateService)

-- | The deployment controller to use for the service. If no deployment
-- controller is specified, the default value of @ECS@ is used.
createService_deploymentController :: Lens.Lens' CreateService (Prelude.Maybe DeploymentController)
createService_deploymentController = Lens.lens (\CreateService' {deploymentController} -> deploymentController) (\s@CreateService' {} a -> s {deploymentController = a} :: CreateService)

-- | The number of instantiations of the specified task definition to place
-- and keep running on your cluster.
--
-- This is required if @schedulingStrategy@ is @REPLICA@ or isn\'t
-- specified. If @schedulingStrategy@ is @DAEMON@ then this isn\'t
-- required.
createService_desiredCount :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Int)
createService_desiredCount = Lens.lens (\CreateService' {desiredCount} -> desiredCount) (\s@CreateService' {} a -> s {desiredCount = a} :: CreateService)

-- | Specifies whether to turn on Amazon ECS managed tags for the tasks
-- within the service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging your Amazon ECS resources>
-- in the /Amazon Elastic Container Service Developer Guide/.
createService_enableECSManagedTags :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Bool)
createService_enableECSManagedTags = Lens.lens (\CreateService' {enableECSManagedTags} -> enableECSManagedTags) (\s@CreateService' {} a -> s {enableECSManagedTags = a} :: CreateService)

-- | Determines whether the execute command functionality is enabled for the
-- service. If @true@, this enables execute command functionality on all
-- containers in the service tasks.
createService_enableExecuteCommand :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Bool)
createService_enableExecuteCommand = Lens.lens (\CreateService' {enableExecuteCommand} -> enableExecuteCommand) (\s@CreateService' {} a -> s {enableExecuteCommand = a} :: CreateService)

-- | The period of time, in seconds, that the Amazon ECS service scheduler
-- ignores unhealthy Elastic Load Balancing target health checks after a
-- task has first started. This is only used when your service is
-- configured to use a load balancer. If your service has a load balancer
-- defined and you don\'t specify a health check grace period value, the
-- default value of @0@ is used.
--
-- If you do not use an Elastic Load Balancing, we recommend that you use
-- the @startPeriod@ in the task definition health check parameters. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_HealthCheck.html Health check>.
--
-- If your service\'s tasks take a while to start and respond to Elastic
-- Load Balancing health checks, you can specify a health check grace
-- period of up to 2,147,483,647 seconds (about 69 years). During that
-- time, the Amazon ECS service scheduler ignores health check status. This
-- grace period can prevent the service scheduler from marking tasks as
-- unhealthy and stopping them before they have time to come up.
createService_healthCheckGracePeriodSeconds :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Int)
createService_healthCheckGracePeriodSeconds = Lens.lens (\CreateService' {healthCheckGracePeriodSeconds} -> healthCheckGracePeriodSeconds) (\s@CreateService' {} a -> s {healthCheckGracePeriodSeconds = a} :: CreateService)

-- | The infrastructure that you run your service on. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- The @FARGATE@ launch type runs your tasks on Fargate On-Demand
-- infrastructure.
--
-- Fargate Spot infrastructure is available for use but a capacity provider
-- strategy must be used. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/userguide/fargate-capacity-providers.html Fargate capacity providers>
-- in the /Amazon ECS User Guide for Fargate/.
--
-- The @EC2@ launch type runs your tasks on Amazon EC2 instances registered
-- to your cluster.
--
-- The @EXTERNAL@ launch type runs your tasks on your on-premises server or
-- virtual machine (VM) capacity registered to your cluster.
--
-- A service can use either a launch type or a capacity provider strategy.
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter
-- must be omitted.
createService_launchType :: Lens.Lens' CreateService (Prelude.Maybe LaunchType)
createService_launchType = Lens.lens (\CreateService' {launchType} -> launchType) (\s@CreateService' {} a -> s {launchType = a} :: CreateService)

-- | A load balancer object representing the load balancers to use with your
-- service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-load-balancing.html Service load balancing>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If the service uses the rolling update (@ECS@) deployment controller and
-- using either an Application Load Balancer or Network Load Balancer, you
-- must specify one or more target group ARNs to attach to the service. The
-- service-linked role is required for services that use multiple target
-- groups. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using service-linked roles for Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If the service uses the @CODE_DEPLOY@ deployment controller, the service
-- is required to use either an Application Load Balancer or Network Load
-- Balancer. When creating an CodeDeploy deployment group, you specify two
-- target groups (referred to as a @targetGroupPair@). During a deployment,
-- CodeDeploy determines which task set in your service has the status
-- @PRIMARY@, and it associates one target group with it. Then, it also
-- associates the other target group with the replacement task set. The
-- load balancer can also have up to two listeners: a required listener for
-- production traffic and an optional listener that you can use to perform
-- validation tests with Lambda functions before routing production traffic
-- to it.
--
-- If you use the @CODE_DEPLOY@ deployment controller, these values can be
-- changed when updating the service.
--
-- For Application Load Balancers and Network Load Balancers, this object
-- must contain the load balancer target group ARN, the container name, and
-- the container port to access from the load balancer. The container name
-- must be as it appears in a container definition. The load balancer name
-- parameter must be omitted. When a task from this service is placed on a
-- container instance, the container instance and port combination is
-- registered as a target in the target group that\'s specified here.
--
-- For Classic Load Balancers, this object must contain the load balancer
-- name, the container name , and the container port to access from the
-- load balancer. The container name must be as it appears in a container
-- definition. The target group ARN parameter must be omitted. When a task
-- from this service is placed on a container instance, the container
-- instance is registered with the load balancer that\'s specified here.
--
-- Services with tasks that use the @awsvpc@ network mode (for example,
-- those with the Fargate launch type) only support Application Load
-- Balancers and Network Load Balancers. Classic Load Balancers aren\'t
-- supported. Also, when you create any target groups for these services,
-- you must choose @ip@ as the target type, not @instance@. This is because
-- tasks that use the @awsvpc@ network mode are associated with an elastic
-- network interface, not an Amazon EC2 instance.
createService_loadBalancers :: Lens.Lens' CreateService (Prelude.Maybe [LoadBalancer])
createService_loadBalancers = Lens.lens (\CreateService' {loadBalancers} -> loadBalancers) (\s@CreateService' {} a -> s {loadBalancers = a} :: CreateService) Prelude.. Lens.mapping Lens.coerced

-- | The network configuration for the service. This parameter is required
-- for task definitions that use the @awsvpc@ network mode to receive their
-- own elastic network interface, and it isn\'t supported for other network
-- modes. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task networking>
-- in the /Amazon Elastic Container Service Developer Guide/.
createService_networkConfiguration :: Lens.Lens' CreateService (Prelude.Maybe NetworkConfiguration)
createService_networkConfiguration = Lens.lens (\CreateService' {networkConfiguration} -> networkConfiguration) (\s@CreateService' {} a -> s {networkConfiguration = a} :: CreateService)

-- | An array of placement constraint objects to use for tasks in your
-- service. You can specify a maximum of 10 constraints for each task. This
-- limit includes constraints in the task definition and those specified at
-- runtime.
createService_placementConstraints :: Lens.Lens' CreateService (Prelude.Maybe [PlacementConstraint])
createService_placementConstraints = Lens.lens (\CreateService' {placementConstraints} -> placementConstraints) (\s@CreateService' {} a -> s {placementConstraints = a} :: CreateService) Prelude.. Lens.mapping Lens.coerced

-- | The placement strategy objects to use for tasks in your service. You can
-- specify a maximum of 5 strategy rules for each service.
createService_placementStrategy :: Lens.Lens' CreateService (Prelude.Maybe [PlacementStrategy])
createService_placementStrategy = Lens.lens (\CreateService' {placementStrategy} -> placementStrategy) (\s@CreateService' {} a -> s {placementStrategy = a} :: CreateService) Prelude.. Lens.mapping Lens.coerced

-- | The platform version that your tasks in the service are running on. A
-- platform version is specified only for tasks using the Fargate launch
-- type. If one isn\'t specified, the @LATEST@ platform version is used.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate platform versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
createService_platformVersion :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_platformVersion = Lens.lens (\CreateService' {platformVersion} -> platformVersion) (\s@CreateService' {} a -> s {platformVersion = a} :: CreateService)

-- | Specifies whether to propagate the tags from the task definition to the
-- task. If no value is specified, the tags aren\'t propagated. Tags can
-- only be propagated to the task during task creation. To add tags to a
-- task after task creation, use the TagResource API action.
createService_propagateTags :: Lens.Lens' CreateService (Prelude.Maybe PropagateTags)
createService_propagateTags = Lens.lens (\CreateService' {propagateTags} -> propagateTags) (\s@CreateService' {} a -> s {propagateTags = a} :: CreateService)

-- | The name or full Amazon Resource Name (ARN) of the IAM role that allows
-- Amazon ECS to make calls to your load balancer on your behalf. This
-- parameter is only permitted if you are using a load balancer with your
-- service and your task definition doesn\'t use the @awsvpc@ network mode.
-- If you specify the @role@ parameter, you must also specify a load
-- balancer object with the @loadBalancers@ parameter.
--
-- If your account has already created the Amazon ECS service-linked role,
-- that role is used for your service unless you specify a role here. The
-- service-linked role is required if your task definition uses the
-- @awsvpc@ network mode or if the service is configured to use service
-- discovery, an external deployment controller, multiple target groups, or
-- Elastic Inference accelerators in which case you don\'t specify a role
-- here. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using service-linked roles for Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If your specified role has a path other than @\/@, then you must either
-- specify the full role ARN (this is recommended) or prefix the role name
-- with the path. For example, if a role with the name @bar@ has a path of
-- @\/foo\/@ then you would specify @\/foo\/bar@ as the role name. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-friendly-names Friendly names and paths>
-- in the /IAM User Guide/.
createService_role :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_role = Lens.lens (\CreateService' {role'} -> role') (\s@CreateService' {} a -> s {role' = a} :: CreateService)

-- | The scheduling strategy to use for the service. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services>.
--
-- There are two service scheduler strategies available:
--
-- -   @REPLICA@-The replica scheduling strategy places and maintains the
--     desired number of tasks across your cluster. By default, the service
--     scheduler spreads tasks across Availability Zones. You can use task
--     placement strategies and constraints to customize task placement
--     decisions. This scheduler strategy is required if the service uses
--     the @CODE_DEPLOY@ or @EXTERNAL@ deployment controller types.
--
-- -   @DAEMON@-The daemon scheduling strategy deploys exactly one task on
--     each active container instance that meets all of the task placement
--     constraints that you specify in your cluster. The service scheduler
--     also evaluates the task placement constraints for running tasks and
--     will stop tasks that don\'t meet the placement constraints. When
--     you\'re using this strategy, you don\'t need to specify a desired
--     number of tasks, a task placement strategy, or use Service Auto
--     Scaling policies.
--
--     Tasks using the Fargate launch type or the @CODE_DEPLOY@ or
--     @EXTERNAL@ deployment controller types don\'t support the @DAEMON@
--     scheduling strategy.
createService_schedulingStrategy :: Lens.Lens' CreateService (Prelude.Maybe SchedulingStrategy)
createService_schedulingStrategy = Lens.lens (\CreateService' {schedulingStrategy} -> schedulingStrategy) (\s@CreateService' {} a -> s {schedulingStrategy = a} :: CreateService)

-- | The configuration for this service to discover and connect to services,
-- and be discovered by, and connected from, other services within a
-- namespace.
--
-- Tasks that run in a namespace can use short names to connect to services
-- in the namespace. Tasks can connect to services across all of the
-- clusters in the namespace. Tasks connect through a managed proxy
-- container that collects logs and metrics for increased visibility. Only
-- the tasks that Amazon ECS services create are supported with Service
-- Connect. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
createService_serviceConnectConfiguration :: Lens.Lens' CreateService (Prelude.Maybe ServiceConnectConfiguration)
createService_serviceConnectConfiguration = Lens.lens (\CreateService' {serviceConnectConfiguration} -> serviceConnectConfiguration) (\s@CreateService' {} a -> s {serviceConnectConfiguration = a} :: CreateService)

-- | The details of the service discovery registry to associate with this
-- service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service discovery>.
--
-- Each service may be associated with one service registry. Multiple
-- service registries for each service isn\'t supported.
createService_serviceRegistries :: Lens.Lens' CreateService (Prelude.Maybe [ServiceRegistry])
createService_serviceRegistries = Lens.lens (\CreateService' {serviceRegistries} -> serviceRegistries) (\s@CreateService' {} a -> s {serviceRegistries = a} :: CreateService) Prelude.. Lens.mapping Lens.coerced

-- | The metadata that you apply to the service to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define. When a service is deleted, the tags are deleted as
-- well.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8
--
-- -   Maximum value length - 256 Unicode characters in UTF-8
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case-sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for either keys or values as it is reserved for
--     Amazon Web Services use. You cannot edit or delete tag keys or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per resource limit.
createService_tags :: Lens.Lens' CreateService (Prelude.Maybe [Tag])
createService_tags = Lens.lens (\CreateService' {tags} -> tags) (\s@CreateService' {} a -> s {tags = a} :: CreateService) Prelude.. Lens.mapping Lens.coerced

-- | The @family@ and @revision@ (@family:revision@) or full ARN of the task
-- definition to run in your service. If a @revision@ isn\'t specified, the
-- latest @ACTIVE@ revision is used.
--
-- A task definition must be specified if the service uses either the @ECS@
-- or @CODE_DEPLOY@ deployment controllers.
createService_taskDefinition :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_taskDefinition = Lens.lens (\CreateService' {taskDefinition} -> taskDefinition) (\s@CreateService' {} a -> s {taskDefinition = a} :: CreateService)

-- | The name of your service. Up to 255 letters (uppercase and lowercase),
-- numbers, underscores, and hyphens are allowed. Service names must be
-- unique within a cluster, but you can have similarly named services in
-- multiple clusters within a Region or across multiple Regions.
createService_serviceName :: Lens.Lens' CreateService Prelude.Text
createService_serviceName = Lens.lens (\CreateService' {serviceName} -> serviceName) (\s@CreateService' {} a -> s {serviceName = a} :: CreateService)

instance Core.AWSRequest CreateService where
  type
    AWSResponse CreateService =
      CreateServiceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceResponse'
            Prelude.<$> (x Data..?> "service")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateService where
  hashWithSalt _salt CreateService' {..} =
    _salt
      `Prelude.hashWithSalt` capacityProviderStrategy
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` deploymentConfiguration
      `Prelude.hashWithSalt` deploymentController
      `Prelude.hashWithSalt` desiredCount
      `Prelude.hashWithSalt` enableECSManagedTags
      `Prelude.hashWithSalt` enableExecuteCommand
      `Prelude.hashWithSalt` healthCheckGracePeriodSeconds
      `Prelude.hashWithSalt` launchType
      `Prelude.hashWithSalt` loadBalancers
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` placementConstraints
      `Prelude.hashWithSalt` placementStrategy
      `Prelude.hashWithSalt` platformVersion
      `Prelude.hashWithSalt` propagateTags
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` schedulingStrategy
      `Prelude.hashWithSalt` serviceConnectConfiguration
      `Prelude.hashWithSalt` serviceRegistries
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` taskDefinition
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData CreateService where
  rnf CreateService' {..} =
    Prelude.rnf capacityProviderStrategy
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf deploymentConfiguration
      `Prelude.seq` Prelude.rnf deploymentController
      `Prelude.seq` Prelude.rnf desiredCount
      `Prelude.seq` Prelude.rnf enableECSManagedTags
      `Prelude.seq` Prelude.rnf enableExecuteCommand
      `Prelude.seq` Prelude.rnf healthCheckGracePeriodSeconds
      `Prelude.seq` Prelude.rnf launchType
      `Prelude.seq` Prelude.rnf loadBalancers
      `Prelude.seq` Prelude.rnf networkConfiguration
      `Prelude.seq` Prelude.rnf placementConstraints
      `Prelude.seq` Prelude.rnf placementStrategy
      `Prelude.seq` Prelude.rnf platformVersion
      `Prelude.seq` Prelude.rnf propagateTags
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf schedulingStrategy
      `Prelude.seq` Prelude.rnf
        serviceConnectConfiguration
      `Prelude.seq` Prelude.rnf serviceRegistries
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf taskDefinition
      `Prelude.seq` Prelude.rnf serviceName

instance Data.ToHeaders CreateService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.CreateService" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateService where
  toJSON CreateService' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("capacityProviderStrategy" Data..=)
              Prelude.<$> capacityProviderStrategy,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("cluster" Data..=) Prelude.<$> cluster,
            ("deploymentConfiguration" Data..=)
              Prelude.<$> deploymentConfiguration,
            ("deploymentController" Data..=)
              Prelude.<$> deploymentController,
            ("desiredCount" Data..=) Prelude.<$> desiredCount,
            ("enableECSManagedTags" Data..=)
              Prelude.<$> enableECSManagedTags,
            ("enableExecuteCommand" Data..=)
              Prelude.<$> enableExecuteCommand,
            ("healthCheckGracePeriodSeconds" Data..=)
              Prelude.<$> healthCheckGracePeriodSeconds,
            ("launchType" Data..=) Prelude.<$> launchType,
            ("loadBalancers" Data..=) Prelude.<$> loadBalancers,
            ("networkConfiguration" Data..=)
              Prelude.<$> networkConfiguration,
            ("placementConstraints" Data..=)
              Prelude.<$> placementConstraints,
            ("placementStrategy" Data..=)
              Prelude.<$> placementStrategy,
            ("platformVersion" Data..=)
              Prelude.<$> platformVersion,
            ("propagateTags" Data..=) Prelude.<$> propagateTags,
            ("role" Data..=) Prelude.<$> role',
            ("schedulingStrategy" Data..=)
              Prelude.<$> schedulingStrategy,
            ("serviceConnectConfiguration" Data..=)
              Prelude.<$> serviceConnectConfiguration,
            ("serviceRegistries" Data..=)
              Prelude.<$> serviceRegistries,
            ("tags" Data..=) Prelude.<$> tags,
            ("taskDefinition" Data..=)
              Prelude.<$> taskDefinition,
            Prelude.Just ("serviceName" Data..= serviceName)
          ]
      )

instance Data.ToPath CreateService where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServiceResponse' smart constructor.
data CreateServiceResponse = CreateServiceResponse'
  { -- | The full description of your service following the create call.
    --
    -- A service will return either a @capacityProviderStrategy@ or
    -- @launchType@ parameter, but not both, depending where one was specified
    -- when it was created.
    --
    -- If a service is using the @ECS@ deployment controller, the
    -- @deploymentController@ and @taskSets@ parameters will not be returned.
    --
    -- if the service uses the @CODE_DEPLOY@ deployment controller, the
    -- @deploymentController@, @taskSets@ and @deployments@ parameters will be
    -- returned, however the @deployments@ parameter will be an empty list.
    service :: Prelude.Maybe ContainerService,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'service', 'createServiceResponse_service' - The full description of your service following the create call.
--
-- A service will return either a @capacityProviderStrategy@ or
-- @launchType@ parameter, but not both, depending where one was specified
-- when it was created.
--
-- If a service is using the @ECS@ deployment controller, the
-- @deploymentController@ and @taskSets@ parameters will not be returned.
--
-- if the service uses the @CODE_DEPLOY@ deployment controller, the
-- @deploymentController@, @taskSets@ and @deployments@ parameters will be
-- returned, however the @deployments@ parameter will be an empty list.
--
-- 'httpStatus', 'createServiceResponse_httpStatus' - The response's http status code.
newCreateServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateServiceResponse
newCreateServiceResponse pHttpStatus_ =
  CreateServiceResponse'
    { service = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of your service following the create call.
--
-- A service will return either a @capacityProviderStrategy@ or
-- @launchType@ parameter, but not both, depending where one was specified
-- when it was created.
--
-- If a service is using the @ECS@ deployment controller, the
-- @deploymentController@ and @taskSets@ parameters will not be returned.
--
-- if the service uses the @CODE_DEPLOY@ deployment controller, the
-- @deploymentController@, @taskSets@ and @deployments@ parameters will be
-- returned, however the @deployments@ parameter will be an empty list.
createServiceResponse_service :: Lens.Lens' CreateServiceResponse (Prelude.Maybe ContainerService)
createServiceResponse_service = Lens.lens (\CreateServiceResponse' {service} -> service) (\s@CreateServiceResponse' {} a -> s {service = a} :: CreateServiceResponse)

-- | The response's http status code.
createServiceResponse_httpStatus :: Lens.Lens' CreateServiceResponse Prelude.Int
createServiceResponse_httpStatus = Lens.lens (\CreateServiceResponse' {httpStatus} -> httpStatus) (\s@CreateServiceResponse' {} a -> s {httpStatus = a} :: CreateServiceResponse)

instance Prelude.NFData CreateServiceResponse where
  rnf CreateServiceResponse' {..} =
    Prelude.rnf service
      `Prelude.seq` Prelude.rnf httpStatus
