{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ECS.CreateService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs and maintains a desired number of tasks from a specified task
-- definition. If the number of tasks running in a service drops below the
-- @desiredCount@, Amazon ECS runs another copy of the task in the
-- specified cluster. To update an existing service, see the UpdateService
-- action.
--
-- In addition to maintaining the desired count of tasks in your service,
-- you can optionally run your service behind one or more load balancers.
-- The load balancers distribute traffic across the tasks that are
-- associated with the service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-load-balancing.html Service Load Balancing>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- Tasks for services that /do not/ use a load balancer are considered
-- healthy if they\'re in the @RUNNING@ state. Tasks for services that /do/
-- use a load balancer are considered healthy if they\'re in the @RUNNING@
-- state and the container instance that they\'re hosted on is reported as
-- healthy by the load balancer.
--
-- There are two service scheduler strategies available:
--
-- -   @REPLICA@ - The replica scheduling strategy places and maintains the
--     desired number of tasks across your cluster. By default, the service
--     scheduler spreads tasks across Availability Zones. You can use task
--     placement strategies and constraints to customize task placement
--     decisions. For more information, see
--     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Service Scheduler Concepts>
--     in the /Amazon Elastic Container Service Developer Guide/.
--
-- -   @DAEMON@ - The daemon scheduling strategy deploys exactly one task
--     on each active container instance that meets all of the task
--     placement constraints that you specify in your cluster. The service
--     scheduler also evaluates the task placement constraints for running
--     tasks and will stop tasks that do not meet the placement
--     constraints. When using this strategy, you don\'t need to specify a
--     desired number of tasks, a task placement strategy, or use Service
--     Auto Scaling policies. For more information, see
--     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Service Scheduler Concepts>
--     in the /Amazon Elastic Container Service Developer Guide/.
--
-- You can optionally specify a deployment configuration for your service.
-- The deployment is triggered by changing properties, such as the task
-- definition or the desired count of a service, with an UpdateService
-- operation. The default value for a replica service for
-- @minimumHealthyPercent@ is 100%. The default value for a daemon service
-- for @minimumHealthyPercent@ is 0%.
--
-- If a service is using the @ECS@ deployment controller, the minimum
-- healthy percent represents a lower limit on the number of tasks in a
-- service that must remain in the @RUNNING@ state during a deployment, as
-- a percentage of the desired number of tasks (rounded up to the nearest
-- integer), and while any container instances are in the @DRAINING@ state
-- if the service contains tasks using the EC2 launch type. This parameter
-- enables you to deploy without using additional cluster capacity. For
-- example, if your service has a desired number of four tasks and a
-- minimum healthy percent of 50%, the scheduler might stop two existing
-- tasks to free up cluster capacity before starting two new tasks. Tasks
-- for services that /do not/ use a load balancer are considered healthy if
-- they\'re in the @RUNNING@ state. Tasks for services that /do/ use a load
-- balancer are considered healthy if they\'re in the @RUNNING@ state and
-- they\'re reported as healthy by the load balancer. The default value for
-- minimum healthy percent is 100%.
--
-- If a service is using the @ECS@ deployment controller, the __maximum
-- percent__ parameter represents an upper limit on the number of tasks in
-- a service that are allowed in the @RUNNING@ or @PENDING@ state during a
-- deployment, as a percentage of the desired number of tasks (rounded down
-- to the nearest integer), and while any container instances are in the
-- @DRAINING@ state if the service contains tasks using the EC2 launch
-- type. This parameter enables you to define the deployment batch size.
-- For example, if your service has a desired number of four tasks and a
-- maximum percent value of 200%, the scheduler may start four new tasks
-- before stopping the four older tasks (provided that the cluster
-- resources required to do this are available). The default value for
-- maximum percent is 200%.
--
-- If a service is using either the @CODE_DEPLOY@ or @EXTERNAL@ deployment
-- controller types and tasks that use the EC2 launch type, the __minimum
-- healthy percent__ and __maximum percent__ values are used only to define
-- the lower and upper limit on the number of the tasks in the service that
-- remain in the @RUNNING@ state while the container instances are in the
-- @DRAINING@ state. If the tasks in the service use the Fargate launch
-- type, the minimum healthy percent and maximum percent values aren\'t
-- used, although they\'re currently visible when describing your service.
--
-- When creating a service that uses the @EXTERNAL@ deployment controller,
-- you can specify only parameters that aren\'t controlled at the task set
-- level. The only required parameter is the service name. You control your
-- services using the CreateTaskSet operation. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- When the service scheduler launches new tasks, it determines task
-- placement in your cluster using the following logic:
--
-- -   Determine which of the container instances in your cluster can
--     support your service\'s task definition (for example, they have the
--     required CPU, memory, ports, and container instance attributes).
--
-- -   By default, the service scheduler attempts to balance tasks across
--     Availability Zones in this manner (although you can choose a
--     different placement strategy) with the @placementStrategy@
--     parameter):
--
--     -   Sort the valid container instances, giving priority to instances
--         that have the fewest number of running tasks for this service in
--         their respective Availability Zone. For example, if zone A has
--         one running service task and zones B and C each have zero, valid
--         container instances in either zone B or C are considered optimal
--         for placement.
--
--     -   Place the new service task on a valid container instance in an
--         optimal Availability Zone (based on the previous steps),
--         favoring container instances with the fewest number of running
--         tasks for this service.
module Network.AWS.ECS.CreateService
  ( -- * Creating a Request
    CreateService (..),
    newCreateService,

    -- * Request Lenses
    createService_deploymentConfiguration,
    createService_networkConfiguration,
    createService_capacityProviderStrategy,
    createService_desiredCount,
    createService_enableECSManagedTags,
    createService_launchType,
    createService_platformVersion,
    createService_deploymentController,
    createService_placementStrategy,
    createService_placementConstraints,
    createService_role,
    createService_loadBalancers,
    createService_tags,
    createService_serviceRegistries,
    createService_healthCheckGracePeriodSeconds,
    createService_schedulingStrategy,
    createService_taskDefinition,
    createService_cluster,
    createService_clientToken,
    createService_propagateTags,
    createService_serviceName,

    -- * Destructuring the Response
    CreateServiceResponse (..),
    newCreateServiceResponse,

    -- * Response Lenses
    createServiceResponse_service,
    createServiceResponse_httpStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateService' smart constructor.
data CreateService = CreateService'
  { -- | Optional deployment parameters that control how many tasks run during
    -- the deployment and the ordering of stopping and starting tasks.
    deploymentConfiguration :: Prelude.Maybe DeploymentConfiguration,
    -- | The network configuration for the service. This parameter is required
    -- for task definitions that use the @awsvpc@ network mode to receive their
    -- own elastic network interface, and it is not supported for other network
    -- modes. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | The capacity provider strategy to use for the service.
    --
    -- A capacity provider strategy consists of one or more capacity providers
    -- along with the @base@ and @weight@ to assign to them. A capacity
    -- provider must be associated with the cluster to be used in a capacity
    -- provider strategy. The PutClusterCapacityProviders API is used to
    -- associate a capacity provider with a cluster. Only capacity providers
    -- with an @ACTIVE@ or @UPDATING@ status can be used.
    --
    -- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter
    -- must be omitted. If no @capacityProviderStrategy@ or @launchType@ is
    -- specified, the @defaultCapacityProviderStrategy@ for the cluster is
    -- used.
    --
    -- If specifying a capacity provider that uses an Auto Scaling group, the
    -- capacity provider must already be created. New capacity providers can be
    -- created with the CreateCapacityProvider API operation.
    --
    -- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or
    -- @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers
    -- are available to all accounts and only need to be associated with a
    -- cluster to be used.
    --
    -- The PutClusterCapacityProviders API operation is used to update the list
    -- of available capacity providers for a cluster after the cluster is
    -- created.
    capacityProviderStrategy :: Prelude.Maybe [CapacityProviderStrategyItem],
    -- | The number of instantiations of the specified task definition to place
    -- and keep running on your cluster.
    --
    -- This is required if @schedulingStrategy@ is @REPLICA@ or is not
    -- specified. If @schedulingStrategy@ is @DAEMON@ then this is not
    -- required.
    desiredCount :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether to enable Amazon ECS managed tags for the tasks within
    -- the service. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    enableECSManagedTags :: Prelude.Maybe Prelude.Bool,
    -- | The launch type on which to run your service. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter
    -- must be omitted.
    launchType :: Prelude.Maybe LaunchType,
    -- | The platform version that your tasks in the service are running on. A
    -- platform version is specified only for tasks using the Fargate launch
    -- type. If one isn\'t specified, the @LATEST@ platform version is used by
    -- default. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | The deployment controller to use for the service.
    deploymentController :: Prelude.Maybe DeploymentController,
    -- | The placement strategy objects to use for tasks in your service. You can
    -- specify a maximum of five strategy rules per service.
    placementStrategy :: Prelude.Maybe [PlacementStrategy],
    -- | An array of placement constraint objects to use for tasks in your
    -- service. You can specify a maximum of 10 constraints per task (this
    -- limit includes constraints in the task definition and those specified at
    -- runtime).
    placementConstraints :: Prelude.Maybe [PlacementConstraint],
    -- | The name or full Amazon Resource Name (ARN) of the IAM role that allows
    -- Amazon ECS to make calls to your load balancer on your behalf. This
    -- parameter is only permitted if you are using a load balancer with your
    -- service and your task definition does not use the @awsvpc@ network mode.
    -- If you specify the @role@ parameter, you must also specify a load
    -- balancer object with the @loadBalancers@ parameter.
    --
    -- If your account has already created the Amazon ECS service-linked role,
    -- that role is used by default for your service unless you specify a role
    -- here. The service-linked role is required if your task definition uses
    -- the @awsvpc@ network mode or if the service is configured to use service
    -- discovery, an external deployment controller, multiple target groups, or
    -- Elastic Inference accelerators in which case you should not specify a
    -- role here. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon ECS>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- If your specified role has a path other than @\/@, then you must either
    -- specify the full role ARN (this is recommended) or prefix the role name
    -- with the path. For example, if a role with the name @bar@ has a path of
    -- @\/foo\/@ then you would specify @\/foo\/bar@ as the role name. For more
    -- information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-friendly-names Friendly Names and Paths>
    -- in the /IAM User Guide/.
    role' :: Prelude.Maybe Prelude.Text,
    -- | A load balancer object representing the load balancers to use with your
    -- service. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-load-balancing.html Service Load Balancing>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- If the service is using the rolling update (@ECS@) deployment controller
    -- and using either an Application Load Balancer or Network Load Balancer,
    -- you must specify one or more target group ARNs to attach to the service.
    -- The service-linked role is required for services that make use of
    -- multiple target groups. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon ECS>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- If the service is using the @CODE_DEPLOY@ deployment controller, the
    -- service is required to use either an Application Load Balancer or
    -- Network Load Balancer. When creating an AWS CodeDeploy deployment group,
    -- you specify two target groups (referred to as a @targetGroupPair@).
    -- During a deployment, AWS CodeDeploy determines which task set in your
    -- service has the status @PRIMARY@ and associates one target group with
    -- it, and then associates the other target group with the replacement task
    -- set. The load balancer can also have up to two listeners: a required
    -- listener for production traffic and an optional listener that allows you
    -- perform validation tests with Lambda functions before routing production
    -- traffic to it.
    --
    -- After you create a service using the @ECS@ deployment controller, the
    -- load balancer name or target group ARN, container name, and container
    -- port specified in the service definition are immutable. If you are using
    -- the @CODE_DEPLOY@ deployment controller, these values can be changed
    -- when updating the service.
    --
    -- For Application Load Balancers and Network Load Balancers, this object
    -- must contain the load balancer target group ARN, the container name (as
    -- it appears in a container definition), and the container port to access
    -- from the load balancer. The load balancer name parameter must be
    -- omitted. When a task from this service is placed on a container
    -- instance, the container instance and port combination is registered as a
    -- target in the target group specified here.
    --
    -- For Classic Load Balancers, this object must contain the load balancer
    -- name, the container name (as it appears in a container definition), and
    -- the container port to access from the load balancer. The target group
    -- ARN parameter must be omitted. When a task from this service is placed
    -- on a container instance, the container instance is registered with the
    -- load balancer specified here.
    --
    -- Services with tasks that use the @awsvpc@ network mode (for example,
    -- those with the Fargate launch type) only support Application Load
    -- Balancers and Network Load Balancers. Classic Load Balancers are not
    -- supported. Also, when you create any target groups for these services,
    -- you must choose @ip@ as the target type, not @instance@, because tasks
    -- that use the @awsvpc@ network mode are associated with an elastic
    -- network interface, not an Amazon EC2 instance.
    loadBalancers :: Prelude.Maybe [LoadBalancer],
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
    --     such as a prefix for either keys or values as it is reserved for AWS
    --     use. You cannot edit or delete tag keys or values with this prefix.
    --     Tags with this prefix do not count against your tags per resource
    --     limit.
    tags :: Prelude.Maybe [Tag],
    -- | The details of the service discovery registries to assign to this
    -- service. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
    --
    -- Service discovery is supported for Fargate tasks if you are using
    -- platform version v1.1.0 or later. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>.
    serviceRegistries :: Prelude.Maybe [ServiceRegistry],
    -- | The period of time, in seconds, that the Amazon ECS service scheduler
    -- should ignore unhealthy Elastic Load Balancing target health checks
    -- after a task has first started. This is only used when your service is
    -- configured to use a load balancer. If your service has a load balancer
    -- defined and you don\'t specify a health check grace period value, the
    -- default value of @0@ is used.
    --
    -- If your service\'s tasks take a while to start and respond to Elastic
    -- Load Balancing health checks, you can specify a health check grace
    -- period of up to 2,147,483,647 seconds. During that time, the Amazon ECS
    -- service scheduler ignores health check status. This grace period can
    -- prevent the service scheduler from marking tasks as unhealthy and
    -- stopping them before they have time to come up.
    healthCheckGracePeriodSeconds :: Prelude.Maybe Prelude.Int,
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
    --     decisions. This scheduler strategy is required if the service is
    --     using the @CODE_DEPLOY@ or @EXTERNAL@ deployment controller types.
    --
    -- -   @DAEMON@-The daemon scheduling strategy deploys exactly one task on
    --     each active container instance that meets all of the task placement
    --     constraints that you specify in your cluster. The service scheduler
    --     also evaluates the task placement constraints for running tasks and
    --     will stop tasks that do not meet the placement constraints. When
    --     you\'re using this strategy, you don\'t need to specify a desired
    --     number of tasks, a task placement strategy, or use Service Auto
    --     Scaling policies.
    --
    --     Tasks using the Fargate launch type or the @CODE_DEPLOY@ or
    --     @EXTERNAL@ deployment controller types don\'t support the @DAEMON@
    --     scheduling strategy.
    schedulingStrategy :: Prelude.Maybe SchedulingStrategy,
    -- | The @family@ and @revision@ (@family:revision@) or full ARN of the task
    -- definition to run in your service. If a @revision@ is not specified, the
    -- latest @ACTIVE@ revision is used.
    --
    -- A task definition must be specified if the service is using either the
    -- @ECS@ or @CODE_DEPLOY@ deployment controllers.
    taskDefinition :: Prelude.Maybe Prelude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster on
    -- which to run your service. If you do not specify a cluster, the default
    -- cluster is assumed.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. Up to 32 ASCII characters are allowed.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to propagate the tags from the task definition or the
    -- service to the tasks in the service. If no value is specified, the tags
    -- are not propagated. Tags can only be propagated to the tasks within the
    -- service during service creation. To add tags to a task after service
    -- creation, use the TagResource API action.
    propagateTags :: Prelude.Maybe PropagateTags,
    -- | The name of your service. Up to 255 letters (uppercase and lowercase),
    -- numbers, and hyphens are allowed. Service names must be unique within a
    -- cluster, but you can have similarly named services in multiple clusters
    -- within a Region or across multiple Regions.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentConfiguration', 'createService_deploymentConfiguration' - Optional deployment parameters that control how many tasks run during
-- the deployment and the ordering of stopping and starting tasks.
--
-- 'networkConfiguration', 'createService_networkConfiguration' - The network configuration for the service. This parameter is required
-- for task definitions that use the @awsvpc@ network mode to receive their
-- own elastic network interface, and it is not supported for other network
-- modes. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'capacityProviderStrategy', 'createService_capacityProviderStrategy' - The capacity provider strategy to use for the service.
--
-- A capacity provider strategy consists of one or more capacity providers
-- along with the @base@ and @weight@ to assign to them. A capacity
-- provider must be associated with the cluster to be used in a capacity
-- provider strategy. The PutClusterCapacityProviders API is used to
-- associate a capacity provider with a cluster. Only capacity providers
-- with an @ACTIVE@ or @UPDATING@ status can be used.
--
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter
-- must be omitted. If no @capacityProviderStrategy@ or @launchType@ is
-- specified, the @defaultCapacityProviderStrategy@ for the cluster is
-- used.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the
-- capacity provider must already be created. New capacity providers can be
-- created with the CreateCapacityProvider API operation.
--
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers
-- are available to all accounts and only need to be associated with a
-- cluster to be used.
--
-- The PutClusterCapacityProviders API operation is used to update the list
-- of available capacity providers for a cluster after the cluster is
-- created.
--
-- 'desiredCount', 'createService_desiredCount' - The number of instantiations of the specified task definition to place
-- and keep running on your cluster.
--
-- This is required if @schedulingStrategy@ is @REPLICA@ or is not
-- specified. If @schedulingStrategy@ is @DAEMON@ then this is not
-- required.
--
-- 'enableECSManagedTags', 'createService_enableECSManagedTags' - Specifies whether to enable Amazon ECS managed tags for the tasks within
-- the service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'launchType', 'createService_launchType' - The launch type on which to run your service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter
-- must be omitted.
--
-- 'platformVersion', 'createService_platformVersion' - The platform version that your tasks in the service are running on. A
-- platform version is specified only for tasks using the Fargate launch
-- type. If one isn\'t specified, the @LATEST@ platform version is used by
-- default. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'deploymentController', 'createService_deploymentController' - The deployment controller to use for the service.
--
-- 'placementStrategy', 'createService_placementStrategy' - The placement strategy objects to use for tasks in your service. You can
-- specify a maximum of five strategy rules per service.
--
-- 'placementConstraints', 'createService_placementConstraints' - An array of placement constraint objects to use for tasks in your
-- service. You can specify a maximum of 10 constraints per task (this
-- limit includes constraints in the task definition and those specified at
-- runtime).
--
-- 'role'', 'createService_role' - The name or full Amazon Resource Name (ARN) of the IAM role that allows
-- Amazon ECS to make calls to your load balancer on your behalf. This
-- parameter is only permitted if you are using a load balancer with your
-- service and your task definition does not use the @awsvpc@ network mode.
-- If you specify the @role@ parameter, you must also specify a load
-- balancer object with the @loadBalancers@ parameter.
--
-- If your account has already created the Amazon ECS service-linked role,
-- that role is used by default for your service unless you specify a role
-- here. The service-linked role is required if your task definition uses
-- the @awsvpc@ network mode or if the service is configured to use service
-- discovery, an external deployment controller, multiple target groups, or
-- Elastic Inference accelerators in which case you should not specify a
-- role here. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If your specified role has a path other than @\/@, then you must either
-- specify the full role ARN (this is recommended) or prefix the role name
-- with the path. For example, if a role with the name @bar@ has a path of
-- @\/foo\/@ then you would specify @\/foo\/bar@ as the role name. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-friendly-names Friendly Names and Paths>
-- in the /IAM User Guide/.
--
-- 'loadBalancers', 'createService_loadBalancers' - A load balancer object representing the load balancers to use with your
-- service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-load-balancing.html Service Load Balancing>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If the service is using the rolling update (@ECS@) deployment controller
-- and using either an Application Load Balancer or Network Load Balancer,
-- you must specify one or more target group ARNs to attach to the service.
-- The service-linked role is required for services that make use of
-- multiple target groups. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If the service is using the @CODE_DEPLOY@ deployment controller, the
-- service is required to use either an Application Load Balancer or
-- Network Load Balancer. When creating an AWS CodeDeploy deployment group,
-- you specify two target groups (referred to as a @targetGroupPair@).
-- During a deployment, AWS CodeDeploy determines which task set in your
-- service has the status @PRIMARY@ and associates one target group with
-- it, and then associates the other target group with the replacement task
-- set. The load balancer can also have up to two listeners: a required
-- listener for production traffic and an optional listener that allows you
-- perform validation tests with Lambda functions before routing production
-- traffic to it.
--
-- After you create a service using the @ECS@ deployment controller, the
-- load balancer name or target group ARN, container name, and container
-- port specified in the service definition are immutable. If you are using
-- the @CODE_DEPLOY@ deployment controller, these values can be changed
-- when updating the service.
--
-- For Application Load Balancers and Network Load Balancers, this object
-- must contain the load balancer target group ARN, the container name (as
-- it appears in a container definition), and the container port to access
-- from the load balancer. The load balancer name parameter must be
-- omitted. When a task from this service is placed on a container
-- instance, the container instance and port combination is registered as a
-- target in the target group specified here.
--
-- For Classic Load Balancers, this object must contain the load balancer
-- name, the container name (as it appears in a container definition), and
-- the container port to access from the load balancer. The target group
-- ARN parameter must be omitted. When a task from this service is placed
-- on a container instance, the container instance is registered with the
-- load balancer specified here.
--
-- Services with tasks that use the @awsvpc@ network mode (for example,
-- those with the Fargate launch type) only support Application Load
-- Balancers and Network Load Balancers. Classic Load Balancers are not
-- supported. Also, when you create any target groups for these services,
-- you must choose @ip@ as the target type, not @instance@, because tasks
-- that use the @awsvpc@ network mode are associated with an elastic
-- network interface, not an Amazon EC2 instance.
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
--     such as a prefix for either keys or values as it is reserved for AWS
--     use. You cannot edit or delete tag keys or values with this prefix.
--     Tags with this prefix do not count against your tags per resource
--     limit.
--
-- 'serviceRegistries', 'createService_serviceRegistries' - The details of the service discovery registries to assign to this
-- service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
--
-- Service discovery is supported for Fargate tasks if you are using
-- platform version v1.1.0 or later. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>.
--
-- 'healthCheckGracePeriodSeconds', 'createService_healthCheckGracePeriodSeconds' - The period of time, in seconds, that the Amazon ECS service scheduler
-- should ignore unhealthy Elastic Load Balancing target health checks
-- after a task has first started. This is only used when your service is
-- configured to use a load balancer. If your service has a load balancer
-- defined and you don\'t specify a health check grace period value, the
-- default value of @0@ is used.
--
-- If your service\'s tasks take a while to start and respond to Elastic
-- Load Balancing health checks, you can specify a health check grace
-- period of up to 2,147,483,647 seconds. During that time, the Amazon ECS
-- service scheduler ignores health check status. This grace period can
-- prevent the service scheduler from marking tasks as unhealthy and
-- stopping them before they have time to come up.
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
--     decisions. This scheduler strategy is required if the service is
--     using the @CODE_DEPLOY@ or @EXTERNAL@ deployment controller types.
--
-- -   @DAEMON@-The daemon scheduling strategy deploys exactly one task on
--     each active container instance that meets all of the task placement
--     constraints that you specify in your cluster. The service scheduler
--     also evaluates the task placement constraints for running tasks and
--     will stop tasks that do not meet the placement constraints. When
--     you\'re using this strategy, you don\'t need to specify a desired
--     number of tasks, a task placement strategy, or use Service Auto
--     Scaling policies.
--
--     Tasks using the Fargate launch type or the @CODE_DEPLOY@ or
--     @EXTERNAL@ deployment controller types don\'t support the @DAEMON@
--     scheduling strategy.
--
-- 'taskDefinition', 'createService_taskDefinition' - The @family@ and @revision@ (@family:revision@) or full ARN of the task
-- definition to run in your service. If a @revision@ is not specified, the
-- latest @ACTIVE@ revision is used.
--
-- A task definition must be specified if the service is using either the
-- @ECS@ or @CODE_DEPLOY@ deployment controllers.
--
-- 'cluster', 'createService_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster on
-- which to run your service. If you do not specify a cluster, the default
-- cluster is assumed.
--
-- 'clientToken', 'createService_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 32 ASCII characters are allowed.
--
-- 'propagateTags', 'createService_propagateTags' - Specifies whether to propagate the tags from the task definition or the
-- service to the tasks in the service. If no value is specified, the tags
-- are not propagated. Tags can only be propagated to the tasks within the
-- service during service creation. To add tags to a task after service
-- creation, use the TagResource API action.
--
-- 'serviceName', 'createService_serviceName' - The name of your service. Up to 255 letters (uppercase and lowercase),
-- numbers, and hyphens are allowed. Service names must be unique within a
-- cluster, but you can have similarly named services in multiple clusters
-- within a Region or across multiple Regions.
newCreateService ::
  -- | 'serviceName'
  Prelude.Text ->
  CreateService
newCreateService pServiceName_ =
  CreateService'
    { deploymentConfiguration =
        Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      capacityProviderStrategy = Prelude.Nothing,
      desiredCount = Prelude.Nothing,
      enableECSManagedTags = Prelude.Nothing,
      launchType = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      deploymentController = Prelude.Nothing,
      placementStrategy = Prelude.Nothing,
      placementConstraints = Prelude.Nothing,
      role' = Prelude.Nothing,
      loadBalancers = Prelude.Nothing,
      tags = Prelude.Nothing,
      serviceRegistries = Prelude.Nothing,
      healthCheckGracePeriodSeconds = Prelude.Nothing,
      schedulingStrategy = Prelude.Nothing,
      taskDefinition = Prelude.Nothing,
      cluster = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      propagateTags = Prelude.Nothing,
      serviceName = pServiceName_
    }

-- | Optional deployment parameters that control how many tasks run during
-- the deployment and the ordering of stopping and starting tasks.
createService_deploymentConfiguration :: Lens.Lens' CreateService (Prelude.Maybe DeploymentConfiguration)
createService_deploymentConfiguration = Lens.lens (\CreateService' {deploymentConfiguration} -> deploymentConfiguration) (\s@CreateService' {} a -> s {deploymentConfiguration = a} :: CreateService)

-- | The network configuration for the service. This parameter is required
-- for task definitions that use the @awsvpc@ network mode to receive their
-- own elastic network interface, and it is not supported for other network
-- modes. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking>
-- in the /Amazon Elastic Container Service Developer Guide/.
createService_networkConfiguration :: Lens.Lens' CreateService (Prelude.Maybe NetworkConfiguration)
createService_networkConfiguration = Lens.lens (\CreateService' {networkConfiguration} -> networkConfiguration) (\s@CreateService' {} a -> s {networkConfiguration = a} :: CreateService)

-- | The capacity provider strategy to use for the service.
--
-- A capacity provider strategy consists of one or more capacity providers
-- along with the @base@ and @weight@ to assign to them. A capacity
-- provider must be associated with the cluster to be used in a capacity
-- provider strategy. The PutClusterCapacityProviders API is used to
-- associate a capacity provider with a cluster. Only capacity providers
-- with an @ACTIVE@ or @UPDATING@ status can be used.
--
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter
-- must be omitted. If no @capacityProviderStrategy@ or @launchType@ is
-- specified, the @defaultCapacityProviderStrategy@ for the cluster is
-- used.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the
-- capacity provider must already be created. New capacity providers can be
-- created with the CreateCapacityProvider API operation.
--
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers
-- are available to all accounts and only need to be associated with a
-- cluster to be used.
--
-- The PutClusterCapacityProviders API operation is used to update the list
-- of available capacity providers for a cluster after the cluster is
-- created.
createService_capacityProviderStrategy :: Lens.Lens' CreateService (Prelude.Maybe [CapacityProviderStrategyItem])
createService_capacityProviderStrategy = Lens.lens (\CreateService' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@CreateService' {} a -> s {capacityProviderStrategy = a} :: CreateService) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of instantiations of the specified task definition to place
-- and keep running on your cluster.
--
-- This is required if @schedulingStrategy@ is @REPLICA@ or is not
-- specified. If @schedulingStrategy@ is @DAEMON@ then this is not
-- required.
createService_desiredCount :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Int)
createService_desiredCount = Lens.lens (\CreateService' {desiredCount} -> desiredCount) (\s@CreateService' {} a -> s {desiredCount = a} :: CreateService)

-- | Specifies whether to enable Amazon ECS managed tags for the tasks within
-- the service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the /Amazon Elastic Container Service Developer Guide/.
createService_enableECSManagedTags :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Bool)
createService_enableECSManagedTags = Lens.lens (\CreateService' {enableECSManagedTags} -> enableECSManagedTags) (\s@CreateService' {} a -> s {enableECSManagedTags = a} :: CreateService)

-- | The launch type on which to run your service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter
-- must be omitted.
createService_launchType :: Lens.Lens' CreateService (Prelude.Maybe LaunchType)
createService_launchType = Lens.lens (\CreateService' {launchType} -> launchType) (\s@CreateService' {} a -> s {launchType = a} :: CreateService)

-- | The platform version that your tasks in the service are running on. A
-- platform version is specified only for tasks using the Fargate launch
-- type. If one isn\'t specified, the @LATEST@ platform version is used by
-- default. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
createService_platformVersion :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_platformVersion = Lens.lens (\CreateService' {platformVersion} -> platformVersion) (\s@CreateService' {} a -> s {platformVersion = a} :: CreateService)

-- | The deployment controller to use for the service.
createService_deploymentController :: Lens.Lens' CreateService (Prelude.Maybe DeploymentController)
createService_deploymentController = Lens.lens (\CreateService' {deploymentController} -> deploymentController) (\s@CreateService' {} a -> s {deploymentController = a} :: CreateService)

-- | The placement strategy objects to use for tasks in your service. You can
-- specify a maximum of five strategy rules per service.
createService_placementStrategy :: Lens.Lens' CreateService (Prelude.Maybe [PlacementStrategy])
createService_placementStrategy = Lens.lens (\CreateService' {placementStrategy} -> placementStrategy) (\s@CreateService' {} a -> s {placementStrategy = a} :: CreateService) Prelude.. Lens.mapping Prelude._Coerce

-- | An array of placement constraint objects to use for tasks in your
-- service. You can specify a maximum of 10 constraints per task (this
-- limit includes constraints in the task definition and those specified at
-- runtime).
createService_placementConstraints :: Lens.Lens' CreateService (Prelude.Maybe [PlacementConstraint])
createService_placementConstraints = Lens.lens (\CreateService' {placementConstraints} -> placementConstraints) (\s@CreateService' {} a -> s {placementConstraints = a} :: CreateService) Prelude.. Lens.mapping Prelude._Coerce

-- | The name or full Amazon Resource Name (ARN) of the IAM role that allows
-- Amazon ECS to make calls to your load balancer on your behalf. This
-- parameter is only permitted if you are using a load balancer with your
-- service and your task definition does not use the @awsvpc@ network mode.
-- If you specify the @role@ parameter, you must also specify a load
-- balancer object with the @loadBalancers@ parameter.
--
-- If your account has already created the Amazon ECS service-linked role,
-- that role is used by default for your service unless you specify a role
-- here. The service-linked role is required if your task definition uses
-- the @awsvpc@ network mode or if the service is configured to use service
-- discovery, an external deployment controller, multiple target groups, or
-- Elastic Inference accelerators in which case you should not specify a
-- role here. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If your specified role has a path other than @\/@, then you must either
-- specify the full role ARN (this is recommended) or prefix the role name
-- with the path. For example, if a role with the name @bar@ has a path of
-- @\/foo\/@ then you would specify @\/foo\/bar@ as the role name. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-friendly-names Friendly Names and Paths>
-- in the /IAM User Guide/.
createService_role :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_role = Lens.lens (\CreateService' {role'} -> role') (\s@CreateService' {} a -> s {role' = a} :: CreateService)

-- | A load balancer object representing the load balancers to use with your
-- service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-load-balancing.html Service Load Balancing>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If the service is using the rolling update (@ECS@) deployment controller
-- and using either an Application Load Balancer or Network Load Balancer,
-- you must specify one or more target group ARNs to attach to the service.
-- The service-linked role is required for services that make use of
-- multiple target groups. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If the service is using the @CODE_DEPLOY@ deployment controller, the
-- service is required to use either an Application Load Balancer or
-- Network Load Balancer. When creating an AWS CodeDeploy deployment group,
-- you specify two target groups (referred to as a @targetGroupPair@).
-- During a deployment, AWS CodeDeploy determines which task set in your
-- service has the status @PRIMARY@ and associates one target group with
-- it, and then associates the other target group with the replacement task
-- set. The load balancer can also have up to two listeners: a required
-- listener for production traffic and an optional listener that allows you
-- perform validation tests with Lambda functions before routing production
-- traffic to it.
--
-- After you create a service using the @ECS@ deployment controller, the
-- load balancer name or target group ARN, container name, and container
-- port specified in the service definition are immutable. If you are using
-- the @CODE_DEPLOY@ deployment controller, these values can be changed
-- when updating the service.
--
-- For Application Load Balancers and Network Load Balancers, this object
-- must contain the load balancer target group ARN, the container name (as
-- it appears in a container definition), and the container port to access
-- from the load balancer. The load balancer name parameter must be
-- omitted. When a task from this service is placed on a container
-- instance, the container instance and port combination is registered as a
-- target in the target group specified here.
--
-- For Classic Load Balancers, this object must contain the load balancer
-- name, the container name (as it appears in a container definition), and
-- the container port to access from the load balancer. The target group
-- ARN parameter must be omitted. When a task from this service is placed
-- on a container instance, the container instance is registered with the
-- load balancer specified here.
--
-- Services with tasks that use the @awsvpc@ network mode (for example,
-- those with the Fargate launch type) only support Application Load
-- Balancers and Network Load Balancers. Classic Load Balancers are not
-- supported. Also, when you create any target groups for these services,
-- you must choose @ip@ as the target type, not @instance@, because tasks
-- that use the @awsvpc@ network mode are associated with an elastic
-- network interface, not an Amazon EC2 instance.
createService_loadBalancers :: Lens.Lens' CreateService (Prelude.Maybe [LoadBalancer])
createService_loadBalancers = Lens.lens (\CreateService' {loadBalancers} -> loadBalancers) (\s@CreateService' {} a -> s {loadBalancers = a} :: CreateService) Prelude.. Lens.mapping Prelude._Coerce

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
--     such as a prefix for either keys or values as it is reserved for AWS
--     use. You cannot edit or delete tag keys or values with this prefix.
--     Tags with this prefix do not count against your tags per resource
--     limit.
createService_tags :: Lens.Lens' CreateService (Prelude.Maybe [Tag])
createService_tags = Lens.lens (\CreateService' {tags} -> tags) (\s@CreateService' {} a -> s {tags = a} :: CreateService) Prelude.. Lens.mapping Prelude._Coerce

-- | The details of the service discovery registries to assign to this
-- service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
--
-- Service discovery is supported for Fargate tasks if you are using
-- platform version v1.1.0 or later. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>.
createService_serviceRegistries :: Lens.Lens' CreateService (Prelude.Maybe [ServiceRegistry])
createService_serviceRegistries = Lens.lens (\CreateService' {serviceRegistries} -> serviceRegistries) (\s@CreateService' {} a -> s {serviceRegistries = a} :: CreateService) Prelude.. Lens.mapping Prelude._Coerce

-- | The period of time, in seconds, that the Amazon ECS service scheduler
-- should ignore unhealthy Elastic Load Balancing target health checks
-- after a task has first started. This is only used when your service is
-- configured to use a load balancer. If your service has a load balancer
-- defined and you don\'t specify a health check grace period value, the
-- default value of @0@ is used.
--
-- If your service\'s tasks take a while to start and respond to Elastic
-- Load Balancing health checks, you can specify a health check grace
-- period of up to 2,147,483,647 seconds. During that time, the Amazon ECS
-- service scheduler ignores health check status. This grace period can
-- prevent the service scheduler from marking tasks as unhealthy and
-- stopping them before they have time to come up.
createService_healthCheckGracePeriodSeconds :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Int)
createService_healthCheckGracePeriodSeconds = Lens.lens (\CreateService' {healthCheckGracePeriodSeconds} -> healthCheckGracePeriodSeconds) (\s@CreateService' {} a -> s {healthCheckGracePeriodSeconds = a} :: CreateService)

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
--     decisions. This scheduler strategy is required if the service is
--     using the @CODE_DEPLOY@ or @EXTERNAL@ deployment controller types.
--
-- -   @DAEMON@-The daemon scheduling strategy deploys exactly one task on
--     each active container instance that meets all of the task placement
--     constraints that you specify in your cluster. The service scheduler
--     also evaluates the task placement constraints for running tasks and
--     will stop tasks that do not meet the placement constraints. When
--     you\'re using this strategy, you don\'t need to specify a desired
--     number of tasks, a task placement strategy, or use Service Auto
--     Scaling policies.
--
--     Tasks using the Fargate launch type or the @CODE_DEPLOY@ or
--     @EXTERNAL@ deployment controller types don\'t support the @DAEMON@
--     scheduling strategy.
createService_schedulingStrategy :: Lens.Lens' CreateService (Prelude.Maybe SchedulingStrategy)
createService_schedulingStrategy = Lens.lens (\CreateService' {schedulingStrategy} -> schedulingStrategy) (\s@CreateService' {} a -> s {schedulingStrategy = a} :: CreateService)

-- | The @family@ and @revision@ (@family:revision@) or full ARN of the task
-- definition to run in your service. If a @revision@ is not specified, the
-- latest @ACTIVE@ revision is used.
--
-- A task definition must be specified if the service is using either the
-- @ECS@ or @CODE_DEPLOY@ deployment controllers.
createService_taskDefinition :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_taskDefinition = Lens.lens (\CreateService' {taskDefinition} -> taskDefinition) (\s@CreateService' {} a -> s {taskDefinition = a} :: CreateService)

-- | The short name or full Amazon Resource Name (ARN) of the cluster on
-- which to run your service. If you do not specify a cluster, the default
-- cluster is assumed.
createService_cluster :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_cluster = Lens.lens (\CreateService' {cluster} -> cluster) (\s@CreateService' {} a -> s {cluster = a} :: CreateService)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 32 ASCII characters are allowed.
createService_clientToken :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_clientToken = Lens.lens (\CreateService' {clientToken} -> clientToken) (\s@CreateService' {} a -> s {clientToken = a} :: CreateService)

-- | Specifies whether to propagate the tags from the task definition or the
-- service to the tasks in the service. If no value is specified, the tags
-- are not propagated. Tags can only be propagated to the tasks within the
-- service during service creation. To add tags to a task after service
-- creation, use the TagResource API action.
createService_propagateTags :: Lens.Lens' CreateService (Prelude.Maybe PropagateTags)
createService_propagateTags = Lens.lens (\CreateService' {propagateTags} -> propagateTags) (\s@CreateService' {} a -> s {propagateTags = a} :: CreateService)

-- | The name of your service. Up to 255 letters (uppercase and lowercase),
-- numbers, and hyphens are allowed. Service names must be unique within a
-- cluster, but you can have similarly named services in multiple clusters
-- within a Region or across multiple Regions.
createService_serviceName :: Lens.Lens' CreateService Prelude.Text
createService_serviceName = Lens.lens (\CreateService' {serviceName} -> serviceName) (\s@CreateService' {} a -> s {serviceName = a} :: CreateService)

instance Prelude.AWSRequest CreateService where
  type Rs CreateService = CreateServiceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceResponse'
            Prelude.<$> (x Prelude..?> "service")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateService

instance Prelude.NFData CreateService

instance Prelude.ToHeaders CreateService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerServiceV20141113.CreateService" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateService where
  toJSON CreateService' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("deploymentConfiguration" Prelude..=)
              Prelude.<$> deploymentConfiguration,
            ("networkConfiguration" Prelude..=)
              Prelude.<$> networkConfiguration,
            ("capacityProviderStrategy" Prelude..=)
              Prelude.<$> capacityProviderStrategy,
            ("desiredCount" Prelude..=) Prelude.<$> desiredCount,
            ("enableECSManagedTags" Prelude..=)
              Prelude.<$> enableECSManagedTags,
            ("launchType" Prelude..=) Prelude.<$> launchType,
            ("platformVersion" Prelude..=)
              Prelude.<$> platformVersion,
            ("deploymentController" Prelude..=)
              Prelude.<$> deploymentController,
            ("placementStrategy" Prelude..=)
              Prelude.<$> placementStrategy,
            ("placementConstraints" Prelude..=)
              Prelude.<$> placementConstraints,
            ("role" Prelude..=) Prelude.<$> role',
            ("loadBalancers" Prelude..=)
              Prelude.<$> loadBalancers,
            ("tags" Prelude..=) Prelude.<$> tags,
            ("serviceRegistries" Prelude..=)
              Prelude.<$> serviceRegistries,
            ("healthCheckGracePeriodSeconds" Prelude..=)
              Prelude.<$> healthCheckGracePeriodSeconds,
            ("schedulingStrategy" Prelude..=)
              Prelude.<$> schedulingStrategy,
            ("taskDefinition" Prelude..=)
              Prelude.<$> taskDefinition,
            ("cluster" Prelude..=) Prelude.<$> cluster,
            ("clientToken" Prelude..=) Prelude.<$> clientToken,
            ("propagateTags" Prelude..=)
              Prelude.<$> propagateTags,
            Prelude.Just ("serviceName" Prelude..= serviceName)
          ]
      )

instance Prelude.ToPath CreateService where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServiceResponse' smart constructor.
data CreateServiceResponse = CreateServiceResponse'
  { -- | The full description of your service following the create call.
    --
    -- If a service is using the @ECS@ deployment controller, the
    -- @deploymentController@ and @taskSets@ parameters will not be returned.
    --
    -- If the service is using the @CODE_DEPLOY@ deployment controller, the
    -- @deploymentController@, @taskSets@ and @deployments@ parameters will be
    -- returned, however the @deployments@ parameter will be an empty list.
    service :: Prelude.Maybe ContainerService,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- If a service is using the @ECS@ deployment controller, the
-- @deploymentController@ and @taskSets@ parameters will not be returned.
--
-- If the service is using the @CODE_DEPLOY@ deployment controller, the
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
-- If a service is using the @ECS@ deployment controller, the
-- @deploymentController@ and @taskSets@ parameters will not be returned.
--
-- If the service is using the @CODE_DEPLOY@ deployment controller, the
-- @deploymentController@, @taskSets@ and @deployments@ parameters will be
-- returned, however the @deployments@ parameter will be an empty list.
createServiceResponse_service :: Lens.Lens' CreateServiceResponse (Prelude.Maybe ContainerService)
createServiceResponse_service = Lens.lens (\CreateServiceResponse' {service} -> service) (\s@CreateServiceResponse' {} a -> s {service = a} :: CreateServiceResponse)

-- | The response's http status code.
createServiceResponse_httpStatus :: Lens.Lens' CreateServiceResponse Prelude.Int
createServiceResponse_httpStatus = Lens.lens (\CreateServiceResponse' {httpStatus} -> httpStatus) (\s@CreateServiceResponse' {} a -> s {httpStatus = a} :: CreateServiceResponse)

instance Prelude.NFData CreateServiceResponse
