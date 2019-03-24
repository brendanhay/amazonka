{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.CreateService
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs and maintains a desired number of tasks from a specified task definition. If the number of tasks running in a service drops below @desiredCount@ , Amazon ECS spawns another copy of the task in the specified cluster. To update an existing service, see 'UpdateService' .
--
--
-- In addition to maintaining the desired count of tasks in your service, you can optionally run your service behind a load balancer. The load balancer distributes traffic across the tasks that are associated with the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-load-balancing.html Service Load Balancing> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- You can optionally specify a deployment configuration for your service. The deployment is triggered by changing properties, such as the task definition or the desired count of a service, with an 'UpdateService' operation.
--
-- If a service is using the @ECS@ deployment controller, the __minimum healthy percent__ represents a lower limit on the number of tasks in a service that must remain in the @RUNNING@ state during a deployment, as a percentage of the desired number of tasks (rounded up to the nearest integer), and while any container instances are in the @DRAINING@ state if the service contains tasks using the EC2 launch type. This parameter enables you to deploy without using additional cluster capacity. For example, if your service has a desired number of four tasks and a minimum healthy percent of 50%, the scheduler may stop two existing tasks to free up cluster capacity before starting two new tasks. Tasks for services that /do not/ use a load balancer are considered healthy if they are in the @RUNNING@ state; tasks for services that /do/ use a load balancer are considered healthy if they are in the @RUNNING@ state and they are reported as healthy by the load balancer. The default value for minimum healthy percent is 100%.
--
-- If a service is using the @ECS@ deployment controller, the __maximum percent__ parameter represents an upper limit on the number of tasks in a service that are allowed in the @RUNNING@ or @PENDING@ state during a deployment, as a percentage of the desired number of tasks (rounded down to the nearest integer), and while any container instances are in the @DRAINING@ state if the service contains tasks using the EC2 launch type. This parameter enables you to define the deployment batch size. For example, if your service has a desired number of four tasks and a maximum percent value of 200%, the scheduler may start four new tasks before stopping the four older tasks (provided that the cluster resources required to do this are available). The default value for maximum percent is 200%.
--
-- If a service is using the @CODE_DEPLOY@ deployment controller and tasks that use the EC2 launch type, the __minimum healthy percent__ and __maximum percent__ values are only used to define the lower and upper limit on the number of the tasks in the service that remain in the @RUNNING@ state while the container instances are in the @DRAINING@ state. If the tasks in the service use the Fargate launch type, the minimum healthy percent and maximum percent values are not used, although they are currently visible when describing your service.
--
-- Tasks for services that /do not/ use a load balancer are considered healthy if they are in the @RUNNING@ state. Tasks for services that /do/ use a load balancer are considered healthy if they are in the @RUNNING@ state and the container instance they are hosted on is reported as healthy by the load balancer. The default value for a replica service for @minimumHealthyPercent@ is 100%. The default value for a daemon service for @minimumHealthyPercent@ is 0%.
--
-- When the service scheduler launches new tasks, it determines task placement in your cluster using the following logic:
--
--     * Determine which of the container instances in your cluster can support your service's task definition (for example, they have the required CPU, memory, ports, and container instance attributes).
--
--     * By default, the service scheduler attempts to balance tasks across Availability Zones in this manner (although you can choose a different placement strategy) with the @placementStrategy@ parameter):
--
--     * Sort the valid container instances, giving priority to instances that have the fewest number of running tasks for this service in their respective Availability Zone. For example, if zone A has one running service task and zones B and C each have zero, valid container instances in either zone B or C are considered optimal for placement.
--
--     * Place the new service task on a valid container instance in an optimal Availability Zone (based on the previous steps), favoring container instances with the fewest number of running tasks for this service.
--
--
--
--
--
module Network.AWS.ECS.CreateService
    (
    -- * Creating a Request
      createService
    , CreateService
    -- * Request Lenses
    , creCluster
    , creClientToken
    , crePropagateTags
    , crePlatformVersion
    , creEnableECSManagedTags
    , creDesiredCount
    , creLoadBalancers
    , creRole
    , crePlacementConstraints
    , crePlacementStrategy
    , creDeploymentController
    , creLaunchType
    , creSchedulingStrategy
    , creHealthCheckGracePeriodSeconds
    , creNetworkConfiguration
    , creServiceRegistries
    , creTags
    , creDeploymentConfiguration
    , creServiceName
    , creTaskDefinition

    -- * Destructuring the Response
    , createServiceResponse
    , CreateServiceResponse
    -- * Response Lenses
    , csrsService
    , csrsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createService' smart constructor.
data CreateService = CreateService'
  { _creCluster                       :: !(Maybe Text)
  , _creClientToken                   :: !(Maybe Text)
  , _crePropagateTags                 :: !(Maybe PropagateTags)
  , _crePlatformVersion               :: !(Maybe Text)
  , _creEnableECSManagedTags          :: !(Maybe Bool)
  , _creDesiredCount                  :: !(Maybe Int)
  , _creLoadBalancers                 :: !(Maybe [LoadBalancer])
  , _creRole                          :: !(Maybe Text)
  , _crePlacementConstraints          :: !(Maybe [PlacementConstraint])
  , _crePlacementStrategy             :: !(Maybe [PlacementStrategy])
  , _creDeploymentController          :: !(Maybe DeploymentController)
  , _creLaunchType                    :: !(Maybe LaunchType)
  , _creSchedulingStrategy            :: !(Maybe SchedulingStrategy)
  , _creHealthCheckGracePeriodSeconds :: !(Maybe Int)
  , _creNetworkConfiguration          :: !(Maybe NetworkConfiguration)
  , _creServiceRegistries             :: !(Maybe [ServiceRegistry])
  , _creTags                          :: !(Maybe [Tag])
  , _creDeploymentConfiguration       :: !(Maybe DeploymentConfiguration)
  , _creServiceName                   :: !Text
  , _creTaskDefinition                :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'creCluster' - The short name or full Amazon Resource Name (ARN) of the cluster on which to run your service. If you do not specify a cluster, the default cluster is assumed.
--
-- * 'creClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 32 ASCII characters are allowed.
--
-- * 'crePropagateTags' - Specifies whether to propagate the tags from the task definition or the service to the tasks in the service. If no value is specified, the tags are not propagated. Tags can only be propagated to the tasks within the service during service creation. To add tags to a task after service creation, use the 'TagResource' API action.
--
-- * 'crePlatformVersion' - The platform version on which your tasks in the service are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'creEnableECSManagedTags' - Specifies whether to enable Amazon ECS managed tags for the tasks within the service. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'creDesiredCount' - The number of instantiations of the specified task definition to place and keep running on your cluster.
--
-- * 'creLoadBalancers' - A load balancer object representing the load balancer to use with your service. If the service is using the @ECS@ deployment controller, you are limited to one load balancer or target group. If the service is using the @CODE_DEPLOY@ deployment controller, the service is required to use either an Application Load Balancer or Network Load Balancer. When creating an AWS CodeDeploy deployment group, you specify two target groups (referred to as a @targetGroupPair@ ). During a deployment, AWS CodeDeploy determines which task set in your service has the status @PRIMARY@ and associates one target group with it, and then associates the other target group with the replacement task set. The load balancer can also have up to two listeners: a required listener for production traffic and an optional listener that allows you perform validation tests with Lambda functions before routing production traffic to it. After you create a service using the @ECS@ deployment controller, the load balancer name or target group ARN, container name, and container port specified in the service definition are immutable. If you are using the @CODE_DEPLOY@ deployment controller, these values can be changed when updating the service. For Classic Load Balancers, this object must contain the load balancer name, the container name (as it appears in a container definition), and the container port to access from the load balancer. When a task from this service is placed on a container instance, the container instance is registered with the load balancer specified here. For Application Load Balancers and Network Load Balancers, this object must contain the load balancer target group ARN, the container name (as it appears in a container definition), and the container port to access from the load balancer. When a task from this service is placed on a container instance, the container instance and port combination is registered as a target in the target group specified here. Services with tasks that use the @awsvpc@ network mode (for example, those with the Fargate launch type) only support Application Load Balancers and Network Load Balancers. Classic Load Balancers are not supported. Also, when you create any target groups for these services, you must choose @ip@ as the target type, not @instance@ , because tasks that use the @awsvpc@ network mode are associated with an elastic network interface, not an Amazon EC2 instance.
--
-- * 'creRole' - The name or full Amazon Resource Name (ARN) of the IAM role that allows Amazon ECS to make calls to your load balancer on your behalf. This parameter is only permitted if you are using a load balancer with your service and your task definition does not use the @awsvpc@ network mode. If you specify the @role@ parameter, you must also specify a load balancer object with the @loadBalancers@ parameter. /Important:/ If your account has already created the Amazon ECS service-linked role, that role is used by default for your service unless you specify a role here. The service-linked role is required if your task definition uses the @awsvpc@ network mode, in which case you should not specify a role here. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/ . If your specified role has a path other than @/@ , then you must either specify the full role ARN (this is recommended) or prefix the role name with the path. For example, if a role with the name @bar@ has a path of @/foo/@ then you would specify @/foo/bar@ as the role name. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-friendly-names Friendly Names and Paths> in the /IAM User Guide/ .
--
-- * 'crePlacementConstraints' - An array of placement constraint objects to use for tasks in your service. You can specify a maximum of 10 constraints per task (this limit includes constraints in the task definition and those specified at runtime).
--
-- * 'crePlacementStrategy' - The placement strategy objects to use for tasks in your service. You can specify a maximum of five strategy rules per service.
--
-- * 'creDeploymentController' - The deployment controller to use for the service.
--
-- * 'creLaunchType' - The launch type on which to run your service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'creSchedulingStrategy' - The scheduling strategy to use for the service. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services> . There are two service scheduler strategies available:     * @REPLICA@ -The replica scheduling strategy places and maintains the desired number of tasks across your cluster. By default, the service scheduler spreads tasks across Availability Zones. You can use task placement strategies and constraints to customize task placement decisions. This scheduler strategy is required if using the @CODE_DEPLOY@ deployment controller.     * @DAEMON@ -The daemon scheduling strategy deploys exactly one task on each active container instance that meets all of the task placement constraints that you specify in your cluster. When you are using this strategy, there is no need to specify a desired number of tasks, a task placement strategy, or use Service Auto Scaling policies.
--
-- * 'creHealthCheckGracePeriodSeconds' - The period of time, in seconds, that the Amazon ECS service scheduler should ignore unhealthy Elastic Load Balancing target health checks after a task has first started. This is only valid if your service is configured to use a load balancer. If your service's tasks take a while to start and respond to Elastic Load Balancing health checks, you can specify a health check grace period of up to 2,147,483,647 seconds. During that time, the ECS service scheduler ignores health check status. This grace period can prevent the ECS service scheduler from marking tasks as unhealthy and stopping them before they have time to come up.
--
-- * 'creNetworkConfiguration' - The network configuration for the service. This parameter is required for task definitions that use the @awsvpc@ network mode to receive their own elastic network interface, and it is not supported for other network modes. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'creServiceRegistries' - The details of the service discovery registries to assign to this service. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
--
-- * 'creTags' - The metadata that you apply to the service to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. When a service is deleted, the tags are deleted as well. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
--
-- * 'creDeploymentConfiguration' - Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
--
-- * 'creServiceName' - The name of your service. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. Service names must be unique within a cluster, but you can have similarly named services in multiple clusters within a Region or across multiple Regions.
--
-- * 'creTaskDefinition' - The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to run in your service. If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
createService
    :: Text -- ^ 'creServiceName'
    -> Text -- ^ 'creTaskDefinition'
    -> CreateService
createService pServiceName_ pTaskDefinition_ =
  CreateService'
    { _creCluster = Nothing
    , _creClientToken = Nothing
    , _crePropagateTags = Nothing
    , _crePlatformVersion = Nothing
    , _creEnableECSManagedTags = Nothing
    , _creDesiredCount = Nothing
    , _creLoadBalancers = Nothing
    , _creRole = Nothing
    , _crePlacementConstraints = Nothing
    , _crePlacementStrategy = Nothing
    , _creDeploymentController = Nothing
    , _creLaunchType = Nothing
    , _creSchedulingStrategy = Nothing
    , _creHealthCheckGracePeriodSeconds = Nothing
    , _creNetworkConfiguration = Nothing
    , _creServiceRegistries = Nothing
    , _creTags = Nothing
    , _creDeploymentConfiguration = Nothing
    , _creServiceName = pServiceName_
    , _creTaskDefinition = pTaskDefinition_
    }


-- | The short name or full Amazon Resource Name (ARN) of the cluster on which to run your service. If you do not specify a cluster, the default cluster is assumed.
creCluster :: Lens' CreateService (Maybe Text)
creCluster = lens _creCluster (\ s a -> s{_creCluster = a})

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 32 ASCII characters are allowed.
creClientToken :: Lens' CreateService (Maybe Text)
creClientToken = lens _creClientToken (\ s a -> s{_creClientToken = a})

-- | Specifies whether to propagate the tags from the task definition or the service to the tasks in the service. If no value is specified, the tags are not propagated. Tags can only be propagated to the tasks within the service during service creation. To add tags to a task after service creation, use the 'TagResource' API action.
crePropagateTags :: Lens' CreateService (Maybe PropagateTags)
crePropagateTags = lens _crePropagateTags (\ s a -> s{_crePropagateTags = a})

-- | The platform version on which your tasks in the service are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
crePlatformVersion :: Lens' CreateService (Maybe Text)
crePlatformVersion = lens _crePlatformVersion (\ s a -> s{_crePlatformVersion = a})

-- | Specifies whether to enable Amazon ECS managed tags for the tasks within the service. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
creEnableECSManagedTags :: Lens' CreateService (Maybe Bool)
creEnableECSManagedTags = lens _creEnableECSManagedTags (\ s a -> s{_creEnableECSManagedTags = a})

-- | The number of instantiations of the specified task definition to place and keep running on your cluster.
creDesiredCount :: Lens' CreateService (Maybe Int)
creDesiredCount = lens _creDesiredCount (\ s a -> s{_creDesiredCount = a})

-- | A load balancer object representing the load balancer to use with your service. If the service is using the @ECS@ deployment controller, you are limited to one load balancer or target group. If the service is using the @CODE_DEPLOY@ deployment controller, the service is required to use either an Application Load Balancer or Network Load Balancer. When creating an AWS CodeDeploy deployment group, you specify two target groups (referred to as a @targetGroupPair@ ). During a deployment, AWS CodeDeploy determines which task set in your service has the status @PRIMARY@ and associates one target group with it, and then associates the other target group with the replacement task set. The load balancer can also have up to two listeners: a required listener for production traffic and an optional listener that allows you perform validation tests with Lambda functions before routing production traffic to it. After you create a service using the @ECS@ deployment controller, the load balancer name or target group ARN, container name, and container port specified in the service definition are immutable. If you are using the @CODE_DEPLOY@ deployment controller, these values can be changed when updating the service. For Classic Load Balancers, this object must contain the load balancer name, the container name (as it appears in a container definition), and the container port to access from the load balancer. When a task from this service is placed on a container instance, the container instance is registered with the load balancer specified here. For Application Load Balancers and Network Load Balancers, this object must contain the load balancer target group ARN, the container name (as it appears in a container definition), and the container port to access from the load balancer. When a task from this service is placed on a container instance, the container instance and port combination is registered as a target in the target group specified here. Services with tasks that use the @awsvpc@ network mode (for example, those with the Fargate launch type) only support Application Load Balancers and Network Load Balancers. Classic Load Balancers are not supported. Also, when you create any target groups for these services, you must choose @ip@ as the target type, not @instance@ , because tasks that use the @awsvpc@ network mode are associated with an elastic network interface, not an Amazon EC2 instance.
creLoadBalancers :: Lens' CreateService [LoadBalancer]
creLoadBalancers = lens _creLoadBalancers (\ s a -> s{_creLoadBalancers = a}) . _Default . _Coerce

-- | The name or full Amazon Resource Name (ARN) of the IAM role that allows Amazon ECS to make calls to your load balancer on your behalf. This parameter is only permitted if you are using a load balancer with your service and your task definition does not use the @awsvpc@ network mode. If you specify the @role@ parameter, you must also specify a load balancer object with the @loadBalancers@ parameter. /Important:/ If your account has already created the Amazon ECS service-linked role, that role is used by default for your service unless you specify a role here. The service-linked role is required if your task definition uses the @awsvpc@ network mode, in which case you should not specify a role here. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/ . If your specified role has a path other than @/@ , then you must either specify the full role ARN (this is recommended) or prefix the role name with the path. For example, if a role with the name @bar@ has a path of @/foo/@ then you would specify @/foo/bar@ as the role name. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-friendly-names Friendly Names and Paths> in the /IAM User Guide/ .
creRole :: Lens' CreateService (Maybe Text)
creRole = lens _creRole (\ s a -> s{_creRole = a})

-- | An array of placement constraint objects to use for tasks in your service. You can specify a maximum of 10 constraints per task (this limit includes constraints in the task definition and those specified at runtime).
crePlacementConstraints :: Lens' CreateService [PlacementConstraint]
crePlacementConstraints = lens _crePlacementConstraints (\ s a -> s{_crePlacementConstraints = a}) . _Default . _Coerce

-- | The placement strategy objects to use for tasks in your service. You can specify a maximum of five strategy rules per service.
crePlacementStrategy :: Lens' CreateService [PlacementStrategy]
crePlacementStrategy = lens _crePlacementStrategy (\ s a -> s{_crePlacementStrategy = a}) . _Default . _Coerce

-- | The deployment controller to use for the service.
creDeploymentController :: Lens' CreateService (Maybe DeploymentController)
creDeploymentController = lens _creDeploymentController (\ s a -> s{_creDeploymentController = a})

-- | The launch type on which to run your service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
creLaunchType :: Lens' CreateService (Maybe LaunchType)
creLaunchType = lens _creLaunchType (\ s a -> s{_creLaunchType = a})

-- | The scheduling strategy to use for the service. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services> . There are two service scheduler strategies available:     * @REPLICA@ -The replica scheduling strategy places and maintains the desired number of tasks across your cluster. By default, the service scheduler spreads tasks across Availability Zones. You can use task placement strategies and constraints to customize task placement decisions. This scheduler strategy is required if using the @CODE_DEPLOY@ deployment controller.     * @DAEMON@ -The daemon scheduling strategy deploys exactly one task on each active container instance that meets all of the task placement constraints that you specify in your cluster. When you are using this strategy, there is no need to specify a desired number of tasks, a task placement strategy, or use Service Auto Scaling policies.
creSchedulingStrategy :: Lens' CreateService (Maybe SchedulingStrategy)
creSchedulingStrategy = lens _creSchedulingStrategy (\ s a -> s{_creSchedulingStrategy = a})

-- | The period of time, in seconds, that the Amazon ECS service scheduler should ignore unhealthy Elastic Load Balancing target health checks after a task has first started. This is only valid if your service is configured to use a load balancer. If your service's tasks take a while to start and respond to Elastic Load Balancing health checks, you can specify a health check grace period of up to 2,147,483,647 seconds. During that time, the ECS service scheduler ignores health check status. This grace period can prevent the ECS service scheduler from marking tasks as unhealthy and stopping them before they have time to come up.
creHealthCheckGracePeriodSeconds :: Lens' CreateService (Maybe Int)
creHealthCheckGracePeriodSeconds = lens _creHealthCheckGracePeriodSeconds (\ s a -> s{_creHealthCheckGracePeriodSeconds = a})

-- | The network configuration for the service. This parameter is required for task definitions that use the @awsvpc@ network mode to receive their own elastic network interface, and it is not supported for other network modes. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
creNetworkConfiguration :: Lens' CreateService (Maybe NetworkConfiguration)
creNetworkConfiguration = lens _creNetworkConfiguration (\ s a -> s{_creNetworkConfiguration = a})

-- | The details of the service discovery registries to assign to this service. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
creServiceRegistries :: Lens' CreateService [ServiceRegistry]
creServiceRegistries = lens _creServiceRegistries (\ s a -> s{_creServiceRegistries = a}) . _Default . _Coerce

-- | The metadata that you apply to the service to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. When a service is deleted, the tags are deleted as well. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
creTags :: Lens' CreateService [Tag]
creTags = lens _creTags (\ s a -> s{_creTags = a}) . _Default . _Coerce

-- | Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
creDeploymentConfiguration :: Lens' CreateService (Maybe DeploymentConfiguration)
creDeploymentConfiguration = lens _creDeploymentConfiguration (\ s a -> s{_creDeploymentConfiguration = a})

-- | The name of your service. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. Service names must be unique within a cluster, but you can have similarly named services in multiple clusters within a Region or across multiple Regions.
creServiceName :: Lens' CreateService Text
creServiceName = lens _creServiceName (\ s a -> s{_creServiceName = a})

-- | The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to run in your service. If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
creTaskDefinition :: Lens' CreateService Text
creTaskDefinition = lens _creTaskDefinition (\ s a -> s{_creTaskDefinition = a})

instance AWSRequest CreateService where
        type Rs CreateService = CreateServiceResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 CreateServiceResponse' <$>
                   (x .?> "service") <*> (pure (fromEnum s)))

instance Hashable CreateService where

instance NFData CreateService where

instance ToHeaders CreateService where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.CreateService"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateService where
        toJSON CreateService'{..}
          = object
              (catMaybes
                 [("cluster" .=) <$> _creCluster,
                  ("clientToken" .=) <$> _creClientToken,
                  ("propagateTags" .=) <$> _crePropagateTags,
                  ("platformVersion" .=) <$> _crePlatformVersion,
                  ("enableECSManagedTags" .=) <$>
                    _creEnableECSManagedTags,
                  ("desiredCount" .=) <$> _creDesiredCount,
                  ("loadBalancers" .=) <$> _creLoadBalancers,
                  ("role" .=) <$> _creRole,
                  ("placementConstraints" .=) <$>
                    _crePlacementConstraints,
                  ("placementStrategy" .=) <$> _crePlacementStrategy,
                  ("deploymentController" .=) <$>
                    _creDeploymentController,
                  ("launchType" .=) <$> _creLaunchType,
                  ("schedulingStrategy" .=) <$> _creSchedulingStrategy,
                  ("healthCheckGracePeriodSeconds" .=) <$>
                    _creHealthCheckGracePeriodSeconds,
                  ("networkConfiguration" .=) <$>
                    _creNetworkConfiguration,
                  ("serviceRegistries" .=) <$> _creServiceRegistries,
                  ("tags" .=) <$> _creTags,
                  ("deploymentConfiguration" .=) <$>
                    _creDeploymentConfiguration,
                  Just ("serviceName" .= _creServiceName),
                  Just ("taskDefinition" .= _creTaskDefinition)])

instance ToPath CreateService where
        toPath = const "/"

instance ToQuery CreateService where
        toQuery = const mempty

-- | /See:/ 'createServiceResponse' smart constructor.
data CreateServiceResponse = CreateServiceResponse'
  { _csrsService        :: !(Maybe ContainerService)
  , _csrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateServiceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsService' - The full description of your service following the create call. If a service is using the @ECS@ deployment controller, the @deploymentController@ and @taskSets@ parameters will not be returned. If the service is using the @CODE_DEPLOY@ deployment controller, the @deploymentController@ , @taskSets@ and @deployments@ parameters will be returned, however the @deployments@ parameter will be an empty list.
--
-- * 'csrsResponseStatus' - -- | The response status code.
createServiceResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> CreateServiceResponse
createServiceResponse pResponseStatus_ =
  CreateServiceResponse'
    {_csrsService = Nothing, _csrsResponseStatus = pResponseStatus_}


-- | The full description of your service following the create call. If a service is using the @ECS@ deployment controller, the @deploymentController@ and @taskSets@ parameters will not be returned. If the service is using the @CODE_DEPLOY@ deployment controller, the @deploymentController@ , @taskSets@ and @deployments@ parameters will be returned, however the @deployments@ parameter will be an empty list.
csrsService :: Lens' CreateServiceResponse (Maybe ContainerService)
csrsService = lens _csrsService (\ s a -> s{_csrsService = a})

-- | -- | The response status code.
csrsResponseStatus :: Lens' CreateServiceResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a})

instance NFData CreateServiceResponse where
