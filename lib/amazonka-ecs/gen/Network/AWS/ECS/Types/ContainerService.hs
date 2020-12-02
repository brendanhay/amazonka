{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerService where

import Network.AWS.ECS.Types.CapacityProviderStrategyItem
import Network.AWS.ECS.Types.Deployment
import Network.AWS.ECS.Types.DeploymentConfiguration
import Network.AWS.ECS.Types.DeploymentController
import Network.AWS.ECS.Types.LaunchType
import Network.AWS.ECS.Types.LoadBalancer
import Network.AWS.ECS.Types.NetworkConfiguration
import Network.AWS.ECS.Types.PlacementConstraint
import Network.AWS.ECS.Types.PlacementStrategy
import Network.AWS.ECS.Types.PropagateTags
import Network.AWS.ECS.Types.SchedulingStrategy
import Network.AWS.ECS.Types.ServiceEvent
import Network.AWS.ECS.Types.ServiceRegistry
import Network.AWS.ECS.Types.Tag
import Network.AWS.ECS.Types.TaskSet
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details on a service within a cluster
--
--
--
-- /See:/ 'containerService' smart constructor.
data ContainerService = ContainerService'
  { _csTaskSets ::
      !(Maybe [TaskSet]),
    _csRunningCount :: !(Maybe Int),
    _csStatus :: !(Maybe Text),
    _csClusterARN :: !(Maybe Text),
    _csPropagateTags :: !(Maybe PropagateTags),
    _csCreatedAt :: !(Maybe POSIX),
    _csPlatformVersion :: !(Maybe Text),
    _csEnableECSManagedTags :: !(Maybe Bool),
    _csCreatedBy :: !(Maybe Text),
    _csDesiredCount :: !(Maybe Int),
    _csLoadBalancers :: !(Maybe [LoadBalancer]),
    _csPendingCount :: !(Maybe Int),
    _csPlacementConstraints :: !(Maybe [PlacementConstraint]),
    _csEvents :: !(Maybe [ServiceEvent]),
    _csPlacementStrategy :: !(Maybe [PlacementStrategy]),
    _csDeployments :: !(Maybe [Deployment]),
    _csServiceName :: !(Maybe Text),
    _csDeploymentController :: !(Maybe DeploymentController),
    _csLaunchType :: !(Maybe LaunchType),
    _csServiceARN :: !(Maybe Text),
    _csTaskDefinition :: !(Maybe Text),
    _csSchedulingStrategy :: !(Maybe SchedulingStrategy),
    _csHealthCheckGracePeriodSeconds :: !(Maybe Int),
    _csNetworkConfiguration :: !(Maybe NetworkConfiguration),
    _csServiceRegistries :: !(Maybe [ServiceRegistry]),
    _csCapacityProviderStrategy ::
      !(Maybe [CapacityProviderStrategyItem]),
    _csTags :: !(Maybe [Tag]),
    _csRoleARN :: !(Maybe Text),
    _csDeploymentConfiguration ::
      !(Maybe DeploymentConfiguration)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContainerService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csTaskSets' - Information about a set of Amazon ECS tasks in either an AWS CodeDeploy or an @EXTERNAL@ deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic.
--
-- * 'csRunningCount' - The number of tasks in the cluster that are in the @RUNNING@ state.
--
-- * 'csStatus' - The status of the service. The valid values are @ACTIVE@ , @DRAINING@ , or @INACTIVE@ .
--
-- * 'csClusterARN' - The Amazon Resource Name (ARN) of the cluster that hosts the service.
--
-- * 'csPropagateTags' - Specifies whether to propagate the tags from the task definition or the service to the task. If no value is specified, the tags are not propagated.
--
-- * 'csCreatedAt' - The Unix timestamp for when the service was created.
--
-- * 'csPlatformVersion' - The platform version on which to run your service. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'csEnableECSManagedTags' - Specifies whether to enable Amazon ECS managed tags for the tasks in the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'csCreatedBy' - The principal that created the service.
--
-- * 'csDesiredCount' - The desired number of instantiations of the task definition to keep running on the service. This value is specified when the service is created with 'CreateService' , and it can be modified with 'UpdateService' .
--
-- * 'csLoadBalancers' - A list of Elastic Load Balancing load balancer objects, containing the load balancer name, the container name (as it appears in a container definition), and the container port to access from the load balancer.
--
-- * 'csPendingCount' - The number of tasks in the cluster that are in the @PENDING@ state.
--
-- * 'csPlacementConstraints' - The placement constraints for the tasks in the service.
--
-- * 'csEvents' - The event stream for your service. A maximum of 100 of the latest events are displayed.
--
-- * 'csPlacementStrategy' - The placement strategy that determines how tasks for the service are placed.
--
-- * 'csDeployments' - The current state of deployments for the service.
--
-- * 'csServiceName' - The name of your service. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. Service names must be unique within a cluster, but you can have similarly named services in multiple clusters within a Region or across multiple Regions.
--
-- * 'csDeploymentController' - The deployment controller type the service is using. When using the DescribeServices API, this field is omitted if the service is using the @ECS@ deployment controller type.
--
-- * 'csLaunchType' - The launch type on which your service is running. If no value is specified, it will default to @EC2@ . Valid values include @EC2@ and @FARGATE@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'csServiceARN' - The ARN that identifies the service. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the service, the AWS account ID of the service owner, the @service@ namespace, and then the service name. For example, @arn:aws:ecs:region:012345678910:service/my-service@ .
--
-- * 'csTaskDefinition' - The task definition to use for tasks in the service. This value is specified when the service is created with 'CreateService' , and it can be modified with 'UpdateService' .
--
-- * 'csSchedulingStrategy' - The scheduling strategy to use for the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services> . There are two service scheduler strategies available:     * @REPLICA@ -The replica scheduling strategy places and maintains the desired number of tasks across your cluster. By default, the service scheduler spreads tasks across Availability Zones. You can use task placement strategies and constraints to customize task placement decisions.     * @DAEMON@ -The daemon scheduling strategy deploys exactly one task on each active container instance that meets all of the task placement constraints that you specify in your cluster. The service scheduler also evaluates the task placement constraints for running tasks and will stop tasks that do not meet the placement constraints.
--
-- * 'csHealthCheckGracePeriodSeconds' - The period of time, in seconds, that the Amazon ECS service scheduler ignores unhealthy Elastic Load Balancing target health checks after a task has first started.
--
-- * 'csNetworkConfiguration' - The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
--
-- * 'csServiceRegistries' - The details of the service discovery registries to assign to this service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
--
-- * 'csCapacityProviderStrategy' - The capacity provider strategy associated with the service.
--
-- * 'csTags' - The metadata that you apply to the service to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
--
-- * 'csRoleARN' - The ARN of the IAM role associated with the service that allows the Amazon ECS container agent to register container instances with an Elastic Load Balancing load balancer.
--
-- * 'csDeploymentConfiguration' - Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
containerService ::
  ContainerService
containerService =
  ContainerService'
    { _csTaskSets = Nothing,
      _csRunningCount = Nothing,
      _csStatus = Nothing,
      _csClusterARN = Nothing,
      _csPropagateTags = Nothing,
      _csCreatedAt = Nothing,
      _csPlatformVersion = Nothing,
      _csEnableECSManagedTags = Nothing,
      _csCreatedBy = Nothing,
      _csDesiredCount = Nothing,
      _csLoadBalancers = Nothing,
      _csPendingCount = Nothing,
      _csPlacementConstraints = Nothing,
      _csEvents = Nothing,
      _csPlacementStrategy = Nothing,
      _csDeployments = Nothing,
      _csServiceName = Nothing,
      _csDeploymentController = Nothing,
      _csLaunchType = Nothing,
      _csServiceARN = Nothing,
      _csTaskDefinition = Nothing,
      _csSchedulingStrategy = Nothing,
      _csHealthCheckGracePeriodSeconds = Nothing,
      _csNetworkConfiguration = Nothing,
      _csServiceRegistries = Nothing,
      _csCapacityProviderStrategy = Nothing,
      _csTags = Nothing,
      _csRoleARN = Nothing,
      _csDeploymentConfiguration = Nothing
    }

-- | Information about a set of Amazon ECS tasks in either an AWS CodeDeploy or an @EXTERNAL@ deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic.
csTaskSets :: Lens' ContainerService [TaskSet]
csTaskSets = lens _csTaskSets (\s a -> s {_csTaskSets = a}) . _Default . _Coerce

-- | The number of tasks in the cluster that are in the @RUNNING@ state.
csRunningCount :: Lens' ContainerService (Maybe Int)
csRunningCount = lens _csRunningCount (\s a -> s {_csRunningCount = a})

-- | The status of the service. The valid values are @ACTIVE@ , @DRAINING@ , or @INACTIVE@ .
csStatus :: Lens' ContainerService (Maybe Text)
csStatus = lens _csStatus (\s a -> s {_csStatus = a})

-- | The Amazon Resource Name (ARN) of the cluster that hosts the service.
csClusterARN :: Lens' ContainerService (Maybe Text)
csClusterARN = lens _csClusterARN (\s a -> s {_csClusterARN = a})

-- | Specifies whether to propagate the tags from the task definition or the service to the task. If no value is specified, the tags are not propagated.
csPropagateTags :: Lens' ContainerService (Maybe PropagateTags)
csPropagateTags = lens _csPropagateTags (\s a -> s {_csPropagateTags = a})

-- | The Unix timestamp for when the service was created.
csCreatedAt :: Lens' ContainerService (Maybe UTCTime)
csCreatedAt = lens _csCreatedAt (\s a -> s {_csCreatedAt = a}) . mapping _Time

-- | The platform version on which to run your service. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
csPlatformVersion :: Lens' ContainerService (Maybe Text)
csPlatformVersion = lens _csPlatformVersion (\s a -> s {_csPlatformVersion = a})

-- | Specifies whether to enable Amazon ECS managed tags for the tasks in the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
csEnableECSManagedTags :: Lens' ContainerService (Maybe Bool)
csEnableECSManagedTags = lens _csEnableECSManagedTags (\s a -> s {_csEnableECSManagedTags = a})

-- | The principal that created the service.
csCreatedBy :: Lens' ContainerService (Maybe Text)
csCreatedBy = lens _csCreatedBy (\s a -> s {_csCreatedBy = a})

-- | The desired number of instantiations of the task definition to keep running on the service. This value is specified when the service is created with 'CreateService' , and it can be modified with 'UpdateService' .
csDesiredCount :: Lens' ContainerService (Maybe Int)
csDesiredCount = lens _csDesiredCount (\s a -> s {_csDesiredCount = a})

-- | A list of Elastic Load Balancing load balancer objects, containing the load balancer name, the container name (as it appears in a container definition), and the container port to access from the load balancer.
csLoadBalancers :: Lens' ContainerService [LoadBalancer]
csLoadBalancers = lens _csLoadBalancers (\s a -> s {_csLoadBalancers = a}) . _Default . _Coerce

-- | The number of tasks in the cluster that are in the @PENDING@ state.
csPendingCount :: Lens' ContainerService (Maybe Int)
csPendingCount = lens _csPendingCount (\s a -> s {_csPendingCount = a})

-- | The placement constraints for the tasks in the service.
csPlacementConstraints :: Lens' ContainerService [PlacementConstraint]
csPlacementConstraints = lens _csPlacementConstraints (\s a -> s {_csPlacementConstraints = a}) . _Default . _Coerce

-- | The event stream for your service. A maximum of 100 of the latest events are displayed.
csEvents :: Lens' ContainerService [ServiceEvent]
csEvents = lens _csEvents (\s a -> s {_csEvents = a}) . _Default . _Coerce

-- | The placement strategy that determines how tasks for the service are placed.
csPlacementStrategy :: Lens' ContainerService [PlacementStrategy]
csPlacementStrategy = lens _csPlacementStrategy (\s a -> s {_csPlacementStrategy = a}) . _Default . _Coerce

-- | The current state of deployments for the service.
csDeployments :: Lens' ContainerService [Deployment]
csDeployments = lens _csDeployments (\s a -> s {_csDeployments = a}) . _Default . _Coerce

-- | The name of your service. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. Service names must be unique within a cluster, but you can have similarly named services in multiple clusters within a Region or across multiple Regions.
csServiceName :: Lens' ContainerService (Maybe Text)
csServiceName = lens _csServiceName (\s a -> s {_csServiceName = a})

-- | The deployment controller type the service is using. When using the DescribeServices API, this field is omitted if the service is using the @ECS@ deployment controller type.
csDeploymentController :: Lens' ContainerService (Maybe DeploymentController)
csDeploymentController = lens _csDeploymentController (\s a -> s {_csDeploymentController = a})

-- | The launch type on which your service is running. If no value is specified, it will default to @EC2@ . Valid values include @EC2@ and @FARGATE@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
csLaunchType :: Lens' ContainerService (Maybe LaunchType)
csLaunchType = lens _csLaunchType (\s a -> s {_csLaunchType = a})

-- | The ARN that identifies the service. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the service, the AWS account ID of the service owner, the @service@ namespace, and then the service name. For example, @arn:aws:ecs:region:012345678910:service/my-service@ .
csServiceARN :: Lens' ContainerService (Maybe Text)
csServiceARN = lens _csServiceARN (\s a -> s {_csServiceARN = a})

-- | The task definition to use for tasks in the service. This value is specified when the service is created with 'CreateService' , and it can be modified with 'UpdateService' .
csTaskDefinition :: Lens' ContainerService (Maybe Text)
csTaskDefinition = lens _csTaskDefinition (\s a -> s {_csTaskDefinition = a})

-- | The scheduling strategy to use for the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services> . There are two service scheduler strategies available:     * @REPLICA@ -The replica scheduling strategy places and maintains the desired number of tasks across your cluster. By default, the service scheduler spreads tasks across Availability Zones. You can use task placement strategies and constraints to customize task placement decisions.     * @DAEMON@ -The daemon scheduling strategy deploys exactly one task on each active container instance that meets all of the task placement constraints that you specify in your cluster. The service scheduler also evaluates the task placement constraints for running tasks and will stop tasks that do not meet the placement constraints.
csSchedulingStrategy :: Lens' ContainerService (Maybe SchedulingStrategy)
csSchedulingStrategy = lens _csSchedulingStrategy (\s a -> s {_csSchedulingStrategy = a})

-- | The period of time, in seconds, that the Amazon ECS service scheduler ignores unhealthy Elastic Load Balancing target health checks after a task has first started.
csHealthCheckGracePeriodSeconds :: Lens' ContainerService (Maybe Int)
csHealthCheckGracePeriodSeconds = lens _csHealthCheckGracePeriodSeconds (\s a -> s {_csHealthCheckGracePeriodSeconds = a})

-- | The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
csNetworkConfiguration :: Lens' ContainerService (Maybe NetworkConfiguration)
csNetworkConfiguration = lens _csNetworkConfiguration (\s a -> s {_csNetworkConfiguration = a})

-- | The details of the service discovery registries to assign to this service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
csServiceRegistries :: Lens' ContainerService [ServiceRegistry]
csServiceRegistries = lens _csServiceRegistries (\s a -> s {_csServiceRegistries = a}) . _Default . _Coerce

-- | The capacity provider strategy associated with the service.
csCapacityProviderStrategy :: Lens' ContainerService [CapacityProviderStrategyItem]
csCapacityProviderStrategy = lens _csCapacityProviderStrategy (\s a -> s {_csCapacityProviderStrategy = a}) . _Default . _Coerce

-- | The metadata that you apply to the service to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
csTags :: Lens' ContainerService [Tag]
csTags = lens _csTags (\s a -> s {_csTags = a}) . _Default . _Coerce

-- | The ARN of the IAM role associated with the service that allows the Amazon ECS container agent to register container instances with an Elastic Load Balancing load balancer.
csRoleARN :: Lens' ContainerService (Maybe Text)
csRoleARN = lens _csRoleARN (\s a -> s {_csRoleARN = a})

-- | Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
csDeploymentConfiguration :: Lens' ContainerService (Maybe DeploymentConfiguration)
csDeploymentConfiguration = lens _csDeploymentConfiguration (\s a -> s {_csDeploymentConfiguration = a})

instance FromJSON ContainerService where
  parseJSON =
    withObject
      "ContainerService"
      ( \x ->
          ContainerService'
            <$> (x .:? "taskSets" .!= mempty)
            <*> (x .:? "runningCount")
            <*> (x .:? "status")
            <*> (x .:? "clusterArn")
            <*> (x .:? "propagateTags")
            <*> (x .:? "createdAt")
            <*> (x .:? "platformVersion")
            <*> (x .:? "enableECSManagedTags")
            <*> (x .:? "createdBy")
            <*> (x .:? "desiredCount")
            <*> (x .:? "loadBalancers" .!= mempty)
            <*> (x .:? "pendingCount")
            <*> (x .:? "placementConstraints" .!= mempty)
            <*> (x .:? "events" .!= mempty)
            <*> (x .:? "placementStrategy" .!= mempty)
            <*> (x .:? "deployments" .!= mempty)
            <*> (x .:? "serviceName")
            <*> (x .:? "deploymentController")
            <*> (x .:? "launchType")
            <*> (x .:? "serviceArn")
            <*> (x .:? "taskDefinition")
            <*> (x .:? "schedulingStrategy")
            <*> (x .:? "healthCheckGracePeriodSeconds")
            <*> (x .:? "networkConfiguration")
            <*> (x .:? "serviceRegistries" .!= mempty)
            <*> (x .:? "capacityProviderStrategy" .!= mempty)
            <*> (x .:? "tags" .!= mempty)
            <*> (x .:? "roleArn")
            <*> (x .:? "deploymentConfiguration")
      )

instance Hashable ContainerService

instance NFData ContainerService
