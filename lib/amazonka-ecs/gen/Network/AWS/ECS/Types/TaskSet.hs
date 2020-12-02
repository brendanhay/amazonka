{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskSet where

import Network.AWS.ECS.Types.CapacityProviderStrategyItem
import Network.AWS.ECS.Types.LaunchType
import Network.AWS.ECS.Types.LoadBalancer
import Network.AWS.ECS.Types.NetworkConfiguration
import Network.AWS.ECS.Types.Scale
import Network.AWS.ECS.Types.ServiceRegistry
import Network.AWS.ECS.Types.StabilityStatus
import Network.AWS.ECS.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a set of Amazon ECS tasks in either an AWS CodeDeploy or an @EXTERNAL@ deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic.
--
--
--
-- /See:/ 'taskSet' smart constructor.
data TaskSet = TaskSet'
  { _tsRunningCount :: !(Maybe Int),
    _tsStatus :: !(Maybe Text),
    _tsClusterARN :: !(Maybe Text),
    _tsComputedDesiredCount :: !(Maybe Int),
    _tsCreatedAt :: !(Maybe POSIX),
    _tsPlatformVersion :: !(Maybe Text),
    _tsScale :: !(Maybe Scale),
    _tsLoadBalancers :: !(Maybe [LoadBalancer]),
    _tsStabilityStatusAt :: !(Maybe POSIX),
    _tsPendingCount :: !(Maybe Int),
    _tsTaskSetARN :: !(Maybe Text),
    _tsStartedBy :: !(Maybe Text),
    _tsId :: !(Maybe Text),
    _tsLaunchType :: !(Maybe LaunchType),
    _tsUpdatedAt :: !(Maybe POSIX),
    _tsServiceARN :: !(Maybe Text),
    _tsTaskDefinition :: !(Maybe Text),
    _tsExternalId :: !(Maybe Text),
    _tsNetworkConfiguration :: !(Maybe NetworkConfiguration),
    _tsServiceRegistries :: !(Maybe [ServiceRegistry]),
    _tsCapacityProviderStrategy ::
      !(Maybe [CapacityProviderStrategyItem]),
    _tsStabilityStatus :: !(Maybe StabilityStatus),
    _tsTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TaskSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsRunningCount' - The number of tasks in the task set that are in the @RUNNING@ status during a deployment. A task in the @RUNNING@ state is running and ready for use.
--
-- * 'tsStatus' - The status of the task set. The following describes each state:     * PRIMARY    * The task set is serving production traffic.     * ACTIVE    * The task set is not serving production traffic.     * DRAINING    * The tasks in the task set are being stopped and their corresponding targets are being deregistered from their target group.
--
-- * 'tsClusterARN' - The Amazon Resource Name (ARN) of the cluster that the service that hosts the task set exists in.
--
-- * 'tsComputedDesiredCount' - The computed desired count for the task set. This is calculated by multiplying the service's @desiredCount@ by the task set's @scale@ percentage. The result is always rounded up. For example, if the computed desired count is 1.2, it rounds up to 2 tasks.
--
-- * 'tsCreatedAt' - The Unix timestamp for when the task set was created.
--
-- * 'tsPlatformVersion' - The platform version on which the tasks in the task set are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'tsScale' - A floating-point percentage of the desired number of tasks to place and keep running in the task set.
--
-- * 'tsLoadBalancers' - Details on a load balancer that is used with a task set.
--
-- * 'tsStabilityStatusAt' - The Unix timestamp for when the task set stability status was retrieved.
--
-- * 'tsPendingCount' - The number of tasks in the task set that are in the @PENDING@ status during a deployment. A task in the @PENDING@ state is preparing to enter the @RUNNING@ state. A task set enters the @PENDING@ status when it launches for the first time or when it is restarted after being in the @STOPPED@ state.
--
-- * 'tsTaskSetARN' - The Amazon Resource Name (ARN) of the task set.
--
-- * 'tsStartedBy' - The tag specified when a task set is started. If the task set is created by an AWS CodeDeploy deployment, the @startedBy@ parameter is @CODE_DEPLOY@ . For a task set created for an external deployment, the startedBy field isn't used.
--
-- * 'tsId' - The ID of the task set.
--
-- * 'tsLaunchType' - The launch type the tasks in the task set are using. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'tsUpdatedAt' - The Unix timestamp for when the task set was last updated.
--
-- * 'tsServiceARN' - The Amazon Resource Name (ARN) of the service the task set exists in.
--
-- * 'tsTaskDefinition' - The task definition the task set is using.
--
-- * 'tsExternalId' - The external ID associated with the task set. If a task set is created by an AWS CodeDeploy deployment, the @externalId@ parameter contains the AWS CodeDeploy deployment ID. If a task set is created for an external deployment and is associated with a service discovery registry, the @externalId@ parameter contains the @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute.
--
-- * 'tsNetworkConfiguration' - The network configuration for the task set.
--
-- * 'tsServiceRegistries' - The details of the service discovery registries to assign to this task set. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
--
-- * 'tsCapacityProviderStrategy' - The capacity provider strategy associated with the task set.
--
-- * 'tsStabilityStatus' - The stability status, which indicates whether the task set has reached a steady state. If the following conditions are met, the task set will be in @STEADY_STATE@ :     * The task @runningCount@ is equal to the @computedDesiredCount@ .     * The @pendingCount@ is @0@ .     * There are no tasks running on container instances in the @DRAINING@ status.     * All tasks are reporting a healthy status from the load balancers, service discovery, and container health checks. If any of those conditions are not met, the stability status returns @STABILIZING@ .
--
-- * 'tsTags' - The metadata that you apply to the task set to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
taskSet ::
  TaskSet
taskSet =
  TaskSet'
    { _tsRunningCount = Nothing,
      _tsStatus = Nothing,
      _tsClusterARN = Nothing,
      _tsComputedDesiredCount = Nothing,
      _tsCreatedAt = Nothing,
      _tsPlatformVersion = Nothing,
      _tsScale = Nothing,
      _tsLoadBalancers = Nothing,
      _tsStabilityStatusAt = Nothing,
      _tsPendingCount = Nothing,
      _tsTaskSetARN = Nothing,
      _tsStartedBy = Nothing,
      _tsId = Nothing,
      _tsLaunchType = Nothing,
      _tsUpdatedAt = Nothing,
      _tsServiceARN = Nothing,
      _tsTaskDefinition = Nothing,
      _tsExternalId = Nothing,
      _tsNetworkConfiguration = Nothing,
      _tsServiceRegistries = Nothing,
      _tsCapacityProviderStrategy = Nothing,
      _tsStabilityStatus = Nothing,
      _tsTags = Nothing
    }

-- | The number of tasks in the task set that are in the @RUNNING@ status during a deployment. A task in the @RUNNING@ state is running and ready for use.
tsRunningCount :: Lens' TaskSet (Maybe Int)
tsRunningCount = lens _tsRunningCount (\s a -> s {_tsRunningCount = a})

-- | The status of the task set. The following describes each state:     * PRIMARY    * The task set is serving production traffic.     * ACTIVE    * The task set is not serving production traffic.     * DRAINING    * The tasks in the task set are being stopped and their corresponding targets are being deregistered from their target group.
tsStatus :: Lens' TaskSet (Maybe Text)
tsStatus = lens _tsStatus (\s a -> s {_tsStatus = a})

-- | The Amazon Resource Name (ARN) of the cluster that the service that hosts the task set exists in.
tsClusterARN :: Lens' TaskSet (Maybe Text)
tsClusterARN = lens _tsClusterARN (\s a -> s {_tsClusterARN = a})

-- | The computed desired count for the task set. This is calculated by multiplying the service's @desiredCount@ by the task set's @scale@ percentage. The result is always rounded up. For example, if the computed desired count is 1.2, it rounds up to 2 tasks.
tsComputedDesiredCount :: Lens' TaskSet (Maybe Int)
tsComputedDesiredCount = lens _tsComputedDesiredCount (\s a -> s {_tsComputedDesiredCount = a})

-- | The Unix timestamp for when the task set was created.
tsCreatedAt :: Lens' TaskSet (Maybe UTCTime)
tsCreatedAt = lens _tsCreatedAt (\s a -> s {_tsCreatedAt = a}) . mapping _Time

-- | The platform version on which the tasks in the task set are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
tsPlatformVersion :: Lens' TaskSet (Maybe Text)
tsPlatformVersion = lens _tsPlatformVersion (\s a -> s {_tsPlatformVersion = a})

-- | A floating-point percentage of the desired number of tasks to place and keep running in the task set.
tsScale :: Lens' TaskSet (Maybe Scale)
tsScale = lens _tsScale (\s a -> s {_tsScale = a})

-- | Details on a load balancer that is used with a task set.
tsLoadBalancers :: Lens' TaskSet [LoadBalancer]
tsLoadBalancers = lens _tsLoadBalancers (\s a -> s {_tsLoadBalancers = a}) . _Default . _Coerce

-- | The Unix timestamp for when the task set stability status was retrieved.
tsStabilityStatusAt :: Lens' TaskSet (Maybe UTCTime)
tsStabilityStatusAt = lens _tsStabilityStatusAt (\s a -> s {_tsStabilityStatusAt = a}) . mapping _Time

-- | The number of tasks in the task set that are in the @PENDING@ status during a deployment. A task in the @PENDING@ state is preparing to enter the @RUNNING@ state. A task set enters the @PENDING@ status when it launches for the first time or when it is restarted after being in the @STOPPED@ state.
tsPendingCount :: Lens' TaskSet (Maybe Int)
tsPendingCount = lens _tsPendingCount (\s a -> s {_tsPendingCount = a})

-- | The Amazon Resource Name (ARN) of the task set.
tsTaskSetARN :: Lens' TaskSet (Maybe Text)
tsTaskSetARN = lens _tsTaskSetARN (\s a -> s {_tsTaskSetARN = a})

-- | The tag specified when a task set is started. If the task set is created by an AWS CodeDeploy deployment, the @startedBy@ parameter is @CODE_DEPLOY@ . For a task set created for an external deployment, the startedBy field isn't used.
tsStartedBy :: Lens' TaskSet (Maybe Text)
tsStartedBy = lens _tsStartedBy (\s a -> s {_tsStartedBy = a})

-- | The ID of the task set.
tsId :: Lens' TaskSet (Maybe Text)
tsId = lens _tsId (\s a -> s {_tsId = a})

-- | The launch type the tasks in the task set are using. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
tsLaunchType :: Lens' TaskSet (Maybe LaunchType)
tsLaunchType = lens _tsLaunchType (\s a -> s {_tsLaunchType = a})

-- | The Unix timestamp for when the task set was last updated.
tsUpdatedAt :: Lens' TaskSet (Maybe UTCTime)
tsUpdatedAt = lens _tsUpdatedAt (\s a -> s {_tsUpdatedAt = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the service the task set exists in.
tsServiceARN :: Lens' TaskSet (Maybe Text)
tsServiceARN = lens _tsServiceARN (\s a -> s {_tsServiceARN = a})

-- | The task definition the task set is using.
tsTaskDefinition :: Lens' TaskSet (Maybe Text)
tsTaskDefinition = lens _tsTaskDefinition (\s a -> s {_tsTaskDefinition = a})

-- | The external ID associated with the task set. If a task set is created by an AWS CodeDeploy deployment, the @externalId@ parameter contains the AWS CodeDeploy deployment ID. If a task set is created for an external deployment and is associated with a service discovery registry, the @externalId@ parameter contains the @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute.
tsExternalId :: Lens' TaskSet (Maybe Text)
tsExternalId = lens _tsExternalId (\s a -> s {_tsExternalId = a})

-- | The network configuration for the task set.
tsNetworkConfiguration :: Lens' TaskSet (Maybe NetworkConfiguration)
tsNetworkConfiguration = lens _tsNetworkConfiguration (\s a -> s {_tsNetworkConfiguration = a})

-- | The details of the service discovery registries to assign to this task set. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
tsServiceRegistries :: Lens' TaskSet [ServiceRegistry]
tsServiceRegistries = lens _tsServiceRegistries (\s a -> s {_tsServiceRegistries = a}) . _Default . _Coerce

-- | The capacity provider strategy associated with the task set.
tsCapacityProviderStrategy :: Lens' TaskSet [CapacityProviderStrategyItem]
tsCapacityProviderStrategy = lens _tsCapacityProviderStrategy (\s a -> s {_tsCapacityProviderStrategy = a}) . _Default . _Coerce

-- | The stability status, which indicates whether the task set has reached a steady state. If the following conditions are met, the task set will be in @STEADY_STATE@ :     * The task @runningCount@ is equal to the @computedDesiredCount@ .     * The @pendingCount@ is @0@ .     * There are no tasks running on container instances in the @DRAINING@ status.     * All tasks are reporting a healthy status from the load balancers, service discovery, and container health checks. If any of those conditions are not met, the stability status returns @STABILIZING@ .
tsStabilityStatus :: Lens' TaskSet (Maybe StabilityStatus)
tsStabilityStatus = lens _tsStabilityStatus (\s a -> s {_tsStabilityStatus = a})

-- | The metadata that you apply to the task set to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
tsTags :: Lens' TaskSet [Tag]
tsTags = lens _tsTags (\s a -> s {_tsTags = a}) . _Default . _Coerce

instance FromJSON TaskSet where
  parseJSON =
    withObject
      "TaskSet"
      ( \x ->
          TaskSet'
            <$> (x .:? "runningCount")
            <*> (x .:? "status")
            <*> (x .:? "clusterArn")
            <*> (x .:? "computedDesiredCount")
            <*> (x .:? "createdAt")
            <*> (x .:? "platformVersion")
            <*> (x .:? "scale")
            <*> (x .:? "loadBalancers" .!= mempty)
            <*> (x .:? "stabilityStatusAt")
            <*> (x .:? "pendingCount")
            <*> (x .:? "taskSetArn")
            <*> (x .:? "startedBy")
            <*> (x .:? "id")
            <*> (x .:? "launchType")
            <*> (x .:? "updatedAt")
            <*> (x .:? "serviceArn")
            <*> (x .:? "taskDefinition")
            <*> (x .:? "externalId")
            <*> (x .:? "networkConfiguration")
            <*> (x .:? "serviceRegistries" .!= mempty)
            <*> (x .:? "capacityProviderStrategy" .!= mempty)
            <*> (x .:? "stabilityStatus")
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable TaskSet

instance NFData TaskSet
