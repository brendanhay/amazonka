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
-- Module      : Network.AWS.ECS.UpdateService
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the desired count, deployment configuration, network configuration, or task definition used in a service.
--
--
-- You can add to or subtract from the number of instantiations of a task definition in a service by specifying the cluster that the service is running in and a new @desiredCount@ parameter.
--
-- If you have updated the Docker image of your application, you can create a new task definition with that image and deploy it to your service. The service scheduler uses the minimum healthy percent and maximum percent parameters (in the service's deployment configuration) to determine the deployment strategy.
--
-- You can also update the deployment configuration of a service. When a deployment is triggered by updating the task definition of a service, the service scheduler uses the deployment configuration parameters, @minimumHealthyPercent@ and @maximumPercent@ , to determine the deployment strategy.
--
--     * If @minimumHealthyPercent@ is below 100%, the scheduler can ignore @desiredCount@ temporarily during a deployment. For example, if @desiredCount@ is four tasks, a minimum of 50% allows the scheduler to stop two existing tasks before starting two new tasks. Tasks for services that do not use a load balancer are considered healthy if they are in the @RUNNING@ state. Tasks for services that use a load balancer are considered healthy if they are in the @RUNNING@ state and the container instance they are hosted on is reported as healthy by the load balancer.
--
--     * The @maximumPercent@ parameter represents an upper limit on the number of running tasks during a deployment, which enables you to define the deployment batch size. For example, if @desiredCount@ is four tasks, a maximum of 200% starts four new tasks before stopping the four older tasks (provided that the cluster resources required to do this are available).
--
--
--
-- When 'UpdateService' stops a task during a deployment, the equivalent of @docker stop@ is issued to the containers running in the task. This results in a @SIGTERM@ and a 30-second timeout, after which @SIGKILL@ is sent and the containers are forcibly stopped. If the container handles the @SIGTERM@ gracefully and exits within 30 seconds from receiving it, no @SIGKILL@ is sent.
--
-- When the service scheduler launches new tasks, it determines task placement in your cluster with the following logic:
--
--     * Determine which of the container instances in your cluster can support your service's task definition (for example, they have the required CPU, memory, ports, and container instance attributes).
--
--     * By default, the service scheduler attempts to balance tasks across Availability Zones in this manner (although you can choose a different placement strategy):
--
--     * Sort the valid container instances by the fewest number of running tasks for this service in the same Availability Zone as the instance. For example, if zone A has one running service task and zones B and C each have zero, valid container instances in either zone B or C are considered optimal for placement.
--
--     * Place the new service task on a valid container instance in an optimal Availability Zone (based on the previous steps), favoring container instances with the fewest number of running tasks for this service.
--
--
--
--
--
-- When the service scheduler stops running tasks, it attempts to maintain balance across the Availability Zones in your cluster using the following logic:
--
--     * Sort the container instances by the largest number of running tasks for this service in the same Availability Zone as the instance. For example, if zone A has one running service task and zones B and C each have two, container instances in either zone B or C are considered optimal for termination.
--
--     * Stop the task on a container instance in an optimal Availability Zone (based on the previous steps), favoring container instances with the largest number of running tasks for this service.
--
--
--
module Network.AWS.ECS.UpdateService
    (
    -- * Creating a Request
      updateService
    , UpdateService
    -- * Request Lenses
    , usCluster
    , usPlatformVersion
    , usDesiredCount
    , usForceNewDeployment
    , usTaskDefinition
    , usHealthCheckGracePeriodSeconds
    , usNetworkConfiguration
    , usDeploymentConfiguration
    , usService

    -- * Destructuring the Response
    , updateServiceResponse
    , UpdateServiceResponse
    -- * Response Lenses
    , usrsService
    , usrsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateService' smart constructor.
data UpdateService = UpdateService'
  { _usCluster                       :: !(Maybe Text)
  , _usPlatformVersion               :: !(Maybe Text)
  , _usDesiredCount                  :: !(Maybe Int)
  , _usForceNewDeployment            :: !(Maybe Bool)
  , _usTaskDefinition                :: !(Maybe Text)
  , _usHealthCheckGracePeriodSeconds :: !(Maybe Int)
  , _usNetworkConfiguration          :: !(Maybe NetworkConfiguration)
  , _usDeploymentConfiguration       :: !(Maybe DeploymentConfiguration)
  , _usService                       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usCluster' - The short name or full Amazon Resource Name (ARN) of the cluster that your service is running on. If you do not specify a cluster, the default cluster is assumed.
--
-- * 'usPlatformVersion' - The platform version you want to update your service to run.
--
-- * 'usDesiredCount' - The number of instantiations of the task to place and keep running in your service.
--
-- * 'usForceNewDeployment' - Whether to force a new deployment of the service. Deployments are not forced by default. You can use this option to trigger a new deployment with no service definition changes. For example, you can update a service's tasks to use a newer Docker image with the same image/tag combination (@my_image:latest@ ) or to roll Fargate tasks onto a newer platform version.
--
-- * 'usTaskDefinition' - The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to run in your service. If a @revision@ is not specified, the latest @ACTIVE@ revision is used. If you modify the task definition with @UpdateService@ , Amazon ECS spawns a task with the new version of the task definition and then stops an old task after the new version is running.
--
-- * 'usHealthCheckGracePeriodSeconds' - The period of time, in seconds, that the Amazon ECS service scheduler should ignore unhealthy Elastic Load Balancing target health checks after a task has first started. This is only valid if your service is configured to use a load balancer. If your service's tasks take a while to start and respond to Elastic Load Balancing health checks, you can specify a health check grace period of up to 1,800 seconds during which the ECS service scheduler ignores the Elastic Load Balancing health check status. This grace period can prevent the ECS service scheduler from marking tasks as unhealthy and stopping them before they have time to come up.
--
-- * 'usNetworkConfiguration' - The network configuration for the service. This parameter is required for task definitions that use the @awsvpc@ network mode to receive their own elastic network interface, and it is not supported for other network modes. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'usDeploymentConfiguration' - Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
--
-- * 'usService' - The name of the service to update.
updateService
    :: Text -- ^ 'usService'
    -> UpdateService
updateService pService_ =
  UpdateService'
    { _usCluster = Nothing
    , _usPlatformVersion = Nothing
    , _usDesiredCount = Nothing
    , _usForceNewDeployment = Nothing
    , _usTaskDefinition = Nothing
    , _usHealthCheckGracePeriodSeconds = Nothing
    , _usNetworkConfiguration = Nothing
    , _usDeploymentConfiguration = Nothing
    , _usService = pService_
    }


-- | The short name or full Amazon Resource Name (ARN) of the cluster that your service is running on. If you do not specify a cluster, the default cluster is assumed.
usCluster :: Lens' UpdateService (Maybe Text)
usCluster = lens _usCluster (\ s a -> s{_usCluster = a})

-- | The platform version you want to update your service to run.
usPlatformVersion :: Lens' UpdateService (Maybe Text)
usPlatformVersion = lens _usPlatformVersion (\ s a -> s{_usPlatformVersion = a})

-- | The number of instantiations of the task to place and keep running in your service.
usDesiredCount :: Lens' UpdateService (Maybe Int)
usDesiredCount = lens _usDesiredCount (\ s a -> s{_usDesiredCount = a})

-- | Whether to force a new deployment of the service. Deployments are not forced by default. You can use this option to trigger a new deployment with no service definition changes. For example, you can update a service's tasks to use a newer Docker image with the same image/tag combination (@my_image:latest@ ) or to roll Fargate tasks onto a newer platform version.
usForceNewDeployment :: Lens' UpdateService (Maybe Bool)
usForceNewDeployment = lens _usForceNewDeployment (\ s a -> s{_usForceNewDeployment = a})

-- | The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to run in your service. If a @revision@ is not specified, the latest @ACTIVE@ revision is used. If you modify the task definition with @UpdateService@ , Amazon ECS spawns a task with the new version of the task definition and then stops an old task after the new version is running.
usTaskDefinition :: Lens' UpdateService (Maybe Text)
usTaskDefinition = lens _usTaskDefinition (\ s a -> s{_usTaskDefinition = a})

-- | The period of time, in seconds, that the Amazon ECS service scheduler should ignore unhealthy Elastic Load Balancing target health checks after a task has first started. This is only valid if your service is configured to use a load balancer. If your service's tasks take a while to start and respond to Elastic Load Balancing health checks, you can specify a health check grace period of up to 1,800 seconds during which the ECS service scheduler ignores the Elastic Load Balancing health check status. This grace period can prevent the ECS service scheduler from marking tasks as unhealthy and stopping them before they have time to come up.
usHealthCheckGracePeriodSeconds :: Lens' UpdateService (Maybe Int)
usHealthCheckGracePeriodSeconds = lens _usHealthCheckGracePeriodSeconds (\ s a -> s{_usHealthCheckGracePeriodSeconds = a})

-- | The network configuration for the service. This parameter is required for task definitions that use the @awsvpc@ network mode to receive their own elastic network interface, and it is not supported for other network modes. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
usNetworkConfiguration :: Lens' UpdateService (Maybe NetworkConfiguration)
usNetworkConfiguration = lens _usNetworkConfiguration (\ s a -> s{_usNetworkConfiguration = a})

-- | Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
usDeploymentConfiguration :: Lens' UpdateService (Maybe DeploymentConfiguration)
usDeploymentConfiguration = lens _usDeploymentConfiguration (\ s a -> s{_usDeploymentConfiguration = a})

-- | The name of the service to update.
usService :: Lens' UpdateService Text
usService = lens _usService (\ s a -> s{_usService = a})

instance AWSRequest UpdateService where
        type Rs UpdateService = UpdateServiceResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 UpdateServiceResponse' <$>
                   (x .?> "service") <*> (pure (fromEnum s)))

instance Hashable UpdateService where

instance NFData UpdateService where

instance ToHeaders UpdateService where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.UpdateService"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateService where
        toJSON UpdateService'{..}
          = object
              (catMaybes
                 [("cluster" .=) <$> _usCluster,
                  ("platformVersion" .=) <$> _usPlatformVersion,
                  ("desiredCount" .=) <$> _usDesiredCount,
                  ("forceNewDeployment" .=) <$> _usForceNewDeployment,
                  ("taskDefinition" .=) <$> _usTaskDefinition,
                  ("healthCheckGracePeriodSeconds" .=) <$>
                    _usHealthCheckGracePeriodSeconds,
                  ("networkConfiguration" .=) <$>
                    _usNetworkConfiguration,
                  ("deploymentConfiguration" .=) <$>
                    _usDeploymentConfiguration,
                  Just ("service" .= _usService)])

instance ToPath UpdateService where
        toPath = const "/"

instance ToQuery UpdateService where
        toQuery = const mempty

-- | /See:/ 'updateServiceResponse' smart constructor.
data UpdateServiceResponse = UpdateServiceResponse'
  { _usrsService        :: !(Maybe ContainerService)
  , _usrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateServiceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usrsService' - The full description of your service following the update call.
--
-- * 'usrsResponseStatus' - -- | The response status code.
updateServiceResponse
    :: Int -- ^ 'usrsResponseStatus'
    -> UpdateServiceResponse
updateServiceResponse pResponseStatus_ =
  UpdateServiceResponse'
    {_usrsService = Nothing, _usrsResponseStatus = pResponseStatus_}


-- | The full description of your service following the update call.
usrsService :: Lens' UpdateServiceResponse (Maybe ContainerService)
usrsService = lens _usrsService (\ s a -> s{_usrsService = a})

-- | -- | The response status code.
usrsResponseStatus :: Lens' UpdateServiceResponse Int
usrsResponseStatus = lens _usrsResponseStatus (\ s a -> s{_usrsResponseStatus = a})

instance NFData UpdateServiceResponse where
