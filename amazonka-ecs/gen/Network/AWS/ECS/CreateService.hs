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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs and maintains a desired number of tasks from a specified task definition. If the number of tasks running in a service drops below 'desiredCount', Amazon ECS spawns another instantiation of the task in the specified cluster. To update an existing service, see < UpdateService>.
--
-- You can optionally specify a deployment configuration for your service. During a deployment (which is triggered by changing the task definition of a service with an < UpdateService> operation), the service scheduler uses the 'minimumHealthyPercent' and 'maximumPercent' parameters to determine the deployment strategy.
--
-- If the 'minimumHealthyPercent' is below 100%, the scheduler can ignore the 'desiredCount' temporarily during a deployment. For example, if your service has a 'desiredCount' of four tasks, a 'minimumHealthyPercent' of 50% allows the scheduler to stop two existing tasks before starting two new tasks. Tasks for services that /do not/ use a load balancer are considered healthy if they are in the 'RUNNING' state; tasks for services that /do/ use a load balancer are considered healthy if they are in the 'RUNNING' state and the container instance it is hosted on is reported as healthy by the load balancer. The default value for 'minimumHealthyPercent' is 50% in the console and 100% for the AWS CLI, the AWS SDKs, and the APIs.
--
-- The 'maximumPercent' parameter represents an upper limit on the number of running tasks during a deployment, which enables you to define the deployment batch size. For example, if your service has a 'desiredCount' of four tasks, a 'maximumPercent' value of 200% starts four new tasks before stopping the four older tasks (provided that the cluster resources required to do this are available). The default value for 'maximumPercent' is 200%.
--
-- When the service scheduler launches new tasks, it attempts to balance them across the Availability Zones in your cluster with the following logic:
--
-- -   Determine which of the container instances in your cluster can support your service\'s task definition (for example, they have the required CPU, memory, ports, and container instance attributes).
--
-- -   Sort the valid container instances by the fewest number of running tasks for this service in the same Availability Zone as the instance. For example, if zone A has one running service task and zones B and C each have zero, valid container instances in either zone B or C are considered optimal for placement.
--
-- -   Place the new service task on a valid container instance in an optimal Availability Zone (based on the previous steps), favoring container instances with the fewest number of running tasks for this service.
--
module Network.AWS.ECS.CreateService
    (
    -- * Creating a Request
      createService
    , CreateService
    -- * Request Lenses
    , cCluster
    , cClientToken
    , cLoadBalancers
    , cRole
    , cDeploymentConfiguration
    , cServiceName
    , cTaskDefinition
    , cDesiredCount

    -- * Destructuring the Response
    , createServiceResponse
    , CreateServiceResponse
    -- * Response Lenses
    , csrsService
    , csrsResponseStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createService' smart constructor.
data CreateService = CreateService'
    { _cCluster                 :: !(Maybe Text)
    , _cClientToken             :: !(Maybe Text)
    , _cLoadBalancers           :: !(Maybe [LoadBalancer])
    , _cRole                    :: !(Maybe Text)
    , _cDeploymentConfiguration :: !(Maybe DeploymentConfiguration)
    , _cServiceName             :: !Text
    , _cTaskDefinition          :: !Text
    , _cDesiredCount            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCluster'
--
-- * 'cClientToken'
--
-- * 'cLoadBalancers'
--
-- * 'cRole'
--
-- * 'cDeploymentConfiguration'
--
-- * 'cServiceName'
--
-- * 'cTaskDefinition'
--
-- * 'cDesiredCount'
createService
    :: Text -- ^ 'cServiceName'
    -> Text -- ^ 'cTaskDefinition'
    -> Int -- ^ 'cDesiredCount'
    -> CreateService
createService pServiceName_ pTaskDefinition_ pDesiredCount_ =
    CreateService'
    { _cCluster = Nothing
    , _cClientToken = Nothing
    , _cLoadBalancers = Nothing
    , _cRole = Nothing
    , _cDeploymentConfiguration = Nothing
    , _cServiceName = pServiceName_
    , _cTaskDefinition = pTaskDefinition_
    , _cDesiredCount = pDesiredCount_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster on which to run your service. If you do not specify a cluster, the default cluster is assumed.
cCluster :: Lens' CreateService (Maybe Text)
cCluster = lens _cCluster (\ s a -> s{_cCluster = a});

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. Up to 32 ASCII characters are allowed.
cClientToken :: Lens' CreateService (Maybe Text)
cClientToken = lens _cClientToken (\ s a -> s{_cClientToken = a});

-- | A list of load balancer objects, containing the load balancer name, the container name (as it appears in a container definition), and the container port to access from the load balancer.
cLoadBalancers :: Lens' CreateService [LoadBalancer]
cLoadBalancers = lens _cLoadBalancers (\ s a -> s{_cLoadBalancers = a}) . _Default . _Coerce;

-- | The name or full Amazon Resource Name (ARN) of the IAM role that allows your Amazon ECS container agent to make calls to your load balancer on your behalf. This parameter is only required if you are using a load balancer with your service.
cRole :: Lens' CreateService (Maybe Text)
cRole = lens _cRole (\ s a -> s{_cRole = a});

-- | Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
cDeploymentConfiguration :: Lens' CreateService (Maybe DeploymentConfiguration)
cDeploymentConfiguration = lens _cDeploymentConfiguration (\ s a -> s{_cDeploymentConfiguration = a});

-- | The name of your service. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. Service names must be unique within a cluster, but you can have similarly named services in multiple clusters within a region or across multiple regions.
cServiceName :: Lens' CreateService Text
cServiceName = lens _cServiceName (\ s a -> s{_cServiceName = a});

-- | The 'family' and 'revision' ('family:revision') or full Amazon Resource Name (ARN) of the task definition to run in your service. If a 'revision' is not specified, the latest 'ACTIVE' revision is used.
cTaskDefinition :: Lens' CreateService Text
cTaskDefinition = lens _cTaskDefinition (\ s a -> s{_cTaskDefinition = a});

-- | The number of instantiations of the specified task definition to place and keep running on your cluster.
cDesiredCount :: Lens' CreateService Int
cDesiredCount = lens _cDesiredCount (\ s a -> s{_cDesiredCount = a});

instance AWSRequest CreateService where
        type Rs CreateService = CreateServiceResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 CreateServiceResponse' <$>
                   (x .?> "service") <*> (pure (fromEnum s)))

instance Hashable CreateService

instance NFData CreateService

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
                 [("cluster" .=) <$> _cCluster,
                  ("clientToken" .=) <$> _cClientToken,
                  ("loadBalancers" .=) <$> _cLoadBalancers,
                  ("role" .=) <$> _cRole,
                  ("deploymentConfiguration" .=) <$>
                    _cDeploymentConfiguration,
                  Just ("serviceName" .= _cServiceName),
                  Just ("taskDefinition" .= _cTaskDefinition),
                  Just ("desiredCount" .= _cDesiredCount)])

instance ToPath CreateService where
        toPath = const "/"

instance ToQuery CreateService where
        toQuery = const mempty

-- | /See:/ 'createServiceResponse' smart constructor.
data CreateServiceResponse = CreateServiceResponse'
    { _csrsService        :: !(Maybe ContainerService)
    , _csrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateServiceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsService'
--
-- * 'csrsResponseStatus'
createServiceResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> CreateServiceResponse
createServiceResponse pResponseStatus_ =
    CreateServiceResponse'
    { _csrsService = Nothing
    , _csrsResponseStatus = pResponseStatus_
    }

-- | The full description of your service following the create call.
csrsService :: Lens' CreateServiceResponse (Maybe ContainerService)
csrsService = lens _csrsService (\ s a -> s{_csrsService = a});

-- | The response status code.
csrsResponseStatus :: Lens' CreateServiceResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a});

instance NFData CreateServiceResponse
