{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ECS.CreateService
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Runs and maintains a desired number of tasks from a specified task
-- definition. If the number of tasks running in a service drops below
-- @desiredCount@, Amazon ECS will spawn another instantiation of the task
-- in the specified cluster.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_CreateService.html>
module Network.AWS.ECS.CreateService
    (
    -- * Request
      CreateService
    -- ** Request constructor
    , createService
    -- ** Request lenses
    , creCluster
    , creClientToken
    , creDesiredCount
    , creLoadBalancers
    , creRole
    , creTaskDefinition
    , creServiceName

    -- * Response
    , CreateServiceResponse
    -- ** Response constructor
    , createServiceResponse
    -- ** Response lenses
    , csrContainerService
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ECS.Types

-- | /See:/ 'createService' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creCluster'
--
-- * 'creClientToken'
--
-- * 'creDesiredCount'
--
-- * 'creLoadBalancers'
--
-- * 'creRole'
--
-- * 'creTaskDefinition'
--
-- * 'creServiceName'
data CreateService = CreateService'{_creCluster :: Maybe Text, _creClientToken :: Maybe Text, _creDesiredCount :: Maybe Int, _creLoadBalancers :: Maybe [LoadBalancer], _creRole :: Maybe Text, _creTaskDefinition :: Maybe Text, _creServiceName :: Text} deriving (Eq, Read, Show)

-- | 'CreateService' smart constructor.
createService :: Text -> CreateService
createService pServiceName = CreateService'{_creCluster = Nothing, _creClientToken = Nothing, _creDesiredCount = Nothing, _creLoadBalancers = Nothing, _creRole = Nothing, _creTaskDefinition = Nothing, _creServiceName = pServiceName};

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- you want to run your service on. If you do not specify a cluster, the
-- default cluster is assumed.
creCluster :: Lens' CreateService (Maybe Text)
creCluster = lens _creCluster (\ s a -> s{_creCluster = a});

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. Up to 32 ASCII characters are allowed.
creClientToken :: Lens' CreateService (Maybe Text)
creClientToken = lens _creClientToken (\ s a -> s{_creClientToken = a});

-- | The number of instantiations of the specified task definition that you
-- would like to place and keep running on your cluster.
creDesiredCount :: Lens' CreateService (Maybe Int)
creDesiredCount = lens _creDesiredCount (\ s a -> s{_creDesiredCount = a});

-- | A list of load balancer objects, containing the load balancer name, the
-- container name (as it appears in a container definition), and the
-- container port to access from the load balancer.
creLoadBalancers :: Lens' CreateService (Maybe [LoadBalancer])
creLoadBalancers = lens _creLoadBalancers (\ s a -> s{_creLoadBalancers = a});

-- | The name or full Amazon Resource Name (ARN) of the IAM role that allows
-- your Amazon ECS container agent to make calls to your load balancer on
-- your behalf. This parameter is only required if you are using a load
-- balancer with your service.
creRole :: Lens' CreateService (Maybe Text)
creRole = lens _creRole (\ s a -> s{_creRole = a});

-- | The @family@ and @revision@ (@family:revision@) or full Amazon Resource
-- Name (ARN) of the task definition that you want to run in your service.
creTaskDefinition :: Lens' CreateService (Maybe Text)
creTaskDefinition = lens _creTaskDefinition (\ s a -> s{_creTaskDefinition = a});

-- | The name of your service. Up to 255 letters (uppercase and lowercase),
-- numbers, hyphens, and underscores are allowed.
creServiceName :: Lens' CreateService Text
creServiceName = lens _creServiceName (\ s a -> s{_creServiceName = a});

instance AWSRequest CreateService where
        type Sv CreateService = ECS
        type Rs CreateService = CreateServiceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateServiceResponse' <$> x .?> "ContainerService")

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
              ["cluster" .= _creCluster,
               "clientToken" .= _creClientToken,
               "desiredCount" .= _creDesiredCount,
               "loadBalancers" .= _creLoadBalancers,
               "role" .= _creRole,
               "taskDefinition" .= _creTaskDefinition,
               "serviceName" .= _creServiceName]

instance ToPath CreateService where
        toPath = const "/"

instance ToQuery CreateService where
        toQuery = const mempty

-- | /See:/ 'createServiceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrContainerService'
newtype CreateServiceResponse = CreateServiceResponse'{_csrContainerService :: Maybe ContainerService} deriving (Eq, Read, Show)

-- | 'CreateServiceResponse' smart constructor.
createServiceResponse :: CreateServiceResponse
createServiceResponse = CreateServiceResponse'{_csrContainerService = Nothing};

-- | The full description of your service following the create call.
csrContainerService :: Lens' CreateServiceResponse (Maybe ContainerService)
csrContainerService = lens _csrContainerService (\ s a -> s{_csrContainerService = a});
