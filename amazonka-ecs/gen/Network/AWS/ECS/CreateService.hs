{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ECS.CreateService
-- Copyright   : (c) 2013-2015 Brendan Hay
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
    , creLoadBalancers
    , creRole
    , creServiceName
    , creTaskDefinition
    , creDesiredCount

    -- * Response
    , CreateServiceResponse
    -- ** Response constructor
    , createServiceResponse
    -- ** Response lenses
    , csrService
    , csrStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createService' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creCluster'
--
-- * 'creClientToken'
--
-- * 'creLoadBalancers'
--
-- * 'creRole'
--
-- * 'creServiceName'
--
-- * 'creTaskDefinition'
--
-- * 'creDesiredCount'
data CreateService = CreateService'
    { _creCluster        :: !(Maybe Text)
    , _creClientToken    :: !(Maybe Text)
    , _creLoadBalancers  :: !(Maybe [LoadBalancer])
    , _creRole           :: !(Maybe Text)
    , _creServiceName    :: !Text
    , _creTaskDefinition :: !Text
    , _creDesiredCount   :: !Int
    } deriving (Eq,Read,Show)

-- | 'CreateService' smart constructor.
createService :: Text -> Text -> Int -> CreateService
createService pServiceName pTaskDefinition pDesiredCount =
    CreateService'
    { _creCluster = Nothing
    , _creClientToken = Nothing
    , _creLoadBalancers = Nothing
    , _creRole = Nothing
    , _creServiceName = pServiceName
    , _creTaskDefinition = pTaskDefinition
    , _creDesiredCount = pDesiredCount
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- you want to run your service on. If you do not specify a cluster, the
-- default cluster is assumed.
creCluster :: Lens' CreateService (Maybe Text)
creCluster = lens _creCluster (\ s a -> s{_creCluster = a});

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. Up to 32 ASCII characters are allowed.
creClientToken :: Lens' CreateService (Maybe Text)
creClientToken = lens _creClientToken (\ s a -> s{_creClientToken = a});

-- | A list of load balancer objects, containing the load balancer name, the
-- container name (as it appears in a container definition), and the
-- container port to access from the load balancer.
creLoadBalancers :: Lens' CreateService [LoadBalancer]
creLoadBalancers = lens _creLoadBalancers (\ s a -> s{_creLoadBalancers = a}) . _Default;

-- | The name or full Amazon Resource Name (ARN) of the IAM role that allows
-- your Amazon ECS container agent to make calls to your load balancer on
-- your behalf. This parameter is only required if you are using a load
-- balancer with your service.
creRole :: Lens' CreateService (Maybe Text)
creRole = lens _creRole (\ s a -> s{_creRole = a});

-- | The name of your service. Up to 255 letters (uppercase and lowercase),
-- numbers, hyphens, and underscores are allowed.
creServiceName :: Lens' CreateService Text
creServiceName = lens _creServiceName (\ s a -> s{_creServiceName = a});

-- | The @family@ and @revision@ (@family:revision@) or full Amazon Resource
-- Name (ARN) of the task definition that you want to run in your service.
-- If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
creTaskDefinition :: Lens' CreateService Text
creTaskDefinition = lens _creTaskDefinition (\ s a -> s{_creTaskDefinition = a});

-- | The number of instantiations of the specified task definition that you
-- would like to place and keep running on your cluster.
creDesiredCount :: Lens' CreateService Int
creDesiredCount = lens _creDesiredCount (\ s a -> s{_creDesiredCount = a});

instance AWSRequest CreateService where
        type Sv CreateService = ECS
        type Rs CreateService = CreateServiceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateServiceResponse' <$>
                   (x .?> "service") <*> (pure (fromEnum s)))

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
               "loadBalancers" .= _creLoadBalancers,
               "role" .= _creRole, "serviceName" .= _creServiceName,
               "taskDefinition" .= _creTaskDefinition,
               "desiredCount" .= _creDesiredCount]

instance ToPath CreateService where
        toPath = const "/"

instance ToQuery CreateService where
        toQuery = const mempty

-- | /See:/ 'createServiceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrService'
--
-- * 'csrStatus'
data CreateServiceResponse = CreateServiceResponse'
    { _csrService :: !(Maybe ContainerService)
    , _csrStatus  :: !Int
    } deriving (Eq,Read,Show)

-- | 'CreateServiceResponse' smart constructor.
createServiceResponse :: Int -> CreateServiceResponse
createServiceResponse pStatus =
    CreateServiceResponse'
    { _csrService = Nothing
    , _csrStatus = pStatus
    }

-- | The full description of your service following the create call.
csrService :: Lens' CreateServiceResponse (Maybe ContainerService)
csrService = lens _csrService (\ s a -> s{_csrService = a});

-- | FIXME: Undocumented member.
csrStatus :: Lens' CreateServiceResponse Int
csrStatus = lens _csrStatus (\ s a -> s{_csrStatus = a});
