{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.CreateService
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Runs and maintains a desired number of tasks from a specified task
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
    , csrqCluster
    , csrqClientToken
    , csrqLoadBalancers
    , csrqRole
    , csrqServiceName
    , csrqTaskDefinition
    , csrqDesiredCount

    -- * Response
    , CreateServiceResponse
    -- ** Response constructor
    , createServiceResponse
    -- ** Response lenses
    , csrsService
    , csrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createService' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrqCluster'
--
-- * 'csrqClientToken'
--
-- * 'csrqLoadBalancers'
--
-- * 'csrqRole'
--
-- * 'csrqServiceName'
--
-- * 'csrqTaskDefinition'
--
-- * 'csrqDesiredCount'
data CreateService = CreateService'
    { _csrqCluster        :: !(Maybe Text)
    , _csrqClientToken    :: !(Maybe Text)
    , _csrqLoadBalancers  :: !(Maybe [LoadBalancer])
    , _csrqRole           :: !(Maybe Text)
    , _csrqServiceName    :: !Text
    , _csrqTaskDefinition :: !Text
    , _csrqDesiredCount   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateService' smart constructor.
createService :: Text -> Text -> Int -> CreateService
createService pServiceName pTaskDefinition pDesiredCount =
    CreateService'
    { _csrqCluster = Nothing
    , _csrqClientToken = Nothing
    , _csrqLoadBalancers = Nothing
    , _csrqRole = Nothing
    , _csrqServiceName = pServiceName
    , _csrqTaskDefinition = pTaskDefinition
    , _csrqDesiredCount = pDesiredCount
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- you want to run your service on. If you do not specify a cluster, the
-- default cluster is assumed.
csrqCluster :: Lens' CreateService (Maybe Text)
csrqCluster = lens _csrqCluster (\ s a -> s{_csrqCluster = a});

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. Up to 32 ASCII characters are allowed.
csrqClientToken :: Lens' CreateService (Maybe Text)
csrqClientToken = lens _csrqClientToken (\ s a -> s{_csrqClientToken = a});

-- | A list of load balancer objects, containing the load balancer name, the
-- container name (as it appears in a container definition), and the
-- container port to access from the load balancer.
csrqLoadBalancers :: Lens' CreateService [LoadBalancer]
csrqLoadBalancers = lens _csrqLoadBalancers (\ s a -> s{_csrqLoadBalancers = a}) . _Default;

-- | The name or full Amazon Resource Name (ARN) of the IAM role that allows
-- your Amazon ECS container agent to make calls to your load balancer on
-- your behalf. This parameter is only required if you are using a load
-- balancer with your service.
csrqRole :: Lens' CreateService (Maybe Text)
csrqRole = lens _csrqRole (\ s a -> s{_csrqRole = a});

-- | The name of your service. Up to 255 letters (uppercase and lowercase),
-- numbers, hyphens, and underscores are allowed. Service names must be
-- unique within a cluster, but you can have similarly named services in
-- multiple clusters within a region or across multiple regions.
csrqServiceName :: Lens' CreateService Text
csrqServiceName = lens _csrqServiceName (\ s a -> s{_csrqServiceName = a});

-- | The @family@ and @revision@ (@family:revision@) or full Amazon Resource
-- Name (ARN) of the task definition that you want to run in your service.
-- If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
csrqTaskDefinition :: Lens' CreateService Text
csrqTaskDefinition = lens _csrqTaskDefinition (\ s a -> s{_csrqTaskDefinition = a});

-- | The number of instantiations of the specified task definition that you
-- would like to place and keep running on your cluster.
csrqDesiredCount :: Lens' CreateService Int
csrqDesiredCount = lens _csrqDesiredCount (\ s a -> s{_csrqDesiredCount = a});

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
              ["cluster" .= _csrqCluster,
               "clientToken" .= _csrqClientToken,
               "loadBalancers" .= _csrqLoadBalancers,
               "role" .= _csrqRole,
               "serviceName" .= _csrqServiceName,
               "taskDefinition" .= _csrqTaskDefinition,
               "desiredCount" .= _csrqDesiredCount]

instance ToPath CreateService where
        toPath = const "/"

instance ToQuery CreateService where
        toQuery = const mempty

-- | /See:/ 'createServiceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrsService'
--
-- * 'csrsStatus'
data CreateServiceResponse = CreateServiceResponse'
    { _csrsService :: !(Maybe ContainerService)
    , _csrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateServiceResponse' smart constructor.
createServiceResponse :: Int -> CreateServiceResponse
createServiceResponse pStatus =
    CreateServiceResponse'
    { _csrsService = Nothing
    , _csrsStatus = pStatus
    }

-- | The full description of your service following the create call.
csrsService :: Lens' CreateServiceResponse (Maybe ContainerService)
csrsService = lens _csrsService (\ s a -> s{_csrsService = a});

-- | FIXME: Undocumented member.
csrsStatus :: Lens' CreateServiceResponse Int
csrsStatus = lens _csrsStatus (\ s a -> s{_csrsStatus = a});
