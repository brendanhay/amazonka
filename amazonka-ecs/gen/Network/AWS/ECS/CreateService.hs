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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs and maintains a desired number of tasks from a specified task
-- definition. If the number of tasks running in a service drops below
-- 'desiredCount', Amazon ECS will spawn another instantiation of the task
-- in the specified cluster.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_CreateService.html AWS API Reference> for CreateService.
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
    , cServiceName
    , cTaskDefinition
    , cDesiredCount

    -- * Destructuring the Response
    , createServiceResponse
    , CreateServiceResponse
    -- * Response Lenses
    , csrsService
    , csrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createService' smart constructor.
data CreateService = CreateService'
    { _cCluster        :: !(Maybe Text)
    , _cClientToken    :: !(Maybe Text)
    , _cLoadBalancers  :: !(Maybe [LoadBalancer])
    , _cRole           :: !(Maybe Text)
    , _cServiceName    :: !Text
    , _cTaskDefinition :: !Text
    , _cDesiredCount   :: !Int
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
    , _cServiceName = pServiceName_
    , _cTaskDefinition = pTaskDefinition_
    , _cDesiredCount = pDesiredCount_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- you want to run your service on. If you do not specify a cluster, the
-- default cluster is assumed.
cCluster :: Lens' CreateService (Maybe Text)
cCluster = lens _cCluster (\ s a -> s{_cCluster = a});

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. Up to 32 ASCII characters are allowed.
cClientToken :: Lens' CreateService (Maybe Text)
cClientToken = lens _cClientToken (\ s a -> s{_cClientToken = a});

-- | A list of load balancer objects, containing the load balancer name, the
-- container name (as it appears in a container definition), and the
-- container port to access from the load balancer.
cLoadBalancers :: Lens' CreateService [LoadBalancer]
cLoadBalancers = lens _cLoadBalancers (\ s a -> s{_cLoadBalancers = a}) . _Default . _Coerce;

-- | The name or full Amazon Resource Name (ARN) of the IAM role that allows
-- your Amazon ECS container agent to make calls to your load balancer on
-- your behalf. This parameter is only required if you are using a load
-- balancer with your service.
cRole :: Lens' CreateService (Maybe Text)
cRole = lens _cRole (\ s a -> s{_cRole = a});

-- | The name of your service. Up to 255 letters (uppercase and lowercase),
-- numbers, hyphens, and underscores are allowed. Service names must be
-- unique within a cluster, but you can have similarly named services in
-- multiple clusters within a region or across multiple regions.
cServiceName :: Lens' CreateService Text
cServiceName = lens _cServiceName (\ s a -> s{_cServiceName = a});

-- | The 'family' and 'revision' ('family:revision') or full Amazon Resource
-- Name (ARN) of the task definition that you want to run in your service.
-- If a 'revision' is not specified, the latest 'ACTIVE' revision is used.
cTaskDefinition :: Lens' CreateService Text
cTaskDefinition = lens _cTaskDefinition (\ s a -> s{_cTaskDefinition = a});

-- | The number of instantiations of the specified task definition that you
-- would like to place and keep running on your cluster.
cDesiredCount :: Lens' CreateService Int
cDesiredCount = lens _cDesiredCount (\ s a -> s{_cDesiredCount = a});

instance AWSRequest CreateService where
        type Rs CreateService = CreateServiceResponse
        request = postJSON eCS
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
              (catMaybes
                 [("cluster" .=) <$> _cCluster,
                  ("clientToken" .=) <$> _cClientToken,
                  ("loadBalancers" .=) <$> _cLoadBalancers,
                  ("role" .=) <$> _cRole,
                  Just ("serviceName" .= _cServiceName),
                  Just ("taskDefinition" .= _cTaskDefinition),
                  Just ("desiredCount" .= _cDesiredCount)])

instance ToPath CreateService where
        toPath = const "/"

instance ToQuery CreateService where
        toQuery = const mempty

-- | /See:/ 'createServiceResponse' smart constructor.
data CreateServiceResponse = CreateServiceResponse'
    { _csrsService :: !(Maybe ContainerService)
    , _csrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateServiceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsService'
--
-- * 'csrsStatus'
createServiceResponse
    :: Int -- ^ 'csrsStatus'
    -> CreateServiceResponse
createServiceResponse pStatus_ =
    CreateServiceResponse'
    { _csrsService = Nothing
    , _csrsStatus = pStatus_
    }

-- | The full description of your service following the create call.
csrsService :: Lens' CreateServiceResponse (Maybe ContainerService)
csrsService = lens _csrsService (\ s a -> s{_csrsService = a});

-- | The response status code.
csrsStatus :: Lens' CreateServiceResponse Int
csrsStatus = lens _csrsStatus (\ s a -> s{_csrsStatus = a});
