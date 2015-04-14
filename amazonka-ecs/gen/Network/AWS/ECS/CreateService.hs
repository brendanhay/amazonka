{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ECS.CreateService
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
-- definition. If the number of tasks running in a service drops below 'desiredCount', Amazon ECS will spawn another instantiation of the task in the specified
-- cluster.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_CreateService.html>
module Network.AWS.ECS.CreateService
    (
    -- * Request
      CreateService
    -- ** Request constructor
    , createService
    -- ** Request lenses
    , cs1ClientToken
    , cs1Cluster
    , cs1DesiredCount
    , cs1LoadBalancers
    , cs1Role
    , cs1ServiceName
    , cs1TaskDefinition

    -- * Response
    , CreateServiceResponse
    -- ** Response constructor
    , createServiceResponse
    -- ** Response lenses
    , csrService
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.ECS.Types
import qualified GHC.Exts

data CreateService = CreateService
    { _cs1ClientToken    :: Maybe Text
    , _cs1Cluster        :: Maybe Text
    , _cs1DesiredCount   :: Maybe Int
    , _cs1LoadBalancers  :: List "loadBalancers" LoadBalancer
    , _cs1Role           :: Maybe Text
    , _cs1ServiceName    :: Text
    , _cs1TaskDefinition :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'CreateService' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cs1ClientToken' @::@ 'Maybe' 'Text'
--
-- * 'cs1Cluster' @::@ 'Maybe' 'Text'
--
-- * 'cs1DesiredCount' @::@ 'Maybe' 'Int'
--
-- * 'cs1LoadBalancers' @::@ ['LoadBalancer']
--
-- * 'cs1Role' @::@ 'Maybe' 'Text'
--
-- * 'cs1ServiceName' @::@ 'Text'
--
-- * 'cs1TaskDefinition' @::@ 'Maybe' 'Text'
--
createService :: Text -- ^ 'cs1ServiceName'
              -> CreateService
createService p1 = CreateService
    { _cs1ServiceName    = p1
    , _cs1Cluster        = Nothing
    , _cs1TaskDefinition = Nothing
    , _cs1LoadBalancers  = mempty
    , _cs1DesiredCount   = Nothing
    , _cs1ClientToken    = Nothing
    , _cs1Role           = Nothing
    }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of
-- the request. Up to 32 ASCII characters are allowed.
cs1ClientToken :: Lens' CreateService (Maybe Text)
cs1ClientToken = lens _cs1ClientToken (\s a -> s { _cs1ClientToken = a })

-- | The short name or full Amazon Resource Name (ARN) of the cluster that you
-- want to run your service on. If you do not specify a cluster, the default
-- cluster is assumed.
cs1Cluster :: Lens' CreateService (Maybe Text)
cs1Cluster = lens _cs1Cluster (\s a -> s { _cs1Cluster = a })

-- | The number of instantiations of the specified task definition that you would
-- like to place and keep running on your cluster.
cs1DesiredCount :: Lens' CreateService (Maybe Int)
cs1DesiredCount = lens _cs1DesiredCount (\s a -> s { _cs1DesiredCount = a })

-- | A list of load balancer objects, containing the load balancer name, the
-- container name (as it appears in a container definition), and the container
-- port to access from the load balancer.
cs1LoadBalancers :: Lens' CreateService [LoadBalancer]
cs1LoadBalancers = lens _cs1LoadBalancers (\s a -> s { _cs1LoadBalancers = a }) . _List

-- | The name or full Amazon Resource Name (ARN) of the IAM role that allows your
-- Amazon ECS container agent to make calls to your load balancer on your
-- behalf. This parameter is only required if you are using a load balancer with
-- your service.
cs1Role :: Lens' CreateService (Maybe Text)
cs1Role = lens _cs1Role (\s a -> s { _cs1Role = a })

-- | The name of your service. Up to 255 letters (uppercase and lowercase),
-- numbers, hyphens, and underscores are allowed.
cs1ServiceName :: Lens' CreateService Text
cs1ServiceName = lens _cs1ServiceName (\s a -> s { _cs1ServiceName = a })

-- | The 'family' and 'revision' ('family:revision') or full Amazon Resource Name (ARN)
-- of the task definition that you want to run in your service.
cs1TaskDefinition :: Lens' CreateService (Maybe Text)
cs1TaskDefinition =
    lens _cs1TaskDefinition (\s a -> s { _cs1TaskDefinition = a })

newtype CreateServiceResponse = CreateServiceResponse
    { _csrService :: Maybe ContainerService
    } deriving (Eq, Read, Show)

-- | 'CreateServiceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrService' @::@ 'Maybe' 'ContainerService'
--
createServiceResponse :: CreateServiceResponse
createServiceResponse = CreateServiceResponse
    { _csrService = Nothing
    }

-- | The full description of your service following the create call.
csrService :: Lens' CreateServiceResponse (Maybe ContainerService)
csrService = lens _csrService (\s a -> s { _csrService = a })

instance ToPath CreateService where
    toPath = const "/"

instance ToQuery CreateService where
    toQuery = const mempty

instance ToHeaders CreateService

instance ToJSON CreateService where
    toJSON CreateService{..} = object
        [ "cluster"        .= _cs1Cluster
        , "serviceName"    .= _cs1ServiceName
        , "taskDefinition" .= _cs1TaskDefinition
        , "loadBalancers"  .= _cs1LoadBalancers
        , "desiredCount"   .= _cs1DesiredCount
        , "clientToken"    .= _cs1ClientToken
        , "role"           .= _cs1Role
        ]

instance AWSRequest CreateService where
    type Sv CreateService = ECS
    type Rs CreateService = CreateServiceResponse

    request  = post "CreateService"
    response = jsonResponse

instance FromJSON CreateServiceResponse where
    parseJSON = withObject "CreateServiceResponse" $ \o -> CreateServiceResponse
        <$> o .:? "service"
