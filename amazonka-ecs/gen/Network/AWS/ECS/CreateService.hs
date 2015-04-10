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
    , csClientToken
    , csCluster
    , csDesiredCount
    , csLoadBalancers
    , csRole
    , csServiceName
    , csTaskDefinition

    -- * Response
    , CreateServiceResponse
    -- ** Response constructor
    , createServiceResponse
    -- ** Response lenses
    , csrService
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.ECS.Types
import qualified GHC.Exts

data CreateService = CreateService
    { _csClientToken    :: Maybe Text
    , _csCluster        :: Maybe Text
    , _csDesiredCount   :: Maybe Int
    , _csLoadBalancers  :: List "loadBalancers" LoadBalancer
    , _csRole           :: Maybe Text
    , _csServiceName    :: Text
    , _csTaskDefinition :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'CreateService' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csClientToken' @::@ 'Maybe' 'Text'
--
-- * 'csCluster' @::@ 'Maybe' 'Text'
--
-- * 'csDesiredCount' @::@ 'Maybe' 'Int'
--
-- * 'csLoadBalancers' @::@ ['LoadBalancer']
--
-- * 'csRole' @::@ 'Maybe' 'Text'
--
-- * 'csServiceName' @::@ 'Text'
--
-- * 'csTaskDefinition' @::@ 'Maybe' 'Text'
--
createService :: Text -- ^ 'csServiceName'
              -> CreateService
createService p1 = CreateService
    { _csServiceName    = p1
    , _csCluster        = Nothing
    , _csTaskDefinition = Nothing
    , _csLoadBalancers  = mempty
    , _csDesiredCount   = Nothing
    , _csClientToken    = Nothing
    , _csRole           = Nothing
    }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of
-- the request. Up to 64 ASCII characters are allowed.
csClientToken :: Lens' CreateService (Maybe Text)
csClientToken = lens _csClientToken (\s a -> s { _csClientToken = a })

-- | The short name or full Amazon Resource Name (ARN) of the cluster that you
-- want to run your service on. If you do not specify a cluster, the default
-- cluster is assumed.
csCluster :: Lens' CreateService (Maybe Text)
csCluster = lens _csCluster (\s a -> s { _csCluster = a })

-- | The number of instantiations of the specified task definition that you would
-- like to place and keep running on your cluster.
csDesiredCount :: Lens' CreateService (Maybe Int)
csDesiredCount = lens _csDesiredCount (\s a -> s { _csDesiredCount = a })

-- | A list of load balancer objects, containing the load balancer name, the
-- container name (as it appears in a container definition), and the container
-- port to access from the load balancer.
csLoadBalancers :: Lens' CreateService [LoadBalancer]
csLoadBalancers = lens _csLoadBalancers (\s a -> s { _csLoadBalancers = a }) . _List

-- | The name or full Amazon Resource Name (ARN) of the IAM role that allows your
-- Amazon ECS container agent to make calls to your load balancer on your
-- behalf. This parameter is only required if you are using a load balancer with
-- your service.
csRole :: Lens' CreateService (Maybe Text)
csRole = lens _csRole (\s a -> s { _csRole = a })

-- | The name of your service. Up to 255 letters (uppercase and lowercase),
-- numbers, hyphens, and underscores are allowed.
csServiceName :: Lens' CreateService Text
csServiceName = lens _csServiceName (\s a -> s { _csServiceName = a })

-- | The 'family' and 'revision' ('family:revision') or full Amazon Resource Name (ARN)
-- of the task definition that you want to run in your service.
csTaskDefinition :: Lens' CreateService (Maybe Text)
csTaskDefinition = lens _csTaskDefinition (\s a -> s { _csTaskDefinition = a })

newtype CreateServiceResponse = CreateServiceResponse
    { _csrService :: Maybe Service
    } deriving (Eq, Read, Show)

-- | 'CreateServiceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrService' @::@ 'Maybe' 'Service'
--
createServiceResponse :: CreateServiceResponse
createServiceResponse = CreateServiceResponse
    { _csrService = Nothing
    }

-- | The full description of your service following the create call.
csrService :: Lens' CreateServiceResponse (Maybe Service)
csrService = lens _csrService (\s a -> s { _csrService = a })

instance ToPath CreateService where
    toPath = const "/"

instance ToQuery CreateService where
    toQuery = const mempty

instance ToHeaders CreateService

instance ToJSON CreateService where
    toJSON CreateService{..} = object
        [ "cluster"        .= _csCluster
        , "serviceName"    .= _csServiceName
        , "taskDefinition" .= _csTaskDefinition
        , "loadBalancers"  .= _csLoadBalancers
        , "desiredCount"   .= _csDesiredCount
        , "clientToken"    .= _csClientToken
        , "role"           .= _csRole
        ]

instance AWSRequest CreateService where
    type Sv CreateService = ECS
    type Rs CreateService = CreateServiceResponse

    request  = post "CreateService"
    response = jsonResponse

instance FromJSON CreateServiceResponse where
    parseJSON = withObject "CreateServiceResponse" $ \o -> CreateServiceResponse
        <$> o .:? "service"
