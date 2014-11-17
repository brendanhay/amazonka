{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.DeregisterInstancesFromLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deregisters instances from the load balancer. Once the instance is
-- deregistered, it will stop receiving traffic from the load balancer. In
-- order to successfully call this API, the same account credentials as those
-- used to create the load balancer must be provided. For more information,
-- see De-register and Register Amazon EC2 Instances in the Elastic Load
-- Balancing Developer Guide. You can use DescribeLoadBalancers to verify if
-- the instance is deregistered from the load balancer.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeregisterInstancesFromLoadBalancer.html>
module Network.AWS.ELB.DeregisterInstancesFromLoadBalancer
    (
    -- * Request
      DeregisterInstancesFromLoadBalancer
    -- ** Request constructor
    , deregisterInstancesFromLoadBalancer
    -- ** Request lenses
    , diflbInstances
    , diflbLoadBalancerName

    -- * Response
    , DeregisterInstancesFromLoadBalancerResponse
    -- ** Response constructor
    , deregisterInstancesFromLoadBalancerResponse
    -- ** Response lenses
    , diflbrInstances
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

data DeregisterInstancesFromLoadBalancer = DeregisterInstancesFromLoadBalancer
    { _diflbInstances        :: [Instance]
    , _diflbLoadBalancerName :: Text
    } deriving (Eq, Show, Generic)

-- | 'DeregisterInstancesFromLoadBalancer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diflbInstances' @::@ ['Instance']
--
-- * 'diflbLoadBalancerName' @::@ 'Text'
--
deregisterInstancesFromLoadBalancer :: Text -- ^ 'diflbLoadBalancerName'
                                    -> DeregisterInstancesFromLoadBalancer
deregisterInstancesFromLoadBalancer p1 = DeregisterInstancesFromLoadBalancer
    { _diflbLoadBalancerName = p1
    , _diflbInstances        = mempty
    }

-- | A list of EC2 instance IDs consisting of all instances to be
-- deregistered.
diflbInstances :: Lens' DeregisterInstancesFromLoadBalancer [Instance]
diflbInstances = lens _diflbInstances (\s a -> s { _diflbInstances = a })

-- | The name associated with the load balancer.
diflbLoadBalancerName :: Lens' DeregisterInstancesFromLoadBalancer Text
diflbLoadBalancerName =
    lens _diflbLoadBalancerName (\s a -> s { _diflbLoadBalancerName = a })

newtype DeregisterInstancesFromLoadBalancerResponse = DeregisterInstancesFromLoadBalancerResponse
    { _diflbrInstances :: [Instance]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DeregisterInstancesFromLoadBalancerResponse where
    type Item DeregisterInstancesFromLoadBalancerResponse = Instance

    fromList = DeregisterInstancesFromLoadBalancerResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _diflbrInstances

-- | 'DeregisterInstancesFromLoadBalancerResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diflbrInstances' @::@ ['Instance']
--
deregisterInstancesFromLoadBalancerResponse :: DeregisterInstancesFromLoadBalancerResponse
deregisterInstancesFromLoadBalancerResponse = DeregisterInstancesFromLoadBalancerResponse
    { _diflbrInstances = mempty
    }

-- | An updated list of remaining instances registered with the load balancer.
diflbrInstances :: Lens' DeregisterInstancesFromLoadBalancerResponse [Instance]
diflbrInstances = lens _diflbrInstances (\s a -> s { _diflbrInstances = a })

instance AWSRequest DeregisterInstancesFromLoadBalancer where
    type Sv DeregisterInstancesFromLoadBalancer = ELB
    type Rs DeregisterInstancesFromLoadBalancer = DeregisterInstancesFromLoadBalancerResponse

    request  = post "DeregisterInstancesFromLoadBalancer"
    response = xmlResponse

instance FromXML DeregisterInstancesFromLoadBalancerResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeregisterInstancesFromLoadBalancerResponse"

instance ToPath DeregisterInstancesFromLoadBalancer where
    toPath = const "/"

instance ToHeaders DeregisterInstancesFromLoadBalancer

instance ToQuery DeregisterInstancesFromLoadBalancer
