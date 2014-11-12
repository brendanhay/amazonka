{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.ELB.DeregisterInstancesFromLoadBalancer
    (
    -- * Request
      DeregisterEndPointsInput
    -- ** Request constructor
    , deregisterInstancesFromLoadBalancer
    -- ** Request lenses
    , depiInstances
    , depiLoadBalancerName

    -- * Response
    , DeregisterEndPointsOutput
    -- ** Response constructor
    , deregisterInstancesFromLoadBalancerResponse
    -- ** Response lenses
    , depoInstances
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data DeregisterEndPointsInput = DeregisterEndPointsInput
    { _depiInstances        :: [Instance]
    , _depiLoadBalancerName :: Text
    } deriving (Eq, Show, Generic)

-- | 'DeregisterEndPointsInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'depiInstances' @::@ ['Instance']
--
-- * 'depiLoadBalancerName' @::@ 'Text'
--
deregisterInstancesFromLoadBalancer :: Text -- ^ 'depiLoadBalancerName'
                                    -> DeregisterEndPointsInput
deregisterInstancesFromLoadBalancer p1 = DeregisterEndPointsInput
    { _depiLoadBalancerName = p1
    , _depiInstances        = mempty
    }

-- | A list of EC2 instance IDs consisting of all instances to be
-- deregistered.
depiInstances :: Lens' DeregisterEndPointsInput [Instance]
depiInstances = lens _depiInstances (\s a -> s { _depiInstances = a })

-- | The name associated with the load balancer.
depiLoadBalancerName :: Lens' DeregisterEndPointsInput Text
depiLoadBalancerName =
    lens _depiLoadBalancerName (\s a -> s { _depiLoadBalancerName = a })

instance ToQuery DeregisterEndPointsInput

instance ToPath DeregisterEndPointsInput where
    toPath = const "/"

newtype DeregisterEndPointsOutput = DeregisterEndPointsOutput
    { _depoInstances :: [Instance]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList DeregisterEndPointsOutput where
    type Item DeregisterEndPointsOutput = Instance

    fromList = DeregisterEndPointsOutput . fromList
    toList   = toList . _depoInstances

-- | 'DeregisterEndPointsOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'depoInstances' @::@ ['Instance']
--
deregisterInstancesFromLoadBalancerResponse :: DeregisterEndPointsOutput
deregisterInstancesFromLoadBalancerResponse = DeregisterEndPointsOutput
    { _depoInstances = mempty
    }

-- | An updated list of remaining instances registered with the load balancer.
depoInstances :: Lens' DeregisterEndPointsOutput [Instance]
depoInstances = lens _depoInstances (\s a -> s { _depoInstances = a })

instance FromXML DeregisterEndPointsOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeregisterEndPointsOutput"

instance AWSRequest DeregisterEndPointsInput where
    type Sv DeregisterEndPointsInput = ELB
    type Rs DeregisterEndPointsInput = DeregisterEndPointsOutput

    request  = post "DeregisterInstancesFromLoadBalancer"
    response = xmlResponse $ \h x -> DeregisterEndPointsOutput
        <$> x %| "Instances"
