{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.CreateLoadBalancerListeners
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates one or more listeners on a load balancer for the specified port. If
-- a listener with the given port does not already exist, it will be created;
-- otherwise, the properties of the new listener must match the properties of
-- the existing listener. For more information, see Add a Listener to Your
-- Load Balancer in the Elastic Load Balancing Developer Guide. Create an
-- HTTPS Load Balancer listener in EC2-Classic
-- https://elasticloadbalancing.amazonaws.com/?Listeners.member.1.Protocol=https
-- &Listeners.member.1.LoadBalancerPort=443
-- &Listeners.member.1.InstancePort=443
-- &Listeners.member.1.InstanceProtocol=https
-- &Listeners.member.1.SSLCertificateId=arn:aws:iam::123456789012:server-certificate/servercert
-- &LoadBalancerName=MyHTTPSLoadBalancer &Version=2012-06-01
-- &Action=CreateLoadBalancerListeners &AUTHPARAMS
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE.
module Network.AWS.ELB.CreateLoadBalancerListeners
    (
    -- * Request
      CreateLoadBalancerListeners
    -- ** Request constructor
    , createLoadBalancerListeners
    -- ** Request lenses
    , clblLoadBalancerName
    , clblListeners

    -- * Response
    , CreateLoadBalancerListenersResponse
    -- ** Response constructor
    , createLoadBalancerListenersResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import Network.AWS.Prelude

-- | The input for the CreateLoadBalancerListeners action.
data CreateLoadBalancerListeners = CreateLoadBalancerListeners
    { _clblLoadBalancerName :: Text
    , _clblListeners :: [Listener]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateLoadBalancerListeners' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Text@
--
-- * @Listeners ::@ @[Listener]@
--
createLoadBalancerListeners :: Text -- ^ 'clblLoadBalancerName'
                            -> [Listener] -- ^ 'clblListeners'
                            -> CreateLoadBalancerListeners
createLoadBalancerListeners p1 p2 = CreateLoadBalancerListeners
    { _clblLoadBalancerName = p1
    , _clblListeners = p2
    }

-- | The name of the load balancer.
clblLoadBalancerName :: Lens' CreateLoadBalancerListeners Text
clblLoadBalancerName =
    lens _clblLoadBalancerName (\s a -> s { _clblLoadBalancerName = a })

-- | A list of LoadBalancerPort, InstancePort, Protocol, InstanceProtocol, and
-- SSLCertificateId items.
clblListeners :: Lens' CreateLoadBalancerListeners [Listener]
clblListeners = lens _clblListeners (\s a -> s { _clblListeners = a })

instance ToQuery CreateLoadBalancerListeners where
    toQuery = genericQuery def

-- | The output for the CreateLoadBalancerListeners action.
data CreateLoadBalancerListenersResponse = CreateLoadBalancerListenersResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateLoadBalancerListenersResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
createLoadBalancerListenersResponse :: CreateLoadBalancerListenersResponse
createLoadBalancerListenersResponse = CreateLoadBalancerListenersResponse

instance AWSRequest CreateLoadBalancerListeners where
    type Sv CreateLoadBalancerListeners = ELB
    type Rs CreateLoadBalancerListeners = CreateLoadBalancerListenersResponse

    request = post "CreateLoadBalancerListeners"
    response _ = nullaryResponse CreateLoadBalancerListenersResponse
