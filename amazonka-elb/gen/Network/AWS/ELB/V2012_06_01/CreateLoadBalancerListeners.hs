{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.CreateLoadBalancerListeners
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
module Network.AWS.ELB.V2012_06_01.CreateLoadBalancerListeners
    (
    -- * Request
      CreateLoadBalancerListeners
    -- ** Request constructor
    , createLoadBalancerListeners
    -- ** Request lenses
    , clbliLoadBalancerName
    , clbliListeners

    -- * Response
    , CreateLoadBalancerListenersResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateLoadBalancerListeners' request.
createLoadBalancerListeners :: Text -- ^ 'clbliLoadBalancerName'
                            -> [Listener] -- ^ 'clbliListeners'
                            -> CreateLoadBalancerListeners
createLoadBalancerListeners p1 p2 = CreateLoadBalancerListeners
    { _clbliLoadBalancerName = p1
    , _clbliListeners = p2
    }
{-# INLINE createLoadBalancerListeners #-}

data CreateLoadBalancerListeners = CreateLoadBalancerListeners
    { _clbliLoadBalancerName :: Text
      -- ^ The name of the load balancer.
    , _clbliListeners :: [Listener]
      -- ^ A list of LoadBalancerPort, InstancePort, Protocol,
      -- InstanceProtocol, and SSLCertificateId items.
    } deriving (Show, Generic)

-- | The name of the load balancer.
clbliLoadBalancerName :: Lens' CreateLoadBalancerListeners (Text)
clbliLoadBalancerName f x =
    f (_clbliLoadBalancerName x)
        <&> \y -> x { _clbliLoadBalancerName = y }
{-# INLINE clbliLoadBalancerName #-}

-- | A list of LoadBalancerPort, InstancePort, Protocol, InstanceProtocol, and
-- SSLCertificateId items.
clbliListeners :: Lens' CreateLoadBalancerListeners ([Listener])
clbliListeners f x =
    f (_clbliListeners x)
        <&> \y -> x { _clbliListeners = y }
{-# INLINE clbliListeners #-}

instance ToQuery CreateLoadBalancerListeners where
    toQuery = genericQuery def

data CreateLoadBalancerListenersResponse = CreateLoadBalancerListenersResponse
    deriving (Eq, Show, Generic)

instance AWSRequest CreateLoadBalancerListeners where
    type Sv CreateLoadBalancerListeners = ELB
    type Rs CreateLoadBalancerListeners = CreateLoadBalancerListenersResponse

    request = post "CreateLoadBalancerListeners"
    response _ = nullaryResponse CreateLoadBalancerListenersResponse
