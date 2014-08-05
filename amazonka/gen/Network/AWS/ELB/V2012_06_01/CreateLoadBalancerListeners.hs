{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
-- Load Balancer in the Elastic Load Balancing Developer Guide. Create HTTPS
-- load balancer listener in EC2-Classic
-- https://elasticloadbalancing.amazonaws.com/?Listeners.member.1.Protocol=https
-- &Listeners.member.1.LoadBalancerPort=443
-- &Listeners.member.1.InstancePort=443
-- &Listeners.member.1.InstanceProtocol=https
-- &Listeners.member.1.SSLCertificateId=arn:aws:iam::123456789012:server-certificate/servercert
-- &LoadBalancerName=MyHTTPSLoadBalancer &Version=2012-06-01
-- &Action=CreateLoadBalancerListeners &AUTHPARAMS
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE.
module Network.AWS.ELB.V2012_06_01.CreateLoadBalancerListeners where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

data CreateLoadBalancerListeners = CreateLoadBalancerListeners
    { _clbliLoadBalancerName :: Text
      -- ^ The name of the load balancer.
    , _clbliListeners :: [Listener]
      -- ^ A list of LoadBalancerPort, InstancePort, Protocol,
      -- InstanceProtocol, and SSLCertificateId items.
    } deriving (Show, Generic)

makeLenses ''CreateLoadBalancerListeners

instance ToQuery CreateLoadBalancerListeners where
    toQuery = genericToQuery def

data CreateLoadBalancerListenersResponse = CreateLoadBalancerListenersResponse
    deriving (Eq, Show, Generic)

makeLenses ''CreateLoadBalancerListenersResponse

instance AWSRequest CreateLoadBalancerListeners where
    type Sv CreateLoadBalancerListeners = ELB
    type Rs CreateLoadBalancerListeners = CreateLoadBalancerListenersResponse

    request = post "CreateLoadBalancerListeners"
    response _ _ = return (Right CreateLoadBalancerListenersResponse)
