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
-- Load Balancer in the Elastic Load Balancing Developer Guide.
module Network.AWS.ELB.CreateLoadBalancerListeners
    (
    -- * Request
      CreateLoadBalancerListeners
    -- ** Request constructor
    , createLoadBalancerListeners
    -- ** Request lenses
    , clblListeners
    , clblLoadBalancerName

    -- * Response
    , CreateLoadBalancerListenersResponse
    -- ** Response constructor
    , createLoadBalancerListenersResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data CreateLoadBalancerListeners = CreateLoadBalancerListeners
    { _clblListeners        :: [Listener]
    , _clblLoadBalancerName :: Text
    } deriving (Eq, Show, Generic)

-- | 'CreateLoadBalancerListeners' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clblListeners' @::@ ['Listener']
--
-- * 'clblLoadBalancerName' @::@ 'Text'
--
createLoadBalancerListeners :: Text -- ^ 'clblLoadBalancerName'
                            -> CreateLoadBalancerListeners
createLoadBalancerListeners p1 = CreateLoadBalancerListeners
    { _clblLoadBalancerName = p1
    , _clblListeners        = mempty
    }

-- | A list of LoadBalancerPort, InstancePort, Protocol, InstanceProtocol, and
-- SSLCertificateId items.
clblListeners :: Lens' CreateLoadBalancerListeners [Listener]
clblListeners = lens _clblListeners (\s a -> s { _clblListeners = a })

-- | The name of the load balancer.
clblLoadBalancerName :: Lens' CreateLoadBalancerListeners Text
clblLoadBalancerName =
    lens _clblLoadBalancerName (\s a -> s { _clblLoadBalancerName = a })

instance ToQuery CreateLoadBalancerListeners

instance ToPath CreateLoadBalancerListeners where
    toPath = const "/"

data CreateLoadBalancerListenersResponse = CreateLoadBalancerListenersResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateLoadBalancerListenersResponse' constructor.
createLoadBalancerListenersResponse :: CreateLoadBalancerListenersResponse
createLoadBalancerListenersResponse = CreateLoadBalancerListenersResponse

instance FromXML CreateLoadBalancerListenersResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateLoadBalancerListenersResponse"

instance AWSRequest CreateLoadBalancerListeners where
    type Sv CreateLoadBalancerListeners = ELB
    type Rs CreateLoadBalancerListeners = CreateLoadBalancerListenersResponse

    request  = post "CreateLoadBalancerListeners"
    response = nullaryResponse CreateLoadBalancerListenersResponse
