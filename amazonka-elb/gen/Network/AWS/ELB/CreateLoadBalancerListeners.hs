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
      CreateLoadBalancerListenerInput
    -- ** Request constructor
    , createLoadBalancerListenerInput
    -- ** Request lenses
    , clbliListeners
    , clbliLoadBalancerName

    -- * Response
    , CreateLoadBalancerListenersResponse
    -- ** Response constructor
    , createLoadBalancerListenersResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data CreateLoadBalancerListenerInput = CreateLoadBalancerListenerInput
    { _clbliListeners        :: [Listener]
    , _clbliLoadBalancerName :: Text
    } deriving (Eq, Show, Generic)

-- | 'CreateLoadBalancerListenerInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clbliListeners' @::@ ['Listener']
--
-- * 'clbliLoadBalancerName' @::@ 'Text'
--
createLoadBalancerListenerInput :: Text -- ^ 'clbliLoadBalancerName'
                                -> CreateLoadBalancerListenerInput
createLoadBalancerListenerInput p1 = CreateLoadBalancerListenerInput
    { _clbliLoadBalancerName = p1
    , _clbliListeners        = mempty
    }

-- | A list of LoadBalancerPort, InstancePort, Protocol, InstanceProtocol, and
-- SSLCertificateId items.
clbliListeners :: Lens' CreateLoadBalancerListenerInput [Listener]
clbliListeners = lens _clbliListeners (\s a -> s { _clbliListeners = a })

-- | The name of the load balancer.
clbliLoadBalancerName :: Lens' CreateLoadBalancerListenerInput Text
clbliLoadBalancerName =
    lens _clbliLoadBalancerName (\s a -> s { _clbliLoadBalancerName = a })

instance ToQuery CreateLoadBalancerListenerInput

instance ToPath CreateLoadBalancerListenerInput where
    toPath = const "/"

data CreateLoadBalancerListenersResponse = CreateLoadBalancerListenersResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateLoadBalancerListenersResponse' constructor.
createLoadBalancerListenersResponse :: CreateLoadBalancerListenersResponse
createLoadBalancerListenersResponse = CreateLoadBalancerListenersResponse

instance FromXML CreateLoadBalancerListenersResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateLoadBalancerListenersResponse"

instance AWSRequest CreateLoadBalancerListenerInput where
    type Sv CreateLoadBalancerListenerInput = ELB
    type Rs CreateLoadBalancerListenerInput = CreateLoadBalancerListenersResponse

    request  = post "CreateLoadBalancerListeners"
    response = nullaryResponse CreateLoadBalancerListenersResponse
