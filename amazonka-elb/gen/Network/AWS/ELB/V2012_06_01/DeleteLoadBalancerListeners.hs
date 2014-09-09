{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.DeleteLoadBalancerListeners
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes listeners from the load balancer for the specified port.
module Network.AWS.ELB.V2012_06_01.DeleteLoadBalancerListeners
    (
    -- * Request
      DeleteLoadBalancerListeners
    -- ** Request constructor
    , mkDeleteLoadBalancerListeners
    -- ** Request lenses
    , dlblLoadBalancerName
    , dlblLoadBalancerPorts

    -- * Response
    , DeleteLoadBalancerListenersResponse
    -- ** Response constructor
    , mkDeleteLoadBalancerListenersResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | The input for the DeleteLoadBalancerListeners action.
data DeleteLoadBalancerListeners = DeleteLoadBalancerListeners
    { _dlblLoadBalancerName :: Text
    , _dlblLoadBalancerPorts :: [Integer]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLoadBalancerListeners' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Text@
--
-- * @LoadBalancerPorts ::@ @[Integer]@
--
mkDeleteLoadBalancerListeners :: Text -- ^ 'dlblLoadBalancerName'
                              -> [Integer] -- ^ 'dlblLoadBalancerPorts'
                              -> DeleteLoadBalancerListeners
mkDeleteLoadBalancerListeners p1 p2 = DeleteLoadBalancerListeners
    { _dlblLoadBalancerName = p1
    , _dlblLoadBalancerPorts = p2
    }

-- | The mnemonic name associated with the load balancer.
dlblLoadBalancerName :: Lens' DeleteLoadBalancerListeners Text
dlblLoadBalancerName =
    lens _dlblLoadBalancerName (\s a -> s { _dlblLoadBalancerName = a })

-- | The client port number(s) of the load balancer listener(s) to be removed.
dlblLoadBalancerPorts :: Lens' DeleteLoadBalancerListeners [Integer]
dlblLoadBalancerPorts =
    lens _dlblLoadBalancerPorts (\s a -> s { _dlblLoadBalancerPorts = a })

instance ToQuery DeleteLoadBalancerListeners where
    toQuery = genericQuery def

-- | The output for the DeleteLoadBalancerListeners action.
data DeleteLoadBalancerListenersResponse = DeleteLoadBalancerListenersResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLoadBalancerListenersResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteLoadBalancerListenersResponse :: DeleteLoadBalancerListenersResponse
mkDeleteLoadBalancerListenersResponse = DeleteLoadBalancerListenersResponse

instance AWSRequest DeleteLoadBalancerListeners where
    type Sv DeleteLoadBalancerListeners = ELB
    type Rs DeleteLoadBalancerListeners = DeleteLoadBalancerListenersResponse

    request = post "DeleteLoadBalancerListeners"
    response _ = nullaryResponse DeleteLoadBalancerListenersResponse
