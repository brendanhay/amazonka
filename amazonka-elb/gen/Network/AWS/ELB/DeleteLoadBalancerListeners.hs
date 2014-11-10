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

-- Module      : Network.AWS.ELB.DeleteLoadBalancerListeners
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes listeners from the load balancer for the specified port.
module Network.AWS.ELB.DeleteLoadBalancerListeners
    (
    -- * Request
      DeleteLoadBalancerListenerInput
    -- ** Request constructor
    , deleteLoadBalancerListeners
    -- ** Request lenses
    , dlbliLoadBalancerName
    , dlbliLoadBalancerPorts

    -- * Response
    , DeleteLoadBalancerListenersResponse
    -- ** Response constructor
    , deleteLoadBalancerListenersResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data DeleteLoadBalancerListenerInput = DeleteLoadBalancerListenerInput
    { _dlbliLoadBalancerName  :: Text
    , _dlbliLoadBalancerPorts :: [Int]
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteLoadBalancerListenerInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbliLoadBalancerName' @::@ 'Text'
--
-- * 'dlbliLoadBalancerPorts' @::@ ['Int']
--
deleteLoadBalancerListeners :: Text -- ^ 'dlbliLoadBalancerName'
                            -> DeleteLoadBalancerListenerInput
deleteLoadBalancerListeners p1 = DeleteLoadBalancerListenerInput
    { _dlbliLoadBalancerName  = p1
    , _dlbliLoadBalancerPorts = mempty
    }

-- | The mnemonic name associated with the load balancer.
dlbliLoadBalancerName :: Lens' DeleteLoadBalancerListenerInput Text
dlbliLoadBalancerName =
    lens _dlbliLoadBalancerName (\s a -> s { _dlbliLoadBalancerName = a })

-- | The client port number(s) of the load balancer listener(s) to be removed.
dlbliLoadBalancerPorts :: Lens' DeleteLoadBalancerListenerInput [Int]
dlbliLoadBalancerPorts =
    lens _dlbliLoadBalancerPorts (\s a -> s { _dlbliLoadBalancerPorts = a })

instance ToPath DeleteLoadBalancerListenerInput where
    toPath = const "/"

instance ToQuery DeleteLoadBalancerListenerInput

data DeleteLoadBalancerListenersResponse = DeleteLoadBalancerListenersResponse

-- | 'DeleteLoadBalancerListenersResponse' constructor.
deleteLoadBalancerListenersResponse :: DeleteLoadBalancerListenersResponse
deleteLoadBalancerListenersResponse = DeleteLoadBalancerListenersResponse

instance AWSRequest DeleteLoadBalancerListenerInput where
    type Sv DeleteLoadBalancerListenerInput = ELB
    type Rs DeleteLoadBalancerListenerInput = DeleteLoadBalancerListenersResponse

    request  = post "DeleteLoadBalancerListeners"
    response = const (nullaryResponse DeleteLoadBalancerListenersResponse)
