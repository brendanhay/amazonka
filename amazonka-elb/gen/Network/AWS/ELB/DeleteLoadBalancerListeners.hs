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
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeleteLoadBalancerListeners.html>
module Network.AWS.ELB.DeleteLoadBalancerListeners
    (
    -- * Request
      DeleteLoadBalancerListeners
    -- ** Request constructor
    , deleteLoadBalancerListeners
    -- ** Request lenses
    , dlblLoadBalancerName
    , dlblLoadBalancerPorts

    -- * Response
    , DeleteLoadBalancerListenersResponse
    -- ** Response constructor
    , deleteLoadBalancerListenersResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

data DeleteLoadBalancerListeners = DeleteLoadBalancerListeners
    { _dlblLoadBalancerName  :: Text
    , _dlblLoadBalancerPorts :: List "LoadBalancerPorts" Int
    } deriving (Eq, Ord, Show)

-- | 'DeleteLoadBalancerListeners' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlblLoadBalancerName' @::@ 'Text'
--
-- * 'dlblLoadBalancerPorts' @::@ ['Int']
--
deleteLoadBalancerListeners :: Text -- ^ 'dlblLoadBalancerName'
                            -> DeleteLoadBalancerListeners
deleteLoadBalancerListeners p1 = DeleteLoadBalancerListeners
    { _dlblLoadBalancerName  = p1
    , _dlblLoadBalancerPorts = mempty
    }

-- | The mnemonic name associated with the load balancer.
dlblLoadBalancerName :: Lens' DeleteLoadBalancerListeners Text
dlblLoadBalancerName =
    lens _dlblLoadBalancerName (\s a -> s { _dlblLoadBalancerName = a })

-- | The client port number(s) of the load balancer listener(s) to be removed.
dlblLoadBalancerPorts :: Lens' DeleteLoadBalancerListeners [Int]
dlblLoadBalancerPorts =
    lens _dlblLoadBalancerPorts (\s a -> s { _dlblLoadBalancerPorts = a })
        . _List

data DeleteLoadBalancerListenersResponse = DeleteLoadBalancerListenersResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteLoadBalancerListenersResponse' constructor.
deleteLoadBalancerListenersResponse :: DeleteLoadBalancerListenersResponse
deleteLoadBalancerListenersResponse = DeleteLoadBalancerListenersResponse

instance ToPath DeleteLoadBalancerListeners where
    toPath = const "/"

instance ToQuery DeleteLoadBalancerListeners where
    toQuery DeleteLoadBalancerListeners{..} = mconcat
        [ "LoadBalancerName"  =? _dlblLoadBalancerName
        , "LoadBalancerPorts" =? _dlblLoadBalancerPorts
        ]

instance ToHeaders DeleteLoadBalancerListeners

instance AWSRequest DeleteLoadBalancerListeners where
    type Sv DeleteLoadBalancerListeners = ELB
    type Rs DeleteLoadBalancerListeners = DeleteLoadBalancerListenersResponse

    request  = post "DeleteLoadBalancerListeners"
    response = nullResponse DeleteLoadBalancerListenersResponse


Some kind of operator / class to check the types whether to continue?
