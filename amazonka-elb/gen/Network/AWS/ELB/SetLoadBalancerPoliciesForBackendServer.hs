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

-- Module      : Network.AWS.ELB.SetLoadBalancerPoliciesForBackendServer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Replaces the set of policies associated with the specified port on which the
-- back-end server is listening with a new set of policies. At this time, only
-- the back-end server authentication policy type can be applied to the back-end
-- ports; this policy type is composed of multiple public key policies.
--
-- Each time you use 'SetLoadBalancerPoliciesForBackendServer' to enable the
-- policies, use the 'PolicyNames' parameter to list the policies that you want to
-- enable.
--
-- You can use 'DescribeLoadBalancers' or 'DescribeLoadBalancerPolicies' to verify
-- that the policy is associated with the back-end server.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_SetLoadBalancerPoliciesForBackendServer.html>
module Network.AWS.ELB.SetLoadBalancerPoliciesForBackendServer
    (
    -- * Request
      SetLoadBalancerPoliciesForBackendServer
    -- ** Request constructor
    , setLoadBalancerPoliciesForBackendServer
    -- ** Request lenses
    , slbpfbsInstancePort
    , slbpfbsLoadBalancerName
    , slbpfbsPolicyNames

    -- * Response
    , SetLoadBalancerPoliciesForBackendServerResponse
    -- ** Response constructor
    , setLoadBalancerPoliciesForBackendServerResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

data SetLoadBalancerPoliciesForBackendServer = SetLoadBalancerPoliciesForBackendServer
    { _slbpfbsInstancePort     :: Int
    , _slbpfbsLoadBalancerName :: Text
    , _slbpfbsPolicyNames      :: List "member" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'SetLoadBalancerPoliciesForBackendServer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slbpfbsInstancePort' @::@ 'Int'
--
-- * 'slbpfbsLoadBalancerName' @::@ 'Text'
--
-- * 'slbpfbsPolicyNames' @::@ ['Text']
--
setLoadBalancerPoliciesForBackendServer :: Text -- ^ 'slbpfbsLoadBalancerName'
                                        -> Int -- ^ 'slbpfbsInstancePort'
                                        -> SetLoadBalancerPoliciesForBackendServer
setLoadBalancerPoliciesForBackendServer p1 p2 = SetLoadBalancerPoliciesForBackendServer
    { _slbpfbsLoadBalancerName = p1
    , _slbpfbsInstancePort     = p2
    , _slbpfbsPolicyNames      = mempty
    }

-- | The port number associated with the back-end server.
slbpfbsInstancePort :: Lens' SetLoadBalancerPoliciesForBackendServer Int
slbpfbsInstancePort =
    lens _slbpfbsInstancePort (\s a -> s { _slbpfbsInstancePort = a })

-- | The name of the load balancer.
slbpfbsLoadBalancerName :: Lens' SetLoadBalancerPoliciesForBackendServer Text
slbpfbsLoadBalancerName =
    lens _slbpfbsLoadBalancerName (\s a -> s { _slbpfbsLoadBalancerName = a })

-- | The names of the policies. If the list is empty, then all current polices are
-- removed from the back-end server.
slbpfbsPolicyNames :: Lens' SetLoadBalancerPoliciesForBackendServer [Text]
slbpfbsPolicyNames =
    lens _slbpfbsPolicyNames (\s a -> s { _slbpfbsPolicyNames = a })
        . _List

data SetLoadBalancerPoliciesForBackendServerResponse = SetLoadBalancerPoliciesForBackendServerResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'SetLoadBalancerPoliciesForBackendServerResponse' constructor.
setLoadBalancerPoliciesForBackendServerResponse :: SetLoadBalancerPoliciesForBackendServerResponse
setLoadBalancerPoliciesForBackendServerResponse = SetLoadBalancerPoliciesForBackendServerResponse

instance ToPath SetLoadBalancerPoliciesForBackendServer where
    toPath = const "/"

instance ToQuery SetLoadBalancerPoliciesForBackendServer where
    toQuery SetLoadBalancerPoliciesForBackendServer{..} = mconcat
        [ "InstancePort"     =? _slbpfbsInstancePort
        , "LoadBalancerName" =? _slbpfbsLoadBalancerName
        , "PolicyNames"      =? _slbpfbsPolicyNames
        ]

instance ToHeaders SetLoadBalancerPoliciesForBackendServer

instance AWSRequest SetLoadBalancerPoliciesForBackendServer where
    type Sv SetLoadBalancerPoliciesForBackendServer = ELB
    type Rs SetLoadBalancerPoliciesForBackendServer = SetLoadBalancerPoliciesForBackendServerResponse

    request  = post "SetLoadBalancerPoliciesForBackendServer"
    response = nullResponse SetLoadBalancerPoliciesForBackendServerResponse
