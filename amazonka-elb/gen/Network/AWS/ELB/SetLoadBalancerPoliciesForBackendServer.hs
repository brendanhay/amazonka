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

-- Module      : Network.AWS.ELB.SetLoadBalancerPoliciesForBackendServer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Replaces the current set of policies associated with a port on which the
-- back-end server is listening with a new set of policies. After the policies
-- have been created using CreateLoadBalancerPolicy, they can be applied here
-- as a list. At this time, only the back-end server authentication policy
-- type can be applied to the back-end ports; this policy type is composed of
-- multiple public key policies. The SetLoadBalancerPoliciesForBackendServer
-- replaces the current set of policies associated with the specified instance
-- port. Every time you use this action to enable the policies, use the
-- PolicyNames parameter to list all the policies you want to enable. You can
-- use DescribeLoadBalancers or DescribeLoadBalancerPolicies action to verify
-- that the policy has been associated with the back-end server.
module Network.AWS.ELB.SetLoadBalancerPoliciesForBackendServer
    (
    -- * Request
      SetLoadBalancerPoliciesForBackendServerInput
    -- ** Request constructor
    , setLoadBalancerPoliciesForBackendServerInput
    -- ** Request lenses
    , slbpfbsiInstancePort
    , slbpfbsiLoadBalancerName
    , slbpfbsiPolicyNames

    -- * Response
    , SetLoadBalancerPoliciesForBackendServerResponse
    -- ** Response constructor
    , setLoadBalancerPoliciesForBackendServerResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data SetLoadBalancerPoliciesForBackendServerInput = SetLoadBalancerPoliciesForBackendServerInput
    { _slbpfbsiInstancePort     :: Int
    , _slbpfbsiLoadBalancerName :: Text
    , _slbpfbsiPolicyNames      :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'SetLoadBalancerPoliciesForBackendServerInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slbpfbsiInstancePort' @::@ 'Int'
--
-- * 'slbpfbsiLoadBalancerName' @::@ 'Text'
--
-- * 'slbpfbsiPolicyNames' @::@ ['Text']
--
setLoadBalancerPoliciesForBackendServerInput :: Text -- ^ 'slbpfbsiLoadBalancerName'
                                             -> Int -- ^ 'slbpfbsiInstancePort'
                                             -> SetLoadBalancerPoliciesForBackendServerInput
setLoadBalancerPoliciesForBackendServerInput p1 p2 = SetLoadBalancerPoliciesForBackendServerInput
    { _slbpfbsiLoadBalancerName = p1
    , _slbpfbsiInstancePort     = p2
    , _slbpfbsiPolicyNames      = mempty
    }

-- | The port number associated with the back-end server.
slbpfbsiInstancePort :: Lens' SetLoadBalancerPoliciesForBackendServerInput Int
slbpfbsiInstancePort =
    lens _slbpfbsiInstancePort (\s a -> s { _slbpfbsiInstancePort = a })

-- | The mnemonic name associated with the load balancer. This name must be
-- unique within the set of your load balancers.
slbpfbsiLoadBalancerName :: Lens' SetLoadBalancerPoliciesForBackendServerInput Text
slbpfbsiLoadBalancerName =
    lens _slbpfbsiLoadBalancerName
        (\s a -> s { _slbpfbsiLoadBalancerName = a })

-- | List of policy names to be set. If the list is empty, then all current
-- polices are removed from the back-end server.
slbpfbsiPolicyNames :: Lens' SetLoadBalancerPoliciesForBackendServerInput [Text]
slbpfbsiPolicyNames =
    lens _slbpfbsiPolicyNames (\s a -> s { _slbpfbsiPolicyNames = a })

instance ToPath SetLoadBalancerPoliciesForBackendServerInput where
    toPath = const "/"

instance ToQuery SetLoadBalancerPoliciesForBackendServerInput

data SetLoadBalancerPoliciesForBackendServerResponse = SetLoadBalancerPoliciesForBackendServerResponse

-- | 'SetLoadBalancerPoliciesForBackendServerResponse' constructor.
setLoadBalancerPoliciesForBackendServerResponse :: SetLoadBalancerPoliciesForBackendServerResponse
setLoadBalancerPoliciesForBackendServerResponse = SetLoadBalancerPoliciesForBackendServerResponse

instance AWSRequest SetLoadBalancerPoliciesForBackendServerInput where
    type Sv SetLoadBalancerPoliciesForBackendServerInput = ELB
    type Rs SetLoadBalancerPoliciesForBackendServerInput = SetLoadBalancerPoliciesForBackendServerResponse

    request  = post "SetLoadBalancerPoliciesForBackendServer"
    response = const (nullaryResponse SetLoadBalancerPoliciesForBackendServerResponse)
