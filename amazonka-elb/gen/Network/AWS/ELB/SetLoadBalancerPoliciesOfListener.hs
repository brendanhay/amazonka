{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.SetLoadBalancerPoliciesOfListener
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Associates, updates, or disables a policy with a listener on the load
-- balancer. You can associate multiple policies with a listener. Associate
-- MySSLNegotiationPolicy with the load balancer port 443 on the
-- MyInternalLoadbalancer load balancer.
-- https://elasticloadbalancing.amazonaws.com/?PolicyNames.member.1=MySSLNegotiationPolicy
-- &LoadBalancerName=MyInternalLoadBalancer &LoadBalancerPort=443
-- &Version=2012-06-01 &Action=SetLoadBalancerPoliciesOfListener &AUTHPARAMS
-- azonaws.com/doc/2012-06-01/"> 07b1ecbc-1100-11e3-acaf-dd7edEXAMPLE.
module Network.AWS.ELB
    (
    -- * Request
      SetLoadBalancerPoliciesOfListener
    -- ** Request constructor
    , mkSetLoadBalancerPoliciesOfListener
    -- ** Request lenses
    , slbpolLoadBalancerName
    , slbpolLoadBalancerPort
    , slbpolPolicyNames

    -- * Response
    , SetLoadBalancerPoliciesOfListenerResponse
    -- ** Response constructor
    , mkSetLoadBalancerPoliciesOfListenerResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import Network.AWS.Prelude

-- | The input for the SetLoadBalancerPoliciesOfListener action.
data SetLoadBalancerPoliciesOfListener = SetLoadBalancerPoliciesOfListener
    { _slbpolLoadBalancerName :: !Text
    , _slbpolLoadBalancerPort :: !Integer
    , _slbpolPolicyNames :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetLoadBalancerPoliciesOfListener' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Text@
--
-- * @LoadBalancerPort ::@ @Integer@
--
-- * @PolicyNames ::@ @[Text]@
--
mkSetLoadBalancerPoliciesOfListener :: Text -- ^ 'slbpolLoadBalancerName'
                                    -> Integer -- ^ 'slbpolLoadBalancerPort'
                                    -> [Text] -- ^ 'slbpolPolicyNames'
                                    -> SetLoadBalancerPoliciesOfListener
mkSetLoadBalancerPoliciesOfListener p1 p2 p3 = SetLoadBalancerPoliciesOfListener
    { _slbpolLoadBalancerName = p1
    , _slbpolLoadBalancerPort = p2
    , _slbpolPolicyNames = p3
    }

-- | The name of the load balancer.
slbpolLoadBalancerName :: Lens' SetLoadBalancerPoliciesOfListener Text
slbpolLoadBalancerName =
    lens _slbpolLoadBalancerName (\s a -> s { _slbpolLoadBalancerName = a })

-- | The external port of the load balancer to associate the policy.
slbpolLoadBalancerPort :: Lens' SetLoadBalancerPoliciesOfListener Integer
slbpolLoadBalancerPort =
    lens _slbpolLoadBalancerPort (\s a -> s { _slbpolLoadBalancerPort = a })

-- | List of policies to be associated with the listener. If the list is empty,
-- the current policy is removed from the listener.
slbpolPolicyNames :: Lens' SetLoadBalancerPoliciesOfListener [Text]
slbpolPolicyNames =
    lens _slbpolPolicyNames (\s a -> s { _slbpolPolicyNames = a })

instance ToQuery SetLoadBalancerPoliciesOfListener where
    toQuery = genericQuery def

-- | The output for the SetLoadBalancerPoliciesOfListener action.
data SetLoadBalancerPoliciesOfListenerResponse = SetLoadBalancerPoliciesOfListenerResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetLoadBalancerPoliciesOfListenerResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkSetLoadBalancerPoliciesOfListenerResponse :: SetLoadBalancerPoliciesOfListenerResponse
mkSetLoadBalancerPoliciesOfListenerResponse = SetLoadBalancerPoliciesOfListenerResponse

instance AWSRequest SetLoadBalancerPoliciesOfListener where
    type Sv SetLoadBalancerPoliciesOfListener = ELB
    type Rs SetLoadBalancerPoliciesOfListener = SetLoadBalancerPoliciesOfListenerResponse

    request = post "SetLoadBalancerPoliciesOfListener"
    response _ = nullaryResponse SetLoadBalancerPoliciesOfListenerResponse
