{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns detailed descriptions of the policies. If you specify a load
-- balancer name, the action returns the descriptions of all the policies
-- created for the load balancer. If you specify a policy name associated with
-- your load balancer, the action returns the description of that policy. If
-- you don't specify a load balancer name, the action returns descriptions of
-- the specified sample policies, or descriptions of all the sample policies.
-- The names of the sample policies have the ELBSample- prefix. Description of
-- all the policies associated with a load balancer
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=MyLoadBalancer
-- &Version=2012-06-01 &Action=DescribeLoadBalancerPolicies &AUTHPARAMS
-- MyDurationStickyPolicy LBCookieStickinessPolicyType CookieExpirationPeriod
-- 60 MyAppStickyPolicy AppCookieStickinessPolicyType CookieName MyAppCookie
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE Description of a specified policy
-- associated with the load balancer
-- https://elasticloadbalancing.amazonaws.com/?PolicyNames.member.1=EnableProxyProtocol
-- &LoadBalancerName=my-test-loadbalancer &Version=2012-06-01
-- &Action=DescribeLoadBalancerPolicies &AUTHPARAMS EnableProxyProtocol
-- ProxyProtocolPolicyType ProxyProtocol true
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE.
module Network.AWS.ELB
    (
    -- * Request
      DescribeLoadBalancerPolicies
    -- ** Request constructor
    , mkDescribeLoadBalancerPolicies
    -- ** Request lenses
    , dlbp1LoadBalancerName
    , dlbp1PolicyNames

    -- * Response
    , DescribeLoadBalancerPoliciesResponse
    -- ** Response constructor
    , mkDescribeLoadBalancerPoliciesResponse
    -- ** Response lenses
    , dlbprrPolicyDescriptions
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import Network.AWS.Prelude

data DescribeLoadBalancerPolicies = DescribeLoadBalancerPolicies
    { _dlbp1LoadBalancerName :: !(Maybe Text)
    , _dlbp1PolicyNames :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLoadBalancerPolicies' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Maybe Text@
--
-- * @PolicyNames ::@ @[Text]@
--
mkDescribeLoadBalancerPolicies :: DescribeLoadBalancerPolicies
mkDescribeLoadBalancerPolicies = DescribeLoadBalancerPolicies
    { _dlbp1LoadBalancerName = Nothing
    , _dlbp1PolicyNames = mempty
    }

-- | The mnemonic name associated with the load balancer. If no name is
-- specified, the operation returns the attributes of either all the sample
-- policies pre-defined by Elastic Load Balancing or the specified sample
-- polices.
dlbp1LoadBalancerName :: Lens' DescribeLoadBalancerPolicies (Maybe Text)
dlbp1LoadBalancerName =
    lens _dlbp1LoadBalancerName (\s a -> s { _dlbp1LoadBalancerName = a })

-- | The names of load balancer policies you've created or Elastic Load
-- Balancing sample policy names.
dlbp1PolicyNames :: Lens' DescribeLoadBalancerPolicies [Text]
dlbp1PolicyNames =
    lens _dlbp1PolicyNames (\s a -> s { _dlbp1PolicyNames = a })

instance ToQuery DescribeLoadBalancerPolicies where
    toQuery = genericQuery def

-- | The output for the DescribeLoadBalancerPolicies action.
newtype DescribeLoadBalancerPoliciesResponse = DescribeLoadBalancerPoliciesResponse
    { _dlbprrPolicyDescriptions :: [PolicyDescription]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLoadBalancerPoliciesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PolicyDescriptions ::@ @[PolicyDescription]@
--
mkDescribeLoadBalancerPoliciesResponse :: DescribeLoadBalancerPoliciesResponse
mkDescribeLoadBalancerPoliciesResponse = DescribeLoadBalancerPoliciesResponse
    { _dlbprrPolicyDescriptions = mempty
    }

-- | A list of policy description structures.
dlbprrPolicyDescriptions :: Lens' DescribeLoadBalancerPoliciesResponse [PolicyDescription]
dlbprrPolicyDescriptions =
    lens _dlbprrPolicyDescriptions
         (\s a -> s { _dlbprrPolicyDescriptions = a })

instance FromXML DescribeLoadBalancerPoliciesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeLoadBalancerPolicies where
    type Sv DescribeLoadBalancerPolicies = ELB
    type Rs DescribeLoadBalancerPolicies = DescribeLoadBalancerPoliciesResponse

    request = post "DescribeLoadBalancerPolicies"
    response _ = xmlResponse
