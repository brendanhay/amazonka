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

-- Module      : Network.AWS.ELB.DescribeLoadBalancerPolicies
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
-- The names of the sample policies have the ELBSample- prefix.
module Network.AWS.ELB.DescribeLoadBalancerPolicies
    (
    -- * Request
      DescribeLoadBalancerPoliciesInput
    -- ** Request constructor
    , describeLoadBalancerPoliciesInput
    -- ** Request lenses
    , dlbpiLoadBalancerName
    , dlbpiPolicyNames

    -- * Response
    , DescribeLoadBalancerPoliciesOutput
    -- ** Response constructor
    , describeLoadBalancerPoliciesOutput
    -- ** Response lenses
    , dlbpoPolicyDescriptions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data DescribeLoadBalancerPoliciesInput = DescribeLoadBalancerPoliciesInput
    { _dlbpiLoadBalancerName :: Maybe Text
    , _dlbpiPolicyNames      :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeLoadBalancerPoliciesInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbpiLoadBalancerName' @::@ 'Maybe' 'Text'
--
-- * 'dlbpiPolicyNames' @::@ ['Text']
--
describeLoadBalancerPoliciesInput :: DescribeLoadBalancerPoliciesInput
describeLoadBalancerPoliciesInput = DescribeLoadBalancerPoliciesInput
    { _dlbpiLoadBalancerName = Nothing
    , _dlbpiPolicyNames      = mempty
    }

-- | The mnemonic name associated with the load balancer. If no name is
-- specified, the operation returns the attributes of either all the sample
-- policies pre-defined by Elastic Load Balancing or the specified sample
-- polices.
dlbpiLoadBalancerName :: Lens' DescribeLoadBalancerPoliciesInput (Maybe Text)
dlbpiLoadBalancerName =
    lens _dlbpiLoadBalancerName (\s a -> s { _dlbpiLoadBalancerName = a })

-- | The names of load balancer policies you've created or Elastic Load
-- Balancing sample policy names.
dlbpiPolicyNames :: Lens' DescribeLoadBalancerPoliciesInput [Text]
dlbpiPolicyNames = lens _dlbpiPolicyNames (\s a -> s { _dlbpiPolicyNames = a })

instance ToQuery DescribeLoadBalancerPoliciesInput

instance ToPath DescribeLoadBalancerPoliciesInput where
    toPath = const "/"

newtype DescribeLoadBalancerPoliciesOutput = DescribeLoadBalancerPoliciesOutput
    { _dlbpoPolicyDescriptions :: [PolicyDescription]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList DescribeLoadBalancerPoliciesOutput where
    type Item DescribeLoadBalancerPoliciesOutput = PolicyDescription

    fromList = DescribeLoadBalancerPoliciesOutput . fromList
    toList   = toList . _dlbpoPolicyDescriptions

-- | 'DescribeLoadBalancerPoliciesOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbpoPolicyDescriptions' @::@ ['PolicyDescription']
--
describeLoadBalancerPoliciesOutput :: DescribeLoadBalancerPoliciesOutput
describeLoadBalancerPoliciesOutput = DescribeLoadBalancerPoliciesOutput
    { _dlbpoPolicyDescriptions = mempty
    }

-- | A list of policy description structures.
dlbpoPolicyDescriptions :: Lens' DescribeLoadBalancerPoliciesOutput [PolicyDescription]
dlbpoPolicyDescriptions =
    lens _dlbpoPolicyDescriptions (\s a -> s { _dlbpoPolicyDescriptions = a })

instance FromXML DescribeLoadBalancerPoliciesOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeLoadBalancerPoliciesOutput"

instance AWSRequest DescribeLoadBalancerPoliciesInput where
    type Sv DescribeLoadBalancerPoliciesInput = ELB
    type Rs DescribeLoadBalancerPoliciesInput = DescribeLoadBalancerPoliciesOutput

    request  = post "DescribeLoadBalancerPolicies"
    response = xmlResponse $ \h x -> DescribeLoadBalancerPoliciesOutput
        <$> x %| "PolicyDescriptions"
