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

-- Module      : Network.AWS.ELB.DescribeLoadBalancerPolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns detailed descriptions of the policies. If you specify a load balancer
-- name, the action returns the descriptions of all the policies created for the
-- load balancer. If you specify a policy name associated with your load
-- balancer, the action returns the description of that policy. If you don't
-- specify a load balancer name, the action returns descriptions of the
-- specified sample policies, or descriptions of all the sample policies. The
-- names of the sample policies have the 'ELBSample-' prefix.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeLoadBalancerPolicies.html>
module Network.AWS.ELB.DescribeLoadBalancerPolicies
    (
    -- * Request
      DescribeLoadBalancerPolicies
    -- ** Request constructor
    , describeLoadBalancerPolicies
    -- ** Request lenses
    , dlbpLoadBalancerName
    , dlbpPolicyNames

    -- * Response
    , DescribeLoadBalancerPoliciesResponse
    -- ** Response constructor
    , describeLoadBalancerPoliciesResponse
    -- ** Response lenses
    , dlbprPolicyDescriptions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

data DescribeLoadBalancerPolicies = DescribeLoadBalancerPolicies
    { _dlbpLoadBalancerName :: Maybe Text
    , _dlbpPolicyNames      :: List "PolicyNames" Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeLoadBalancerPolicies' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbpLoadBalancerName' @::@ 'Maybe' 'Text'
--
-- * 'dlbpPolicyNames' @::@ ['Text']
--
describeLoadBalancerPolicies :: DescribeLoadBalancerPolicies
describeLoadBalancerPolicies = DescribeLoadBalancerPolicies
    { _dlbpLoadBalancerName = Nothing
    , _dlbpPolicyNames      = mempty
    }

-- | The mnemonic name associated with the load balancer. If no name is
-- specified, the operation returns the attributes of either all the sample
-- policies pre-defined by Elastic Load Balancing or the specified sample
-- polices.
--
dlbpLoadBalancerName :: Lens' DescribeLoadBalancerPolicies (Maybe Text)
dlbpLoadBalancerName =
    lens _dlbpLoadBalancerName (\s a -> s { _dlbpLoadBalancerName = a })

-- | The names of load balancer policies you've created or Elastic Load Balancing
-- sample policy names.
--
dlbpPolicyNames :: Lens' DescribeLoadBalancerPolicies [Text]
dlbpPolicyNames = lens _dlbpPolicyNames (\s a -> s { _dlbpPolicyNames = a }) . _List

newtype DescribeLoadBalancerPoliciesResponse = DescribeLoadBalancerPoliciesResponse
    { _dlbprPolicyDescriptions :: List "PolicyDescriptions" PolicyDescription
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeLoadBalancerPoliciesResponse where
    type Item DescribeLoadBalancerPoliciesResponse = PolicyDescription

    fromList = DescribeLoadBalancerPoliciesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dlbprPolicyDescriptions

-- | 'DescribeLoadBalancerPoliciesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbprPolicyDescriptions' @::@ ['PolicyDescription']
--
describeLoadBalancerPoliciesResponse :: DescribeLoadBalancerPoliciesResponse
describeLoadBalancerPoliciesResponse = DescribeLoadBalancerPoliciesResponse
    { _dlbprPolicyDescriptions = mempty
    }

-- | A list of policy description structures.
--
dlbprPolicyDescriptions :: Lens' DescribeLoadBalancerPoliciesResponse [PolicyDescription]
dlbprPolicyDescriptions =
    lens _dlbprPolicyDescriptions (\s a -> s { _dlbprPolicyDescriptions = a })
        . _List

instance ToPath DescribeLoadBalancerPolicies where
    toPath = const "/"

instance ToQuery DescribeLoadBalancerPolicies where
    toQuery DescribeLoadBalancerPolicies{..} = mconcat
        [ "LoadBalancerName" =? _dlbpLoadBalancerName
        , "PolicyNames"      =? _dlbpPolicyNames
        ]

instance ToHeaders DescribeLoadBalancerPolicies

instance AWSRequest DescribeLoadBalancerPolicies where
    type Sv DescribeLoadBalancerPolicies = ELB
    type Rs DescribeLoadBalancerPolicies = DescribeLoadBalancerPoliciesResponse

    request  = post "DescribeLoadBalancerPolicies"
    response = xmlResponse

instance FromXML DescribeLoadBalancerPoliciesResponse where
    parseXML = withElement "DescribeLoadBalancerPoliciesResult" $ \x -> DescribeLoadBalancerPoliciesResponse
        <$> x .@  "PolicyDescriptions"
