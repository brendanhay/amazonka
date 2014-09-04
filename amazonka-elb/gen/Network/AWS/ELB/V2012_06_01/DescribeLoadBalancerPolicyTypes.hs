{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.DescribeLoadBalancerPolicyTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns meta-information on the specified load balancer policies defined by
-- the Elastic Load Balancing service. The policy types that are returned from
-- this action can be used in a CreateLoadBalancerPolicy action to instantiate
-- specific policy configurations that will be applied to a load balancer.
-- Partial description of all the policy types defined by Elastic Load
-- Balancing for your account
-- https://elasticloadbalancing.amazonaws.com/?Version=2012-06-01
-- &Action=DescribeLoadBalancerPolicyTypes &AUTHPARAMS
-- SSLNegotiationPolicyType BackendServerAuthenticationPolicyType
-- PublicKeyPolicyType AppCookieStickinessPolicyType
-- LBCookieStickinessPolicyType ProxyProtocolPolicyType
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE Description of ProxyProtocolPolicyType
-- https://elasticloadbalancing.amazonaws.com/?PolicyTypeNames.member.1=ProxyProtocolPolicyType
-- &Version=2012-06-01 &Action=DescribeLoadBalancerPolicyTypes &AUTHPARAMS
-- ProxyProtocol Boolean ONE ProxyProtocolPolicyType Policy that controls
-- whether to include the IP address and port of the originating request for
-- TCP messages. This policy operates on TCP/SSL listeners only
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE.
module Network.AWS.ELB.V2012_06_01.DescribeLoadBalancerPolicyTypes
    (
    -- * Request
      DescribeLoadBalancerPolicyTypes
    -- ** Request constructor
    , mkDescribeLoadBalancerPolicyTypesInput
    -- ** Request lenses
    , dlbptiPolicyTypeNames

    -- * Response
    , DescribeLoadBalancerPolicyTypesResponse
    -- ** Response lenses
    , dlbptoPolicyTypeDescriptions
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLoadBalancerPolicyTypes' request.
mkDescribeLoadBalancerPolicyTypesInput :: DescribeLoadBalancerPolicyTypes
mkDescribeLoadBalancerPolicyTypesInput = DescribeLoadBalancerPolicyTypes
    { _dlbptiPolicyTypeNames = mempty
    }
{-# INLINE mkDescribeLoadBalancerPolicyTypesInput #-}

newtype DescribeLoadBalancerPolicyTypes = DescribeLoadBalancerPolicyTypes
    { _dlbptiPolicyTypeNames :: [Text]
      -- ^ Specifies the name of the policy types. If no names are
      -- specified, returns the description of all the policy types
      -- defined by Elastic Load Balancing service.
    } deriving (Show, Generic)

-- | Specifies the name of the policy types. If no names are specified, returns
-- the description of all the policy types defined by Elastic Load Balancing
-- service.
dlbptiPolicyTypeNames :: Lens' DescribeLoadBalancerPolicyTypes ([Text])
dlbptiPolicyTypeNames = lens _dlbptiPolicyTypeNames (\s a -> s { _dlbptiPolicyTypeNames = a })
{-# INLINE dlbptiPolicyTypeNames #-}

instance ToQuery DescribeLoadBalancerPolicyTypes where
    toQuery = genericQuery def

newtype DescribeLoadBalancerPolicyTypesResponse = DescribeLoadBalancerPolicyTypesResponse
    { _dlbptoPolicyTypeDescriptions :: [PolicyTypeDescription]
      -- ^ List of policy type description structures of the specified
      -- policy type. If no policy type names are specified, returns the
      -- description of all the policy types defined by Elastic Load
      -- Balancing service.
    } deriving (Show, Generic)

-- | List of policy type description structures of the specified policy type. If
-- no policy type names are specified, returns the description of all the
-- policy types defined by Elastic Load Balancing service.
dlbptoPolicyTypeDescriptions :: Lens' DescribeLoadBalancerPolicyTypesResponse ([PolicyTypeDescription])
dlbptoPolicyTypeDescriptions = lens _dlbptoPolicyTypeDescriptions (\s a -> s { _dlbptoPolicyTypeDescriptions = a })
{-# INLINE dlbptoPolicyTypeDescriptions #-}

instance FromXML DescribeLoadBalancerPolicyTypesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeLoadBalancerPolicyTypes where
    type Sv DescribeLoadBalancerPolicyTypes = ELB
    type Rs DescribeLoadBalancerPolicyTypes = DescribeLoadBalancerPolicyTypesResponse

    request = post "DescribeLoadBalancerPolicyTypes"
    response _ = xmlResponse
