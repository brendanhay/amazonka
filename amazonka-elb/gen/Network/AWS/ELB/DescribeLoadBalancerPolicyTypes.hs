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

-- Module      : Network.AWS.ELB.DescribeLoadBalancerPolicyTypes
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
module Network.AWS.ELB.DescribeLoadBalancerPolicyTypes
    (
    -- * Request
      DescribeLoadBalancerPolicyTypes
    -- ** Request constructor
    , describeLoadBalancerPolicyTypes
    -- ** Request lenses
    , dlbptPolicyTypeNames

    -- * Response
    , DescribeLoadBalancerPolicyTypesResponse
    -- ** Response constructor
    , describeLoadBalancerPolicyTypesResponse
    -- ** Response lenses
    , dlbptrPolicyTypeDescriptions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

newtype DescribeLoadBalancerPolicyTypes = DescribeLoadBalancerPolicyTypes
    { _dlbptPolicyTypeNames :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance IsList DescribeLoadBalancerPolicyTypes where
    type Item DescribeLoadBalancerPolicyTypes = Text

    fromList = DescribeLoadBalancerPolicyTypes . fromList
    toList   = toList . _dlbptPolicyTypeNames

-- | 'DescribeLoadBalancerPolicyTypes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbptPolicyTypeNames' @::@ ['Text']
--
describeLoadBalancerPolicyTypes :: DescribeLoadBalancerPolicyTypes
describeLoadBalancerPolicyTypes = DescribeLoadBalancerPolicyTypes
    { _dlbptPolicyTypeNames = mempty
    }

-- | Specifies the name of the policy types. If no names are specified,
-- returns the description of all the policy types defined by Elastic Load
-- Balancing service.
dlbptPolicyTypeNames :: Lens' DescribeLoadBalancerPolicyTypes [Text]
dlbptPolicyTypeNames =
    lens _dlbptPolicyTypeNames (\s a -> s { _dlbptPolicyTypeNames = a })

instance ToQuery DescribeLoadBalancerPolicyTypes

instance ToPath DescribeLoadBalancerPolicyTypes where
    toPath = const "/"

newtype DescribeLoadBalancerPolicyTypesResponse = DescribeLoadBalancerPolicyTypesResponse
    { _dlbptrPolicyTypeDescriptions :: [PolicyTypeDescription]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList DescribeLoadBalancerPolicyTypesResponse where
    type Item DescribeLoadBalancerPolicyTypesResponse = PolicyTypeDescription

    fromList = DescribeLoadBalancerPolicyTypesResponse . fromList
    toList   = toList . _dlbptrPolicyTypeDescriptions

-- | 'DescribeLoadBalancerPolicyTypesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbptrPolicyTypeDescriptions' @::@ ['PolicyTypeDescription']
--
describeLoadBalancerPolicyTypesResponse :: DescribeLoadBalancerPolicyTypesResponse
describeLoadBalancerPolicyTypesResponse = DescribeLoadBalancerPolicyTypesResponse
    { _dlbptrPolicyTypeDescriptions = mempty
    }

-- | List of policy type description structures of the specified policy type.
-- If no policy type names are specified, returns the description of all the
-- policy types defined by Elastic Load Balancing service.
dlbptrPolicyTypeDescriptions :: Lens' DescribeLoadBalancerPolicyTypesResponse [PolicyTypeDescription]
dlbptrPolicyTypeDescriptions =
    lens _dlbptrPolicyTypeDescriptions
        (\s a -> s { _dlbptrPolicyTypeDescriptions = a })

instance FromXML DescribeLoadBalancerPolicyTypesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeLoadBalancerPolicyTypesResponse"

instance AWSRequest DescribeLoadBalancerPolicyTypes where
    type Sv DescribeLoadBalancerPolicyTypes = ELB
    type Rs DescribeLoadBalancerPolicyTypes = DescribeLoadBalancerPolicyTypesResponse

    request  = post "DescribeLoadBalancerPolicyTypes"
    response = xmlResponse $ \h x -> DescribeLoadBalancerPolicyTypesResponse
        <$> x %| "PolicyTypeDescriptions"
