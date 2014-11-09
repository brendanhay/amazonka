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
      DescribeLoadBalancerPolicyTypesInput
    -- ** Request constructor
    , describeLoadBalancerPolicyTypesInput
    -- ** Request lenses
    , dlbptiPolicyTypeNames

    -- * Response
    , DescribeLoadBalancerPolicyTypesOutput
    -- ** Response constructor
    , describeLoadBalancerPolicyTypesOutput
    -- ** Response lenses
    , dlbptoPolicyTypeDescriptions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

newtype DescribeLoadBalancerPolicyTypesInput = DescribeLoadBalancerPolicyTypesInput
    { _dlbptiPolicyTypeNames :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DescribeLoadBalancerPolicyTypesInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbptiPolicyTypeNames' @::@ ['Text']
--
describeLoadBalancerPolicyTypesInput :: DescribeLoadBalancerPolicyTypesInput
describeLoadBalancerPolicyTypesInput = DescribeLoadBalancerPolicyTypesInput
    { _dlbptiPolicyTypeNames = mempty
    }

-- | Specifies the name of the policy types. If no names are specified,
-- returns the description of all the policy types defined by Elastic Load
-- Balancing service.
dlbptiPolicyTypeNames :: Lens' DescribeLoadBalancerPolicyTypesInput [Text]
dlbptiPolicyTypeNames =
    lens _dlbptiPolicyTypeNames (\s a -> s { _dlbptiPolicyTypeNames = a })

instance ToPath DescribeLoadBalancerPolicyTypesInput where
    toPath = const "/"

instance ToQuery DescribeLoadBalancerPolicyTypesInput

newtype DescribeLoadBalancerPolicyTypesOutput = DescribeLoadBalancerPolicyTypesOutput
    { _dlbptoPolicyTypeDescriptions :: [PolicyTypeDescription]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'DescribeLoadBalancerPolicyTypesOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbptoPolicyTypeDescriptions' @::@ ['PolicyTypeDescription']
--
describeLoadBalancerPolicyTypesOutput :: DescribeLoadBalancerPolicyTypesOutput
describeLoadBalancerPolicyTypesOutput = DescribeLoadBalancerPolicyTypesOutput
    { _dlbptoPolicyTypeDescriptions = mempty
    }

-- | List of policy type description structures of the specified policy type.
-- If no policy type names are specified, returns the description of all the
-- policy types defined by Elastic Load Balancing service.
dlbptoPolicyTypeDescriptions :: Lens' DescribeLoadBalancerPolicyTypesOutput [PolicyTypeDescription]
dlbptoPolicyTypeDescriptions =
    lens _dlbptoPolicyTypeDescriptions
        (\s a -> s { _dlbptoPolicyTypeDescriptions = a })

instance AWSRequest DescribeLoadBalancerPolicyTypesInput where
    type Sv DescribeLoadBalancerPolicyTypesInput = ELB
    type Rs DescribeLoadBalancerPolicyTypesInput = DescribeLoadBalancerPolicyTypesOutput

    request  = post "DescribeLoadBalancerPolicyTypes"
    response = const . xmlResponse $ \h x -> DescribeLoadBalancerPolicyTypesOutput
newtype
