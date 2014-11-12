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

-- Module      : Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Associates one or more security groups with your load balancer in Amazon
-- Virtual Private Cloud (Amazon VPC). The provided security group IDs will
-- override any currently applied security groups. For more information, see
-- Manage Security Groups in Amazon VPC in the Elastic Load Balancing
-- Developer Guide.
module Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer
    (
    -- * Request
      ApplySecurityGroupsToLoadBalancerInput
    -- ** Request constructor
    , applySecurityGroupsToLoadBalancerInput
    -- ** Request lenses
    , asgtlbiLoadBalancerName
    , asgtlbiSecurityGroups

    -- * Response
    , ApplySecurityGroupsToLoadBalancerOutput
    -- ** Response constructor
    , applySecurityGroupsToLoadBalancerOutput
    -- ** Response lenses
    , asgtlboSecurityGroups
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data ApplySecurityGroupsToLoadBalancerInput = ApplySecurityGroupsToLoadBalancerInput
    { _asgtlbiLoadBalancerName :: Text
    , _asgtlbiSecurityGroups   :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'ApplySecurityGroupsToLoadBalancerInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgtlbiLoadBalancerName' @::@ 'Text'
--
-- * 'asgtlbiSecurityGroups' @::@ ['Text']
--
applySecurityGroupsToLoadBalancerInput :: Text -- ^ 'asgtlbiLoadBalancerName'
                                       -> ApplySecurityGroupsToLoadBalancerInput
applySecurityGroupsToLoadBalancerInput p1 = ApplySecurityGroupsToLoadBalancerInput
    { _asgtlbiLoadBalancerName = p1
    , _asgtlbiSecurityGroups   = mempty
    }

-- | The name associated with the load balancer. The name must be unique
-- within the set of load balancers associated with your AWS account.
asgtlbiLoadBalancerName :: Lens' ApplySecurityGroupsToLoadBalancerInput Text
asgtlbiLoadBalancerName =
    lens _asgtlbiLoadBalancerName (\s a -> s { _asgtlbiLoadBalancerName = a })

-- | A list of security group IDs to associate with your load balancer in VPC.
-- The security group IDs must be provided as the ID and not the security
-- group name (For example, sg-1234).
asgtlbiSecurityGroups :: Lens' ApplySecurityGroupsToLoadBalancerInput [Text]
asgtlbiSecurityGroups =
    lens _asgtlbiSecurityGroups (\s a -> s { _asgtlbiSecurityGroups = a })

instance ToQuery ApplySecurityGroupsToLoadBalancerInput

instance ToPath ApplySecurityGroupsToLoadBalancerInput where
    toPath = const "/"

newtype ApplySecurityGroupsToLoadBalancerOutput = ApplySecurityGroupsToLoadBalancerOutput
    { _asgtlboSecurityGroups :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup, IsString)

instance IsList ApplySecurityGroupsToLoadBalancerOutput
    type Item ApplySecurityGroupsToLoadBalancerOutput = Text

    fromList = ApplySecurityGroupsToLoadBalancerOutput . fromList
    toList   = toList . _asgtlboSecurityGroups

-- | 'ApplySecurityGroupsToLoadBalancerOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgtlboSecurityGroups' @::@ ['Text']
--
applySecurityGroupsToLoadBalancerOutput :: ApplySecurityGroupsToLoadBalancerOutput
applySecurityGroupsToLoadBalancerOutput = ApplySecurityGroupsToLoadBalancerOutput
    { _asgtlboSecurityGroups = mempty
    }

-- | A list of security group IDs associated with your load balancer.
asgtlboSecurityGroups :: Lens' ApplySecurityGroupsToLoadBalancerOutput [Text]
asgtlboSecurityGroups =
    lens _asgtlboSecurityGroups (\s a -> s { _asgtlboSecurityGroups = a })

instance FromXML ApplySecurityGroupsToLoadBalancerOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ApplySecurityGroupsToLoadBalancerOutput"

instance AWSRequest ApplySecurityGroupsToLoadBalancerInput where
    type Sv ApplySecurityGroupsToLoadBalancerInput = ELB
    type Rs ApplySecurityGroupsToLoadBalancerInput = ApplySecurityGroupsToLoadBalancerOutput

    request  = post "ApplySecurityGroupsToLoadBalancer"
    response = xmlResponse $ \h x -> ApplySecurityGroupsToLoadBalancerOutput
        <$> x %| "SecurityGroups"
