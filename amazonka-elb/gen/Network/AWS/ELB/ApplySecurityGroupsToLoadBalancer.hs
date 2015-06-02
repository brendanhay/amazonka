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

-- Module      : Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer
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

-- | Associates one or more security groups with your load balancer in a virtual
-- private cloud (VPC). The specified security groups override the previously
-- associated security groups.
--
-- For more information, see <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/USVPC_ApplySG.html Manage Security Groups for Amazon VPC> in the /Elastic Load Balancing Developer Guide/.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_ApplySecurityGroupsToLoadBalancer.html>
module Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer
    (
    -- * Request
      ApplySecurityGroupsToLoadBalancer
    -- ** Request constructor
    , applySecurityGroupsToLoadBalancer
    -- ** Request lenses
    , asgtlbLoadBalancerName
    , asgtlbSecurityGroups

    -- * Response
    , ApplySecurityGroupsToLoadBalancerResponse
    -- ** Response constructor
    , applySecurityGroupsToLoadBalancerResponse
    -- ** Response lenses
    , asgtlbrSecurityGroups
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

data ApplySecurityGroupsToLoadBalancer = ApplySecurityGroupsToLoadBalancer
    { _asgtlbLoadBalancerName :: Text
    , _asgtlbSecurityGroups   :: List "member" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ApplySecurityGroupsToLoadBalancer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgtlbLoadBalancerName' @::@ 'Text'
--
-- * 'asgtlbSecurityGroups' @::@ ['Text']
--
applySecurityGroupsToLoadBalancer :: Text -- ^ 'asgtlbLoadBalancerName'
                                  -> ApplySecurityGroupsToLoadBalancer
applySecurityGroupsToLoadBalancer p1 = ApplySecurityGroupsToLoadBalancer
    { _asgtlbLoadBalancerName = p1
    , _asgtlbSecurityGroups   = mempty
    }

-- | The name of the load balancer.
asgtlbLoadBalancerName :: Lens' ApplySecurityGroupsToLoadBalancer Text
asgtlbLoadBalancerName =
    lens _asgtlbLoadBalancerName (\s a -> s { _asgtlbLoadBalancerName = a })

-- | The IDs of the security groups to associate with the load balancer. Note that
-- you cannot specify the name of the security group.
asgtlbSecurityGroups :: Lens' ApplySecurityGroupsToLoadBalancer [Text]
asgtlbSecurityGroups =
    lens _asgtlbSecurityGroups (\s a -> s { _asgtlbSecurityGroups = a })
        . _List

newtype ApplySecurityGroupsToLoadBalancerResponse = ApplySecurityGroupsToLoadBalancerResponse
    { _asgtlbrSecurityGroups :: List "member" Text
    } deriving (Eq, Ord, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList ApplySecurityGroupsToLoadBalancerResponse where
    type Item ApplySecurityGroupsToLoadBalancerResponse = Text

    fromList = ApplySecurityGroupsToLoadBalancerResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _asgtlbrSecurityGroups

-- | 'ApplySecurityGroupsToLoadBalancerResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgtlbrSecurityGroups' @::@ ['Text']
--
applySecurityGroupsToLoadBalancerResponse :: ApplySecurityGroupsToLoadBalancerResponse
applySecurityGroupsToLoadBalancerResponse = ApplySecurityGroupsToLoadBalancerResponse
    { _asgtlbrSecurityGroups = mempty
    }

-- | The IDs of the security groups associated with the load balancer.
asgtlbrSecurityGroups :: Lens' ApplySecurityGroupsToLoadBalancerResponse [Text]
asgtlbrSecurityGroups =
    lens _asgtlbrSecurityGroups (\s a -> s { _asgtlbrSecurityGroups = a })
        . _List

instance ToPath ApplySecurityGroupsToLoadBalancer where
    toPath = const "/"

instance ToQuery ApplySecurityGroupsToLoadBalancer where
    toQuery ApplySecurityGroupsToLoadBalancer{..} = mconcat
        [ "LoadBalancerName" =? _asgtlbLoadBalancerName
        , "SecurityGroups"   =? _asgtlbSecurityGroups
        ]

instance ToHeaders ApplySecurityGroupsToLoadBalancer

instance AWSRequest ApplySecurityGroupsToLoadBalancer where
    type Sv ApplySecurityGroupsToLoadBalancer = ELB
    type Rs ApplySecurityGroupsToLoadBalancer = ApplySecurityGroupsToLoadBalancerResponse

    request  = post "ApplySecurityGroupsToLoadBalancer"
    response = xmlResponse

instance FromXML ApplySecurityGroupsToLoadBalancerResponse where
    parseXML = withElement "ApplySecurityGroupsToLoadBalancerResult" $ \x -> ApplySecurityGroupsToLoadBalancerResponse
        <$> x .@? "SecurityGroups" .!@ mempty
