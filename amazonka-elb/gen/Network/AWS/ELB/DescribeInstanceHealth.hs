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

-- Module      : Network.AWS.ELB.DescribeInstanceHealth
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

-- | Returns the current state of the specified instances registered with the
-- specified load balancer. If no instances are specified, the state of all the
-- instances registered with the load balancer is returned.
--
-- You must provide the same account credentials as those that were used to
-- create the load balancer.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeInstanceHealth.html>
module Network.AWS.ELB.DescribeInstanceHealth
    (
    -- * Request
      DescribeInstanceHealth
    -- ** Request constructor
    , describeInstanceHealth
    -- ** Request lenses
    , dihInstances
    , dihLoadBalancerName

    -- * Response
    , DescribeInstanceHealthResponse
    -- ** Response constructor
    , describeInstanceHealthResponse
    -- ** Response lenses
    , dihrInstanceStates
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

data DescribeInstanceHealth = DescribeInstanceHealth
    { _dihInstances        :: List "Instances" Instance
    , _dihLoadBalancerName :: Text
    } deriving (Eq, Show)

-- | 'DescribeInstanceHealth' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dihInstances' @::@ ['Instance']
--
-- * 'dihLoadBalancerName' @::@ 'Text'
--
describeInstanceHealth :: Text -- ^ 'dihLoadBalancerName'
                       -> DescribeInstanceHealth
describeInstanceHealth p1 = DescribeInstanceHealth
    { _dihLoadBalancerName = p1
    , _dihInstances        = mempty
    }

-- | A list of instance IDs whose states are being queried.
dihInstances :: Lens' DescribeInstanceHealth [Instance]
dihInstances = lens _dihInstances (\s a -> s { _dihInstances = a }) . _List

-- | The name of the load balancer.
dihLoadBalancerName :: Lens' DescribeInstanceHealth Text
dihLoadBalancerName =
    lens _dihLoadBalancerName (\s a -> s { _dihLoadBalancerName = a })

newtype DescribeInstanceHealthResponse = DescribeInstanceHealthResponse
    { _dihrInstanceStates :: List "InstanceStates" InstanceState
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeInstanceHealthResponse where
    type Item DescribeInstanceHealthResponse = InstanceState

    fromList = DescribeInstanceHealthResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dihrInstanceStates

-- | 'DescribeInstanceHealthResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dihrInstanceStates' @::@ ['InstanceState']
--
describeInstanceHealthResponse :: DescribeInstanceHealthResponse
describeInstanceHealthResponse = DescribeInstanceHealthResponse
    { _dihrInstanceStates = mempty
    }

-- | A list containing health information for the specified instances.
dihrInstanceStates :: Lens' DescribeInstanceHealthResponse [InstanceState]
dihrInstanceStates =
    lens _dihrInstanceStates (\s a -> s { _dihrInstanceStates = a })
        . _List

instance ToPath DescribeInstanceHealth where
    toPath = const "/"

instance ToQuery DescribeInstanceHealth where
    toQuery DescribeInstanceHealth{..} = mconcat
        [ "Instances"        =? _dihInstances
        , "LoadBalancerName" =? _dihLoadBalancerName
        ]

instance ToHeaders DescribeInstanceHealth

instance AWSRequest DescribeInstanceHealth where
    type Sv DescribeInstanceHealth = ELB
    type Rs DescribeInstanceHealth = DescribeInstanceHealthResponse

    request  = post "DescribeInstanceHealth"
    response = xmlResponse

instance FromXML DescribeInstanceHealthResponse where
    parseXML = withElement "DescribeInstanceHealthResult" $ \x -> DescribeInstanceHealthResponse
        <$> x .@  "InstanceStates"
