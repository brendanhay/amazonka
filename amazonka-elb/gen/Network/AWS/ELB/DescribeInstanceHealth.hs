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

-- Module      : Network.AWS.ELB.DescribeInstanceHealth
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the current state of the specified instances registered with the
-- specified load balancer. If no instances are specified, the state of all
-- the instances registered with the load balancer is returned. You must
-- provide the same account credentials as those that were used to create the
-- load balancer.
module Network.AWS.ELB.DescribeInstanceHealth
    (
    -- * Request
      DescribeEndPointStateInput
    -- ** Request constructor
    , describeEndPointStateInput
    -- ** Request lenses
    , depsiInstances
    , depsiLoadBalancerName

    -- * Response
    , DescribeEndPointStateOutput
    -- ** Response constructor
    , describeEndPointStateOutput
    -- ** Response lenses
    , depsoInstanceStates
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data DescribeEndPointStateInput = DescribeEndPointStateInput
    { _depsiInstances        :: [Instance]
    , _depsiLoadBalancerName :: Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeEndPointStateInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'depsiInstances' @::@ ['Instance']
--
-- * 'depsiLoadBalancerName' @::@ 'Text'
--
describeEndPointStateInput :: Text -- ^ 'depsiLoadBalancerName'
                           -> DescribeEndPointStateInput
describeEndPointStateInput p1 = DescribeEndPointStateInput
    { _depsiLoadBalancerName = p1
    , _depsiInstances        = mempty
    }

-- | A list of instance IDs whose states are being queried.
depsiInstances :: Lens' DescribeEndPointStateInput [Instance]
depsiInstances = lens _depsiInstances (\s a -> s { _depsiInstances = a })

-- | The name of the load balancer.
depsiLoadBalancerName :: Lens' DescribeEndPointStateInput Text
depsiLoadBalancerName =
    lens _depsiLoadBalancerName (\s a -> s { _depsiLoadBalancerName = a })
instance ToQuery DescribeEndPointStateInput

instance ToPath DescribeEndPointStateInput where
    toPath = const "/"

newtype DescribeEndPointStateOutput = DescribeEndPointStateOutput
    { _depsoInstanceStates :: [InstanceState]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'DescribeEndPointStateOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'depsoInstanceStates' @::@ ['InstanceState']
--
describeEndPointStateOutput :: DescribeEndPointStateOutput
describeEndPointStateOutput = DescribeEndPointStateOutput
    { _depsoInstanceStates = mempty
    }

-- | A list containing health information for the specified instances.
depsoInstanceStates :: Lens' DescribeEndPointStateOutput [InstanceState]
depsoInstanceStates =
    lens _depsoInstanceStates (\s a -> s { _depsoInstanceStates = a })
instance FromXML DescribeEndPointStateOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeEndPointStateOutput"

instance AWSRequest DescribeEndPointStateInput where
    type Sv DescribeEndPointStateInput = ELB
    type Rs DescribeEndPointStateInput = DescribeEndPointStateOutput

    request  = post "DescribeInstanceHealth"
    response = xmlResponse $ \h x -> DescribeEndPointStateOutput
        <$> x %| "InstanceStates"
