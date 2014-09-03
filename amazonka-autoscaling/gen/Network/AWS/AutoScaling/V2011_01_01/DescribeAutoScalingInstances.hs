{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeAutoScalingInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a description of each Auto Scaling instance in the InstanceIds
-- list. If a list is not provided, the service returns the full details of
-- all instances up to a maximum of 50. By default, the service returns a list
-- of 20 items. This action supports pagination by returning a token if there
-- are more pages to retrieve. To get the next page, call this action again
-- with the returned token as the NextToken parameter.
-- https://autoscaling.amazonaws.com/?MaxRecords=20
-- &InstanceIds.member.1=i-78e0d40b &Version=2011-01-01
-- &Action=DescribeAutoScalingInstances &AUTHPARAMS Healthy my-test-asg
-- us-east-1e i-78e0d40b my-test-lc InService
-- df992dc3-b72f-11e2-81e1-750aa6EXAMPLE.
module Network.AWS.AutoScaling.V2011_01_01.DescribeAutoScalingInstances
    (
    -- * Request
      DescribeAutoScalingInstances
    -- ** Request constructor
    , describeAutoScalingInstances
    -- ** Request lenses
    , dasitInstanceIds
    , dasitMaxRecords
    , dasitNextToken

    -- * Response
    , DescribeAutoScalingInstancesResponse
    -- ** Response lenses
    , asitAutoScalingInstances
    , asitNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeAutoScalingInstances' request.
describeAutoScalingInstances :: DescribeAutoScalingInstances
describeAutoScalingInstances = DescribeAutoScalingInstances
    { _dasitInstanceIds = mempty
    , _dasitMaxRecords = Nothing
    , _dasitNextToken = Nothing
    }

data DescribeAutoScalingInstances = DescribeAutoScalingInstances
    { _dasitInstanceIds :: [Text]
      -- ^ The list of Auto Scaling instances to describe. If this list is
      -- omitted, all auto scaling instances are described. The list of
      -- requested instances cannot contain more than 50 items. If unknown
      -- instances are requested, they are ignored with no error.
    , _dasitMaxRecords :: Maybe Integer
      -- ^ The maximum number of Auto Scaling instances to be described with
      -- each call.
    , _dasitNextToken :: Maybe Text
      -- ^ The token returned by a previous call to indicate that there is
      -- more data available.
    } deriving (Show, Generic)

-- | The list of Auto Scaling instances to describe. If this list is omitted,
-- all auto scaling instances are described. The list of requested instances
-- cannot contain more than 50 items. If unknown instances are requested, they
-- are ignored with no error.
dasitInstanceIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeAutoScalingInstances
    -> f DescribeAutoScalingInstances
dasitInstanceIds f x =
    (\y -> x { _dasitInstanceIds = y })
       <$> f (_dasitInstanceIds x)
{-# INLINE dasitInstanceIds #-}

-- | The maximum number of Auto Scaling instances to be described with each
-- call.
dasitMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeAutoScalingInstances
    -> f DescribeAutoScalingInstances
dasitMaxRecords f x =
    (\y -> x { _dasitMaxRecords = y })
       <$> f (_dasitMaxRecords x)
{-# INLINE dasitMaxRecords #-}

-- | The token returned by a previous call to indicate that there is more data
-- available.
dasitNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeAutoScalingInstances
    -> f DescribeAutoScalingInstances
dasitNextToken f x =
    (\y -> x { _dasitNextToken = y })
       <$> f (_dasitNextToken x)
{-# INLINE dasitNextToken #-}

instance ToQuery DescribeAutoScalingInstances where
    toQuery = genericQuery def

data DescribeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse
    { _asitAutoScalingInstances :: [AutoScalingInstanceDetails]
      -- ^ A list of Auto Scaling instances.
    , _asitNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Show, Generic)

-- | A list of Auto Scaling instances.
asitAutoScalingInstances
    :: Functor f
    => ([AutoScalingInstanceDetails]
    -> f ([AutoScalingInstanceDetails]))
    -> DescribeAutoScalingInstancesResponse
    -> f DescribeAutoScalingInstancesResponse
asitAutoScalingInstances f x =
    (\y -> x { _asitAutoScalingInstances = y })
       <$> f (_asitAutoScalingInstances x)
{-# INLINE asitAutoScalingInstances #-}

-- | A string that marks the start of the next batch of returned results.
asitNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeAutoScalingInstancesResponse
    -> f DescribeAutoScalingInstancesResponse
asitNextToken f x =
    (\y -> x { _asitNextToken = y })
       <$> f (_asitNextToken x)
{-# INLINE asitNextToken #-}

instance FromXML DescribeAutoScalingInstancesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAutoScalingInstances where
    type Sv DescribeAutoScalingInstances = AutoScaling
    type Rs DescribeAutoScalingInstances = DescribeAutoScalingInstancesResponse

    request = post "DescribeAutoScalingInstances"
    response _ = xmlResponse

instance AWSPager DescribeAutoScalingInstances where
    next rq rs = (\x -> rq { _dasitNextToken = Just x })
        <$> (_asitNextToken rs)
