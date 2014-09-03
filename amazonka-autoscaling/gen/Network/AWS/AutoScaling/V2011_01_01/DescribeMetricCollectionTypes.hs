{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeMetricCollectionTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of metrics and a corresponding list of granularities for
-- each metric.
-- https://autoscaling.amazonaws.com/?Version=2011-01-01&Action=DescribeMetricCollectionTypes
-- &AUTHPARAMS oc/2011-01-01/"> GroupMinSize GroupMaxSize GroupDesiredCapacity
-- GroupInServiceInstances GroupPendingInstances GroupStandyInstances
-- GroupTerminatingInstances GroupTotalInstances 1Minute
-- 07f3fea2-bf3c-11e2-9b6f-f3cdbb80c073 The GroupStandbyInstances metric is
-- not returned by default. You must explicitly request it when calling
-- EnableMetricsCollection.
module Network.AWS.AutoScaling.V2011_01_01.DescribeMetricCollectionTypes
    (
    -- * Request
      DescribeMetricCollectionTypes
    -- ** Request constructor
    , describeMetricCollectionTypes
    -- * Response
    , DescribeMetricCollectionTypesResponse
    -- ** Response lenses
    , dmctaMetrics
    , dmctaGranularities
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeMetricCollectionTypes' request.
describeMetricCollectionTypes :: DescribeMetricCollectionTypes
describeMetricCollectionTypes = DescribeMetricCollectionTypes

data DescribeMetricCollectionTypes = DescribeMetricCollectionTypes
    deriving (Eq, Show, Generic)

instance ToQuery DescribeMetricCollectionTypes where
    toQuery = genericQuery def

data DescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse
    { _dmctaMetrics :: [MetricCollectionType]
      -- ^ The list of Metrics collected. The following metrics are
      -- supported: GroupMinSize GroupMaxSize GroupDesiredCapacity
      -- GroupInServiceInstances GroupPendingInstances
      -- GroupStandbyInstances GroupTerminatingInstances
      -- GroupTotalInstances The GroupStandbyInstances metric is not
      -- returned by default. You must explicitly request it when calling
      -- EnableMetricsCollection.
    , _dmctaGranularities :: [MetricGranularityType]
      -- ^ A list of granularities for the listed Metrics.
    } deriving (Show, Generic)

-- | The list of Metrics collected. The following metrics are supported:
-- GroupMinSize GroupMaxSize GroupDesiredCapacity GroupInServiceInstances
-- GroupPendingInstances GroupStandbyInstances GroupTerminatingInstances
-- GroupTotalInstances The GroupStandbyInstances metric is not returned by
-- default. You must explicitly request it when calling
-- EnableMetricsCollection.
dmctaMetrics
    :: Functor f
    => ([MetricCollectionType]
    -> f ([MetricCollectionType]))
    -> DescribeMetricCollectionTypesResponse
    -> f DescribeMetricCollectionTypesResponse
dmctaMetrics f x =
    (\y -> x { _dmctaMetrics = y })
       <$> f (_dmctaMetrics x)
{-# INLINE dmctaMetrics #-}

-- | A list of granularities for the listed Metrics.
dmctaGranularities
    :: Functor f
    => ([MetricGranularityType]
    -> f ([MetricGranularityType]))
    -> DescribeMetricCollectionTypesResponse
    -> f DescribeMetricCollectionTypesResponse
dmctaGranularities f x =
    (\y -> x { _dmctaGranularities = y })
       <$> f (_dmctaGranularities x)
{-# INLINE dmctaGranularities #-}

instance FromXML DescribeMetricCollectionTypesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeMetricCollectionTypes where
    type Sv DescribeMetricCollectionTypes = AutoScaling
    type Rs DescribeMetricCollectionTypes = DescribeMetricCollectionTypesResponse

    request = post "DescribeMetricCollectionTypes"
    response _ = xmlResponse
