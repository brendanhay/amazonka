{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling
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
module Network.AWS.AutoScaling
    (
    -- * Request
      DescribeMetricCollectionTypes
    -- ** Request constructor
    , mkDescribeMetricCollectionTypes
    -- * Response
    , DescribeMetricCollectionTypesResponse
    -- ** Response constructor
    , mkDescribeMetricCollectionTypesResponse
    -- ** Response lenses
    , dmctrMetrics
    , dmctrGranularities
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

data DescribeMetricCollectionTypes = DescribeMetricCollectionTypes
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeMetricCollectionTypes' request.
mkDescribeMetricCollectionTypes :: DescribeMetricCollectionTypes
mkDescribeMetricCollectionTypes = DescribeMetricCollectionTypes

instance ToQuery DescribeMetricCollectionTypes where
    toQuery = genericQuery def

-- | The output of the DescribeMetricCollectionTypes action.
data DescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse
    { _dmctrMetrics :: [MetricCollectionType]
    , _dmctrGranularities :: [MetricGranularityType]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeMetricCollectionTypesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Metrics ::@ @[MetricCollectionType]@
--
-- * @Granularities ::@ @[MetricGranularityType]@
--
mkDescribeMetricCollectionTypesResponse :: DescribeMetricCollectionTypesResponse
mkDescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse
    { _dmctrMetrics = mempty
    , _dmctrGranularities = mempty
    }

-- | The list of Metrics collected. The following metrics are supported:
-- GroupMinSize GroupMaxSize GroupDesiredCapacity GroupInServiceInstances
-- GroupPendingInstances GroupStandbyInstances GroupTerminatingInstances
-- GroupTotalInstances The GroupStandbyInstances metric is not returned by
-- default. You must explicitly request it when calling
-- EnableMetricsCollection.
dmctrMetrics :: Lens' DescribeMetricCollectionTypesResponse [MetricCollectionType]
dmctrMetrics = lens _dmctrMetrics (\s a -> s { _dmctrMetrics = a })

-- | A list of granularities for the listed Metrics.
dmctrGranularities :: Lens' DescribeMetricCollectionTypesResponse [MetricGranularityType]
dmctrGranularities =
    lens _dmctrGranularities (\s a -> s { _dmctrGranularities = a })

instance FromXML DescribeMetricCollectionTypesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeMetricCollectionTypes where
    type Sv DescribeMetricCollectionTypes = AutoScaling
    type Rs DescribeMetricCollectionTypes = DescribeMetricCollectionTypesResponse

    request = post "DescribeMetricCollectionTypes"
    response _ = xmlResponse
