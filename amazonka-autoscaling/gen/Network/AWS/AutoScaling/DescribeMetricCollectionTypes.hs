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

-- Module      : Network.AWS.AutoScaling.DescribeMetricCollectionTypes
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
module Network.AWS.AutoScaling.DescribeMetricCollectionTypes
    (
    -- * Request
      DescribeMetricCollectionTypes
    -- ** Request constructor
    , describeMetricCollectionTypes

    -- * Response
    , DescribeMetricCollectionTypesAnswer
    -- ** Response constructor
    , describeMetricCollectionTypesAnswer
    -- ** Response lenses
    , dmctaGranularities
    , dmctaMetrics
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DescribeMetricCollectionTypes = DescribeMetricCollectionTypes
    deriving (Eq, Ord, Show, Generic)

-- | 'DescribeMetricCollectionTypes' constructor.
describeMetricCollectionTypes :: DescribeMetricCollectionTypes
describeMetricCollectionTypes = DescribeMetricCollectionTypes
instance ToQuery DescribeMetricCollectionTypes

instance ToPath DescribeMetricCollectionTypes where
    toPath = const "/"

data DescribeMetricCollectionTypesAnswer = DescribeMetricCollectionTypesAnswer
    { _dmctaGranularities :: [MetricGranularityType]
    , _dmctaMetrics       :: [MetricCollectionType]
    } deriving (Eq, Show, Generic)

-- | 'DescribeMetricCollectionTypesAnswer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmctaGranularities' @::@ ['MetricGranularityType']
--
-- * 'dmctaMetrics' @::@ ['MetricCollectionType']
--
describeMetricCollectionTypesAnswer :: DescribeMetricCollectionTypesAnswer
describeMetricCollectionTypesAnswer = DescribeMetricCollectionTypesAnswer
    { _dmctaMetrics       = mempty
    , _dmctaGranularities = mempty
    }

-- | A list of granularities for the listed Metrics.
dmctaGranularities :: Lens' DescribeMetricCollectionTypesAnswer [MetricGranularityType]
dmctaGranularities =
    lens _dmctaGranularities (\s a -> s { _dmctaGranularities = a })

-- | The list of Metrics collected. The following metrics are supported:
-- GroupMinSize GroupMaxSize GroupDesiredCapacity GroupInServiceInstances
-- GroupPendingInstances GroupStandbyInstances GroupTerminatingInstances
-- GroupTotalInstances.
dmctaMetrics :: Lens' DescribeMetricCollectionTypesAnswer [MetricCollectionType]
dmctaMetrics = lens _dmctaMetrics (\s a -> s { _dmctaMetrics = a })
instance FromXML DescribeMetricCollectionTypesAnswer where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeMetricCollectionTypesAnswer"

instance AWSRequest DescribeMetricCollectionTypes where
    type Sv DescribeMetricCollectionTypes = AutoScaling
    type Rs DescribeMetricCollectionTypes = DescribeMetricCollectionTypesAnswer

    request  = post "DescribeMetricCollectionTypes"
    response = xmlResponse $ \h x -> DescribeMetricCollectionTypesAnswer
        <$> x %| "Granularities"
        <*> x %| "Metrics"
