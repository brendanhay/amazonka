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
    , DescribeMetricCollectionTypesResponse
    -- ** Response constructor
    , describeMetricCollectionTypesResponse
    -- ** Response lenses
    , dmctrGranularities
    , dmctrMetrics
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DescribeMetricCollectionTypes = DescribeMetricCollectionTypes
    deriving (Eq, Ord, Show, Generic)

-- | 'DescribeMetricCollectionTypes' constructor.
describeMetricCollectionTypes :: DescribeMetricCollectionTypes
describeMetricCollectionTypes = DescribeMetricCollectionTypes

instance ToQuery DescribeMetricCollectionTypes

instance ToPath DescribeMetricCollectionTypes where
    toPath = const "/"

data DescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse
    { _dmctrGranularities :: [MetricGranularityType]
    , _dmctrMetrics       :: [MetricCollectionType]
    } deriving (Eq, Show, Generic)

-- | 'DescribeMetricCollectionTypesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmctrGranularities' @::@ ['MetricGranularityType']
--
-- * 'dmctrMetrics' @::@ ['MetricCollectionType']
--
describeMetricCollectionTypesResponse :: DescribeMetricCollectionTypesResponse
describeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse
    { _dmctrMetrics       = mempty
    , _dmctrGranularities = mempty
    }

-- | A list of granularities for the listed Metrics.
dmctrGranularities :: Lens' DescribeMetricCollectionTypesResponse [MetricGranularityType]
dmctrGranularities =
    lens _dmctrGranularities (\s a -> s { _dmctrGranularities = a })

-- | The list of Metrics collected. The following metrics are supported:
-- GroupMinSize GroupMaxSize GroupDesiredCapacity GroupInServiceInstances
-- GroupPendingInstances GroupStandbyInstances GroupTerminatingInstances
-- GroupTotalInstances.
dmctrMetrics :: Lens' DescribeMetricCollectionTypesResponse [MetricCollectionType]
dmctrMetrics = lens _dmctrMetrics (\s a -> s { _dmctrMetrics = a })

instance AWSRequest DescribeMetricCollectionTypes where
    type Sv DescribeMetricCollectionTypes = AutoScaling
    type Rs DescribeMetricCollectionTypes = DescribeMetricCollectionTypesResponse

    request  = post "DescribeMetricCollectionTypes"
    response = xmlResponse $ \h x -> DescribeMetricCollectionTypesResponse
        <$> x %| "Granularities"
        <*> x %| "Metrics"
