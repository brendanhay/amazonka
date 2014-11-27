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

-- Module      : Network.AWS.AutoScaling.DescribeMetricCollectionTypes
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

-- | Returns a list of metrics and a corresponding list of granularities for each
-- metric.
--
-- The 'GroupStandbyInstances' metric is not returned by default. You must
-- explicitly request it when calling 'EnableMetricsCollection'.
--
--
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeMetricCollectionTypes.html>
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

data DescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse
    { _dmctrGranularities :: List "member" MetricGranularityType
    , _dmctrMetrics       :: List "member" MetricCollectionType
    } deriving (Eq, Show)

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

-- | The granularities for the listed metrics.
dmctrGranularities :: Lens' DescribeMetricCollectionTypesResponse [MetricGranularityType]
dmctrGranularities =
    lens _dmctrGranularities (\s a -> s { _dmctrGranularities = a })
        . _List

-- | One or more of the following metrics:
--
-- GroupMinSize
--
-- GroupMaxSize
--
-- GroupDesiredCapacity
--
-- GroupInServiceInstances
--
-- GroupPendingInstances
--
-- GroupStandbyInstances
--
-- GroupTerminatingInstances
--
-- GroupTotalInstances
--
-- The 'GroupStandbyInstances' metric is not returned by default. You must
-- explicitly request it when calling 'EnableMetricsCollection'.
--
--
dmctrMetrics :: Lens' DescribeMetricCollectionTypesResponse [MetricCollectionType]
dmctrMetrics = lens _dmctrMetrics (\s a -> s { _dmctrMetrics = a }) . _List

instance ToPath DescribeMetricCollectionTypes where
    toPath = const "/"

instance ToQuery DescribeMetricCollectionTypes where
    toQuery = const mempty

instance ToHeaders DescribeMetricCollectionTypes

instance AWSRequest DescribeMetricCollectionTypes where
    type Sv DescribeMetricCollectionTypes = AutoScaling
    type Rs DescribeMetricCollectionTypes = DescribeMetricCollectionTypesResponse

    request  = post "DescribeMetricCollectionTypes"
    response = xmlResponse

instance FromXML DescribeMetricCollectionTypesResponse where
    parseXML = withElement "DescribeMetricCollectionTypesResult" $ \x -> DescribeMetricCollectionTypesResponse
        <$> x .@  "Granularities"
        <*> x .@  "Metrics"
