{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.DescribeMetricCollectionTypes
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a list of metrics and a corresponding list of granularities for
-- each metric.
--
-- The @GroupStandbyInstances@ metric is not returned by default. You must
-- explicitly request it when calling EnableMetricsCollection.
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
    , dmctrMetrics
    , dmctrGranularities
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.AutoScaling.Types

-- | /See:/ 'describeMetricCollectionTypes' smart constructor.
data DescribeMetricCollectionTypes = DescribeMetricCollectionTypes' deriving (Eq, Read, Show)

-- | 'DescribeMetricCollectionTypes' smart constructor.
describeMetricCollectionTypes :: DescribeMetricCollectionTypes
describeMetricCollectionTypes = DescribeMetricCollectionTypes';

instance AWSRequest DescribeMetricCollectionTypes
         where
        type Sv DescribeMetricCollectionTypes = AutoScaling
        type Rs DescribeMetricCollectionTypes =
             DescribeMetricCollectionTypesResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeMetricCollectionTypesResult"
              (\ s h x ->
                 DescribeMetricCollectionTypesResponse' <$>
                   (x .@? "Metrics" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*>
                     (x .@? "Granularities" .!@ mempty >>=
                        may (parseXMLList "member")))

instance ToHeaders DescribeMetricCollectionTypes
         where
        toHeaders = const mempty

instance ToPath DescribeMetricCollectionTypes where
        toPath = const "/"

instance ToQuery DescribeMetricCollectionTypes where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("DescribeMetricCollectionTypes" :: ByteString),
                  "Version" =: ("2011-01-01" :: ByteString)])

-- | /See:/ 'describeMetricCollectionTypesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmctrMetrics'
--
-- * 'dmctrGranularities'
data DescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse'{_dmctrMetrics :: Maybe [MetricCollectionType], _dmctrGranularities :: Maybe [MetricGranularityType]} deriving (Eq, Read, Show)

-- | 'DescribeMetricCollectionTypesResponse' smart constructor.
describeMetricCollectionTypesResponse :: DescribeMetricCollectionTypesResponse
describeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse'{_dmctrMetrics = Nothing, _dmctrGranularities = Nothing};

-- | One or more of the following metrics:
--
-- -   GroupMinSize
--
-- -   GroupMaxSize
--
-- -   GroupDesiredCapacity
--
-- -   GroupInServiceInstances
--
-- -   GroupPendingInstances
--
-- -   GroupStandbyInstances
--
-- -   GroupTerminatingInstances
--
-- -   GroupTotalInstances
--
-- The @GroupStandbyInstances@ metric is not returned by default. You must
-- explicitly request it when calling EnableMetricsCollection.
dmctrMetrics :: Lens' DescribeMetricCollectionTypesResponse [MetricCollectionType]
dmctrMetrics = lens _dmctrMetrics (\ s a -> s{_dmctrMetrics = a}) . _Default;

-- | The granularities for the listed metrics.
dmctrGranularities :: Lens' DescribeMetricCollectionTypesResponse [MetricGranularityType]
dmctrGranularities = lens _dmctrGranularities (\ s a -> s{_dmctrGranularities = a}) . _Default;
