{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeMetricCollectionTypes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Describes the available CloudWatch metrics for Auto Scaling.
--
-- Note that the @GroupStandbyInstances@ metric is not returned by default.
-- You must explicitly request this metric when calling
-- EnableMetricsCollection.
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
    , dmctrStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeMetricCollectionTypes' smart constructor.
data DescribeMetricCollectionTypes =
    DescribeMetricCollectionTypes'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeMetricCollectionTypes' smart constructor.
describeMetricCollectionTypes :: DescribeMetricCollectionTypes
describeMetricCollectionTypes = DescribeMetricCollectionTypes'

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
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

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
--
-- * 'dmctrStatus'
data DescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse'
    { _dmctrMetrics       :: !(Maybe [MetricCollectionType])
    , _dmctrGranularities :: !(Maybe [MetricGranularityType])
    , _dmctrStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeMetricCollectionTypesResponse' smart constructor.
describeMetricCollectionTypesResponse :: Int -> DescribeMetricCollectionTypesResponse
describeMetricCollectionTypesResponse pStatus =
    DescribeMetricCollectionTypesResponse'
    { _dmctrMetrics = Nothing
    , _dmctrGranularities = Nothing
    , _dmctrStatus = pStatus
    }

-- | One or more metrics.
dmctrMetrics :: Lens' DescribeMetricCollectionTypesResponse [MetricCollectionType]
dmctrMetrics = lens _dmctrMetrics (\ s a -> s{_dmctrMetrics = a}) . _Default;

-- | The granularities for the metrics.
dmctrGranularities :: Lens' DescribeMetricCollectionTypesResponse [MetricGranularityType]
dmctrGranularities = lens _dmctrGranularities (\ s a -> s{_dmctrGranularities = a}) . _Default;

-- | FIXME: Undocumented member.
dmctrStatus :: Lens' DescribeMetricCollectionTypesResponse Int
dmctrStatus = lens _dmctrStatus (\ s a -> s{_dmctrStatus = a});
