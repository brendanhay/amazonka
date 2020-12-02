{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeMetricCollectionTypes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available CloudWatch metrics for Auto Scaling.
--
--
-- Note that the @GroupStandbyInstances@ metric is not returned by default. You must explicitly request this metric when calling 'EnableMetricsCollection' .
--
module Network.AWS.AutoScaling.DescribeMetricCollectionTypes
    (
    -- * Creating a Request
      describeMetricCollectionTypes
    , DescribeMetricCollectionTypes

    -- * Destructuring the Response
    , describeMetricCollectionTypesResponse
    , DescribeMetricCollectionTypesResponse
    -- * Response Lenses
    , dmctrsMetrics
    , dmctrsGranularities
    , dmctrsResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeMetricCollectionTypes' smart constructor.
data DescribeMetricCollectionTypes =
  DescribeMetricCollectionTypes'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMetricCollectionTypes' with the minimum fields required to make a request.
--
describeMetricCollectionTypes
    :: DescribeMetricCollectionTypes
describeMetricCollectionTypes = DescribeMetricCollectionTypes'


instance AWSRequest DescribeMetricCollectionTypes
         where
        type Rs DescribeMetricCollectionTypes =
             DescribeMetricCollectionTypesResponse
        request = postQuery autoScaling
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

instance Hashable DescribeMetricCollectionTypes where

instance NFData DescribeMetricCollectionTypes where

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
data DescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse'
  { _dmctrsMetrics        :: !(Maybe [MetricCollectionType])
  , _dmctrsGranularities  :: !(Maybe [MetricGranularityType])
  , _dmctrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMetricCollectionTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmctrsMetrics' - One or more metrics.
--
-- * 'dmctrsGranularities' - The granularities for the metrics.
--
-- * 'dmctrsResponseStatus' - -- | The response status code.
describeMetricCollectionTypesResponse
    :: Int -- ^ 'dmctrsResponseStatus'
    -> DescribeMetricCollectionTypesResponse
describeMetricCollectionTypesResponse pResponseStatus_ =
  DescribeMetricCollectionTypesResponse'
    { _dmctrsMetrics = Nothing
    , _dmctrsGranularities = Nothing
    , _dmctrsResponseStatus = pResponseStatus_
    }


-- | One or more metrics.
dmctrsMetrics :: Lens' DescribeMetricCollectionTypesResponse [MetricCollectionType]
dmctrsMetrics = lens _dmctrsMetrics (\ s a -> s{_dmctrsMetrics = a}) . _Default . _Coerce

-- | The granularities for the metrics.
dmctrsGranularities :: Lens' DescribeMetricCollectionTypesResponse [MetricGranularityType]
dmctrsGranularities = lens _dmctrsGranularities (\ s a -> s{_dmctrsGranularities = a}) . _Default . _Coerce

-- | -- | The response status code.
dmctrsResponseStatus :: Lens' DescribeMetricCollectionTypesResponse Int
dmctrsResponseStatus = lens _dmctrsResponseStatus (\ s a -> s{_dmctrsResponseStatus = a})

instance NFData DescribeMetricCollectionTypesResponse
         where
