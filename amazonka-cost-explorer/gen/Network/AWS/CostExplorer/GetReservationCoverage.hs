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
-- Module      : Network.AWS.CostExplorer.GetReservationCoverage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the reservation coverage for your account. This allows you to see how much of your Amazon Elastic Compute Cloud, Amazon ElastiCache, Amazon Relational Database Service, or Amazon Redshift usage is covered by a reservation. An organization's master account can see the coverage of the associated member accounts. For any time period, you can filter data about reservation usage by the following dimensions:
--
--
--     * AZ
--
--     * CACHE_ENGINE
--
--     * DATABASE_ENGINE
--
--     * DEPLOYMENT_OPTION
--
--     * INSTANCE_TYPE
--
--     * LINKED_ACCOUNT
--
--     * OPERATING_SYSTEM
--
--     * PLATFORM
--
--     * REGION
--
--     * SERVICE
--
--     * TAG
--
--     * TENANCY
--
--
--
-- To determine valid values for a dimension, use the @GetDimensionValues@ operation.
--
module Network.AWS.CostExplorer.GetReservationCoverage
    (
    -- * Creating a Request
      getReservationCoverage
    , GetReservationCoverage
    -- * Request Lenses
    , grcGroupBy
    , grcNextPageToken
    , grcGranularity
    , grcFilter
    , grcTimePeriod

    -- * Destructuring the Response
    , getReservationCoverageResponse
    , GetReservationCoverageResponse
    -- * Response Lenses
    , grcrsNextPageToken
    , grcrsTotal
    , grcrsResponseStatus
    , grcrsCoveragesByTime
    ) where

import Network.AWS.CostExplorer.Types
import Network.AWS.CostExplorer.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | You can use the following request parameters to query for how much of your instance usage is covered by a reservation.
--
--
--
-- /See:/ 'getReservationCoverage' smart constructor.
data GetReservationCoverage = GetReservationCoverage'
  { _grcGroupBy       :: !(Maybe [GroupDefinition])
  , _grcNextPageToken :: !(Maybe Text)
  , _grcGranularity   :: !(Maybe Granularity)
  , _grcFilter        :: !(Maybe Expression)
  , _grcTimePeriod    :: !DateInterval
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetReservationCoverage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grcGroupBy' - You can group the data by the following attributes:     * AZ     * CACHE_ENGINE     * DATABASE_ENGINE     * DEPLOYMENT_OPTION     * INSTANCE_TYPE     * LINKED_ACCOUNT     * OPERATING_SYSTEM     * PLATFORM     * REGION     * TAG     * TENANCY
--
-- * 'grcNextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'grcGranularity' - The granularity of the AWS cost data for the reservation. Valid values are @MONTHLY@ and @DAILY@ . If @GroupBy@ is set, @Granularity@ can't be set. If @Granularity@ isn't set, the response object doesn't include @Granularity@ , either @MONTHLY@ or @DAILY@ .
--
-- * 'grcFilter' - Filters utilization data by dimensions. You can filter by the following dimensions:     * AZ     * CACHE_ENGINE     * DATABASE_ENGINE     * DEPLOYMENT_OPTION     * INSTANCE_TYPE     * LINKED_ACCOUNT     * OPERATING_SYSTEM     * PLATFORM     * REGION     * SERVICE     * TAG     * TENANCY @GetReservationCoverage@ uses the same @<http://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> @ object as the other operations, but only @AND@ is supported among each dimension. You can nest only one level deep. If there are multiple values for a dimension, they are OR'd together.
--
-- * 'grcTimePeriod' - The start and end dates of the period for which you want to retrieve data about reservation coverage. You can retrieve data for a maximum of 13 months: the last 12 months and the current month. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
getReservationCoverage
    :: DateInterval -- ^ 'grcTimePeriod'
    -> GetReservationCoverage
getReservationCoverage pTimePeriod_ =
  GetReservationCoverage'
    { _grcGroupBy = Nothing
    , _grcNextPageToken = Nothing
    , _grcGranularity = Nothing
    , _grcFilter = Nothing
    , _grcTimePeriod = pTimePeriod_
    }


-- | You can group the data by the following attributes:     * AZ     * CACHE_ENGINE     * DATABASE_ENGINE     * DEPLOYMENT_OPTION     * INSTANCE_TYPE     * LINKED_ACCOUNT     * OPERATING_SYSTEM     * PLATFORM     * REGION     * TAG     * TENANCY
grcGroupBy :: Lens' GetReservationCoverage [GroupDefinition]
grcGroupBy = lens _grcGroupBy (\ s a -> s{_grcGroupBy = a}) . _Default . _Coerce

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
grcNextPageToken :: Lens' GetReservationCoverage (Maybe Text)
grcNextPageToken = lens _grcNextPageToken (\ s a -> s{_grcNextPageToken = a})

-- | The granularity of the AWS cost data for the reservation. Valid values are @MONTHLY@ and @DAILY@ . If @GroupBy@ is set, @Granularity@ can't be set. If @Granularity@ isn't set, the response object doesn't include @Granularity@ , either @MONTHLY@ or @DAILY@ .
grcGranularity :: Lens' GetReservationCoverage (Maybe Granularity)
grcGranularity = lens _grcGranularity (\ s a -> s{_grcGranularity = a})

-- | Filters utilization data by dimensions. You can filter by the following dimensions:     * AZ     * CACHE_ENGINE     * DATABASE_ENGINE     * DEPLOYMENT_OPTION     * INSTANCE_TYPE     * LINKED_ACCOUNT     * OPERATING_SYSTEM     * PLATFORM     * REGION     * SERVICE     * TAG     * TENANCY @GetReservationCoverage@ uses the same @<http://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> @ object as the other operations, but only @AND@ is supported among each dimension. You can nest only one level deep. If there are multiple values for a dimension, they are OR'd together.
grcFilter :: Lens' GetReservationCoverage (Maybe Expression)
grcFilter = lens _grcFilter (\ s a -> s{_grcFilter = a})

-- | The start and end dates of the period for which you want to retrieve data about reservation coverage. You can retrieve data for a maximum of 13 months: the last 12 months and the current month. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
grcTimePeriod :: Lens' GetReservationCoverage DateInterval
grcTimePeriod = lens _grcTimePeriod (\ s a -> s{_grcTimePeriod = a})

instance AWSRequest GetReservationCoverage where
        type Rs GetReservationCoverage =
             GetReservationCoverageResponse
        request = postJSON costExplorer
        response
          = receiveJSON
              (\ s h x ->
                 GetReservationCoverageResponse' <$>
                   (x .?> "NextPageToken") <*> (x .?> "Total") <*>
                     (pure (fromEnum s))
                     <*> (x .?> "CoveragesByTime" .!@ mempty))

instance Hashable GetReservationCoverage where

instance NFData GetReservationCoverage where

instance ToHeaders GetReservationCoverage where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSInsightsIndexService.GetReservationCoverage" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetReservationCoverage where
        toJSON GetReservationCoverage'{..}
          = object
              (catMaybes
                 [("GroupBy" .=) <$> _grcGroupBy,
                  ("NextPageToken" .=) <$> _grcNextPageToken,
                  ("Granularity" .=) <$> _grcGranularity,
                  ("Filter" .=) <$> _grcFilter,
                  Just ("TimePeriod" .= _grcTimePeriod)])

instance ToPath GetReservationCoverage where
        toPath = const "/"

instance ToQuery GetReservationCoverage where
        toQuery = const mempty

-- | /See:/ 'getReservationCoverageResponse' smart constructor.
data GetReservationCoverageResponse = GetReservationCoverageResponse'
  { _grcrsNextPageToken   :: !(Maybe Text)
  , _grcrsTotal           :: !(Maybe Coverage)
  , _grcrsResponseStatus  :: !Int
  , _grcrsCoveragesByTime :: ![CoverageByTime]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetReservationCoverageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grcrsNextPageToken' - The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'grcrsTotal' - The total amount of instance usage that is covered by a reservation.
--
-- * 'grcrsResponseStatus' - -- | The response status code.
--
-- * 'grcrsCoveragesByTime' - The amount of time that your reservations covered.
getReservationCoverageResponse
    :: Int -- ^ 'grcrsResponseStatus'
    -> GetReservationCoverageResponse
getReservationCoverageResponse pResponseStatus_ =
  GetReservationCoverageResponse'
    { _grcrsNextPageToken = Nothing
    , _grcrsTotal = Nothing
    , _grcrsResponseStatus = pResponseStatus_
    , _grcrsCoveragesByTime = mempty
    }


-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
grcrsNextPageToken :: Lens' GetReservationCoverageResponse (Maybe Text)
grcrsNextPageToken = lens _grcrsNextPageToken (\ s a -> s{_grcrsNextPageToken = a})

-- | The total amount of instance usage that is covered by a reservation.
grcrsTotal :: Lens' GetReservationCoverageResponse (Maybe Coverage)
grcrsTotal = lens _grcrsTotal (\ s a -> s{_grcrsTotal = a})

-- | -- | The response status code.
grcrsResponseStatus :: Lens' GetReservationCoverageResponse Int
grcrsResponseStatus = lens _grcrsResponseStatus (\ s a -> s{_grcrsResponseStatus = a})

-- | The amount of time that your reservations covered.
grcrsCoveragesByTime :: Lens' GetReservationCoverageResponse [CoverageByTime]
grcrsCoveragesByTime = lens _grcrsCoveragesByTime (\ s a -> s{_grcrsCoveragesByTime = a}) . _Coerce

instance NFData GetReservationCoverageResponse where
