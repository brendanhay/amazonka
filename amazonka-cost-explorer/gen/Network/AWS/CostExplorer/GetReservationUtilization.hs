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
-- Module      : Network.AWS.CostExplorer.GetReservationUtilization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the reservation utilization for your account. Master accounts in an organization have access to member accounts. You can filter data by dimensions in a time period. You can use @GetDimensionValues@ to determine the possible dimension values. Currently, you can group only by @SUBSCRIPTION_ID@ .
--
--
module Network.AWS.CostExplorer.GetReservationUtilization
    (
    -- * Creating a Request
      getReservationUtilization
    , GetReservationUtilization
    -- * Request Lenses
    , gruGroupBy
    , gruNextPageToken
    , gruGranularity
    , gruFilter
    , gruTimePeriod

    -- * Destructuring the Response
    , getReservationUtilizationResponse
    , GetReservationUtilizationResponse
    -- * Response Lenses
    , grursNextPageToken
    , grursTotal
    , grursResponseStatus
    , grursUtilizationsByTime
    ) where

import Network.AWS.CostExplorer.Types
import Network.AWS.CostExplorer.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getReservationUtilization' smart constructor.
data GetReservationUtilization = GetReservationUtilization'
  { _gruGroupBy       :: !(Maybe [GroupDefinition])
  , _gruNextPageToken :: !(Maybe Text)
  , _gruGranularity   :: !(Maybe Granularity)
  , _gruFilter        :: !(Maybe Expression)
  , _gruTimePeriod    :: !DateInterval
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetReservationUtilization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gruGroupBy' - Groups only by @SUBSCRIPTION_ID@ . Metadata is included.
--
-- * 'gruNextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gruGranularity' - If @GroupBy@ is set, @Granularity@ can't be set. If @Granularity@ isn't set, the response object doesn't include @Granularity@ , either @MONTHLY@ or @DAILY@ . If both @GroupBy@ and @Granularity@ aren't set, @GetReservationUtilization@ defaults to @DAILY@ .
--
-- * 'gruFilter' - Filters utilization data by dimensions. You can filter by the following dimensions:     * AZ     * CACHE_ENGINE     * DATABASE_ENGINE     * DEPLOYMENT_OPTION     * INSTANCE_TYPE     * LINKED_ACCOUNT     * OPERATING_SYSTEM     * PLATFORM     * REGION     * SERVICE     * SCOPE     * TENANCY @GetReservationUtilization@ uses the same @<http://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> @ object as the other operations, but only @AND@ is supported among each dimension, and nesting is supported up to only one level deep. If there are multiple values for a dimension, they are OR'd together.
--
-- * 'gruTimePeriod' - Sets the start and end dates for retrieving Reserved Instance (RI) utilization. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
getReservationUtilization
    :: DateInterval -- ^ 'gruTimePeriod'
    -> GetReservationUtilization
getReservationUtilization pTimePeriod_ =
  GetReservationUtilization'
    { _gruGroupBy = Nothing
    , _gruNextPageToken = Nothing
    , _gruGranularity = Nothing
    , _gruFilter = Nothing
    , _gruTimePeriod = pTimePeriod_
    }


-- | Groups only by @SUBSCRIPTION_ID@ . Metadata is included.
gruGroupBy :: Lens' GetReservationUtilization [GroupDefinition]
gruGroupBy = lens _gruGroupBy (\ s a -> s{_gruGroupBy = a}) . _Default . _Coerce

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
gruNextPageToken :: Lens' GetReservationUtilization (Maybe Text)
gruNextPageToken = lens _gruNextPageToken (\ s a -> s{_gruNextPageToken = a})

-- | If @GroupBy@ is set, @Granularity@ can't be set. If @Granularity@ isn't set, the response object doesn't include @Granularity@ , either @MONTHLY@ or @DAILY@ . If both @GroupBy@ and @Granularity@ aren't set, @GetReservationUtilization@ defaults to @DAILY@ .
gruGranularity :: Lens' GetReservationUtilization (Maybe Granularity)
gruGranularity = lens _gruGranularity (\ s a -> s{_gruGranularity = a})

-- | Filters utilization data by dimensions. You can filter by the following dimensions:     * AZ     * CACHE_ENGINE     * DATABASE_ENGINE     * DEPLOYMENT_OPTION     * INSTANCE_TYPE     * LINKED_ACCOUNT     * OPERATING_SYSTEM     * PLATFORM     * REGION     * SERVICE     * SCOPE     * TENANCY @GetReservationUtilization@ uses the same @<http://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> @ object as the other operations, but only @AND@ is supported among each dimension, and nesting is supported up to only one level deep. If there are multiple values for a dimension, they are OR'd together.
gruFilter :: Lens' GetReservationUtilization (Maybe Expression)
gruFilter = lens _gruFilter (\ s a -> s{_gruFilter = a})

-- | Sets the start and end dates for retrieving Reserved Instance (RI) utilization. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
gruTimePeriod :: Lens' GetReservationUtilization DateInterval
gruTimePeriod = lens _gruTimePeriod (\ s a -> s{_gruTimePeriod = a})

instance AWSRequest GetReservationUtilization where
        type Rs GetReservationUtilization =
             GetReservationUtilizationResponse
        request = postJSON costExplorer
        response
          = receiveJSON
              (\ s h x ->
                 GetReservationUtilizationResponse' <$>
                   (x .?> "NextPageToken") <*> (x .?> "Total") <*>
                     (pure (fromEnum s))
                     <*> (x .?> "UtilizationsByTime" .!@ mempty))

instance Hashable GetReservationUtilization where

instance NFData GetReservationUtilization where

instance ToHeaders GetReservationUtilization where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSInsightsIndexService.GetReservationUtilization"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetReservationUtilization where
        toJSON GetReservationUtilization'{..}
          = object
              (catMaybes
                 [("GroupBy" .=) <$> _gruGroupBy,
                  ("NextPageToken" .=) <$> _gruNextPageToken,
                  ("Granularity" .=) <$> _gruGranularity,
                  ("Filter" .=) <$> _gruFilter,
                  Just ("TimePeriod" .= _gruTimePeriod)])

instance ToPath GetReservationUtilization where
        toPath = const "/"

instance ToQuery GetReservationUtilization where
        toQuery = const mempty

-- | /See:/ 'getReservationUtilizationResponse' smart constructor.
data GetReservationUtilizationResponse = GetReservationUtilizationResponse'
  { _grursNextPageToken      :: !(Maybe Text)
  , _grursTotal              :: !(Maybe ReservationAggregates)
  , _grursResponseStatus     :: !Int
  , _grursUtilizationsByTime :: ![UtilizationByTime]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetReservationUtilizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grursNextPageToken' - The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'grursTotal' - The total amount of time that you utilized your RIs.
--
-- * 'grursResponseStatus' - -- | The response status code.
--
-- * 'grursUtilizationsByTime' - The amount of time that you utilized your RIs.
getReservationUtilizationResponse
    :: Int -- ^ 'grursResponseStatus'
    -> GetReservationUtilizationResponse
getReservationUtilizationResponse pResponseStatus_ =
  GetReservationUtilizationResponse'
    { _grursNextPageToken = Nothing
    , _grursTotal = Nothing
    , _grursResponseStatus = pResponseStatus_
    , _grursUtilizationsByTime = mempty
    }


-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
grursNextPageToken :: Lens' GetReservationUtilizationResponse (Maybe Text)
grursNextPageToken = lens _grursNextPageToken (\ s a -> s{_grursNextPageToken = a})

-- | The total amount of time that you utilized your RIs.
grursTotal :: Lens' GetReservationUtilizationResponse (Maybe ReservationAggregates)
grursTotal = lens _grursTotal (\ s a -> s{_grursTotal = a})

-- | -- | The response status code.
grursResponseStatus :: Lens' GetReservationUtilizationResponse Int
grursResponseStatus = lens _grursResponseStatus (\ s a -> s{_grursResponseStatus = a})

-- | The amount of time that you utilized your RIs.
grursUtilizationsByTime :: Lens' GetReservationUtilizationResponse [UtilizationByTime]
grursUtilizationsByTime = lens _grursUtilizationsByTime (\ s a -> s{_grursUtilizationsByTime = a}) . _Coerce

instance NFData GetReservationUtilizationResponse
         where
