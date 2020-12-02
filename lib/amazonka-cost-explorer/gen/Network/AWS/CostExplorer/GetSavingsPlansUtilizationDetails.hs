{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetSavingsPlansUtilizationDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves attribute data along with aggregate utilization and savings data for a given time period. This doesn't support granular or grouped data (daily/monthly) in response. You can't retrieve data by dates in a single response similar to @GetSavingsPlanUtilization@ , but you have the option to make multiple calls to @GetSavingsPlanUtilizationDetails@ by providing individual dates. You can use @GetDimensionValues@ in @SAVINGS_PLANS@ to determine the possible dimension values.
module Network.AWS.CostExplorer.GetSavingsPlansUtilizationDetails
  ( -- * Creating a Request
    getSavingsPlansUtilizationDetails,
    GetSavingsPlansUtilizationDetails,

    -- * Request Lenses
    gspudNextToken,
    gspudFilter,
    gspudMaxResults,
    gspudTimePeriod,

    -- * Destructuring the Response
    getSavingsPlansUtilizationDetailsResponse,
    GetSavingsPlansUtilizationDetailsResponse,

    -- * Response Lenses
    gspudrsNextToken,
    gspudrsTotal,
    gspudrsResponseStatus,
    gspudrsSavingsPlansUtilizationDetails,
    gspudrsTimePeriod,
  )
where

import Network.AWS.CostExplorer.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSavingsPlansUtilizationDetails' smart constructor.
data GetSavingsPlansUtilizationDetails = GetSavingsPlansUtilizationDetails'
  { _gspudNextToken ::
      !(Maybe Text),
    _gspudFilter ::
      !(Maybe Expression),
    _gspudMaxResults ::
      !(Maybe Nat),
    _gspudTimePeriod ::
      !DateInterval
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSavingsPlansUtilizationDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gspudNextToken' - The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gspudFilter' - Filters Savings Plans utilization coverage data for active Savings Plans dimensions. You can filter data with the following dimensions:     * @LINKED_ACCOUNT@      * @SAVINGS_PLAN_ARN@      * @REGION@      * @PAYMENT_OPTION@      * @INSTANCE_TYPE_FAMILY@  @GetSavingsPlansUtilizationDetails@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension.
--
-- * 'gspudMaxResults' - The number of items to be returned in a response. The default is @20@ , with a minimum value of @1@ .
--
-- * 'gspudTimePeriod' - The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
getSavingsPlansUtilizationDetails ::
  -- | 'gspudTimePeriod'
  DateInterval ->
  GetSavingsPlansUtilizationDetails
getSavingsPlansUtilizationDetails pTimePeriod_ =
  GetSavingsPlansUtilizationDetails'
    { _gspudNextToken = Nothing,
      _gspudFilter = Nothing,
      _gspudMaxResults = Nothing,
      _gspudTimePeriod = pTimePeriod_
    }

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
gspudNextToken :: Lens' GetSavingsPlansUtilizationDetails (Maybe Text)
gspudNextToken = lens _gspudNextToken (\s a -> s {_gspudNextToken = a})

-- | Filters Savings Plans utilization coverage data for active Savings Plans dimensions. You can filter data with the following dimensions:     * @LINKED_ACCOUNT@      * @SAVINGS_PLAN_ARN@      * @REGION@      * @PAYMENT_OPTION@      * @INSTANCE_TYPE_FAMILY@  @GetSavingsPlansUtilizationDetails@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension.
gspudFilter :: Lens' GetSavingsPlansUtilizationDetails (Maybe Expression)
gspudFilter = lens _gspudFilter (\s a -> s {_gspudFilter = a})

-- | The number of items to be returned in a response. The default is @20@ , with a minimum value of @1@ .
gspudMaxResults :: Lens' GetSavingsPlansUtilizationDetails (Maybe Natural)
gspudMaxResults = lens _gspudMaxResults (\s a -> s {_gspudMaxResults = a}) . mapping _Nat

-- | The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
gspudTimePeriod :: Lens' GetSavingsPlansUtilizationDetails DateInterval
gspudTimePeriod = lens _gspudTimePeriod (\s a -> s {_gspudTimePeriod = a})

instance AWSRequest GetSavingsPlansUtilizationDetails where
  type
    Rs GetSavingsPlansUtilizationDetails =
      GetSavingsPlansUtilizationDetailsResponse
  request = postJSON costExplorer
  response =
    receiveJSON
      ( \s h x ->
          GetSavingsPlansUtilizationDetailsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "Total")
            <*> (pure (fromEnum s))
            <*> (x .?> "SavingsPlansUtilizationDetails" .!@ mempty)
            <*> (x .:> "TimePeriod")
      )

instance Hashable GetSavingsPlansUtilizationDetails

instance NFData GetSavingsPlansUtilizationDetails

instance ToHeaders GetSavingsPlansUtilizationDetails where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSInsightsIndexService.GetSavingsPlansUtilizationDetails" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetSavingsPlansUtilizationDetails where
  toJSON GetSavingsPlansUtilizationDetails' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _gspudNextToken,
            ("Filter" .=) <$> _gspudFilter,
            ("MaxResults" .=) <$> _gspudMaxResults,
            Just ("TimePeriod" .= _gspudTimePeriod)
          ]
      )

instance ToPath GetSavingsPlansUtilizationDetails where
  toPath = const "/"

instance ToQuery GetSavingsPlansUtilizationDetails where
  toQuery = const mempty

-- | /See:/ 'getSavingsPlansUtilizationDetailsResponse' smart constructor.
data GetSavingsPlansUtilizationDetailsResponse = GetSavingsPlansUtilizationDetailsResponse'
  { _gspudrsNextToken ::
      !( Maybe
           Text
       ),
    _gspudrsTotal ::
      !( Maybe
           SavingsPlansUtilizationAggregates
       ),
    _gspudrsResponseStatus ::
      !Int,
    _gspudrsSavingsPlansUtilizationDetails ::
      ![SavingsPlansUtilizationDetail],
    _gspudrsTimePeriod ::
      !DateInterval
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'GetSavingsPlansUtilizationDetailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gspudrsNextToken' - The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gspudrsTotal' - The total Savings Plans utilization, regardless of time period.
--
-- * 'gspudrsResponseStatus' - -- | The response status code.
--
-- * 'gspudrsSavingsPlansUtilizationDetails' - Retrieves a single daily or monthly Savings Plans utilization rate and details for your account.
--
-- * 'gspudrsTimePeriod' - Undocumented member.
getSavingsPlansUtilizationDetailsResponse ::
  -- | 'gspudrsResponseStatus'
  Int ->
  -- | 'gspudrsTimePeriod'
  DateInterval ->
  GetSavingsPlansUtilizationDetailsResponse
getSavingsPlansUtilizationDetailsResponse
  pResponseStatus_
  pTimePeriod_ =
    GetSavingsPlansUtilizationDetailsResponse'
      { _gspudrsNextToken =
          Nothing,
        _gspudrsTotal = Nothing,
        _gspudrsResponseStatus = pResponseStatus_,
        _gspudrsSavingsPlansUtilizationDetails = mempty,
        _gspudrsTimePeriod = pTimePeriod_
      }

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
gspudrsNextToken :: Lens' GetSavingsPlansUtilizationDetailsResponse (Maybe Text)
gspudrsNextToken = lens _gspudrsNextToken (\s a -> s {_gspudrsNextToken = a})

-- | The total Savings Plans utilization, regardless of time period.
gspudrsTotal :: Lens' GetSavingsPlansUtilizationDetailsResponse (Maybe SavingsPlansUtilizationAggregates)
gspudrsTotal = lens _gspudrsTotal (\s a -> s {_gspudrsTotal = a})

-- | -- | The response status code.
gspudrsResponseStatus :: Lens' GetSavingsPlansUtilizationDetailsResponse Int
gspudrsResponseStatus = lens _gspudrsResponseStatus (\s a -> s {_gspudrsResponseStatus = a})

-- | Retrieves a single daily or monthly Savings Plans utilization rate and details for your account.
gspudrsSavingsPlansUtilizationDetails :: Lens' GetSavingsPlansUtilizationDetailsResponse [SavingsPlansUtilizationDetail]
gspudrsSavingsPlansUtilizationDetails = lens _gspudrsSavingsPlansUtilizationDetails (\s a -> s {_gspudrsSavingsPlansUtilizationDetails = a}) . _Coerce

-- | Undocumented member.
gspudrsTimePeriod :: Lens' GetSavingsPlansUtilizationDetailsResponse DateInterval
gspudrsTimePeriod = lens _gspudrsTimePeriod (\s a -> s {_gspudrsTimePeriod = a})

instance NFData GetSavingsPlansUtilizationDetailsResponse
