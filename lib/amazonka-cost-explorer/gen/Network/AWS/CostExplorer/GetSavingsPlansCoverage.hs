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
-- Module      : Network.AWS.CostExplorer.GetSavingsPlansCoverage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the Savings Plans covered for your account. This enables you to see how much of your cost is covered by a Savings Plan. An organization’s management account can see the coverage of the associated member accounts. This supports dimensions, Cost Categories, and nested expressions. For any time period, you can filter data for Savings Plans usage with the following dimensions:
--
--
--     * @LINKED_ACCOUNT@
--
--     * @REGION@
--
--     * @SERVICE@
--
--     * @INSTANCE_FAMILY@
--
--
--
-- To determine valid values for a dimension, use the @GetDimensionValues@ operation.
module Network.AWS.CostExplorer.GetSavingsPlansCoverage
  ( -- * Creating a Request
    getSavingsPlansCoverage,
    GetSavingsPlansCoverage,

    -- * Request Lenses
    gspcGroupBy,
    gspcMetrics,
    gspcGranularity,
    gspcNextToken,
    gspcFilter,
    gspcMaxResults,
    gspcTimePeriod,

    -- * Destructuring the Response
    getSavingsPlansCoverageResponse,
    GetSavingsPlansCoverageResponse,

    -- * Response Lenses
    gspcrsNextToken,
    gspcrsResponseStatus,
    gspcrsSavingsPlansCoverages,
  )
where

import Network.AWS.CostExplorer.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSavingsPlansCoverage' smart constructor.
data GetSavingsPlansCoverage = GetSavingsPlansCoverage'
  { _gspcGroupBy ::
      !(Maybe [GroupDefinition]),
    _gspcMetrics :: !(Maybe [Text]),
    _gspcGranularity :: !(Maybe Granularity),
    _gspcNextToken :: !(Maybe Text),
    _gspcFilter :: !(Maybe Expression),
    _gspcMaxResults :: !(Maybe Nat),
    _gspcTimePeriod :: !DateInterval
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSavingsPlansCoverage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gspcGroupBy' - You can group the data using the attributes @INSTANCE_FAMILY@ , @REGION@ , or @SERVICE@ .
--
-- * 'gspcMetrics' - The measurement that you want your Savings Plans coverage reported in. The only valid value is @SpendCoveredBySavingsPlans@ .
--
-- * 'gspcGranularity' - The granularity of the Amazon Web Services cost data for your Savings Plans. @Granularity@ can't be set if @GroupBy@ is set. The @GetSavingsPlansCoverage@ operation supports only @DAILY@ and @MONTHLY@ granularities.
--
-- * 'gspcNextToken' - The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gspcFilter' - Filters Savings Plans coverage data by dimensions. You can filter data for Savings Plans usage with the following dimensions:     * @LINKED_ACCOUNT@      * @REGION@      * @SERVICE@      * @INSTANCE_FAMILY@  @GetSavingsPlansCoverage@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension. If there are multiple values for a dimension, they are OR'd together. Cost category is also supported.
--
-- * 'gspcMaxResults' - The number of items to be returned in a response. The default is @20@ , with a minimum value of @1@ .
--
-- * 'gspcTimePeriod' - The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
getSavingsPlansCoverage ::
  -- | 'gspcTimePeriod'
  DateInterval ->
  GetSavingsPlansCoverage
getSavingsPlansCoverage pTimePeriod_ =
  GetSavingsPlansCoverage'
    { _gspcGroupBy = Nothing,
      _gspcMetrics = Nothing,
      _gspcGranularity = Nothing,
      _gspcNextToken = Nothing,
      _gspcFilter = Nothing,
      _gspcMaxResults = Nothing,
      _gspcTimePeriod = pTimePeriod_
    }

-- | You can group the data using the attributes @INSTANCE_FAMILY@ , @REGION@ , or @SERVICE@ .
gspcGroupBy :: Lens' GetSavingsPlansCoverage [GroupDefinition]
gspcGroupBy = lens _gspcGroupBy (\s a -> s {_gspcGroupBy = a}) . _Default . _Coerce

-- | The measurement that you want your Savings Plans coverage reported in. The only valid value is @SpendCoveredBySavingsPlans@ .
gspcMetrics :: Lens' GetSavingsPlansCoverage [Text]
gspcMetrics = lens _gspcMetrics (\s a -> s {_gspcMetrics = a}) . _Default . _Coerce

-- | The granularity of the Amazon Web Services cost data for your Savings Plans. @Granularity@ can't be set if @GroupBy@ is set. The @GetSavingsPlansCoverage@ operation supports only @DAILY@ and @MONTHLY@ granularities.
gspcGranularity :: Lens' GetSavingsPlansCoverage (Maybe Granularity)
gspcGranularity = lens _gspcGranularity (\s a -> s {_gspcGranularity = a})

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
gspcNextToken :: Lens' GetSavingsPlansCoverage (Maybe Text)
gspcNextToken = lens _gspcNextToken (\s a -> s {_gspcNextToken = a})

-- | Filters Savings Plans coverage data by dimensions. You can filter data for Savings Plans usage with the following dimensions:     * @LINKED_ACCOUNT@      * @REGION@      * @SERVICE@      * @INSTANCE_FAMILY@  @GetSavingsPlansCoverage@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension. If there are multiple values for a dimension, they are OR'd together. Cost category is also supported.
gspcFilter :: Lens' GetSavingsPlansCoverage (Maybe Expression)
gspcFilter = lens _gspcFilter (\s a -> s {_gspcFilter = a})

-- | The number of items to be returned in a response. The default is @20@ , with a minimum value of @1@ .
gspcMaxResults :: Lens' GetSavingsPlansCoverage (Maybe Natural)
gspcMaxResults = lens _gspcMaxResults (\s a -> s {_gspcMaxResults = a}) . mapping _Nat

-- | The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
gspcTimePeriod :: Lens' GetSavingsPlansCoverage DateInterval
gspcTimePeriod = lens _gspcTimePeriod (\s a -> s {_gspcTimePeriod = a})

instance AWSRequest GetSavingsPlansCoverage where
  type Rs GetSavingsPlansCoverage = GetSavingsPlansCoverageResponse
  request = postJSON costExplorer
  response =
    receiveJSON
      ( \s h x ->
          GetSavingsPlansCoverageResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "SavingsPlansCoverages" .!@ mempty)
      )

instance Hashable GetSavingsPlansCoverage

instance NFData GetSavingsPlansCoverage

instance ToHeaders GetSavingsPlansCoverage where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSInsightsIndexService.GetSavingsPlansCoverage" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetSavingsPlansCoverage where
  toJSON GetSavingsPlansCoverage' {..} =
    object
      ( catMaybes
          [ ("GroupBy" .=) <$> _gspcGroupBy,
            ("Metrics" .=) <$> _gspcMetrics,
            ("Granularity" .=) <$> _gspcGranularity,
            ("NextToken" .=) <$> _gspcNextToken,
            ("Filter" .=) <$> _gspcFilter,
            ("MaxResults" .=) <$> _gspcMaxResults,
            Just ("TimePeriod" .= _gspcTimePeriod)
          ]
      )

instance ToPath GetSavingsPlansCoverage where
  toPath = const "/"

instance ToQuery GetSavingsPlansCoverage where
  toQuery = const mempty

-- | /See:/ 'getSavingsPlansCoverageResponse' smart constructor.
data GetSavingsPlansCoverageResponse = GetSavingsPlansCoverageResponse'
  { _gspcrsNextToken ::
      !(Maybe Text),
    _gspcrsResponseStatus ::
      !Int,
    _gspcrsSavingsPlansCoverages ::
      ![SavingsPlansCoverage]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSavingsPlansCoverageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gspcrsNextToken' - The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gspcrsResponseStatus' - -- | The response status code.
--
-- * 'gspcrsSavingsPlansCoverages' - The amount of spend that your Savings Plans covered.
getSavingsPlansCoverageResponse ::
  -- | 'gspcrsResponseStatus'
  Int ->
  GetSavingsPlansCoverageResponse
getSavingsPlansCoverageResponse pResponseStatus_ =
  GetSavingsPlansCoverageResponse'
    { _gspcrsNextToken = Nothing,
      _gspcrsResponseStatus = pResponseStatus_,
      _gspcrsSavingsPlansCoverages = mempty
    }

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
gspcrsNextToken :: Lens' GetSavingsPlansCoverageResponse (Maybe Text)
gspcrsNextToken = lens _gspcrsNextToken (\s a -> s {_gspcrsNextToken = a})

-- | -- | The response status code.
gspcrsResponseStatus :: Lens' GetSavingsPlansCoverageResponse Int
gspcrsResponseStatus = lens _gspcrsResponseStatus (\s a -> s {_gspcrsResponseStatus = a})

-- | The amount of spend that your Savings Plans covered.
gspcrsSavingsPlansCoverages :: Lens' GetSavingsPlansCoverageResponse [SavingsPlansCoverage]
gspcrsSavingsPlansCoverages = lens _gspcrsSavingsPlansCoverages (\s a -> s {_gspcrsSavingsPlansCoverages = a}) . _Coerce

instance NFData GetSavingsPlansCoverageResponse
