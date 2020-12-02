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
-- Module      : Network.AWS.CostExplorer.GetSavingsPlansUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the Savings Plans utilization for your account across date ranges with daily or monthly granularity. Management account in an organization have access to member accounts. You can use @GetDimensionValues@ in @SAVINGS_PLANS@ to determine the possible dimension values.
module Network.AWS.CostExplorer.GetSavingsPlansUtilization
  ( -- * Creating a Request
    getSavingsPlansUtilization,
    GetSavingsPlansUtilization,

    -- * Request Lenses
    gspuGranularity,
    gspuFilter,
    gspuTimePeriod,

    -- * Destructuring the Response
    getSavingsPlansUtilizationResponse,
    GetSavingsPlansUtilizationResponse,

    -- * Response Lenses
    gspursSavingsPlansUtilizationsByTime,
    gspursResponseStatus,
    gspursTotal,
  )
where

import Network.AWS.CostExplorer.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSavingsPlansUtilization' smart constructor.
data GetSavingsPlansUtilization = GetSavingsPlansUtilization'
  { _gspuGranularity ::
      !(Maybe Granularity),
    _gspuFilter :: !(Maybe Expression),
    _gspuTimePeriod :: !DateInterval
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSavingsPlansUtilization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gspuGranularity' - The granularity of the Amazon Web Services utillization data for your Savings Plans. The @GetSavingsPlansUtilization@ operation supports only @DAILY@ and @MONTHLY@ granularities.
--
-- * 'gspuFilter' - Filters Savings Plans utilization coverage data for active Savings Plans dimensions. You can filter data with the following dimensions:     * @LINKED_ACCOUNT@      * @SAVINGS_PLAN_ARN@      * @SAVINGS_PLANS_TYPE@      * @REGION@      * @PAYMENT_OPTION@      * @INSTANCE_TYPE_FAMILY@  @GetSavingsPlansUtilization@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension.
--
-- * 'gspuTimePeriod' - The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
getSavingsPlansUtilization ::
  -- | 'gspuTimePeriod'
  DateInterval ->
  GetSavingsPlansUtilization
getSavingsPlansUtilization pTimePeriod_ =
  GetSavingsPlansUtilization'
    { _gspuGranularity = Nothing,
      _gspuFilter = Nothing,
      _gspuTimePeriod = pTimePeriod_
    }

-- | The granularity of the Amazon Web Services utillization data for your Savings Plans. The @GetSavingsPlansUtilization@ operation supports only @DAILY@ and @MONTHLY@ granularities.
gspuGranularity :: Lens' GetSavingsPlansUtilization (Maybe Granularity)
gspuGranularity = lens _gspuGranularity (\s a -> s {_gspuGranularity = a})

-- | Filters Savings Plans utilization coverage data for active Savings Plans dimensions. You can filter data with the following dimensions:     * @LINKED_ACCOUNT@      * @SAVINGS_PLAN_ARN@      * @SAVINGS_PLANS_TYPE@      * @REGION@      * @PAYMENT_OPTION@      * @INSTANCE_TYPE_FAMILY@  @GetSavingsPlansUtilization@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension.
gspuFilter :: Lens' GetSavingsPlansUtilization (Maybe Expression)
gspuFilter = lens _gspuFilter (\s a -> s {_gspuFilter = a})

-- | The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
gspuTimePeriod :: Lens' GetSavingsPlansUtilization DateInterval
gspuTimePeriod = lens _gspuTimePeriod (\s a -> s {_gspuTimePeriod = a})

instance AWSRequest GetSavingsPlansUtilization where
  type
    Rs GetSavingsPlansUtilization =
      GetSavingsPlansUtilizationResponse
  request = postJSON costExplorer
  response =
    receiveJSON
      ( \s h x ->
          GetSavingsPlansUtilizationResponse'
            <$> (x .?> "SavingsPlansUtilizationsByTime" .!@ mempty)
            <*> (pure (fromEnum s))
            <*> (x .:> "Total")
      )

instance Hashable GetSavingsPlansUtilization

instance NFData GetSavingsPlansUtilization

instance ToHeaders GetSavingsPlansUtilization where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSInsightsIndexService.GetSavingsPlansUtilization" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetSavingsPlansUtilization where
  toJSON GetSavingsPlansUtilization' {..} =
    object
      ( catMaybes
          [ ("Granularity" .=) <$> _gspuGranularity,
            ("Filter" .=) <$> _gspuFilter,
            Just ("TimePeriod" .= _gspuTimePeriod)
          ]
      )

instance ToPath GetSavingsPlansUtilization where
  toPath = const "/"

instance ToQuery GetSavingsPlansUtilization where
  toQuery = const mempty

-- | /See:/ 'getSavingsPlansUtilizationResponse' smart constructor.
data GetSavingsPlansUtilizationResponse = GetSavingsPlansUtilizationResponse'
  { _gspursSavingsPlansUtilizationsByTime ::
      !( Maybe
           [SavingsPlansUtilizationByTime]
       ),
    _gspursResponseStatus ::
      !Int,
    _gspursTotal ::
      !SavingsPlansUtilizationAggregates
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSavingsPlansUtilizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gspursSavingsPlansUtilizationsByTime' - The amount of cost/commitment you used your Savings Plans. This allows you to specify date ranges.
--
-- * 'gspursResponseStatus' - -- | The response status code.
--
-- * 'gspursTotal' - The total amount of cost/commitment that you used your Savings Plans, regardless of date ranges.
getSavingsPlansUtilizationResponse ::
  -- | 'gspursResponseStatus'
  Int ->
  -- | 'gspursTotal'
  SavingsPlansUtilizationAggregates ->
  GetSavingsPlansUtilizationResponse
getSavingsPlansUtilizationResponse pResponseStatus_ pTotal_ =
  GetSavingsPlansUtilizationResponse'
    { _gspursSavingsPlansUtilizationsByTime =
        Nothing,
      _gspursResponseStatus = pResponseStatus_,
      _gspursTotal = pTotal_
    }

-- | The amount of cost/commitment you used your Savings Plans. This allows you to specify date ranges.
gspursSavingsPlansUtilizationsByTime :: Lens' GetSavingsPlansUtilizationResponse [SavingsPlansUtilizationByTime]
gspursSavingsPlansUtilizationsByTime = lens _gspursSavingsPlansUtilizationsByTime (\s a -> s {_gspursSavingsPlansUtilizationsByTime = a}) . _Default . _Coerce

-- | -- | The response status code.
gspursResponseStatus :: Lens' GetSavingsPlansUtilizationResponse Int
gspursResponseStatus = lens _gspursResponseStatus (\s a -> s {_gspursResponseStatus = a})

-- | The total amount of cost/commitment that you used your Savings Plans, regardless of date ranges.
gspursTotal :: Lens' GetSavingsPlansUtilizationResponse SavingsPlansUtilizationAggregates
gspursTotal = lens _gspursTotal (\s a -> s {_gspursTotal = a})

instance NFData GetSavingsPlansUtilizationResponse
