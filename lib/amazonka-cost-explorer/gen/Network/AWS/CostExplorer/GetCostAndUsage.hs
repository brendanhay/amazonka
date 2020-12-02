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
-- Module      : Network.AWS.CostExplorer.GetCostAndUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves cost and usage metrics for your account. You can specify which cost and usage-related metric, such as @BlendedCosts@ or @UsageQuantity@ , that you want the request to return. You can also filter and group your data by various dimensions, such as @SERVICE@ or @AZ@ , in a specific time range. For a complete list of valid dimensions, see the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_GetDimensionValues.html GetDimensionValues> operation. Management account in an organization in AWS Organizations have access to all member accounts.
--
--
-- For information about filter limitations, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/billing-limits.html Quotas and restrictions> in the /Billing and Cost Management User Guide/ .
module Network.AWS.CostExplorer.GetCostAndUsage
  ( -- * Creating a Request
    getCostAndUsage,
    GetCostAndUsage,

    -- * Request Lenses
    gcauGroupBy,
    gcauNextPageToken,
    gcauGranularity,
    gcauFilter,
    gcauTimePeriod,
    gcauMetrics,

    -- * Destructuring the Response
    getCostAndUsageResponse,
    GetCostAndUsageResponse,

    -- * Response Lenses
    gcaursResultsByTime,
    gcaursNextPageToken,
    gcaursGroupDefinitions,
    gcaursResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCostAndUsage' smart constructor.
data GetCostAndUsage = GetCostAndUsage'
  { _gcauGroupBy ::
      !(Maybe [GroupDefinition]),
    _gcauNextPageToken :: !(Maybe Text),
    _gcauGranularity :: !(Maybe Granularity),
    _gcauFilter :: !(Maybe Expression),
    _gcauTimePeriod :: !DateInterval,
    _gcauMetrics :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCostAndUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcauGroupBy' - You can group AWS costs using up to two different groups, either dimensions, tag keys, cost categories, or any two group by types. When you group by tag key, you get all tag values, including empty strings. Valid values are @AZ@ , @INSTANCE_TYPE@ , @LEGAL_ENTITY_NAME@ , @LINKED_ACCOUNT@ , @OPERATION@ , @PLATFORM@ , @PURCHASE_TYPE@ , @SERVICE@ , @TAGS@ , @TENANCY@ , @RECORD_TYPE@ , and @USAGE_TYPE@ .
--
-- * 'gcauNextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gcauGranularity' - Sets the AWS cost granularity to @MONTHLY@ or @DAILY@ , or @HOURLY@ . If @Granularity@ isn't set, the response object doesn't include the @Granularity@ , either @MONTHLY@ or @DAILY@ , or @HOURLY@ .
--
-- * 'gcauFilter' - Filters AWS costs by different dimensions. For example, you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that are associated with that account's usage of that service. You can nest @Expression@ objects to define any combination of dimension filters. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> .
--
-- * 'gcauTimePeriod' - Sets the start and end dates for retrieving AWS costs. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
--
-- * 'gcauMetrics' - Which metrics are returned in the query. For more information about blended and unblended rates, see <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> .  Valid values are @AmortizedCost@ , @BlendedCost@ , @NetAmortizedCost@ , @NetUnblendedCost@ , @NormalizedUsageAmount@ , @UnblendedCost@ , and @UsageQuantity@ .  @Metrics@ is required for @GetCostAndUsage@ requests.
getCostAndUsage ::
  -- | 'gcauTimePeriod'
  DateInterval ->
  GetCostAndUsage
getCostAndUsage pTimePeriod_ =
  GetCostAndUsage'
    { _gcauGroupBy = Nothing,
      _gcauNextPageToken = Nothing,
      _gcauGranularity = Nothing,
      _gcauFilter = Nothing,
      _gcauTimePeriod = pTimePeriod_,
      _gcauMetrics = mempty
    }

-- | You can group AWS costs using up to two different groups, either dimensions, tag keys, cost categories, or any two group by types. When you group by tag key, you get all tag values, including empty strings. Valid values are @AZ@ , @INSTANCE_TYPE@ , @LEGAL_ENTITY_NAME@ , @LINKED_ACCOUNT@ , @OPERATION@ , @PLATFORM@ , @PURCHASE_TYPE@ , @SERVICE@ , @TAGS@ , @TENANCY@ , @RECORD_TYPE@ , and @USAGE_TYPE@ .
gcauGroupBy :: Lens' GetCostAndUsage [GroupDefinition]
gcauGroupBy = lens _gcauGroupBy (\s a -> s {_gcauGroupBy = a}) . _Default . _Coerce

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
gcauNextPageToken :: Lens' GetCostAndUsage (Maybe Text)
gcauNextPageToken = lens _gcauNextPageToken (\s a -> s {_gcauNextPageToken = a})

-- | Sets the AWS cost granularity to @MONTHLY@ or @DAILY@ , or @HOURLY@ . If @Granularity@ isn't set, the response object doesn't include the @Granularity@ , either @MONTHLY@ or @DAILY@ , or @HOURLY@ .
gcauGranularity :: Lens' GetCostAndUsage (Maybe Granularity)
gcauGranularity = lens _gcauGranularity (\s a -> s {_gcauGranularity = a})

-- | Filters AWS costs by different dimensions. For example, you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that are associated with that account's usage of that service. You can nest @Expression@ objects to define any combination of dimension filters. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> .
gcauFilter :: Lens' GetCostAndUsage (Maybe Expression)
gcauFilter = lens _gcauFilter (\s a -> s {_gcauFilter = a})

-- | Sets the start and end dates for retrieving AWS costs. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
gcauTimePeriod :: Lens' GetCostAndUsage DateInterval
gcauTimePeriod = lens _gcauTimePeriod (\s a -> s {_gcauTimePeriod = a})

-- | Which metrics are returned in the query. For more information about blended and unblended rates, see <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> .  Valid values are @AmortizedCost@ , @BlendedCost@ , @NetAmortizedCost@ , @NetUnblendedCost@ , @NormalizedUsageAmount@ , @UnblendedCost@ , and @UsageQuantity@ .  @Metrics@ is required for @GetCostAndUsage@ requests.
gcauMetrics :: Lens' GetCostAndUsage [Text]
gcauMetrics = lens _gcauMetrics (\s a -> s {_gcauMetrics = a}) . _Coerce

instance AWSRequest GetCostAndUsage where
  type Rs GetCostAndUsage = GetCostAndUsageResponse
  request = postJSON costExplorer
  response =
    receiveJSON
      ( \s h x ->
          GetCostAndUsageResponse'
            <$> (x .?> "ResultsByTime" .!@ mempty)
            <*> (x .?> "NextPageToken")
            <*> (x .?> "GroupDefinitions" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetCostAndUsage

instance NFData GetCostAndUsage

instance ToHeaders GetCostAndUsage where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSInsightsIndexService.GetCostAndUsage" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetCostAndUsage where
  toJSON GetCostAndUsage' {..} =
    object
      ( catMaybes
          [ ("GroupBy" .=) <$> _gcauGroupBy,
            ("NextPageToken" .=) <$> _gcauNextPageToken,
            ("Granularity" .=) <$> _gcauGranularity,
            ("Filter" .=) <$> _gcauFilter,
            Just ("TimePeriod" .= _gcauTimePeriod),
            Just ("Metrics" .= _gcauMetrics)
          ]
      )

instance ToPath GetCostAndUsage where
  toPath = const "/"

instance ToQuery GetCostAndUsage where
  toQuery = const mempty

-- | /See:/ 'getCostAndUsageResponse' smart constructor.
data GetCostAndUsageResponse = GetCostAndUsageResponse'
  { _gcaursResultsByTime ::
      !(Maybe [ResultByTime]),
    _gcaursNextPageToken :: !(Maybe Text),
    _gcaursGroupDefinitions ::
      !(Maybe [GroupDefinition]),
    _gcaursResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCostAndUsageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcaursResultsByTime' - The time period that is covered by the results in the response.
--
-- * 'gcaursNextPageToken' - The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gcaursGroupDefinitions' - The groups that are specified by the @Filter@ or @GroupBy@ parameters in the request.
--
-- * 'gcaursResponseStatus' - -- | The response status code.
getCostAndUsageResponse ::
  -- | 'gcaursResponseStatus'
  Int ->
  GetCostAndUsageResponse
getCostAndUsageResponse pResponseStatus_ =
  GetCostAndUsageResponse'
    { _gcaursResultsByTime = Nothing,
      _gcaursNextPageToken = Nothing,
      _gcaursGroupDefinitions = Nothing,
      _gcaursResponseStatus = pResponseStatus_
    }

-- | The time period that is covered by the results in the response.
gcaursResultsByTime :: Lens' GetCostAndUsageResponse [ResultByTime]
gcaursResultsByTime = lens _gcaursResultsByTime (\s a -> s {_gcaursResultsByTime = a}) . _Default . _Coerce

-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
gcaursNextPageToken :: Lens' GetCostAndUsageResponse (Maybe Text)
gcaursNextPageToken = lens _gcaursNextPageToken (\s a -> s {_gcaursNextPageToken = a})

-- | The groups that are specified by the @Filter@ or @GroupBy@ parameters in the request.
gcaursGroupDefinitions :: Lens' GetCostAndUsageResponse [GroupDefinition]
gcaursGroupDefinitions = lens _gcaursGroupDefinitions (\s a -> s {_gcaursGroupDefinitions = a}) . _Default . _Coerce

-- | -- | The response status code.
gcaursResponseStatus :: Lens' GetCostAndUsageResponse Int
gcaursResponseStatus = lens _gcaursResponseStatus (\s a -> s {_gcaursResponseStatus = a})

instance NFData GetCostAndUsageResponse
