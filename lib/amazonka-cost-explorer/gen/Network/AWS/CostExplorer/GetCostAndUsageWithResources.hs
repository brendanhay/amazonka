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
-- Module      : Network.AWS.CostExplorer.GetCostAndUsageWithResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves cost and usage metrics with resources for your account. You can specify which cost and usage-related metric, such as @BlendedCosts@ or @UsageQuantity@ , that you want the request to return. You can also filter and group your data by various dimensions, such as @SERVICE@ or @AZ@ , in a specific time range. For a complete list of valid dimensions, see the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_GetDimensionValues.html GetDimensionValues> operation. Management account in an organization in AWS Organizations have access to all member accounts. This API is currently available for the Amazon Elastic Compute Cloud – Compute service only.
module Network.AWS.CostExplorer.GetCostAndUsageWithResources
  ( -- * Creating a Request
    getCostAndUsageWithResources,
    GetCostAndUsageWithResources,

    -- * Request Lenses
    gcauwrGroupBy,
    gcauwrNextPageToken,
    gcauwrMetrics,
    gcauwrGranularity,
    gcauwrTimePeriod,
    gcauwrFilter,

    -- * Destructuring the Response
    getCostAndUsageWithResourcesResponse,
    GetCostAndUsageWithResourcesResponse,

    -- * Response Lenses
    gcauwrrsResultsByTime,
    gcauwrrsNextPageToken,
    gcauwrrsGroupDefinitions,
    gcauwrrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCostAndUsageWithResources' smart constructor.
data GetCostAndUsageWithResources = GetCostAndUsageWithResources'
  { _gcauwrGroupBy ::
      !(Maybe [GroupDefinition]),
    _gcauwrNextPageToken ::
      !(Maybe Text),
    _gcauwrMetrics :: !(Maybe [Text]),
    _gcauwrGranularity ::
      !(Maybe Granularity),
    _gcauwrTimePeriod ::
      !DateInterval,
    _gcauwrFilter :: !Expression
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCostAndUsageWithResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcauwrGroupBy' - You can group Amazon Web Services costs using up to two different groups: @DIMENSION@ , @TAG@ , @COST_CATEGORY@ .
--
-- * 'gcauwrNextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gcauwrMetrics' - Which metrics are returned in the query. For more information about blended and unblended rates, see <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> .  Valid values are @AmortizedCost@ , @BlendedCost@ , @NetAmortizedCost@ , @NetUnblendedCost@ , @NormalizedUsageAmount@ , @UnblendedCost@ , and @UsageQuantity@ .  @Metrics@ is required for @GetCostAndUsageWithResources@ requests.
--
-- * 'gcauwrGranularity' - Sets the AWS cost granularity to @MONTHLY@ , @DAILY@ , or @HOURLY@ . If @Granularity@ isn't set, the response object doesn't include the @Granularity@ , @MONTHLY@ , @DAILY@ , or @HOURLY@ .
--
-- * 'gcauwrTimePeriod' - Sets the start and end dates for retrieving Amazon Web Services costs. The range must be within the last 14 days (the start date cannot be earlier than 14 days ago). The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
--
-- * 'gcauwrFilter' - Filters Amazon Web Services costs by different dimensions. For example, you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that are associated with that account's usage of that service. You can nest @Expression@ objects to define any combination of dimension filters. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> .  The @GetCostAndUsageWithResources@ operation requires that you either group by or filter by a @ResourceId@ . It requires the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> @"SERVICE = Amazon Elastic Compute Cloud - Compute"@ in the filter.
getCostAndUsageWithResources ::
  -- | 'gcauwrTimePeriod'
  DateInterval ->
  -- | 'gcauwrFilter'
  Expression ->
  GetCostAndUsageWithResources
getCostAndUsageWithResources pTimePeriod_ pFilter_ =
  GetCostAndUsageWithResources'
    { _gcauwrGroupBy = Nothing,
      _gcauwrNextPageToken = Nothing,
      _gcauwrMetrics = Nothing,
      _gcauwrGranularity = Nothing,
      _gcauwrTimePeriod = pTimePeriod_,
      _gcauwrFilter = pFilter_
    }

-- | You can group Amazon Web Services costs using up to two different groups: @DIMENSION@ , @TAG@ , @COST_CATEGORY@ .
gcauwrGroupBy :: Lens' GetCostAndUsageWithResources [GroupDefinition]
gcauwrGroupBy = lens _gcauwrGroupBy (\s a -> s {_gcauwrGroupBy = a}) . _Default . _Coerce

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
gcauwrNextPageToken :: Lens' GetCostAndUsageWithResources (Maybe Text)
gcauwrNextPageToken = lens _gcauwrNextPageToken (\s a -> s {_gcauwrNextPageToken = a})

-- | Which metrics are returned in the query. For more information about blended and unblended rates, see <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> .  Valid values are @AmortizedCost@ , @BlendedCost@ , @NetAmortizedCost@ , @NetUnblendedCost@ , @NormalizedUsageAmount@ , @UnblendedCost@ , and @UsageQuantity@ .  @Metrics@ is required for @GetCostAndUsageWithResources@ requests.
gcauwrMetrics :: Lens' GetCostAndUsageWithResources [Text]
gcauwrMetrics = lens _gcauwrMetrics (\s a -> s {_gcauwrMetrics = a}) . _Default . _Coerce

-- | Sets the AWS cost granularity to @MONTHLY@ , @DAILY@ , or @HOURLY@ . If @Granularity@ isn't set, the response object doesn't include the @Granularity@ , @MONTHLY@ , @DAILY@ , or @HOURLY@ .
gcauwrGranularity :: Lens' GetCostAndUsageWithResources (Maybe Granularity)
gcauwrGranularity = lens _gcauwrGranularity (\s a -> s {_gcauwrGranularity = a})

-- | Sets the start and end dates for retrieving Amazon Web Services costs. The range must be within the last 14 days (the start date cannot be earlier than 14 days ago). The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
gcauwrTimePeriod :: Lens' GetCostAndUsageWithResources DateInterval
gcauwrTimePeriod = lens _gcauwrTimePeriod (\s a -> s {_gcauwrTimePeriod = a})

-- | Filters Amazon Web Services costs by different dimensions. For example, you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that are associated with that account's usage of that service. You can nest @Expression@ objects to define any combination of dimension filters. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> .  The @GetCostAndUsageWithResources@ operation requires that you either group by or filter by a @ResourceId@ . It requires the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> @"SERVICE = Amazon Elastic Compute Cloud - Compute"@ in the filter.
gcauwrFilter :: Lens' GetCostAndUsageWithResources Expression
gcauwrFilter = lens _gcauwrFilter (\s a -> s {_gcauwrFilter = a})

instance AWSRequest GetCostAndUsageWithResources where
  type
    Rs GetCostAndUsageWithResources =
      GetCostAndUsageWithResourcesResponse
  request = postJSON costExplorer
  response =
    receiveJSON
      ( \s h x ->
          GetCostAndUsageWithResourcesResponse'
            <$> (x .?> "ResultsByTime" .!@ mempty)
            <*> (x .?> "NextPageToken")
            <*> (x .?> "GroupDefinitions" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetCostAndUsageWithResources

instance NFData GetCostAndUsageWithResources

instance ToHeaders GetCostAndUsageWithResources where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSInsightsIndexService.GetCostAndUsageWithResources" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetCostAndUsageWithResources where
  toJSON GetCostAndUsageWithResources' {..} =
    object
      ( catMaybes
          [ ("GroupBy" .=) <$> _gcauwrGroupBy,
            ("NextPageToken" .=) <$> _gcauwrNextPageToken,
            ("Metrics" .=) <$> _gcauwrMetrics,
            ("Granularity" .=) <$> _gcauwrGranularity,
            Just ("TimePeriod" .= _gcauwrTimePeriod),
            Just ("Filter" .= _gcauwrFilter)
          ]
      )

instance ToPath GetCostAndUsageWithResources where
  toPath = const "/"

instance ToQuery GetCostAndUsageWithResources where
  toQuery = const mempty

-- | /See:/ 'getCostAndUsageWithResourcesResponse' smart constructor.
data GetCostAndUsageWithResourcesResponse = GetCostAndUsageWithResourcesResponse'
  { _gcauwrrsResultsByTime ::
      !( Maybe
           [ResultByTime]
       ),
    _gcauwrrsNextPageToken ::
      !(Maybe Text),
    _gcauwrrsGroupDefinitions ::
      !( Maybe
           [GroupDefinition]
       ),
    _gcauwrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCostAndUsageWithResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcauwrrsResultsByTime' - The time period that is covered by the results in the response.
--
-- * 'gcauwrrsNextPageToken' - The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gcauwrrsGroupDefinitions' - The groups that are specified by the @Filter@ or @GroupBy@ parameters in the request.
--
-- * 'gcauwrrsResponseStatus' - -- | The response status code.
getCostAndUsageWithResourcesResponse ::
  -- | 'gcauwrrsResponseStatus'
  Int ->
  GetCostAndUsageWithResourcesResponse
getCostAndUsageWithResourcesResponse pResponseStatus_ =
  GetCostAndUsageWithResourcesResponse'
    { _gcauwrrsResultsByTime =
        Nothing,
      _gcauwrrsNextPageToken = Nothing,
      _gcauwrrsGroupDefinitions = Nothing,
      _gcauwrrsResponseStatus = pResponseStatus_
    }

-- | The time period that is covered by the results in the response.
gcauwrrsResultsByTime :: Lens' GetCostAndUsageWithResourcesResponse [ResultByTime]
gcauwrrsResultsByTime = lens _gcauwrrsResultsByTime (\s a -> s {_gcauwrrsResultsByTime = a}) . _Default . _Coerce

-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
gcauwrrsNextPageToken :: Lens' GetCostAndUsageWithResourcesResponse (Maybe Text)
gcauwrrsNextPageToken = lens _gcauwrrsNextPageToken (\s a -> s {_gcauwrrsNextPageToken = a})

-- | The groups that are specified by the @Filter@ or @GroupBy@ parameters in the request.
gcauwrrsGroupDefinitions :: Lens' GetCostAndUsageWithResourcesResponse [GroupDefinition]
gcauwrrsGroupDefinitions = lens _gcauwrrsGroupDefinitions (\s a -> s {_gcauwrrsGroupDefinitions = a}) . _Default . _Coerce

-- | -- | The response status code.
gcauwrrsResponseStatus :: Lens' GetCostAndUsageWithResourcesResponse Int
gcauwrrsResponseStatus = lens _gcauwrrsResponseStatus (\s a -> s {_gcauwrrsResponseStatus = a})

instance NFData GetCostAndUsageWithResourcesResponse
