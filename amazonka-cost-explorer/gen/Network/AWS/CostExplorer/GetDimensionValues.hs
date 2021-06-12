{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetDimensionValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all available filter values for a specified filter over a
-- period of time. You can search the dimension values for an arbitrary
-- string.
module Network.AWS.CostExplorer.GetDimensionValues
  ( -- * Creating a Request
    GetDimensionValues (..),
    newGetDimensionValues,

    -- * Request Lenses
    getDimensionValues_maxResults,
    getDimensionValues_searchString,
    getDimensionValues_nextPageToken,
    getDimensionValues_context,
    getDimensionValues_sortBy,
    getDimensionValues_filter,
    getDimensionValues_timePeriod,
    getDimensionValues_dimension,

    -- * Destructuring the Response
    GetDimensionValuesResponse (..),
    newGetDimensionValuesResponse,

    -- * Response Lenses
    getDimensionValuesResponse_nextPageToken,
    getDimensionValuesResponse_httpStatus,
    getDimensionValuesResponse_dimensionValues,
    getDimensionValuesResponse_returnSize,
    getDimensionValuesResponse_totalSize,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDimensionValues' smart constructor.
data GetDimensionValues = GetDimensionValues'
  { -- | This field is only used when SortBy is provided in the request. The
    -- maximum number of objects that to be returned for this request. If
    -- MaxResults is not specified with SortBy, the request will return 1000
    -- results as the default value for this parameter.
    --
    -- For @GetDimensionValues@, MaxResults has an upper limit of 1000.
    maxResults :: Core.Maybe Core.Natural,
    -- | The value that you want to search the filter values for.
    searchString :: Core.Maybe Core.Text,
    -- | The token to retrieve the next set of results. AWS provides the token
    -- when the response from a previous call has more results than the maximum
    -- page size.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The context for the call to @GetDimensionValues@. This can be
    -- @RESERVATIONS@ or @COST_AND_USAGE@. The default value is
    -- @COST_AND_USAGE@. If the context is set to @RESERVATIONS@, the resulting
    -- dimension values can be used in the @GetReservationUtilization@
    -- operation. If the context is set to @COST_AND_USAGE@, the resulting
    -- dimension values can be used in the @GetCostAndUsage@ operation.
    --
    -- If you set the context to @COST_AND_USAGE@, you can use the following
    -- dimensions for searching:
    --
    -- -   AZ - The Availability Zone. An example is @us-east-1a@.
    --
    -- -   DATABASE_ENGINE - The Amazon Relational Database Service database.
    --     Examples are Aurora or MySQL.
    --
    -- -   INSTANCE_TYPE - The type of Amazon EC2 instance. An example is
    --     @m4.xlarge@.
    --
    -- -   LEGAL_ENTITY_NAME - The name of the organization that sells you AWS
    --     services, such as Amazon Web Services.
    --
    -- -   LINKED_ACCOUNT - The description in the attribute map that includes
    --     the full name of the member account. The value field contains the
    --     AWS ID of the member account.
    --
    -- -   OPERATING_SYSTEM - The operating system. Examples are Windows or
    --     Linux.
    --
    -- -   OPERATION - The action performed. Examples include @RunInstance@ and
    --     @CreateBucket@.
    --
    -- -   PLATFORM - The Amazon EC2 operating system. Examples are Windows or
    --     Linux.
    --
    -- -   PURCHASE_TYPE - The reservation type of the purchase to which this
    --     usage is related. Examples include On-Demand Instances and Standard
    --     Reserved Instances.
    --
    -- -   SERVICE - The AWS service such as Amazon DynamoDB.
    --
    -- -   USAGE_TYPE - The type of usage. An example is DataTransfer-In-Bytes.
    --     The response for the @GetDimensionValues@ operation includes a unit
    --     attribute. Examples include GB and Hrs.
    --
    -- -   USAGE_TYPE_GROUP - The grouping of common usage types. An example is
    --     Amazon EC2: CloudWatch – Alarms. The response for this operation
    --     includes a unit attribute.
    --
    -- -   REGION - The AWS Region.
    --
    -- -   RECORD_TYPE - The different types of charges such as RI fees, usage
    --     costs, tax refunds, and credits.
    --
    -- -   RESOURCE_ID - The unique identifier of the resource. ResourceId is
    --     an opt-in feature only available for last 14 days for EC2-Compute
    --     Service.
    --
    -- If you set the context to @RESERVATIONS@, you can use the following
    -- dimensions for searching:
    --
    -- -   AZ - The Availability Zone. An example is @us-east-1a@.
    --
    -- -   CACHE_ENGINE - The Amazon ElastiCache operating system. Examples are
    --     Windows or Linux.
    --
    -- -   DEPLOYMENT_OPTION - The scope of Amazon Relational Database Service
    --     deployments. Valid values are @SingleAZ@ and @MultiAZ@.
    --
    -- -   INSTANCE_TYPE - The type of Amazon EC2 instance. An example is
    --     @m4.xlarge@.
    --
    -- -   LINKED_ACCOUNT - The description in the attribute map that includes
    --     the full name of the member account. The value field contains the
    --     AWS ID of the member account.
    --
    -- -   PLATFORM - The Amazon EC2 operating system. Examples are Windows or
    --     Linux.
    --
    -- -   REGION - The AWS Region.
    --
    -- -   SCOPE (Utilization only) - The scope of a Reserved Instance (RI).
    --     Values are regional or a single Availability Zone.
    --
    -- -   TAG (Coverage only) - The tags that are associated with a Reserved
    --     Instance (RI).
    --
    -- -   TENANCY - The tenancy of a resource. Examples are shared or
    --     dedicated.
    --
    -- If you set the context to @SAVINGS_PLANS@, you can use the following
    -- dimensions for searching:
    --
    -- -   SAVINGS_PLANS_TYPE - Type of Savings Plans (EC2 Instance or Compute)
    --
    -- -   PAYMENT_OPTION - Payment option for the given Savings Plans (for
    --     example, All Upfront)
    --
    -- -   REGION - The AWS Region.
    --
    -- -   INSTANCE_TYPE_FAMILY - The family of instances (For example, @m5@)
    --
    -- -   LINKED_ACCOUNT - The description in the attribute map that includes
    --     the full name of the member account. The value field contains the
    --     AWS ID of the member account.
    --
    -- -   SAVINGS_PLAN_ARN - The unique identifier for your Savings Plan
    context :: Core.Maybe Context,
    -- | The value by which you want to sort the data.
    --
    -- The key represents cost and usage metrics. The following values are
    -- supported:
    --
    -- -   @BlendedCost@
    --
    -- -   @UnblendedCost@
    --
    -- -   @AmortizedCost@
    --
    -- -   @NetAmortizedCost@
    --
    -- -   @NetUnblendedCost@
    --
    -- -   @UsageQuantity@
    --
    -- -   @NormalizedUsageAmount@
    --
    -- Supported values for @SortOrder@ are @ASCENDING@ or @DESCENDING@.
    --
    -- When you specify a @SortBy@ paramater, the context must be
    -- @COST_AND_USAGE@. Further, when using @SortBy@, @NextPageToken@ and
    -- @SearchString@ are not supported.
    sortBy :: Core.Maybe [SortDefinition],
    filter' :: Core.Maybe Expression,
    -- | The start and end dates for retrieving the dimension values. The start
    -- date is inclusive, but the end date is exclusive. For example, if
    -- @start@ is @2017-01-01@ and @end@ is @2017-05-01@, then the cost and
    -- usage data is retrieved from @2017-01-01@ up to and including
    -- @2017-04-30@ but not including @2017-05-01@.
    timePeriod :: DateInterval,
    -- | The name of the dimension. Each @Dimension@ is available for a different
    -- @Context@. For more information, see @Context@.
    dimension :: Dimension
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDimensionValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getDimensionValues_maxResults' - This field is only used when SortBy is provided in the request. The
-- maximum number of objects that to be returned for this request. If
-- MaxResults is not specified with SortBy, the request will return 1000
-- results as the default value for this parameter.
--
-- For @GetDimensionValues@, MaxResults has an upper limit of 1000.
--
-- 'searchString', 'getDimensionValues_searchString' - The value that you want to search the filter values for.
--
-- 'nextPageToken', 'getDimensionValues_nextPageToken' - The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
--
-- 'context', 'getDimensionValues_context' - The context for the call to @GetDimensionValues@. This can be
-- @RESERVATIONS@ or @COST_AND_USAGE@. The default value is
-- @COST_AND_USAGE@. If the context is set to @RESERVATIONS@, the resulting
-- dimension values can be used in the @GetReservationUtilization@
-- operation. If the context is set to @COST_AND_USAGE@, the resulting
-- dimension values can be used in the @GetCostAndUsage@ operation.
--
-- If you set the context to @COST_AND_USAGE@, you can use the following
-- dimensions for searching:
--
-- -   AZ - The Availability Zone. An example is @us-east-1a@.
--
-- -   DATABASE_ENGINE - The Amazon Relational Database Service database.
--     Examples are Aurora or MySQL.
--
-- -   INSTANCE_TYPE - The type of Amazon EC2 instance. An example is
--     @m4.xlarge@.
--
-- -   LEGAL_ENTITY_NAME - The name of the organization that sells you AWS
--     services, such as Amazon Web Services.
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     AWS ID of the member account.
--
-- -   OPERATING_SYSTEM - The operating system. Examples are Windows or
--     Linux.
--
-- -   OPERATION - The action performed. Examples include @RunInstance@ and
--     @CreateBucket@.
--
-- -   PLATFORM - The Amazon EC2 operating system. Examples are Windows or
--     Linux.
--
-- -   PURCHASE_TYPE - The reservation type of the purchase to which this
--     usage is related. Examples include On-Demand Instances and Standard
--     Reserved Instances.
--
-- -   SERVICE - The AWS service such as Amazon DynamoDB.
--
-- -   USAGE_TYPE - The type of usage. An example is DataTransfer-In-Bytes.
--     The response for the @GetDimensionValues@ operation includes a unit
--     attribute. Examples include GB and Hrs.
--
-- -   USAGE_TYPE_GROUP - The grouping of common usage types. An example is
--     Amazon EC2: CloudWatch – Alarms. The response for this operation
--     includes a unit attribute.
--
-- -   REGION - The AWS Region.
--
-- -   RECORD_TYPE - The different types of charges such as RI fees, usage
--     costs, tax refunds, and credits.
--
-- -   RESOURCE_ID - The unique identifier of the resource. ResourceId is
--     an opt-in feature only available for last 14 days for EC2-Compute
--     Service.
--
-- If you set the context to @RESERVATIONS@, you can use the following
-- dimensions for searching:
--
-- -   AZ - The Availability Zone. An example is @us-east-1a@.
--
-- -   CACHE_ENGINE - The Amazon ElastiCache operating system. Examples are
--     Windows or Linux.
--
-- -   DEPLOYMENT_OPTION - The scope of Amazon Relational Database Service
--     deployments. Valid values are @SingleAZ@ and @MultiAZ@.
--
-- -   INSTANCE_TYPE - The type of Amazon EC2 instance. An example is
--     @m4.xlarge@.
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     AWS ID of the member account.
--
-- -   PLATFORM - The Amazon EC2 operating system. Examples are Windows or
--     Linux.
--
-- -   REGION - The AWS Region.
--
-- -   SCOPE (Utilization only) - The scope of a Reserved Instance (RI).
--     Values are regional or a single Availability Zone.
--
-- -   TAG (Coverage only) - The tags that are associated with a Reserved
--     Instance (RI).
--
-- -   TENANCY - The tenancy of a resource. Examples are shared or
--     dedicated.
--
-- If you set the context to @SAVINGS_PLANS@, you can use the following
-- dimensions for searching:
--
-- -   SAVINGS_PLANS_TYPE - Type of Savings Plans (EC2 Instance or Compute)
--
-- -   PAYMENT_OPTION - Payment option for the given Savings Plans (for
--     example, All Upfront)
--
-- -   REGION - The AWS Region.
--
-- -   INSTANCE_TYPE_FAMILY - The family of instances (For example, @m5@)
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     AWS ID of the member account.
--
-- -   SAVINGS_PLAN_ARN - The unique identifier for your Savings Plan
--
-- 'sortBy', 'getDimensionValues_sortBy' - The value by which you want to sort the data.
--
-- The key represents cost and usage metrics. The following values are
-- supported:
--
-- -   @BlendedCost@
--
-- -   @UnblendedCost@
--
-- -   @AmortizedCost@
--
-- -   @NetAmortizedCost@
--
-- -   @NetUnblendedCost@
--
-- -   @UsageQuantity@
--
-- -   @NormalizedUsageAmount@
--
-- Supported values for @SortOrder@ are @ASCENDING@ or @DESCENDING@.
--
-- When you specify a @SortBy@ paramater, the context must be
-- @COST_AND_USAGE@. Further, when using @SortBy@, @NextPageToken@ and
-- @SearchString@ are not supported.
--
-- 'filter'', 'getDimensionValues_filter' - Undocumented member.
--
-- 'timePeriod', 'getDimensionValues_timePeriod' - The start and end dates for retrieving the dimension values. The start
-- date is inclusive, but the end date is exclusive. For example, if
-- @start@ is @2017-01-01@ and @end@ is @2017-05-01@, then the cost and
-- usage data is retrieved from @2017-01-01@ up to and including
-- @2017-04-30@ but not including @2017-05-01@.
--
-- 'dimension', 'getDimensionValues_dimension' - The name of the dimension. Each @Dimension@ is available for a different
-- @Context@. For more information, see @Context@.
newGetDimensionValues ::
  -- | 'timePeriod'
  DateInterval ->
  -- | 'dimension'
  Dimension ->
  GetDimensionValues
newGetDimensionValues pTimePeriod_ pDimension_ =
  GetDimensionValues'
    { maxResults = Core.Nothing,
      searchString = Core.Nothing,
      nextPageToken = Core.Nothing,
      context = Core.Nothing,
      sortBy = Core.Nothing,
      filter' = Core.Nothing,
      timePeriod = pTimePeriod_,
      dimension = pDimension_
    }

-- | This field is only used when SortBy is provided in the request. The
-- maximum number of objects that to be returned for this request. If
-- MaxResults is not specified with SortBy, the request will return 1000
-- results as the default value for this parameter.
--
-- For @GetDimensionValues@, MaxResults has an upper limit of 1000.
getDimensionValues_maxResults :: Lens.Lens' GetDimensionValues (Core.Maybe Core.Natural)
getDimensionValues_maxResults = Lens.lens (\GetDimensionValues' {maxResults} -> maxResults) (\s@GetDimensionValues' {} a -> s {maxResults = a} :: GetDimensionValues)

-- | The value that you want to search the filter values for.
getDimensionValues_searchString :: Lens.Lens' GetDimensionValues (Core.Maybe Core.Text)
getDimensionValues_searchString = Lens.lens (\GetDimensionValues' {searchString} -> searchString) (\s@GetDimensionValues' {} a -> s {searchString = a} :: GetDimensionValues)

-- | The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
getDimensionValues_nextPageToken :: Lens.Lens' GetDimensionValues (Core.Maybe Core.Text)
getDimensionValues_nextPageToken = Lens.lens (\GetDimensionValues' {nextPageToken} -> nextPageToken) (\s@GetDimensionValues' {} a -> s {nextPageToken = a} :: GetDimensionValues)

-- | The context for the call to @GetDimensionValues@. This can be
-- @RESERVATIONS@ or @COST_AND_USAGE@. The default value is
-- @COST_AND_USAGE@. If the context is set to @RESERVATIONS@, the resulting
-- dimension values can be used in the @GetReservationUtilization@
-- operation. If the context is set to @COST_AND_USAGE@, the resulting
-- dimension values can be used in the @GetCostAndUsage@ operation.
--
-- If you set the context to @COST_AND_USAGE@, you can use the following
-- dimensions for searching:
--
-- -   AZ - The Availability Zone. An example is @us-east-1a@.
--
-- -   DATABASE_ENGINE - The Amazon Relational Database Service database.
--     Examples are Aurora or MySQL.
--
-- -   INSTANCE_TYPE - The type of Amazon EC2 instance. An example is
--     @m4.xlarge@.
--
-- -   LEGAL_ENTITY_NAME - The name of the organization that sells you AWS
--     services, such as Amazon Web Services.
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     AWS ID of the member account.
--
-- -   OPERATING_SYSTEM - The operating system. Examples are Windows or
--     Linux.
--
-- -   OPERATION - The action performed. Examples include @RunInstance@ and
--     @CreateBucket@.
--
-- -   PLATFORM - The Amazon EC2 operating system. Examples are Windows or
--     Linux.
--
-- -   PURCHASE_TYPE - The reservation type of the purchase to which this
--     usage is related. Examples include On-Demand Instances and Standard
--     Reserved Instances.
--
-- -   SERVICE - The AWS service such as Amazon DynamoDB.
--
-- -   USAGE_TYPE - The type of usage. An example is DataTransfer-In-Bytes.
--     The response for the @GetDimensionValues@ operation includes a unit
--     attribute. Examples include GB and Hrs.
--
-- -   USAGE_TYPE_GROUP - The grouping of common usage types. An example is
--     Amazon EC2: CloudWatch – Alarms. The response for this operation
--     includes a unit attribute.
--
-- -   REGION - The AWS Region.
--
-- -   RECORD_TYPE - The different types of charges such as RI fees, usage
--     costs, tax refunds, and credits.
--
-- -   RESOURCE_ID - The unique identifier of the resource. ResourceId is
--     an opt-in feature only available for last 14 days for EC2-Compute
--     Service.
--
-- If you set the context to @RESERVATIONS@, you can use the following
-- dimensions for searching:
--
-- -   AZ - The Availability Zone. An example is @us-east-1a@.
--
-- -   CACHE_ENGINE - The Amazon ElastiCache operating system. Examples are
--     Windows or Linux.
--
-- -   DEPLOYMENT_OPTION - The scope of Amazon Relational Database Service
--     deployments. Valid values are @SingleAZ@ and @MultiAZ@.
--
-- -   INSTANCE_TYPE - The type of Amazon EC2 instance. An example is
--     @m4.xlarge@.
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     AWS ID of the member account.
--
-- -   PLATFORM - The Amazon EC2 operating system. Examples are Windows or
--     Linux.
--
-- -   REGION - The AWS Region.
--
-- -   SCOPE (Utilization only) - The scope of a Reserved Instance (RI).
--     Values are regional or a single Availability Zone.
--
-- -   TAG (Coverage only) - The tags that are associated with a Reserved
--     Instance (RI).
--
-- -   TENANCY - The tenancy of a resource. Examples are shared or
--     dedicated.
--
-- If you set the context to @SAVINGS_PLANS@, you can use the following
-- dimensions for searching:
--
-- -   SAVINGS_PLANS_TYPE - Type of Savings Plans (EC2 Instance or Compute)
--
-- -   PAYMENT_OPTION - Payment option for the given Savings Plans (for
--     example, All Upfront)
--
-- -   REGION - The AWS Region.
--
-- -   INSTANCE_TYPE_FAMILY - The family of instances (For example, @m5@)
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     AWS ID of the member account.
--
-- -   SAVINGS_PLAN_ARN - The unique identifier for your Savings Plan
getDimensionValues_context :: Lens.Lens' GetDimensionValues (Core.Maybe Context)
getDimensionValues_context = Lens.lens (\GetDimensionValues' {context} -> context) (\s@GetDimensionValues' {} a -> s {context = a} :: GetDimensionValues)

-- | The value by which you want to sort the data.
--
-- The key represents cost and usage metrics. The following values are
-- supported:
--
-- -   @BlendedCost@
--
-- -   @UnblendedCost@
--
-- -   @AmortizedCost@
--
-- -   @NetAmortizedCost@
--
-- -   @NetUnblendedCost@
--
-- -   @UsageQuantity@
--
-- -   @NormalizedUsageAmount@
--
-- Supported values for @SortOrder@ are @ASCENDING@ or @DESCENDING@.
--
-- When you specify a @SortBy@ paramater, the context must be
-- @COST_AND_USAGE@. Further, when using @SortBy@, @NextPageToken@ and
-- @SearchString@ are not supported.
getDimensionValues_sortBy :: Lens.Lens' GetDimensionValues (Core.Maybe [SortDefinition])
getDimensionValues_sortBy = Lens.lens (\GetDimensionValues' {sortBy} -> sortBy) (\s@GetDimensionValues' {} a -> s {sortBy = a} :: GetDimensionValues) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getDimensionValues_filter :: Lens.Lens' GetDimensionValues (Core.Maybe Expression)
getDimensionValues_filter = Lens.lens (\GetDimensionValues' {filter'} -> filter') (\s@GetDimensionValues' {} a -> s {filter' = a} :: GetDimensionValues)

-- | The start and end dates for retrieving the dimension values. The start
-- date is inclusive, but the end date is exclusive. For example, if
-- @start@ is @2017-01-01@ and @end@ is @2017-05-01@, then the cost and
-- usage data is retrieved from @2017-01-01@ up to and including
-- @2017-04-30@ but not including @2017-05-01@.
getDimensionValues_timePeriod :: Lens.Lens' GetDimensionValues DateInterval
getDimensionValues_timePeriod = Lens.lens (\GetDimensionValues' {timePeriod} -> timePeriod) (\s@GetDimensionValues' {} a -> s {timePeriod = a} :: GetDimensionValues)

-- | The name of the dimension. Each @Dimension@ is available for a different
-- @Context@. For more information, see @Context@.
getDimensionValues_dimension :: Lens.Lens' GetDimensionValues Dimension
getDimensionValues_dimension = Lens.lens (\GetDimensionValues' {dimension} -> dimension) (\s@GetDimensionValues' {} a -> s {dimension = a} :: GetDimensionValues)

instance Core.AWSRequest GetDimensionValues where
  type
    AWSResponse GetDimensionValues =
      GetDimensionValuesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDimensionValuesResponse'
            Core.<$> (x Core..?> "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "DimensionValues" Core..!@ Core.mempty)
            Core.<*> (x Core..:> "ReturnSize")
            Core.<*> (x Core..:> "TotalSize")
      )

instance Core.Hashable GetDimensionValues

instance Core.NFData GetDimensionValues

instance Core.ToHeaders GetDimensionValues where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.GetDimensionValues" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDimensionValues where
  toJSON GetDimensionValues' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("SearchString" Core..=) Core.<$> searchString,
            ("NextPageToken" Core..=) Core.<$> nextPageToken,
            ("Context" Core..=) Core.<$> context,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("Filter" Core..=) Core.<$> filter',
            Core.Just ("TimePeriod" Core..= timePeriod),
            Core.Just ("Dimension" Core..= dimension)
          ]
      )

instance Core.ToPath GetDimensionValues where
  toPath = Core.const "/"

instance Core.ToQuery GetDimensionValues where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDimensionValuesResponse' smart constructor.
data GetDimensionValuesResponse = GetDimensionValuesResponse'
  { -- | The token for the next set of retrievable results. AWS provides the
    -- token when the response from a previous call has more results than the
    -- maximum page size.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The filters that you used to filter your request. Some dimensions are
    -- available only for a specific context.
    --
    -- If you set the context to @COST_AND_USAGE@, you can use the following
    -- dimensions for searching:
    --
    -- -   AZ - The Availability Zone. An example is @us-east-1a@.
    --
    -- -   DATABASE_ENGINE - The Amazon Relational Database Service database.
    --     Examples are Aurora or MySQL.
    --
    -- -   INSTANCE_TYPE - The type of Amazon EC2 instance. An example is
    --     @m4.xlarge@.
    --
    -- -   LEGAL_ENTITY_NAME - The name of the organization that sells you AWS
    --     services, such as Amazon Web Services.
    --
    -- -   LINKED_ACCOUNT - The description in the attribute map that includes
    --     the full name of the member account. The value field contains the
    --     AWS ID of the member account.
    --
    -- -   OPERATING_SYSTEM - The operating system. Examples are Windows or
    --     Linux.
    --
    -- -   OPERATION - The action performed. Examples include @RunInstance@ and
    --     @CreateBucket@.
    --
    -- -   PLATFORM - The Amazon EC2 operating system. Examples are Windows or
    --     Linux.
    --
    -- -   PURCHASE_TYPE - The reservation type of the purchase to which this
    --     usage is related. Examples include On-Demand Instances and Standard
    --     Reserved Instances.
    --
    -- -   SERVICE - The AWS service such as Amazon DynamoDB.
    --
    -- -   USAGE_TYPE - The type of usage. An example is DataTransfer-In-Bytes.
    --     The response for the @GetDimensionValues@ operation includes a unit
    --     attribute. Examples include GB and Hrs.
    --
    -- -   USAGE_TYPE_GROUP - The grouping of common usage types. An example is
    --     Amazon EC2: CloudWatch – Alarms. The response for this operation
    --     includes a unit attribute.
    --
    -- -   RECORD_TYPE - The different types of charges such as RI fees, usage
    --     costs, tax refunds, and credits.
    --
    -- -   RESOURCE_ID - The unique identifier of the resource. ResourceId is
    --     an opt-in feature only available for last 14 days for EC2-Compute
    --     Service.
    --
    -- If you set the context to @RESERVATIONS@, you can use the following
    -- dimensions for searching:
    --
    -- -   AZ - The Availability Zone. An example is @us-east-1a@.
    --
    -- -   CACHE_ENGINE - The Amazon ElastiCache operating system. Examples are
    --     Windows or Linux.
    --
    -- -   DEPLOYMENT_OPTION - The scope of Amazon Relational Database Service
    --     deployments. Valid values are @SingleAZ@ and @MultiAZ@.
    --
    -- -   INSTANCE_TYPE - The type of Amazon EC2 instance. An example is
    --     @m4.xlarge@.
    --
    -- -   LINKED_ACCOUNT - The description in the attribute map that includes
    --     the full name of the member account. The value field contains the
    --     AWS ID of the member account.
    --
    -- -   PLATFORM - The Amazon EC2 operating system. Examples are Windows or
    --     Linux.
    --
    -- -   REGION - The AWS Region.
    --
    -- -   SCOPE (Utilization only) - The scope of a Reserved Instance (RI).
    --     Values are regional or a single Availability Zone.
    --
    -- -   TAG (Coverage only) - The tags that are associated with a Reserved
    --     Instance (RI).
    --
    -- -   TENANCY - The tenancy of a resource. Examples are shared or
    --     dedicated.
    --
    -- If you set the context to @SAVINGS_PLANS@, you can use the following
    -- dimensions for searching:
    --
    -- -   SAVINGS_PLANS_TYPE - Type of Savings Plans (EC2 Instance or Compute)
    --
    -- -   PAYMENT_OPTION - Payment option for the given Savings Plans (for
    --     example, All Upfront)
    --
    -- -   REGION - The AWS Region.
    --
    -- -   INSTANCE_TYPE_FAMILY - The family of instances (For example, @m5@)
    --
    -- -   LINKED_ACCOUNT - The description in the attribute map that includes
    --     the full name of the member account. The value field contains the
    --     AWS ID of the member account.
    --
    -- -   SAVINGS_PLAN_ARN - The unique identifier for your Savings Plan
    dimensionValues :: [DimensionValuesWithAttributes],
    -- | The number of results that AWS returned at one time.
    returnSize :: Core.Int,
    -- | The total number of search results.
    totalSize :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDimensionValuesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getDimensionValuesResponse_nextPageToken' - The token for the next set of retrievable results. AWS provides the
-- token when the response from a previous call has more results than the
-- maximum page size.
--
-- 'httpStatus', 'getDimensionValuesResponse_httpStatus' - The response's http status code.
--
-- 'dimensionValues', 'getDimensionValuesResponse_dimensionValues' - The filters that you used to filter your request. Some dimensions are
-- available only for a specific context.
--
-- If you set the context to @COST_AND_USAGE@, you can use the following
-- dimensions for searching:
--
-- -   AZ - The Availability Zone. An example is @us-east-1a@.
--
-- -   DATABASE_ENGINE - The Amazon Relational Database Service database.
--     Examples are Aurora or MySQL.
--
-- -   INSTANCE_TYPE - The type of Amazon EC2 instance. An example is
--     @m4.xlarge@.
--
-- -   LEGAL_ENTITY_NAME - The name of the organization that sells you AWS
--     services, such as Amazon Web Services.
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     AWS ID of the member account.
--
-- -   OPERATING_SYSTEM - The operating system. Examples are Windows or
--     Linux.
--
-- -   OPERATION - The action performed. Examples include @RunInstance@ and
--     @CreateBucket@.
--
-- -   PLATFORM - The Amazon EC2 operating system. Examples are Windows or
--     Linux.
--
-- -   PURCHASE_TYPE - The reservation type of the purchase to which this
--     usage is related. Examples include On-Demand Instances and Standard
--     Reserved Instances.
--
-- -   SERVICE - The AWS service such as Amazon DynamoDB.
--
-- -   USAGE_TYPE - The type of usage. An example is DataTransfer-In-Bytes.
--     The response for the @GetDimensionValues@ operation includes a unit
--     attribute. Examples include GB and Hrs.
--
-- -   USAGE_TYPE_GROUP - The grouping of common usage types. An example is
--     Amazon EC2: CloudWatch – Alarms. The response for this operation
--     includes a unit attribute.
--
-- -   RECORD_TYPE - The different types of charges such as RI fees, usage
--     costs, tax refunds, and credits.
--
-- -   RESOURCE_ID - The unique identifier of the resource. ResourceId is
--     an opt-in feature only available for last 14 days for EC2-Compute
--     Service.
--
-- If you set the context to @RESERVATIONS@, you can use the following
-- dimensions for searching:
--
-- -   AZ - The Availability Zone. An example is @us-east-1a@.
--
-- -   CACHE_ENGINE - The Amazon ElastiCache operating system. Examples are
--     Windows or Linux.
--
-- -   DEPLOYMENT_OPTION - The scope of Amazon Relational Database Service
--     deployments. Valid values are @SingleAZ@ and @MultiAZ@.
--
-- -   INSTANCE_TYPE - The type of Amazon EC2 instance. An example is
--     @m4.xlarge@.
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     AWS ID of the member account.
--
-- -   PLATFORM - The Amazon EC2 operating system. Examples are Windows or
--     Linux.
--
-- -   REGION - The AWS Region.
--
-- -   SCOPE (Utilization only) - The scope of a Reserved Instance (RI).
--     Values are regional or a single Availability Zone.
--
-- -   TAG (Coverage only) - The tags that are associated with a Reserved
--     Instance (RI).
--
-- -   TENANCY - The tenancy of a resource. Examples are shared or
--     dedicated.
--
-- If you set the context to @SAVINGS_PLANS@, you can use the following
-- dimensions for searching:
--
-- -   SAVINGS_PLANS_TYPE - Type of Savings Plans (EC2 Instance or Compute)
--
-- -   PAYMENT_OPTION - Payment option for the given Savings Plans (for
--     example, All Upfront)
--
-- -   REGION - The AWS Region.
--
-- -   INSTANCE_TYPE_FAMILY - The family of instances (For example, @m5@)
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     AWS ID of the member account.
--
-- -   SAVINGS_PLAN_ARN - The unique identifier for your Savings Plan
--
-- 'returnSize', 'getDimensionValuesResponse_returnSize' - The number of results that AWS returned at one time.
--
-- 'totalSize', 'getDimensionValuesResponse_totalSize' - The total number of search results.
newGetDimensionValuesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'returnSize'
  Core.Int ->
  -- | 'totalSize'
  Core.Int ->
  GetDimensionValuesResponse
newGetDimensionValuesResponse
  pHttpStatus_
  pReturnSize_
  pTotalSize_ =
    GetDimensionValuesResponse'
      { nextPageToken =
          Core.Nothing,
        httpStatus = pHttpStatus_,
        dimensionValues = Core.mempty,
        returnSize = pReturnSize_,
        totalSize = pTotalSize_
      }

-- | The token for the next set of retrievable results. AWS provides the
-- token when the response from a previous call has more results than the
-- maximum page size.
getDimensionValuesResponse_nextPageToken :: Lens.Lens' GetDimensionValuesResponse (Core.Maybe Core.Text)
getDimensionValuesResponse_nextPageToken = Lens.lens (\GetDimensionValuesResponse' {nextPageToken} -> nextPageToken) (\s@GetDimensionValuesResponse' {} a -> s {nextPageToken = a} :: GetDimensionValuesResponse)

-- | The response's http status code.
getDimensionValuesResponse_httpStatus :: Lens.Lens' GetDimensionValuesResponse Core.Int
getDimensionValuesResponse_httpStatus = Lens.lens (\GetDimensionValuesResponse' {httpStatus} -> httpStatus) (\s@GetDimensionValuesResponse' {} a -> s {httpStatus = a} :: GetDimensionValuesResponse)

-- | The filters that you used to filter your request. Some dimensions are
-- available only for a specific context.
--
-- If you set the context to @COST_AND_USAGE@, you can use the following
-- dimensions for searching:
--
-- -   AZ - The Availability Zone. An example is @us-east-1a@.
--
-- -   DATABASE_ENGINE - The Amazon Relational Database Service database.
--     Examples are Aurora or MySQL.
--
-- -   INSTANCE_TYPE - The type of Amazon EC2 instance. An example is
--     @m4.xlarge@.
--
-- -   LEGAL_ENTITY_NAME - The name of the organization that sells you AWS
--     services, such as Amazon Web Services.
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     AWS ID of the member account.
--
-- -   OPERATING_SYSTEM - The operating system. Examples are Windows or
--     Linux.
--
-- -   OPERATION - The action performed. Examples include @RunInstance@ and
--     @CreateBucket@.
--
-- -   PLATFORM - The Amazon EC2 operating system. Examples are Windows or
--     Linux.
--
-- -   PURCHASE_TYPE - The reservation type of the purchase to which this
--     usage is related. Examples include On-Demand Instances and Standard
--     Reserved Instances.
--
-- -   SERVICE - The AWS service such as Amazon DynamoDB.
--
-- -   USAGE_TYPE - The type of usage. An example is DataTransfer-In-Bytes.
--     The response for the @GetDimensionValues@ operation includes a unit
--     attribute. Examples include GB and Hrs.
--
-- -   USAGE_TYPE_GROUP - The grouping of common usage types. An example is
--     Amazon EC2: CloudWatch – Alarms. The response for this operation
--     includes a unit attribute.
--
-- -   RECORD_TYPE - The different types of charges such as RI fees, usage
--     costs, tax refunds, and credits.
--
-- -   RESOURCE_ID - The unique identifier of the resource. ResourceId is
--     an opt-in feature only available for last 14 days for EC2-Compute
--     Service.
--
-- If you set the context to @RESERVATIONS@, you can use the following
-- dimensions for searching:
--
-- -   AZ - The Availability Zone. An example is @us-east-1a@.
--
-- -   CACHE_ENGINE - The Amazon ElastiCache operating system. Examples are
--     Windows or Linux.
--
-- -   DEPLOYMENT_OPTION - The scope of Amazon Relational Database Service
--     deployments. Valid values are @SingleAZ@ and @MultiAZ@.
--
-- -   INSTANCE_TYPE - The type of Amazon EC2 instance. An example is
--     @m4.xlarge@.
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     AWS ID of the member account.
--
-- -   PLATFORM - The Amazon EC2 operating system. Examples are Windows or
--     Linux.
--
-- -   REGION - The AWS Region.
--
-- -   SCOPE (Utilization only) - The scope of a Reserved Instance (RI).
--     Values are regional or a single Availability Zone.
--
-- -   TAG (Coverage only) - The tags that are associated with a Reserved
--     Instance (RI).
--
-- -   TENANCY - The tenancy of a resource. Examples are shared or
--     dedicated.
--
-- If you set the context to @SAVINGS_PLANS@, you can use the following
-- dimensions for searching:
--
-- -   SAVINGS_PLANS_TYPE - Type of Savings Plans (EC2 Instance or Compute)
--
-- -   PAYMENT_OPTION - Payment option for the given Savings Plans (for
--     example, All Upfront)
--
-- -   REGION - The AWS Region.
--
-- -   INSTANCE_TYPE_FAMILY - The family of instances (For example, @m5@)
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     AWS ID of the member account.
--
-- -   SAVINGS_PLAN_ARN - The unique identifier for your Savings Plan
getDimensionValuesResponse_dimensionValues :: Lens.Lens' GetDimensionValuesResponse [DimensionValuesWithAttributes]
getDimensionValuesResponse_dimensionValues = Lens.lens (\GetDimensionValuesResponse' {dimensionValues} -> dimensionValues) (\s@GetDimensionValuesResponse' {} a -> s {dimensionValues = a} :: GetDimensionValuesResponse) Core.. Lens._Coerce

-- | The number of results that AWS returned at one time.
getDimensionValuesResponse_returnSize :: Lens.Lens' GetDimensionValuesResponse Core.Int
getDimensionValuesResponse_returnSize = Lens.lens (\GetDimensionValuesResponse' {returnSize} -> returnSize) (\s@GetDimensionValuesResponse' {} a -> s {returnSize = a} :: GetDimensionValuesResponse)

-- | The total number of search results.
getDimensionValuesResponse_totalSize :: Lens.Lens' GetDimensionValuesResponse Core.Int
getDimensionValuesResponse_totalSize = Lens.lens (\GetDimensionValuesResponse' {totalSize} -> totalSize) (\s@GetDimensionValuesResponse' {} a -> s {totalSize = a} :: GetDimensionValuesResponse)

instance Core.NFData GetDimensionValuesResponse
