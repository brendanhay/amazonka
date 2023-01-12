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
-- Module      : Amazonka.CostExplorer.GetDimensionValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all available filter values for a specified filter over a
-- period of time. You can search the dimension values for an arbitrary
-- string.
module Amazonka.CostExplorer.GetDimensionValues
  ( -- * Creating a Request
    GetDimensionValues (..),
    newGetDimensionValues,

    -- * Request Lenses
    getDimensionValues_context,
    getDimensionValues_filter,
    getDimensionValues_maxResults,
    getDimensionValues_nextPageToken,
    getDimensionValues_searchString,
    getDimensionValues_sortBy,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDimensionValues' smart constructor.
data GetDimensionValues = GetDimensionValues'
  { -- | The context for the call to @GetDimensionValues@. This can be
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
    -- -   BILLING_ENTITY - The Amazon Web Services seller that your account is
    --     with. Possible values are the following:
    --
    --     - Amazon Web Services(Amazon Web Services): The entity that sells
    --     Amazon Web Services.
    --
    --     - AISPL (Amazon Internet Services Pvt. Ltd.): The local Indian
    --     entity that\'s an acting reseller for Amazon Web Services in India.
    --
    --     - Amazon Web Services Marketplace: The entity that supports the sale
    --     of solutions that are built on Amazon Web Services by third-party
    --     software providers.
    --
    -- -   CACHE_ENGINE - The Amazon ElastiCache operating system. Examples are
    --     Windows or Linux.
    --
    -- -   DEPLOYMENT_OPTION - The scope of Amazon Relational Database Service
    --     deployments. Valid values are @SingleAZ@ and @MultiAZ@.
    --
    -- -   DATABASE_ENGINE - The Amazon Relational Database Service database.
    --     Examples are Aurora or MySQL.
    --
    -- -   INSTANCE_TYPE - The type of Amazon EC2 instance. An example is
    --     @m4.xlarge@.
    --
    -- -   INSTANCE_TYPE_FAMILY - A family of instance types optimized to fit
    --     different use cases. Examples are @Compute Optimized@ (for example,
    --     @C4@, @C5@, @C6g@, and @C7g@), @Memory Optimization@ (for example,
    --     @R4@, @R5n@, @R5b@, and @R6g@).
    --
    -- -   INVOICING_ENTITY - The name of the entity that issues the Amazon Web
    --     Services invoice.
    --
    -- -   LEGAL_ENTITY_NAME - The name of the organization that sells you
    --     Amazon Web Services services, such as Amazon Web Services.
    --
    -- -   LINKED_ACCOUNT - The description in the attribute map that includes
    --     the full name of the member account. The value field contains the
    --     Amazon Web Services ID of the member account.
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
    -- -   PURCHASE_TYPE - The reservation type of the purchase that this usage
    --     is related to. Examples include On-Demand Instances and Standard
    --     Reserved Instances.
    --
    -- -   RESERVATION_ID - The unique identifier for an Amazon Web Services
    --     Reservation Instance.
    --
    -- -   SAVINGS_PLAN_ARN - The unique identifier for your Savings Plans.
    --
    -- -   SAVINGS_PLANS_TYPE - Type of Savings Plans (EC2 Instance or
    --     Compute).
    --
    -- -   SERVICE - The Amazon Web Services service such as Amazon DynamoDB.
    --
    -- -   TENANCY - The tenancy of a resource. Examples are shared or
    --     dedicated.
    --
    -- -   USAGE_TYPE - The type of usage. An example is DataTransfer-In-Bytes.
    --     The response for the @GetDimensionValues@ operation includes a unit
    --     attribute. Examples include GB and Hrs.
    --
    -- -   USAGE_TYPE_GROUP - The grouping of common usage types. An example is
    --     Amazon EC2: CloudWatch – Alarms. The response for this operation
    --     includes a unit attribute.
    --
    -- -   REGION - The Amazon Web Services Region.
    --
    -- -   RECORD_TYPE - The different types of charges such as Reserved
    --     Instance (RI) fees, usage costs, tax refunds, and credits.
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
    --     Amazon Web Services ID of the member account.
    --
    -- -   PLATFORM - The Amazon EC2 operating system. Examples are Windows or
    --     Linux.
    --
    -- -   REGION - The Amazon Web Services Region.
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
    -- -   PAYMENT_OPTION - The payment option for the given Savings Plans (for
    --     example, All Upfront)
    --
    -- -   REGION - The Amazon Web Services Region.
    --
    -- -   INSTANCE_TYPE_FAMILY - The family of instances (For example, @m5@)
    --
    -- -   LINKED_ACCOUNT - The description in the attribute map that includes
    --     the full name of the member account. The value field contains the
    --     Amazon Web Services ID of the member account.
    --
    -- -   SAVINGS_PLAN_ARN - The unique identifier for your Savings Plans.
    context :: Prelude.Maybe Context,
    filter' :: Prelude.Maybe Expression,
    -- | This field is only used when SortBy is provided in the request. The
    -- maximum number of objects that are returned for this request. If
    -- MaxResults isn\'t specified with SortBy, the request returns 1000
    -- results as the default value for this parameter.
    --
    -- For @GetDimensionValues@, MaxResults has an upper limit of 1000.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The value that you want to search the filter values for.
    searchString :: Prelude.Maybe Prelude.Text,
    -- | The value that you want to sort the data by.
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
    -- The supported values for the @SortOrder@ key are @ASCENDING@ or
    -- @DESCENDING@.
    --
    -- When you specify a @SortBy@ paramater, the context must be
    -- @COST_AND_USAGE@. Further, when using @SortBy@, @NextPageToken@ and
    -- @SearchString@ aren\'t supported.
    sortBy :: Prelude.Maybe [SortDefinition],
    -- | The start date and end date for retrieving the dimension values. The
    -- start date is inclusive, but the end date is exclusive. For example, if
    -- @start@ is @2017-01-01@ and @end@ is @2017-05-01@, then the cost and
    -- usage data is retrieved from @2017-01-01@ up to and including
    -- @2017-04-30@ but not including @2017-05-01@.
    timePeriod :: DateInterval,
    -- | The name of the dimension. Each @Dimension@ is available for a different
    -- @Context@. For more information, see @Context@. @LINK_ACCOUNT_NAME@ and
    -- @SERVICE_CODE@ can only be used in
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/AAPI_CostCategoryRule.html CostCategoryRule>.
    dimension :: Dimension
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDimensionValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- -   BILLING_ENTITY - The Amazon Web Services seller that your account is
--     with. Possible values are the following:
--
--     - Amazon Web Services(Amazon Web Services): The entity that sells
--     Amazon Web Services.
--
--     - AISPL (Amazon Internet Services Pvt. Ltd.): The local Indian
--     entity that\'s an acting reseller for Amazon Web Services in India.
--
--     - Amazon Web Services Marketplace: The entity that supports the sale
--     of solutions that are built on Amazon Web Services by third-party
--     software providers.
--
-- -   CACHE_ENGINE - The Amazon ElastiCache operating system. Examples are
--     Windows or Linux.
--
-- -   DEPLOYMENT_OPTION - The scope of Amazon Relational Database Service
--     deployments. Valid values are @SingleAZ@ and @MultiAZ@.
--
-- -   DATABASE_ENGINE - The Amazon Relational Database Service database.
--     Examples are Aurora or MySQL.
--
-- -   INSTANCE_TYPE - The type of Amazon EC2 instance. An example is
--     @m4.xlarge@.
--
-- -   INSTANCE_TYPE_FAMILY - A family of instance types optimized to fit
--     different use cases. Examples are @Compute Optimized@ (for example,
--     @C4@, @C5@, @C6g@, and @C7g@), @Memory Optimization@ (for example,
--     @R4@, @R5n@, @R5b@, and @R6g@).
--
-- -   INVOICING_ENTITY - The name of the entity that issues the Amazon Web
--     Services invoice.
--
-- -   LEGAL_ENTITY_NAME - The name of the organization that sells you
--     Amazon Web Services services, such as Amazon Web Services.
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     Amazon Web Services ID of the member account.
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
-- -   PURCHASE_TYPE - The reservation type of the purchase that this usage
--     is related to. Examples include On-Demand Instances and Standard
--     Reserved Instances.
--
-- -   RESERVATION_ID - The unique identifier for an Amazon Web Services
--     Reservation Instance.
--
-- -   SAVINGS_PLAN_ARN - The unique identifier for your Savings Plans.
--
-- -   SAVINGS_PLANS_TYPE - Type of Savings Plans (EC2 Instance or
--     Compute).
--
-- -   SERVICE - The Amazon Web Services service such as Amazon DynamoDB.
--
-- -   TENANCY - The tenancy of a resource. Examples are shared or
--     dedicated.
--
-- -   USAGE_TYPE - The type of usage. An example is DataTransfer-In-Bytes.
--     The response for the @GetDimensionValues@ operation includes a unit
--     attribute. Examples include GB and Hrs.
--
-- -   USAGE_TYPE_GROUP - The grouping of common usage types. An example is
--     Amazon EC2: CloudWatch – Alarms. The response for this operation
--     includes a unit attribute.
--
-- -   REGION - The Amazon Web Services Region.
--
-- -   RECORD_TYPE - The different types of charges such as Reserved
--     Instance (RI) fees, usage costs, tax refunds, and credits.
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
--     Amazon Web Services ID of the member account.
--
-- -   PLATFORM - The Amazon EC2 operating system. Examples are Windows or
--     Linux.
--
-- -   REGION - The Amazon Web Services Region.
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
-- -   PAYMENT_OPTION - The payment option for the given Savings Plans (for
--     example, All Upfront)
--
-- -   REGION - The Amazon Web Services Region.
--
-- -   INSTANCE_TYPE_FAMILY - The family of instances (For example, @m5@)
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     Amazon Web Services ID of the member account.
--
-- -   SAVINGS_PLAN_ARN - The unique identifier for your Savings Plans.
--
-- 'filter'', 'getDimensionValues_filter' - Undocumented member.
--
-- 'maxResults', 'getDimensionValues_maxResults' - This field is only used when SortBy is provided in the request. The
-- maximum number of objects that are returned for this request. If
-- MaxResults isn\'t specified with SortBy, the request returns 1000
-- results as the default value for this parameter.
--
-- For @GetDimensionValues@, MaxResults has an upper limit of 1000.
--
-- 'nextPageToken', 'getDimensionValues_nextPageToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'searchString', 'getDimensionValues_searchString' - The value that you want to search the filter values for.
--
-- 'sortBy', 'getDimensionValues_sortBy' - The value that you want to sort the data by.
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
-- The supported values for the @SortOrder@ key are @ASCENDING@ or
-- @DESCENDING@.
--
-- When you specify a @SortBy@ paramater, the context must be
-- @COST_AND_USAGE@. Further, when using @SortBy@, @NextPageToken@ and
-- @SearchString@ aren\'t supported.
--
-- 'timePeriod', 'getDimensionValues_timePeriod' - The start date and end date for retrieving the dimension values. The
-- start date is inclusive, but the end date is exclusive. For example, if
-- @start@ is @2017-01-01@ and @end@ is @2017-05-01@, then the cost and
-- usage data is retrieved from @2017-01-01@ up to and including
-- @2017-04-30@ but not including @2017-05-01@.
--
-- 'dimension', 'getDimensionValues_dimension' - The name of the dimension. Each @Dimension@ is available for a different
-- @Context@. For more information, see @Context@. @LINK_ACCOUNT_NAME@ and
-- @SERVICE_CODE@ can only be used in
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/AAPI_CostCategoryRule.html CostCategoryRule>.
newGetDimensionValues ::
  -- | 'timePeriod'
  DateInterval ->
  -- | 'dimension'
  Dimension ->
  GetDimensionValues
newGetDimensionValues pTimePeriod_ pDimension_ =
  GetDimensionValues'
    { context = Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      searchString = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      timePeriod = pTimePeriod_,
      dimension = pDimension_
    }

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
-- -   BILLING_ENTITY - The Amazon Web Services seller that your account is
--     with. Possible values are the following:
--
--     - Amazon Web Services(Amazon Web Services): The entity that sells
--     Amazon Web Services.
--
--     - AISPL (Amazon Internet Services Pvt. Ltd.): The local Indian
--     entity that\'s an acting reseller for Amazon Web Services in India.
--
--     - Amazon Web Services Marketplace: The entity that supports the sale
--     of solutions that are built on Amazon Web Services by third-party
--     software providers.
--
-- -   CACHE_ENGINE - The Amazon ElastiCache operating system. Examples are
--     Windows or Linux.
--
-- -   DEPLOYMENT_OPTION - The scope of Amazon Relational Database Service
--     deployments. Valid values are @SingleAZ@ and @MultiAZ@.
--
-- -   DATABASE_ENGINE - The Amazon Relational Database Service database.
--     Examples are Aurora or MySQL.
--
-- -   INSTANCE_TYPE - The type of Amazon EC2 instance. An example is
--     @m4.xlarge@.
--
-- -   INSTANCE_TYPE_FAMILY - A family of instance types optimized to fit
--     different use cases. Examples are @Compute Optimized@ (for example,
--     @C4@, @C5@, @C6g@, and @C7g@), @Memory Optimization@ (for example,
--     @R4@, @R5n@, @R5b@, and @R6g@).
--
-- -   INVOICING_ENTITY - The name of the entity that issues the Amazon Web
--     Services invoice.
--
-- -   LEGAL_ENTITY_NAME - The name of the organization that sells you
--     Amazon Web Services services, such as Amazon Web Services.
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     Amazon Web Services ID of the member account.
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
-- -   PURCHASE_TYPE - The reservation type of the purchase that this usage
--     is related to. Examples include On-Demand Instances and Standard
--     Reserved Instances.
--
-- -   RESERVATION_ID - The unique identifier for an Amazon Web Services
--     Reservation Instance.
--
-- -   SAVINGS_PLAN_ARN - The unique identifier for your Savings Plans.
--
-- -   SAVINGS_PLANS_TYPE - Type of Savings Plans (EC2 Instance or
--     Compute).
--
-- -   SERVICE - The Amazon Web Services service such as Amazon DynamoDB.
--
-- -   TENANCY - The tenancy of a resource. Examples are shared or
--     dedicated.
--
-- -   USAGE_TYPE - The type of usage. An example is DataTransfer-In-Bytes.
--     The response for the @GetDimensionValues@ operation includes a unit
--     attribute. Examples include GB and Hrs.
--
-- -   USAGE_TYPE_GROUP - The grouping of common usage types. An example is
--     Amazon EC2: CloudWatch – Alarms. The response for this operation
--     includes a unit attribute.
--
-- -   REGION - The Amazon Web Services Region.
--
-- -   RECORD_TYPE - The different types of charges such as Reserved
--     Instance (RI) fees, usage costs, tax refunds, and credits.
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
--     Amazon Web Services ID of the member account.
--
-- -   PLATFORM - The Amazon EC2 operating system. Examples are Windows or
--     Linux.
--
-- -   REGION - The Amazon Web Services Region.
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
-- -   PAYMENT_OPTION - The payment option for the given Savings Plans (for
--     example, All Upfront)
--
-- -   REGION - The Amazon Web Services Region.
--
-- -   INSTANCE_TYPE_FAMILY - The family of instances (For example, @m5@)
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     Amazon Web Services ID of the member account.
--
-- -   SAVINGS_PLAN_ARN - The unique identifier for your Savings Plans.
getDimensionValues_context :: Lens.Lens' GetDimensionValues (Prelude.Maybe Context)
getDimensionValues_context = Lens.lens (\GetDimensionValues' {context} -> context) (\s@GetDimensionValues' {} a -> s {context = a} :: GetDimensionValues)

-- | Undocumented member.
getDimensionValues_filter :: Lens.Lens' GetDimensionValues (Prelude.Maybe Expression)
getDimensionValues_filter = Lens.lens (\GetDimensionValues' {filter'} -> filter') (\s@GetDimensionValues' {} a -> s {filter' = a} :: GetDimensionValues)

-- | This field is only used when SortBy is provided in the request. The
-- maximum number of objects that are returned for this request. If
-- MaxResults isn\'t specified with SortBy, the request returns 1000
-- results as the default value for this parameter.
--
-- For @GetDimensionValues@, MaxResults has an upper limit of 1000.
getDimensionValues_maxResults :: Lens.Lens' GetDimensionValues (Prelude.Maybe Prelude.Natural)
getDimensionValues_maxResults = Lens.lens (\GetDimensionValues' {maxResults} -> maxResults) (\s@GetDimensionValues' {} a -> s {maxResults = a} :: GetDimensionValues)

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getDimensionValues_nextPageToken :: Lens.Lens' GetDimensionValues (Prelude.Maybe Prelude.Text)
getDimensionValues_nextPageToken = Lens.lens (\GetDimensionValues' {nextPageToken} -> nextPageToken) (\s@GetDimensionValues' {} a -> s {nextPageToken = a} :: GetDimensionValues)

-- | The value that you want to search the filter values for.
getDimensionValues_searchString :: Lens.Lens' GetDimensionValues (Prelude.Maybe Prelude.Text)
getDimensionValues_searchString = Lens.lens (\GetDimensionValues' {searchString} -> searchString) (\s@GetDimensionValues' {} a -> s {searchString = a} :: GetDimensionValues)

-- | The value that you want to sort the data by.
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
-- The supported values for the @SortOrder@ key are @ASCENDING@ or
-- @DESCENDING@.
--
-- When you specify a @SortBy@ paramater, the context must be
-- @COST_AND_USAGE@. Further, when using @SortBy@, @NextPageToken@ and
-- @SearchString@ aren\'t supported.
getDimensionValues_sortBy :: Lens.Lens' GetDimensionValues (Prelude.Maybe [SortDefinition])
getDimensionValues_sortBy = Lens.lens (\GetDimensionValues' {sortBy} -> sortBy) (\s@GetDimensionValues' {} a -> s {sortBy = a} :: GetDimensionValues) Prelude.. Lens.mapping Lens.coerced

-- | The start date and end date for retrieving the dimension values. The
-- start date is inclusive, but the end date is exclusive. For example, if
-- @start@ is @2017-01-01@ and @end@ is @2017-05-01@, then the cost and
-- usage data is retrieved from @2017-01-01@ up to and including
-- @2017-04-30@ but not including @2017-05-01@.
getDimensionValues_timePeriod :: Lens.Lens' GetDimensionValues DateInterval
getDimensionValues_timePeriod = Lens.lens (\GetDimensionValues' {timePeriod} -> timePeriod) (\s@GetDimensionValues' {} a -> s {timePeriod = a} :: GetDimensionValues)

-- | The name of the dimension. Each @Dimension@ is available for a different
-- @Context@. For more information, see @Context@. @LINK_ACCOUNT_NAME@ and
-- @SERVICE_CODE@ can only be used in
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/AAPI_CostCategoryRule.html CostCategoryRule>.
getDimensionValues_dimension :: Lens.Lens' GetDimensionValues Dimension
getDimensionValues_dimension = Lens.lens (\GetDimensionValues' {dimension} -> dimension) (\s@GetDimensionValues' {} a -> s {dimension = a} :: GetDimensionValues)

instance Core.AWSRequest GetDimensionValues where
  type
    AWSResponse GetDimensionValues =
      GetDimensionValuesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDimensionValuesResponse'
            Prelude.<$> (x Data..?> "NextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "DimensionValues"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..:> "ReturnSize")
            Prelude.<*> (x Data..:> "TotalSize")
      )

instance Prelude.Hashable GetDimensionValues where
  hashWithSalt _salt GetDimensionValues' {..} =
    _salt `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextPageToken
      `Prelude.hashWithSalt` searchString
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` timePeriod
      `Prelude.hashWithSalt` dimension

instance Prelude.NFData GetDimensionValues where
  rnf GetDimensionValues' {..} =
    Prelude.rnf context
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf searchString
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf timePeriod
      `Prelude.seq` Prelude.rnf dimension

instance Data.ToHeaders GetDimensionValues where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSInsightsIndexService.GetDimensionValues" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDimensionValues where
  toJSON GetDimensionValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Context" Data..=) Prelude.<$> context,
            ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextPageToken" Data..=) Prelude.<$> nextPageToken,
            ("SearchString" Data..=) Prelude.<$> searchString,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            Prelude.Just ("TimePeriod" Data..= timePeriod),
            Prelude.Just ("Dimension" Data..= dimension)
          ]
      )

instance Data.ToPath GetDimensionValues where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDimensionValues where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDimensionValuesResponse' smart constructor.
data GetDimensionValuesResponse = GetDimensionValuesResponse'
  { -- | The token for the next set of retrievable results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
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
    -- -   LEGAL_ENTITY_NAME - The name of the organization that sells you
    --     Amazon Web Services services, such as Amazon Web Services.
    --
    -- -   LINKED_ACCOUNT - The description in the attribute map that includes
    --     the full name of the member account. The value field contains the
    --     Amazon Web Services ID of the member account.
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
    -- -   SERVICE - The Amazon Web Services service such as Amazon DynamoDB.
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
    --     Service. You can opt-in by enabling @Hourly@ and
    --     @Resource Level Data@ in Cost Management Console preferences.
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
    --     Amazon Web Services ID of the member account.
    --
    -- -   PLATFORM - The Amazon EC2 operating system. Examples are Windows or
    --     Linux.
    --
    -- -   REGION - The Amazon Web Services Region.
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
    -- -   REGION - The Amazon Web Services Region.
    --
    -- -   INSTANCE_TYPE_FAMILY - The family of instances (For example, @m5@)
    --
    -- -   LINKED_ACCOUNT - The description in the attribute map that includes
    --     the full name of the member account. The value field contains the
    --     Amazon Web Services ID of the member account.
    --
    -- -   SAVINGS_PLAN_ARN - The unique identifier for your Savings Plan
    dimensionValues :: [DimensionValuesWithAttributes],
    -- | The number of results that Amazon Web Services returned at one time.
    returnSize :: Prelude.Int,
    -- | The total number of search results.
    totalSize :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDimensionValuesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getDimensionValuesResponse_nextPageToken' - The token for the next set of retrievable results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
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
-- -   LEGAL_ENTITY_NAME - The name of the organization that sells you
--     Amazon Web Services services, such as Amazon Web Services.
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     Amazon Web Services ID of the member account.
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
-- -   SERVICE - The Amazon Web Services service such as Amazon DynamoDB.
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
--     Service. You can opt-in by enabling @Hourly@ and
--     @Resource Level Data@ in Cost Management Console preferences.
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
--     Amazon Web Services ID of the member account.
--
-- -   PLATFORM - The Amazon EC2 operating system. Examples are Windows or
--     Linux.
--
-- -   REGION - The Amazon Web Services Region.
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
-- -   REGION - The Amazon Web Services Region.
--
-- -   INSTANCE_TYPE_FAMILY - The family of instances (For example, @m5@)
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     Amazon Web Services ID of the member account.
--
-- -   SAVINGS_PLAN_ARN - The unique identifier for your Savings Plan
--
-- 'returnSize', 'getDimensionValuesResponse_returnSize' - The number of results that Amazon Web Services returned at one time.
--
-- 'totalSize', 'getDimensionValuesResponse_totalSize' - The total number of search results.
newGetDimensionValuesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'returnSize'
  Prelude.Int ->
  -- | 'totalSize'
  Prelude.Int ->
  GetDimensionValuesResponse
newGetDimensionValuesResponse
  pHttpStatus_
  pReturnSize_
  pTotalSize_ =
    GetDimensionValuesResponse'
      { nextPageToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        dimensionValues = Prelude.mempty,
        returnSize = pReturnSize_,
        totalSize = pTotalSize_
      }

-- | The token for the next set of retrievable results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getDimensionValuesResponse_nextPageToken :: Lens.Lens' GetDimensionValuesResponse (Prelude.Maybe Prelude.Text)
getDimensionValuesResponse_nextPageToken = Lens.lens (\GetDimensionValuesResponse' {nextPageToken} -> nextPageToken) (\s@GetDimensionValuesResponse' {} a -> s {nextPageToken = a} :: GetDimensionValuesResponse)

-- | The response's http status code.
getDimensionValuesResponse_httpStatus :: Lens.Lens' GetDimensionValuesResponse Prelude.Int
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
-- -   LEGAL_ENTITY_NAME - The name of the organization that sells you
--     Amazon Web Services services, such as Amazon Web Services.
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     Amazon Web Services ID of the member account.
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
-- -   SERVICE - The Amazon Web Services service such as Amazon DynamoDB.
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
--     Service. You can opt-in by enabling @Hourly@ and
--     @Resource Level Data@ in Cost Management Console preferences.
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
--     Amazon Web Services ID of the member account.
--
-- -   PLATFORM - The Amazon EC2 operating system. Examples are Windows or
--     Linux.
--
-- -   REGION - The Amazon Web Services Region.
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
-- -   REGION - The Amazon Web Services Region.
--
-- -   INSTANCE_TYPE_FAMILY - The family of instances (For example, @m5@)
--
-- -   LINKED_ACCOUNT - The description in the attribute map that includes
--     the full name of the member account. The value field contains the
--     Amazon Web Services ID of the member account.
--
-- -   SAVINGS_PLAN_ARN - The unique identifier for your Savings Plan
getDimensionValuesResponse_dimensionValues :: Lens.Lens' GetDimensionValuesResponse [DimensionValuesWithAttributes]
getDimensionValuesResponse_dimensionValues = Lens.lens (\GetDimensionValuesResponse' {dimensionValues} -> dimensionValues) (\s@GetDimensionValuesResponse' {} a -> s {dimensionValues = a} :: GetDimensionValuesResponse) Prelude.. Lens.coerced

-- | The number of results that Amazon Web Services returned at one time.
getDimensionValuesResponse_returnSize :: Lens.Lens' GetDimensionValuesResponse Prelude.Int
getDimensionValuesResponse_returnSize = Lens.lens (\GetDimensionValuesResponse' {returnSize} -> returnSize) (\s@GetDimensionValuesResponse' {} a -> s {returnSize = a} :: GetDimensionValuesResponse)

-- | The total number of search results.
getDimensionValuesResponse_totalSize :: Lens.Lens' GetDimensionValuesResponse Prelude.Int
getDimensionValuesResponse_totalSize = Lens.lens (\GetDimensionValuesResponse' {totalSize} -> totalSize) (\s@GetDimensionValuesResponse' {} a -> s {totalSize = a} :: GetDimensionValuesResponse)

instance Prelude.NFData GetDimensionValuesResponse where
  rnf GetDimensionValuesResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf dimensionValues
      `Prelude.seq` Prelude.rnf returnSize
      `Prelude.seq` Prelude.rnf totalSize
