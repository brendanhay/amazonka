{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetDimensionValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all available filter values for a specified filter over a period of time. You can search the dimension values for an arbitrary string.
module Network.AWS.CostExplorer.GetDimensionValues
  ( -- * Creating a request
    GetDimensionValues (..),
    mkGetDimensionValues,

    -- ** Request lenses
    gdvNextPageToken,
    gdvContext,
    gdvSearchString,
    gdvTimePeriod,
    gdvDimension,

    -- * Destructuring the response
    GetDimensionValuesResponse (..),
    mkGetDimensionValuesResponse,

    -- ** Response lenses
    gdvrsNextPageToken,
    gdvrsResponseStatus,
    gdvrsDimensionValues,
    gdvrsReturnSize,
    gdvrsTotalSize,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDimensionValues' smart constructor.
data GetDimensionValues = GetDimensionValues'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    context :: Lude.Maybe Context,
    searchString :: Lude.Maybe Lude.Text,
    timePeriod :: DateInterval,
    dimension :: Dimension
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDimensionValues' with the minimum fields required to make a request.
--
-- * 'context' - The context for the call to @GetDimensionValues@ . This can be @RESERVATIONS@ or @COST_AND_USAGE@ . The default value is @COST_AND_USAGE@ . If the context is set to @RESERVATIONS@ , the resulting dimension values can be used in the @GetReservationUtilization@ operation. If the context is set to @COST_AND_USAGE@ , the resulting dimension values can be used in the @GetCostAndUsage@ operation.
--
-- If you set the context to @COST_AND_USAGE@ , you can use the following dimensions for searching:
--
--     * AZ - The Availability Zone. An example is @us-east-1a@ .
--
--
--     * DATABASE_ENGINE - The Amazon Relational Database Service database. Examples are Aurora or MySQL.
--
--
--     * INSTANCE_TYPE - The type of Amazon EC2 instance. An example is @m4.xlarge@ .
--
--
--     * LEGAL_ENTITY_NAME - The name of the organization that sells you AWS services, such as Amazon Web Services.
--
--
--     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.
--
--
--     * OPERATING_SYSTEM - The operating system. Examples are Windows or Linux.
--
--
--     * OPERATION - The action performed. Examples include @RunInstance@ and @CreateBucket@ .
--
--
--     * PLATFORM - The Amazon EC2 operating system. Examples are Windows or Linux.
--
--
--     * PURCHASE_TYPE - The reservation type of the purchase to which this usage is related. Examples include On-Demand Instances and Standard Reserved Instances.
--
--
--     * SERVICE - The AWS service such as Amazon DynamoDB.
--
--
--     * USAGE_TYPE - The type of usage. An example is DataTransfer-In-Bytes. The response for the @GetDimensionValues@ operation includes a unit attribute. Examples include GB and Hrs.
--
--
--     * USAGE_TYPE_GROUP - The grouping of common usage types. An example is Amazon EC2: CloudWatch – Alarms. The response for this operation includes a unit attribute.
--
--
--     * REGION - The AWS Region.
--
--
--     * RECORD_TYPE - The different types of charges such as RI fees, usage costs, tax refunds, and credits.
--
--
--     * RESOURCE_ID - The unique identifier of the resource. ResourceId is an opt-in feature only available for last 14 days for EC2-Compute Service.
--
--
-- If you set the context to @RESERVATIONS@ , you can use the following dimensions for searching:
--
--     * AZ - The Availability Zone. An example is @us-east-1a@ .
--
--
--     * CACHE_ENGINE - The Amazon ElastiCache operating system. Examples are Windows or Linux.
--
--
--     * DEPLOYMENT_OPTION - The scope of Amazon Relational Database Service deployments. Valid values are @SingleAZ@ and @MultiAZ@ .
--
--
--     * INSTANCE_TYPE - The type of Amazon EC2 instance. An example is @m4.xlarge@ .
--
--
--     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.
--
--
--     * PLATFORM - The Amazon EC2 operating system. Examples are Windows or Linux.
--
--
--     * REGION - The AWS Region.
--
--
--     * SCOPE (Utilization only) - The scope of a Reserved Instance (RI). Values are regional or a single Availability Zone.
--
--
--     * TAG (Coverage only) - The tags that are associated with a Reserved Instance (RI).
--
--
--     * TENANCY - The tenancy of a resource. Examples are shared or dedicated.
--
--
-- If you set the context to @SAVINGS_PLANS@ , you can use the following dimensions for searching:
--
--     * SAVINGS_PLANS_TYPE - Type of Savings Plans (EC2 Instance or Compute)
--
--
--     * PAYMENT_OPTION - Payment option for the given Savings Plans (for example, All Upfront)
--
--
--     * REGION - The AWS Region.
--
--
--     * INSTANCE_TYPE_FAMILY - The family of instances (For example, @m5@ )
--
--
--     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.
--
--
--     * SAVINGS_PLAN_ARN - The unique identifier for your Savings Plan
--
--
-- * 'dimension' - The name of the dimension. Each @Dimension@ is available for a different @Context@ . For more information, see @Context@ .
-- * 'nextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
-- * 'searchString' - The value that you want to search the filter values for.
-- * 'timePeriod' - The start and end dates for retrieving the dimension values. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
mkGetDimensionValues ::
  -- | 'timePeriod'
  DateInterval ->
  -- | 'dimension'
  Dimension ->
  GetDimensionValues
mkGetDimensionValues pTimePeriod_ pDimension_ =
  GetDimensionValues'
    { nextPageToken = Lude.Nothing,
      context = Lude.Nothing,
      searchString = Lude.Nothing,
      timePeriod = pTimePeriod_,
      dimension = pDimension_
    }

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvNextPageToken :: Lens.Lens' GetDimensionValues (Lude.Maybe Lude.Text)
gdvNextPageToken = Lens.lens (nextPageToken :: GetDimensionValues -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetDimensionValues)
{-# DEPRECATED gdvNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The context for the call to @GetDimensionValues@ . This can be @RESERVATIONS@ or @COST_AND_USAGE@ . The default value is @COST_AND_USAGE@ . If the context is set to @RESERVATIONS@ , the resulting dimension values can be used in the @GetReservationUtilization@ operation. If the context is set to @COST_AND_USAGE@ , the resulting dimension values can be used in the @GetCostAndUsage@ operation.
--
-- If you set the context to @COST_AND_USAGE@ , you can use the following dimensions for searching:
--
--     * AZ - The Availability Zone. An example is @us-east-1a@ .
--
--
--     * DATABASE_ENGINE - The Amazon Relational Database Service database. Examples are Aurora or MySQL.
--
--
--     * INSTANCE_TYPE - The type of Amazon EC2 instance. An example is @m4.xlarge@ .
--
--
--     * LEGAL_ENTITY_NAME - The name of the organization that sells you AWS services, such as Amazon Web Services.
--
--
--     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.
--
--
--     * OPERATING_SYSTEM - The operating system. Examples are Windows or Linux.
--
--
--     * OPERATION - The action performed. Examples include @RunInstance@ and @CreateBucket@ .
--
--
--     * PLATFORM - The Amazon EC2 operating system. Examples are Windows or Linux.
--
--
--     * PURCHASE_TYPE - The reservation type of the purchase to which this usage is related. Examples include On-Demand Instances and Standard Reserved Instances.
--
--
--     * SERVICE - The AWS service such as Amazon DynamoDB.
--
--
--     * USAGE_TYPE - The type of usage. An example is DataTransfer-In-Bytes. The response for the @GetDimensionValues@ operation includes a unit attribute. Examples include GB and Hrs.
--
--
--     * USAGE_TYPE_GROUP - The grouping of common usage types. An example is Amazon EC2: CloudWatch – Alarms. The response for this operation includes a unit attribute.
--
--
--     * REGION - The AWS Region.
--
--
--     * RECORD_TYPE - The different types of charges such as RI fees, usage costs, tax refunds, and credits.
--
--
--     * RESOURCE_ID - The unique identifier of the resource. ResourceId is an opt-in feature only available for last 14 days for EC2-Compute Service.
--
--
-- If you set the context to @RESERVATIONS@ , you can use the following dimensions for searching:
--
--     * AZ - The Availability Zone. An example is @us-east-1a@ .
--
--
--     * CACHE_ENGINE - The Amazon ElastiCache operating system. Examples are Windows or Linux.
--
--
--     * DEPLOYMENT_OPTION - The scope of Amazon Relational Database Service deployments. Valid values are @SingleAZ@ and @MultiAZ@ .
--
--
--     * INSTANCE_TYPE - The type of Amazon EC2 instance. An example is @m4.xlarge@ .
--
--
--     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.
--
--
--     * PLATFORM - The Amazon EC2 operating system. Examples are Windows or Linux.
--
--
--     * REGION - The AWS Region.
--
--
--     * SCOPE (Utilization only) - The scope of a Reserved Instance (RI). Values are regional or a single Availability Zone.
--
--
--     * TAG (Coverage only) - The tags that are associated with a Reserved Instance (RI).
--
--
--     * TENANCY - The tenancy of a resource. Examples are shared or dedicated.
--
--
-- If you set the context to @SAVINGS_PLANS@ , you can use the following dimensions for searching:
--
--     * SAVINGS_PLANS_TYPE - Type of Savings Plans (EC2 Instance or Compute)
--
--
--     * PAYMENT_OPTION - Payment option for the given Savings Plans (for example, All Upfront)
--
--
--     * REGION - The AWS Region.
--
--
--     * INSTANCE_TYPE_FAMILY - The family of instances (For example, @m5@ )
--
--
--     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.
--
--
--     * SAVINGS_PLAN_ARN - The unique identifier for your Savings Plan
--
--
--
-- /Note:/ Consider using 'context' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvContext :: Lens.Lens' GetDimensionValues (Lude.Maybe Context)
gdvContext = Lens.lens (context :: GetDimensionValues -> Lude.Maybe Context) (\s a -> s {context = a} :: GetDimensionValues)
{-# DEPRECATED gdvContext "Use generic-lens or generic-optics with 'context' instead." #-}

-- | The value that you want to search the filter values for.
--
-- /Note:/ Consider using 'searchString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvSearchString :: Lens.Lens' GetDimensionValues (Lude.Maybe Lude.Text)
gdvSearchString = Lens.lens (searchString :: GetDimensionValues -> Lude.Maybe Lude.Text) (\s a -> s {searchString = a} :: GetDimensionValues)
{-# DEPRECATED gdvSearchString "Use generic-lens or generic-optics with 'searchString' instead." #-}

-- | The start and end dates for retrieving the dimension values. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvTimePeriod :: Lens.Lens' GetDimensionValues DateInterval
gdvTimePeriod = Lens.lens (timePeriod :: GetDimensionValues -> DateInterval) (\s a -> s {timePeriod = a} :: GetDimensionValues)
{-# DEPRECATED gdvTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | The name of the dimension. Each @Dimension@ is available for a different @Context@ . For more information, see @Context@ .
--
-- /Note:/ Consider using 'dimension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvDimension :: Lens.Lens' GetDimensionValues Dimension
gdvDimension = Lens.lens (dimension :: GetDimensionValues -> Dimension) (\s a -> s {dimension = a} :: GetDimensionValues)
{-# DEPRECATED gdvDimension "Use generic-lens or generic-optics with 'dimension' instead." #-}

instance Lude.AWSRequest GetDimensionValues where
  type Rs GetDimensionValues = GetDimensionValuesResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDimensionValuesResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "DimensionValues" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "ReturnSize")
            Lude.<*> (x Lude..:> "TotalSize")
      )

instance Lude.ToHeaders GetDimensionValues where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSInsightsIndexService.GetDimensionValues" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDimensionValues where
  toJSON GetDimensionValues' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextPageToken" Lude..=) Lude.<$> nextPageToken,
            ("Context" Lude..=) Lude.<$> context,
            ("SearchString" Lude..=) Lude.<$> searchString,
            Lude.Just ("TimePeriod" Lude..= timePeriod),
            Lude.Just ("Dimension" Lude..= dimension)
          ]
      )

instance Lude.ToPath GetDimensionValues where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDimensionValues where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDimensionValuesResponse' smart constructor.
data GetDimensionValuesResponse = GetDimensionValuesResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    dimensionValues ::
      [DimensionValuesWithAttributes],
    returnSize :: Lude.Int,
    totalSize :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDimensionValuesResponse' with the minimum fields required to make a request.
--
-- * 'dimensionValues' - The filters that you used to filter your request. Some dimensions are available only for a specific context.
--
-- If you set the context to @COST_AND_USAGE@ , you can use the following dimensions for searching:
--
--     * AZ - The Availability Zone. An example is @us-east-1a@ .
--
--
--     * DATABASE_ENGINE - The Amazon Relational Database Service database. Examples are Aurora or MySQL.
--
--
--     * INSTANCE_TYPE - The type of Amazon EC2 instance. An example is @m4.xlarge@ .
--
--
--     * LEGAL_ENTITY_NAME - The name of the organization that sells you AWS services, such as Amazon Web Services.
--
--
--     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.
--
--
--     * OPERATING_SYSTEM - The operating system. Examples are Windows or Linux.
--
--
--     * OPERATION - The action performed. Examples include @RunInstance@ and @CreateBucket@ .
--
--
--     * PLATFORM - The Amazon EC2 operating system. Examples are Windows or Linux.
--
--
--     * PURCHASE_TYPE - The reservation type of the purchase to which this usage is related. Examples include On-Demand Instances and Standard Reserved Instances.
--
--
--     * SERVICE - The AWS service such as Amazon DynamoDB.
--
--
--     * USAGE_TYPE - The type of usage. An example is DataTransfer-In-Bytes. The response for the @GetDimensionValues@ operation includes a unit attribute. Examples include GB and Hrs.
--
--
--     * USAGE_TYPE_GROUP - The grouping of common usage types. An example is Amazon EC2: CloudWatch – Alarms. The response for this operation includes a unit attribute.
--
--
--     * RECORD_TYPE - The different types of charges such as RI fees, usage costs, tax refunds, and credits.
--
--
--     * RESOURCE_ID - The unique identifier of the resource. ResourceId is an opt-in feature only available for last 14 days for EC2-Compute Service.
--
--
-- If you set the context to @RESERVATIONS@ , you can use the following dimensions for searching:
--
--     * AZ - The Availability Zone. An example is @us-east-1a@ .
--
--
--     * CACHE_ENGINE - The Amazon ElastiCache operating system. Examples are Windows or Linux.
--
--
--     * DEPLOYMENT_OPTION - The scope of Amazon Relational Database Service deployments. Valid values are @SingleAZ@ and @MultiAZ@ .
--
--
--     * INSTANCE_TYPE - The type of Amazon EC2 instance. An example is @m4.xlarge@ .
--
--
--     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.
--
--
--     * PLATFORM - The Amazon EC2 operating system. Examples are Windows or Linux.
--
--
--     * REGION - The AWS Region.
--
--
--     * SCOPE (Utilization only) - The scope of a Reserved Instance (RI). Values are regional or a single Availability Zone.
--
--
--     * TAG (Coverage only) - The tags that are associated with a Reserved Instance (RI).
--
--
--     * TENANCY - The tenancy of a resource. Examples are shared or dedicated.
--
--
-- If you set the context to @SAVINGS_PLANS@ , you can use the following dimensions for searching:
--
--     * SAVINGS_PLANS_TYPE - Type of Savings Plans (EC2 Instance or Compute)
--
--
--     * PAYMENT_OPTION - Payment option for the given Savings Plans (for example, All Upfront)
--
--
--     * REGION - The AWS Region.
--
--
--     * INSTANCE_TYPE_FAMILY - The family of instances (For example, @m5@ )
--
--
--     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.
--
--
--     * SAVINGS_PLAN_ARN - The unique identifier for your Savings Plan
--
--
-- * 'nextPageToken' - The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
-- * 'responseStatus' - The response status code.
-- * 'returnSize' - The number of results that AWS returned at one time.
-- * 'totalSize' - The total number of search results.
mkGetDimensionValuesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'returnSize'
  Lude.Int ->
  -- | 'totalSize'
  Lude.Int ->
  GetDimensionValuesResponse
mkGetDimensionValuesResponse
  pResponseStatus_
  pReturnSize_
  pTotalSize_ =
    GetDimensionValuesResponse'
      { nextPageToken = Lude.Nothing,
        responseStatus = pResponseStatus_,
        dimensionValues = Lude.mempty,
        returnSize = pReturnSize_,
        totalSize = pTotalSize_
      }

-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrsNextPageToken :: Lens.Lens' GetDimensionValuesResponse (Lude.Maybe Lude.Text)
gdvrsNextPageToken = Lens.lens (nextPageToken :: GetDimensionValuesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetDimensionValuesResponse)
{-# DEPRECATED gdvrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrsResponseStatus :: Lens.Lens' GetDimensionValuesResponse Lude.Int
gdvrsResponseStatus = Lens.lens (responseStatus :: GetDimensionValuesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDimensionValuesResponse)
{-# DEPRECATED gdvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The filters that you used to filter your request. Some dimensions are available only for a specific context.
--
-- If you set the context to @COST_AND_USAGE@ , you can use the following dimensions for searching:
--
--     * AZ - The Availability Zone. An example is @us-east-1a@ .
--
--
--     * DATABASE_ENGINE - The Amazon Relational Database Service database. Examples are Aurora or MySQL.
--
--
--     * INSTANCE_TYPE - The type of Amazon EC2 instance. An example is @m4.xlarge@ .
--
--
--     * LEGAL_ENTITY_NAME - The name of the organization that sells you AWS services, such as Amazon Web Services.
--
--
--     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.
--
--
--     * OPERATING_SYSTEM - The operating system. Examples are Windows or Linux.
--
--
--     * OPERATION - The action performed. Examples include @RunInstance@ and @CreateBucket@ .
--
--
--     * PLATFORM - The Amazon EC2 operating system. Examples are Windows or Linux.
--
--
--     * PURCHASE_TYPE - The reservation type of the purchase to which this usage is related. Examples include On-Demand Instances and Standard Reserved Instances.
--
--
--     * SERVICE - The AWS service such as Amazon DynamoDB.
--
--
--     * USAGE_TYPE - The type of usage. An example is DataTransfer-In-Bytes. The response for the @GetDimensionValues@ operation includes a unit attribute. Examples include GB and Hrs.
--
--
--     * USAGE_TYPE_GROUP - The grouping of common usage types. An example is Amazon EC2: CloudWatch – Alarms. The response for this operation includes a unit attribute.
--
--
--     * RECORD_TYPE - The different types of charges such as RI fees, usage costs, tax refunds, and credits.
--
--
--     * RESOURCE_ID - The unique identifier of the resource. ResourceId is an opt-in feature only available for last 14 days for EC2-Compute Service.
--
--
-- If you set the context to @RESERVATIONS@ , you can use the following dimensions for searching:
--
--     * AZ - The Availability Zone. An example is @us-east-1a@ .
--
--
--     * CACHE_ENGINE - The Amazon ElastiCache operating system. Examples are Windows or Linux.
--
--
--     * DEPLOYMENT_OPTION - The scope of Amazon Relational Database Service deployments. Valid values are @SingleAZ@ and @MultiAZ@ .
--
--
--     * INSTANCE_TYPE - The type of Amazon EC2 instance. An example is @m4.xlarge@ .
--
--
--     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.
--
--
--     * PLATFORM - The Amazon EC2 operating system. Examples are Windows or Linux.
--
--
--     * REGION - The AWS Region.
--
--
--     * SCOPE (Utilization only) - The scope of a Reserved Instance (RI). Values are regional or a single Availability Zone.
--
--
--     * TAG (Coverage only) - The tags that are associated with a Reserved Instance (RI).
--
--
--     * TENANCY - The tenancy of a resource. Examples are shared or dedicated.
--
--
-- If you set the context to @SAVINGS_PLANS@ , you can use the following dimensions for searching:
--
--     * SAVINGS_PLANS_TYPE - Type of Savings Plans (EC2 Instance or Compute)
--
--
--     * PAYMENT_OPTION - Payment option for the given Savings Plans (for example, All Upfront)
--
--
--     * REGION - The AWS Region.
--
--
--     * INSTANCE_TYPE_FAMILY - The family of instances (For example, @m5@ )
--
--
--     * LINKED_ACCOUNT - The description in the attribute map that includes the full name of the member account. The value field contains the AWS ID of the member account.
--
--
--     * SAVINGS_PLAN_ARN - The unique identifier for your Savings Plan
--
--
--
-- /Note:/ Consider using 'dimensionValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrsDimensionValues :: Lens.Lens' GetDimensionValuesResponse [DimensionValuesWithAttributes]
gdvrsDimensionValues = Lens.lens (dimensionValues :: GetDimensionValuesResponse -> [DimensionValuesWithAttributes]) (\s a -> s {dimensionValues = a} :: GetDimensionValuesResponse)
{-# DEPRECATED gdvrsDimensionValues "Use generic-lens or generic-optics with 'dimensionValues' instead." #-}

-- | The number of results that AWS returned at one time.
--
-- /Note:/ Consider using 'returnSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrsReturnSize :: Lens.Lens' GetDimensionValuesResponse Lude.Int
gdvrsReturnSize = Lens.lens (returnSize :: GetDimensionValuesResponse -> Lude.Int) (\s a -> s {returnSize = a} :: GetDimensionValuesResponse)
{-# DEPRECATED gdvrsReturnSize "Use generic-lens or generic-optics with 'returnSize' instead." #-}

-- | The total number of search results.
--
-- /Note:/ Consider using 'totalSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrsTotalSize :: Lens.Lens' GetDimensionValuesResponse Lude.Int
gdvrsTotalSize = Lens.lens (totalSize :: GetDimensionValuesResponse -> Lude.Int) (\s a -> s {totalSize = a} :: GetDimensionValuesResponse)
{-# DEPRECATED gdvrsTotalSize "Use generic-lens or generic-optics with 'totalSize' instead." #-}
