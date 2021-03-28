{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetDimensionValues (..)
    , mkGetDimensionValues
    -- ** Request lenses
    , gdvTimePeriod
    , gdvDimension
    , gdvContext
    , gdvNextPageToken
    , gdvSearchString

    -- * Destructuring the response
    , GetDimensionValuesResponse (..)
    , mkGetDimensionValuesResponse
    -- ** Response lenses
    , gdvrrsDimensionValues
    , gdvrrsReturnSize
    , gdvrrsTotalSize
    , gdvrrsNextPageToken
    , gdvrrsResponseStatus
    ) where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDimensionValues' smart constructor.
data GetDimensionValues = GetDimensionValues'
  { timePeriod :: Types.DateInterval
    -- ^ The start and end dates for retrieving the dimension values. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
  , dimension :: Types.Dimension
    -- ^ The name of the dimension. Each @Dimension@ is available for a different @Context@ . For more information, see @Context@ . 
  , context :: Core.Maybe Types.Context
    -- ^ The context for the call to @GetDimensionValues@ . This can be @RESERVATIONS@ or @COST_AND_USAGE@ . The default value is @COST_AND_USAGE@ . If the context is set to @RESERVATIONS@ , the resulting dimension values can be used in the @GetReservationUtilization@ operation. If the context is set to @COST_AND_USAGE@ , the resulting dimension values can be used in the @GetCostAndUsage@ operation.
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
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
  , searchString :: Core.Maybe Types.SearchString
    -- ^ The value that you want to search the filter values for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDimensionValues' value with any optional fields omitted.
mkGetDimensionValues
    :: Types.DateInterval -- ^ 'timePeriod'
    -> Types.Dimension -- ^ 'dimension'
    -> GetDimensionValues
mkGetDimensionValues timePeriod dimension
  = GetDimensionValues'{timePeriod, dimension,
                        context = Core.Nothing, nextPageToken = Core.Nothing,
                        searchString = Core.Nothing}

-- | The start and end dates for retrieving the dimension values. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvTimePeriod :: Lens.Lens' GetDimensionValues Types.DateInterval
gdvTimePeriod = Lens.field @"timePeriod"
{-# INLINEABLE gdvTimePeriod #-}
{-# DEPRECATED timePeriod "Use generic-lens or generic-optics with 'timePeriod' instead"  #-}

-- | The name of the dimension. Each @Dimension@ is available for a different @Context@ . For more information, see @Context@ . 
--
-- /Note:/ Consider using 'dimension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvDimension :: Lens.Lens' GetDimensionValues Types.Dimension
gdvDimension = Lens.field @"dimension"
{-# INLINEABLE gdvDimension #-}
{-# DEPRECATED dimension "Use generic-lens or generic-optics with 'dimension' instead"  #-}

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
gdvContext :: Lens.Lens' GetDimensionValues (Core.Maybe Types.Context)
gdvContext = Lens.field @"context"
{-# INLINEABLE gdvContext #-}
{-# DEPRECATED context "Use generic-lens or generic-optics with 'context' instead"  #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvNextPageToken :: Lens.Lens' GetDimensionValues (Core.Maybe Types.NextPageToken)
gdvNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gdvNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The value that you want to search the filter values for.
--
-- /Note:/ Consider using 'searchString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvSearchString :: Lens.Lens' GetDimensionValues (Core.Maybe Types.SearchString)
gdvSearchString = Lens.field @"searchString"
{-# INLINEABLE gdvSearchString #-}
{-# DEPRECATED searchString "Use generic-lens or generic-optics with 'searchString' instead"  #-}

instance Core.ToQuery GetDimensionValues where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDimensionValues where
        toHeaders GetDimensionValues{..}
          = Core.pure
              ("X-Amz-Target", "AWSInsightsIndexService.GetDimensionValues")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDimensionValues where
        toJSON GetDimensionValues{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TimePeriod" Core..= timePeriod),
                  Core.Just ("Dimension" Core..= dimension),
                  ("Context" Core..=) Core.<$> context,
                  ("NextPageToken" Core..=) Core.<$> nextPageToken,
                  ("SearchString" Core..=) Core.<$> searchString])

instance Core.AWSRequest GetDimensionValues where
        type Rs GetDimensionValues = GetDimensionValuesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDimensionValuesResponse' Core.<$>
                   (x Core..:? "DimensionValues" Core..!= Core.mempty) Core.<*>
                     x Core..: "ReturnSize"
                     Core.<*> x Core..: "TotalSize"
                     Core.<*> x Core..:? "NextPageToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDimensionValuesResponse' smart constructor.
data GetDimensionValuesResponse = GetDimensionValuesResponse'
  { dimensionValues :: [Types.DimensionValuesWithAttributes]
    -- ^ The filters that you used to filter your request. Some dimensions are available only for a specific context.
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
  , returnSize :: Core.Int
    -- ^ The number of results that AWS returned at one time.
  , totalSize :: Core.Int
    -- ^ The total number of search results.
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDimensionValuesResponse' value with any optional fields omitted.
mkGetDimensionValuesResponse
    :: Core.Int -- ^ 'returnSize'
    -> Core.Int -- ^ 'totalSize'
    -> Core.Int -- ^ 'responseStatus'
    -> GetDimensionValuesResponse
mkGetDimensionValuesResponse returnSize totalSize responseStatus
  = GetDimensionValuesResponse'{dimensionValues = Core.mempty,
                                returnSize, totalSize, nextPageToken = Core.Nothing,
                                responseStatus}

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
gdvrrsDimensionValues :: Lens.Lens' GetDimensionValuesResponse [Types.DimensionValuesWithAttributes]
gdvrrsDimensionValues = Lens.field @"dimensionValues"
{-# INLINEABLE gdvrrsDimensionValues #-}
{-# DEPRECATED dimensionValues "Use generic-lens or generic-optics with 'dimensionValues' instead"  #-}

-- | The number of results that AWS returned at one time.
--
-- /Note:/ Consider using 'returnSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrrsReturnSize :: Lens.Lens' GetDimensionValuesResponse Core.Int
gdvrrsReturnSize = Lens.field @"returnSize"
{-# INLINEABLE gdvrrsReturnSize #-}
{-# DEPRECATED returnSize "Use generic-lens or generic-optics with 'returnSize' instead"  #-}

-- | The total number of search results.
--
-- /Note:/ Consider using 'totalSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrrsTotalSize :: Lens.Lens' GetDimensionValuesResponse Core.Int
gdvrrsTotalSize = Lens.field @"totalSize"
{-# INLINEABLE gdvrrsTotalSize #-}
{-# DEPRECATED totalSize "Use generic-lens or generic-optics with 'totalSize' instead"  #-}

-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrrsNextPageToken :: Lens.Lens' GetDimensionValuesResponse (Core.Maybe Types.NextPageToken)
gdvrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gdvrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrrsResponseStatus :: Lens.Lens' GetDimensionValuesResponse Core.Int
gdvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
