{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceAnalytics.GenerateDataSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given a data set type and data set publication date, asynchronously publishes the requested data set to the specified S3 bucket and notifies the specified SNS topic once the data is available. Returns a unique request identifier that can be used to correlate requests with notifications from the SNS topic. Data sets will be published in comma-separated values (CSV) format with the file name {data_set_type}_YYYY-MM-DD.csv. If a file with the same name already exists (e.g. if the same data set is requested twice), the original file will be overwritten by the new file. Requires a Role with an attached permissions policy providing Allow permissions for the following actions: s3:PutObject, s3:GetBucketLocation, sns:GetTopicAttributes, sns:Publish, iam:GetRolePolicy.
module Network.AWS.MarketplaceAnalytics.GenerateDataSet
    (
    -- * Creating a request
      GenerateDataSet (..)
    , mkGenerateDataSet
    -- ** Request lenses
    , gdsDataSetType
    , gdsDataSetPublicationDate
    , gdsRoleNameArn
    , gdsDestinationS3BucketName
    , gdsSnsTopicArn
    , gdsCustomerDefinedValues
    , gdsDestinationS3Prefix

    -- * Destructuring the response
    , GenerateDataSetResponse (..)
    , mkGenerateDataSetResponse
    -- ** Response lenses
    , gdsrrsDataSetRequestId
    , gdsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MarketplaceAnalytics.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the GenerateDataSet operation.
--
-- /See:/ 'mkGenerateDataSet' smart constructor.
data GenerateDataSet = GenerateDataSet'
  { dataSetType :: Types.DataSetType
    -- ^ The desired data set type.
--
--
--     * __customer_subscriber_hourly_monthly_subscriptions__ From 2017-09-15 to present: Available daily by 24:00 UTC.
--
--
--     * __customer_subscriber_annual_subscriptions__ From 2017-09-15 to present: Available daily by 24:00 UTC.
--
--
--     * __daily_business_usage_by_instance_type__ From 2017-09-15 to present: Available daily by 24:00 UTC.
--
--
--     * __daily_business_fees__ From 2017-09-15 to present: Available daily by 24:00 UTC.
--
--
--     * __daily_business_free_trial_conversions__ From 2017-09-15 to present: Available daily by 24:00 UTC.
--
--
--     * __daily_business_new_instances__ From 2017-09-15 to present: Available daily by 24:00 UTC.
--
--
--     * __daily_business_new_product_subscribers__ From 2017-09-15 to present: Available daily by 24:00 UTC.
--
--
--     * __daily_business_canceled_product_subscribers__ From 2017-09-15 to present: Available daily by 24:00 UTC.
--
--
--     * __monthly_revenue_billing_and_revenue_data__ From 2017-09-15 to present: Available monthly on the 15th day of the month by 24:00 UTC. Data includes metered transactions (e.g. hourly) from one month prior.
--
--
--     * __monthly_revenue_annual_subscriptions__ From 2017-09-15 to present: Available monthly on the 15th day of the month by 24:00 UTC. Data includes up-front software charges (e.g. annual) from one month prior.
--
--
--     * __monthly_revenue_field_demonstration_usage__ From 2018-03-15 to present: Available monthly on the 15th day of the month by 24:00 UTC.
--
--
--     * __monthly_revenue_flexible_payment_schedule__ From 2018-11-15 to present: Available monthly on the 15th day of the month by 24:00 UTC.
--
--
--     * __disbursed_amount_by_product__ From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
--
--     * __disbursed_amount_by_instance_hours__ From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
--
--     * __disbursed_amount_by_customer_geo__ From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
--
--     * __disbursed_amount_by_age_of_uncollected_funds__ From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
--
--     * __disbursed_amount_by_age_of_disbursed_funds__ From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
--
--     * __disbursed_amount_by_age_of_past_due_funds__ From 2018-04-07 to present: Available every 30 days by 24:00 UTC.
--
--
--     * __disbursed_amount_by_uncollected_funds_breakdown__ From 2019-10-04 to present: Available every 30 days by 24:00 UTC.
--
--
--     * __sales_compensation_billed_revenue__ From 2017-09-15 to present: Available monthly on the 15th day of the month by 24:00 UTC. Data includes metered transactions (e.g. hourly) from one month prior, and up-front software charges (e.g. annual) from one month prior.
--
--
--     * __us_sales_and_use_tax_records__ From 2017-09-15 to present: Available monthly on the 15th day of the month by 24:00 UTC.
--
--
--     * __disbursed_amount_by_product_with_uncollected_funds__ This data set is deprecated. Download related reports from AMMP instead!
--
--
--     * __customer_profile_by_industry__ This data set is deprecated. Download related reports from AMMP instead!
--
--
--     * __customer_profile_by_revenue__ This data set is deprecated. Download related reports from AMMP instead!
--
--
--     * __customer_profile_by_geography__ This data set is deprecated. Download related reports from AMMP instead!
--
--
--
  , dataSetPublicationDate :: Core.NominalDiffTime
    -- ^ The date a data set was published. For daily data sets, provide a date with day-level granularity for the desired day. For monthly data sets except those with prefix disbursed_amount, provide a date with month-level granularity for the desired month (the day value will be ignored). For data sets with prefix disbursed_amount, provide a date with day-level granularity for the desired day. For these data sets we will look backwards in time over the range of 31 days until the first data set is found (the latest one).
  , roleNameArn :: Types.RoleNameArn
    -- ^ The Amazon Resource Name (ARN) of the Role with an attached permissions policy to interact with the provided AWS services.
  , destinationS3BucketName :: Types.DestinationS3BucketName
    -- ^ The name (friendly name, not ARN) of the destination S3 bucket.
  , snsTopicArn :: Types.SnsTopicArn
    -- ^ Amazon Resource Name (ARN) for the SNS Topic that will be notified when the data set has been published or if an error has occurred.
  , customerDefinedValues :: Core.Maybe (Core.HashMap Types.OptionalKey Types.OptionalValue)
    -- ^ (Optional) Key-value pairs which will be returned, unmodified, in the Amazon SNS notification message and the data set metadata file. These key-value pairs can be used to correlated responses with tracking information from other systems.
  , destinationS3Prefix :: Core.Maybe Types.DestinationS3Prefix
    -- ^ (Optional) The desired S3 prefix for the published data set, similar to a directory path in standard file systems. For example, if given the bucket name "mybucket" and the prefix "myprefix/mydatasets", the output file "outputfile" would be published to "s3://mybucket/myprefix/mydatasets/outputfile". If the prefix directory structure does not exist, it will be created. If no prefix is provided, the data set will be published to the S3 bucket root.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GenerateDataSet' value with any optional fields omitted.
mkGenerateDataSet
    :: Types.DataSetType -- ^ 'dataSetType'
    -> Core.NominalDiffTime -- ^ 'dataSetPublicationDate'
    -> Types.RoleNameArn -- ^ 'roleNameArn'
    -> Types.DestinationS3BucketName -- ^ 'destinationS3BucketName'
    -> Types.SnsTopicArn -- ^ 'snsTopicArn'
    -> GenerateDataSet
mkGenerateDataSet dataSetType dataSetPublicationDate roleNameArn
  destinationS3BucketName snsTopicArn
  = GenerateDataSet'{dataSetType, dataSetPublicationDate,
                     roleNameArn, destinationS3BucketName, snsTopicArn,
                     customerDefinedValues = Core.Nothing,
                     destinationS3Prefix = Core.Nothing}

-- | The desired data set type.
--
--
--     * __customer_subscriber_hourly_monthly_subscriptions__ From 2017-09-15 to present: Available daily by 24:00 UTC.
--
--
--     * __customer_subscriber_annual_subscriptions__ From 2017-09-15 to present: Available daily by 24:00 UTC.
--
--
--     * __daily_business_usage_by_instance_type__ From 2017-09-15 to present: Available daily by 24:00 UTC.
--
--
--     * __daily_business_fees__ From 2017-09-15 to present: Available daily by 24:00 UTC.
--
--
--     * __daily_business_free_trial_conversions__ From 2017-09-15 to present: Available daily by 24:00 UTC.
--
--
--     * __daily_business_new_instances__ From 2017-09-15 to present: Available daily by 24:00 UTC.
--
--
--     * __daily_business_new_product_subscribers__ From 2017-09-15 to present: Available daily by 24:00 UTC.
--
--
--     * __daily_business_canceled_product_subscribers__ From 2017-09-15 to present: Available daily by 24:00 UTC.
--
--
--     * __monthly_revenue_billing_and_revenue_data__ From 2017-09-15 to present: Available monthly on the 15th day of the month by 24:00 UTC. Data includes metered transactions (e.g. hourly) from one month prior.
--
--
--     * __monthly_revenue_annual_subscriptions__ From 2017-09-15 to present: Available monthly on the 15th day of the month by 24:00 UTC. Data includes up-front software charges (e.g. annual) from one month prior.
--
--
--     * __monthly_revenue_field_demonstration_usage__ From 2018-03-15 to present: Available monthly on the 15th day of the month by 24:00 UTC.
--
--
--     * __monthly_revenue_flexible_payment_schedule__ From 2018-11-15 to present: Available monthly on the 15th day of the month by 24:00 UTC.
--
--
--     * __disbursed_amount_by_product__ From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
--
--     * __disbursed_amount_by_instance_hours__ From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
--
--     * __disbursed_amount_by_customer_geo__ From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
--
--     * __disbursed_amount_by_age_of_uncollected_funds__ From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
--
--     * __disbursed_amount_by_age_of_disbursed_funds__ From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
--
--     * __disbursed_amount_by_age_of_past_due_funds__ From 2018-04-07 to present: Available every 30 days by 24:00 UTC.
--
--
--     * __disbursed_amount_by_uncollected_funds_breakdown__ From 2019-10-04 to present: Available every 30 days by 24:00 UTC.
--
--
--     * __sales_compensation_billed_revenue__ From 2017-09-15 to present: Available monthly on the 15th day of the month by 24:00 UTC. Data includes metered transactions (e.g. hourly) from one month prior, and up-front software charges (e.g. annual) from one month prior.
--
--
--     * __us_sales_and_use_tax_records__ From 2017-09-15 to present: Available monthly on the 15th day of the month by 24:00 UTC.
--
--
--     * __disbursed_amount_by_product_with_uncollected_funds__ This data set is deprecated. Download related reports from AMMP instead!
--
--
--     * __customer_profile_by_industry__ This data set is deprecated. Download related reports from AMMP instead!
--
--
--     * __customer_profile_by_revenue__ This data set is deprecated. Download related reports from AMMP instead!
--
--
--     * __customer_profile_by_geography__ This data set is deprecated. Download related reports from AMMP instead!
--
--
--
--
-- /Note:/ Consider using 'dataSetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsDataSetType :: Lens.Lens' GenerateDataSet Types.DataSetType
gdsDataSetType = Lens.field @"dataSetType"
{-# INLINEABLE gdsDataSetType #-}
{-# DEPRECATED dataSetType "Use generic-lens or generic-optics with 'dataSetType' instead"  #-}

-- | The date a data set was published. For daily data sets, provide a date with day-level granularity for the desired day. For monthly data sets except those with prefix disbursed_amount, provide a date with month-level granularity for the desired month (the day value will be ignored). For data sets with prefix disbursed_amount, provide a date with day-level granularity for the desired day. For these data sets we will look backwards in time over the range of 31 days until the first data set is found (the latest one).
--
-- /Note:/ Consider using 'dataSetPublicationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsDataSetPublicationDate :: Lens.Lens' GenerateDataSet Core.NominalDiffTime
gdsDataSetPublicationDate = Lens.field @"dataSetPublicationDate"
{-# INLINEABLE gdsDataSetPublicationDate #-}
{-# DEPRECATED dataSetPublicationDate "Use generic-lens or generic-optics with 'dataSetPublicationDate' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Role with an attached permissions policy to interact with the provided AWS services.
--
-- /Note:/ Consider using 'roleNameArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsRoleNameArn :: Lens.Lens' GenerateDataSet Types.RoleNameArn
gdsRoleNameArn = Lens.field @"roleNameArn"
{-# INLINEABLE gdsRoleNameArn #-}
{-# DEPRECATED roleNameArn "Use generic-lens or generic-optics with 'roleNameArn' instead"  #-}

-- | The name (friendly name, not ARN) of the destination S3 bucket.
--
-- /Note:/ Consider using 'destinationS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsDestinationS3BucketName :: Lens.Lens' GenerateDataSet Types.DestinationS3BucketName
gdsDestinationS3BucketName = Lens.field @"destinationS3BucketName"
{-# INLINEABLE gdsDestinationS3BucketName #-}
{-# DEPRECATED destinationS3BucketName "Use generic-lens or generic-optics with 'destinationS3BucketName' instead"  #-}

-- | Amazon Resource Name (ARN) for the SNS Topic that will be notified when the data set has been published or if an error has occurred.
--
-- /Note:/ Consider using 'snsTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsSnsTopicArn :: Lens.Lens' GenerateDataSet Types.SnsTopicArn
gdsSnsTopicArn = Lens.field @"snsTopicArn"
{-# INLINEABLE gdsSnsTopicArn #-}
{-# DEPRECATED snsTopicArn "Use generic-lens or generic-optics with 'snsTopicArn' instead"  #-}

-- | (Optional) Key-value pairs which will be returned, unmodified, in the Amazon SNS notification message and the data set metadata file. These key-value pairs can be used to correlated responses with tracking information from other systems.
--
-- /Note:/ Consider using 'customerDefinedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsCustomerDefinedValues :: Lens.Lens' GenerateDataSet (Core.Maybe (Core.HashMap Types.OptionalKey Types.OptionalValue))
gdsCustomerDefinedValues = Lens.field @"customerDefinedValues"
{-# INLINEABLE gdsCustomerDefinedValues #-}
{-# DEPRECATED customerDefinedValues "Use generic-lens or generic-optics with 'customerDefinedValues' instead"  #-}

-- | (Optional) The desired S3 prefix for the published data set, similar to a directory path in standard file systems. For example, if given the bucket name "mybucket" and the prefix "myprefix/mydatasets", the output file "outputfile" would be published to "s3://mybucket/myprefix/mydatasets/outputfile". If the prefix directory structure does not exist, it will be created. If no prefix is provided, the data set will be published to the S3 bucket root.
--
-- /Note:/ Consider using 'destinationS3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsDestinationS3Prefix :: Lens.Lens' GenerateDataSet (Core.Maybe Types.DestinationS3Prefix)
gdsDestinationS3Prefix = Lens.field @"destinationS3Prefix"
{-# INLINEABLE gdsDestinationS3Prefix #-}
{-# DEPRECATED destinationS3Prefix "Use generic-lens or generic-optics with 'destinationS3Prefix' instead"  #-}

instance Core.ToQuery GenerateDataSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GenerateDataSet where
        toHeaders GenerateDataSet{..}
          = Core.pure
              ("X-Amz-Target",
               "MarketplaceCommerceAnalytics20150701.GenerateDataSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GenerateDataSet where
        toJSON GenerateDataSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("dataSetType" Core..= dataSetType),
                  Core.Just
                    ("dataSetPublicationDate" Core..= dataSetPublicationDate),
                  Core.Just ("roleNameArn" Core..= roleNameArn),
                  Core.Just
                    ("destinationS3BucketName" Core..= destinationS3BucketName),
                  Core.Just ("snsTopicArn" Core..= snsTopicArn),
                  ("customerDefinedValues" Core..=) Core.<$> customerDefinedValues,
                  ("destinationS3Prefix" Core..=) Core.<$> destinationS3Prefix])

instance Core.AWSRequest GenerateDataSet where
        type Rs GenerateDataSet = GenerateDataSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GenerateDataSetResponse' Core.<$>
                   (x Core..:? "dataSetRequestId") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Container for the result of the GenerateDataSet operation.
--
-- /See:/ 'mkGenerateDataSetResponse' smart constructor.
data GenerateDataSetResponse = GenerateDataSetResponse'
  { dataSetRequestId :: Core.Maybe Types.DataSetRequestId
    -- ^ A unique identifier representing a specific request to the GenerateDataSet operation. This identifier can be used to correlate a request with notifications from the SNS topic.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateDataSetResponse' value with any optional fields omitted.
mkGenerateDataSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GenerateDataSetResponse
mkGenerateDataSetResponse responseStatus
  = GenerateDataSetResponse'{dataSetRequestId = Core.Nothing,
                             responseStatus}

-- | A unique identifier representing a specific request to the GenerateDataSet operation. This identifier can be used to correlate a request with notifications from the SNS topic.
--
-- /Note:/ Consider using 'dataSetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsDataSetRequestId :: Lens.Lens' GenerateDataSetResponse (Core.Maybe Types.DataSetRequestId)
gdsrrsDataSetRequestId = Lens.field @"dataSetRequestId"
{-# INLINEABLE gdsrrsDataSetRequestId #-}
{-# DEPRECATED dataSetRequestId "Use generic-lens or generic-optics with 'dataSetRequestId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsResponseStatus :: Lens.Lens' GenerateDataSetResponse Core.Int
gdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
