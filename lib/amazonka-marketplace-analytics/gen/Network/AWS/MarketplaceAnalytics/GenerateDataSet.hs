{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GenerateDataSet (..),
    mkGenerateDataSet,

    -- ** Request lenses
    gdsCustomerDefinedValues,
    gdsDestinationS3Prefix,
    gdsDataSetType,
    gdsDataSetPublicationDate,
    gdsRoleNameARN,
    gdsDestinationS3BucketName,
    gdsSnsTopicARN,

    -- * Destructuring the response
    GenerateDataSetResponse (..),
    mkGenerateDataSetResponse,

    -- ** Response lenses
    gdsrsDataSetRequestId,
    gdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceAnalytics.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the GenerateDataSet operation.
--
-- /See:/ 'mkGenerateDataSet' smart constructor.
data GenerateDataSet = GenerateDataSet'
  { customerDefinedValues ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    destinationS3Prefix :: Lude.Maybe Lude.Text,
    dataSetType :: DataSetType,
    dataSetPublicationDate :: Lude.Timestamp,
    roleNameARN :: Lude.Text,
    destinationS3BucketName :: Lude.Text,
    snsTopicARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateDataSet' with the minimum fields required to make a request.
--
-- * 'customerDefinedValues' - (Optional) Key-value pairs which will be returned, unmodified, in the Amazon SNS notification message and the data set metadata file. These key-value pairs can be used to correlated responses with tracking information from other systems.
-- * 'dataSetPublicationDate' - The date a data set was published. For daily data sets, provide a date with day-level granularity for the desired day. For monthly data sets except those with prefix disbursed_amount, provide a date with month-level granularity for the desired month (the day value will be ignored). For data sets with prefix disbursed_amount, provide a date with day-level granularity for the desired day. For these data sets we will look backwards in time over the range of 31 days until the first data set is found (the latest one).
-- * 'dataSetType' - The desired data set type.
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
-- * 'destinationS3BucketName' - The name (friendly name, not ARN) of the destination S3 bucket.
-- * 'destinationS3Prefix' - (Optional) The desired S3 prefix for the published data set, similar to a directory path in standard file systems. For example, if given the bucket name "mybucket" and the prefix "myprefix/mydatasets", the output file "outputfile" would be published to "s3://mybucket/myprefix/mydatasets/outputfile". If the prefix directory structure does not exist, it will be created. If no prefix is provided, the data set will be published to the S3 bucket root.
-- * 'roleNameARN' - The Amazon Resource Name (ARN) of the Role with an attached permissions policy to interact with the provided AWS services.
-- * 'snsTopicARN' - Amazon Resource Name (ARN) for the SNS Topic that will be notified when the data set has been published or if an error has occurred.
mkGenerateDataSet ::
  -- | 'dataSetType'
  DataSetType ->
  -- | 'dataSetPublicationDate'
  Lude.Timestamp ->
  -- | 'roleNameARN'
  Lude.Text ->
  -- | 'destinationS3BucketName'
  Lude.Text ->
  -- | 'snsTopicARN'
  Lude.Text ->
  GenerateDataSet
mkGenerateDataSet
  pDataSetType_
  pDataSetPublicationDate_
  pRoleNameARN_
  pDestinationS3BucketName_
  pSnsTopicARN_ =
    GenerateDataSet'
      { customerDefinedValues = Lude.Nothing,
        destinationS3Prefix = Lude.Nothing,
        dataSetType = pDataSetType_,
        dataSetPublicationDate = pDataSetPublicationDate_,
        roleNameARN = pRoleNameARN_,
        destinationS3BucketName = pDestinationS3BucketName_,
        snsTopicARN = pSnsTopicARN_
      }

-- | (Optional) Key-value pairs which will be returned, unmodified, in the Amazon SNS notification message and the data set metadata file. These key-value pairs can be used to correlated responses with tracking information from other systems.
--
-- /Note:/ Consider using 'customerDefinedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsCustomerDefinedValues :: Lens.Lens' GenerateDataSet (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gdsCustomerDefinedValues = Lens.lens (customerDefinedValues :: GenerateDataSet -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {customerDefinedValues = a} :: GenerateDataSet)
{-# DEPRECATED gdsCustomerDefinedValues "Use generic-lens or generic-optics with 'customerDefinedValues' instead." #-}

-- | (Optional) The desired S3 prefix for the published data set, similar to a directory path in standard file systems. For example, if given the bucket name "mybucket" and the prefix "myprefix/mydatasets", the output file "outputfile" would be published to "s3://mybucket/myprefix/mydatasets/outputfile". If the prefix directory structure does not exist, it will be created. If no prefix is provided, the data set will be published to the S3 bucket root.
--
-- /Note:/ Consider using 'destinationS3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsDestinationS3Prefix :: Lens.Lens' GenerateDataSet (Lude.Maybe Lude.Text)
gdsDestinationS3Prefix = Lens.lens (destinationS3Prefix :: GenerateDataSet -> Lude.Maybe Lude.Text) (\s a -> s {destinationS3Prefix = a} :: GenerateDataSet)
{-# DEPRECATED gdsDestinationS3Prefix "Use generic-lens or generic-optics with 'destinationS3Prefix' instead." #-}

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
gdsDataSetType :: Lens.Lens' GenerateDataSet DataSetType
gdsDataSetType = Lens.lens (dataSetType :: GenerateDataSet -> DataSetType) (\s a -> s {dataSetType = a} :: GenerateDataSet)
{-# DEPRECATED gdsDataSetType "Use generic-lens or generic-optics with 'dataSetType' instead." #-}

-- | The date a data set was published. For daily data sets, provide a date with day-level granularity for the desired day. For monthly data sets except those with prefix disbursed_amount, provide a date with month-level granularity for the desired month (the day value will be ignored). For data sets with prefix disbursed_amount, provide a date with day-level granularity for the desired day. For these data sets we will look backwards in time over the range of 31 days until the first data set is found (the latest one).
--
-- /Note:/ Consider using 'dataSetPublicationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsDataSetPublicationDate :: Lens.Lens' GenerateDataSet Lude.Timestamp
gdsDataSetPublicationDate = Lens.lens (dataSetPublicationDate :: GenerateDataSet -> Lude.Timestamp) (\s a -> s {dataSetPublicationDate = a} :: GenerateDataSet)
{-# DEPRECATED gdsDataSetPublicationDate "Use generic-lens or generic-optics with 'dataSetPublicationDate' instead." #-}

-- | The Amazon Resource Name (ARN) of the Role with an attached permissions policy to interact with the provided AWS services.
--
-- /Note:/ Consider using 'roleNameARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsRoleNameARN :: Lens.Lens' GenerateDataSet Lude.Text
gdsRoleNameARN = Lens.lens (roleNameARN :: GenerateDataSet -> Lude.Text) (\s a -> s {roleNameARN = a} :: GenerateDataSet)
{-# DEPRECATED gdsRoleNameARN "Use generic-lens or generic-optics with 'roleNameARN' instead." #-}

-- | The name (friendly name, not ARN) of the destination S3 bucket.
--
-- /Note:/ Consider using 'destinationS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsDestinationS3BucketName :: Lens.Lens' GenerateDataSet Lude.Text
gdsDestinationS3BucketName = Lens.lens (destinationS3BucketName :: GenerateDataSet -> Lude.Text) (\s a -> s {destinationS3BucketName = a} :: GenerateDataSet)
{-# DEPRECATED gdsDestinationS3BucketName "Use generic-lens or generic-optics with 'destinationS3BucketName' instead." #-}

-- | Amazon Resource Name (ARN) for the SNS Topic that will be notified when the data set has been published or if an error has occurred.
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsSnsTopicARN :: Lens.Lens' GenerateDataSet Lude.Text
gdsSnsTopicARN = Lens.lens (snsTopicARN :: GenerateDataSet -> Lude.Text) (\s a -> s {snsTopicARN = a} :: GenerateDataSet)
{-# DEPRECATED gdsSnsTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

instance Lude.AWSRequest GenerateDataSet where
  type Rs GenerateDataSet = GenerateDataSetResponse
  request = Req.postJSON marketplaceAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GenerateDataSetResponse'
            Lude.<$> (x Lude..?> "dataSetRequestId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GenerateDataSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MarketplaceCommerceAnalytics20150701.GenerateDataSet" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GenerateDataSet where
  toJSON GenerateDataSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("customerDefinedValues" Lude..=) Lude.<$> customerDefinedValues,
            ("destinationS3Prefix" Lude..=) Lude.<$> destinationS3Prefix,
            Lude.Just ("dataSetType" Lude..= dataSetType),
            Lude.Just
              ("dataSetPublicationDate" Lude..= dataSetPublicationDate),
            Lude.Just ("roleNameArn" Lude..= roleNameARN),
            Lude.Just
              ("destinationS3BucketName" Lude..= destinationS3BucketName),
            Lude.Just ("snsTopicArn" Lude..= snsTopicARN)
          ]
      )

instance Lude.ToPath GenerateDataSet where
  toPath = Lude.const "/"

instance Lude.ToQuery GenerateDataSet where
  toQuery = Lude.const Lude.mempty

-- | Container for the result of the GenerateDataSet operation.
--
-- /See:/ 'mkGenerateDataSetResponse' smart constructor.
data GenerateDataSetResponse = GenerateDataSetResponse'
  { dataSetRequestId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateDataSetResponse' with the minimum fields required to make a request.
--
-- * 'dataSetRequestId' - A unique identifier representing a specific request to the GenerateDataSet operation. This identifier can be used to correlate a request with notifications from the SNS topic.
-- * 'responseStatus' - The response status code.
mkGenerateDataSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GenerateDataSetResponse
mkGenerateDataSetResponse pResponseStatus_ =
  GenerateDataSetResponse'
    { dataSetRequestId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique identifier representing a specific request to the GenerateDataSet operation. This identifier can be used to correlate a request with notifications from the SNS topic.
--
-- /Note:/ Consider using 'dataSetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsDataSetRequestId :: Lens.Lens' GenerateDataSetResponse (Lude.Maybe Lude.Text)
gdsrsDataSetRequestId = Lens.lens (dataSetRequestId :: GenerateDataSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {dataSetRequestId = a} :: GenerateDataSetResponse)
{-# DEPRECATED gdsrsDataSetRequestId "Use generic-lens or generic-optics with 'dataSetRequestId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsResponseStatus :: Lens.Lens' GenerateDataSetResponse Lude.Int
gdsrsResponseStatus = Lens.lens (responseStatus :: GenerateDataSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GenerateDataSetResponse)
{-# DEPRECATED gdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
