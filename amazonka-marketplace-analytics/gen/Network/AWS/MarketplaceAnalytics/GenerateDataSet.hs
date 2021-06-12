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
-- Module      : Network.AWS.MarketplaceAnalytics.GenerateDataSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given a data set type and data set publication date, asynchronously
-- publishes the requested data set to the specified S3 bucket and notifies
-- the specified SNS topic once the data is available. Returns a unique
-- request identifier that can be used to correlate requests with
-- notifications from the SNS topic. Data sets will be published in
-- comma-separated values (CSV) format with the file name
-- {data_set_type}_YYYY-MM-DD.csv. If a file with the same name already
-- exists (e.g. if the same data set is requested twice), the original file
-- will be overwritten by the new file. Requires a Role with an attached
-- permissions policy providing Allow permissions for the following
-- actions: s3:PutObject, s3:GetBucketLocation, sns:GetTopicAttributes,
-- sns:Publish, iam:GetRolePolicy.
module Network.AWS.MarketplaceAnalytics.GenerateDataSet
  ( -- * Creating a Request
    GenerateDataSet (..),
    newGenerateDataSet,

    -- * Request Lenses
    generateDataSet_destinationS3Prefix,
    generateDataSet_customerDefinedValues,
    generateDataSet_dataSetType,
    generateDataSet_dataSetPublicationDate,
    generateDataSet_roleNameArn,
    generateDataSet_destinationS3BucketName,
    generateDataSet_snsTopicArn,

    -- * Destructuring the Response
    GenerateDataSetResponse (..),
    newGenerateDataSetResponse,

    -- * Response Lenses
    generateDataSetResponse_dataSetRequestId,
    generateDataSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceAnalytics.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the GenerateDataSet operation.
--
-- /See:/ 'newGenerateDataSet' smart constructor.
data GenerateDataSet = GenerateDataSet'
  { -- | (Optional) The desired S3 prefix for the published data set, similar to
    -- a directory path in standard file systems. For example, if given the
    -- bucket name \"mybucket\" and the prefix \"myprefix\/mydatasets\", the
    -- output file \"outputfile\" would be published to
    -- \"s3:\/\/mybucket\/myprefix\/mydatasets\/outputfile\". If the prefix
    -- directory structure does not exist, it will be created. If no prefix is
    -- provided, the data set will be published to the S3 bucket root.
    destinationS3Prefix :: Core.Maybe Core.Text,
    -- | (Optional) Key-value pairs which will be returned, unmodified, in the
    -- Amazon SNS notification message and the data set metadata file. These
    -- key-value pairs can be used to correlated responses with tracking
    -- information from other systems.
    customerDefinedValues :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The desired data set type.
    --
    -- -   __customer_subscriber_hourly_monthly_subscriptions__
    --
    --     From 2017-09-15 to present: Available daily by 24:00 UTC.
    --
    -- -   __customer_subscriber_annual_subscriptions__
    --
    --     From 2017-09-15 to present: Available daily by 24:00 UTC.
    --
    -- -   __daily_business_usage_by_instance_type__
    --
    --     From 2017-09-15 to present: Available daily by 24:00 UTC.
    --
    -- -   __daily_business_fees__
    --
    --     From 2017-09-15 to present: Available daily by 24:00 UTC.
    --
    -- -   __daily_business_free_trial_conversions__
    --
    --     From 2017-09-15 to present: Available daily by 24:00 UTC.
    --
    -- -   __daily_business_new_instances__
    --
    --     From 2017-09-15 to present: Available daily by 24:00 UTC.
    --
    -- -   __daily_business_new_product_subscribers__
    --
    --     From 2017-09-15 to present: Available daily by 24:00 UTC.
    --
    -- -   __daily_business_canceled_product_subscribers__
    --
    --     From 2017-09-15 to present: Available daily by 24:00 UTC.
    --
    -- -   __monthly_revenue_billing_and_revenue_data__
    --
    --     From 2017-09-15 to present: Available monthly on the 15th day of the
    --     month by 24:00 UTC. Data includes metered transactions (e.g. hourly)
    --     from one month prior.
    --
    -- -   __monthly_revenue_annual_subscriptions__
    --
    --     From 2017-09-15 to present: Available monthly on the 15th day of the
    --     month by 24:00 UTC. Data includes up-front software charges (e.g.
    --     annual) from one month prior.
    --
    -- -   __monthly_revenue_field_demonstration_usage__
    --
    --     From 2018-03-15 to present: Available monthly on the 15th day of the
    --     month by 24:00 UTC.
    --
    -- -   __monthly_revenue_flexible_payment_schedule__
    --
    --     From 2018-11-15 to present: Available monthly on the 15th day of the
    --     month by 24:00 UTC.
    --
    -- -   __disbursed_amount_by_product__
    --
    --     From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
    --
    -- -   __disbursed_amount_by_instance_hours__
    --
    --     From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
    --
    -- -   __disbursed_amount_by_customer_geo__
    --
    --     From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
    --
    -- -   __disbursed_amount_by_age_of_uncollected_funds__
    --
    --     From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
    --
    -- -   __disbursed_amount_by_age_of_disbursed_funds__
    --
    --     From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
    --
    -- -   __disbursed_amount_by_age_of_past_due_funds__
    --
    --     From 2018-04-07 to present: Available every 30 days by 24:00 UTC.
    --
    -- -   __disbursed_amount_by_uncollected_funds_breakdown__
    --
    --     From 2019-10-04 to present: Available every 30 days by 24:00 UTC.
    --
    -- -   __sales_compensation_billed_revenue__
    --
    --     From 2017-09-15 to present: Available monthly on the 15th day of the
    --     month by 24:00 UTC. Data includes metered transactions (e.g. hourly)
    --     from one month prior, and up-front software charges (e.g. annual)
    --     from one month prior.
    --
    -- -   __us_sales_and_use_tax_records__
    --
    --     From 2017-09-15 to present: Available monthly on the 15th day of the
    --     month by 24:00 UTC.
    --
    -- -   __disbursed_amount_by_product_with_uncollected_funds__
    --
    --     This data set is deprecated. Download related reports from AMMP
    --     instead!
    --
    -- -   __customer_profile_by_industry__
    --
    --     This data set is deprecated. Download related reports from AMMP
    --     instead!
    --
    -- -   __customer_profile_by_revenue__
    --
    --     This data set is deprecated. Download related reports from AMMP
    --     instead!
    --
    -- -   __customer_profile_by_geography__
    --
    --     This data set is deprecated. Download related reports from AMMP
    --     instead!
    dataSetType :: DataSetType,
    -- | The date a data set was published. For daily data sets, provide a date
    -- with day-level granularity for the desired day. For monthly data sets
    -- except those with prefix disbursed_amount, provide a date with
    -- month-level granularity for the desired month (the day value will be
    -- ignored). For data sets with prefix disbursed_amount, provide a date
    -- with day-level granularity for the desired day. For these data sets we
    -- will look backwards in time over the range of 31 days until the first
    -- data set is found (the latest one).
    dataSetPublicationDate :: Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the Role with an attached permissions
    -- policy to interact with the provided AWS services.
    roleNameArn :: Core.Text,
    -- | The name (friendly name, not ARN) of the destination S3 bucket.
    destinationS3BucketName :: Core.Text,
    -- | Amazon Resource Name (ARN) for the SNS Topic that will be notified when
    -- the data set has been published or if an error has occurred.
    snsTopicArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GenerateDataSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationS3Prefix', 'generateDataSet_destinationS3Prefix' - (Optional) The desired S3 prefix for the published data set, similar to
-- a directory path in standard file systems. For example, if given the
-- bucket name \"mybucket\" and the prefix \"myprefix\/mydatasets\", the
-- output file \"outputfile\" would be published to
-- \"s3:\/\/mybucket\/myprefix\/mydatasets\/outputfile\". If the prefix
-- directory structure does not exist, it will be created. If no prefix is
-- provided, the data set will be published to the S3 bucket root.
--
-- 'customerDefinedValues', 'generateDataSet_customerDefinedValues' - (Optional) Key-value pairs which will be returned, unmodified, in the
-- Amazon SNS notification message and the data set metadata file. These
-- key-value pairs can be used to correlated responses with tracking
-- information from other systems.
--
-- 'dataSetType', 'generateDataSet_dataSetType' - The desired data set type.
--
-- -   __customer_subscriber_hourly_monthly_subscriptions__
--
--     From 2017-09-15 to present: Available daily by 24:00 UTC.
--
-- -   __customer_subscriber_annual_subscriptions__
--
--     From 2017-09-15 to present: Available daily by 24:00 UTC.
--
-- -   __daily_business_usage_by_instance_type__
--
--     From 2017-09-15 to present: Available daily by 24:00 UTC.
--
-- -   __daily_business_fees__
--
--     From 2017-09-15 to present: Available daily by 24:00 UTC.
--
-- -   __daily_business_free_trial_conversions__
--
--     From 2017-09-15 to present: Available daily by 24:00 UTC.
--
-- -   __daily_business_new_instances__
--
--     From 2017-09-15 to present: Available daily by 24:00 UTC.
--
-- -   __daily_business_new_product_subscribers__
--
--     From 2017-09-15 to present: Available daily by 24:00 UTC.
--
-- -   __daily_business_canceled_product_subscribers__
--
--     From 2017-09-15 to present: Available daily by 24:00 UTC.
--
-- -   __monthly_revenue_billing_and_revenue_data__
--
--     From 2017-09-15 to present: Available monthly on the 15th day of the
--     month by 24:00 UTC. Data includes metered transactions (e.g. hourly)
--     from one month prior.
--
-- -   __monthly_revenue_annual_subscriptions__
--
--     From 2017-09-15 to present: Available monthly on the 15th day of the
--     month by 24:00 UTC. Data includes up-front software charges (e.g.
--     annual) from one month prior.
--
-- -   __monthly_revenue_field_demonstration_usage__
--
--     From 2018-03-15 to present: Available monthly on the 15th day of the
--     month by 24:00 UTC.
--
-- -   __monthly_revenue_flexible_payment_schedule__
--
--     From 2018-11-15 to present: Available monthly on the 15th day of the
--     month by 24:00 UTC.
--
-- -   __disbursed_amount_by_product__
--
--     From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
-- -   __disbursed_amount_by_instance_hours__
--
--     From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
-- -   __disbursed_amount_by_customer_geo__
--
--     From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
-- -   __disbursed_amount_by_age_of_uncollected_funds__
--
--     From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
-- -   __disbursed_amount_by_age_of_disbursed_funds__
--
--     From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
-- -   __disbursed_amount_by_age_of_past_due_funds__
--
--     From 2018-04-07 to present: Available every 30 days by 24:00 UTC.
--
-- -   __disbursed_amount_by_uncollected_funds_breakdown__
--
--     From 2019-10-04 to present: Available every 30 days by 24:00 UTC.
--
-- -   __sales_compensation_billed_revenue__
--
--     From 2017-09-15 to present: Available monthly on the 15th day of the
--     month by 24:00 UTC. Data includes metered transactions (e.g. hourly)
--     from one month prior, and up-front software charges (e.g. annual)
--     from one month prior.
--
-- -   __us_sales_and_use_tax_records__
--
--     From 2017-09-15 to present: Available monthly on the 15th day of the
--     month by 24:00 UTC.
--
-- -   __disbursed_amount_by_product_with_uncollected_funds__
--
--     This data set is deprecated. Download related reports from AMMP
--     instead!
--
-- -   __customer_profile_by_industry__
--
--     This data set is deprecated. Download related reports from AMMP
--     instead!
--
-- -   __customer_profile_by_revenue__
--
--     This data set is deprecated. Download related reports from AMMP
--     instead!
--
-- -   __customer_profile_by_geography__
--
--     This data set is deprecated. Download related reports from AMMP
--     instead!
--
-- 'dataSetPublicationDate', 'generateDataSet_dataSetPublicationDate' - The date a data set was published. For daily data sets, provide a date
-- with day-level granularity for the desired day. For monthly data sets
-- except those with prefix disbursed_amount, provide a date with
-- month-level granularity for the desired month (the day value will be
-- ignored). For data sets with prefix disbursed_amount, provide a date
-- with day-level granularity for the desired day. For these data sets we
-- will look backwards in time over the range of 31 days until the first
-- data set is found (the latest one).
--
-- 'roleNameArn', 'generateDataSet_roleNameArn' - The Amazon Resource Name (ARN) of the Role with an attached permissions
-- policy to interact with the provided AWS services.
--
-- 'destinationS3BucketName', 'generateDataSet_destinationS3BucketName' - The name (friendly name, not ARN) of the destination S3 bucket.
--
-- 'snsTopicArn', 'generateDataSet_snsTopicArn' - Amazon Resource Name (ARN) for the SNS Topic that will be notified when
-- the data set has been published or if an error has occurred.
newGenerateDataSet ::
  -- | 'dataSetType'
  DataSetType ->
  -- | 'dataSetPublicationDate'
  Core.UTCTime ->
  -- | 'roleNameArn'
  Core.Text ->
  -- | 'destinationS3BucketName'
  Core.Text ->
  -- | 'snsTopicArn'
  Core.Text ->
  GenerateDataSet
newGenerateDataSet
  pDataSetType_
  pDataSetPublicationDate_
  pRoleNameArn_
  pDestinationS3BucketName_
  pSnsTopicArn_ =
    GenerateDataSet'
      { destinationS3Prefix =
          Core.Nothing,
        customerDefinedValues = Core.Nothing,
        dataSetType = pDataSetType_,
        dataSetPublicationDate =
          Core._Time Lens.# pDataSetPublicationDate_,
        roleNameArn = pRoleNameArn_,
        destinationS3BucketName = pDestinationS3BucketName_,
        snsTopicArn = pSnsTopicArn_
      }

-- | (Optional) The desired S3 prefix for the published data set, similar to
-- a directory path in standard file systems. For example, if given the
-- bucket name \"mybucket\" and the prefix \"myprefix\/mydatasets\", the
-- output file \"outputfile\" would be published to
-- \"s3:\/\/mybucket\/myprefix\/mydatasets\/outputfile\". If the prefix
-- directory structure does not exist, it will be created. If no prefix is
-- provided, the data set will be published to the S3 bucket root.
generateDataSet_destinationS3Prefix :: Lens.Lens' GenerateDataSet (Core.Maybe Core.Text)
generateDataSet_destinationS3Prefix = Lens.lens (\GenerateDataSet' {destinationS3Prefix} -> destinationS3Prefix) (\s@GenerateDataSet' {} a -> s {destinationS3Prefix = a} :: GenerateDataSet)

-- | (Optional) Key-value pairs which will be returned, unmodified, in the
-- Amazon SNS notification message and the data set metadata file. These
-- key-value pairs can be used to correlated responses with tracking
-- information from other systems.
generateDataSet_customerDefinedValues :: Lens.Lens' GenerateDataSet (Core.Maybe (Core.HashMap Core.Text Core.Text))
generateDataSet_customerDefinedValues = Lens.lens (\GenerateDataSet' {customerDefinedValues} -> customerDefinedValues) (\s@GenerateDataSet' {} a -> s {customerDefinedValues = a} :: GenerateDataSet) Core.. Lens.mapping Lens._Coerce

-- | The desired data set type.
--
-- -   __customer_subscriber_hourly_monthly_subscriptions__
--
--     From 2017-09-15 to present: Available daily by 24:00 UTC.
--
-- -   __customer_subscriber_annual_subscriptions__
--
--     From 2017-09-15 to present: Available daily by 24:00 UTC.
--
-- -   __daily_business_usage_by_instance_type__
--
--     From 2017-09-15 to present: Available daily by 24:00 UTC.
--
-- -   __daily_business_fees__
--
--     From 2017-09-15 to present: Available daily by 24:00 UTC.
--
-- -   __daily_business_free_trial_conversions__
--
--     From 2017-09-15 to present: Available daily by 24:00 UTC.
--
-- -   __daily_business_new_instances__
--
--     From 2017-09-15 to present: Available daily by 24:00 UTC.
--
-- -   __daily_business_new_product_subscribers__
--
--     From 2017-09-15 to present: Available daily by 24:00 UTC.
--
-- -   __daily_business_canceled_product_subscribers__
--
--     From 2017-09-15 to present: Available daily by 24:00 UTC.
--
-- -   __monthly_revenue_billing_and_revenue_data__
--
--     From 2017-09-15 to present: Available monthly on the 15th day of the
--     month by 24:00 UTC. Data includes metered transactions (e.g. hourly)
--     from one month prior.
--
-- -   __monthly_revenue_annual_subscriptions__
--
--     From 2017-09-15 to present: Available monthly on the 15th day of the
--     month by 24:00 UTC. Data includes up-front software charges (e.g.
--     annual) from one month prior.
--
-- -   __monthly_revenue_field_demonstration_usage__
--
--     From 2018-03-15 to present: Available monthly on the 15th day of the
--     month by 24:00 UTC.
--
-- -   __monthly_revenue_flexible_payment_schedule__
--
--     From 2018-11-15 to present: Available monthly on the 15th day of the
--     month by 24:00 UTC.
--
-- -   __disbursed_amount_by_product__
--
--     From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
-- -   __disbursed_amount_by_instance_hours__
--
--     From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
-- -   __disbursed_amount_by_customer_geo__
--
--     From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
-- -   __disbursed_amount_by_age_of_uncollected_funds__
--
--     From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
-- -   __disbursed_amount_by_age_of_disbursed_funds__
--
--     From 2017-09-15 to present: Available every 30 days by 24:00 UTC.
--
-- -   __disbursed_amount_by_age_of_past_due_funds__
--
--     From 2018-04-07 to present: Available every 30 days by 24:00 UTC.
--
-- -   __disbursed_amount_by_uncollected_funds_breakdown__
--
--     From 2019-10-04 to present: Available every 30 days by 24:00 UTC.
--
-- -   __sales_compensation_billed_revenue__
--
--     From 2017-09-15 to present: Available monthly on the 15th day of the
--     month by 24:00 UTC. Data includes metered transactions (e.g. hourly)
--     from one month prior, and up-front software charges (e.g. annual)
--     from one month prior.
--
-- -   __us_sales_and_use_tax_records__
--
--     From 2017-09-15 to present: Available monthly on the 15th day of the
--     month by 24:00 UTC.
--
-- -   __disbursed_amount_by_product_with_uncollected_funds__
--
--     This data set is deprecated. Download related reports from AMMP
--     instead!
--
-- -   __customer_profile_by_industry__
--
--     This data set is deprecated. Download related reports from AMMP
--     instead!
--
-- -   __customer_profile_by_revenue__
--
--     This data set is deprecated. Download related reports from AMMP
--     instead!
--
-- -   __customer_profile_by_geography__
--
--     This data set is deprecated. Download related reports from AMMP
--     instead!
generateDataSet_dataSetType :: Lens.Lens' GenerateDataSet DataSetType
generateDataSet_dataSetType = Lens.lens (\GenerateDataSet' {dataSetType} -> dataSetType) (\s@GenerateDataSet' {} a -> s {dataSetType = a} :: GenerateDataSet)

-- | The date a data set was published. For daily data sets, provide a date
-- with day-level granularity for the desired day. For monthly data sets
-- except those with prefix disbursed_amount, provide a date with
-- month-level granularity for the desired month (the day value will be
-- ignored). For data sets with prefix disbursed_amount, provide a date
-- with day-level granularity for the desired day. For these data sets we
-- will look backwards in time over the range of 31 days until the first
-- data set is found (the latest one).
generateDataSet_dataSetPublicationDate :: Lens.Lens' GenerateDataSet Core.UTCTime
generateDataSet_dataSetPublicationDate = Lens.lens (\GenerateDataSet' {dataSetPublicationDate} -> dataSetPublicationDate) (\s@GenerateDataSet' {} a -> s {dataSetPublicationDate = a} :: GenerateDataSet) Core.. Core._Time

-- | The Amazon Resource Name (ARN) of the Role with an attached permissions
-- policy to interact with the provided AWS services.
generateDataSet_roleNameArn :: Lens.Lens' GenerateDataSet Core.Text
generateDataSet_roleNameArn = Lens.lens (\GenerateDataSet' {roleNameArn} -> roleNameArn) (\s@GenerateDataSet' {} a -> s {roleNameArn = a} :: GenerateDataSet)

-- | The name (friendly name, not ARN) of the destination S3 bucket.
generateDataSet_destinationS3BucketName :: Lens.Lens' GenerateDataSet Core.Text
generateDataSet_destinationS3BucketName = Lens.lens (\GenerateDataSet' {destinationS3BucketName} -> destinationS3BucketName) (\s@GenerateDataSet' {} a -> s {destinationS3BucketName = a} :: GenerateDataSet)

-- | Amazon Resource Name (ARN) for the SNS Topic that will be notified when
-- the data set has been published or if an error has occurred.
generateDataSet_snsTopicArn :: Lens.Lens' GenerateDataSet Core.Text
generateDataSet_snsTopicArn = Lens.lens (\GenerateDataSet' {snsTopicArn} -> snsTopicArn) (\s@GenerateDataSet' {} a -> s {snsTopicArn = a} :: GenerateDataSet)

instance Core.AWSRequest GenerateDataSet where
  type
    AWSResponse GenerateDataSet =
      GenerateDataSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateDataSetResponse'
            Core.<$> (x Core..?> "dataSetRequestId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GenerateDataSet

instance Core.NFData GenerateDataSet

instance Core.ToHeaders GenerateDataSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MarketplaceCommerceAnalytics20150701.GenerateDataSet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GenerateDataSet where
  toJSON GenerateDataSet' {..} =
    Core.object
      ( Core.catMaybes
          [ ("destinationS3Prefix" Core..=)
              Core.<$> destinationS3Prefix,
            ("customerDefinedValues" Core..=)
              Core.<$> customerDefinedValues,
            Core.Just ("dataSetType" Core..= dataSetType),
            Core.Just
              ( "dataSetPublicationDate"
                  Core..= dataSetPublicationDate
              ),
            Core.Just ("roleNameArn" Core..= roleNameArn),
            Core.Just
              ( "destinationS3BucketName"
                  Core..= destinationS3BucketName
              ),
            Core.Just ("snsTopicArn" Core..= snsTopicArn)
          ]
      )

instance Core.ToPath GenerateDataSet where
  toPath = Core.const "/"

instance Core.ToQuery GenerateDataSet where
  toQuery = Core.const Core.mempty

-- | Container for the result of the GenerateDataSet operation.
--
-- /See:/ 'newGenerateDataSetResponse' smart constructor.
data GenerateDataSetResponse = GenerateDataSetResponse'
  { -- | A unique identifier representing a specific request to the
    -- GenerateDataSet operation. This identifier can be used to correlate a
    -- request with notifications from the SNS topic.
    dataSetRequestId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GenerateDataSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetRequestId', 'generateDataSetResponse_dataSetRequestId' - A unique identifier representing a specific request to the
-- GenerateDataSet operation. This identifier can be used to correlate a
-- request with notifications from the SNS topic.
--
-- 'httpStatus', 'generateDataSetResponse_httpStatus' - The response's http status code.
newGenerateDataSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GenerateDataSetResponse
newGenerateDataSetResponse pHttpStatus_ =
  GenerateDataSetResponse'
    { dataSetRequestId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier representing a specific request to the
-- GenerateDataSet operation. This identifier can be used to correlate a
-- request with notifications from the SNS topic.
generateDataSetResponse_dataSetRequestId :: Lens.Lens' GenerateDataSetResponse (Core.Maybe Core.Text)
generateDataSetResponse_dataSetRequestId = Lens.lens (\GenerateDataSetResponse' {dataSetRequestId} -> dataSetRequestId) (\s@GenerateDataSetResponse' {} a -> s {dataSetRequestId = a} :: GenerateDataSetResponse)

-- | The response's http status code.
generateDataSetResponse_httpStatus :: Lens.Lens' GenerateDataSetResponse Core.Int
generateDataSetResponse_httpStatus = Lens.lens (\GenerateDataSetResponse' {httpStatus} -> httpStatus) (\s@GenerateDataSetResponse' {} a -> s {httpStatus = a} :: GenerateDataSetResponse)

instance Core.NFData GenerateDataSetResponse
