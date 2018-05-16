{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceAnalytics.GenerateDataSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given a data set type and data set publication date, asynchronously publishes the requested data set to the specified S3 bucket and notifies the specified SNS topic once the data is available. Returns a unique request identifier that can be used to correlate requests with notifications from the SNS topic. Data sets will be published in comma-separated values (CSV) format with the file name {data_set_type}_YYYY-MM-DD.csv. If a file with the same name already exists (e.g. if the same data set is requested twice), the original file will be overwritten by the new file. Requires a Role with an attached permissions policy providing Allow permissions for the following actions: s3:PutObject, s3:GetBucketLocation, sns:GetTopicAttributes, sns:Publish, iam:GetRolePolicy.
module Network.AWS.MarketplaceAnalytics.GenerateDataSet
    (
    -- * Creating a Request
      generateDataSet
    , GenerateDataSet
    -- * Request Lenses
    , gdsCustomerDefinedValues
    , gdsDestinationS3Prefix
    , gdsDataSetType
    , gdsDataSetPublicationDate
    , gdsRoleNameARN
    , gdsDestinationS3BucketName
    , gdsSnsTopicARN

    -- * Destructuring the Response
    , generateDataSetResponse
    , GenerateDataSetResponse
    -- * Response Lenses
    , gdsrsDataSetRequestId
    , gdsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MarketplaceAnalytics.Types
import Network.AWS.MarketplaceAnalytics.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the GenerateDataSet operation.
--
-- /See:/ 'generateDataSet' smart constructor.
data GenerateDataSet = GenerateDataSet'
  { _gdsCustomerDefinedValues   :: !(Maybe (Map Text Text))
  , _gdsDestinationS3Prefix     :: !(Maybe Text)
  , _gdsDataSetType             :: !DataSetType
  , _gdsDataSetPublicationDate  :: !POSIX
  , _gdsRoleNameARN             :: !Text
  , _gdsDestinationS3BucketName :: !Text
  , _gdsSnsTopicARN             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GenerateDataSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsCustomerDefinedValues' - (Optional) Key-value pairs which will be returned, unmodified, in the Amazon SNS notification message and the data set metadata file. These key-value pairs can be used to correlated responses with tracking information from other systems.
--
-- * 'gdsDestinationS3Prefix' - (Optional) The desired S3 prefix for the published data set, similar to a directory path in standard file systems. For example, if given the bucket name "mybucket" and the prefix "myprefix/mydatasets", the output file "outputfile" would be published to "s3://mybucket/myprefix/mydatasets/outputfile". If the prefix directory structure does not exist, it will be created. If no prefix is provided, the data set will be published to the S3 bucket root.
--
-- * 'gdsDataSetType' - The desired data set type.     * __customer_subscriber_hourly_monthly_subscriptions__ From 2014-07-21 to present: Available daily by 5:00 PM Pacific Time.     * __customer_subscriber_annual_subscriptions__ From 2014-07-21 to present: Available daily by 5:00 PM Pacific Time.     * __daily_business_usage_by_instance_type__ From 2015-01-26 to present: Available daily by 5:00 PM Pacific Time.     * __daily_business_fees__ From 2015-01-26 to present: Available daily by 5:00 PM Pacific Time.     * __daily_business_free_trial_conversions__ From 2015-01-26 to present: Available daily by 5:00 PM Pacific Time.     * __daily_business_new_instances__ From 2015-01-26 to present: Available daily by 5:00 PM Pacific Time.     * __daily_business_new_product_subscribers__ From 2015-01-26 to present: Available daily by 5:00 PM Pacific Time.     * __daily_business_canceled_product_subscribers__ From 2015-01-26 to present: Available daily by 5:00 PM Pacific Time.     * __monthly_revenue_billing_and_revenue_data__ From 2015-02 to 2017-06: Available monthly on the 4th day of the month by 5:00pm Pacific Time. Data includes metered transactions (e.g. hourly) from two months prior. From 2017-07 to present: Available monthly on the 15th day of the month by 5:00pm Pacific Time. Data includes metered transactions (e.g. hourly) from one month prior.     * __monthly_revenue_annual_subscriptions__ From 2015-02 to 2017-06: Available monthly on the 4th day of the month by 5:00pm Pacific Time. Data includes up-front software charges (e.g. annual) from one month prior. From 2017-07 to present: Available monthly on the 15th day of the month by 5:00pm Pacific Time. Data includes up-front software charges (e.g. annual) from one month prior.     * __disbursed_amount_by_product__ From 2015-01-26 to present: Available every 30 days by 5:00 PM Pacific Time.     * __disbursed_amount_by_product_with_uncollected_funds__ From 2012-04-19 to 2015-01-25: Available every 30 days by 5:00 PM Pacific Time. From 2015-01-26 to present: This data set was split into three data sets: disbursed_amount_by_product, disbursed_amount_by_age_of_uncollected_funds, and disbursed_amount_by_age_of_disbursed_funds.     * __disbursed_amount_by_instance_hours__ From 2012-09-04 to present: Available every 30 days by 5:00 PM Pacific Time.     * __disbursed_amount_by_customer_geo__ From 2012-04-19 to present: Available every 30 days by 5:00 PM Pacific Time.     * __disbursed_amount_by_age_of_uncollected_funds__ From 2015-01-26 to present: Available every 30 days by 5:00 PM Pacific Time.     * __disbursed_amount_by_age_of_disbursed_funds__ From 2015-01-26 to present: Available every 30 days by 5:00 PM Pacific Time.     * __customer_profile_by_industry__ From 2015-10-01 to 2017-06-29: Available daily by 5:00 PM Pacific Time. From 2017-06-30 to present: This data set is no longer available.     * __customer_profile_by_revenue__ From 2015-10-01 to 2017-06-29: Available daily by 5:00 PM Pacific Time. From 2017-06-30 to present: This data set is no longer available.     * __customer_profile_by_geography__ From 2015-10-01 to 2017-06-29: Available daily by 5:00 PM Pacific Time. From 2017-06-30 to present: This data set is no longer available.     * __sales_compensation_billed_revenue__ From 2016-12 to 2017-06: Available monthly on the 4th day of the month by 5:00pm Pacific Time. Data includes metered transactions (e.g. hourly) from two months prior, and up-front software charges (e.g. annual) from one month prior. From 2017-06 to present: Available monthly on the 15th day of the month by 5:00pm Pacific Time. Data includes metered transactions (e.g. hourly) from one month prior, and up-front software charges (e.g. annual) from one month prior.     * __us_sales_and_use_tax_records__ From 2017-02-15 to present: Available monthly on the 15th day of the month by 5:00 PM Pacific Time.
--
-- * 'gdsDataSetPublicationDate' - The date a data set was published. For daily data sets, provide a date with day-level granularity for the desired day. For weekly data sets, provide a date with day-level granularity within the desired week (the day value will be ignored). For monthly data sets, provide a date with month-level granularity for the desired month (the day value will be ignored).
--
-- * 'gdsRoleNameARN' - The Amazon Resource Name (ARN) of the Role with an attached permissions policy to interact with the provided AWS services.
--
-- * 'gdsDestinationS3BucketName' - The name (friendly name, not ARN) of the destination S3 bucket.
--
-- * 'gdsSnsTopicARN' - Amazon Resource Name (ARN) for the SNS Topic that will be notified when the data set has been published or if an error has occurred.
generateDataSet
    :: DataSetType -- ^ 'gdsDataSetType'
    -> UTCTime -- ^ 'gdsDataSetPublicationDate'
    -> Text -- ^ 'gdsRoleNameARN'
    -> Text -- ^ 'gdsDestinationS3BucketName'
    -> Text -- ^ 'gdsSnsTopicARN'
    -> GenerateDataSet
generateDataSet pDataSetType_ pDataSetPublicationDate_ pRoleNameARN_ pDestinationS3BucketName_ pSnsTopicARN_ =
  GenerateDataSet'
    { _gdsCustomerDefinedValues = Nothing
    , _gdsDestinationS3Prefix = Nothing
    , _gdsDataSetType = pDataSetType_
    , _gdsDataSetPublicationDate = _Time # pDataSetPublicationDate_
    , _gdsRoleNameARN = pRoleNameARN_
    , _gdsDestinationS3BucketName = pDestinationS3BucketName_
    , _gdsSnsTopicARN = pSnsTopicARN_
    }


-- | (Optional) Key-value pairs which will be returned, unmodified, in the Amazon SNS notification message and the data set metadata file. These key-value pairs can be used to correlated responses with tracking information from other systems.
gdsCustomerDefinedValues :: Lens' GenerateDataSet (HashMap Text Text)
gdsCustomerDefinedValues = lens _gdsCustomerDefinedValues (\ s a -> s{_gdsCustomerDefinedValues = a}) . _Default . _Map

-- | (Optional) The desired S3 prefix for the published data set, similar to a directory path in standard file systems. For example, if given the bucket name "mybucket" and the prefix "myprefix/mydatasets", the output file "outputfile" would be published to "s3://mybucket/myprefix/mydatasets/outputfile". If the prefix directory structure does not exist, it will be created. If no prefix is provided, the data set will be published to the S3 bucket root.
gdsDestinationS3Prefix :: Lens' GenerateDataSet (Maybe Text)
gdsDestinationS3Prefix = lens _gdsDestinationS3Prefix (\ s a -> s{_gdsDestinationS3Prefix = a})

-- | The desired data set type.     * __customer_subscriber_hourly_monthly_subscriptions__ From 2014-07-21 to present: Available daily by 5:00 PM Pacific Time.     * __customer_subscriber_annual_subscriptions__ From 2014-07-21 to present: Available daily by 5:00 PM Pacific Time.     * __daily_business_usage_by_instance_type__ From 2015-01-26 to present: Available daily by 5:00 PM Pacific Time.     * __daily_business_fees__ From 2015-01-26 to present: Available daily by 5:00 PM Pacific Time.     * __daily_business_free_trial_conversions__ From 2015-01-26 to present: Available daily by 5:00 PM Pacific Time.     * __daily_business_new_instances__ From 2015-01-26 to present: Available daily by 5:00 PM Pacific Time.     * __daily_business_new_product_subscribers__ From 2015-01-26 to present: Available daily by 5:00 PM Pacific Time.     * __daily_business_canceled_product_subscribers__ From 2015-01-26 to present: Available daily by 5:00 PM Pacific Time.     * __monthly_revenue_billing_and_revenue_data__ From 2015-02 to 2017-06: Available monthly on the 4th day of the month by 5:00pm Pacific Time. Data includes metered transactions (e.g. hourly) from two months prior. From 2017-07 to present: Available monthly on the 15th day of the month by 5:00pm Pacific Time. Data includes metered transactions (e.g. hourly) from one month prior.     * __monthly_revenue_annual_subscriptions__ From 2015-02 to 2017-06: Available monthly on the 4th day of the month by 5:00pm Pacific Time. Data includes up-front software charges (e.g. annual) from one month prior. From 2017-07 to present: Available monthly on the 15th day of the month by 5:00pm Pacific Time. Data includes up-front software charges (e.g. annual) from one month prior.     * __disbursed_amount_by_product__ From 2015-01-26 to present: Available every 30 days by 5:00 PM Pacific Time.     * __disbursed_amount_by_product_with_uncollected_funds__ From 2012-04-19 to 2015-01-25: Available every 30 days by 5:00 PM Pacific Time. From 2015-01-26 to present: This data set was split into three data sets: disbursed_amount_by_product, disbursed_amount_by_age_of_uncollected_funds, and disbursed_amount_by_age_of_disbursed_funds.     * __disbursed_amount_by_instance_hours__ From 2012-09-04 to present: Available every 30 days by 5:00 PM Pacific Time.     * __disbursed_amount_by_customer_geo__ From 2012-04-19 to present: Available every 30 days by 5:00 PM Pacific Time.     * __disbursed_amount_by_age_of_uncollected_funds__ From 2015-01-26 to present: Available every 30 days by 5:00 PM Pacific Time.     * __disbursed_amount_by_age_of_disbursed_funds__ From 2015-01-26 to present: Available every 30 days by 5:00 PM Pacific Time.     * __customer_profile_by_industry__ From 2015-10-01 to 2017-06-29: Available daily by 5:00 PM Pacific Time. From 2017-06-30 to present: This data set is no longer available.     * __customer_profile_by_revenue__ From 2015-10-01 to 2017-06-29: Available daily by 5:00 PM Pacific Time. From 2017-06-30 to present: This data set is no longer available.     * __customer_profile_by_geography__ From 2015-10-01 to 2017-06-29: Available daily by 5:00 PM Pacific Time. From 2017-06-30 to present: This data set is no longer available.     * __sales_compensation_billed_revenue__ From 2016-12 to 2017-06: Available monthly on the 4th day of the month by 5:00pm Pacific Time. Data includes metered transactions (e.g. hourly) from two months prior, and up-front software charges (e.g. annual) from one month prior. From 2017-06 to present: Available monthly on the 15th day of the month by 5:00pm Pacific Time. Data includes metered transactions (e.g. hourly) from one month prior, and up-front software charges (e.g. annual) from one month prior.     * __us_sales_and_use_tax_records__ From 2017-02-15 to present: Available monthly on the 15th day of the month by 5:00 PM Pacific Time.
gdsDataSetType :: Lens' GenerateDataSet DataSetType
gdsDataSetType = lens _gdsDataSetType (\ s a -> s{_gdsDataSetType = a})

-- | The date a data set was published. For daily data sets, provide a date with day-level granularity for the desired day. For weekly data sets, provide a date with day-level granularity within the desired week (the day value will be ignored). For monthly data sets, provide a date with month-level granularity for the desired month (the day value will be ignored).
gdsDataSetPublicationDate :: Lens' GenerateDataSet UTCTime
gdsDataSetPublicationDate = lens _gdsDataSetPublicationDate (\ s a -> s{_gdsDataSetPublicationDate = a}) . _Time

-- | The Amazon Resource Name (ARN) of the Role with an attached permissions policy to interact with the provided AWS services.
gdsRoleNameARN :: Lens' GenerateDataSet Text
gdsRoleNameARN = lens _gdsRoleNameARN (\ s a -> s{_gdsRoleNameARN = a})

-- | The name (friendly name, not ARN) of the destination S3 bucket.
gdsDestinationS3BucketName :: Lens' GenerateDataSet Text
gdsDestinationS3BucketName = lens _gdsDestinationS3BucketName (\ s a -> s{_gdsDestinationS3BucketName = a})

-- | Amazon Resource Name (ARN) for the SNS Topic that will be notified when the data set has been published or if an error has occurred.
gdsSnsTopicARN :: Lens' GenerateDataSet Text
gdsSnsTopicARN = lens _gdsSnsTopicARN (\ s a -> s{_gdsSnsTopicARN = a})

instance AWSRequest GenerateDataSet where
        type Rs GenerateDataSet = GenerateDataSetResponse
        request = postJSON marketplaceAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 GenerateDataSetResponse' <$>
                   (x .?> "dataSetRequestId") <*> (pure (fromEnum s)))

instance Hashable GenerateDataSet where

instance NFData GenerateDataSet where

instance ToHeaders GenerateDataSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MarketplaceCommerceAnalytics20150701.GenerateDataSet"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GenerateDataSet where
        toJSON GenerateDataSet'{..}
          = object
              (catMaybes
                 [("customerDefinedValues" .=) <$>
                    _gdsCustomerDefinedValues,
                  ("destinationS3Prefix" .=) <$>
                    _gdsDestinationS3Prefix,
                  Just ("dataSetType" .= _gdsDataSetType),
                  Just
                    ("dataSetPublicationDate" .=
                       _gdsDataSetPublicationDate),
                  Just ("roleNameArn" .= _gdsRoleNameARN),
                  Just
                    ("destinationS3BucketName" .=
                       _gdsDestinationS3BucketName),
                  Just ("snsTopicArn" .= _gdsSnsTopicARN)])

instance ToPath GenerateDataSet where
        toPath = const "/"

instance ToQuery GenerateDataSet where
        toQuery = const mempty

-- | Container for the result of the GenerateDataSet operation.
--
-- /See:/ 'generateDataSetResponse' smart constructor.
data GenerateDataSetResponse = GenerateDataSetResponse'
  { _gdsrsDataSetRequestId :: !(Maybe Text)
  , _gdsrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GenerateDataSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsrsDataSetRequestId' - A unique identifier representing a specific request to the GenerateDataSet operation. This identifier can be used to correlate a request with notifications from the SNS topic.
--
-- * 'gdsrsResponseStatus' - -- | The response status code.
generateDataSetResponse
    :: Int -- ^ 'gdsrsResponseStatus'
    -> GenerateDataSetResponse
generateDataSetResponse pResponseStatus_ =
  GenerateDataSetResponse'
    {_gdsrsDataSetRequestId = Nothing, _gdsrsResponseStatus = pResponseStatus_}


-- | A unique identifier representing a specific request to the GenerateDataSet operation. This identifier can be used to correlate a request with notifications from the SNS topic.
gdsrsDataSetRequestId :: Lens' GenerateDataSetResponse (Maybe Text)
gdsrsDataSetRequestId = lens _gdsrsDataSetRequestId (\ s a -> s{_gdsrsDataSetRequestId = a})

-- | -- | The response status code.
gdsrsResponseStatus :: Lens' GenerateDataSetResponse Int
gdsrsResponseStatus = lens _gdsrsResponseStatus (\ s a -> s{_gdsrsResponseStatus = a})

instance NFData GenerateDataSetResponse where
