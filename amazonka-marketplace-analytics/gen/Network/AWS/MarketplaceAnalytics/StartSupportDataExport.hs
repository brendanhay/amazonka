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
-- Module      : Network.AWS.MarketplaceAnalytics.StartSupportDataExport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given a data set type and a from date, asynchronously publishes the requested customer support data to the specified S3 bucket and notifies the specified SNS topic once the data is available. Returns a unique request identifier that can be used to correlate requests with notifications from the SNS topic. Data sets will be published in comma-separated values (CSV) format with the file name {data_set_type}_YYYY-MM-DD'T'HH-mm-ss'Z'.csv. If a file with the same name already exists (e.g. if the same data set is requested twice), the original file will be overwritten by the new file. Requires a Role with an attached permissions policy providing Allow permissions for the following actions: s3:PutObject, s3:GetBucketLocation, sns:GetTopicAttributes, sns:Publish, iam:GetRolePolicy.
module Network.AWS.MarketplaceAnalytics.StartSupportDataExport
    (
    -- * Creating a Request
      startSupportDataExport
    , StartSupportDataExport
    -- * Request Lenses
    , ssdeCustomerDefinedValues
    , ssdeDestinationS3Prefix
    , ssdeDataSetType
    , ssdeFromDate
    , ssdeRoleNameARN
    , ssdeDestinationS3BucketName
    , ssdeSnsTopicARN

    -- * Destructuring the Response
    , startSupportDataExportResponse
    , StartSupportDataExportResponse
    -- * Response Lenses
    , ssdersDataSetRequestId
    , ssdersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MarketplaceAnalytics.Types
import Network.AWS.MarketplaceAnalytics.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the StartSupportDataExport operation.
--
-- /See:/ 'startSupportDataExport' smart constructor.
data StartSupportDataExport = StartSupportDataExport'
  { _ssdeCustomerDefinedValues   :: !(Maybe (Map Text Text))
  , _ssdeDestinationS3Prefix     :: !(Maybe Text)
  , _ssdeDataSetType             :: !SupportDataSetType
  , _ssdeFromDate                :: !POSIX
  , _ssdeRoleNameARN             :: !Text
  , _ssdeDestinationS3BucketName :: !Text
  , _ssdeSnsTopicARN             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartSupportDataExport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssdeCustomerDefinedValues' - (Optional) Key-value pairs which will be returned, unmodified, in the Amazon SNS notification message and the data set metadata file.
--
-- * 'ssdeDestinationS3Prefix' - (Optional) The desired S3 prefix for the published data set, similar to a directory path in standard file systems. For example, if given the bucket name "mybucket" and the prefix "myprefix/mydatasets", the output file "outputfile" would be published to "s3://mybucket/myprefix/mydatasets/outputfile". If the prefix directory structure does not exist, it will be created. If no prefix is provided, the data set will be published to the S3 bucket root.
--
-- * 'ssdeDataSetType' - Specifies the data set type to be written to the output csv file. The data set types customer_support_contacts_data and test_customer_support_contacts_data both result in a csv file containing the following fields: Product Id, Product Code, Customer Guid, Subscription Guid, Subscription Start Date, Organization, AWS Account Id, Given Name, Surname, Telephone Number, Email, Title, Country Code, ZIP Code, Operation Type, and Operation Time.      * /customer_support_contacts_data/ Customer support contact data. The data set will contain all changes (Creates, Updates, and Deletes) to customer support contact data from the date specified in the from_date parameter.    * /test_customer_support_contacts_data/ An example data set containing static test data in the same format as customer_support_contacts_data
--
-- * 'ssdeFromDate' - The start date from which to retrieve the data set in UTC. This parameter only affects the customer_support_contacts_data data set type.
--
-- * 'ssdeRoleNameARN' - The Amazon Resource Name (ARN) of the Role with an attached permissions policy to interact with the provided AWS services.
--
-- * 'ssdeDestinationS3BucketName' - The name (friendly name, not ARN) of the destination S3 bucket.
--
-- * 'ssdeSnsTopicARN' - Amazon Resource Name (ARN) for the SNS Topic that will be notified when the data set has been published or if an error has occurred.
startSupportDataExport
    :: SupportDataSetType -- ^ 'ssdeDataSetType'
    -> UTCTime -- ^ 'ssdeFromDate'
    -> Text -- ^ 'ssdeRoleNameARN'
    -> Text -- ^ 'ssdeDestinationS3BucketName'
    -> Text -- ^ 'ssdeSnsTopicARN'
    -> StartSupportDataExport
startSupportDataExport pDataSetType_ pFromDate_ pRoleNameARN_ pDestinationS3BucketName_ pSnsTopicARN_ =
  StartSupportDataExport'
    { _ssdeCustomerDefinedValues = Nothing
    , _ssdeDestinationS3Prefix = Nothing
    , _ssdeDataSetType = pDataSetType_
    , _ssdeFromDate = _Time # pFromDate_
    , _ssdeRoleNameARN = pRoleNameARN_
    , _ssdeDestinationS3BucketName = pDestinationS3BucketName_
    , _ssdeSnsTopicARN = pSnsTopicARN_
    }


-- | (Optional) Key-value pairs which will be returned, unmodified, in the Amazon SNS notification message and the data set metadata file.
ssdeCustomerDefinedValues :: Lens' StartSupportDataExport (HashMap Text Text)
ssdeCustomerDefinedValues = lens _ssdeCustomerDefinedValues (\ s a -> s{_ssdeCustomerDefinedValues = a}) . _Default . _Map

-- | (Optional) The desired S3 prefix for the published data set, similar to a directory path in standard file systems. For example, if given the bucket name "mybucket" and the prefix "myprefix/mydatasets", the output file "outputfile" would be published to "s3://mybucket/myprefix/mydatasets/outputfile". If the prefix directory structure does not exist, it will be created. If no prefix is provided, the data set will be published to the S3 bucket root.
ssdeDestinationS3Prefix :: Lens' StartSupportDataExport (Maybe Text)
ssdeDestinationS3Prefix = lens _ssdeDestinationS3Prefix (\ s a -> s{_ssdeDestinationS3Prefix = a})

-- | Specifies the data set type to be written to the output csv file. The data set types customer_support_contacts_data and test_customer_support_contacts_data both result in a csv file containing the following fields: Product Id, Product Code, Customer Guid, Subscription Guid, Subscription Start Date, Organization, AWS Account Id, Given Name, Surname, Telephone Number, Email, Title, Country Code, ZIP Code, Operation Type, and Operation Time.      * /customer_support_contacts_data/ Customer support contact data. The data set will contain all changes (Creates, Updates, and Deletes) to customer support contact data from the date specified in the from_date parameter.    * /test_customer_support_contacts_data/ An example data set containing static test data in the same format as customer_support_contacts_data
ssdeDataSetType :: Lens' StartSupportDataExport SupportDataSetType
ssdeDataSetType = lens _ssdeDataSetType (\ s a -> s{_ssdeDataSetType = a})

-- | The start date from which to retrieve the data set in UTC. This parameter only affects the customer_support_contacts_data data set type.
ssdeFromDate :: Lens' StartSupportDataExport UTCTime
ssdeFromDate = lens _ssdeFromDate (\ s a -> s{_ssdeFromDate = a}) . _Time

-- | The Amazon Resource Name (ARN) of the Role with an attached permissions policy to interact with the provided AWS services.
ssdeRoleNameARN :: Lens' StartSupportDataExport Text
ssdeRoleNameARN = lens _ssdeRoleNameARN (\ s a -> s{_ssdeRoleNameARN = a})

-- | The name (friendly name, not ARN) of the destination S3 bucket.
ssdeDestinationS3BucketName :: Lens' StartSupportDataExport Text
ssdeDestinationS3BucketName = lens _ssdeDestinationS3BucketName (\ s a -> s{_ssdeDestinationS3BucketName = a})

-- | Amazon Resource Name (ARN) for the SNS Topic that will be notified when the data set has been published or if an error has occurred.
ssdeSnsTopicARN :: Lens' StartSupportDataExport Text
ssdeSnsTopicARN = lens _ssdeSnsTopicARN (\ s a -> s{_ssdeSnsTopicARN = a})

instance AWSRequest StartSupportDataExport where
        type Rs StartSupportDataExport =
             StartSupportDataExportResponse
        request = postJSON marketplaceAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 StartSupportDataExportResponse' <$>
                   (x .?> "dataSetRequestId") <*> (pure (fromEnum s)))

instance Hashable StartSupportDataExport where

instance NFData StartSupportDataExport where

instance ToHeaders StartSupportDataExport where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MarketplaceCommerceAnalytics20150701.StartSupportDataExport"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartSupportDataExport where
        toJSON StartSupportDataExport'{..}
          = object
              (catMaybes
                 [("customerDefinedValues" .=) <$>
                    _ssdeCustomerDefinedValues,
                  ("destinationS3Prefix" .=) <$>
                    _ssdeDestinationS3Prefix,
                  Just ("dataSetType" .= _ssdeDataSetType),
                  Just ("fromDate" .= _ssdeFromDate),
                  Just ("roleNameArn" .= _ssdeRoleNameARN),
                  Just
                    ("destinationS3BucketName" .=
                       _ssdeDestinationS3BucketName),
                  Just ("snsTopicArn" .= _ssdeSnsTopicARN)])

instance ToPath StartSupportDataExport where
        toPath = const "/"

instance ToQuery StartSupportDataExport where
        toQuery = const mempty

-- | Container for the result of the StartSupportDataExport operation.
--
-- /See:/ 'startSupportDataExportResponse' smart constructor.
data StartSupportDataExportResponse = StartSupportDataExportResponse'
  { _ssdersDataSetRequestId :: !(Maybe Text)
  , _ssdersResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartSupportDataExportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssdersDataSetRequestId' - A unique identifier representing a specific request to the StartSupportDataExport operation. This identifier can be used to correlate a request with notifications from the SNS topic.
--
-- * 'ssdersResponseStatus' - -- | The response status code.
startSupportDataExportResponse
    :: Int -- ^ 'ssdersResponseStatus'
    -> StartSupportDataExportResponse
startSupportDataExportResponse pResponseStatus_ =
  StartSupportDataExportResponse'
    { _ssdersDataSetRequestId = Nothing
    , _ssdersResponseStatus = pResponseStatus_
    }


-- | A unique identifier representing a specific request to the StartSupportDataExport operation. This identifier can be used to correlate a request with notifications from the SNS topic.
ssdersDataSetRequestId :: Lens' StartSupportDataExportResponse (Maybe Text)
ssdersDataSetRequestId = lens _ssdersDataSetRequestId (\ s a -> s{_ssdersDataSetRequestId = a})

-- | -- | The response status code.
ssdersResponseStatus :: Lens' StartSupportDataExportResponse Int
ssdersResponseStatus = lens _ssdersResponseStatus (\ s a -> s{_ssdersResponseStatus = a})

instance NFData StartSupportDataExportResponse where
