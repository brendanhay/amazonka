{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceAnalytics.StartSupportDataExport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given a data set type and a from date, asynchronously publishes the requested customer support data to the specified S3 bucket and notifies the specified SNS topic once the data is available. Returns a unique request identifier that can be used to correlate requests with notifications from the SNS topic. Data sets will be published in comma-separated values (CSV) format with the file name {data_set_type}_YYYY-MM-DD'T'HH-mm-ss'Z'.csv. If a file with the same name already exists (e.g. if the same data set is requested twice), the original file will be overwritten by the new file. Requires a Role with an attached permissions policy providing Allow permissions for the following actions: s3:PutObject, s3:GetBucketLocation, sns:GetTopicAttributes, sns:Publish, iam:GetRolePolicy.
module Network.AWS.MarketplaceAnalytics.StartSupportDataExport
  ( -- * Creating a request
    StartSupportDataExport (..),
    mkStartSupportDataExport,

    -- ** Request lenses
    ssdeCustomerDefinedValues,
    ssdeDestinationS3Prefix,
    ssdeDataSetType,
    ssdeFromDate,
    ssdeRoleNameARN,
    ssdeDestinationS3BucketName,
    ssdeSnsTopicARN,

    -- * Destructuring the response
    StartSupportDataExportResponse (..),
    mkStartSupportDataExportResponse,

    -- ** Response lenses
    ssdersDataSetRequestId,
    ssdersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceAnalytics.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the StartSupportDataExport operation.
--
-- /See:/ 'mkStartSupportDataExport' smart constructor.
data StartSupportDataExport = StartSupportDataExport'
  { customerDefinedValues ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    destinationS3Prefix :: Lude.Maybe Lude.Text,
    dataSetType :: SupportDataSetType,
    fromDate :: Lude.Timestamp,
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

-- | Creates a value of 'StartSupportDataExport' with the minimum fields required to make a request.
--
-- * 'customerDefinedValues' - (Optional) Key-value pairs which will be returned, unmodified, in the Amazon SNS notification message and the data set metadata file.
-- * 'dataSetType' - Specifies the data set type to be written to the output csv file. The data set types customer_support_contacts_data and test_customer_support_contacts_data both result in a csv file containing the following fields: Product Id, Product Code, Customer Guid, Subscription Guid, Subscription Start Date, Organization, AWS Account Id, Given Name, Surname, Telephone Number, Email, Title, Country Code, ZIP Code, Operation Type, and Operation Time.
--
--
--     * /customer_support_contacts_data/ Customer support contact data. The data set will contain all changes (Creates, Updates, and Deletes) to customer support contact data from the date specified in the from_date parameter.
--
--     * /test_customer_support_contacts_data/ An example data set containing static test data in the same format as customer_support_contacts_data
--
--
-- * 'destinationS3BucketName' - The name (friendly name, not ARN) of the destination S3 bucket.
-- * 'destinationS3Prefix' - (Optional) The desired S3 prefix for the published data set, similar to a directory path in standard file systems. For example, if given the bucket name "mybucket" and the prefix "myprefix/mydatasets", the output file "outputfile" would be published to "s3://mybucket/myprefix/mydatasets/outputfile". If the prefix directory structure does not exist, it will be created. If no prefix is provided, the data set will be published to the S3 bucket root.
-- * 'fromDate' - The start date from which to retrieve the data set in UTC. This parameter only affects the customer_support_contacts_data data set type.
-- * 'roleNameARN' - The Amazon Resource Name (ARN) of the Role with an attached permissions policy to interact with the provided AWS services.
-- * 'snsTopicARN' - Amazon Resource Name (ARN) for the SNS Topic that will be notified when the data set has been published or if an error has occurred.
mkStartSupportDataExport ::
  -- | 'dataSetType'
  SupportDataSetType ->
  -- | 'fromDate'
  Lude.Timestamp ->
  -- | 'roleNameARN'
  Lude.Text ->
  -- | 'destinationS3BucketName'
  Lude.Text ->
  -- | 'snsTopicARN'
  Lude.Text ->
  StartSupportDataExport
mkStartSupportDataExport
  pDataSetType_
  pFromDate_
  pRoleNameARN_
  pDestinationS3BucketName_
  pSnsTopicARN_ =
    StartSupportDataExport'
      { customerDefinedValues = Lude.Nothing,
        destinationS3Prefix = Lude.Nothing,
        dataSetType = pDataSetType_,
        fromDate = pFromDate_,
        roleNameARN = pRoleNameARN_,
        destinationS3BucketName = pDestinationS3BucketName_,
        snsTopicARN = pSnsTopicARN_
      }

-- | (Optional) Key-value pairs which will be returned, unmodified, in the Amazon SNS notification message and the data set metadata file.
--
-- /Note:/ Consider using 'customerDefinedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdeCustomerDefinedValues :: Lens.Lens' StartSupportDataExport (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ssdeCustomerDefinedValues = Lens.lens (customerDefinedValues :: StartSupportDataExport -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {customerDefinedValues = a} :: StartSupportDataExport)
{-# DEPRECATED ssdeCustomerDefinedValues "Use generic-lens or generic-optics with 'customerDefinedValues' instead." #-}

-- | (Optional) The desired S3 prefix for the published data set, similar to a directory path in standard file systems. For example, if given the bucket name "mybucket" and the prefix "myprefix/mydatasets", the output file "outputfile" would be published to "s3://mybucket/myprefix/mydatasets/outputfile". If the prefix directory structure does not exist, it will be created. If no prefix is provided, the data set will be published to the S3 bucket root.
--
-- /Note:/ Consider using 'destinationS3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdeDestinationS3Prefix :: Lens.Lens' StartSupportDataExport (Lude.Maybe Lude.Text)
ssdeDestinationS3Prefix = Lens.lens (destinationS3Prefix :: StartSupportDataExport -> Lude.Maybe Lude.Text) (\s a -> s {destinationS3Prefix = a} :: StartSupportDataExport)
{-# DEPRECATED ssdeDestinationS3Prefix "Use generic-lens or generic-optics with 'destinationS3Prefix' instead." #-}

-- | Specifies the data set type to be written to the output csv file. The data set types customer_support_contacts_data and test_customer_support_contacts_data both result in a csv file containing the following fields: Product Id, Product Code, Customer Guid, Subscription Guid, Subscription Start Date, Organization, AWS Account Id, Given Name, Surname, Telephone Number, Email, Title, Country Code, ZIP Code, Operation Type, and Operation Time.
--
--
--     * /customer_support_contacts_data/ Customer support contact data. The data set will contain all changes (Creates, Updates, and Deletes) to customer support contact data from the date specified in the from_date parameter.
--
--     * /test_customer_support_contacts_data/ An example data set containing static test data in the same format as customer_support_contacts_data
--
--
--
-- /Note:/ Consider using 'dataSetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdeDataSetType :: Lens.Lens' StartSupportDataExport SupportDataSetType
ssdeDataSetType = Lens.lens (dataSetType :: StartSupportDataExport -> SupportDataSetType) (\s a -> s {dataSetType = a} :: StartSupportDataExport)
{-# DEPRECATED ssdeDataSetType "Use generic-lens or generic-optics with 'dataSetType' instead." #-}

-- | The start date from which to retrieve the data set in UTC. This parameter only affects the customer_support_contacts_data data set type.
--
-- /Note:/ Consider using 'fromDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdeFromDate :: Lens.Lens' StartSupportDataExport Lude.Timestamp
ssdeFromDate = Lens.lens (fromDate :: StartSupportDataExport -> Lude.Timestamp) (\s a -> s {fromDate = a} :: StartSupportDataExport)
{-# DEPRECATED ssdeFromDate "Use generic-lens or generic-optics with 'fromDate' instead." #-}

-- | The Amazon Resource Name (ARN) of the Role with an attached permissions policy to interact with the provided AWS services.
--
-- /Note:/ Consider using 'roleNameARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdeRoleNameARN :: Lens.Lens' StartSupportDataExport Lude.Text
ssdeRoleNameARN = Lens.lens (roleNameARN :: StartSupportDataExport -> Lude.Text) (\s a -> s {roleNameARN = a} :: StartSupportDataExport)
{-# DEPRECATED ssdeRoleNameARN "Use generic-lens or generic-optics with 'roleNameARN' instead." #-}

-- | The name (friendly name, not ARN) of the destination S3 bucket.
--
-- /Note:/ Consider using 'destinationS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdeDestinationS3BucketName :: Lens.Lens' StartSupportDataExport Lude.Text
ssdeDestinationS3BucketName = Lens.lens (destinationS3BucketName :: StartSupportDataExport -> Lude.Text) (\s a -> s {destinationS3BucketName = a} :: StartSupportDataExport)
{-# DEPRECATED ssdeDestinationS3BucketName "Use generic-lens or generic-optics with 'destinationS3BucketName' instead." #-}

-- | Amazon Resource Name (ARN) for the SNS Topic that will be notified when the data set has been published or if an error has occurred.
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdeSnsTopicARN :: Lens.Lens' StartSupportDataExport Lude.Text
ssdeSnsTopicARN = Lens.lens (snsTopicARN :: StartSupportDataExport -> Lude.Text) (\s a -> s {snsTopicARN = a} :: StartSupportDataExport)
{-# DEPRECATED ssdeSnsTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

instance Lude.AWSRequest StartSupportDataExport where
  type Rs StartSupportDataExport = StartSupportDataExportResponse
  request = Req.postJSON marketplaceAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartSupportDataExportResponse'
            Lude.<$> (x Lude..?> "dataSetRequestId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartSupportDataExport where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MarketplaceCommerceAnalytics20150701.StartSupportDataExport" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartSupportDataExport where
  toJSON StartSupportDataExport' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("customerDefinedValues" Lude..=) Lude.<$> customerDefinedValues,
            ("destinationS3Prefix" Lude..=) Lude.<$> destinationS3Prefix,
            Lude.Just ("dataSetType" Lude..= dataSetType),
            Lude.Just ("fromDate" Lude..= fromDate),
            Lude.Just ("roleNameArn" Lude..= roleNameARN),
            Lude.Just
              ("destinationS3BucketName" Lude..= destinationS3BucketName),
            Lude.Just ("snsTopicArn" Lude..= snsTopicARN)
          ]
      )

instance Lude.ToPath StartSupportDataExport where
  toPath = Lude.const "/"

instance Lude.ToQuery StartSupportDataExport where
  toQuery = Lude.const Lude.mempty

-- | Container for the result of the StartSupportDataExport operation.
--
-- /See:/ 'mkStartSupportDataExportResponse' smart constructor.
data StartSupportDataExportResponse = StartSupportDataExportResponse'
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

-- | Creates a value of 'StartSupportDataExportResponse' with the minimum fields required to make a request.
--
-- * 'dataSetRequestId' - A unique identifier representing a specific request to the StartSupportDataExport operation. This identifier can be used to correlate a request with notifications from the SNS topic.
-- * 'responseStatus' - The response status code.
mkStartSupportDataExportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartSupportDataExportResponse
mkStartSupportDataExportResponse pResponseStatus_ =
  StartSupportDataExportResponse'
    { dataSetRequestId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique identifier representing a specific request to the StartSupportDataExport operation. This identifier can be used to correlate a request with notifications from the SNS topic.
--
-- /Note:/ Consider using 'dataSetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdersDataSetRequestId :: Lens.Lens' StartSupportDataExportResponse (Lude.Maybe Lude.Text)
ssdersDataSetRequestId = Lens.lens (dataSetRequestId :: StartSupportDataExportResponse -> Lude.Maybe Lude.Text) (\s a -> s {dataSetRequestId = a} :: StartSupportDataExportResponse)
{-# DEPRECATED ssdersDataSetRequestId "Use generic-lens or generic-optics with 'dataSetRequestId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdersResponseStatus :: Lens.Lens' StartSupportDataExportResponse Lude.Int
ssdersResponseStatus = Lens.lens (responseStatus :: StartSupportDataExportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartSupportDataExportResponse)
{-# DEPRECATED ssdersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
