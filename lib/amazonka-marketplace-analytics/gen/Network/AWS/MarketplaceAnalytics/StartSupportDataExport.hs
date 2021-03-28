{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      StartSupportDataExport (..)
    , mkStartSupportDataExport
    -- ** Request lenses
    , ssdeDataSetType
    , ssdeFromDate
    , ssdeRoleNameArn
    , ssdeDestinationS3BucketName
    , ssdeSnsTopicArn
    , ssdeCustomerDefinedValues
    , ssdeDestinationS3Prefix

    -- * Destructuring the response
    , StartSupportDataExportResponse (..)
    , mkStartSupportDataExportResponse
    -- ** Response lenses
    , ssderrsDataSetRequestId
    , ssderrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MarketplaceAnalytics.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the StartSupportDataExport operation.
--
-- /See:/ 'mkStartSupportDataExport' smart constructor.
data StartSupportDataExport = StartSupportDataExport'
  { dataSetType :: Types.SupportDataSetType
    -- ^ Specifies the data set type to be written to the output csv file. The data set types customer_support_contacts_data and test_customer_support_contacts_data both result in a csv file containing the following fields: Product Id, Product Code, Customer Guid, Subscription Guid, Subscription Start Date, Organization, AWS Account Id, Given Name, Surname, Telephone Number, Email, Title, Country Code, ZIP Code, Operation Type, and Operation Time. 
--
--
--     * /customer_support_contacts_data/ Customer support contact data. The data set will contain all changes (Creates, Updates, and Deletes) to customer support contact data from the date specified in the from_date parameter.
--
--     * /test_customer_support_contacts_data/ An example data set containing static test data in the same format as customer_support_contacts_data
--
--
  , fromDate :: Core.NominalDiffTime
    -- ^ The start date from which to retrieve the data set in UTC. This parameter only affects the customer_support_contacts_data data set type.
  , roleNameArn :: Types.RoleNameArn
    -- ^ The Amazon Resource Name (ARN) of the Role with an attached permissions policy to interact with the provided AWS services.
  , destinationS3BucketName :: Types.DestinationS3BucketName
    -- ^ The name (friendly name, not ARN) of the destination S3 bucket.
  , snsTopicArn :: Types.SnsTopicArn
    -- ^ Amazon Resource Name (ARN) for the SNS Topic that will be notified when the data set has been published or if an error has occurred.
  , customerDefinedValues :: Core.Maybe (Core.HashMap Types.OptionalKey Types.OptionalValue)
    -- ^ (Optional) Key-value pairs which will be returned, unmodified, in the Amazon SNS notification message and the data set metadata file.
  , destinationS3Prefix :: Core.Maybe Types.DestinationS3Prefix
    -- ^ (Optional) The desired S3 prefix for the published data set, similar to a directory path in standard file systems. For example, if given the bucket name "mybucket" and the prefix "myprefix/mydatasets", the output file "outputfile" would be published to "s3://mybucket/myprefix/mydatasets/outputfile". If the prefix directory structure does not exist, it will be created. If no prefix is provided, the data set will be published to the S3 bucket root.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartSupportDataExport' value with any optional fields omitted.
mkStartSupportDataExport
    :: Types.SupportDataSetType -- ^ 'dataSetType'
    -> Core.NominalDiffTime -- ^ 'fromDate'
    -> Types.RoleNameArn -- ^ 'roleNameArn'
    -> Types.DestinationS3BucketName -- ^ 'destinationS3BucketName'
    -> Types.SnsTopicArn -- ^ 'snsTopicArn'
    -> StartSupportDataExport
mkStartSupportDataExport dataSetType fromDate roleNameArn
  destinationS3BucketName snsTopicArn
  = StartSupportDataExport'{dataSetType, fromDate, roleNameArn,
                            destinationS3BucketName, snsTopicArn,
                            customerDefinedValues = Core.Nothing,
                            destinationS3Prefix = Core.Nothing}

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
ssdeDataSetType :: Lens.Lens' StartSupportDataExport Types.SupportDataSetType
ssdeDataSetType = Lens.field @"dataSetType"
{-# INLINEABLE ssdeDataSetType #-}
{-# DEPRECATED dataSetType "Use generic-lens or generic-optics with 'dataSetType' instead"  #-}

-- | The start date from which to retrieve the data set in UTC. This parameter only affects the customer_support_contacts_data data set type.
--
-- /Note:/ Consider using 'fromDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdeFromDate :: Lens.Lens' StartSupportDataExport Core.NominalDiffTime
ssdeFromDate = Lens.field @"fromDate"
{-# INLINEABLE ssdeFromDate #-}
{-# DEPRECATED fromDate "Use generic-lens or generic-optics with 'fromDate' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Role with an attached permissions policy to interact with the provided AWS services.
--
-- /Note:/ Consider using 'roleNameArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdeRoleNameArn :: Lens.Lens' StartSupportDataExport Types.RoleNameArn
ssdeRoleNameArn = Lens.field @"roleNameArn"
{-# INLINEABLE ssdeRoleNameArn #-}
{-# DEPRECATED roleNameArn "Use generic-lens or generic-optics with 'roleNameArn' instead"  #-}

-- | The name (friendly name, not ARN) of the destination S3 bucket.
--
-- /Note:/ Consider using 'destinationS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdeDestinationS3BucketName :: Lens.Lens' StartSupportDataExport Types.DestinationS3BucketName
ssdeDestinationS3BucketName = Lens.field @"destinationS3BucketName"
{-# INLINEABLE ssdeDestinationS3BucketName #-}
{-# DEPRECATED destinationS3BucketName "Use generic-lens or generic-optics with 'destinationS3BucketName' instead"  #-}

-- | Amazon Resource Name (ARN) for the SNS Topic that will be notified when the data set has been published or if an error has occurred.
--
-- /Note:/ Consider using 'snsTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdeSnsTopicArn :: Lens.Lens' StartSupportDataExport Types.SnsTopicArn
ssdeSnsTopicArn = Lens.field @"snsTopicArn"
{-# INLINEABLE ssdeSnsTopicArn #-}
{-# DEPRECATED snsTopicArn "Use generic-lens or generic-optics with 'snsTopicArn' instead"  #-}

-- | (Optional) Key-value pairs which will be returned, unmodified, in the Amazon SNS notification message and the data set metadata file.
--
-- /Note:/ Consider using 'customerDefinedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdeCustomerDefinedValues :: Lens.Lens' StartSupportDataExport (Core.Maybe (Core.HashMap Types.OptionalKey Types.OptionalValue))
ssdeCustomerDefinedValues = Lens.field @"customerDefinedValues"
{-# INLINEABLE ssdeCustomerDefinedValues #-}
{-# DEPRECATED customerDefinedValues "Use generic-lens or generic-optics with 'customerDefinedValues' instead"  #-}

-- | (Optional) The desired S3 prefix for the published data set, similar to a directory path in standard file systems. For example, if given the bucket name "mybucket" and the prefix "myprefix/mydatasets", the output file "outputfile" would be published to "s3://mybucket/myprefix/mydatasets/outputfile". If the prefix directory structure does not exist, it will be created. If no prefix is provided, the data set will be published to the S3 bucket root.
--
-- /Note:/ Consider using 'destinationS3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdeDestinationS3Prefix :: Lens.Lens' StartSupportDataExport (Core.Maybe Types.DestinationS3Prefix)
ssdeDestinationS3Prefix = Lens.field @"destinationS3Prefix"
{-# INLINEABLE ssdeDestinationS3Prefix #-}
{-# DEPRECATED destinationS3Prefix "Use generic-lens or generic-optics with 'destinationS3Prefix' instead"  #-}

instance Core.ToQuery StartSupportDataExport where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartSupportDataExport where
        toHeaders StartSupportDataExport{..}
          = Core.pure
              ("X-Amz-Target",
               "MarketplaceCommerceAnalytics20150701.StartSupportDataExport")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartSupportDataExport where
        toJSON StartSupportDataExport{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("dataSetType" Core..= dataSetType),
                  Core.Just ("fromDate" Core..= fromDate),
                  Core.Just ("roleNameArn" Core..= roleNameArn),
                  Core.Just
                    ("destinationS3BucketName" Core..= destinationS3BucketName),
                  Core.Just ("snsTopicArn" Core..= snsTopicArn),
                  ("customerDefinedValues" Core..=) Core.<$> customerDefinedValues,
                  ("destinationS3Prefix" Core..=) Core.<$> destinationS3Prefix])

instance Core.AWSRequest StartSupportDataExport where
        type Rs StartSupportDataExport = StartSupportDataExportResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartSupportDataExportResponse' Core.<$>
                   (x Core..:? "dataSetRequestId") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Container for the result of the StartSupportDataExport operation.
--
-- /See:/ 'mkStartSupportDataExportResponse' smart constructor.
data StartSupportDataExportResponse = StartSupportDataExportResponse'
  { dataSetRequestId :: Core.Maybe Types.DataSetRequestId
    -- ^ A unique identifier representing a specific request to the StartSupportDataExport operation. This identifier can be used to correlate a request with notifications from the SNS topic.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSupportDataExportResponse' value with any optional fields omitted.
mkStartSupportDataExportResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartSupportDataExportResponse
mkStartSupportDataExportResponse responseStatus
  = StartSupportDataExportResponse'{dataSetRequestId = Core.Nothing,
                                    responseStatus}

-- | A unique identifier representing a specific request to the StartSupportDataExport operation. This identifier can be used to correlate a request with notifications from the SNS topic.
--
-- /Note:/ Consider using 'dataSetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssderrsDataSetRequestId :: Lens.Lens' StartSupportDataExportResponse (Core.Maybe Types.DataSetRequestId)
ssderrsDataSetRequestId = Lens.field @"dataSetRequestId"
{-# INLINEABLE ssderrsDataSetRequestId #-}
{-# DEPRECATED dataSetRequestId "Use generic-lens or generic-optics with 'dataSetRequestId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssderrsResponseStatus :: Lens.Lens' StartSupportDataExportResponse Core.Int
ssderrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ssderrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
