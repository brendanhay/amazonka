{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.CreateDeliveryStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Kinesis Data Firehose delivery stream.
--
-- By default, you can create up to 50 delivery streams per AWS Region.
-- This is an asynchronous operation that immediately returns. The initial status of the delivery stream is @CREATING@ . After the delivery stream is created, its status is @ACTIVE@ and it now accepts data. If the delivery stream creation fails, the status transitions to @CREATING_FAILED@ . Attempts to send data to a delivery stream that is not in the @ACTIVE@ state cause an exception. To check the state of a delivery stream, use 'DescribeDeliveryStream' .
-- If the status of a delivery stream is @CREATING_FAILED@ , this status doesn't change, and you can't invoke @CreateDeliveryStream@ again on it. However, you can invoke the 'DeleteDeliveryStream' operation to delete it.
-- A Kinesis Data Firehose delivery stream can be configured to receive records directly from providers using 'PutRecord' or 'PutRecordBatch' , or it can be configured to use an existing Kinesis stream as its source. To specify a Kinesis data stream as input, set the @DeliveryStreamType@ parameter to @KinesisStreamAsSource@ , and provide the Kinesis stream Amazon Resource Name (ARN) and role ARN in the @KinesisStreamSourceConfiguration@ parameter.
-- To create a delivery stream with server-side encryption (SSE) enabled, include 'DeliveryStreamEncryptionConfigurationInput' in your request. This is optional. You can also invoke 'StartDeliveryStreamEncryption' to turn on SSE for an existing delivery stream that doesn't have SSE enabled.
-- A delivery stream is configured with a single destination: Amazon S3, Amazon ES, Amazon Redshift, or Splunk. You must specify only one of the following destination configuration parameters: @ExtendedS3DestinationConfiguration@ , @S3DestinationConfiguration@ , @ElasticsearchDestinationConfiguration@ , @RedshiftDestinationConfiguration@ , or @SplunkDestinationConfiguration@ .
-- When you specify @S3DestinationConfiguration@ , you can also provide the following optional values: BufferingHints, @EncryptionConfiguration@ , and @CompressionFormat@ . By default, if no @BufferingHints@ value is provided, Kinesis Data Firehose buffers data up to 5 MB or for 5 minutes, whichever condition is satisfied first. @BufferingHints@ is a hint, so there are some cases where the service cannot adhere to these conditions strictly. For example, record boundaries might be such that the size is a little over or under the configured buffering size. By default, no encryption is performed. We strongly recommend that you enable encryption to ensure secure data storage in Amazon S3.
-- A few notes about Amazon Redshift as a destination:
--
--     * An Amazon Redshift destination requires an S3 bucket as intermediate location. Kinesis Data Firehose first delivers data to Amazon S3 and then uses @COPY@ syntax to load data into an Amazon Redshift table. This is specified in the @RedshiftDestinationConfiguration.S3Configuration@ parameter.
--
--
--     * The compression formats @SNAPPY@ or @ZIP@ cannot be specified in @RedshiftDestinationConfiguration.S3Configuration@ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
--
--
--     * We strongly recommend that you use the user name and password you provide exclusively with Kinesis Data Firehose, and that the permissions for the account are restricted for Amazon Redshift @INSERT@ permissions.
--
--
-- Kinesis Data Firehose assumes the IAM role that is configured as part of the destination. The role should allow the Kinesis Data Firehose principal to assume the role, and the role should have permissions that allow the service to deliver the data. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination> in the /Amazon Kinesis Data Firehose Developer Guide/ .
module Network.AWS.Firehose.CreateDeliveryStream
    (
    -- * Creating a request
      CreateDeliveryStream (..)
    , mkCreateDeliveryStream
    -- ** Request lenses
    , cdsDeliveryStreamName
    , cdsDeliveryStreamEncryptionConfigurationInput
    , cdsDeliveryStreamType
    , cdsElasticsearchDestinationConfiguration
    , cdsExtendedS3DestinationConfiguration
    , cdsHttpEndpointDestinationConfiguration
    , cdsKinesisStreamSourceConfiguration
    , cdsRedshiftDestinationConfiguration
    , cdsS3DestinationConfiguration
    , cdsSplunkDestinationConfiguration
    , cdsTags

    -- * Destructuring the response
    , CreateDeliveryStreamResponse (..)
    , mkCreateDeliveryStreamResponse
    -- ** Response lenses
    , cdsrrsDeliveryStreamARN
    , cdsrrsResponseStatus
    ) where

import qualified Network.AWS.Firehose.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDeliveryStream' smart constructor.
data CreateDeliveryStream = CreateDeliveryStream'
  { deliveryStreamName :: Types.DeliveryStreamName
    -- ^ The name of the delivery stream. This name must be unique per AWS account in the same AWS Region. If the delivery streams are in different accounts or different Regions, you can have multiple delivery streams with the same name.
  , deliveryStreamEncryptionConfigurationInput :: Core.Maybe Types.DeliveryStreamEncryptionConfigurationInput
    -- ^ Used to specify the type and Amazon Resource Name (ARN) of the KMS key needed for Server-Side Encryption (SSE).
  , deliveryStreamType :: Core.Maybe Types.DeliveryStreamType
    -- ^ The delivery stream type. This parameter can be one of the following values:
--
--
--     * @DirectPut@ : Provider applications access the delivery stream directly.
--
--
--     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
--
--
  , elasticsearchDestinationConfiguration :: Core.Maybe Types.ElasticsearchDestinationConfiguration
    -- ^ The destination in Amazon ES. You can specify only one destination.
  , extendedS3DestinationConfiguration :: Core.Maybe Types.ExtendedS3DestinationConfiguration
    -- ^ The destination in Amazon S3. You can specify only one destination.
  , httpEndpointDestinationConfiguration :: Core.Maybe Types.HttpEndpointDestinationConfiguration
    -- ^ Enables configuring Kinesis Firehose to deliver data to any HTTP endpoint destination. You can specify only one destination.
  , kinesisStreamSourceConfiguration :: Core.Maybe Types.KinesisStreamSourceConfiguration
    -- ^ When a Kinesis data stream is used as the source for the delivery stream, a 'KinesisStreamSourceConfiguration' containing the Kinesis data stream Amazon Resource Name (ARN) and the role ARN for the source stream.
  , redshiftDestinationConfiguration :: Core.Maybe Types.RedshiftDestinationConfiguration
    -- ^ The destination in Amazon Redshift. You can specify only one destination.
  , s3DestinationConfiguration :: Core.Maybe Types.S3DestinationConfiguration
    -- ^ [Deprecated] The destination in Amazon S3. You can specify only one destination.
  , splunkDestinationConfiguration :: Core.Maybe Types.SplunkDestinationConfiguration
    -- ^ The destination in Splunk. You can specify only one destination.
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ A set of tags to assign to the delivery stream. A tag is a key-value pair that you can define and assign to AWS resources. Tags are metadata. For example, you can add friendly names and descriptions or other types of information that can help you distinguish the delivery stream. For more information about tags, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the AWS Billing and Cost Management User Guide.
--
-- You can specify up to 50 tags when creating a delivery stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDeliveryStream' value with any optional fields omitted.
mkCreateDeliveryStream
    :: Types.DeliveryStreamName -- ^ 'deliveryStreamName'
    -> CreateDeliveryStream
mkCreateDeliveryStream deliveryStreamName
  = CreateDeliveryStream'{deliveryStreamName,
                          deliveryStreamEncryptionConfigurationInput = Core.Nothing,
                          deliveryStreamType = Core.Nothing,
                          elasticsearchDestinationConfiguration = Core.Nothing,
                          extendedS3DestinationConfiguration = Core.Nothing,
                          httpEndpointDestinationConfiguration = Core.Nothing,
                          kinesisStreamSourceConfiguration = Core.Nothing,
                          redshiftDestinationConfiguration = Core.Nothing,
                          s3DestinationConfiguration = Core.Nothing,
                          splunkDestinationConfiguration = Core.Nothing, tags = Core.Nothing}

-- | The name of the delivery stream. This name must be unique per AWS account in the same AWS Region. If the delivery streams are in different accounts or different Regions, you can have multiple delivery streams with the same name.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDeliveryStreamName :: Lens.Lens' CreateDeliveryStream Types.DeliveryStreamName
cdsDeliveryStreamName = Lens.field @"deliveryStreamName"
{-# INLINEABLE cdsDeliveryStreamName #-}
{-# DEPRECATED deliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead"  #-}

-- | Used to specify the type and Amazon Resource Name (ARN) of the KMS key needed for Server-Side Encryption (SSE).
--
-- /Note:/ Consider using 'deliveryStreamEncryptionConfigurationInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDeliveryStreamEncryptionConfigurationInput :: Lens.Lens' CreateDeliveryStream (Core.Maybe Types.DeliveryStreamEncryptionConfigurationInput)
cdsDeliveryStreamEncryptionConfigurationInput = Lens.field @"deliveryStreamEncryptionConfigurationInput"
{-# INLINEABLE cdsDeliveryStreamEncryptionConfigurationInput #-}
{-# DEPRECATED deliveryStreamEncryptionConfigurationInput "Use generic-lens or generic-optics with 'deliveryStreamEncryptionConfigurationInput' instead"  #-}

-- | The delivery stream type. This parameter can be one of the following values:
--
--
--     * @DirectPut@ : Provider applications access the delivery stream directly.
--
--
--     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
--
--
--
-- /Note:/ Consider using 'deliveryStreamType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDeliveryStreamType :: Lens.Lens' CreateDeliveryStream (Core.Maybe Types.DeliveryStreamType)
cdsDeliveryStreamType = Lens.field @"deliveryStreamType"
{-# INLINEABLE cdsDeliveryStreamType #-}
{-# DEPRECATED deliveryStreamType "Use generic-lens or generic-optics with 'deliveryStreamType' instead"  #-}

-- | The destination in Amazon ES. You can specify only one destination.
--
-- /Note:/ Consider using 'elasticsearchDestinationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsElasticsearchDestinationConfiguration :: Lens.Lens' CreateDeliveryStream (Core.Maybe Types.ElasticsearchDestinationConfiguration)
cdsElasticsearchDestinationConfiguration = Lens.field @"elasticsearchDestinationConfiguration"
{-# INLINEABLE cdsElasticsearchDestinationConfiguration #-}
{-# DEPRECATED elasticsearchDestinationConfiguration "Use generic-lens or generic-optics with 'elasticsearchDestinationConfiguration' instead"  #-}

-- | The destination in Amazon S3. You can specify only one destination.
--
-- /Note:/ Consider using 'extendedS3DestinationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsExtendedS3DestinationConfiguration :: Lens.Lens' CreateDeliveryStream (Core.Maybe Types.ExtendedS3DestinationConfiguration)
cdsExtendedS3DestinationConfiguration = Lens.field @"extendedS3DestinationConfiguration"
{-# INLINEABLE cdsExtendedS3DestinationConfiguration #-}
{-# DEPRECATED extendedS3DestinationConfiguration "Use generic-lens or generic-optics with 'extendedS3DestinationConfiguration' instead"  #-}

-- | Enables configuring Kinesis Firehose to deliver data to any HTTP endpoint destination. You can specify only one destination.
--
-- /Note:/ Consider using 'httpEndpointDestinationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsHttpEndpointDestinationConfiguration :: Lens.Lens' CreateDeliveryStream (Core.Maybe Types.HttpEndpointDestinationConfiguration)
cdsHttpEndpointDestinationConfiguration = Lens.field @"httpEndpointDestinationConfiguration"
{-# INLINEABLE cdsHttpEndpointDestinationConfiguration #-}
{-# DEPRECATED httpEndpointDestinationConfiguration "Use generic-lens or generic-optics with 'httpEndpointDestinationConfiguration' instead"  #-}

-- | When a Kinesis data stream is used as the source for the delivery stream, a 'KinesisStreamSourceConfiguration' containing the Kinesis data stream Amazon Resource Name (ARN) and the role ARN for the source stream.
--
-- /Note:/ Consider using 'kinesisStreamSourceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsKinesisStreamSourceConfiguration :: Lens.Lens' CreateDeliveryStream (Core.Maybe Types.KinesisStreamSourceConfiguration)
cdsKinesisStreamSourceConfiguration = Lens.field @"kinesisStreamSourceConfiguration"
{-# INLINEABLE cdsKinesisStreamSourceConfiguration #-}
{-# DEPRECATED kinesisStreamSourceConfiguration "Use generic-lens or generic-optics with 'kinesisStreamSourceConfiguration' instead"  #-}

-- | The destination in Amazon Redshift. You can specify only one destination.
--
-- /Note:/ Consider using 'redshiftDestinationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsRedshiftDestinationConfiguration :: Lens.Lens' CreateDeliveryStream (Core.Maybe Types.RedshiftDestinationConfiguration)
cdsRedshiftDestinationConfiguration = Lens.field @"redshiftDestinationConfiguration"
{-# INLINEABLE cdsRedshiftDestinationConfiguration #-}
{-# DEPRECATED redshiftDestinationConfiguration "Use generic-lens or generic-optics with 'redshiftDestinationConfiguration' instead"  #-}

-- | [Deprecated] The destination in Amazon S3. You can specify only one destination.
--
-- /Note:/ Consider using 's3DestinationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsS3DestinationConfiguration :: Lens.Lens' CreateDeliveryStream (Core.Maybe Types.S3DestinationConfiguration)
cdsS3DestinationConfiguration = Lens.field @"s3DestinationConfiguration"
{-# INLINEABLE cdsS3DestinationConfiguration #-}
{-# DEPRECATED s3DestinationConfiguration "Use generic-lens or generic-optics with 's3DestinationConfiguration' instead"  #-}

-- | The destination in Splunk. You can specify only one destination.
--
-- /Note:/ Consider using 'splunkDestinationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsSplunkDestinationConfiguration :: Lens.Lens' CreateDeliveryStream (Core.Maybe Types.SplunkDestinationConfiguration)
cdsSplunkDestinationConfiguration = Lens.field @"splunkDestinationConfiguration"
{-# INLINEABLE cdsSplunkDestinationConfiguration #-}
{-# DEPRECATED splunkDestinationConfiguration "Use generic-lens or generic-optics with 'splunkDestinationConfiguration' instead"  #-}

-- | A set of tags to assign to the delivery stream. A tag is a key-value pair that you can define and assign to AWS resources. Tags are metadata. For example, you can add friendly names and descriptions or other types of information that can help you distinguish the delivery stream. For more information about tags, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the AWS Billing and Cost Management User Guide.
--
-- You can specify up to 50 tags when creating a delivery stream.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsTags :: Lens.Lens' CreateDeliveryStream (Core.Maybe (Core.NonEmpty Types.Tag))
cdsTags = Lens.field @"tags"
{-# INLINEABLE cdsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateDeliveryStream where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDeliveryStream where
        toHeaders CreateDeliveryStream{..}
          = Core.pure
              ("X-Amz-Target", "Firehose_20150804.CreateDeliveryStream")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDeliveryStream where
        toJSON CreateDeliveryStream{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DeliveryStreamName" Core..= deliveryStreamName),
                  ("DeliveryStreamEncryptionConfigurationInput" Core..=) Core.<$>
                    deliveryStreamEncryptionConfigurationInput,
                  ("DeliveryStreamType" Core..=) Core.<$> deliveryStreamType,
                  ("ElasticsearchDestinationConfiguration" Core..=) Core.<$>
                    elasticsearchDestinationConfiguration,
                  ("ExtendedS3DestinationConfiguration" Core..=) Core.<$>
                    extendedS3DestinationConfiguration,
                  ("HttpEndpointDestinationConfiguration" Core..=) Core.<$>
                    httpEndpointDestinationConfiguration,
                  ("KinesisStreamSourceConfiguration" Core..=) Core.<$>
                    kinesisStreamSourceConfiguration,
                  ("RedshiftDestinationConfiguration" Core..=) Core.<$>
                    redshiftDestinationConfiguration,
                  ("S3DestinationConfiguration" Core..=) Core.<$>
                    s3DestinationConfiguration,
                  ("SplunkDestinationConfiguration" Core..=) Core.<$>
                    splunkDestinationConfiguration,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateDeliveryStream where
        type Rs CreateDeliveryStream = CreateDeliveryStreamResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDeliveryStreamResponse' Core.<$>
                   (x Core..:? "DeliveryStreamARN") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDeliveryStreamResponse' smart constructor.
data CreateDeliveryStreamResponse = CreateDeliveryStreamResponse'
  { deliveryStreamARN :: Core.Maybe Types.DeliveryStreamARN
    -- ^ The ARN of the delivery stream.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDeliveryStreamResponse' value with any optional fields omitted.
mkCreateDeliveryStreamResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDeliveryStreamResponse
mkCreateDeliveryStreamResponse responseStatus
  = CreateDeliveryStreamResponse'{deliveryStreamARN = Core.Nothing,
                                  responseStatus}

-- | The ARN of the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrrsDeliveryStreamARN :: Lens.Lens' CreateDeliveryStreamResponse (Core.Maybe Types.DeliveryStreamARN)
cdsrrsDeliveryStreamARN = Lens.field @"deliveryStreamARN"
{-# INLINEABLE cdsrrsDeliveryStreamARN #-}
{-# DEPRECATED deliveryStreamARN "Use generic-lens or generic-optics with 'deliveryStreamARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrrsResponseStatus :: Lens.Lens' CreateDeliveryStreamResponse Core.Int
cdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
