{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateDeliveryStream (..),
    mkCreateDeliveryStream,

    -- ** Request lenses
    cdsS3DestinationConfiguration,
    cdsRedshiftDestinationConfiguration,
    cdsElasticsearchDestinationConfiguration,
    cdsExtendedS3DestinationConfiguration,
    cdsKinesisStreamSourceConfiguration,
    cdsHTTPEndpointDestinationConfiguration,
    cdsDeliveryStreamName,
    cdsDeliveryStreamType,
    cdsSplunkDestinationConfiguration,
    cdsTags,
    cdsDeliveryStreamEncryptionConfigurationInput,

    -- * Destructuring the response
    CreateDeliveryStreamResponse (..),
    mkCreateDeliveryStreamResponse,

    -- ** Response lenses
    cdsrsDeliveryStreamARN,
    cdsrsResponseStatus,
  )
where

import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDeliveryStream' smart constructor.
data CreateDeliveryStream = CreateDeliveryStream'
  { -- | [Deprecated] The destination in Amazon S3. You can specify only one destination.
    s3DestinationConfiguration :: Lude.Maybe S3DestinationConfiguration,
    -- | The destination in Amazon Redshift. You can specify only one destination.
    redshiftDestinationConfiguration :: Lude.Maybe RedshiftDestinationConfiguration,
    -- | The destination in Amazon ES. You can specify only one destination.
    elasticsearchDestinationConfiguration :: Lude.Maybe ElasticsearchDestinationConfiguration,
    -- | The destination in Amazon S3. You can specify only one destination.
    extendedS3DestinationConfiguration :: Lude.Maybe ExtendedS3DestinationConfiguration,
    -- | When a Kinesis data stream is used as the source for the delivery stream, a 'KinesisStreamSourceConfiguration' containing the Kinesis data stream Amazon Resource Name (ARN) and the role ARN for the source stream.
    kinesisStreamSourceConfiguration :: Lude.Maybe KinesisStreamSourceConfiguration,
    -- | Enables configuring Kinesis Firehose to deliver data to any HTTP endpoint destination. You can specify only one destination.
    hTTPEndpointDestinationConfiguration :: Lude.Maybe HTTPEndpointDestinationConfiguration,
    -- | The name of the delivery stream. This name must be unique per AWS account in the same AWS Region. If the delivery streams are in different accounts or different Regions, you can have multiple delivery streams with the same name.
    deliveryStreamName :: Lude.Text,
    -- | The delivery stream type. This parameter can be one of the following values:
    --
    --
    --     * @DirectPut@ : Provider applications access the delivery stream directly.
    --
    --
    --     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
    deliveryStreamType :: Lude.Maybe DeliveryStreamType,
    -- | The destination in Splunk. You can specify only one destination.
    splunkDestinationConfiguration :: Lude.Maybe SplunkDestinationConfiguration,
    -- | A set of tags to assign to the delivery stream. A tag is a key-value pair that you can define and assign to AWS resources. Tags are metadata. For example, you can add friendly names and descriptions or other types of information that can help you distinguish the delivery stream. For more information about tags, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the AWS Billing and Cost Management User Guide.
    --
    -- You can specify up to 50 tags when creating a delivery stream.
    tags :: Lude.Maybe (Lude.NonEmpty Tag),
    -- | Used to specify the type and Amazon Resource Name (ARN) of the KMS key needed for Server-Side Encryption (SSE).
    deliveryStreamEncryptionConfigurationInput :: Lude.Maybe DeliveryStreamEncryptionConfigurationInput
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDeliveryStream' with the minimum fields required to make a request.
--
-- * 's3DestinationConfiguration' - [Deprecated] The destination in Amazon S3. You can specify only one destination.
-- * 'redshiftDestinationConfiguration' - The destination in Amazon Redshift. You can specify only one destination.
-- * 'elasticsearchDestinationConfiguration' - The destination in Amazon ES. You can specify only one destination.
-- * 'extendedS3DestinationConfiguration' - The destination in Amazon S3. You can specify only one destination.
-- * 'kinesisStreamSourceConfiguration' - When a Kinesis data stream is used as the source for the delivery stream, a 'KinesisStreamSourceConfiguration' containing the Kinesis data stream Amazon Resource Name (ARN) and the role ARN for the source stream.
-- * 'hTTPEndpointDestinationConfiguration' - Enables configuring Kinesis Firehose to deliver data to any HTTP endpoint destination. You can specify only one destination.
-- * 'deliveryStreamName' - The name of the delivery stream. This name must be unique per AWS account in the same AWS Region. If the delivery streams are in different accounts or different Regions, you can have multiple delivery streams with the same name.
-- * 'deliveryStreamType' - The delivery stream type. This parameter can be one of the following values:
--
--
--     * @DirectPut@ : Provider applications access the delivery stream directly.
--
--
--     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
--
--
-- * 'splunkDestinationConfiguration' - The destination in Splunk. You can specify only one destination.
-- * 'tags' - A set of tags to assign to the delivery stream. A tag is a key-value pair that you can define and assign to AWS resources. Tags are metadata. For example, you can add friendly names and descriptions or other types of information that can help you distinguish the delivery stream. For more information about tags, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the AWS Billing and Cost Management User Guide.
--
-- You can specify up to 50 tags when creating a delivery stream.
-- * 'deliveryStreamEncryptionConfigurationInput' - Used to specify the type and Amazon Resource Name (ARN) of the KMS key needed for Server-Side Encryption (SSE).
mkCreateDeliveryStream ::
  -- | 'deliveryStreamName'
  Lude.Text ->
  CreateDeliveryStream
mkCreateDeliveryStream pDeliveryStreamName_ =
  CreateDeliveryStream'
    { s3DestinationConfiguration = Lude.Nothing,
      redshiftDestinationConfiguration = Lude.Nothing,
      elasticsearchDestinationConfiguration = Lude.Nothing,
      extendedS3DestinationConfiguration = Lude.Nothing,
      kinesisStreamSourceConfiguration = Lude.Nothing,
      hTTPEndpointDestinationConfiguration = Lude.Nothing,
      deliveryStreamName = pDeliveryStreamName_,
      deliveryStreamType = Lude.Nothing,
      splunkDestinationConfiguration = Lude.Nothing,
      tags = Lude.Nothing,
      deliveryStreamEncryptionConfigurationInput = Lude.Nothing
    }

-- | [Deprecated] The destination in Amazon S3. You can specify only one destination.
--
-- /Note:/ Consider using 's3DestinationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsS3DestinationConfiguration :: Lens.Lens' CreateDeliveryStream (Lude.Maybe S3DestinationConfiguration)
cdsS3DestinationConfiguration = Lens.lens (s3DestinationConfiguration :: CreateDeliveryStream -> Lude.Maybe S3DestinationConfiguration) (\s a -> s {s3DestinationConfiguration = a} :: CreateDeliveryStream)
{-# DEPRECATED cdsS3DestinationConfiguration "Use generic-lens or generic-optics with 's3DestinationConfiguration' instead." #-}

-- | The destination in Amazon Redshift. You can specify only one destination.
--
-- /Note:/ Consider using 'redshiftDestinationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsRedshiftDestinationConfiguration :: Lens.Lens' CreateDeliveryStream (Lude.Maybe RedshiftDestinationConfiguration)
cdsRedshiftDestinationConfiguration = Lens.lens (redshiftDestinationConfiguration :: CreateDeliveryStream -> Lude.Maybe RedshiftDestinationConfiguration) (\s a -> s {redshiftDestinationConfiguration = a} :: CreateDeliveryStream)
{-# DEPRECATED cdsRedshiftDestinationConfiguration "Use generic-lens or generic-optics with 'redshiftDestinationConfiguration' instead." #-}

-- | The destination in Amazon ES. You can specify only one destination.
--
-- /Note:/ Consider using 'elasticsearchDestinationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsElasticsearchDestinationConfiguration :: Lens.Lens' CreateDeliveryStream (Lude.Maybe ElasticsearchDestinationConfiguration)
cdsElasticsearchDestinationConfiguration = Lens.lens (elasticsearchDestinationConfiguration :: CreateDeliveryStream -> Lude.Maybe ElasticsearchDestinationConfiguration) (\s a -> s {elasticsearchDestinationConfiguration = a} :: CreateDeliveryStream)
{-# DEPRECATED cdsElasticsearchDestinationConfiguration "Use generic-lens or generic-optics with 'elasticsearchDestinationConfiguration' instead." #-}

-- | The destination in Amazon S3. You can specify only one destination.
--
-- /Note:/ Consider using 'extendedS3DestinationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsExtendedS3DestinationConfiguration :: Lens.Lens' CreateDeliveryStream (Lude.Maybe ExtendedS3DestinationConfiguration)
cdsExtendedS3DestinationConfiguration = Lens.lens (extendedS3DestinationConfiguration :: CreateDeliveryStream -> Lude.Maybe ExtendedS3DestinationConfiguration) (\s a -> s {extendedS3DestinationConfiguration = a} :: CreateDeliveryStream)
{-# DEPRECATED cdsExtendedS3DestinationConfiguration "Use generic-lens or generic-optics with 'extendedS3DestinationConfiguration' instead." #-}

-- | When a Kinesis data stream is used as the source for the delivery stream, a 'KinesisStreamSourceConfiguration' containing the Kinesis data stream Amazon Resource Name (ARN) and the role ARN for the source stream.
--
-- /Note:/ Consider using 'kinesisStreamSourceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsKinesisStreamSourceConfiguration :: Lens.Lens' CreateDeliveryStream (Lude.Maybe KinesisStreamSourceConfiguration)
cdsKinesisStreamSourceConfiguration = Lens.lens (kinesisStreamSourceConfiguration :: CreateDeliveryStream -> Lude.Maybe KinesisStreamSourceConfiguration) (\s a -> s {kinesisStreamSourceConfiguration = a} :: CreateDeliveryStream)
{-# DEPRECATED cdsKinesisStreamSourceConfiguration "Use generic-lens or generic-optics with 'kinesisStreamSourceConfiguration' instead." #-}

-- | Enables configuring Kinesis Firehose to deliver data to any HTTP endpoint destination. You can specify only one destination.
--
-- /Note:/ Consider using 'hTTPEndpointDestinationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsHTTPEndpointDestinationConfiguration :: Lens.Lens' CreateDeliveryStream (Lude.Maybe HTTPEndpointDestinationConfiguration)
cdsHTTPEndpointDestinationConfiguration = Lens.lens (hTTPEndpointDestinationConfiguration :: CreateDeliveryStream -> Lude.Maybe HTTPEndpointDestinationConfiguration) (\s a -> s {hTTPEndpointDestinationConfiguration = a} :: CreateDeliveryStream)
{-# DEPRECATED cdsHTTPEndpointDestinationConfiguration "Use generic-lens or generic-optics with 'hTTPEndpointDestinationConfiguration' instead." #-}

-- | The name of the delivery stream. This name must be unique per AWS account in the same AWS Region. If the delivery streams are in different accounts or different Regions, you can have multiple delivery streams with the same name.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDeliveryStreamName :: Lens.Lens' CreateDeliveryStream Lude.Text
cdsDeliveryStreamName = Lens.lens (deliveryStreamName :: CreateDeliveryStream -> Lude.Text) (\s a -> s {deliveryStreamName = a} :: CreateDeliveryStream)
{-# DEPRECATED cdsDeliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead." #-}

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
cdsDeliveryStreamType :: Lens.Lens' CreateDeliveryStream (Lude.Maybe DeliveryStreamType)
cdsDeliveryStreamType = Lens.lens (deliveryStreamType :: CreateDeliveryStream -> Lude.Maybe DeliveryStreamType) (\s a -> s {deliveryStreamType = a} :: CreateDeliveryStream)
{-# DEPRECATED cdsDeliveryStreamType "Use generic-lens or generic-optics with 'deliveryStreamType' instead." #-}

-- | The destination in Splunk. You can specify only one destination.
--
-- /Note:/ Consider using 'splunkDestinationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsSplunkDestinationConfiguration :: Lens.Lens' CreateDeliveryStream (Lude.Maybe SplunkDestinationConfiguration)
cdsSplunkDestinationConfiguration = Lens.lens (splunkDestinationConfiguration :: CreateDeliveryStream -> Lude.Maybe SplunkDestinationConfiguration) (\s a -> s {splunkDestinationConfiguration = a} :: CreateDeliveryStream)
{-# DEPRECATED cdsSplunkDestinationConfiguration "Use generic-lens or generic-optics with 'splunkDestinationConfiguration' instead." #-}

-- | A set of tags to assign to the delivery stream. A tag is a key-value pair that you can define and assign to AWS resources. Tags are metadata. For example, you can add friendly names and descriptions or other types of information that can help you distinguish the delivery stream. For more information about tags, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the AWS Billing and Cost Management User Guide.
--
-- You can specify up to 50 tags when creating a delivery stream.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsTags :: Lens.Lens' CreateDeliveryStream (Lude.Maybe (Lude.NonEmpty Tag))
cdsTags = Lens.lens (tags :: CreateDeliveryStream -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateDeliveryStream)
{-# DEPRECATED cdsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Used to specify the type and Amazon Resource Name (ARN) of the KMS key needed for Server-Side Encryption (SSE).
--
-- /Note:/ Consider using 'deliveryStreamEncryptionConfigurationInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDeliveryStreamEncryptionConfigurationInput :: Lens.Lens' CreateDeliveryStream (Lude.Maybe DeliveryStreamEncryptionConfigurationInput)
cdsDeliveryStreamEncryptionConfigurationInput = Lens.lens (deliveryStreamEncryptionConfigurationInput :: CreateDeliveryStream -> Lude.Maybe DeliveryStreamEncryptionConfigurationInput) (\s a -> s {deliveryStreamEncryptionConfigurationInput = a} :: CreateDeliveryStream)
{-# DEPRECATED cdsDeliveryStreamEncryptionConfigurationInput "Use generic-lens or generic-optics with 'deliveryStreamEncryptionConfigurationInput' instead." #-}

instance Lude.AWSRequest CreateDeliveryStream where
  type Rs CreateDeliveryStream = CreateDeliveryStreamResponse
  request = Req.postJSON firehoseService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDeliveryStreamResponse'
            Lude.<$> (x Lude..?> "DeliveryStreamARN")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDeliveryStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Firehose_20150804.CreateDeliveryStream" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDeliveryStream where
  toJSON CreateDeliveryStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3DestinationConfiguration" Lude..=)
              Lude.<$> s3DestinationConfiguration,
            ("RedshiftDestinationConfiguration" Lude..=)
              Lude.<$> redshiftDestinationConfiguration,
            ("ElasticsearchDestinationConfiguration" Lude..=)
              Lude.<$> elasticsearchDestinationConfiguration,
            ("ExtendedS3DestinationConfiguration" Lude..=)
              Lude.<$> extendedS3DestinationConfiguration,
            ("KinesisStreamSourceConfiguration" Lude..=)
              Lude.<$> kinesisStreamSourceConfiguration,
            ("HttpEndpointDestinationConfiguration" Lude..=)
              Lude.<$> hTTPEndpointDestinationConfiguration,
            Lude.Just ("DeliveryStreamName" Lude..= deliveryStreamName),
            ("DeliveryStreamType" Lude..=) Lude.<$> deliveryStreamType,
            ("SplunkDestinationConfiguration" Lude..=)
              Lude.<$> splunkDestinationConfiguration,
            ("Tags" Lude..=) Lude.<$> tags,
            ("DeliveryStreamEncryptionConfigurationInput" Lude..=)
              Lude.<$> deliveryStreamEncryptionConfigurationInput
          ]
      )

instance Lude.ToPath CreateDeliveryStream where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDeliveryStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDeliveryStreamResponse' smart constructor.
data CreateDeliveryStreamResponse = CreateDeliveryStreamResponse'
  { -- | The ARN of the delivery stream.
    deliveryStreamARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDeliveryStreamResponse' with the minimum fields required to make a request.
--
-- * 'deliveryStreamARN' - The ARN of the delivery stream.
-- * 'responseStatus' - The response status code.
mkCreateDeliveryStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDeliveryStreamResponse
mkCreateDeliveryStreamResponse pResponseStatus_ =
  CreateDeliveryStreamResponse'
    { deliveryStreamARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrsDeliveryStreamARN :: Lens.Lens' CreateDeliveryStreamResponse (Lude.Maybe Lude.Text)
cdsrsDeliveryStreamARN = Lens.lens (deliveryStreamARN :: CreateDeliveryStreamResponse -> Lude.Maybe Lude.Text) (\s a -> s {deliveryStreamARN = a} :: CreateDeliveryStreamResponse)
{-# DEPRECATED cdsrsDeliveryStreamARN "Use generic-lens or generic-optics with 'deliveryStreamARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrsResponseStatus :: Lens.Lens' CreateDeliveryStreamResponse Lude.Int
cdsrsResponseStatus = Lens.lens (responseStatus :: CreateDeliveryStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDeliveryStreamResponse)
{-# DEPRECATED cdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
