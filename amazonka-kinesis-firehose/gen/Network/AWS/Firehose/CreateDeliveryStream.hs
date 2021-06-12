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
-- Module      : Network.AWS.Firehose.CreateDeliveryStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Kinesis Data Firehose delivery stream.
--
-- By default, you can create up to 50 delivery streams per AWS Region.
--
-- This is an asynchronous operation that immediately returns. The initial
-- status of the delivery stream is @CREATING@. After the delivery stream
-- is created, its status is @ACTIVE@ and it now accepts data. If the
-- delivery stream creation fails, the status transitions to
-- @CREATING_FAILED@. Attempts to send data to a delivery stream that is
-- not in the @ACTIVE@ state cause an exception. To check the state of a
-- delivery stream, use DescribeDeliveryStream.
--
-- If the status of a delivery stream is @CREATING_FAILED@, this status
-- doesn\'t change, and you can\'t invoke @CreateDeliveryStream@ again on
-- it. However, you can invoke the DeleteDeliveryStream operation to delete
-- it.
--
-- A Kinesis Data Firehose delivery stream can be configured to receive
-- records directly from providers using PutRecord or PutRecordBatch, or it
-- can be configured to use an existing Kinesis stream as its source. To
-- specify a Kinesis data stream as input, set the @DeliveryStreamType@
-- parameter to @KinesisStreamAsSource@, and provide the Kinesis stream
-- Amazon Resource Name (ARN) and role ARN in the
-- @KinesisStreamSourceConfiguration@ parameter.
--
-- To create a delivery stream with server-side encryption (SSE) enabled,
-- include DeliveryStreamEncryptionConfigurationInput in your request. This
-- is optional. You can also invoke StartDeliveryStreamEncryption to turn
-- on SSE for an existing delivery stream that doesn\'t have SSE enabled.
--
-- A delivery stream is configured with a single destination: Amazon S3,
-- Amazon ES, Amazon Redshift, or Splunk. You must specify only one of the
-- following destination configuration parameters:
-- @ExtendedS3DestinationConfiguration@, @S3DestinationConfiguration@,
-- @ElasticsearchDestinationConfiguration@,
-- @RedshiftDestinationConfiguration@, or @SplunkDestinationConfiguration@.
--
-- When you specify @S3DestinationConfiguration@, you can also provide the
-- following optional values: BufferingHints, @EncryptionConfiguration@,
-- and @CompressionFormat@. By default, if no @BufferingHints@ value is
-- provided, Kinesis Data Firehose buffers data up to 5 MB or for 5
-- minutes, whichever condition is satisfied first. @BufferingHints@ is a
-- hint, so there are some cases where the service cannot adhere to these
-- conditions strictly. For example, record boundaries might be such that
-- the size is a little over or under the configured buffering size. By
-- default, no encryption is performed. We strongly recommend that you
-- enable encryption to ensure secure data storage in Amazon S3.
--
-- A few notes about Amazon Redshift as a destination:
--
-- -   An Amazon Redshift destination requires an S3 bucket as intermediate
--     location. Kinesis Data Firehose first delivers data to Amazon S3 and
--     then uses @COPY@ syntax to load data into an Amazon Redshift table.
--     This is specified in the
--     @RedshiftDestinationConfiguration.S3Configuration@ parameter.
--
-- -   The compression formats @SNAPPY@ or @ZIP@ cannot be specified in
--     @RedshiftDestinationConfiguration.S3Configuration@ because the
--     Amazon Redshift @COPY@ operation that reads from the S3 bucket
--     doesn\'t support these compression formats.
--
-- -   We strongly recommend that you use the user name and password you
--     provide exclusively with Kinesis Data Firehose, and that the
--     permissions for the account are restricted for Amazon Redshift
--     @INSERT@ permissions.
--
-- Kinesis Data Firehose assumes the IAM role that is configured as part of
-- the destination. The role should allow the Kinesis Data Firehose
-- principal to assume the role, and the role should have permissions that
-- allow the service to deliver the data. For more information, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination>
-- in the /Amazon Kinesis Data Firehose Developer Guide/.
module Network.AWS.Firehose.CreateDeliveryStream
  ( -- * Creating a Request
    CreateDeliveryStream (..),
    newCreateDeliveryStream,

    -- * Request Lenses
    createDeliveryStream_redshiftDestinationConfiguration,
    createDeliveryStream_s3DestinationConfiguration,
    createDeliveryStream_kinesisStreamSourceConfiguration,
    createDeliveryStream_extendedS3DestinationConfiguration,
    createDeliveryStream_httpEndpointDestinationConfiguration,
    createDeliveryStream_elasticsearchDestinationConfiguration,
    createDeliveryStream_tags,
    createDeliveryStream_deliveryStreamEncryptionConfigurationInput,
    createDeliveryStream_splunkDestinationConfiguration,
    createDeliveryStream_deliveryStreamType,
    createDeliveryStream_deliveryStreamName,

    -- * Destructuring the Response
    CreateDeliveryStreamResponse (..),
    newCreateDeliveryStreamResponse,

    -- * Response Lenses
    createDeliveryStreamResponse_deliveryStreamARN,
    createDeliveryStreamResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDeliveryStream' smart constructor.
data CreateDeliveryStream = CreateDeliveryStream'
  { -- | The destination in Amazon Redshift. You can specify only one
    -- destination.
    redshiftDestinationConfiguration :: Core.Maybe RedshiftDestinationConfiguration,
    -- | [Deprecated] The destination in Amazon S3. You can specify only one
    -- destination.
    s3DestinationConfiguration :: Core.Maybe S3DestinationConfiguration,
    -- | When a Kinesis data stream is used as the source for the delivery
    -- stream, a KinesisStreamSourceConfiguration containing the Kinesis data
    -- stream Amazon Resource Name (ARN) and the role ARN for the source
    -- stream.
    kinesisStreamSourceConfiguration :: Core.Maybe KinesisStreamSourceConfiguration,
    -- | The destination in Amazon S3. You can specify only one destination.
    extendedS3DestinationConfiguration :: Core.Maybe ExtendedS3DestinationConfiguration,
    -- | Enables configuring Kinesis Firehose to deliver data to any HTTP
    -- endpoint destination. You can specify only one destination.
    httpEndpointDestinationConfiguration :: Core.Maybe HttpEndpointDestinationConfiguration,
    -- | The destination in Amazon ES. You can specify only one destination.
    elasticsearchDestinationConfiguration :: Core.Maybe ElasticsearchDestinationConfiguration,
    -- | A set of tags to assign to the delivery stream. A tag is a key-value
    -- pair that you can define and assign to AWS resources. Tags are metadata.
    -- For example, you can add friendly names and descriptions or other types
    -- of information that can help you distinguish the delivery stream. For
    -- more information about tags, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags>
    -- in the AWS Billing and Cost Management User Guide.
    --
    -- You can specify up to 50 tags when creating a delivery stream.
    tags :: Core.Maybe (Core.NonEmpty Tag),
    -- | Used to specify the type and Amazon Resource Name (ARN) of the KMS key
    -- needed for Server-Side Encryption (SSE).
    deliveryStreamEncryptionConfigurationInput :: Core.Maybe DeliveryStreamEncryptionConfigurationInput,
    -- | The destination in Splunk. You can specify only one destination.
    splunkDestinationConfiguration :: Core.Maybe SplunkDestinationConfiguration,
    -- | The delivery stream type. This parameter can be one of the following
    -- values:
    --
    -- -   @DirectPut@: Provider applications access the delivery stream
    --     directly.
    --
    -- -   @KinesisStreamAsSource@: The delivery stream uses a Kinesis data
    --     stream as a source.
    deliveryStreamType :: Core.Maybe DeliveryStreamType,
    -- | The name of the delivery stream. This name must be unique per AWS
    -- account in the same AWS Region. If the delivery streams are in different
    -- accounts or different Regions, you can have multiple delivery streams
    -- with the same name.
    deliveryStreamName :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDeliveryStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'redshiftDestinationConfiguration', 'createDeliveryStream_redshiftDestinationConfiguration' - The destination in Amazon Redshift. You can specify only one
-- destination.
--
-- 's3DestinationConfiguration', 'createDeliveryStream_s3DestinationConfiguration' - [Deprecated] The destination in Amazon S3. You can specify only one
-- destination.
--
-- 'kinesisStreamSourceConfiguration', 'createDeliveryStream_kinesisStreamSourceConfiguration' - When a Kinesis data stream is used as the source for the delivery
-- stream, a KinesisStreamSourceConfiguration containing the Kinesis data
-- stream Amazon Resource Name (ARN) and the role ARN for the source
-- stream.
--
-- 'extendedS3DestinationConfiguration', 'createDeliveryStream_extendedS3DestinationConfiguration' - The destination in Amazon S3. You can specify only one destination.
--
-- 'httpEndpointDestinationConfiguration', 'createDeliveryStream_httpEndpointDestinationConfiguration' - Enables configuring Kinesis Firehose to deliver data to any HTTP
-- endpoint destination. You can specify only one destination.
--
-- 'elasticsearchDestinationConfiguration', 'createDeliveryStream_elasticsearchDestinationConfiguration' - The destination in Amazon ES. You can specify only one destination.
--
-- 'tags', 'createDeliveryStream_tags' - A set of tags to assign to the delivery stream. A tag is a key-value
-- pair that you can define and assign to AWS resources. Tags are metadata.
-- For example, you can add friendly names and descriptions or other types
-- of information that can help you distinguish the delivery stream. For
-- more information about tags, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags>
-- in the AWS Billing and Cost Management User Guide.
--
-- You can specify up to 50 tags when creating a delivery stream.
--
-- 'deliveryStreamEncryptionConfigurationInput', 'createDeliveryStream_deliveryStreamEncryptionConfigurationInput' - Used to specify the type and Amazon Resource Name (ARN) of the KMS key
-- needed for Server-Side Encryption (SSE).
--
-- 'splunkDestinationConfiguration', 'createDeliveryStream_splunkDestinationConfiguration' - The destination in Splunk. You can specify only one destination.
--
-- 'deliveryStreamType', 'createDeliveryStream_deliveryStreamType' - The delivery stream type. This parameter can be one of the following
-- values:
--
-- -   @DirectPut@: Provider applications access the delivery stream
--     directly.
--
-- -   @KinesisStreamAsSource@: The delivery stream uses a Kinesis data
--     stream as a source.
--
-- 'deliveryStreamName', 'createDeliveryStream_deliveryStreamName' - The name of the delivery stream. This name must be unique per AWS
-- account in the same AWS Region. If the delivery streams are in different
-- accounts or different Regions, you can have multiple delivery streams
-- with the same name.
newCreateDeliveryStream ::
  -- | 'deliveryStreamName'
  Core.Text ->
  CreateDeliveryStream
newCreateDeliveryStream pDeliveryStreamName_ =
  CreateDeliveryStream'
    { redshiftDestinationConfiguration =
        Core.Nothing,
      s3DestinationConfiguration = Core.Nothing,
      kinesisStreamSourceConfiguration = Core.Nothing,
      extendedS3DestinationConfiguration = Core.Nothing,
      httpEndpointDestinationConfiguration = Core.Nothing,
      elasticsearchDestinationConfiguration = Core.Nothing,
      tags = Core.Nothing,
      deliveryStreamEncryptionConfigurationInput =
        Core.Nothing,
      splunkDestinationConfiguration = Core.Nothing,
      deliveryStreamType = Core.Nothing,
      deliveryStreamName = pDeliveryStreamName_
    }

-- | The destination in Amazon Redshift. You can specify only one
-- destination.
createDeliveryStream_redshiftDestinationConfiguration :: Lens.Lens' CreateDeliveryStream (Core.Maybe RedshiftDestinationConfiguration)
createDeliveryStream_redshiftDestinationConfiguration = Lens.lens (\CreateDeliveryStream' {redshiftDestinationConfiguration} -> redshiftDestinationConfiguration) (\s@CreateDeliveryStream' {} a -> s {redshiftDestinationConfiguration = a} :: CreateDeliveryStream)

-- | [Deprecated] The destination in Amazon S3. You can specify only one
-- destination.
createDeliveryStream_s3DestinationConfiguration :: Lens.Lens' CreateDeliveryStream (Core.Maybe S3DestinationConfiguration)
createDeliveryStream_s3DestinationConfiguration = Lens.lens (\CreateDeliveryStream' {s3DestinationConfiguration} -> s3DestinationConfiguration) (\s@CreateDeliveryStream' {} a -> s {s3DestinationConfiguration = a} :: CreateDeliveryStream)

-- | When a Kinesis data stream is used as the source for the delivery
-- stream, a KinesisStreamSourceConfiguration containing the Kinesis data
-- stream Amazon Resource Name (ARN) and the role ARN for the source
-- stream.
createDeliveryStream_kinesisStreamSourceConfiguration :: Lens.Lens' CreateDeliveryStream (Core.Maybe KinesisStreamSourceConfiguration)
createDeliveryStream_kinesisStreamSourceConfiguration = Lens.lens (\CreateDeliveryStream' {kinesisStreamSourceConfiguration} -> kinesisStreamSourceConfiguration) (\s@CreateDeliveryStream' {} a -> s {kinesisStreamSourceConfiguration = a} :: CreateDeliveryStream)

-- | The destination in Amazon S3. You can specify only one destination.
createDeliveryStream_extendedS3DestinationConfiguration :: Lens.Lens' CreateDeliveryStream (Core.Maybe ExtendedS3DestinationConfiguration)
createDeliveryStream_extendedS3DestinationConfiguration = Lens.lens (\CreateDeliveryStream' {extendedS3DestinationConfiguration} -> extendedS3DestinationConfiguration) (\s@CreateDeliveryStream' {} a -> s {extendedS3DestinationConfiguration = a} :: CreateDeliveryStream)

-- | Enables configuring Kinesis Firehose to deliver data to any HTTP
-- endpoint destination. You can specify only one destination.
createDeliveryStream_httpEndpointDestinationConfiguration :: Lens.Lens' CreateDeliveryStream (Core.Maybe HttpEndpointDestinationConfiguration)
createDeliveryStream_httpEndpointDestinationConfiguration = Lens.lens (\CreateDeliveryStream' {httpEndpointDestinationConfiguration} -> httpEndpointDestinationConfiguration) (\s@CreateDeliveryStream' {} a -> s {httpEndpointDestinationConfiguration = a} :: CreateDeliveryStream)

-- | The destination in Amazon ES. You can specify only one destination.
createDeliveryStream_elasticsearchDestinationConfiguration :: Lens.Lens' CreateDeliveryStream (Core.Maybe ElasticsearchDestinationConfiguration)
createDeliveryStream_elasticsearchDestinationConfiguration = Lens.lens (\CreateDeliveryStream' {elasticsearchDestinationConfiguration} -> elasticsearchDestinationConfiguration) (\s@CreateDeliveryStream' {} a -> s {elasticsearchDestinationConfiguration = a} :: CreateDeliveryStream)

-- | A set of tags to assign to the delivery stream. A tag is a key-value
-- pair that you can define and assign to AWS resources. Tags are metadata.
-- For example, you can add friendly names and descriptions or other types
-- of information that can help you distinguish the delivery stream. For
-- more information about tags, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags>
-- in the AWS Billing and Cost Management User Guide.
--
-- You can specify up to 50 tags when creating a delivery stream.
createDeliveryStream_tags :: Lens.Lens' CreateDeliveryStream (Core.Maybe (Core.NonEmpty Tag))
createDeliveryStream_tags = Lens.lens (\CreateDeliveryStream' {tags} -> tags) (\s@CreateDeliveryStream' {} a -> s {tags = a} :: CreateDeliveryStream) Core.. Lens.mapping Lens._Coerce

-- | Used to specify the type and Amazon Resource Name (ARN) of the KMS key
-- needed for Server-Side Encryption (SSE).
createDeliveryStream_deliveryStreamEncryptionConfigurationInput :: Lens.Lens' CreateDeliveryStream (Core.Maybe DeliveryStreamEncryptionConfigurationInput)
createDeliveryStream_deliveryStreamEncryptionConfigurationInput = Lens.lens (\CreateDeliveryStream' {deliveryStreamEncryptionConfigurationInput} -> deliveryStreamEncryptionConfigurationInput) (\s@CreateDeliveryStream' {} a -> s {deliveryStreamEncryptionConfigurationInput = a} :: CreateDeliveryStream)

-- | The destination in Splunk. You can specify only one destination.
createDeliveryStream_splunkDestinationConfiguration :: Lens.Lens' CreateDeliveryStream (Core.Maybe SplunkDestinationConfiguration)
createDeliveryStream_splunkDestinationConfiguration = Lens.lens (\CreateDeliveryStream' {splunkDestinationConfiguration} -> splunkDestinationConfiguration) (\s@CreateDeliveryStream' {} a -> s {splunkDestinationConfiguration = a} :: CreateDeliveryStream)

-- | The delivery stream type. This parameter can be one of the following
-- values:
--
-- -   @DirectPut@: Provider applications access the delivery stream
--     directly.
--
-- -   @KinesisStreamAsSource@: The delivery stream uses a Kinesis data
--     stream as a source.
createDeliveryStream_deliveryStreamType :: Lens.Lens' CreateDeliveryStream (Core.Maybe DeliveryStreamType)
createDeliveryStream_deliveryStreamType = Lens.lens (\CreateDeliveryStream' {deliveryStreamType} -> deliveryStreamType) (\s@CreateDeliveryStream' {} a -> s {deliveryStreamType = a} :: CreateDeliveryStream)

-- | The name of the delivery stream. This name must be unique per AWS
-- account in the same AWS Region. If the delivery streams are in different
-- accounts or different Regions, you can have multiple delivery streams
-- with the same name.
createDeliveryStream_deliveryStreamName :: Lens.Lens' CreateDeliveryStream Core.Text
createDeliveryStream_deliveryStreamName = Lens.lens (\CreateDeliveryStream' {deliveryStreamName} -> deliveryStreamName) (\s@CreateDeliveryStream' {} a -> s {deliveryStreamName = a} :: CreateDeliveryStream)

instance Core.AWSRequest CreateDeliveryStream where
  type
    AWSResponse CreateDeliveryStream =
      CreateDeliveryStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeliveryStreamResponse'
            Core.<$> (x Core..?> "DeliveryStreamARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDeliveryStream

instance Core.NFData CreateDeliveryStream

instance Core.ToHeaders CreateDeliveryStream where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Firehose_20150804.CreateDeliveryStream" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateDeliveryStream where
  toJSON CreateDeliveryStream' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RedshiftDestinationConfiguration" Core..=)
              Core.<$> redshiftDestinationConfiguration,
            ("S3DestinationConfiguration" Core..=)
              Core.<$> s3DestinationConfiguration,
            ("KinesisStreamSourceConfiguration" Core..=)
              Core.<$> kinesisStreamSourceConfiguration,
            ("ExtendedS3DestinationConfiguration" Core..=)
              Core.<$> extendedS3DestinationConfiguration,
            ("HttpEndpointDestinationConfiguration" Core..=)
              Core.<$> httpEndpointDestinationConfiguration,
            ("ElasticsearchDestinationConfiguration" Core..=)
              Core.<$> elasticsearchDestinationConfiguration,
            ("Tags" Core..=) Core.<$> tags,
            ( "DeliveryStreamEncryptionConfigurationInput"
                Core..=
            )
              Core.<$> deliveryStreamEncryptionConfigurationInput,
            ("SplunkDestinationConfiguration" Core..=)
              Core.<$> splunkDestinationConfiguration,
            ("DeliveryStreamType" Core..=)
              Core.<$> deliveryStreamType,
            Core.Just
              ("DeliveryStreamName" Core..= deliveryStreamName)
          ]
      )

instance Core.ToPath CreateDeliveryStream where
  toPath = Core.const "/"

instance Core.ToQuery CreateDeliveryStream where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateDeliveryStreamResponse' smart constructor.
data CreateDeliveryStreamResponse = CreateDeliveryStreamResponse'
  { -- | The ARN of the delivery stream.
    deliveryStreamARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDeliveryStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryStreamARN', 'createDeliveryStreamResponse_deliveryStreamARN' - The ARN of the delivery stream.
--
-- 'httpStatus', 'createDeliveryStreamResponse_httpStatus' - The response's http status code.
newCreateDeliveryStreamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateDeliveryStreamResponse
newCreateDeliveryStreamResponse pHttpStatus_ =
  CreateDeliveryStreamResponse'
    { deliveryStreamARN =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the delivery stream.
createDeliveryStreamResponse_deliveryStreamARN :: Lens.Lens' CreateDeliveryStreamResponse (Core.Maybe Core.Text)
createDeliveryStreamResponse_deliveryStreamARN = Lens.lens (\CreateDeliveryStreamResponse' {deliveryStreamARN} -> deliveryStreamARN) (\s@CreateDeliveryStreamResponse' {} a -> s {deliveryStreamARN = a} :: CreateDeliveryStreamResponse)

-- | The response's http status code.
createDeliveryStreamResponse_httpStatus :: Lens.Lens' CreateDeliveryStreamResponse Core.Int
createDeliveryStreamResponse_httpStatus = Lens.lens (\CreateDeliveryStreamResponse' {httpStatus} -> httpStatus) (\s@CreateDeliveryStreamResponse' {} a -> s {httpStatus = a} :: CreateDeliveryStreamResponse)

instance Core.NFData CreateDeliveryStreamResponse
