{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
-- By default, you can create up to 50 delivery streams per AWS Region.
--
-- This is an asynchronous operation that immediately returns. The initial status of the delivery stream is @CREATING@ . After the delivery stream is created, its status is @ACTIVE@ and it now accepts data. If the delivery stream creation fails, the status transitions to @CREATING_FAILED@ . Attempts to send data to a delivery stream that is not in the @ACTIVE@ state cause an exception. To check the state of a delivery stream, use 'DescribeDeliveryStream' .
--
-- If the status of a delivery stream is @CREATING_FAILED@ , this status doesn't change, and you can't invoke @CreateDeliveryStream@ again on it. However, you can invoke the 'DeleteDeliveryStream' operation to delete it.
--
-- A Kinesis Data Firehose delivery stream can be configured to receive records directly from providers using 'PutRecord' or 'PutRecordBatch' , or it can be configured to use an existing Kinesis stream as its source. To specify a Kinesis data stream as input, set the @DeliveryStreamType@ parameter to @KinesisStreamAsSource@ , and provide the Kinesis stream Amazon Resource Name (ARN) and role ARN in the @KinesisStreamSourceConfiguration@ parameter.
--
-- To create a delivery stream with server-side encryption (SSE) enabled, include 'DeliveryStreamEncryptionConfigurationInput' in your request. This is optional. You can also invoke 'StartDeliveryStreamEncryption' to turn on SSE for an existing delivery stream that doesn't have SSE enabled.
--
-- A delivery stream is configured with a single destination: Amazon S3, Amazon ES, Amazon Redshift, or Splunk. You must specify only one of the following destination configuration parameters: @ExtendedS3DestinationConfiguration@ , @S3DestinationConfiguration@ , @ElasticsearchDestinationConfiguration@ , @RedshiftDestinationConfiguration@ , or @SplunkDestinationConfiguration@ .
--
-- When you specify @S3DestinationConfiguration@ , you can also provide the following optional values: BufferingHints, @EncryptionConfiguration@ , and @CompressionFormat@ . By default, if no @BufferingHints@ value is provided, Kinesis Data Firehose buffers data up to 5 MB or for 5 minutes, whichever condition is satisfied first. @BufferingHints@ is a hint, so there are some cases where the service cannot adhere to these conditions strictly. For example, record boundaries might be such that the size is a little over or under the configured buffering size. By default, no encryption is performed. We strongly recommend that you enable encryption to ensure secure data storage in Amazon S3.
--
-- A few notes about Amazon Redshift as a destination:
--
--     * An Amazon Redshift destination requires an S3 bucket as intermediate location. Kinesis Data Firehose first delivers data to Amazon S3 and then uses @COPY@ syntax to load data into an Amazon Redshift table. This is specified in the @RedshiftDestinationConfiguration.S3Configuration@ parameter.
--
--     * The compression formats @SNAPPY@ or @ZIP@ cannot be specified in @RedshiftDestinationConfiguration.S3Configuration@ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
--
--     * We strongly recommend that you use the user name and password you provide exclusively with Kinesis Data Firehose, and that the permissions for the account are restricted for Amazon Redshift @INSERT@ permissions.
--
--
--
-- Kinesis Data Firehose assumes the IAM role that is configured as part of the destination. The role should allow the Kinesis Data Firehose principal to assume the role, and the role should have permissions that allow the service to deliver the data. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination> in the /Amazon Kinesis Data Firehose Developer Guide/ .
module Network.AWS.Firehose.CreateDeliveryStream
  ( -- * Creating a Request
    createDeliveryStream,
    CreateDeliveryStream,

    -- * Request Lenses
    cdsS3DestinationConfiguration,
    cdsRedshiftDestinationConfiguration,
    cdsElasticsearchDestinationConfiguration,
    cdsExtendedS3DestinationConfiguration,
    cdsKinesisStreamSourceConfiguration,
    cdsHTTPEndpointDestinationConfiguration,
    cdsDeliveryStreamType,
    cdsSplunkDestinationConfiguration,
    cdsTags,
    cdsDeliveryStreamEncryptionConfigurationInput,
    cdsDeliveryStreamName,

    -- * Destructuring the Response
    createDeliveryStreamResponse,
    CreateDeliveryStreamResponse,

    -- * Response Lenses
    cdsrsDeliveryStreamARN,
    cdsrsResponseStatus,
  )
where

import Network.AWS.Firehose.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDeliveryStream' smart constructor.
data CreateDeliveryStream = CreateDeliveryStream'
  { _cdsS3DestinationConfiguration ::
      !(Maybe S3DestinationConfiguration),
    _cdsRedshiftDestinationConfiguration ::
      !(Maybe RedshiftDestinationConfiguration),
    _cdsElasticsearchDestinationConfiguration ::
      !(Maybe ElasticsearchDestinationConfiguration),
    _cdsExtendedS3DestinationConfiguration ::
      !(Maybe ExtendedS3DestinationConfiguration),
    _cdsKinesisStreamSourceConfiguration ::
      !(Maybe KinesisStreamSourceConfiguration),
    _cdsHTTPEndpointDestinationConfiguration ::
      !(Maybe HTTPEndpointDestinationConfiguration),
    _cdsDeliveryStreamType ::
      !(Maybe DeliveryStreamType),
    _cdsSplunkDestinationConfiguration ::
      !(Maybe SplunkDestinationConfiguration),
    _cdsTags :: !(Maybe (List1 Tag)),
    _cdsDeliveryStreamEncryptionConfigurationInput ::
      !( Maybe
           DeliveryStreamEncryptionConfigurationInput
       ),
    _cdsDeliveryStreamName :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDeliveryStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsS3DestinationConfiguration' - [Deprecated] The destination in Amazon S3. You can specify only one destination.
--
-- * 'cdsRedshiftDestinationConfiguration' - The destination in Amazon Redshift. You can specify only one destination.
--
-- * 'cdsElasticsearchDestinationConfiguration' - The destination in Amazon ES. You can specify only one destination.
--
-- * 'cdsExtendedS3DestinationConfiguration' - The destination in Amazon S3. You can specify only one destination.
--
-- * 'cdsKinesisStreamSourceConfiguration' - When a Kinesis data stream is used as the source for the delivery stream, a 'KinesisStreamSourceConfiguration' containing the Kinesis data stream Amazon Resource Name (ARN) and the role ARN for the source stream.
--
-- * 'cdsHTTPEndpointDestinationConfiguration' - Enables configuring Kinesis Firehose to deliver data to any HTTP endpoint destination. You can specify only one destination.
--
-- * 'cdsDeliveryStreamType' - The delivery stream type. This parameter can be one of the following values:     * @DirectPut@ : Provider applications access the delivery stream directly.     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
--
-- * 'cdsSplunkDestinationConfiguration' - The destination in Splunk. You can specify only one destination.
--
-- * 'cdsTags' - A set of tags to assign to the delivery stream. A tag is a key-value pair that you can define and assign to AWS resources. Tags are metadata. For example, you can add friendly names and descriptions or other types of information that can help you distinguish the delivery stream. For more information about tags, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the AWS Billing and Cost Management User Guide. You can specify up to 50 tags when creating a delivery stream.
--
-- * 'cdsDeliveryStreamEncryptionConfigurationInput' - Used to specify the type and Amazon Resource Name (ARN) of the KMS key needed for Server-Side Encryption (SSE).
--
-- * 'cdsDeliveryStreamName' - The name of the delivery stream. This name must be unique per AWS account in the same AWS Region. If the delivery streams are in different accounts or different Regions, you can have multiple delivery streams with the same name.
createDeliveryStream ::
  -- | 'cdsDeliveryStreamName'
  Text ->
  CreateDeliveryStream
createDeliveryStream pDeliveryStreamName_ =
  CreateDeliveryStream'
    { _cdsS3DestinationConfiguration = Nothing,
      _cdsRedshiftDestinationConfiguration = Nothing,
      _cdsElasticsearchDestinationConfiguration = Nothing,
      _cdsExtendedS3DestinationConfiguration = Nothing,
      _cdsKinesisStreamSourceConfiguration = Nothing,
      _cdsHTTPEndpointDestinationConfiguration = Nothing,
      _cdsDeliveryStreamType = Nothing,
      _cdsSplunkDestinationConfiguration = Nothing,
      _cdsTags = Nothing,
      _cdsDeliveryStreamEncryptionConfigurationInput = Nothing,
      _cdsDeliveryStreamName = pDeliveryStreamName_
    }

-- | [Deprecated] The destination in Amazon S3. You can specify only one destination.
cdsS3DestinationConfiguration :: Lens' CreateDeliveryStream (Maybe S3DestinationConfiguration)
cdsS3DestinationConfiguration = lens _cdsS3DestinationConfiguration (\s a -> s {_cdsS3DestinationConfiguration = a})

-- | The destination in Amazon Redshift. You can specify only one destination.
cdsRedshiftDestinationConfiguration :: Lens' CreateDeliveryStream (Maybe RedshiftDestinationConfiguration)
cdsRedshiftDestinationConfiguration = lens _cdsRedshiftDestinationConfiguration (\s a -> s {_cdsRedshiftDestinationConfiguration = a})

-- | The destination in Amazon ES. You can specify only one destination.
cdsElasticsearchDestinationConfiguration :: Lens' CreateDeliveryStream (Maybe ElasticsearchDestinationConfiguration)
cdsElasticsearchDestinationConfiguration = lens _cdsElasticsearchDestinationConfiguration (\s a -> s {_cdsElasticsearchDestinationConfiguration = a})

-- | The destination in Amazon S3. You can specify only one destination.
cdsExtendedS3DestinationConfiguration :: Lens' CreateDeliveryStream (Maybe ExtendedS3DestinationConfiguration)
cdsExtendedS3DestinationConfiguration = lens _cdsExtendedS3DestinationConfiguration (\s a -> s {_cdsExtendedS3DestinationConfiguration = a})

-- | When a Kinesis data stream is used as the source for the delivery stream, a 'KinesisStreamSourceConfiguration' containing the Kinesis data stream Amazon Resource Name (ARN) and the role ARN for the source stream.
cdsKinesisStreamSourceConfiguration :: Lens' CreateDeliveryStream (Maybe KinesisStreamSourceConfiguration)
cdsKinesisStreamSourceConfiguration = lens _cdsKinesisStreamSourceConfiguration (\s a -> s {_cdsKinesisStreamSourceConfiguration = a})

-- | Enables configuring Kinesis Firehose to deliver data to any HTTP endpoint destination. You can specify only one destination.
cdsHTTPEndpointDestinationConfiguration :: Lens' CreateDeliveryStream (Maybe HTTPEndpointDestinationConfiguration)
cdsHTTPEndpointDestinationConfiguration = lens _cdsHTTPEndpointDestinationConfiguration (\s a -> s {_cdsHTTPEndpointDestinationConfiguration = a})

-- | The delivery stream type. This parameter can be one of the following values:     * @DirectPut@ : Provider applications access the delivery stream directly.     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
cdsDeliveryStreamType :: Lens' CreateDeliveryStream (Maybe DeliveryStreamType)
cdsDeliveryStreamType = lens _cdsDeliveryStreamType (\s a -> s {_cdsDeliveryStreamType = a})

-- | The destination in Splunk. You can specify only one destination.
cdsSplunkDestinationConfiguration :: Lens' CreateDeliveryStream (Maybe SplunkDestinationConfiguration)
cdsSplunkDestinationConfiguration = lens _cdsSplunkDestinationConfiguration (\s a -> s {_cdsSplunkDestinationConfiguration = a})

-- | A set of tags to assign to the delivery stream. A tag is a key-value pair that you can define and assign to AWS resources. Tags are metadata. For example, you can add friendly names and descriptions or other types of information that can help you distinguish the delivery stream. For more information about tags, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the AWS Billing and Cost Management User Guide. You can specify up to 50 tags when creating a delivery stream.
cdsTags :: Lens' CreateDeliveryStream (Maybe (NonEmpty Tag))
cdsTags = lens _cdsTags (\s a -> s {_cdsTags = a}) . mapping _List1

-- | Used to specify the type and Amazon Resource Name (ARN) of the KMS key needed for Server-Side Encryption (SSE).
cdsDeliveryStreamEncryptionConfigurationInput :: Lens' CreateDeliveryStream (Maybe DeliveryStreamEncryptionConfigurationInput)
cdsDeliveryStreamEncryptionConfigurationInput = lens _cdsDeliveryStreamEncryptionConfigurationInput (\s a -> s {_cdsDeliveryStreamEncryptionConfigurationInput = a})

-- | The name of the delivery stream. This name must be unique per AWS account in the same AWS Region. If the delivery streams are in different accounts or different Regions, you can have multiple delivery streams with the same name.
cdsDeliveryStreamName :: Lens' CreateDeliveryStream Text
cdsDeliveryStreamName = lens _cdsDeliveryStreamName (\s a -> s {_cdsDeliveryStreamName = a})

instance AWSRequest CreateDeliveryStream where
  type Rs CreateDeliveryStream = CreateDeliveryStreamResponse
  request = postJSON firehose
  response =
    receiveJSON
      ( \s h x ->
          CreateDeliveryStreamResponse'
            <$> (x .?> "DeliveryStreamARN") <*> (pure (fromEnum s))
      )

instance Hashable CreateDeliveryStream

instance NFData CreateDeliveryStream

instance ToHeaders CreateDeliveryStream where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Firehose_20150804.CreateDeliveryStream" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateDeliveryStream where
  toJSON CreateDeliveryStream' {..} =
    object
      ( catMaybes
          [ ("S3DestinationConfiguration" .=)
              <$> _cdsS3DestinationConfiguration,
            ("RedshiftDestinationConfiguration" .=)
              <$> _cdsRedshiftDestinationConfiguration,
            ("ElasticsearchDestinationConfiguration" .=)
              <$> _cdsElasticsearchDestinationConfiguration,
            ("ExtendedS3DestinationConfiguration" .=)
              <$> _cdsExtendedS3DestinationConfiguration,
            ("KinesisStreamSourceConfiguration" .=)
              <$> _cdsKinesisStreamSourceConfiguration,
            ("HttpEndpointDestinationConfiguration" .=)
              <$> _cdsHTTPEndpointDestinationConfiguration,
            ("DeliveryStreamType" .=) <$> _cdsDeliveryStreamType,
            ("SplunkDestinationConfiguration" .=)
              <$> _cdsSplunkDestinationConfiguration,
            ("Tags" .=) <$> _cdsTags,
            ("DeliveryStreamEncryptionConfigurationInput" .=)
              <$> _cdsDeliveryStreamEncryptionConfigurationInput,
            Just ("DeliveryStreamName" .= _cdsDeliveryStreamName)
          ]
      )

instance ToPath CreateDeliveryStream where
  toPath = const "/"

instance ToQuery CreateDeliveryStream where
  toQuery = const mempty

-- | /See:/ 'createDeliveryStreamResponse' smart constructor.
data CreateDeliveryStreamResponse = CreateDeliveryStreamResponse'
  { _cdsrsDeliveryStreamARN ::
      !(Maybe Text),
    _cdsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDeliveryStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsrsDeliveryStreamARN' - The ARN of the delivery stream.
--
-- * 'cdsrsResponseStatus' - -- | The response status code.
createDeliveryStreamResponse ::
  -- | 'cdsrsResponseStatus'
  Int ->
  CreateDeliveryStreamResponse
createDeliveryStreamResponse pResponseStatus_ =
  CreateDeliveryStreamResponse'
    { _cdsrsDeliveryStreamARN = Nothing,
      _cdsrsResponseStatus = pResponseStatus_
    }

-- | The ARN of the delivery stream.
cdsrsDeliveryStreamARN :: Lens' CreateDeliveryStreamResponse (Maybe Text)
cdsrsDeliveryStreamARN = lens _cdsrsDeliveryStreamARN (\s a -> s {_cdsrsDeliveryStreamARN = a})

-- | -- | The response status code.
cdsrsResponseStatus :: Lens' CreateDeliveryStreamResponse Int
cdsrsResponseStatus = lens _cdsrsResponseStatus (\s a -> s {_cdsrsResponseStatus = a})

instance NFData CreateDeliveryStreamResponse
