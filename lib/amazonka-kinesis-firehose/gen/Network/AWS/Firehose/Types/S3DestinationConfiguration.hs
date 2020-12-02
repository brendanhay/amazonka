{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.S3DestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.S3DestinationConfiguration where

import Network.AWS.Firehose.Types.BufferingHints
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CompressionFormat
import Network.AWS.Firehose.Types.EncryptionConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the configuration of a destination in Amazon S3.
--
--
--
-- /See:/ 's3DestinationConfiguration' smart constructor.
data S3DestinationConfiguration = S3DestinationConfiguration'
  { _sdcPrefix ::
      !(Maybe Text),
    _sdcCloudWatchLoggingOptions ::
      !(Maybe CloudWatchLoggingOptions),
    _sdcErrorOutputPrefix ::
      !(Maybe Text),
    _sdcEncryptionConfiguration ::
      !(Maybe EncryptionConfiguration),
    _sdcCompressionFormat ::
      !(Maybe CompressionFormat),
    _sdcBufferingHints ::
      !(Maybe BufferingHints),
    _sdcRoleARN :: !Text,
    _sdcBucketARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3DestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcPrefix' - The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- * 'sdcCloudWatchLoggingOptions' - The CloudWatch logging options for your delivery stream.
--
-- * 'sdcErrorOutputPrefix' - A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- * 'sdcEncryptionConfiguration' - The encryption configuration. If no value is specified, the default is no encryption.
--
-- * 'sdcCompressionFormat' - The compression format. If no value is specified, the default is @UNCOMPRESSED@ . The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon Redshift destinations because they are not supported by the Amazon Redshift @COPY@ operation that reads from the S3 bucket.
--
-- * 'sdcBufferingHints' - The buffering option. If no value is specified, @BufferingHints@ object default values are used.
--
-- * 'sdcRoleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'sdcBucketARN' - The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
s3DestinationConfiguration ::
  -- | 'sdcRoleARN'
  Text ->
  -- | 'sdcBucketARN'
  Text ->
  S3DestinationConfiguration
s3DestinationConfiguration pRoleARN_ pBucketARN_ =
  S3DestinationConfiguration'
    { _sdcPrefix = Nothing,
      _sdcCloudWatchLoggingOptions = Nothing,
      _sdcErrorOutputPrefix = Nothing,
      _sdcEncryptionConfiguration = Nothing,
      _sdcCompressionFormat = Nothing,
      _sdcBufferingHints = Nothing,
      _sdcRoleARN = pRoleARN_,
      _sdcBucketARN = pBucketARN_
    }

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
sdcPrefix :: Lens' S3DestinationConfiguration (Maybe Text)
sdcPrefix = lens _sdcPrefix (\s a -> s {_sdcPrefix = a})

-- | The CloudWatch logging options for your delivery stream.
sdcCloudWatchLoggingOptions :: Lens' S3DestinationConfiguration (Maybe CloudWatchLoggingOptions)
sdcCloudWatchLoggingOptions = lens _sdcCloudWatchLoggingOptions (\s a -> s {_sdcCloudWatchLoggingOptions = a})

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
sdcErrorOutputPrefix :: Lens' S3DestinationConfiguration (Maybe Text)
sdcErrorOutputPrefix = lens _sdcErrorOutputPrefix (\s a -> s {_sdcErrorOutputPrefix = a})

-- | The encryption configuration. If no value is specified, the default is no encryption.
sdcEncryptionConfiguration :: Lens' S3DestinationConfiguration (Maybe EncryptionConfiguration)
sdcEncryptionConfiguration = lens _sdcEncryptionConfiguration (\s a -> s {_sdcEncryptionConfiguration = a})

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ . The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon Redshift destinations because they are not supported by the Amazon Redshift @COPY@ operation that reads from the S3 bucket.
sdcCompressionFormat :: Lens' S3DestinationConfiguration (Maybe CompressionFormat)
sdcCompressionFormat = lens _sdcCompressionFormat (\s a -> s {_sdcCompressionFormat = a})

-- | The buffering option. If no value is specified, @BufferingHints@ object default values are used.
sdcBufferingHints :: Lens' S3DestinationConfiguration (Maybe BufferingHints)
sdcBufferingHints = lens _sdcBufferingHints (\s a -> s {_sdcBufferingHints = a})

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
sdcRoleARN :: Lens' S3DestinationConfiguration Text
sdcRoleARN = lens _sdcRoleARN (\s a -> s {_sdcRoleARN = a})

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
sdcBucketARN :: Lens' S3DestinationConfiguration Text
sdcBucketARN = lens _sdcBucketARN (\s a -> s {_sdcBucketARN = a})

instance Hashable S3DestinationConfiguration

instance NFData S3DestinationConfiguration

instance ToJSON S3DestinationConfiguration where
  toJSON S3DestinationConfiguration' {..} =
    object
      ( catMaybes
          [ ("Prefix" .=) <$> _sdcPrefix,
            ("CloudWatchLoggingOptions" .=) <$> _sdcCloudWatchLoggingOptions,
            ("ErrorOutputPrefix" .=) <$> _sdcErrorOutputPrefix,
            ("EncryptionConfiguration" .=) <$> _sdcEncryptionConfiguration,
            ("CompressionFormat" .=) <$> _sdcCompressionFormat,
            ("BufferingHints" .=) <$> _sdcBufferingHints,
            Just ("RoleARN" .= _sdcRoleARN),
            Just ("BucketARN" .= _sdcBucketARN)
          ]
      )
