{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.S3DestinationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.S3DestinationUpdate where

import Network.AWS.Firehose.Types.BufferingHints
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CompressionFormat
import Network.AWS.Firehose.Types.EncryptionConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an update for a destination in Amazon S3.
--
--
--
-- /See:/ 's3DestinationUpdate' smart constructor.
data S3DestinationUpdate = S3DestinationUpdate'
  { _sPrefix ::
      !(Maybe Text),
    _sCloudWatchLoggingOptions ::
      !(Maybe CloudWatchLoggingOptions),
    _sErrorOutputPrefix :: !(Maybe Text),
    _sEncryptionConfiguration ::
      !(Maybe EncryptionConfiguration),
    _sCompressionFormat :: !(Maybe CompressionFormat),
    _sBufferingHints :: !(Maybe BufferingHints),
    _sBucketARN :: !(Maybe Text),
    _sRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3DestinationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sPrefix' - The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- * 'sCloudWatchLoggingOptions' - The CloudWatch logging options for your delivery stream.
--
-- * 'sErrorOutputPrefix' - A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- * 'sEncryptionConfiguration' - The encryption configuration. If no value is specified, the default is no encryption.
--
-- * 'sCompressionFormat' - The compression format. If no value is specified, the default is @UNCOMPRESSED@ . The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon Redshift destinations because they are not supported by the Amazon Redshift @COPY@ operation that reads from the S3 bucket.
--
-- * 'sBufferingHints' - The buffering option. If no value is specified, @BufferingHints@ object default values are used.
--
-- * 'sBucketARN' - The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'sRoleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
s3DestinationUpdate ::
  S3DestinationUpdate
s3DestinationUpdate =
  S3DestinationUpdate'
    { _sPrefix = Nothing,
      _sCloudWatchLoggingOptions = Nothing,
      _sErrorOutputPrefix = Nothing,
      _sEncryptionConfiguration = Nothing,
      _sCompressionFormat = Nothing,
      _sBufferingHints = Nothing,
      _sBucketARN = Nothing,
      _sRoleARN = Nothing
    }

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
sPrefix :: Lens' S3DestinationUpdate (Maybe Text)
sPrefix = lens _sPrefix (\s a -> s {_sPrefix = a})

-- | The CloudWatch logging options for your delivery stream.
sCloudWatchLoggingOptions :: Lens' S3DestinationUpdate (Maybe CloudWatchLoggingOptions)
sCloudWatchLoggingOptions = lens _sCloudWatchLoggingOptions (\s a -> s {_sCloudWatchLoggingOptions = a})

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
sErrorOutputPrefix :: Lens' S3DestinationUpdate (Maybe Text)
sErrorOutputPrefix = lens _sErrorOutputPrefix (\s a -> s {_sErrorOutputPrefix = a})

-- | The encryption configuration. If no value is specified, the default is no encryption.
sEncryptionConfiguration :: Lens' S3DestinationUpdate (Maybe EncryptionConfiguration)
sEncryptionConfiguration = lens _sEncryptionConfiguration (\s a -> s {_sEncryptionConfiguration = a})

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ . The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon Redshift destinations because they are not supported by the Amazon Redshift @COPY@ operation that reads from the S3 bucket.
sCompressionFormat :: Lens' S3DestinationUpdate (Maybe CompressionFormat)
sCompressionFormat = lens _sCompressionFormat (\s a -> s {_sCompressionFormat = a})

-- | The buffering option. If no value is specified, @BufferingHints@ object default values are used.
sBufferingHints :: Lens' S3DestinationUpdate (Maybe BufferingHints)
sBufferingHints = lens _sBufferingHints (\s a -> s {_sBufferingHints = a})

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
sBucketARN :: Lens' S3DestinationUpdate (Maybe Text)
sBucketARN = lens _sBucketARN (\s a -> s {_sBucketARN = a})

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
sRoleARN :: Lens' S3DestinationUpdate (Maybe Text)
sRoleARN = lens _sRoleARN (\s a -> s {_sRoleARN = a})

instance Hashable S3DestinationUpdate

instance NFData S3DestinationUpdate

instance ToJSON S3DestinationUpdate where
  toJSON S3DestinationUpdate' {..} =
    object
      ( catMaybes
          [ ("Prefix" .=) <$> _sPrefix,
            ("CloudWatchLoggingOptions" .=) <$> _sCloudWatchLoggingOptions,
            ("ErrorOutputPrefix" .=) <$> _sErrorOutputPrefix,
            ("EncryptionConfiguration" .=) <$> _sEncryptionConfiguration,
            ("CompressionFormat" .=) <$> _sCompressionFormat,
            ("BufferingHints" .=) <$> _sBufferingHints,
            ("BucketARN" .=) <$> _sBucketARN,
            ("RoleARN" .=) <$> _sRoleARN
          ]
      )
