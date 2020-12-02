{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ExtendedS3DestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ExtendedS3DestinationConfiguration where

import Network.AWS.Firehose.Types.BufferingHints
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CompressionFormat
import Network.AWS.Firehose.Types.DataFormatConversionConfiguration
import Network.AWS.Firehose.Types.EncryptionConfiguration
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3BackupMode
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the configuration of a destination in Amazon S3.
--
--
--
-- /See:/ 'extendedS3DestinationConfiguration' smart constructor.
data ExtendedS3DestinationConfiguration = ExtendedS3DestinationConfiguration'
  { _esdcS3BackupMode ::
      !(Maybe S3BackupMode),
    _esdcPrefix ::
      !(Maybe Text),
    _esdcCloudWatchLoggingOptions ::
      !( Maybe
           CloudWatchLoggingOptions
       ),
    _esdcS3BackupConfiguration ::
      !( Maybe
           S3DestinationConfiguration
       ),
    _esdcErrorOutputPrefix ::
      !(Maybe Text),
    _esdcEncryptionConfiguration ::
      !( Maybe
           EncryptionConfiguration
       ),
    _esdcCompressionFormat ::
      !( Maybe
           CompressionFormat
       ),
    _esdcBufferingHints ::
      !( Maybe
           BufferingHints
       ),
    _esdcDataFormatConversionConfiguration ::
      !( Maybe
           DataFormatConversionConfiguration
       ),
    _esdcProcessingConfiguration ::
      !( Maybe
           ProcessingConfiguration
       ),
    _esdcRoleARN :: !Text,
    _esdcBucketARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExtendedS3DestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esdcS3BackupMode' - The Amazon S3 backup mode. After you create a delivery stream, you can update it to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it.
--
-- * 'esdcPrefix' - The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- * 'esdcCloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- * 'esdcS3BackupConfiguration' - The configuration for backup in Amazon S3.
--
-- * 'esdcErrorOutputPrefix' - A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- * 'esdcEncryptionConfiguration' - The encryption configuration. If no value is specified, the default is no encryption.
--
-- * 'esdcCompressionFormat' - The compression format. If no value is specified, the default is UNCOMPRESSED.
--
-- * 'esdcBufferingHints' - The buffering option.
--
-- * 'esdcDataFormatConversionConfiguration' - The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
--
-- * 'esdcProcessingConfiguration' - The data processing configuration.
--
-- * 'esdcRoleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'esdcBucketARN' - The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
extendedS3DestinationConfiguration ::
  -- | 'esdcRoleARN'
  Text ->
  -- | 'esdcBucketARN'
  Text ->
  ExtendedS3DestinationConfiguration
extendedS3DestinationConfiguration pRoleARN_ pBucketARN_ =
  ExtendedS3DestinationConfiguration'
    { _esdcS3BackupMode = Nothing,
      _esdcPrefix = Nothing,
      _esdcCloudWatchLoggingOptions = Nothing,
      _esdcS3BackupConfiguration = Nothing,
      _esdcErrorOutputPrefix = Nothing,
      _esdcEncryptionConfiguration = Nothing,
      _esdcCompressionFormat = Nothing,
      _esdcBufferingHints = Nothing,
      _esdcDataFormatConversionConfiguration = Nothing,
      _esdcProcessingConfiguration = Nothing,
      _esdcRoleARN = pRoleARN_,
      _esdcBucketARN = pBucketARN_
    }

-- | The Amazon S3 backup mode. After you create a delivery stream, you can update it to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it.
esdcS3BackupMode :: Lens' ExtendedS3DestinationConfiguration (Maybe S3BackupMode)
esdcS3BackupMode = lens _esdcS3BackupMode (\s a -> s {_esdcS3BackupMode = a})

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
esdcPrefix :: Lens' ExtendedS3DestinationConfiguration (Maybe Text)
esdcPrefix = lens _esdcPrefix (\s a -> s {_esdcPrefix = a})

-- | The Amazon CloudWatch logging options for your delivery stream.
esdcCloudWatchLoggingOptions :: Lens' ExtendedS3DestinationConfiguration (Maybe CloudWatchLoggingOptions)
esdcCloudWatchLoggingOptions = lens _esdcCloudWatchLoggingOptions (\s a -> s {_esdcCloudWatchLoggingOptions = a})

-- | The configuration for backup in Amazon S3.
esdcS3BackupConfiguration :: Lens' ExtendedS3DestinationConfiguration (Maybe S3DestinationConfiguration)
esdcS3BackupConfiguration = lens _esdcS3BackupConfiguration (\s a -> s {_esdcS3BackupConfiguration = a})

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
esdcErrorOutputPrefix :: Lens' ExtendedS3DestinationConfiguration (Maybe Text)
esdcErrorOutputPrefix = lens _esdcErrorOutputPrefix (\s a -> s {_esdcErrorOutputPrefix = a})

-- | The encryption configuration. If no value is specified, the default is no encryption.
esdcEncryptionConfiguration :: Lens' ExtendedS3DestinationConfiguration (Maybe EncryptionConfiguration)
esdcEncryptionConfiguration = lens _esdcEncryptionConfiguration (\s a -> s {_esdcEncryptionConfiguration = a})

-- | The compression format. If no value is specified, the default is UNCOMPRESSED.
esdcCompressionFormat :: Lens' ExtendedS3DestinationConfiguration (Maybe CompressionFormat)
esdcCompressionFormat = lens _esdcCompressionFormat (\s a -> s {_esdcCompressionFormat = a})

-- | The buffering option.
esdcBufferingHints :: Lens' ExtendedS3DestinationConfiguration (Maybe BufferingHints)
esdcBufferingHints = lens _esdcBufferingHints (\s a -> s {_esdcBufferingHints = a})

-- | The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
esdcDataFormatConversionConfiguration :: Lens' ExtendedS3DestinationConfiguration (Maybe DataFormatConversionConfiguration)
esdcDataFormatConversionConfiguration = lens _esdcDataFormatConversionConfiguration (\s a -> s {_esdcDataFormatConversionConfiguration = a})

-- | The data processing configuration.
esdcProcessingConfiguration :: Lens' ExtendedS3DestinationConfiguration (Maybe ProcessingConfiguration)
esdcProcessingConfiguration = lens _esdcProcessingConfiguration (\s a -> s {_esdcProcessingConfiguration = a})

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
esdcRoleARN :: Lens' ExtendedS3DestinationConfiguration Text
esdcRoleARN = lens _esdcRoleARN (\s a -> s {_esdcRoleARN = a})

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
esdcBucketARN :: Lens' ExtendedS3DestinationConfiguration Text
esdcBucketARN = lens _esdcBucketARN (\s a -> s {_esdcBucketARN = a})

instance Hashable ExtendedS3DestinationConfiguration

instance NFData ExtendedS3DestinationConfiguration

instance ToJSON ExtendedS3DestinationConfiguration where
  toJSON ExtendedS3DestinationConfiguration' {..} =
    object
      ( catMaybes
          [ ("S3BackupMode" .=) <$> _esdcS3BackupMode,
            ("Prefix" .=) <$> _esdcPrefix,
            ("CloudWatchLoggingOptions" .=) <$> _esdcCloudWatchLoggingOptions,
            ("S3BackupConfiguration" .=) <$> _esdcS3BackupConfiguration,
            ("ErrorOutputPrefix" .=) <$> _esdcErrorOutputPrefix,
            ("EncryptionConfiguration" .=) <$> _esdcEncryptionConfiguration,
            ("CompressionFormat" .=) <$> _esdcCompressionFormat,
            ("BufferingHints" .=) <$> _esdcBufferingHints,
            ("DataFormatConversionConfiguration" .=)
              <$> _esdcDataFormatConversionConfiguration,
            ("ProcessingConfiguration" .=) <$> _esdcProcessingConfiguration,
            Just ("RoleARN" .= _esdcRoleARN),
            Just ("BucketARN" .= _esdcBucketARN)
          ]
      )
