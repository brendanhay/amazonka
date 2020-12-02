{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ExtendedS3DestinationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ExtendedS3DestinationDescription where

import Network.AWS.Firehose.Types.BufferingHints
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CompressionFormat
import Network.AWS.Firehose.Types.DataFormatConversionConfiguration
import Network.AWS.Firehose.Types.EncryptionConfiguration
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3BackupMode
import Network.AWS.Firehose.Types.S3DestinationDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a destination in Amazon S3.
--
--
--
-- /See:/ 'extendedS3DestinationDescription' smart constructor.
data ExtendedS3DestinationDescription = ExtendedS3DestinationDescription'
  { _esddS3BackupMode ::
      !(Maybe S3BackupMode),
    _esddS3BackupDescription ::
      !( Maybe
           S3DestinationDescription
       ),
    _esddPrefix ::
      !(Maybe Text),
    _esddCloudWatchLoggingOptions ::
      !( Maybe
           CloudWatchLoggingOptions
       ),
    _esddErrorOutputPrefix ::
      !(Maybe Text),
    _esddDataFormatConversionConfiguration ::
      !( Maybe
           DataFormatConversionConfiguration
       ),
    _esddProcessingConfiguration ::
      !( Maybe
           ProcessingConfiguration
       ),
    _esddRoleARN :: !Text,
    _esddBucketARN :: !Text,
    _esddBufferingHints ::
      !BufferingHints,
    _esddCompressionFormat ::
      !CompressionFormat,
    _esddEncryptionConfiguration ::
      !EncryptionConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExtendedS3DestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esddS3BackupMode' - The Amazon S3 backup mode.
--
-- * 'esddS3BackupDescription' - The configuration for backup in Amazon S3.
--
-- * 'esddPrefix' - The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- * 'esddCloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- * 'esddErrorOutputPrefix' - A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- * 'esddDataFormatConversionConfiguration' - The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
--
-- * 'esddProcessingConfiguration' - The data processing configuration.
--
-- * 'esddRoleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'esddBucketARN' - The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'esddBufferingHints' - The buffering option.
--
-- * 'esddCompressionFormat' - The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
--
-- * 'esddEncryptionConfiguration' - The encryption configuration. If no value is specified, the default is no encryption.
extendedS3DestinationDescription ::
  -- | 'esddRoleARN'
  Text ->
  -- | 'esddBucketARN'
  Text ->
  -- | 'esddBufferingHints'
  BufferingHints ->
  -- | 'esddCompressionFormat'
  CompressionFormat ->
  -- | 'esddEncryptionConfiguration'
  EncryptionConfiguration ->
  ExtendedS3DestinationDescription
extendedS3DestinationDescription
  pRoleARN_
  pBucketARN_
  pBufferingHints_
  pCompressionFormat_
  pEncryptionConfiguration_ =
    ExtendedS3DestinationDescription'
      { _esddS3BackupMode = Nothing,
        _esddS3BackupDescription = Nothing,
        _esddPrefix = Nothing,
        _esddCloudWatchLoggingOptions = Nothing,
        _esddErrorOutputPrefix = Nothing,
        _esddDataFormatConversionConfiguration = Nothing,
        _esddProcessingConfiguration = Nothing,
        _esddRoleARN = pRoleARN_,
        _esddBucketARN = pBucketARN_,
        _esddBufferingHints = pBufferingHints_,
        _esddCompressionFormat = pCompressionFormat_,
        _esddEncryptionConfiguration = pEncryptionConfiguration_
      }

-- | The Amazon S3 backup mode.
esddS3BackupMode :: Lens' ExtendedS3DestinationDescription (Maybe S3BackupMode)
esddS3BackupMode = lens _esddS3BackupMode (\s a -> s {_esddS3BackupMode = a})

-- | The configuration for backup in Amazon S3.
esddS3BackupDescription :: Lens' ExtendedS3DestinationDescription (Maybe S3DestinationDescription)
esddS3BackupDescription = lens _esddS3BackupDescription (\s a -> s {_esddS3BackupDescription = a})

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
esddPrefix :: Lens' ExtendedS3DestinationDescription (Maybe Text)
esddPrefix = lens _esddPrefix (\s a -> s {_esddPrefix = a})

-- | The Amazon CloudWatch logging options for your delivery stream.
esddCloudWatchLoggingOptions :: Lens' ExtendedS3DestinationDescription (Maybe CloudWatchLoggingOptions)
esddCloudWatchLoggingOptions = lens _esddCloudWatchLoggingOptions (\s a -> s {_esddCloudWatchLoggingOptions = a})

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
esddErrorOutputPrefix :: Lens' ExtendedS3DestinationDescription (Maybe Text)
esddErrorOutputPrefix = lens _esddErrorOutputPrefix (\s a -> s {_esddErrorOutputPrefix = a})

-- | The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
esddDataFormatConversionConfiguration :: Lens' ExtendedS3DestinationDescription (Maybe DataFormatConversionConfiguration)
esddDataFormatConversionConfiguration = lens _esddDataFormatConversionConfiguration (\s a -> s {_esddDataFormatConversionConfiguration = a})

-- | The data processing configuration.
esddProcessingConfiguration :: Lens' ExtendedS3DestinationDescription (Maybe ProcessingConfiguration)
esddProcessingConfiguration = lens _esddProcessingConfiguration (\s a -> s {_esddProcessingConfiguration = a})

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
esddRoleARN :: Lens' ExtendedS3DestinationDescription Text
esddRoleARN = lens _esddRoleARN (\s a -> s {_esddRoleARN = a})

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
esddBucketARN :: Lens' ExtendedS3DestinationDescription Text
esddBucketARN = lens _esddBucketARN (\s a -> s {_esddBucketARN = a})

-- | The buffering option.
esddBufferingHints :: Lens' ExtendedS3DestinationDescription BufferingHints
esddBufferingHints = lens _esddBufferingHints (\s a -> s {_esddBufferingHints = a})

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
esddCompressionFormat :: Lens' ExtendedS3DestinationDescription CompressionFormat
esddCompressionFormat = lens _esddCompressionFormat (\s a -> s {_esddCompressionFormat = a})

-- | The encryption configuration. If no value is specified, the default is no encryption.
esddEncryptionConfiguration :: Lens' ExtendedS3DestinationDescription EncryptionConfiguration
esddEncryptionConfiguration = lens _esddEncryptionConfiguration (\s a -> s {_esddEncryptionConfiguration = a})

instance FromJSON ExtendedS3DestinationDescription where
  parseJSON =
    withObject
      "ExtendedS3DestinationDescription"
      ( \x ->
          ExtendedS3DestinationDescription'
            <$> (x .:? "S3BackupMode")
            <*> (x .:? "S3BackupDescription")
            <*> (x .:? "Prefix")
            <*> (x .:? "CloudWatchLoggingOptions")
            <*> (x .:? "ErrorOutputPrefix")
            <*> (x .:? "DataFormatConversionConfiguration")
            <*> (x .:? "ProcessingConfiguration")
            <*> (x .: "RoleARN")
            <*> (x .: "BucketARN")
            <*> (x .: "BufferingHints")
            <*> (x .: "CompressionFormat")
            <*> (x .: "EncryptionConfiguration")
      )

instance Hashable ExtendedS3DestinationDescription

instance NFData ExtendedS3DestinationDescription
