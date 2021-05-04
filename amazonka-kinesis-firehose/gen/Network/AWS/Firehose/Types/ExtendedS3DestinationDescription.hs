{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ExtendedS3DestinationDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a destination in Amazon S3.
--
-- /See:/ 'newExtendedS3DestinationDescription' smart constructor.
data ExtendedS3DestinationDescription = ExtendedS3DestinationDescription'
  { -- | A prefix that Kinesis Data Firehose evaluates and adds to failed records
    -- before writing them to S3. This prefix appears immediately following the
    -- bucket name. For information about how to specify this prefix, see
    -- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
    errorOutputPrefix :: Prelude.Maybe Prelude.Text,
    -- | The data processing configuration.
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The serializer, deserializer, and schema for converting data from the
    -- JSON format to the Parquet or ORC format before writing it to Amazon S3.
    dataFormatConversionConfiguration :: Prelude.Maybe DataFormatConversionConfiguration,
    -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
    -- delivered Amazon S3 files. You can also specify a custom prefix, as
    -- described in
    -- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The configuration for backup in Amazon S3.
    s3BackupDescription :: Prelude.Maybe S3DestinationDescription,
    -- | The Amazon S3 backup mode.
    s3BackupMode :: Prelude.Maybe S3BackupMode,
    -- | The Amazon Resource Name (ARN) of the AWS credentials. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    roleARN :: Prelude.Text,
    -- | The ARN of the S3 bucket. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    bucketARN :: Prelude.Text,
    -- | The buffering option.
    bufferingHints :: BufferingHints,
    -- | The compression format. If no value is specified, the default is
    -- @UNCOMPRESSED@.
    compressionFormat :: CompressionFormat,
    -- | The encryption configuration. If no value is specified, the default is
    -- no encryption.
    encryptionConfiguration :: EncryptionConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExtendedS3DestinationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorOutputPrefix', 'extendedS3DestinationDescription_errorOutputPrefix' - A prefix that Kinesis Data Firehose evaluates and adds to failed records
-- before writing them to S3. This prefix appears immediately following the
-- bucket name. For information about how to specify this prefix, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
--
-- 'processingConfiguration', 'extendedS3DestinationDescription_processingConfiguration' - The data processing configuration.
--
-- 'dataFormatConversionConfiguration', 'extendedS3DestinationDescription_dataFormatConversionConfiguration' - The serializer, deserializer, and schema for converting data from the
-- JSON format to the Parquet or ORC format before writing it to Amazon S3.
--
-- 'cloudWatchLoggingOptions', 'extendedS3DestinationDescription_cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- 'prefix', 'extendedS3DestinationDescription_prefix' - The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered Amazon S3 files. You can also specify a custom prefix, as
-- described in
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
--
-- 's3BackupDescription', 'extendedS3DestinationDescription_s3BackupDescription' - The configuration for backup in Amazon S3.
--
-- 's3BackupMode', 'extendedS3DestinationDescription_s3BackupMode' - The Amazon S3 backup mode.
--
-- 'roleARN', 'extendedS3DestinationDescription_roleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- 'bucketARN', 'extendedS3DestinationDescription_bucketARN' - The ARN of the S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- 'bufferingHints', 'extendedS3DestinationDescription_bufferingHints' - The buffering option.
--
-- 'compressionFormat', 'extendedS3DestinationDescription_compressionFormat' - The compression format. If no value is specified, the default is
-- @UNCOMPRESSED@.
--
-- 'encryptionConfiguration', 'extendedS3DestinationDescription_encryptionConfiguration' - The encryption configuration. If no value is specified, the default is
-- no encryption.
newExtendedS3DestinationDescription ::
  -- | 'roleARN'
  Prelude.Text ->
  -- | 'bucketARN'
  Prelude.Text ->
  -- | 'bufferingHints'
  BufferingHints ->
  -- | 'compressionFormat'
  CompressionFormat ->
  -- | 'encryptionConfiguration'
  EncryptionConfiguration ->
  ExtendedS3DestinationDescription
newExtendedS3DestinationDescription
  pRoleARN_
  pBucketARN_
  pBufferingHints_
  pCompressionFormat_
  pEncryptionConfiguration_ =
    ExtendedS3DestinationDescription'
      { errorOutputPrefix =
          Prelude.Nothing,
        processingConfiguration = Prelude.Nothing,
        dataFormatConversionConfiguration =
          Prelude.Nothing,
        cloudWatchLoggingOptions =
          Prelude.Nothing,
        prefix = Prelude.Nothing,
        s3BackupDescription = Prelude.Nothing,
        s3BackupMode = Prelude.Nothing,
        roleARN = pRoleARN_,
        bucketARN = pBucketARN_,
        bufferingHints = pBufferingHints_,
        compressionFormat = pCompressionFormat_,
        encryptionConfiguration =
          pEncryptionConfiguration_
      }

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records
-- before writing them to S3. This prefix appears immediately following the
-- bucket name. For information about how to specify this prefix, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
extendedS3DestinationDescription_errorOutputPrefix :: Lens.Lens' ExtendedS3DestinationDescription (Prelude.Maybe Prelude.Text)
extendedS3DestinationDescription_errorOutputPrefix = Lens.lens (\ExtendedS3DestinationDescription' {errorOutputPrefix} -> errorOutputPrefix) (\s@ExtendedS3DestinationDescription' {} a -> s {errorOutputPrefix = a} :: ExtendedS3DestinationDescription)

-- | The data processing configuration.
extendedS3DestinationDescription_processingConfiguration :: Lens.Lens' ExtendedS3DestinationDescription (Prelude.Maybe ProcessingConfiguration)
extendedS3DestinationDescription_processingConfiguration = Lens.lens (\ExtendedS3DestinationDescription' {processingConfiguration} -> processingConfiguration) (\s@ExtendedS3DestinationDescription' {} a -> s {processingConfiguration = a} :: ExtendedS3DestinationDescription)

-- | The serializer, deserializer, and schema for converting data from the
-- JSON format to the Parquet or ORC format before writing it to Amazon S3.
extendedS3DestinationDescription_dataFormatConversionConfiguration :: Lens.Lens' ExtendedS3DestinationDescription (Prelude.Maybe DataFormatConversionConfiguration)
extendedS3DestinationDescription_dataFormatConversionConfiguration = Lens.lens (\ExtendedS3DestinationDescription' {dataFormatConversionConfiguration} -> dataFormatConversionConfiguration) (\s@ExtendedS3DestinationDescription' {} a -> s {dataFormatConversionConfiguration = a} :: ExtendedS3DestinationDescription)

-- | The Amazon CloudWatch logging options for your delivery stream.
extendedS3DestinationDescription_cloudWatchLoggingOptions :: Lens.Lens' ExtendedS3DestinationDescription (Prelude.Maybe CloudWatchLoggingOptions)
extendedS3DestinationDescription_cloudWatchLoggingOptions = Lens.lens (\ExtendedS3DestinationDescription' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@ExtendedS3DestinationDescription' {} a -> s {cloudWatchLoggingOptions = a} :: ExtendedS3DestinationDescription)

-- | The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered Amazon S3 files. You can also specify a custom prefix, as
-- described in
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
extendedS3DestinationDescription_prefix :: Lens.Lens' ExtendedS3DestinationDescription (Prelude.Maybe Prelude.Text)
extendedS3DestinationDescription_prefix = Lens.lens (\ExtendedS3DestinationDescription' {prefix} -> prefix) (\s@ExtendedS3DestinationDescription' {} a -> s {prefix = a} :: ExtendedS3DestinationDescription)

-- | The configuration for backup in Amazon S3.
extendedS3DestinationDescription_s3BackupDescription :: Lens.Lens' ExtendedS3DestinationDescription (Prelude.Maybe S3DestinationDescription)
extendedS3DestinationDescription_s3BackupDescription = Lens.lens (\ExtendedS3DestinationDescription' {s3BackupDescription} -> s3BackupDescription) (\s@ExtendedS3DestinationDescription' {} a -> s {s3BackupDescription = a} :: ExtendedS3DestinationDescription)

-- | The Amazon S3 backup mode.
extendedS3DestinationDescription_s3BackupMode :: Lens.Lens' ExtendedS3DestinationDescription (Prelude.Maybe S3BackupMode)
extendedS3DestinationDescription_s3BackupMode = Lens.lens (\ExtendedS3DestinationDescription' {s3BackupMode} -> s3BackupMode) (\s@ExtendedS3DestinationDescription' {} a -> s {s3BackupMode = a} :: ExtendedS3DestinationDescription)

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
extendedS3DestinationDescription_roleARN :: Lens.Lens' ExtendedS3DestinationDescription Prelude.Text
extendedS3DestinationDescription_roleARN = Lens.lens (\ExtendedS3DestinationDescription' {roleARN} -> roleARN) (\s@ExtendedS3DestinationDescription' {} a -> s {roleARN = a} :: ExtendedS3DestinationDescription)

-- | The ARN of the S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
extendedS3DestinationDescription_bucketARN :: Lens.Lens' ExtendedS3DestinationDescription Prelude.Text
extendedS3DestinationDescription_bucketARN = Lens.lens (\ExtendedS3DestinationDescription' {bucketARN} -> bucketARN) (\s@ExtendedS3DestinationDescription' {} a -> s {bucketARN = a} :: ExtendedS3DestinationDescription)

-- | The buffering option.
extendedS3DestinationDescription_bufferingHints :: Lens.Lens' ExtendedS3DestinationDescription BufferingHints
extendedS3DestinationDescription_bufferingHints = Lens.lens (\ExtendedS3DestinationDescription' {bufferingHints} -> bufferingHints) (\s@ExtendedS3DestinationDescription' {} a -> s {bufferingHints = a} :: ExtendedS3DestinationDescription)

-- | The compression format. If no value is specified, the default is
-- @UNCOMPRESSED@.
extendedS3DestinationDescription_compressionFormat :: Lens.Lens' ExtendedS3DestinationDescription CompressionFormat
extendedS3DestinationDescription_compressionFormat = Lens.lens (\ExtendedS3DestinationDescription' {compressionFormat} -> compressionFormat) (\s@ExtendedS3DestinationDescription' {} a -> s {compressionFormat = a} :: ExtendedS3DestinationDescription)

-- | The encryption configuration. If no value is specified, the default is
-- no encryption.
extendedS3DestinationDescription_encryptionConfiguration :: Lens.Lens' ExtendedS3DestinationDescription EncryptionConfiguration
extendedS3DestinationDescription_encryptionConfiguration = Lens.lens (\ExtendedS3DestinationDescription' {encryptionConfiguration} -> encryptionConfiguration) (\s@ExtendedS3DestinationDescription' {} a -> s {encryptionConfiguration = a} :: ExtendedS3DestinationDescription)

instance
  Prelude.FromJSON
    ExtendedS3DestinationDescription
  where
  parseJSON =
    Prelude.withObject
      "ExtendedS3DestinationDescription"
      ( \x ->
          ExtendedS3DestinationDescription'
            Prelude.<$> (x Prelude..:? "ErrorOutputPrefix")
            Prelude.<*> (x Prelude..:? "ProcessingConfiguration")
            Prelude.<*> (x Prelude..:? "DataFormatConversionConfiguration")
            Prelude.<*> (x Prelude..:? "CloudWatchLoggingOptions")
            Prelude.<*> (x Prelude..:? "Prefix")
            Prelude.<*> (x Prelude..:? "S3BackupDescription")
            Prelude.<*> (x Prelude..:? "S3BackupMode")
            Prelude.<*> (x Prelude..: "RoleARN")
            Prelude.<*> (x Prelude..: "BucketARN")
            Prelude.<*> (x Prelude..: "BufferingHints")
            Prelude.<*> (x Prelude..: "CompressionFormat")
            Prelude.<*> (x Prelude..: "EncryptionConfiguration")
      )

instance
  Prelude.Hashable
    ExtendedS3DestinationDescription

instance
  Prelude.NFData
    ExtendedS3DestinationDescription
