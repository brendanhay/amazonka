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
-- Module      : Network.AWS.Firehose.Types.ExtendedS3DestinationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the configuration of a destination in Amazon S3.
--
-- /See:/ 'newExtendedS3DestinationConfiguration' smart constructor.
data ExtendedS3DestinationConfiguration = ExtendedS3DestinationConfiguration'
  { -- | A prefix that Kinesis Data Firehose evaluates and adds to failed records
    -- before writing them to S3. This prefix appears immediately following the
    -- bucket name. For information about how to specify this prefix, see
    -- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
    errorOutputPrefix :: Prelude.Maybe Prelude.Text,
    -- | The encryption configuration. If no value is specified, the default is
    -- no encryption.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The configuration for backup in Amazon S3.
    s3BackupConfiguration :: Prelude.Maybe S3DestinationConfiguration,
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
    -- | The buffering option.
    bufferingHints :: Prelude.Maybe BufferingHints,
    -- | The Amazon S3 backup mode. After you create a delivery stream, you can
    -- update it to enable Amazon S3 backup if it is disabled. If backup is
    -- enabled, you can\'t update the delivery stream to disable it.
    s3BackupMode :: Prelude.Maybe S3BackupMode,
    -- | The compression format. If no value is specified, the default is
    -- UNCOMPRESSED.
    compressionFormat :: Prelude.Maybe CompressionFormat,
    -- | The Amazon Resource Name (ARN) of the AWS credentials. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    roleARN :: Prelude.Text,
    -- | The ARN of the S3 bucket. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    bucketARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExtendedS3DestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorOutputPrefix', 'extendedS3DestinationConfiguration_errorOutputPrefix' - A prefix that Kinesis Data Firehose evaluates and adds to failed records
-- before writing them to S3. This prefix appears immediately following the
-- bucket name. For information about how to specify this prefix, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
--
-- 'encryptionConfiguration', 'extendedS3DestinationConfiguration_encryptionConfiguration' - The encryption configuration. If no value is specified, the default is
-- no encryption.
--
-- 's3BackupConfiguration', 'extendedS3DestinationConfiguration_s3BackupConfiguration' - The configuration for backup in Amazon S3.
--
-- 'processingConfiguration', 'extendedS3DestinationConfiguration_processingConfiguration' - The data processing configuration.
--
-- 'dataFormatConversionConfiguration', 'extendedS3DestinationConfiguration_dataFormatConversionConfiguration' - The serializer, deserializer, and schema for converting data from the
-- JSON format to the Parquet or ORC format before writing it to Amazon S3.
--
-- 'cloudWatchLoggingOptions', 'extendedS3DestinationConfiguration_cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- 'prefix', 'extendedS3DestinationConfiguration_prefix' - The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered Amazon S3 files. You can also specify a custom prefix, as
-- described in
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
--
-- 'bufferingHints', 'extendedS3DestinationConfiguration_bufferingHints' - The buffering option.
--
-- 's3BackupMode', 'extendedS3DestinationConfiguration_s3BackupMode' - The Amazon S3 backup mode. After you create a delivery stream, you can
-- update it to enable Amazon S3 backup if it is disabled. If backup is
-- enabled, you can\'t update the delivery stream to disable it.
--
-- 'compressionFormat', 'extendedS3DestinationConfiguration_compressionFormat' - The compression format. If no value is specified, the default is
-- UNCOMPRESSED.
--
-- 'roleARN', 'extendedS3DestinationConfiguration_roleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- 'bucketARN', 'extendedS3DestinationConfiguration_bucketARN' - The ARN of the S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
newExtendedS3DestinationConfiguration ::
  -- | 'roleARN'
  Prelude.Text ->
  -- | 'bucketARN'
  Prelude.Text ->
  ExtendedS3DestinationConfiguration
newExtendedS3DestinationConfiguration
  pRoleARN_
  pBucketARN_ =
    ExtendedS3DestinationConfiguration'
      { errorOutputPrefix =
          Prelude.Nothing,
        encryptionConfiguration =
          Prelude.Nothing,
        s3BackupConfiguration = Prelude.Nothing,
        processingConfiguration =
          Prelude.Nothing,
        dataFormatConversionConfiguration =
          Prelude.Nothing,
        cloudWatchLoggingOptions =
          Prelude.Nothing,
        prefix = Prelude.Nothing,
        bufferingHints = Prelude.Nothing,
        s3BackupMode = Prelude.Nothing,
        compressionFormat = Prelude.Nothing,
        roleARN = pRoleARN_,
        bucketARN = pBucketARN_
      }

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records
-- before writing them to S3. This prefix appears immediately following the
-- bucket name. For information about how to specify this prefix, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
extendedS3DestinationConfiguration_errorOutputPrefix :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe Prelude.Text)
extendedS3DestinationConfiguration_errorOutputPrefix = Lens.lens (\ExtendedS3DestinationConfiguration' {errorOutputPrefix} -> errorOutputPrefix) (\s@ExtendedS3DestinationConfiguration' {} a -> s {errorOutputPrefix = a} :: ExtendedS3DestinationConfiguration)

-- | The encryption configuration. If no value is specified, the default is
-- no encryption.
extendedS3DestinationConfiguration_encryptionConfiguration :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe EncryptionConfiguration)
extendedS3DestinationConfiguration_encryptionConfiguration = Lens.lens (\ExtendedS3DestinationConfiguration' {encryptionConfiguration} -> encryptionConfiguration) (\s@ExtendedS3DestinationConfiguration' {} a -> s {encryptionConfiguration = a} :: ExtendedS3DestinationConfiguration)

-- | The configuration for backup in Amazon S3.
extendedS3DestinationConfiguration_s3BackupConfiguration :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe S3DestinationConfiguration)
extendedS3DestinationConfiguration_s3BackupConfiguration = Lens.lens (\ExtendedS3DestinationConfiguration' {s3BackupConfiguration} -> s3BackupConfiguration) (\s@ExtendedS3DestinationConfiguration' {} a -> s {s3BackupConfiguration = a} :: ExtendedS3DestinationConfiguration)

-- | The data processing configuration.
extendedS3DestinationConfiguration_processingConfiguration :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe ProcessingConfiguration)
extendedS3DestinationConfiguration_processingConfiguration = Lens.lens (\ExtendedS3DestinationConfiguration' {processingConfiguration} -> processingConfiguration) (\s@ExtendedS3DestinationConfiguration' {} a -> s {processingConfiguration = a} :: ExtendedS3DestinationConfiguration)

-- | The serializer, deserializer, and schema for converting data from the
-- JSON format to the Parquet or ORC format before writing it to Amazon S3.
extendedS3DestinationConfiguration_dataFormatConversionConfiguration :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe DataFormatConversionConfiguration)
extendedS3DestinationConfiguration_dataFormatConversionConfiguration = Lens.lens (\ExtendedS3DestinationConfiguration' {dataFormatConversionConfiguration} -> dataFormatConversionConfiguration) (\s@ExtendedS3DestinationConfiguration' {} a -> s {dataFormatConversionConfiguration = a} :: ExtendedS3DestinationConfiguration)

-- | The Amazon CloudWatch logging options for your delivery stream.
extendedS3DestinationConfiguration_cloudWatchLoggingOptions :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe CloudWatchLoggingOptions)
extendedS3DestinationConfiguration_cloudWatchLoggingOptions = Lens.lens (\ExtendedS3DestinationConfiguration' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@ExtendedS3DestinationConfiguration' {} a -> s {cloudWatchLoggingOptions = a} :: ExtendedS3DestinationConfiguration)

-- | The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered Amazon S3 files. You can also specify a custom prefix, as
-- described in
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
extendedS3DestinationConfiguration_prefix :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe Prelude.Text)
extendedS3DestinationConfiguration_prefix = Lens.lens (\ExtendedS3DestinationConfiguration' {prefix} -> prefix) (\s@ExtendedS3DestinationConfiguration' {} a -> s {prefix = a} :: ExtendedS3DestinationConfiguration)

-- | The buffering option.
extendedS3DestinationConfiguration_bufferingHints :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe BufferingHints)
extendedS3DestinationConfiguration_bufferingHints = Lens.lens (\ExtendedS3DestinationConfiguration' {bufferingHints} -> bufferingHints) (\s@ExtendedS3DestinationConfiguration' {} a -> s {bufferingHints = a} :: ExtendedS3DestinationConfiguration)

-- | The Amazon S3 backup mode. After you create a delivery stream, you can
-- update it to enable Amazon S3 backup if it is disabled. If backup is
-- enabled, you can\'t update the delivery stream to disable it.
extendedS3DestinationConfiguration_s3BackupMode :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe S3BackupMode)
extendedS3DestinationConfiguration_s3BackupMode = Lens.lens (\ExtendedS3DestinationConfiguration' {s3BackupMode} -> s3BackupMode) (\s@ExtendedS3DestinationConfiguration' {} a -> s {s3BackupMode = a} :: ExtendedS3DestinationConfiguration)

-- | The compression format. If no value is specified, the default is
-- UNCOMPRESSED.
extendedS3DestinationConfiguration_compressionFormat :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe CompressionFormat)
extendedS3DestinationConfiguration_compressionFormat = Lens.lens (\ExtendedS3DestinationConfiguration' {compressionFormat} -> compressionFormat) (\s@ExtendedS3DestinationConfiguration' {} a -> s {compressionFormat = a} :: ExtendedS3DestinationConfiguration)

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
extendedS3DestinationConfiguration_roleARN :: Lens.Lens' ExtendedS3DestinationConfiguration Prelude.Text
extendedS3DestinationConfiguration_roleARN = Lens.lens (\ExtendedS3DestinationConfiguration' {roleARN} -> roleARN) (\s@ExtendedS3DestinationConfiguration' {} a -> s {roleARN = a} :: ExtendedS3DestinationConfiguration)

-- | The ARN of the S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
extendedS3DestinationConfiguration_bucketARN :: Lens.Lens' ExtendedS3DestinationConfiguration Prelude.Text
extendedS3DestinationConfiguration_bucketARN = Lens.lens (\ExtendedS3DestinationConfiguration' {bucketARN} -> bucketARN) (\s@ExtendedS3DestinationConfiguration' {} a -> s {bucketARN = a} :: ExtendedS3DestinationConfiguration)

instance
  Prelude.Hashable
    ExtendedS3DestinationConfiguration

instance
  Prelude.NFData
    ExtendedS3DestinationConfiguration

instance
  Prelude.ToJSON
    ExtendedS3DestinationConfiguration
  where
  toJSON ExtendedS3DestinationConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ErrorOutputPrefix" Prelude..=)
              Prelude.<$> errorOutputPrefix,
            ("EncryptionConfiguration" Prelude..=)
              Prelude.<$> encryptionConfiguration,
            ("S3BackupConfiguration" Prelude..=)
              Prelude.<$> s3BackupConfiguration,
            ("ProcessingConfiguration" Prelude..=)
              Prelude.<$> processingConfiguration,
            ("DataFormatConversionConfiguration" Prelude..=)
              Prelude.<$> dataFormatConversionConfiguration,
            ("CloudWatchLoggingOptions" Prelude..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("Prefix" Prelude..=) Prelude.<$> prefix,
            ("BufferingHints" Prelude..=)
              Prelude.<$> bufferingHints,
            ("S3BackupMode" Prelude..=) Prelude.<$> s3BackupMode,
            ("CompressionFormat" Prelude..=)
              Prelude.<$> compressionFormat,
            Prelude.Just ("RoleARN" Prelude..= roleARN),
            Prelude.Just ("BucketARN" Prelude..= bucketARN)
          ]
      )
