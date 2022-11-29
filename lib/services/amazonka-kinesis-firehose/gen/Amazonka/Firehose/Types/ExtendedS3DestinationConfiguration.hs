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
-- Module      : Amazonka.Firehose.Types.ExtendedS3DestinationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.ExtendedS3DestinationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Firehose.Types.BufferingHints
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.CompressionFormat
import Amazonka.Firehose.Types.DataFormatConversionConfiguration
import Amazonka.Firehose.Types.DynamicPartitioningConfiguration
import Amazonka.Firehose.Types.EncryptionConfiguration
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.S3BackupMode
import Amazonka.Firehose.Types.S3DestinationConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of a destination in Amazon S3.
--
-- /See:/ 'newExtendedS3DestinationConfiguration' smart constructor.
data ExtendedS3DestinationConfiguration = ExtendedS3DestinationConfiguration'
  { -- | The configuration for backup in Amazon S3.
    s3BackupConfiguration :: Prelude.Maybe S3DestinationConfiguration,
    -- | The data processing configuration.
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The buffering option.
    bufferingHints :: Prelude.Maybe BufferingHints,
    -- | The serializer, deserializer, and schema for converting data from the
    -- JSON format to the Parquet or ORC format before writing it to Amazon S3.
    dataFormatConversionConfiguration :: Prelude.Maybe DataFormatConversionConfiguration,
    -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The Amazon S3 backup mode. After you create a delivery stream, you can
    -- update it to enable Amazon S3 backup if it is disabled. If backup is
    -- enabled, you can\'t update the delivery stream to disable it.
    s3BackupMode :: Prelude.Maybe S3BackupMode,
    -- | The encryption configuration. If no value is specified, the default is
    -- no encryption.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
    -- delivered Amazon S3 files. You can also specify a custom prefix, as
    -- described in
    -- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The compression format. If no value is specified, the default is
    -- UNCOMPRESSED.
    compressionFormat :: Prelude.Maybe CompressionFormat,
    -- | The configuration of the dynamic partitioning mechanism that creates
    -- smaller data sets from the streaming data by partitioning it based on
    -- partition keys. Currently, dynamic partitioning is only supported for
    -- Amazon S3 destinations. For more information, see
    -- <https://docs.aws.amazon.com/firehose/latest/dev/dynamic-partitioning.html>
    dynamicPartitioningConfiguration :: Prelude.Maybe DynamicPartitioningConfiguration,
    -- | A prefix that Kinesis Data Firehose evaluates and adds to failed records
    -- before writing them to S3. This prefix appears immediately following the
    -- bucket name. For information about how to specify this prefix, see
    -- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
    errorOutputPrefix :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS credentials. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    roleARN :: Prelude.Text,
    -- | The ARN of the S3 bucket. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    bucketARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExtendedS3DestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3BackupConfiguration', 'extendedS3DestinationConfiguration_s3BackupConfiguration' - The configuration for backup in Amazon S3.
--
-- 'processingConfiguration', 'extendedS3DestinationConfiguration_processingConfiguration' - The data processing configuration.
--
-- 'bufferingHints', 'extendedS3DestinationConfiguration_bufferingHints' - The buffering option.
--
-- 'dataFormatConversionConfiguration', 'extendedS3DestinationConfiguration_dataFormatConversionConfiguration' - The serializer, deserializer, and schema for converting data from the
-- JSON format to the Parquet or ORC format before writing it to Amazon S3.
--
-- 'cloudWatchLoggingOptions', 'extendedS3DestinationConfiguration_cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- 's3BackupMode', 'extendedS3DestinationConfiguration_s3BackupMode' - The Amazon S3 backup mode. After you create a delivery stream, you can
-- update it to enable Amazon S3 backup if it is disabled. If backup is
-- enabled, you can\'t update the delivery stream to disable it.
--
-- 'encryptionConfiguration', 'extendedS3DestinationConfiguration_encryptionConfiguration' - The encryption configuration. If no value is specified, the default is
-- no encryption.
--
-- 'prefix', 'extendedS3DestinationConfiguration_prefix' - The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered Amazon S3 files. You can also specify a custom prefix, as
-- described in
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
--
-- 'compressionFormat', 'extendedS3DestinationConfiguration_compressionFormat' - The compression format. If no value is specified, the default is
-- UNCOMPRESSED.
--
-- 'dynamicPartitioningConfiguration', 'extendedS3DestinationConfiguration_dynamicPartitioningConfiguration' - The configuration of the dynamic partitioning mechanism that creates
-- smaller data sets from the streaming data by partitioning it based on
-- partition keys. Currently, dynamic partitioning is only supported for
-- Amazon S3 destinations. For more information, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/dynamic-partitioning.html>
--
-- 'errorOutputPrefix', 'extendedS3DestinationConfiguration_errorOutputPrefix' - A prefix that Kinesis Data Firehose evaluates and adds to failed records
-- before writing them to S3. This prefix appears immediately following the
-- bucket name. For information about how to specify this prefix, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
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
      { s3BackupConfiguration =
          Prelude.Nothing,
        processingConfiguration =
          Prelude.Nothing,
        bufferingHints = Prelude.Nothing,
        dataFormatConversionConfiguration =
          Prelude.Nothing,
        cloudWatchLoggingOptions =
          Prelude.Nothing,
        s3BackupMode = Prelude.Nothing,
        encryptionConfiguration =
          Prelude.Nothing,
        prefix = Prelude.Nothing,
        compressionFormat = Prelude.Nothing,
        dynamicPartitioningConfiguration =
          Prelude.Nothing,
        errorOutputPrefix = Prelude.Nothing,
        roleARN = pRoleARN_,
        bucketARN = pBucketARN_
      }

-- | The configuration for backup in Amazon S3.
extendedS3DestinationConfiguration_s3BackupConfiguration :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe S3DestinationConfiguration)
extendedS3DestinationConfiguration_s3BackupConfiguration = Lens.lens (\ExtendedS3DestinationConfiguration' {s3BackupConfiguration} -> s3BackupConfiguration) (\s@ExtendedS3DestinationConfiguration' {} a -> s {s3BackupConfiguration = a} :: ExtendedS3DestinationConfiguration)

-- | The data processing configuration.
extendedS3DestinationConfiguration_processingConfiguration :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe ProcessingConfiguration)
extendedS3DestinationConfiguration_processingConfiguration = Lens.lens (\ExtendedS3DestinationConfiguration' {processingConfiguration} -> processingConfiguration) (\s@ExtendedS3DestinationConfiguration' {} a -> s {processingConfiguration = a} :: ExtendedS3DestinationConfiguration)

-- | The buffering option.
extendedS3DestinationConfiguration_bufferingHints :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe BufferingHints)
extendedS3DestinationConfiguration_bufferingHints = Lens.lens (\ExtendedS3DestinationConfiguration' {bufferingHints} -> bufferingHints) (\s@ExtendedS3DestinationConfiguration' {} a -> s {bufferingHints = a} :: ExtendedS3DestinationConfiguration)

-- | The serializer, deserializer, and schema for converting data from the
-- JSON format to the Parquet or ORC format before writing it to Amazon S3.
extendedS3DestinationConfiguration_dataFormatConversionConfiguration :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe DataFormatConversionConfiguration)
extendedS3DestinationConfiguration_dataFormatConversionConfiguration = Lens.lens (\ExtendedS3DestinationConfiguration' {dataFormatConversionConfiguration} -> dataFormatConversionConfiguration) (\s@ExtendedS3DestinationConfiguration' {} a -> s {dataFormatConversionConfiguration = a} :: ExtendedS3DestinationConfiguration)

-- | The Amazon CloudWatch logging options for your delivery stream.
extendedS3DestinationConfiguration_cloudWatchLoggingOptions :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe CloudWatchLoggingOptions)
extendedS3DestinationConfiguration_cloudWatchLoggingOptions = Lens.lens (\ExtendedS3DestinationConfiguration' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@ExtendedS3DestinationConfiguration' {} a -> s {cloudWatchLoggingOptions = a} :: ExtendedS3DestinationConfiguration)

-- | The Amazon S3 backup mode. After you create a delivery stream, you can
-- update it to enable Amazon S3 backup if it is disabled. If backup is
-- enabled, you can\'t update the delivery stream to disable it.
extendedS3DestinationConfiguration_s3BackupMode :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe S3BackupMode)
extendedS3DestinationConfiguration_s3BackupMode = Lens.lens (\ExtendedS3DestinationConfiguration' {s3BackupMode} -> s3BackupMode) (\s@ExtendedS3DestinationConfiguration' {} a -> s {s3BackupMode = a} :: ExtendedS3DestinationConfiguration)

-- | The encryption configuration. If no value is specified, the default is
-- no encryption.
extendedS3DestinationConfiguration_encryptionConfiguration :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe EncryptionConfiguration)
extendedS3DestinationConfiguration_encryptionConfiguration = Lens.lens (\ExtendedS3DestinationConfiguration' {encryptionConfiguration} -> encryptionConfiguration) (\s@ExtendedS3DestinationConfiguration' {} a -> s {encryptionConfiguration = a} :: ExtendedS3DestinationConfiguration)

-- | The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered Amazon S3 files. You can also specify a custom prefix, as
-- described in
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
extendedS3DestinationConfiguration_prefix :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe Prelude.Text)
extendedS3DestinationConfiguration_prefix = Lens.lens (\ExtendedS3DestinationConfiguration' {prefix} -> prefix) (\s@ExtendedS3DestinationConfiguration' {} a -> s {prefix = a} :: ExtendedS3DestinationConfiguration)

-- | The compression format. If no value is specified, the default is
-- UNCOMPRESSED.
extendedS3DestinationConfiguration_compressionFormat :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe CompressionFormat)
extendedS3DestinationConfiguration_compressionFormat = Lens.lens (\ExtendedS3DestinationConfiguration' {compressionFormat} -> compressionFormat) (\s@ExtendedS3DestinationConfiguration' {} a -> s {compressionFormat = a} :: ExtendedS3DestinationConfiguration)

-- | The configuration of the dynamic partitioning mechanism that creates
-- smaller data sets from the streaming data by partitioning it based on
-- partition keys. Currently, dynamic partitioning is only supported for
-- Amazon S3 destinations. For more information, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/dynamic-partitioning.html>
extendedS3DestinationConfiguration_dynamicPartitioningConfiguration :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe DynamicPartitioningConfiguration)
extendedS3DestinationConfiguration_dynamicPartitioningConfiguration = Lens.lens (\ExtendedS3DestinationConfiguration' {dynamicPartitioningConfiguration} -> dynamicPartitioningConfiguration) (\s@ExtendedS3DestinationConfiguration' {} a -> s {dynamicPartitioningConfiguration = a} :: ExtendedS3DestinationConfiguration)

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records
-- before writing them to S3. This prefix appears immediately following the
-- bucket name. For information about how to specify this prefix, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
extendedS3DestinationConfiguration_errorOutputPrefix :: Lens.Lens' ExtendedS3DestinationConfiguration (Prelude.Maybe Prelude.Text)
extendedS3DestinationConfiguration_errorOutputPrefix = Lens.lens (\ExtendedS3DestinationConfiguration' {errorOutputPrefix} -> errorOutputPrefix) (\s@ExtendedS3DestinationConfiguration' {} a -> s {errorOutputPrefix = a} :: ExtendedS3DestinationConfiguration)

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
  where
  hashWithSalt
    _salt
    ExtendedS3DestinationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` s3BackupConfiguration
        `Prelude.hashWithSalt` processingConfiguration
        `Prelude.hashWithSalt` bufferingHints
        `Prelude.hashWithSalt` dataFormatConversionConfiguration
        `Prelude.hashWithSalt` cloudWatchLoggingOptions
        `Prelude.hashWithSalt` s3BackupMode
        `Prelude.hashWithSalt` encryptionConfiguration
        `Prelude.hashWithSalt` prefix
        `Prelude.hashWithSalt` compressionFormat
        `Prelude.hashWithSalt` dynamicPartitioningConfiguration
        `Prelude.hashWithSalt` errorOutputPrefix
        `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` bucketARN

instance
  Prelude.NFData
    ExtendedS3DestinationConfiguration
  where
  rnf ExtendedS3DestinationConfiguration' {..} =
    Prelude.rnf s3BackupConfiguration
      `Prelude.seq` Prelude.rnf processingConfiguration
      `Prelude.seq` Prelude.rnf bufferingHints
      `Prelude.seq` Prelude.rnf dataFormatConversionConfiguration
      `Prelude.seq` Prelude.rnf cloudWatchLoggingOptions
      `Prelude.seq` Prelude.rnf s3BackupMode
      `Prelude.seq` Prelude.rnf encryptionConfiguration
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf compressionFormat
      `Prelude.seq` Prelude.rnf dynamicPartitioningConfiguration
      `Prelude.seq` Prelude.rnf errorOutputPrefix
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf bucketARN

instance
  Core.ToJSON
    ExtendedS3DestinationConfiguration
  where
  toJSON ExtendedS3DestinationConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("S3BackupConfiguration" Core..=)
              Prelude.<$> s3BackupConfiguration,
            ("ProcessingConfiguration" Core..=)
              Prelude.<$> processingConfiguration,
            ("BufferingHints" Core..=)
              Prelude.<$> bufferingHints,
            ("DataFormatConversionConfiguration" Core..=)
              Prelude.<$> dataFormatConversionConfiguration,
            ("CloudWatchLoggingOptions" Core..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("S3BackupMode" Core..=) Prelude.<$> s3BackupMode,
            ("EncryptionConfiguration" Core..=)
              Prelude.<$> encryptionConfiguration,
            ("Prefix" Core..=) Prelude.<$> prefix,
            ("CompressionFormat" Core..=)
              Prelude.<$> compressionFormat,
            ("DynamicPartitioningConfiguration" Core..=)
              Prelude.<$> dynamicPartitioningConfiguration,
            ("ErrorOutputPrefix" Core..=)
              Prelude.<$> errorOutputPrefix,
            Prelude.Just ("RoleARN" Core..= roleARN),
            Prelude.Just ("BucketARN" Core..= bucketARN)
          ]
      )
