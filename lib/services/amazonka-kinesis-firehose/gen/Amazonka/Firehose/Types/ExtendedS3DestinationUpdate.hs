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
-- Module      : Amazonka.Firehose.Types.ExtendedS3DestinationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.ExtendedS3DestinationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.BufferingHints
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.CompressionFormat
import Amazonka.Firehose.Types.DataFormatConversionConfiguration
import Amazonka.Firehose.Types.DynamicPartitioningConfiguration
import Amazonka.Firehose.Types.EncryptionConfiguration
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.S3BackupMode
import Amazonka.Firehose.Types.S3DestinationUpdate
import qualified Amazonka.Prelude as Prelude

-- | Describes an update for a destination in Amazon S3.
--
-- /See:/ 'newExtendedS3DestinationUpdate' smart constructor.
data ExtendedS3DestinationUpdate = ExtendedS3DestinationUpdate'
  { -- | The ARN of the S3 bucket. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
    bucketARN :: Prelude.Maybe Prelude.Text,
    -- | The buffering option.
    bufferingHints :: Prelude.Maybe BufferingHints,
    -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The compression format. If no value is specified, the default is
    -- @UNCOMPRESSED@.
    compressionFormat :: Prelude.Maybe CompressionFormat,
    -- | The serializer, deserializer, and schema for converting data from the
    -- JSON format to the Parquet or ORC format before writing it to Amazon S3.
    dataFormatConversionConfiguration :: Prelude.Maybe DataFormatConversionConfiguration,
    -- | The configuration of the dynamic partitioning mechanism that creates
    -- smaller data sets from the streaming data by partitioning it based on
    -- partition keys. Currently, dynamic partitioning is only supported for
    -- Amazon S3 destinations.
    dynamicPartitioningConfiguration :: Prelude.Maybe DynamicPartitioningConfiguration,
    -- | The encryption configuration. If no value is specified, the default is
    -- no encryption.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | A prefix that Kinesis Data Firehose evaluates and adds to failed records
    -- before writing them to S3. This prefix appears immediately following the
    -- bucket name. For information about how to specify this prefix, see
    -- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
    errorOutputPrefix :: Prelude.Maybe Prelude.Text,
    -- | The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
    -- delivered Amazon S3 files. You can also specify a custom prefix, as
    -- described in
    -- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The data processing configuration.
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
    -- For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | You can update a delivery stream to enable Amazon S3 backup if it is
    -- disabled. If backup is enabled, you can\'t update the delivery stream to
    -- disable it.
    s3BackupMode :: Prelude.Maybe S3BackupMode,
    -- | The Amazon S3 destination for backup.
    s3BackupUpdate :: Prelude.Maybe S3DestinationUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExtendedS3DestinationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketARN', 'extendedS3DestinationUpdate_bucketARN' - The ARN of the S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
--
-- 'bufferingHints', 'extendedS3DestinationUpdate_bufferingHints' - The buffering option.
--
-- 'cloudWatchLoggingOptions', 'extendedS3DestinationUpdate_cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- 'compressionFormat', 'extendedS3DestinationUpdate_compressionFormat' - The compression format. If no value is specified, the default is
-- @UNCOMPRESSED@.
--
-- 'dataFormatConversionConfiguration', 'extendedS3DestinationUpdate_dataFormatConversionConfiguration' - The serializer, deserializer, and schema for converting data from the
-- JSON format to the Parquet or ORC format before writing it to Amazon S3.
--
-- 'dynamicPartitioningConfiguration', 'extendedS3DestinationUpdate_dynamicPartitioningConfiguration' - The configuration of the dynamic partitioning mechanism that creates
-- smaller data sets from the streaming data by partitioning it based on
-- partition keys. Currently, dynamic partitioning is only supported for
-- Amazon S3 destinations.
--
-- 'encryptionConfiguration', 'extendedS3DestinationUpdate_encryptionConfiguration' - The encryption configuration. If no value is specified, the default is
-- no encryption.
--
-- 'errorOutputPrefix', 'extendedS3DestinationUpdate_errorOutputPrefix' - A prefix that Kinesis Data Firehose evaluates and adds to failed records
-- before writing them to S3. This prefix appears immediately following the
-- bucket name. For information about how to specify this prefix, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
--
-- 'prefix', 'extendedS3DestinationUpdate_prefix' - The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered Amazon S3 files. You can also specify a custom prefix, as
-- described in
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
--
-- 'processingConfiguration', 'extendedS3DestinationUpdate_processingConfiguration' - The data processing configuration.
--
-- 'roleARN', 'extendedS3DestinationUpdate_roleARN' - The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
--
-- 's3BackupMode', 'extendedS3DestinationUpdate_s3BackupMode' - You can update a delivery stream to enable Amazon S3 backup if it is
-- disabled. If backup is enabled, you can\'t update the delivery stream to
-- disable it.
--
-- 's3BackupUpdate', 'extendedS3DestinationUpdate_s3BackupUpdate' - The Amazon S3 destination for backup.
newExtendedS3DestinationUpdate ::
  ExtendedS3DestinationUpdate
newExtendedS3DestinationUpdate =
  ExtendedS3DestinationUpdate'
    { bucketARN =
        Prelude.Nothing,
      bufferingHints = Prelude.Nothing,
      cloudWatchLoggingOptions = Prelude.Nothing,
      compressionFormat = Prelude.Nothing,
      dataFormatConversionConfiguration =
        Prelude.Nothing,
      dynamicPartitioningConfiguration =
        Prelude.Nothing,
      encryptionConfiguration = Prelude.Nothing,
      errorOutputPrefix = Prelude.Nothing,
      prefix = Prelude.Nothing,
      processingConfiguration = Prelude.Nothing,
      roleARN = Prelude.Nothing,
      s3BackupMode = Prelude.Nothing,
      s3BackupUpdate = Prelude.Nothing
    }

-- | The ARN of the S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
extendedS3DestinationUpdate_bucketARN :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe Prelude.Text)
extendedS3DestinationUpdate_bucketARN = Lens.lens (\ExtendedS3DestinationUpdate' {bucketARN} -> bucketARN) (\s@ExtendedS3DestinationUpdate' {} a -> s {bucketARN = a} :: ExtendedS3DestinationUpdate)

-- | The buffering option.
extendedS3DestinationUpdate_bufferingHints :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe BufferingHints)
extendedS3DestinationUpdate_bufferingHints = Lens.lens (\ExtendedS3DestinationUpdate' {bufferingHints} -> bufferingHints) (\s@ExtendedS3DestinationUpdate' {} a -> s {bufferingHints = a} :: ExtendedS3DestinationUpdate)

-- | The Amazon CloudWatch logging options for your delivery stream.
extendedS3DestinationUpdate_cloudWatchLoggingOptions :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe CloudWatchLoggingOptions)
extendedS3DestinationUpdate_cloudWatchLoggingOptions = Lens.lens (\ExtendedS3DestinationUpdate' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@ExtendedS3DestinationUpdate' {} a -> s {cloudWatchLoggingOptions = a} :: ExtendedS3DestinationUpdate)

-- | The compression format. If no value is specified, the default is
-- @UNCOMPRESSED@.
extendedS3DestinationUpdate_compressionFormat :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe CompressionFormat)
extendedS3DestinationUpdate_compressionFormat = Lens.lens (\ExtendedS3DestinationUpdate' {compressionFormat} -> compressionFormat) (\s@ExtendedS3DestinationUpdate' {} a -> s {compressionFormat = a} :: ExtendedS3DestinationUpdate)

-- | The serializer, deserializer, and schema for converting data from the
-- JSON format to the Parquet or ORC format before writing it to Amazon S3.
extendedS3DestinationUpdate_dataFormatConversionConfiguration :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe DataFormatConversionConfiguration)
extendedS3DestinationUpdate_dataFormatConversionConfiguration = Lens.lens (\ExtendedS3DestinationUpdate' {dataFormatConversionConfiguration} -> dataFormatConversionConfiguration) (\s@ExtendedS3DestinationUpdate' {} a -> s {dataFormatConversionConfiguration = a} :: ExtendedS3DestinationUpdate)

-- | The configuration of the dynamic partitioning mechanism that creates
-- smaller data sets from the streaming data by partitioning it based on
-- partition keys. Currently, dynamic partitioning is only supported for
-- Amazon S3 destinations.
extendedS3DestinationUpdate_dynamicPartitioningConfiguration :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe DynamicPartitioningConfiguration)
extendedS3DestinationUpdate_dynamicPartitioningConfiguration = Lens.lens (\ExtendedS3DestinationUpdate' {dynamicPartitioningConfiguration} -> dynamicPartitioningConfiguration) (\s@ExtendedS3DestinationUpdate' {} a -> s {dynamicPartitioningConfiguration = a} :: ExtendedS3DestinationUpdate)

-- | The encryption configuration. If no value is specified, the default is
-- no encryption.
extendedS3DestinationUpdate_encryptionConfiguration :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe EncryptionConfiguration)
extendedS3DestinationUpdate_encryptionConfiguration = Lens.lens (\ExtendedS3DestinationUpdate' {encryptionConfiguration} -> encryptionConfiguration) (\s@ExtendedS3DestinationUpdate' {} a -> s {encryptionConfiguration = a} :: ExtendedS3DestinationUpdate)

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records
-- before writing them to S3. This prefix appears immediately following the
-- bucket name. For information about how to specify this prefix, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
extendedS3DestinationUpdate_errorOutputPrefix :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe Prelude.Text)
extendedS3DestinationUpdate_errorOutputPrefix = Lens.lens (\ExtendedS3DestinationUpdate' {errorOutputPrefix} -> errorOutputPrefix) (\s@ExtendedS3DestinationUpdate' {} a -> s {errorOutputPrefix = a} :: ExtendedS3DestinationUpdate)

-- | The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered Amazon S3 files. You can also specify a custom prefix, as
-- described in
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
extendedS3DestinationUpdate_prefix :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe Prelude.Text)
extendedS3DestinationUpdate_prefix = Lens.lens (\ExtendedS3DestinationUpdate' {prefix} -> prefix) (\s@ExtendedS3DestinationUpdate' {} a -> s {prefix = a} :: ExtendedS3DestinationUpdate)

-- | The data processing configuration.
extendedS3DestinationUpdate_processingConfiguration :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe ProcessingConfiguration)
extendedS3DestinationUpdate_processingConfiguration = Lens.lens (\ExtendedS3DestinationUpdate' {processingConfiguration} -> processingConfiguration) (\s@ExtendedS3DestinationUpdate' {} a -> s {processingConfiguration = a} :: ExtendedS3DestinationUpdate)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
extendedS3DestinationUpdate_roleARN :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe Prelude.Text)
extendedS3DestinationUpdate_roleARN = Lens.lens (\ExtendedS3DestinationUpdate' {roleARN} -> roleARN) (\s@ExtendedS3DestinationUpdate' {} a -> s {roleARN = a} :: ExtendedS3DestinationUpdate)

-- | You can update a delivery stream to enable Amazon S3 backup if it is
-- disabled. If backup is enabled, you can\'t update the delivery stream to
-- disable it.
extendedS3DestinationUpdate_s3BackupMode :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe S3BackupMode)
extendedS3DestinationUpdate_s3BackupMode = Lens.lens (\ExtendedS3DestinationUpdate' {s3BackupMode} -> s3BackupMode) (\s@ExtendedS3DestinationUpdate' {} a -> s {s3BackupMode = a} :: ExtendedS3DestinationUpdate)

-- | The Amazon S3 destination for backup.
extendedS3DestinationUpdate_s3BackupUpdate :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe S3DestinationUpdate)
extendedS3DestinationUpdate_s3BackupUpdate = Lens.lens (\ExtendedS3DestinationUpdate' {s3BackupUpdate} -> s3BackupUpdate) (\s@ExtendedS3DestinationUpdate' {} a -> s {s3BackupUpdate = a} :: ExtendedS3DestinationUpdate)

instance Prelude.Hashable ExtendedS3DestinationUpdate where
  hashWithSalt _salt ExtendedS3DestinationUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` bucketARN
      `Prelude.hashWithSalt` bufferingHints
      `Prelude.hashWithSalt` cloudWatchLoggingOptions
      `Prelude.hashWithSalt` compressionFormat
      `Prelude.hashWithSalt` dataFormatConversionConfiguration
      `Prelude.hashWithSalt` dynamicPartitioningConfiguration
      `Prelude.hashWithSalt` encryptionConfiguration
      `Prelude.hashWithSalt` errorOutputPrefix
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` processingConfiguration
      `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` s3BackupMode
      `Prelude.hashWithSalt` s3BackupUpdate

instance Prelude.NFData ExtendedS3DestinationUpdate where
  rnf ExtendedS3DestinationUpdate' {..} =
    Prelude.rnf bucketARN
      `Prelude.seq` Prelude.rnf bufferingHints
      `Prelude.seq` Prelude.rnf cloudWatchLoggingOptions
      `Prelude.seq` Prelude.rnf compressionFormat
      `Prelude.seq` Prelude.rnf dataFormatConversionConfiguration
      `Prelude.seq` Prelude.rnf dynamicPartitioningConfiguration
      `Prelude.seq` Prelude.rnf encryptionConfiguration
      `Prelude.seq` Prelude.rnf errorOutputPrefix
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf processingConfiguration
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf s3BackupMode
      `Prelude.seq` Prelude.rnf s3BackupUpdate

instance Data.ToJSON ExtendedS3DestinationUpdate where
  toJSON ExtendedS3DestinationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BucketARN" Data..=) Prelude.<$> bucketARN,
            ("BufferingHints" Data..=)
              Prelude.<$> bufferingHints,
            ("CloudWatchLoggingOptions" Data..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("CompressionFormat" Data..=)
              Prelude.<$> compressionFormat,
            ("DataFormatConversionConfiguration" Data..=)
              Prelude.<$> dataFormatConversionConfiguration,
            ("DynamicPartitioningConfiguration" Data..=)
              Prelude.<$> dynamicPartitioningConfiguration,
            ("EncryptionConfiguration" Data..=)
              Prelude.<$> encryptionConfiguration,
            ("ErrorOutputPrefix" Data..=)
              Prelude.<$> errorOutputPrefix,
            ("Prefix" Data..=) Prelude.<$> prefix,
            ("ProcessingConfiguration" Data..=)
              Prelude.<$> processingConfiguration,
            ("RoleARN" Data..=) Prelude.<$> roleARN,
            ("S3BackupMode" Data..=) Prelude.<$> s3BackupMode,
            ("S3BackupUpdate" Data..=)
              Prelude.<$> s3BackupUpdate
          ]
      )
