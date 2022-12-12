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
-- Module      : Amazonka.Firehose.Types.ExtendedS3DestinationDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.ExtendedS3DestinationDescription where

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
import Amazonka.Firehose.Types.S3DestinationDescription
import qualified Amazonka.Prelude as Prelude

-- | Describes a destination in Amazon S3.
--
-- /See:/ 'newExtendedS3DestinationDescription' smart constructor.
data ExtendedS3DestinationDescription = ExtendedS3DestinationDescription'
  { -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The serializer, deserializer, and schema for converting data from the
    -- JSON format to the Parquet or ORC format before writing it to Amazon S3.
    dataFormatConversionConfiguration :: Prelude.Maybe DataFormatConversionConfiguration,
    -- | The configuration of the dynamic partitioning mechanism that creates
    -- smaller data sets from the streaming data by partitioning it based on
    -- partition keys. Currently, dynamic partitioning is only supported for
    -- Amazon S3 destinations.
    dynamicPartitioningConfiguration :: Prelude.Maybe DynamicPartitioningConfiguration,
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
    -- | The configuration for backup in Amazon S3.
    s3BackupDescription :: Prelude.Maybe S3DestinationDescription,
    -- | The Amazon S3 backup mode.
    s3BackupMode :: Prelude.Maybe S3BackupMode,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
    -- For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
    roleARN :: Prelude.Text,
    -- | The ARN of the S3 bucket. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExtendedS3DestinationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLoggingOptions', 'extendedS3DestinationDescription_cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- 'dataFormatConversionConfiguration', 'extendedS3DestinationDescription_dataFormatConversionConfiguration' - The serializer, deserializer, and schema for converting data from the
-- JSON format to the Parquet or ORC format before writing it to Amazon S3.
--
-- 'dynamicPartitioningConfiguration', 'extendedS3DestinationDescription_dynamicPartitioningConfiguration' - The configuration of the dynamic partitioning mechanism that creates
-- smaller data sets from the streaming data by partitioning it based on
-- partition keys. Currently, dynamic partitioning is only supported for
-- Amazon S3 destinations.
--
-- 'errorOutputPrefix', 'extendedS3DestinationDescription_errorOutputPrefix' - A prefix that Kinesis Data Firehose evaluates and adds to failed records
-- before writing them to S3. This prefix appears immediately following the
-- bucket name. For information about how to specify this prefix, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
--
-- 'prefix', 'extendedS3DestinationDescription_prefix' - The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered Amazon S3 files. You can also specify a custom prefix, as
-- described in
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
--
-- 'processingConfiguration', 'extendedS3DestinationDescription_processingConfiguration' - The data processing configuration.
--
-- 's3BackupDescription', 'extendedS3DestinationDescription_s3BackupDescription' - The configuration for backup in Amazon S3.
--
-- 's3BackupMode', 'extendedS3DestinationDescription_s3BackupMode' - The Amazon S3 backup mode.
--
-- 'roleARN', 'extendedS3DestinationDescription_roleARN' - The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
--
-- 'bucketARN', 'extendedS3DestinationDescription_bucketARN' - The ARN of the S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
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
      { cloudWatchLoggingOptions =
          Prelude.Nothing,
        dataFormatConversionConfiguration =
          Prelude.Nothing,
        dynamicPartitioningConfiguration =
          Prelude.Nothing,
        errorOutputPrefix = Prelude.Nothing,
        prefix = Prelude.Nothing,
        processingConfiguration = Prelude.Nothing,
        s3BackupDescription = Prelude.Nothing,
        s3BackupMode = Prelude.Nothing,
        roleARN = pRoleARN_,
        bucketARN = pBucketARN_,
        bufferingHints = pBufferingHints_,
        compressionFormat = pCompressionFormat_,
        encryptionConfiguration =
          pEncryptionConfiguration_
      }

-- | The Amazon CloudWatch logging options for your delivery stream.
extendedS3DestinationDescription_cloudWatchLoggingOptions :: Lens.Lens' ExtendedS3DestinationDescription (Prelude.Maybe CloudWatchLoggingOptions)
extendedS3DestinationDescription_cloudWatchLoggingOptions = Lens.lens (\ExtendedS3DestinationDescription' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@ExtendedS3DestinationDescription' {} a -> s {cloudWatchLoggingOptions = a} :: ExtendedS3DestinationDescription)

-- | The serializer, deserializer, and schema for converting data from the
-- JSON format to the Parquet or ORC format before writing it to Amazon S3.
extendedS3DestinationDescription_dataFormatConversionConfiguration :: Lens.Lens' ExtendedS3DestinationDescription (Prelude.Maybe DataFormatConversionConfiguration)
extendedS3DestinationDescription_dataFormatConversionConfiguration = Lens.lens (\ExtendedS3DestinationDescription' {dataFormatConversionConfiguration} -> dataFormatConversionConfiguration) (\s@ExtendedS3DestinationDescription' {} a -> s {dataFormatConversionConfiguration = a} :: ExtendedS3DestinationDescription)

-- | The configuration of the dynamic partitioning mechanism that creates
-- smaller data sets from the streaming data by partitioning it based on
-- partition keys. Currently, dynamic partitioning is only supported for
-- Amazon S3 destinations.
extendedS3DestinationDescription_dynamicPartitioningConfiguration :: Lens.Lens' ExtendedS3DestinationDescription (Prelude.Maybe DynamicPartitioningConfiguration)
extendedS3DestinationDescription_dynamicPartitioningConfiguration = Lens.lens (\ExtendedS3DestinationDescription' {dynamicPartitioningConfiguration} -> dynamicPartitioningConfiguration) (\s@ExtendedS3DestinationDescription' {} a -> s {dynamicPartitioningConfiguration = a} :: ExtendedS3DestinationDescription)

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records
-- before writing them to S3. This prefix appears immediately following the
-- bucket name. For information about how to specify this prefix, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
extendedS3DestinationDescription_errorOutputPrefix :: Lens.Lens' ExtendedS3DestinationDescription (Prelude.Maybe Prelude.Text)
extendedS3DestinationDescription_errorOutputPrefix = Lens.lens (\ExtendedS3DestinationDescription' {errorOutputPrefix} -> errorOutputPrefix) (\s@ExtendedS3DestinationDescription' {} a -> s {errorOutputPrefix = a} :: ExtendedS3DestinationDescription)

-- | The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered Amazon S3 files. You can also specify a custom prefix, as
-- described in
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
extendedS3DestinationDescription_prefix :: Lens.Lens' ExtendedS3DestinationDescription (Prelude.Maybe Prelude.Text)
extendedS3DestinationDescription_prefix = Lens.lens (\ExtendedS3DestinationDescription' {prefix} -> prefix) (\s@ExtendedS3DestinationDescription' {} a -> s {prefix = a} :: ExtendedS3DestinationDescription)

-- | The data processing configuration.
extendedS3DestinationDescription_processingConfiguration :: Lens.Lens' ExtendedS3DestinationDescription (Prelude.Maybe ProcessingConfiguration)
extendedS3DestinationDescription_processingConfiguration = Lens.lens (\ExtendedS3DestinationDescription' {processingConfiguration} -> processingConfiguration) (\s@ExtendedS3DestinationDescription' {} a -> s {processingConfiguration = a} :: ExtendedS3DestinationDescription)

-- | The configuration for backup in Amazon S3.
extendedS3DestinationDescription_s3BackupDescription :: Lens.Lens' ExtendedS3DestinationDescription (Prelude.Maybe S3DestinationDescription)
extendedS3DestinationDescription_s3BackupDescription = Lens.lens (\ExtendedS3DestinationDescription' {s3BackupDescription} -> s3BackupDescription) (\s@ExtendedS3DestinationDescription' {} a -> s {s3BackupDescription = a} :: ExtendedS3DestinationDescription)

-- | The Amazon S3 backup mode.
extendedS3DestinationDescription_s3BackupMode :: Lens.Lens' ExtendedS3DestinationDescription (Prelude.Maybe S3BackupMode)
extendedS3DestinationDescription_s3BackupMode = Lens.lens (\ExtendedS3DestinationDescription' {s3BackupMode} -> s3BackupMode) (\s@ExtendedS3DestinationDescription' {} a -> s {s3BackupMode = a} :: ExtendedS3DestinationDescription)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
extendedS3DestinationDescription_roleARN :: Lens.Lens' ExtendedS3DestinationDescription Prelude.Text
extendedS3DestinationDescription_roleARN = Lens.lens (\ExtendedS3DestinationDescription' {roleARN} -> roleARN) (\s@ExtendedS3DestinationDescription' {} a -> s {roleARN = a} :: ExtendedS3DestinationDescription)

-- | The ARN of the S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
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
  Data.FromJSON
    ExtendedS3DestinationDescription
  where
  parseJSON =
    Data.withObject
      "ExtendedS3DestinationDescription"
      ( \x ->
          ExtendedS3DestinationDescription'
            Prelude.<$> (x Data..:? "CloudWatchLoggingOptions")
            Prelude.<*> (x Data..:? "DataFormatConversionConfiguration")
            Prelude.<*> (x Data..:? "DynamicPartitioningConfiguration")
            Prelude.<*> (x Data..:? "ErrorOutputPrefix")
            Prelude.<*> (x Data..:? "Prefix")
            Prelude.<*> (x Data..:? "ProcessingConfiguration")
            Prelude.<*> (x Data..:? "S3BackupDescription")
            Prelude.<*> (x Data..:? "S3BackupMode")
            Prelude.<*> (x Data..: "RoleARN")
            Prelude.<*> (x Data..: "BucketARN")
            Prelude.<*> (x Data..: "BufferingHints")
            Prelude.<*> (x Data..: "CompressionFormat")
            Prelude.<*> (x Data..: "EncryptionConfiguration")
      )

instance
  Prelude.Hashable
    ExtendedS3DestinationDescription
  where
  hashWithSalt
    _salt
    ExtendedS3DestinationDescription' {..} =
      _salt
        `Prelude.hashWithSalt` cloudWatchLoggingOptions
        `Prelude.hashWithSalt` dataFormatConversionConfiguration
        `Prelude.hashWithSalt` dynamicPartitioningConfiguration
        `Prelude.hashWithSalt` errorOutputPrefix
        `Prelude.hashWithSalt` prefix
        `Prelude.hashWithSalt` processingConfiguration
        `Prelude.hashWithSalt` s3BackupDescription
        `Prelude.hashWithSalt` s3BackupMode
        `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` bucketARN
        `Prelude.hashWithSalt` bufferingHints
        `Prelude.hashWithSalt` compressionFormat
        `Prelude.hashWithSalt` encryptionConfiguration

instance
  Prelude.NFData
    ExtendedS3DestinationDescription
  where
  rnf ExtendedS3DestinationDescription' {..} =
    Prelude.rnf cloudWatchLoggingOptions
      `Prelude.seq` Prelude.rnf dataFormatConversionConfiguration
      `Prelude.seq` Prelude.rnf dynamicPartitioningConfiguration
      `Prelude.seq` Prelude.rnf errorOutputPrefix
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf processingConfiguration
      `Prelude.seq` Prelude.rnf s3BackupDescription
      `Prelude.seq` Prelude.rnf s3BackupMode
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf bucketARN
      `Prelude.seq` Prelude.rnf bufferingHints
      `Prelude.seq` Prelude.rnf compressionFormat
      `Prelude.seq` Prelude.rnf encryptionConfiguration
