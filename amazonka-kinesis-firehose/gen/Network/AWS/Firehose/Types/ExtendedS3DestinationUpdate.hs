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
-- Module      : Network.AWS.Firehose.Types.ExtendedS3DestinationUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ExtendedS3DestinationUpdate where

import Network.AWS.Firehose.Types.BufferingHints
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CompressionFormat
import Network.AWS.Firehose.Types.DataFormatConversionConfiguration
import Network.AWS.Firehose.Types.EncryptionConfiguration
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3BackupMode
import Network.AWS.Firehose.Types.S3DestinationUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an update for a destination in Amazon S3.
--
-- /See:/ 'newExtendedS3DestinationUpdate' smart constructor.
data ExtendedS3DestinationUpdate = ExtendedS3DestinationUpdate'
  { -- | A prefix that Kinesis Data Firehose evaluates and adds to failed records
    -- before writing them to S3. This prefix appears immediately following the
    -- bucket name. For information about how to specify this prefix, see
    -- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
    errorOutputPrefix :: Prelude.Maybe Prelude.Text,
    -- | The encryption configuration. If no value is specified, the default is
    -- no encryption.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The Amazon Resource Name (ARN) of the AWS credentials. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the S3 bucket. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    bucketARN :: Prelude.Maybe Prelude.Text,
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
    -- | The Amazon S3 destination for backup.
    s3BackupUpdate :: Prelude.Maybe S3DestinationUpdate,
    -- | The buffering option.
    bufferingHints :: Prelude.Maybe BufferingHints,
    -- | You can update a delivery stream to enable Amazon S3 backup if it is
    -- disabled. If backup is enabled, you can\'t update the delivery stream to
    -- disable it.
    s3BackupMode :: Prelude.Maybe S3BackupMode,
    -- | The compression format. If no value is specified, the default is
    -- @UNCOMPRESSED@.
    compressionFormat :: Prelude.Maybe CompressionFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExtendedS3DestinationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorOutputPrefix', 'extendedS3DestinationUpdate_errorOutputPrefix' - A prefix that Kinesis Data Firehose evaluates and adds to failed records
-- before writing them to S3. This prefix appears immediately following the
-- bucket name. For information about how to specify this prefix, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
--
-- 'encryptionConfiguration', 'extendedS3DestinationUpdate_encryptionConfiguration' - The encryption configuration. If no value is specified, the default is
-- no encryption.
--
-- 'roleARN', 'extendedS3DestinationUpdate_roleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- 'bucketARN', 'extendedS3DestinationUpdate_bucketARN' - The ARN of the S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- 'processingConfiguration', 'extendedS3DestinationUpdate_processingConfiguration' - The data processing configuration.
--
-- 'dataFormatConversionConfiguration', 'extendedS3DestinationUpdate_dataFormatConversionConfiguration' - The serializer, deserializer, and schema for converting data from the
-- JSON format to the Parquet or ORC format before writing it to Amazon S3.
--
-- 'cloudWatchLoggingOptions', 'extendedS3DestinationUpdate_cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- 'prefix', 'extendedS3DestinationUpdate_prefix' - The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered Amazon S3 files. You can also specify a custom prefix, as
-- described in
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
--
-- 's3BackupUpdate', 'extendedS3DestinationUpdate_s3BackupUpdate' - The Amazon S3 destination for backup.
--
-- 'bufferingHints', 'extendedS3DestinationUpdate_bufferingHints' - The buffering option.
--
-- 's3BackupMode', 'extendedS3DestinationUpdate_s3BackupMode' - You can update a delivery stream to enable Amazon S3 backup if it is
-- disabled. If backup is enabled, you can\'t update the delivery stream to
-- disable it.
--
-- 'compressionFormat', 'extendedS3DestinationUpdate_compressionFormat' - The compression format. If no value is specified, the default is
-- @UNCOMPRESSED@.
newExtendedS3DestinationUpdate ::
  ExtendedS3DestinationUpdate
newExtendedS3DestinationUpdate =
  ExtendedS3DestinationUpdate'
    { errorOutputPrefix =
        Prelude.Nothing,
      encryptionConfiguration = Prelude.Nothing,
      roleARN = Prelude.Nothing,
      bucketARN = Prelude.Nothing,
      processingConfiguration = Prelude.Nothing,
      dataFormatConversionConfiguration =
        Prelude.Nothing,
      cloudWatchLoggingOptions = Prelude.Nothing,
      prefix = Prelude.Nothing,
      s3BackupUpdate = Prelude.Nothing,
      bufferingHints = Prelude.Nothing,
      s3BackupMode = Prelude.Nothing,
      compressionFormat = Prelude.Nothing
    }

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records
-- before writing them to S3. This prefix appears immediately following the
-- bucket name. For information about how to specify this prefix, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
extendedS3DestinationUpdate_errorOutputPrefix :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe Prelude.Text)
extendedS3DestinationUpdate_errorOutputPrefix = Lens.lens (\ExtendedS3DestinationUpdate' {errorOutputPrefix} -> errorOutputPrefix) (\s@ExtendedS3DestinationUpdate' {} a -> s {errorOutputPrefix = a} :: ExtendedS3DestinationUpdate)

-- | The encryption configuration. If no value is specified, the default is
-- no encryption.
extendedS3DestinationUpdate_encryptionConfiguration :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe EncryptionConfiguration)
extendedS3DestinationUpdate_encryptionConfiguration = Lens.lens (\ExtendedS3DestinationUpdate' {encryptionConfiguration} -> encryptionConfiguration) (\s@ExtendedS3DestinationUpdate' {} a -> s {encryptionConfiguration = a} :: ExtendedS3DestinationUpdate)

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
extendedS3DestinationUpdate_roleARN :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe Prelude.Text)
extendedS3DestinationUpdate_roleARN = Lens.lens (\ExtendedS3DestinationUpdate' {roleARN} -> roleARN) (\s@ExtendedS3DestinationUpdate' {} a -> s {roleARN = a} :: ExtendedS3DestinationUpdate)

-- | The ARN of the S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
extendedS3DestinationUpdate_bucketARN :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe Prelude.Text)
extendedS3DestinationUpdate_bucketARN = Lens.lens (\ExtendedS3DestinationUpdate' {bucketARN} -> bucketARN) (\s@ExtendedS3DestinationUpdate' {} a -> s {bucketARN = a} :: ExtendedS3DestinationUpdate)

-- | The data processing configuration.
extendedS3DestinationUpdate_processingConfiguration :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe ProcessingConfiguration)
extendedS3DestinationUpdate_processingConfiguration = Lens.lens (\ExtendedS3DestinationUpdate' {processingConfiguration} -> processingConfiguration) (\s@ExtendedS3DestinationUpdate' {} a -> s {processingConfiguration = a} :: ExtendedS3DestinationUpdate)

-- | The serializer, deserializer, and schema for converting data from the
-- JSON format to the Parquet or ORC format before writing it to Amazon S3.
extendedS3DestinationUpdate_dataFormatConversionConfiguration :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe DataFormatConversionConfiguration)
extendedS3DestinationUpdate_dataFormatConversionConfiguration = Lens.lens (\ExtendedS3DestinationUpdate' {dataFormatConversionConfiguration} -> dataFormatConversionConfiguration) (\s@ExtendedS3DestinationUpdate' {} a -> s {dataFormatConversionConfiguration = a} :: ExtendedS3DestinationUpdate)

-- | The Amazon CloudWatch logging options for your delivery stream.
extendedS3DestinationUpdate_cloudWatchLoggingOptions :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe CloudWatchLoggingOptions)
extendedS3DestinationUpdate_cloudWatchLoggingOptions = Lens.lens (\ExtendedS3DestinationUpdate' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@ExtendedS3DestinationUpdate' {} a -> s {cloudWatchLoggingOptions = a} :: ExtendedS3DestinationUpdate)

-- | The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered Amazon S3 files. You can also specify a custom prefix, as
-- described in
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
extendedS3DestinationUpdate_prefix :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe Prelude.Text)
extendedS3DestinationUpdate_prefix = Lens.lens (\ExtendedS3DestinationUpdate' {prefix} -> prefix) (\s@ExtendedS3DestinationUpdate' {} a -> s {prefix = a} :: ExtendedS3DestinationUpdate)

-- | The Amazon S3 destination for backup.
extendedS3DestinationUpdate_s3BackupUpdate :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe S3DestinationUpdate)
extendedS3DestinationUpdate_s3BackupUpdate = Lens.lens (\ExtendedS3DestinationUpdate' {s3BackupUpdate} -> s3BackupUpdate) (\s@ExtendedS3DestinationUpdate' {} a -> s {s3BackupUpdate = a} :: ExtendedS3DestinationUpdate)

-- | The buffering option.
extendedS3DestinationUpdate_bufferingHints :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe BufferingHints)
extendedS3DestinationUpdate_bufferingHints = Lens.lens (\ExtendedS3DestinationUpdate' {bufferingHints} -> bufferingHints) (\s@ExtendedS3DestinationUpdate' {} a -> s {bufferingHints = a} :: ExtendedS3DestinationUpdate)

-- | You can update a delivery stream to enable Amazon S3 backup if it is
-- disabled. If backup is enabled, you can\'t update the delivery stream to
-- disable it.
extendedS3DestinationUpdate_s3BackupMode :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe S3BackupMode)
extendedS3DestinationUpdate_s3BackupMode = Lens.lens (\ExtendedS3DestinationUpdate' {s3BackupMode} -> s3BackupMode) (\s@ExtendedS3DestinationUpdate' {} a -> s {s3BackupMode = a} :: ExtendedS3DestinationUpdate)

-- | The compression format. If no value is specified, the default is
-- @UNCOMPRESSED@.
extendedS3DestinationUpdate_compressionFormat :: Lens.Lens' ExtendedS3DestinationUpdate (Prelude.Maybe CompressionFormat)
extendedS3DestinationUpdate_compressionFormat = Lens.lens (\ExtendedS3DestinationUpdate' {compressionFormat} -> compressionFormat) (\s@ExtendedS3DestinationUpdate' {} a -> s {compressionFormat = a} :: ExtendedS3DestinationUpdate)

instance Prelude.Hashable ExtendedS3DestinationUpdate

instance Prelude.NFData ExtendedS3DestinationUpdate

instance Prelude.ToJSON ExtendedS3DestinationUpdate where
  toJSON ExtendedS3DestinationUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ErrorOutputPrefix" Prelude..=)
              Prelude.<$> errorOutputPrefix,
            ("EncryptionConfiguration" Prelude..=)
              Prelude.<$> encryptionConfiguration,
            ("RoleARN" Prelude..=) Prelude.<$> roleARN,
            ("BucketARN" Prelude..=) Prelude.<$> bucketARN,
            ("ProcessingConfiguration" Prelude..=)
              Prelude.<$> processingConfiguration,
            ("DataFormatConversionConfiguration" Prelude..=)
              Prelude.<$> dataFormatConversionConfiguration,
            ("CloudWatchLoggingOptions" Prelude..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("Prefix" Prelude..=) Prelude.<$> prefix,
            ("S3BackupUpdate" Prelude..=)
              Prelude.<$> s3BackupUpdate,
            ("BufferingHints" Prelude..=)
              Prelude.<$> bufferingHints,
            ("S3BackupMode" Prelude..=) Prelude.<$> s3BackupMode,
            ("CompressionFormat" Prelude..=)
              Prelude.<$> compressionFormat
          ]
      )
