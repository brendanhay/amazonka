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
-- Module      : Amazonka.Firehose.Types.S3DestinationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.S3DestinationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.BufferingHints
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.CompressionFormat
import Amazonka.Firehose.Types.EncryptionConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of a destination in Amazon S3.
--
-- /See:/ 'newS3DestinationConfiguration' smart constructor.
data S3DestinationConfiguration = S3DestinationConfiguration'
  { -- | The buffering option. If no value is specified, @BufferingHints@ object
    -- default values are used.
    bufferingHints :: Prelude.Maybe BufferingHints,
    -- | The CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The compression format. If no value is specified, the default is
    -- @UNCOMPRESSED@.
    --
    -- The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon
    -- Redshift destinations because they are not supported by the Amazon
    -- Redshift @COPY@ operation that reads from the S3 bucket.
    compressionFormat :: Prelude.Maybe CompressionFormat,
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
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
    -- For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
    roleARN :: Prelude.Text,
    -- | The ARN of the S3 bucket. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
    bucketARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3DestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bufferingHints', 's3DestinationConfiguration_bufferingHints' - The buffering option. If no value is specified, @BufferingHints@ object
-- default values are used.
--
-- 'cloudWatchLoggingOptions', 's3DestinationConfiguration_cloudWatchLoggingOptions' - The CloudWatch logging options for your delivery stream.
--
-- 'compressionFormat', 's3DestinationConfiguration_compressionFormat' - The compression format. If no value is specified, the default is
-- @UNCOMPRESSED@.
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon
-- Redshift destinations because they are not supported by the Amazon
-- Redshift @COPY@ operation that reads from the S3 bucket.
--
-- 'encryptionConfiguration', 's3DestinationConfiguration_encryptionConfiguration' - The encryption configuration. If no value is specified, the default is
-- no encryption.
--
-- 'errorOutputPrefix', 's3DestinationConfiguration_errorOutputPrefix' - A prefix that Kinesis Data Firehose evaluates and adds to failed records
-- before writing them to S3. This prefix appears immediately following the
-- bucket name. For information about how to specify this prefix, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
--
-- 'prefix', 's3DestinationConfiguration_prefix' - The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered Amazon S3 files. You can also specify a custom prefix, as
-- described in
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
--
-- 'roleARN', 's3DestinationConfiguration_roleARN' - The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
--
-- 'bucketARN', 's3DestinationConfiguration_bucketARN' - The ARN of the S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
newS3DestinationConfiguration ::
  -- | 'roleARN'
  Prelude.Text ->
  -- | 'bucketARN'
  Prelude.Text ->
  S3DestinationConfiguration
newS3DestinationConfiguration pRoleARN_ pBucketARN_ =
  S3DestinationConfiguration'
    { bufferingHints =
        Prelude.Nothing,
      cloudWatchLoggingOptions = Prelude.Nothing,
      compressionFormat = Prelude.Nothing,
      encryptionConfiguration = Prelude.Nothing,
      errorOutputPrefix = Prelude.Nothing,
      prefix = Prelude.Nothing,
      roleARN = pRoleARN_,
      bucketARN = pBucketARN_
    }

-- | The buffering option. If no value is specified, @BufferingHints@ object
-- default values are used.
s3DestinationConfiguration_bufferingHints :: Lens.Lens' S3DestinationConfiguration (Prelude.Maybe BufferingHints)
s3DestinationConfiguration_bufferingHints = Lens.lens (\S3DestinationConfiguration' {bufferingHints} -> bufferingHints) (\s@S3DestinationConfiguration' {} a -> s {bufferingHints = a} :: S3DestinationConfiguration)

-- | The CloudWatch logging options for your delivery stream.
s3DestinationConfiguration_cloudWatchLoggingOptions :: Lens.Lens' S3DestinationConfiguration (Prelude.Maybe CloudWatchLoggingOptions)
s3DestinationConfiguration_cloudWatchLoggingOptions = Lens.lens (\S3DestinationConfiguration' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@S3DestinationConfiguration' {} a -> s {cloudWatchLoggingOptions = a} :: S3DestinationConfiguration)

-- | The compression format. If no value is specified, the default is
-- @UNCOMPRESSED@.
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon
-- Redshift destinations because they are not supported by the Amazon
-- Redshift @COPY@ operation that reads from the S3 bucket.
s3DestinationConfiguration_compressionFormat :: Lens.Lens' S3DestinationConfiguration (Prelude.Maybe CompressionFormat)
s3DestinationConfiguration_compressionFormat = Lens.lens (\S3DestinationConfiguration' {compressionFormat} -> compressionFormat) (\s@S3DestinationConfiguration' {} a -> s {compressionFormat = a} :: S3DestinationConfiguration)

-- | The encryption configuration. If no value is specified, the default is
-- no encryption.
s3DestinationConfiguration_encryptionConfiguration :: Lens.Lens' S3DestinationConfiguration (Prelude.Maybe EncryptionConfiguration)
s3DestinationConfiguration_encryptionConfiguration = Lens.lens (\S3DestinationConfiguration' {encryptionConfiguration} -> encryptionConfiguration) (\s@S3DestinationConfiguration' {} a -> s {encryptionConfiguration = a} :: S3DestinationConfiguration)

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records
-- before writing them to S3. This prefix appears immediately following the
-- bucket name. For information about how to specify this prefix, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
s3DestinationConfiguration_errorOutputPrefix :: Lens.Lens' S3DestinationConfiguration (Prelude.Maybe Prelude.Text)
s3DestinationConfiguration_errorOutputPrefix = Lens.lens (\S3DestinationConfiguration' {errorOutputPrefix} -> errorOutputPrefix) (\s@S3DestinationConfiguration' {} a -> s {errorOutputPrefix = a} :: S3DestinationConfiguration)

-- | The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered Amazon S3 files. You can also specify a custom prefix, as
-- described in
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
s3DestinationConfiguration_prefix :: Lens.Lens' S3DestinationConfiguration (Prelude.Maybe Prelude.Text)
s3DestinationConfiguration_prefix = Lens.lens (\S3DestinationConfiguration' {prefix} -> prefix) (\s@S3DestinationConfiguration' {} a -> s {prefix = a} :: S3DestinationConfiguration)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
s3DestinationConfiguration_roleARN :: Lens.Lens' S3DestinationConfiguration Prelude.Text
s3DestinationConfiguration_roleARN = Lens.lens (\S3DestinationConfiguration' {roleARN} -> roleARN) (\s@S3DestinationConfiguration' {} a -> s {roleARN = a} :: S3DestinationConfiguration)

-- | The ARN of the S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
s3DestinationConfiguration_bucketARN :: Lens.Lens' S3DestinationConfiguration Prelude.Text
s3DestinationConfiguration_bucketARN = Lens.lens (\S3DestinationConfiguration' {bucketARN} -> bucketARN) (\s@S3DestinationConfiguration' {} a -> s {bucketARN = a} :: S3DestinationConfiguration)

instance Prelude.Hashable S3DestinationConfiguration where
  hashWithSalt _salt S3DestinationConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` bufferingHints
      `Prelude.hashWithSalt` cloudWatchLoggingOptions
      `Prelude.hashWithSalt` compressionFormat
      `Prelude.hashWithSalt` encryptionConfiguration
      `Prelude.hashWithSalt` errorOutputPrefix
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` bucketARN

instance Prelude.NFData S3DestinationConfiguration where
  rnf S3DestinationConfiguration' {..} =
    Prelude.rnf bufferingHints
      `Prelude.seq` Prelude.rnf cloudWatchLoggingOptions
      `Prelude.seq` Prelude.rnf compressionFormat
      `Prelude.seq` Prelude.rnf encryptionConfiguration
      `Prelude.seq` Prelude.rnf errorOutputPrefix
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf bucketARN

instance Data.ToJSON S3DestinationConfiguration where
  toJSON S3DestinationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BufferingHints" Data..=)
              Prelude.<$> bufferingHints,
            ("CloudWatchLoggingOptions" Data..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("CompressionFormat" Data..=)
              Prelude.<$> compressionFormat,
            ("EncryptionConfiguration" Data..=)
              Prelude.<$> encryptionConfiguration,
            ("ErrorOutputPrefix" Data..=)
              Prelude.<$> errorOutputPrefix,
            ("Prefix" Data..=) Prelude.<$> prefix,
            Prelude.Just ("RoleARN" Data..= roleARN),
            Prelude.Just ("BucketARN" Data..= bucketARN)
          ]
      )
