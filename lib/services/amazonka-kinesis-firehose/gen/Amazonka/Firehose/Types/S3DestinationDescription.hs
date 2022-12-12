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
-- Module      : Amazonka.Firehose.Types.S3DestinationDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.S3DestinationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.BufferingHints
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.CompressionFormat
import Amazonka.Firehose.Types.EncryptionConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes a destination in Amazon S3.
--
-- /See:/ 'newS3DestinationDescription' smart constructor.
data S3DestinationDescription = S3DestinationDescription'
  { -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
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
    bucketARN :: Prelude.Text,
    -- | The buffering option. If no value is specified, @BufferingHints@ object
    -- default values are used.
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
-- Create a value of 'S3DestinationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLoggingOptions', 's3DestinationDescription_cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- 'errorOutputPrefix', 's3DestinationDescription_errorOutputPrefix' - A prefix that Kinesis Data Firehose evaluates and adds to failed records
-- before writing them to S3. This prefix appears immediately following the
-- bucket name. For information about how to specify this prefix, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
--
-- 'prefix', 's3DestinationDescription_prefix' - The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered Amazon S3 files. You can also specify a custom prefix, as
-- described in
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
--
-- 'roleARN', 's3DestinationDescription_roleARN' - The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
--
-- 'bucketARN', 's3DestinationDescription_bucketARN' - The ARN of the S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
--
-- 'bufferingHints', 's3DestinationDescription_bufferingHints' - The buffering option. If no value is specified, @BufferingHints@ object
-- default values are used.
--
-- 'compressionFormat', 's3DestinationDescription_compressionFormat' - The compression format. If no value is specified, the default is
-- @UNCOMPRESSED@.
--
-- 'encryptionConfiguration', 's3DestinationDescription_encryptionConfiguration' - The encryption configuration. If no value is specified, the default is
-- no encryption.
newS3DestinationDescription ::
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
  S3DestinationDescription
newS3DestinationDescription
  pRoleARN_
  pBucketARN_
  pBufferingHints_
  pCompressionFormat_
  pEncryptionConfiguration_ =
    S3DestinationDescription'
      { cloudWatchLoggingOptions =
          Prelude.Nothing,
        errorOutputPrefix = Prelude.Nothing,
        prefix = Prelude.Nothing,
        roleARN = pRoleARN_,
        bucketARN = pBucketARN_,
        bufferingHints = pBufferingHints_,
        compressionFormat = pCompressionFormat_,
        encryptionConfiguration =
          pEncryptionConfiguration_
      }

-- | The Amazon CloudWatch logging options for your delivery stream.
s3DestinationDescription_cloudWatchLoggingOptions :: Lens.Lens' S3DestinationDescription (Prelude.Maybe CloudWatchLoggingOptions)
s3DestinationDescription_cloudWatchLoggingOptions = Lens.lens (\S3DestinationDescription' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@S3DestinationDescription' {} a -> s {cloudWatchLoggingOptions = a} :: S3DestinationDescription)

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records
-- before writing them to S3. This prefix appears immediately following the
-- bucket name. For information about how to specify this prefix, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
s3DestinationDescription_errorOutputPrefix :: Lens.Lens' S3DestinationDescription (Prelude.Maybe Prelude.Text)
s3DestinationDescription_errorOutputPrefix = Lens.lens (\S3DestinationDescription' {errorOutputPrefix} -> errorOutputPrefix) (\s@S3DestinationDescription' {} a -> s {errorOutputPrefix = a} :: S3DestinationDescription)

-- | The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered Amazon S3 files. You can also specify a custom prefix, as
-- described in
-- <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects>.
s3DestinationDescription_prefix :: Lens.Lens' S3DestinationDescription (Prelude.Maybe Prelude.Text)
s3DestinationDescription_prefix = Lens.lens (\S3DestinationDescription' {prefix} -> prefix) (\s@S3DestinationDescription' {} a -> s {prefix = a} :: S3DestinationDescription)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
s3DestinationDescription_roleARN :: Lens.Lens' S3DestinationDescription Prelude.Text
s3DestinationDescription_roleARN = Lens.lens (\S3DestinationDescription' {roleARN} -> roleARN) (\s@S3DestinationDescription' {} a -> s {roleARN = a} :: S3DestinationDescription)

-- | The ARN of the S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
s3DestinationDescription_bucketARN :: Lens.Lens' S3DestinationDescription Prelude.Text
s3DestinationDescription_bucketARN = Lens.lens (\S3DestinationDescription' {bucketARN} -> bucketARN) (\s@S3DestinationDescription' {} a -> s {bucketARN = a} :: S3DestinationDescription)

-- | The buffering option. If no value is specified, @BufferingHints@ object
-- default values are used.
s3DestinationDescription_bufferingHints :: Lens.Lens' S3DestinationDescription BufferingHints
s3DestinationDescription_bufferingHints = Lens.lens (\S3DestinationDescription' {bufferingHints} -> bufferingHints) (\s@S3DestinationDescription' {} a -> s {bufferingHints = a} :: S3DestinationDescription)

-- | The compression format. If no value is specified, the default is
-- @UNCOMPRESSED@.
s3DestinationDescription_compressionFormat :: Lens.Lens' S3DestinationDescription CompressionFormat
s3DestinationDescription_compressionFormat = Lens.lens (\S3DestinationDescription' {compressionFormat} -> compressionFormat) (\s@S3DestinationDescription' {} a -> s {compressionFormat = a} :: S3DestinationDescription)

-- | The encryption configuration. If no value is specified, the default is
-- no encryption.
s3DestinationDescription_encryptionConfiguration :: Lens.Lens' S3DestinationDescription EncryptionConfiguration
s3DestinationDescription_encryptionConfiguration = Lens.lens (\S3DestinationDescription' {encryptionConfiguration} -> encryptionConfiguration) (\s@S3DestinationDescription' {} a -> s {encryptionConfiguration = a} :: S3DestinationDescription)

instance Data.FromJSON S3DestinationDescription where
  parseJSON =
    Data.withObject
      "S3DestinationDescription"
      ( \x ->
          S3DestinationDescription'
            Prelude.<$> (x Data..:? "CloudWatchLoggingOptions")
            Prelude.<*> (x Data..:? "ErrorOutputPrefix")
            Prelude.<*> (x Data..:? "Prefix")
            Prelude.<*> (x Data..: "RoleARN")
            Prelude.<*> (x Data..: "BucketARN")
            Prelude.<*> (x Data..: "BufferingHints")
            Prelude.<*> (x Data..: "CompressionFormat")
            Prelude.<*> (x Data..: "EncryptionConfiguration")
      )

instance Prelude.Hashable S3DestinationDescription where
  hashWithSalt _salt S3DestinationDescription' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLoggingOptions
      `Prelude.hashWithSalt` errorOutputPrefix
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` bucketARN
      `Prelude.hashWithSalt` bufferingHints
      `Prelude.hashWithSalt` compressionFormat
      `Prelude.hashWithSalt` encryptionConfiguration

instance Prelude.NFData S3DestinationDescription where
  rnf S3DestinationDescription' {..} =
    Prelude.rnf cloudWatchLoggingOptions
      `Prelude.seq` Prelude.rnf errorOutputPrefix
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf bucketARN
      `Prelude.seq` Prelude.rnf bufferingHints
      `Prelude.seq` Prelude.rnf compressionFormat
      `Prelude.seq` Prelude.rnf encryptionConfiguration
