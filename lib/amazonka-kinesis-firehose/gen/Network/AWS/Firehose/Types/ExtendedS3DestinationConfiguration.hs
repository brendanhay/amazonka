{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ExtendedS3DestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ExtendedS3DestinationConfiguration
  ( ExtendedS3DestinationConfiguration (..),

    -- * Smart constructor
    mkExtendedS3DestinationConfiguration,

    -- * Lenses
    esdcS3BackupMode,
    esdcPrefix,
    esdcCloudWatchLoggingOptions,
    esdcS3BackupConfiguration,
    esdcErrorOutputPrefix,
    esdcEncryptionConfiguration,
    esdcCompressionFormat,
    esdcBufferingHints,
    esdcDataFormatConversionConfiguration,
    esdcProcessingConfiguration,
    esdcRoleARN,
    esdcBucketARN,
  )
where

import Network.AWS.Firehose.Types.BufferingHints
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CompressionFormat
import Network.AWS.Firehose.Types.DataFormatConversionConfiguration
import Network.AWS.Firehose.Types.EncryptionConfiguration
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3BackupMode
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the configuration of a destination in Amazon S3.
--
-- /See:/ 'mkExtendedS3DestinationConfiguration' smart constructor.
data ExtendedS3DestinationConfiguration = ExtendedS3DestinationConfiguration'
  { s3BackupMode ::
      Lude.Maybe
        S3BackupMode,
    prefix ::
      Lude.Maybe Lude.Text,
    cloudWatchLoggingOptions ::
      Lude.Maybe
        CloudWatchLoggingOptions,
    s3BackupConfiguration ::
      Lude.Maybe
        S3DestinationConfiguration,
    errorOutputPrefix ::
      Lude.Maybe Lude.Text,
    encryptionConfiguration ::
      Lude.Maybe
        EncryptionConfiguration,
    compressionFormat ::
      Lude.Maybe
        CompressionFormat,
    bufferingHints ::
      Lude.Maybe
        BufferingHints,
    dataFormatConversionConfiguration ::
      Lude.Maybe
        DataFormatConversionConfiguration,
    processingConfiguration ::
      Lude.Maybe
        ProcessingConfiguration,
    roleARN :: Lude.Text,
    bucketARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExtendedS3DestinationConfiguration' with the minimum fields required to make a request.
--
-- * 'bucketARN' - The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 'bufferingHints' - The buffering option.
-- * 'cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
-- * 'compressionFormat' - The compression format. If no value is specified, the default is UNCOMPRESSED.
-- * 'dataFormatConversionConfiguration' - The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
-- * 'encryptionConfiguration' - The encryption configuration. If no value is specified, the default is no encryption.
-- * 'errorOutputPrefix' - A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
-- * 'prefix' - The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
-- * 'processingConfiguration' - The data processing configuration.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 's3BackupConfiguration' - The configuration for backup in Amazon S3.
-- * 's3BackupMode' - The Amazon S3 backup mode. After you create a delivery stream, you can update it to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it.
mkExtendedS3DestinationConfiguration ::
  -- | 'roleARN'
  Lude.Text ->
  -- | 'bucketARN'
  Lude.Text ->
  ExtendedS3DestinationConfiguration
mkExtendedS3DestinationConfiguration pRoleARN_ pBucketARN_ =
  ExtendedS3DestinationConfiguration'
    { s3BackupMode = Lude.Nothing,
      prefix = Lude.Nothing,
      cloudWatchLoggingOptions = Lude.Nothing,
      s3BackupConfiguration = Lude.Nothing,
      errorOutputPrefix = Lude.Nothing,
      encryptionConfiguration = Lude.Nothing,
      compressionFormat = Lude.Nothing,
      bufferingHints = Lude.Nothing,
      dataFormatConversionConfiguration = Lude.Nothing,
      processingConfiguration = Lude.Nothing,
      roleARN = pRoleARN_,
      bucketARN = pBucketARN_
    }

-- | The Amazon S3 backup mode. After you create a delivery stream, you can update it to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it.
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcS3BackupMode :: Lens.Lens' ExtendedS3DestinationConfiguration (Lude.Maybe S3BackupMode)
esdcS3BackupMode = Lens.lens (s3BackupMode :: ExtendedS3DestinationConfiguration -> Lude.Maybe S3BackupMode) (\s a -> s {s3BackupMode = a} :: ExtendedS3DestinationConfiguration)
{-# DEPRECATED esdcS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcPrefix :: Lens.Lens' ExtendedS3DestinationConfiguration (Lude.Maybe Lude.Text)
esdcPrefix = Lens.lens (prefix :: ExtendedS3DestinationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: ExtendedS3DestinationConfiguration)
{-# DEPRECATED esdcPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcCloudWatchLoggingOptions :: Lens.Lens' ExtendedS3DestinationConfiguration (Lude.Maybe CloudWatchLoggingOptions)
esdcCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: ExtendedS3DestinationConfiguration -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: ExtendedS3DestinationConfiguration)
{-# DEPRECATED esdcCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The configuration for backup in Amazon S3.
--
-- /Note:/ Consider using 's3BackupConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcS3BackupConfiguration :: Lens.Lens' ExtendedS3DestinationConfiguration (Lude.Maybe S3DestinationConfiguration)
esdcS3BackupConfiguration = Lens.lens (s3BackupConfiguration :: ExtendedS3DestinationConfiguration -> Lude.Maybe S3DestinationConfiguration) (\s a -> s {s3BackupConfiguration = a} :: ExtendedS3DestinationConfiguration)
{-# DEPRECATED esdcS3BackupConfiguration "Use generic-lens or generic-optics with 's3BackupConfiguration' instead." #-}

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'errorOutputPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcErrorOutputPrefix :: Lens.Lens' ExtendedS3DestinationConfiguration (Lude.Maybe Lude.Text)
esdcErrorOutputPrefix = Lens.lens (errorOutputPrefix :: ExtendedS3DestinationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {errorOutputPrefix = a} :: ExtendedS3DestinationConfiguration)
{-# DEPRECATED esdcErrorOutputPrefix "Use generic-lens or generic-optics with 'errorOutputPrefix' instead." #-}

-- | The encryption configuration. If no value is specified, the default is no encryption.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcEncryptionConfiguration :: Lens.Lens' ExtendedS3DestinationConfiguration (Lude.Maybe EncryptionConfiguration)
esdcEncryptionConfiguration = Lens.lens (encryptionConfiguration :: ExtendedS3DestinationConfiguration -> Lude.Maybe EncryptionConfiguration) (\s a -> s {encryptionConfiguration = a} :: ExtendedS3DestinationConfiguration)
{-# DEPRECATED esdcEncryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead." #-}

-- | The compression format. If no value is specified, the default is UNCOMPRESSED.
--
-- /Note:/ Consider using 'compressionFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcCompressionFormat :: Lens.Lens' ExtendedS3DestinationConfiguration (Lude.Maybe CompressionFormat)
esdcCompressionFormat = Lens.lens (compressionFormat :: ExtendedS3DestinationConfiguration -> Lude.Maybe CompressionFormat) (\s a -> s {compressionFormat = a} :: ExtendedS3DestinationConfiguration)
{-# DEPRECATED esdcCompressionFormat "Use generic-lens or generic-optics with 'compressionFormat' instead." #-}

-- | The buffering option.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcBufferingHints :: Lens.Lens' ExtendedS3DestinationConfiguration (Lude.Maybe BufferingHints)
esdcBufferingHints = Lens.lens (bufferingHints :: ExtendedS3DestinationConfiguration -> Lude.Maybe BufferingHints) (\s a -> s {bufferingHints = a} :: ExtendedS3DestinationConfiguration)
{-# DEPRECATED esdcBufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead." #-}

-- | The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
--
-- /Note:/ Consider using 'dataFormatConversionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcDataFormatConversionConfiguration :: Lens.Lens' ExtendedS3DestinationConfiguration (Lude.Maybe DataFormatConversionConfiguration)
esdcDataFormatConversionConfiguration = Lens.lens (dataFormatConversionConfiguration :: ExtendedS3DestinationConfiguration -> Lude.Maybe DataFormatConversionConfiguration) (\s a -> s {dataFormatConversionConfiguration = a} :: ExtendedS3DestinationConfiguration)
{-# DEPRECATED esdcDataFormatConversionConfiguration "Use generic-lens or generic-optics with 'dataFormatConversionConfiguration' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcProcessingConfiguration :: Lens.Lens' ExtendedS3DestinationConfiguration (Lude.Maybe ProcessingConfiguration)
esdcProcessingConfiguration = Lens.lens (processingConfiguration :: ExtendedS3DestinationConfiguration -> Lude.Maybe ProcessingConfiguration) (\s a -> s {processingConfiguration = a} :: ExtendedS3DestinationConfiguration)
{-# DEPRECATED esdcProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcRoleARN :: Lens.Lens' ExtendedS3DestinationConfiguration Lude.Text
esdcRoleARN = Lens.lens (roleARN :: ExtendedS3DestinationConfiguration -> Lude.Text) (\s a -> s {roleARN = a} :: ExtendedS3DestinationConfiguration)
{-# DEPRECATED esdcRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcBucketARN :: Lens.Lens' ExtendedS3DestinationConfiguration Lude.Text
esdcBucketARN = Lens.lens (bucketARN :: ExtendedS3DestinationConfiguration -> Lude.Text) (\s a -> s {bucketARN = a} :: ExtendedS3DestinationConfiguration)
{-# DEPRECATED esdcBucketARN "Use generic-lens or generic-optics with 'bucketARN' instead." #-}

instance Lude.ToJSON ExtendedS3DestinationConfiguration where
  toJSON ExtendedS3DestinationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3BackupMode" Lude..=) Lude.<$> s3BackupMode,
            ("Prefix" Lude..=) Lude.<$> prefix,
            ("CloudWatchLoggingOptions" Lude..=)
              Lude.<$> cloudWatchLoggingOptions,
            ("S3BackupConfiguration" Lude..=) Lude.<$> s3BackupConfiguration,
            ("ErrorOutputPrefix" Lude..=) Lude.<$> errorOutputPrefix,
            ("EncryptionConfiguration" Lude..=)
              Lude.<$> encryptionConfiguration,
            ("CompressionFormat" Lude..=) Lude.<$> compressionFormat,
            ("BufferingHints" Lude..=) Lude.<$> bufferingHints,
            ("DataFormatConversionConfiguration" Lude..=)
              Lude.<$> dataFormatConversionConfiguration,
            ("ProcessingConfiguration" Lude..=)
              Lude.<$> processingConfiguration,
            Lude.Just ("RoleARN" Lude..= roleARN),
            Lude.Just ("BucketARN" Lude..= bucketARN)
          ]
      )
