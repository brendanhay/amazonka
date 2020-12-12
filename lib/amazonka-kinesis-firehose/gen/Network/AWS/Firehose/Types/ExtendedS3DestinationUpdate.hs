{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ExtendedS3DestinationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ExtendedS3DestinationUpdate
  ( ExtendedS3DestinationUpdate (..),

    -- * Smart constructor
    mkExtendedS3DestinationUpdate,

    -- * Lenses
    esduS3BackupMode,
    esduPrefix,
    esduCloudWatchLoggingOptions,
    esduErrorOutputPrefix,
    esduS3BackupUpdate,
    esduEncryptionConfiguration,
    esduCompressionFormat,
    esduBufferingHints,
    esduDataFormatConversionConfiguration,
    esduBucketARN,
    esduProcessingConfiguration,
    esduRoleARN,
  )
where

import Network.AWS.Firehose.Types.BufferingHints
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CompressionFormat
import Network.AWS.Firehose.Types.DataFormatConversionConfiguration
import Network.AWS.Firehose.Types.EncryptionConfiguration
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3BackupMode
import Network.AWS.Firehose.Types.S3DestinationUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an update for a destination in Amazon S3.
--
-- /See:/ 'mkExtendedS3DestinationUpdate' smart constructor.
data ExtendedS3DestinationUpdate = ExtendedS3DestinationUpdate'
  { s3BackupMode ::
      Lude.Maybe S3BackupMode,
    prefix :: Lude.Maybe Lude.Text,
    cloudWatchLoggingOptions ::
      Lude.Maybe CloudWatchLoggingOptions,
    errorOutputPrefix ::
      Lude.Maybe Lude.Text,
    s3BackupUpdate ::
      Lude.Maybe S3DestinationUpdate,
    encryptionConfiguration ::
      Lude.Maybe EncryptionConfiguration,
    compressionFormat ::
      Lude.Maybe CompressionFormat,
    bufferingHints ::
      Lude.Maybe BufferingHints,
    dataFormatConversionConfiguration ::
      Lude.Maybe
        DataFormatConversionConfiguration,
    bucketARN :: Lude.Maybe Lude.Text,
    processingConfiguration ::
      Lude.Maybe ProcessingConfiguration,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExtendedS3DestinationUpdate' with the minimum fields required to make a request.
--
-- * 'bucketARN' - The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 'bufferingHints' - The buffering option.
-- * 'cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
-- * 'compressionFormat' - The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
-- * 'dataFormatConversionConfiguration' - The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
-- * 'encryptionConfiguration' - The encryption configuration. If no value is specified, the default is no encryption.
-- * 'errorOutputPrefix' - A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
-- * 'prefix' - The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
-- * 'processingConfiguration' - The data processing configuration.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 's3BackupMode' - You can update a delivery stream to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it.
-- * 's3BackupUpdate' - The Amazon S3 destination for backup.
mkExtendedS3DestinationUpdate ::
  ExtendedS3DestinationUpdate
mkExtendedS3DestinationUpdate =
  ExtendedS3DestinationUpdate'
    { s3BackupMode = Lude.Nothing,
      prefix = Lude.Nothing,
      cloudWatchLoggingOptions = Lude.Nothing,
      errorOutputPrefix = Lude.Nothing,
      s3BackupUpdate = Lude.Nothing,
      encryptionConfiguration = Lude.Nothing,
      compressionFormat = Lude.Nothing,
      bufferingHints = Lude.Nothing,
      dataFormatConversionConfiguration = Lude.Nothing,
      bucketARN = Lude.Nothing,
      processingConfiguration = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | You can update a delivery stream to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it.
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduS3BackupMode :: Lens.Lens' ExtendedS3DestinationUpdate (Lude.Maybe S3BackupMode)
esduS3BackupMode = Lens.lens (s3BackupMode :: ExtendedS3DestinationUpdate -> Lude.Maybe S3BackupMode) (\s a -> s {s3BackupMode = a} :: ExtendedS3DestinationUpdate)
{-# DEPRECATED esduS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduPrefix :: Lens.Lens' ExtendedS3DestinationUpdate (Lude.Maybe Lude.Text)
esduPrefix = Lens.lens (prefix :: ExtendedS3DestinationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: ExtendedS3DestinationUpdate)
{-# DEPRECATED esduPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduCloudWatchLoggingOptions :: Lens.Lens' ExtendedS3DestinationUpdate (Lude.Maybe CloudWatchLoggingOptions)
esduCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: ExtendedS3DestinationUpdate -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: ExtendedS3DestinationUpdate)
{-# DEPRECATED esduCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'errorOutputPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduErrorOutputPrefix :: Lens.Lens' ExtendedS3DestinationUpdate (Lude.Maybe Lude.Text)
esduErrorOutputPrefix = Lens.lens (errorOutputPrefix :: ExtendedS3DestinationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {errorOutputPrefix = a} :: ExtendedS3DestinationUpdate)
{-# DEPRECATED esduErrorOutputPrefix "Use generic-lens or generic-optics with 'errorOutputPrefix' instead." #-}

-- | The Amazon S3 destination for backup.
--
-- /Note:/ Consider using 's3BackupUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduS3BackupUpdate :: Lens.Lens' ExtendedS3DestinationUpdate (Lude.Maybe S3DestinationUpdate)
esduS3BackupUpdate = Lens.lens (s3BackupUpdate :: ExtendedS3DestinationUpdate -> Lude.Maybe S3DestinationUpdate) (\s a -> s {s3BackupUpdate = a} :: ExtendedS3DestinationUpdate)
{-# DEPRECATED esduS3BackupUpdate "Use generic-lens or generic-optics with 's3BackupUpdate' instead." #-}

-- | The encryption configuration. If no value is specified, the default is no encryption.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduEncryptionConfiguration :: Lens.Lens' ExtendedS3DestinationUpdate (Lude.Maybe EncryptionConfiguration)
esduEncryptionConfiguration = Lens.lens (encryptionConfiguration :: ExtendedS3DestinationUpdate -> Lude.Maybe EncryptionConfiguration) (\s a -> s {encryptionConfiguration = a} :: ExtendedS3DestinationUpdate)
{-# DEPRECATED esduEncryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead." #-}

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
--
-- /Note:/ Consider using 'compressionFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduCompressionFormat :: Lens.Lens' ExtendedS3DestinationUpdate (Lude.Maybe CompressionFormat)
esduCompressionFormat = Lens.lens (compressionFormat :: ExtendedS3DestinationUpdate -> Lude.Maybe CompressionFormat) (\s a -> s {compressionFormat = a} :: ExtendedS3DestinationUpdate)
{-# DEPRECATED esduCompressionFormat "Use generic-lens or generic-optics with 'compressionFormat' instead." #-}

-- | The buffering option.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduBufferingHints :: Lens.Lens' ExtendedS3DestinationUpdate (Lude.Maybe BufferingHints)
esduBufferingHints = Lens.lens (bufferingHints :: ExtendedS3DestinationUpdate -> Lude.Maybe BufferingHints) (\s a -> s {bufferingHints = a} :: ExtendedS3DestinationUpdate)
{-# DEPRECATED esduBufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead." #-}

-- | The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
--
-- /Note:/ Consider using 'dataFormatConversionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduDataFormatConversionConfiguration :: Lens.Lens' ExtendedS3DestinationUpdate (Lude.Maybe DataFormatConversionConfiguration)
esduDataFormatConversionConfiguration = Lens.lens (dataFormatConversionConfiguration :: ExtendedS3DestinationUpdate -> Lude.Maybe DataFormatConversionConfiguration) (\s a -> s {dataFormatConversionConfiguration = a} :: ExtendedS3DestinationUpdate)
{-# DEPRECATED esduDataFormatConversionConfiguration "Use generic-lens or generic-optics with 'dataFormatConversionConfiguration' instead." #-}

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduBucketARN :: Lens.Lens' ExtendedS3DestinationUpdate (Lude.Maybe Lude.Text)
esduBucketARN = Lens.lens (bucketARN :: ExtendedS3DestinationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {bucketARN = a} :: ExtendedS3DestinationUpdate)
{-# DEPRECATED esduBucketARN "Use generic-lens or generic-optics with 'bucketARN' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduProcessingConfiguration :: Lens.Lens' ExtendedS3DestinationUpdate (Lude.Maybe ProcessingConfiguration)
esduProcessingConfiguration = Lens.lens (processingConfiguration :: ExtendedS3DestinationUpdate -> Lude.Maybe ProcessingConfiguration) (\s a -> s {processingConfiguration = a} :: ExtendedS3DestinationUpdate)
{-# DEPRECATED esduProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduRoleARN :: Lens.Lens' ExtendedS3DestinationUpdate (Lude.Maybe Lude.Text)
esduRoleARN = Lens.lens (roleARN :: ExtendedS3DestinationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: ExtendedS3DestinationUpdate)
{-# DEPRECATED esduRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.ToJSON ExtendedS3DestinationUpdate where
  toJSON ExtendedS3DestinationUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3BackupMode" Lude..=) Lude.<$> s3BackupMode,
            ("Prefix" Lude..=) Lude.<$> prefix,
            ("CloudWatchLoggingOptions" Lude..=)
              Lude.<$> cloudWatchLoggingOptions,
            ("ErrorOutputPrefix" Lude..=) Lude.<$> errorOutputPrefix,
            ("S3BackupUpdate" Lude..=) Lude.<$> s3BackupUpdate,
            ("EncryptionConfiguration" Lude..=)
              Lude.<$> encryptionConfiguration,
            ("CompressionFormat" Lude..=) Lude.<$> compressionFormat,
            ("BufferingHints" Lude..=) Lude.<$> bufferingHints,
            ("DataFormatConversionConfiguration" Lude..=)
              Lude.<$> dataFormatConversionConfiguration,
            ("BucketARN" Lude..=) Lude.<$> bucketARN,
            ("ProcessingConfiguration" Lude..=)
              Lude.<$> processingConfiguration,
            ("RoleARN" Lude..=) Lude.<$> roleARN
          ]
      )
