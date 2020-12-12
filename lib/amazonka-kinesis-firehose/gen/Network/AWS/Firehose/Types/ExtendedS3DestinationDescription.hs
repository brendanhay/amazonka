{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ExtendedS3DestinationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ExtendedS3DestinationDescription
  ( ExtendedS3DestinationDescription (..),

    -- * Smart constructor
    mkExtendedS3DestinationDescription,

    -- * Lenses
    esddS3BackupMode,
    esddS3BackupDescription,
    esddPrefix,
    esddCloudWatchLoggingOptions,
    esddErrorOutputPrefix,
    esddDataFormatConversionConfiguration,
    esddProcessingConfiguration,
    esddRoleARN,
    esddBucketARN,
    esddBufferingHints,
    esddCompressionFormat,
    esddEncryptionConfiguration,
  )
where

import Network.AWS.Firehose.Types.BufferingHints
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CompressionFormat
import Network.AWS.Firehose.Types.DataFormatConversionConfiguration
import Network.AWS.Firehose.Types.EncryptionConfiguration
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3BackupMode
import Network.AWS.Firehose.Types.S3DestinationDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a destination in Amazon S3.
--
-- /See:/ 'mkExtendedS3DestinationDescription' smart constructor.
data ExtendedS3DestinationDescription = ExtendedS3DestinationDescription'
  { s3BackupMode ::
      Lude.Maybe S3BackupMode,
    s3BackupDescription ::
      Lude.Maybe
        S3DestinationDescription,
    prefix ::
      Lude.Maybe Lude.Text,
    cloudWatchLoggingOptions ::
      Lude.Maybe
        CloudWatchLoggingOptions,
    errorOutputPrefix ::
      Lude.Maybe Lude.Text,
    dataFormatConversionConfiguration ::
      Lude.Maybe
        DataFormatConversionConfiguration,
    processingConfiguration ::
      Lude.Maybe
        ProcessingConfiguration,
    roleARN :: Lude.Text,
    bucketARN :: Lude.Text,
    bufferingHints ::
      BufferingHints,
    compressionFormat ::
      CompressionFormat,
    encryptionConfiguration ::
      EncryptionConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExtendedS3DestinationDescription' with the minimum fields required to make a request.
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
-- * 's3BackupDescription' - The configuration for backup in Amazon S3.
-- * 's3BackupMode' - The Amazon S3 backup mode.
mkExtendedS3DestinationDescription ::
  -- | 'roleARN'
  Lude.Text ->
  -- | 'bucketARN'
  Lude.Text ->
  -- | 'bufferingHints'
  BufferingHints ->
  -- | 'compressionFormat'
  CompressionFormat ->
  -- | 'encryptionConfiguration'
  EncryptionConfiguration ->
  ExtendedS3DestinationDescription
mkExtendedS3DestinationDescription
  pRoleARN_
  pBucketARN_
  pBufferingHints_
  pCompressionFormat_
  pEncryptionConfiguration_ =
    ExtendedS3DestinationDescription'
      { s3BackupMode = Lude.Nothing,
        s3BackupDescription = Lude.Nothing,
        prefix = Lude.Nothing,
        cloudWatchLoggingOptions = Lude.Nothing,
        errorOutputPrefix = Lude.Nothing,
        dataFormatConversionConfiguration = Lude.Nothing,
        processingConfiguration = Lude.Nothing,
        roleARN = pRoleARN_,
        bucketARN = pBucketARN_,
        bufferingHints = pBufferingHints_,
        compressionFormat = pCompressionFormat_,
        encryptionConfiguration = pEncryptionConfiguration_
      }

-- | The Amazon S3 backup mode.
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddS3BackupMode :: Lens.Lens' ExtendedS3DestinationDescription (Lude.Maybe S3BackupMode)
esddS3BackupMode = Lens.lens (s3BackupMode :: ExtendedS3DestinationDescription -> Lude.Maybe S3BackupMode) (\s a -> s {s3BackupMode = a} :: ExtendedS3DestinationDescription)
{-# DEPRECATED esddS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | The configuration for backup in Amazon S3.
--
-- /Note:/ Consider using 's3BackupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddS3BackupDescription :: Lens.Lens' ExtendedS3DestinationDescription (Lude.Maybe S3DestinationDescription)
esddS3BackupDescription = Lens.lens (s3BackupDescription :: ExtendedS3DestinationDescription -> Lude.Maybe S3DestinationDescription) (\s a -> s {s3BackupDescription = a} :: ExtendedS3DestinationDescription)
{-# DEPRECATED esddS3BackupDescription "Use generic-lens or generic-optics with 's3BackupDescription' instead." #-}

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddPrefix :: Lens.Lens' ExtendedS3DestinationDescription (Lude.Maybe Lude.Text)
esddPrefix = Lens.lens (prefix :: ExtendedS3DestinationDescription -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: ExtendedS3DestinationDescription)
{-# DEPRECATED esddPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddCloudWatchLoggingOptions :: Lens.Lens' ExtendedS3DestinationDescription (Lude.Maybe CloudWatchLoggingOptions)
esddCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: ExtendedS3DestinationDescription -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: ExtendedS3DestinationDescription)
{-# DEPRECATED esddCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'errorOutputPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddErrorOutputPrefix :: Lens.Lens' ExtendedS3DestinationDescription (Lude.Maybe Lude.Text)
esddErrorOutputPrefix = Lens.lens (errorOutputPrefix :: ExtendedS3DestinationDescription -> Lude.Maybe Lude.Text) (\s a -> s {errorOutputPrefix = a} :: ExtendedS3DestinationDescription)
{-# DEPRECATED esddErrorOutputPrefix "Use generic-lens or generic-optics with 'errorOutputPrefix' instead." #-}

-- | The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
--
-- /Note:/ Consider using 'dataFormatConversionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddDataFormatConversionConfiguration :: Lens.Lens' ExtendedS3DestinationDescription (Lude.Maybe DataFormatConversionConfiguration)
esddDataFormatConversionConfiguration = Lens.lens (dataFormatConversionConfiguration :: ExtendedS3DestinationDescription -> Lude.Maybe DataFormatConversionConfiguration) (\s a -> s {dataFormatConversionConfiguration = a} :: ExtendedS3DestinationDescription)
{-# DEPRECATED esddDataFormatConversionConfiguration "Use generic-lens or generic-optics with 'dataFormatConversionConfiguration' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddProcessingConfiguration :: Lens.Lens' ExtendedS3DestinationDescription (Lude.Maybe ProcessingConfiguration)
esddProcessingConfiguration = Lens.lens (processingConfiguration :: ExtendedS3DestinationDescription -> Lude.Maybe ProcessingConfiguration) (\s a -> s {processingConfiguration = a} :: ExtendedS3DestinationDescription)
{-# DEPRECATED esddProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddRoleARN :: Lens.Lens' ExtendedS3DestinationDescription Lude.Text
esddRoleARN = Lens.lens (roleARN :: ExtendedS3DestinationDescription -> Lude.Text) (\s a -> s {roleARN = a} :: ExtendedS3DestinationDescription)
{-# DEPRECATED esddRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddBucketARN :: Lens.Lens' ExtendedS3DestinationDescription Lude.Text
esddBucketARN = Lens.lens (bucketARN :: ExtendedS3DestinationDescription -> Lude.Text) (\s a -> s {bucketARN = a} :: ExtendedS3DestinationDescription)
{-# DEPRECATED esddBucketARN "Use generic-lens or generic-optics with 'bucketARN' instead." #-}

-- | The buffering option.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddBufferingHints :: Lens.Lens' ExtendedS3DestinationDescription BufferingHints
esddBufferingHints = Lens.lens (bufferingHints :: ExtendedS3DestinationDescription -> BufferingHints) (\s a -> s {bufferingHints = a} :: ExtendedS3DestinationDescription)
{-# DEPRECATED esddBufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead." #-}

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
--
-- /Note:/ Consider using 'compressionFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddCompressionFormat :: Lens.Lens' ExtendedS3DestinationDescription CompressionFormat
esddCompressionFormat = Lens.lens (compressionFormat :: ExtendedS3DestinationDescription -> CompressionFormat) (\s a -> s {compressionFormat = a} :: ExtendedS3DestinationDescription)
{-# DEPRECATED esddCompressionFormat "Use generic-lens or generic-optics with 'compressionFormat' instead." #-}

-- | The encryption configuration. If no value is specified, the default is no encryption.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddEncryptionConfiguration :: Lens.Lens' ExtendedS3DestinationDescription EncryptionConfiguration
esddEncryptionConfiguration = Lens.lens (encryptionConfiguration :: ExtendedS3DestinationDescription -> EncryptionConfiguration) (\s a -> s {encryptionConfiguration = a} :: ExtendedS3DestinationDescription)
{-# DEPRECATED esddEncryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead." #-}

instance Lude.FromJSON ExtendedS3DestinationDescription where
  parseJSON =
    Lude.withObject
      "ExtendedS3DestinationDescription"
      ( \x ->
          ExtendedS3DestinationDescription'
            Lude.<$> (x Lude..:? "S3BackupMode")
            Lude.<*> (x Lude..:? "S3BackupDescription")
            Lude.<*> (x Lude..:? "Prefix")
            Lude.<*> (x Lude..:? "CloudWatchLoggingOptions")
            Lude.<*> (x Lude..:? "ErrorOutputPrefix")
            Lude.<*> (x Lude..:? "DataFormatConversionConfiguration")
            Lude.<*> (x Lude..:? "ProcessingConfiguration")
            Lude.<*> (x Lude..: "RoleARN")
            Lude.<*> (x Lude..: "BucketARN")
            Lude.<*> (x Lude..: "BufferingHints")
            Lude.<*> (x Lude..: "CompressionFormat")
            Lude.<*> (x Lude..: "EncryptionConfiguration")
      )
