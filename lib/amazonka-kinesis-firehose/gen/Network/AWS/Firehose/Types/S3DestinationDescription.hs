{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.S3DestinationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.S3DestinationDescription
  ( S3DestinationDescription (..),

    -- * Smart constructor
    mkS3DestinationDescription,

    -- * Lenses
    sddfPrefix,
    sddfCloudWatchLoggingOptions,
    sddfErrorOutputPrefix,
    sddfEncryptionConfiguration,
    sddfCompressionFormat,
    sddfBufferingHints,
    sddfBucketARN,
    sddfRoleARN,
  )
where

import Network.AWS.Firehose.Types.BufferingHints
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CompressionFormat
import Network.AWS.Firehose.Types.EncryptionConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a destination in Amazon S3.
--
-- /See:/ 'mkS3DestinationDescription' smart constructor.
data S3DestinationDescription = S3DestinationDescription'
  { -- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
    prefix :: Lude.Maybe Lude.Text,
    -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Lude.Maybe CloudWatchLoggingOptions,
    -- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
    errorOutputPrefix :: Lude.Maybe Lude.Text,
    -- | The encryption configuration. If no value is specified, the default is no encryption.
    encryptionConfiguration :: EncryptionConfiguration,
    -- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
    compressionFormat :: CompressionFormat,
    -- | The buffering option. If no value is specified, @BufferingHints@ object default values are used.
    bufferingHints :: BufferingHints,
    -- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    bucketARN :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3DestinationDescription' with the minimum fields required to make a request.
--
-- * 'prefix' - The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
-- * 'cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
-- * 'errorOutputPrefix' - A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
-- * 'encryptionConfiguration' - The encryption configuration. If no value is specified, the default is no encryption.
-- * 'compressionFormat' - The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
-- * 'bufferingHints' - The buffering option. If no value is specified, @BufferingHints@ object default values are used.
-- * 'bucketARN' - The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 'roleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
mkS3DestinationDescription ::
  -- | 'encryptionConfiguration'
  EncryptionConfiguration ->
  -- | 'compressionFormat'
  CompressionFormat ->
  -- | 'bufferingHints'
  BufferingHints ->
  -- | 'bucketARN'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  S3DestinationDescription
mkS3DestinationDescription
  pEncryptionConfiguration_
  pCompressionFormat_
  pBufferingHints_
  pBucketARN_
  pRoleARN_ =
    S3DestinationDescription'
      { prefix = Lude.Nothing,
        cloudWatchLoggingOptions = Lude.Nothing,
        errorOutputPrefix = Lude.Nothing,
        encryptionConfiguration = pEncryptionConfiguration_,
        compressionFormat = pCompressionFormat_,
        bufferingHints = pBufferingHints_,
        bucketARN = pBucketARN_,
        roleARN = pRoleARN_
      }

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddfPrefix :: Lens.Lens' S3DestinationDescription (Lude.Maybe Lude.Text)
sddfPrefix = Lens.lens (prefix :: S3DestinationDescription -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: S3DestinationDescription)
{-# DEPRECATED sddfPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddfCloudWatchLoggingOptions :: Lens.Lens' S3DestinationDescription (Lude.Maybe CloudWatchLoggingOptions)
sddfCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: S3DestinationDescription -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: S3DestinationDescription)
{-# DEPRECATED sddfCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'errorOutputPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddfErrorOutputPrefix :: Lens.Lens' S3DestinationDescription (Lude.Maybe Lude.Text)
sddfErrorOutputPrefix = Lens.lens (errorOutputPrefix :: S3DestinationDescription -> Lude.Maybe Lude.Text) (\s a -> s {errorOutputPrefix = a} :: S3DestinationDescription)
{-# DEPRECATED sddfErrorOutputPrefix "Use generic-lens or generic-optics with 'errorOutputPrefix' instead." #-}

-- | The encryption configuration. If no value is specified, the default is no encryption.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddfEncryptionConfiguration :: Lens.Lens' S3DestinationDescription EncryptionConfiguration
sddfEncryptionConfiguration = Lens.lens (encryptionConfiguration :: S3DestinationDescription -> EncryptionConfiguration) (\s a -> s {encryptionConfiguration = a} :: S3DestinationDescription)
{-# DEPRECATED sddfEncryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead." #-}

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
--
-- /Note:/ Consider using 'compressionFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddfCompressionFormat :: Lens.Lens' S3DestinationDescription CompressionFormat
sddfCompressionFormat = Lens.lens (compressionFormat :: S3DestinationDescription -> CompressionFormat) (\s a -> s {compressionFormat = a} :: S3DestinationDescription)
{-# DEPRECATED sddfCompressionFormat "Use generic-lens or generic-optics with 'compressionFormat' instead." #-}

-- | The buffering option. If no value is specified, @BufferingHints@ object default values are used.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddfBufferingHints :: Lens.Lens' S3DestinationDescription BufferingHints
sddfBufferingHints = Lens.lens (bufferingHints :: S3DestinationDescription -> BufferingHints) (\s a -> s {bufferingHints = a} :: S3DestinationDescription)
{-# DEPRECATED sddfBufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead." #-}

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddfBucketARN :: Lens.Lens' S3DestinationDescription Lude.Text
sddfBucketARN = Lens.lens (bucketARN :: S3DestinationDescription -> Lude.Text) (\s a -> s {bucketARN = a} :: S3DestinationDescription)
{-# DEPRECATED sddfBucketARN "Use generic-lens or generic-optics with 'bucketARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddfRoleARN :: Lens.Lens' S3DestinationDescription Lude.Text
sddfRoleARN = Lens.lens (roleARN :: S3DestinationDescription -> Lude.Text) (\s a -> s {roleARN = a} :: S3DestinationDescription)
{-# DEPRECATED sddfRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON S3DestinationDescription where
  parseJSON =
    Lude.withObject
      "S3DestinationDescription"
      ( \x ->
          S3DestinationDescription'
            Lude.<$> (x Lude..:? "Prefix")
            Lude.<*> (x Lude..:? "CloudWatchLoggingOptions")
            Lude.<*> (x Lude..:? "ErrorOutputPrefix")
            Lude.<*> (x Lude..: "EncryptionConfiguration")
            Lude.<*> (x Lude..: "CompressionFormat")
            Lude.<*> (x Lude..: "BufferingHints")
            Lude.<*> (x Lude..: "BucketARN")
            Lude.<*> (x Lude..: "RoleARN")
      )
