-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.S3DestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.S3DestinationConfiguration
  ( S3DestinationConfiguration (..),

    -- * Smart constructor
    mkS3DestinationConfiguration,

    -- * Lenses
    sdcPrefix,
    sdcCloudWatchLoggingOptions,
    sdcErrorOutputPrefix,
    sdcEncryptionConfiguration,
    sdcCompressionFormat,
    sdcBufferingHints,
    sdcRoleARN,
    sdcBucketARN,
  )
where

import Network.AWS.Firehose.Types.BufferingHints
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CompressionFormat
import Network.AWS.Firehose.Types.EncryptionConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the configuration of a destination in Amazon S3.
--
-- /See:/ 'mkS3DestinationConfiguration' smart constructor.
data S3DestinationConfiguration = S3DestinationConfiguration'
  { prefix ::
      Lude.Maybe Lude.Text,
    cloudWatchLoggingOptions ::
      Lude.Maybe CloudWatchLoggingOptions,
    errorOutputPrefix ::
      Lude.Maybe Lude.Text,
    encryptionConfiguration ::
      Lude.Maybe EncryptionConfiguration,
    compressionFormat ::
      Lude.Maybe CompressionFormat,
    bufferingHints ::
      Lude.Maybe BufferingHints,
    roleARN :: Lude.Text,
    bucketARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3DestinationConfiguration' with the minimum fields required to make a request.
--
-- * 'bucketARN' - The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 'bufferingHints' - The buffering option. If no value is specified, @BufferingHints@ object default values are used.
-- * 'cloudWatchLoggingOptions' - The CloudWatch logging options for your delivery stream.
-- * 'compressionFormat' - The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon Redshift destinations because they are not supported by the Amazon Redshift @COPY@ operation that reads from the S3 bucket.
-- * 'encryptionConfiguration' - The encryption configuration. If no value is specified, the default is no encryption.
-- * 'errorOutputPrefix' - A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
-- * 'prefix' - The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
-- * 'roleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
mkS3DestinationConfiguration ::
  -- | 'roleARN'
  Lude.Text ->
  -- | 'bucketARN'
  Lude.Text ->
  S3DestinationConfiguration
mkS3DestinationConfiguration pRoleARN_ pBucketARN_ =
  S3DestinationConfiguration'
    { prefix = Lude.Nothing,
      cloudWatchLoggingOptions = Lude.Nothing,
      errorOutputPrefix = Lude.Nothing,
      encryptionConfiguration = Lude.Nothing,
      compressionFormat = Lude.Nothing,
      bufferingHints = Lude.Nothing,
      roleARN = pRoleARN_,
      bucketARN = pBucketARN_
    }

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcPrefix :: Lens.Lens' S3DestinationConfiguration (Lude.Maybe Lude.Text)
sdcPrefix = Lens.lens (prefix :: S3DestinationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: S3DestinationConfiguration)
{-# DEPRECATED sdcPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcCloudWatchLoggingOptions :: Lens.Lens' S3DestinationConfiguration (Lude.Maybe CloudWatchLoggingOptions)
sdcCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: S3DestinationConfiguration -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: S3DestinationConfiguration)
{-# DEPRECATED sdcCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'errorOutputPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcErrorOutputPrefix :: Lens.Lens' S3DestinationConfiguration (Lude.Maybe Lude.Text)
sdcErrorOutputPrefix = Lens.lens (errorOutputPrefix :: S3DestinationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {errorOutputPrefix = a} :: S3DestinationConfiguration)
{-# DEPRECATED sdcErrorOutputPrefix "Use generic-lens or generic-optics with 'errorOutputPrefix' instead." #-}

-- | The encryption configuration. If no value is specified, the default is no encryption.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcEncryptionConfiguration :: Lens.Lens' S3DestinationConfiguration (Lude.Maybe EncryptionConfiguration)
sdcEncryptionConfiguration = Lens.lens (encryptionConfiguration :: S3DestinationConfiguration -> Lude.Maybe EncryptionConfiguration) (\s a -> s {encryptionConfiguration = a} :: S3DestinationConfiguration)
{-# DEPRECATED sdcEncryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead." #-}

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon Redshift destinations because they are not supported by the Amazon Redshift @COPY@ operation that reads from the S3 bucket.
--
-- /Note:/ Consider using 'compressionFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcCompressionFormat :: Lens.Lens' S3DestinationConfiguration (Lude.Maybe CompressionFormat)
sdcCompressionFormat = Lens.lens (compressionFormat :: S3DestinationConfiguration -> Lude.Maybe CompressionFormat) (\s a -> s {compressionFormat = a} :: S3DestinationConfiguration)
{-# DEPRECATED sdcCompressionFormat "Use generic-lens or generic-optics with 'compressionFormat' instead." #-}

-- | The buffering option. If no value is specified, @BufferingHints@ object default values are used.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcBufferingHints :: Lens.Lens' S3DestinationConfiguration (Lude.Maybe BufferingHints)
sdcBufferingHints = Lens.lens (bufferingHints :: S3DestinationConfiguration -> Lude.Maybe BufferingHints) (\s a -> s {bufferingHints = a} :: S3DestinationConfiguration)
{-# DEPRECATED sdcBufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcRoleARN :: Lens.Lens' S3DestinationConfiguration Lude.Text
sdcRoleARN = Lens.lens (roleARN :: S3DestinationConfiguration -> Lude.Text) (\s a -> s {roleARN = a} :: S3DestinationConfiguration)
{-# DEPRECATED sdcRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcBucketARN :: Lens.Lens' S3DestinationConfiguration Lude.Text
sdcBucketARN = Lens.lens (bucketARN :: S3DestinationConfiguration -> Lude.Text) (\s a -> s {bucketARN = a} :: S3DestinationConfiguration)
{-# DEPRECATED sdcBucketARN "Use generic-lens or generic-optics with 'bucketARN' instead." #-}

instance Lude.ToJSON S3DestinationConfiguration where
  toJSON S3DestinationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Prefix" Lude..=) Lude.<$> prefix,
            ("CloudWatchLoggingOptions" Lude..=)
              Lude.<$> cloudWatchLoggingOptions,
            ("ErrorOutputPrefix" Lude..=) Lude.<$> errorOutputPrefix,
            ("EncryptionConfiguration" Lude..=)
              Lude.<$> encryptionConfiguration,
            ("CompressionFormat" Lude..=) Lude.<$> compressionFormat,
            ("BufferingHints" Lude..=) Lude.<$> bufferingHints,
            Lude.Just ("RoleARN" Lude..= roleARN),
            Lude.Just ("BucketARN" Lude..= bucketARN)
          ]
      )
