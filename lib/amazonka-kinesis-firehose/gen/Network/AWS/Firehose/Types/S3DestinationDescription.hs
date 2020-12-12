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
    s3Prefix,
    s3CloudWatchLoggingOptions,
    s3ErrorOutputPrefix,
    s3RoleARN,
    s3BucketARN,
    s3BufferingHints,
    s3CompressionFormat,
    s3EncryptionConfiguration,
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
  { prefix ::
      Lude.Maybe Lude.Text,
    cloudWatchLoggingOptions ::
      Lude.Maybe CloudWatchLoggingOptions,
    errorOutputPrefix :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Text,
    bucketARN :: Lude.Text,
    bufferingHints :: BufferingHints,
    compressionFormat :: CompressionFormat,
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

-- | Creates a value of 'S3DestinationDescription' with the minimum fields required to make a request.
--
-- * 'bucketARN' - The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 'bufferingHints' - The buffering option. If no value is specified, @BufferingHints@ object default values are used.
-- * 'cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
-- * 'compressionFormat' - The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
-- * 'encryptionConfiguration' - The encryption configuration. If no value is specified, the default is no encryption.
-- * 'errorOutputPrefix' - A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
-- * 'prefix' - The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
-- * 'roleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
mkS3DestinationDescription ::
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
  S3DestinationDescription
mkS3DestinationDescription
  pRoleARN_
  pBucketARN_
  pBufferingHints_
  pCompressionFormat_
  pEncryptionConfiguration_ =
    S3DestinationDescription'
      { prefix = Lude.Nothing,
        cloudWatchLoggingOptions = Lude.Nothing,
        errorOutputPrefix = Lude.Nothing,
        roleARN = pRoleARN_,
        bucketARN = pBucketARN_,
        bufferingHints = pBufferingHints_,
        compressionFormat = pCompressionFormat_,
        encryptionConfiguration = pEncryptionConfiguration_
      }

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
s3Prefix :: Lens.Lens' S3DestinationDescription (Lude.Maybe Lude.Text)
s3Prefix = Lens.lens (prefix :: S3DestinationDescription -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: S3DestinationDescription)
{-# DEPRECATED s3Prefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
s3CloudWatchLoggingOptions :: Lens.Lens' S3DestinationDescription (Lude.Maybe CloudWatchLoggingOptions)
s3CloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: S3DestinationDescription -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: S3DestinationDescription)
{-# DEPRECATED s3CloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'errorOutputPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
s3ErrorOutputPrefix :: Lens.Lens' S3DestinationDescription (Lude.Maybe Lude.Text)
s3ErrorOutputPrefix = Lens.lens (errorOutputPrefix :: S3DestinationDescription -> Lude.Maybe Lude.Text) (\s a -> s {errorOutputPrefix = a} :: S3DestinationDescription)
{-# DEPRECATED s3ErrorOutputPrefix "Use generic-lens or generic-optics with 'errorOutputPrefix' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
s3RoleARN :: Lens.Lens' S3DestinationDescription Lude.Text
s3RoleARN = Lens.lens (roleARN :: S3DestinationDescription -> Lude.Text) (\s a -> s {roleARN = a} :: S3DestinationDescription)
{-# DEPRECATED s3RoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
s3BucketARN :: Lens.Lens' S3DestinationDescription Lude.Text
s3BucketARN = Lens.lens (bucketARN :: S3DestinationDescription -> Lude.Text) (\s a -> s {bucketARN = a} :: S3DestinationDescription)
{-# DEPRECATED s3BucketARN "Use generic-lens or generic-optics with 'bucketARN' instead." #-}

-- | The buffering option. If no value is specified, @BufferingHints@ object default values are used.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
s3BufferingHints :: Lens.Lens' S3DestinationDescription BufferingHints
s3BufferingHints = Lens.lens (bufferingHints :: S3DestinationDescription -> BufferingHints) (\s a -> s {bufferingHints = a} :: S3DestinationDescription)
{-# DEPRECATED s3BufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead." #-}

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
--
-- /Note:/ Consider using 'compressionFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
s3CompressionFormat :: Lens.Lens' S3DestinationDescription CompressionFormat
s3CompressionFormat = Lens.lens (compressionFormat :: S3DestinationDescription -> CompressionFormat) (\s a -> s {compressionFormat = a} :: S3DestinationDescription)
{-# DEPRECATED s3CompressionFormat "Use generic-lens or generic-optics with 'compressionFormat' instead." #-}

-- | The encryption configuration. If no value is specified, the default is no encryption.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
s3EncryptionConfiguration :: Lens.Lens' S3DestinationDescription EncryptionConfiguration
s3EncryptionConfiguration = Lens.lens (encryptionConfiguration :: S3DestinationDescription -> EncryptionConfiguration) (\s a -> s {encryptionConfiguration = a} :: S3DestinationDescription)
{-# DEPRECATED s3EncryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead." #-}

instance Lude.FromJSON S3DestinationDescription where
  parseJSON =
    Lude.withObject
      "S3DestinationDescription"
      ( \x ->
          S3DestinationDescription'
            Lude.<$> (x Lude..:? "Prefix")
            Lude.<*> (x Lude..:? "CloudWatchLoggingOptions")
            Lude.<*> (x Lude..:? "ErrorOutputPrefix")
            Lude.<*> (x Lude..: "RoleARN")
            Lude.<*> (x Lude..: "BucketARN")
            Lude.<*> (x Lude..: "BufferingHints")
            Lude.<*> (x Lude..: "CompressionFormat")
            Lude.<*> (x Lude..: "EncryptionConfiguration")
      )
