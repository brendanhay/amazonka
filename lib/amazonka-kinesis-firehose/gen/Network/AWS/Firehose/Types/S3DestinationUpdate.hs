{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.S3DestinationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.S3DestinationUpdate
  ( S3DestinationUpdate (..),

    -- * Smart constructor
    mkS3DestinationUpdate,

    -- * Lenses
    sPrefix,
    sCloudWatchLoggingOptions,
    sErrorOutputPrefix,
    sEncryptionConfiguration,
    sCompressionFormat,
    sBufferingHints,
    sBucketARN,
    sRoleARN,
  )
where

import Network.AWS.Firehose.Types.BufferingHints
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CompressionFormat
import Network.AWS.Firehose.Types.EncryptionConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an update for a destination in Amazon S3.
--
-- /See:/ 'mkS3DestinationUpdate' smart constructor.
data S3DestinationUpdate = S3DestinationUpdate'
  { prefix ::
      Lude.Maybe Lude.Text,
    cloudWatchLoggingOptions ::
      Lude.Maybe CloudWatchLoggingOptions,
    errorOutputPrefix :: Lude.Maybe Lude.Text,
    encryptionConfiguration ::
      Lude.Maybe EncryptionConfiguration,
    compressionFormat :: Lude.Maybe CompressionFormat,
    bufferingHints :: Lude.Maybe BufferingHints,
    bucketARN :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'S3DestinationUpdate' with the minimum fields required to make a request.
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
mkS3DestinationUpdate ::
  S3DestinationUpdate
mkS3DestinationUpdate =
  S3DestinationUpdate'
    { prefix = Lude.Nothing,
      cloudWatchLoggingOptions = Lude.Nothing,
      errorOutputPrefix = Lude.Nothing,
      encryptionConfiguration = Lude.Nothing,
      compressionFormat = Lude.Nothing,
      bufferingHints = Lude.Nothing,
      bucketARN = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPrefix :: Lens.Lens' S3DestinationUpdate (Lude.Maybe Lude.Text)
sPrefix = Lens.lens (prefix :: S3DestinationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: S3DestinationUpdate)
{-# DEPRECATED sPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCloudWatchLoggingOptions :: Lens.Lens' S3DestinationUpdate (Lude.Maybe CloudWatchLoggingOptions)
sCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: S3DestinationUpdate -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: S3DestinationUpdate)
{-# DEPRECATED sCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'errorOutputPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sErrorOutputPrefix :: Lens.Lens' S3DestinationUpdate (Lude.Maybe Lude.Text)
sErrorOutputPrefix = Lens.lens (errorOutputPrefix :: S3DestinationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {errorOutputPrefix = a} :: S3DestinationUpdate)
{-# DEPRECATED sErrorOutputPrefix "Use generic-lens or generic-optics with 'errorOutputPrefix' instead." #-}

-- | The encryption configuration. If no value is specified, the default is no encryption.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEncryptionConfiguration :: Lens.Lens' S3DestinationUpdate (Lude.Maybe EncryptionConfiguration)
sEncryptionConfiguration = Lens.lens (encryptionConfiguration :: S3DestinationUpdate -> Lude.Maybe EncryptionConfiguration) (\s a -> s {encryptionConfiguration = a} :: S3DestinationUpdate)
{-# DEPRECATED sEncryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead." #-}

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon Redshift destinations because they are not supported by the Amazon Redshift @COPY@ operation that reads from the S3 bucket.
--
-- /Note:/ Consider using 'compressionFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCompressionFormat :: Lens.Lens' S3DestinationUpdate (Lude.Maybe CompressionFormat)
sCompressionFormat = Lens.lens (compressionFormat :: S3DestinationUpdate -> Lude.Maybe CompressionFormat) (\s a -> s {compressionFormat = a} :: S3DestinationUpdate)
{-# DEPRECATED sCompressionFormat "Use generic-lens or generic-optics with 'compressionFormat' instead." #-}

-- | The buffering option. If no value is specified, @BufferingHints@ object default values are used.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sBufferingHints :: Lens.Lens' S3DestinationUpdate (Lude.Maybe BufferingHints)
sBufferingHints = Lens.lens (bufferingHints :: S3DestinationUpdate -> Lude.Maybe BufferingHints) (\s a -> s {bufferingHints = a} :: S3DestinationUpdate)
{-# DEPRECATED sBufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead." #-}

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sBucketARN :: Lens.Lens' S3DestinationUpdate (Lude.Maybe Lude.Text)
sBucketARN = Lens.lens (bucketARN :: S3DestinationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {bucketARN = a} :: S3DestinationUpdate)
{-# DEPRECATED sBucketARN "Use generic-lens or generic-optics with 'bucketARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRoleARN :: Lens.Lens' S3DestinationUpdate (Lude.Maybe Lude.Text)
sRoleARN = Lens.lens (roleARN :: S3DestinationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: S3DestinationUpdate)
{-# DEPRECATED sRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.ToJSON S3DestinationUpdate where
  toJSON S3DestinationUpdate' {..} =
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
            ("BucketARN" Lude..=) Lude.<$> bucketARN,
            ("RoleARN" Lude..=) Lude.<$> roleARN
          ]
      )
