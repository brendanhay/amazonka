{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.S3DestinationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.S3DestinationDescription
  ( S3DestinationDescription (..)
  -- * Smart constructor
  , mkS3DestinationDescription
  -- * Lenses
  , sddRoleARN
  , sddBucketARN
  , sddBufferingHints
  , sddCompressionFormat
  , sddEncryptionConfiguration
  , sddCloudWatchLoggingOptions
  , sddErrorOutputPrefix
  , sddPrefix
  ) where

import qualified Network.AWS.Firehose.Types.BucketARN as Types
import qualified Network.AWS.Firehose.Types.BufferingHints as Types
import qualified Network.AWS.Firehose.Types.CloudWatchLoggingOptions as Types
import qualified Network.AWS.Firehose.Types.CompressionFormat as Types
import qualified Network.AWS.Firehose.Types.EncryptionConfiguration as Types
import qualified Network.AWS.Firehose.Types.ErrorOutputPrefix as Types
import qualified Network.AWS.Firehose.Types.Prefix as Types
import qualified Network.AWS.Firehose.Types.RoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a destination in Amazon S3.
--
-- /See:/ 'mkS3DestinationDescription' smart constructor.
data S3DestinationDescription = S3DestinationDescription'
  { roleARN :: Types.RoleARN
    -- ^ The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
  , bucketARN :: Types.BucketARN
    -- ^ The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
  , bufferingHints :: Types.BufferingHints
    -- ^ The buffering option. If no value is specified, @BufferingHints@ object default values are used.
  , compressionFormat :: Types.CompressionFormat
    -- ^ The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
  , encryptionConfiguration :: Types.EncryptionConfiguration
    -- ^ The encryption configuration. If no value is specified, the default is no encryption.
  , cloudWatchLoggingOptions :: Core.Maybe Types.CloudWatchLoggingOptions
    -- ^ The Amazon CloudWatch logging options for your delivery stream.
  , errorOutputPrefix :: Core.Maybe Types.ErrorOutputPrefix
    -- ^ A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
  , prefix :: Core.Maybe Types.Prefix
    -- ^ The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3DestinationDescription' value with any optional fields omitted.
mkS3DestinationDescription
    :: Types.RoleARN -- ^ 'roleARN'
    -> Types.BucketARN -- ^ 'bucketARN'
    -> Types.BufferingHints -- ^ 'bufferingHints'
    -> Types.CompressionFormat -- ^ 'compressionFormat'
    -> Types.EncryptionConfiguration -- ^ 'encryptionConfiguration'
    -> S3DestinationDescription
mkS3DestinationDescription roleARN bucketARN bufferingHints
  compressionFormat encryptionConfiguration
  = S3DestinationDescription'{roleARN, bucketARN, bufferingHints,
                              compressionFormat, encryptionConfiguration,
                              cloudWatchLoggingOptions = Core.Nothing,
                              errorOutputPrefix = Core.Nothing, prefix = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddRoleARN :: Lens.Lens' S3DestinationDescription Types.RoleARN
sddRoleARN = Lens.field @"roleARN"
{-# INLINEABLE sddRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddBucketARN :: Lens.Lens' S3DestinationDescription Types.BucketARN
sddBucketARN = Lens.field @"bucketARN"
{-# INLINEABLE sddBucketARN #-}
{-# DEPRECATED bucketARN "Use generic-lens or generic-optics with 'bucketARN' instead"  #-}

-- | The buffering option. If no value is specified, @BufferingHints@ object default values are used.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddBufferingHints :: Lens.Lens' S3DestinationDescription Types.BufferingHints
sddBufferingHints = Lens.field @"bufferingHints"
{-# INLINEABLE sddBufferingHints #-}
{-# DEPRECATED bufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead"  #-}

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
--
-- /Note:/ Consider using 'compressionFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddCompressionFormat :: Lens.Lens' S3DestinationDescription Types.CompressionFormat
sddCompressionFormat = Lens.field @"compressionFormat"
{-# INLINEABLE sddCompressionFormat #-}
{-# DEPRECATED compressionFormat "Use generic-lens or generic-optics with 'compressionFormat' instead"  #-}

-- | The encryption configuration. If no value is specified, the default is no encryption.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddEncryptionConfiguration :: Lens.Lens' S3DestinationDescription Types.EncryptionConfiguration
sddEncryptionConfiguration = Lens.field @"encryptionConfiguration"
{-# INLINEABLE sddEncryptionConfiguration #-}
{-# DEPRECATED encryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead"  #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddCloudWatchLoggingOptions :: Lens.Lens' S3DestinationDescription (Core.Maybe Types.CloudWatchLoggingOptions)
sddCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# INLINEABLE sddCloudWatchLoggingOptions #-}
{-# DEPRECATED cloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead"  #-}

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'errorOutputPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddErrorOutputPrefix :: Lens.Lens' S3DestinationDescription (Core.Maybe Types.ErrorOutputPrefix)
sddErrorOutputPrefix = Lens.field @"errorOutputPrefix"
{-# INLINEABLE sddErrorOutputPrefix #-}
{-# DEPRECATED errorOutputPrefix "Use generic-lens or generic-optics with 'errorOutputPrefix' instead"  #-}

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddPrefix :: Lens.Lens' S3DestinationDescription (Core.Maybe Types.Prefix)
sddPrefix = Lens.field @"prefix"
{-# INLINEABLE sddPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

instance Core.FromJSON S3DestinationDescription where
        parseJSON
          = Core.withObject "S3DestinationDescription" Core.$
              \ x ->
                S3DestinationDescription' Core.<$>
                  (x Core..: "RoleARN") Core.<*> x Core..: "BucketARN" Core.<*>
                    x Core..: "BufferingHints"
                    Core.<*> x Core..: "CompressionFormat"
                    Core.<*> x Core..: "EncryptionConfiguration"
                    Core.<*> x Core..:? "CloudWatchLoggingOptions"
                    Core.<*> x Core..:? "ErrorOutputPrefix"
                    Core.<*> x Core..:? "Prefix"
