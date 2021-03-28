{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.S3DestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.S3DestinationConfiguration
  ( S3DestinationConfiguration (..)
  -- * Smart constructor
  , mkS3DestinationConfiguration
  -- * Lenses
  , sdcRoleARN
  , sdcBucketARN
  , sdcBufferingHints
  , sdcCloudWatchLoggingOptions
  , sdcCompressionFormat
  , sdcEncryptionConfiguration
  , sdcErrorOutputPrefix
  , sdcPrefix
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

-- | Describes the configuration of a destination in Amazon S3.
--
-- /See:/ 'mkS3DestinationConfiguration' smart constructor.
data S3DestinationConfiguration = S3DestinationConfiguration'
  { roleARN :: Types.RoleARN
    -- ^ The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
  , bucketARN :: Types.BucketARN
    -- ^ The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
  , bufferingHints :: Core.Maybe Types.BufferingHints
    -- ^ The buffering option. If no value is specified, @BufferingHints@ object default values are used.
  , cloudWatchLoggingOptions :: Core.Maybe Types.CloudWatchLoggingOptions
    -- ^ The CloudWatch logging options for your delivery stream.
  , compressionFormat :: Core.Maybe Types.CompressionFormat
    -- ^ The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon Redshift destinations because they are not supported by the Amazon Redshift @COPY@ operation that reads from the S3 bucket.
  , encryptionConfiguration :: Core.Maybe Types.EncryptionConfiguration
    -- ^ The encryption configuration. If no value is specified, the default is no encryption.
  , errorOutputPrefix :: Core.Maybe Types.ErrorOutputPrefix
    -- ^ A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
  , prefix :: Core.Maybe Types.Prefix
    -- ^ The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3DestinationConfiguration' value with any optional fields omitted.
mkS3DestinationConfiguration
    :: Types.RoleARN -- ^ 'roleARN'
    -> Types.BucketARN -- ^ 'bucketARN'
    -> S3DestinationConfiguration
mkS3DestinationConfiguration roleARN bucketARN
  = S3DestinationConfiguration'{roleARN, bucketARN,
                                bufferingHints = Core.Nothing,
                                cloudWatchLoggingOptions = Core.Nothing,
                                compressionFormat = Core.Nothing,
                                encryptionConfiguration = Core.Nothing,
                                errorOutputPrefix = Core.Nothing, prefix = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcRoleARN :: Lens.Lens' S3DestinationConfiguration Types.RoleARN
sdcRoleARN = Lens.field @"roleARN"
{-# INLINEABLE sdcRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcBucketARN :: Lens.Lens' S3DestinationConfiguration Types.BucketARN
sdcBucketARN = Lens.field @"bucketARN"
{-# INLINEABLE sdcBucketARN #-}
{-# DEPRECATED bucketARN "Use generic-lens or generic-optics with 'bucketARN' instead"  #-}

-- | The buffering option. If no value is specified, @BufferingHints@ object default values are used.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcBufferingHints :: Lens.Lens' S3DestinationConfiguration (Core.Maybe Types.BufferingHints)
sdcBufferingHints = Lens.field @"bufferingHints"
{-# INLINEABLE sdcBufferingHints #-}
{-# DEPRECATED bufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead"  #-}

-- | The CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcCloudWatchLoggingOptions :: Lens.Lens' S3DestinationConfiguration (Core.Maybe Types.CloudWatchLoggingOptions)
sdcCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# INLINEABLE sdcCloudWatchLoggingOptions #-}
{-# DEPRECATED cloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead"  #-}

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon Redshift destinations because they are not supported by the Amazon Redshift @COPY@ operation that reads from the S3 bucket.
--
-- /Note:/ Consider using 'compressionFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcCompressionFormat :: Lens.Lens' S3DestinationConfiguration (Core.Maybe Types.CompressionFormat)
sdcCompressionFormat = Lens.field @"compressionFormat"
{-# INLINEABLE sdcCompressionFormat #-}
{-# DEPRECATED compressionFormat "Use generic-lens or generic-optics with 'compressionFormat' instead"  #-}

-- | The encryption configuration. If no value is specified, the default is no encryption.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcEncryptionConfiguration :: Lens.Lens' S3DestinationConfiguration (Core.Maybe Types.EncryptionConfiguration)
sdcEncryptionConfiguration = Lens.field @"encryptionConfiguration"
{-# INLINEABLE sdcEncryptionConfiguration #-}
{-# DEPRECATED encryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead"  #-}

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'errorOutputPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcErrorOutputPrefix :: Lens.Lens' S3DestinationConfiguration (Core.Maybe Types.ErrorOutputPrefix)
sdcErrorOutputPrefix = Lens.field @"errorOutputPrefix"
{-# INLINEABLE sdcErrorOutputPrefix #-}
{-# DEPRECATED errorOutputPrefix "Use generic-lens or generic-optics with 'errorOutputPrefix' instead"  #-}

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcPrefix :: Lens.Lens' S3DestinationConfiguration (Core.Maybe Types.Prefix)
sdcPrefix = Lens.field @"prefix"
{-# INLINEABLE sdcPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

instance Core.FromJSON S3DestinationConfiguration where
        toJSON S3DestinationConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RoleARN" Core..= roleARN),
                  Core.Just ("BucketARN" Core..= bucketARN),
                  ("BufferingHints" Core..=) Core.<$> bufferingHints,
                  ("CloudWatchLoggingOptions" Core..=) Core.<$>
                    cloudWatchLoggingOptions,
                  ("CompressionFormat" Core..=) Core.<$> compressionFormat,
                  ("EncryptionConfiguration" Core..=) Core.<$>
                    encryptionConfiguration,
                  ("ErrorOutputPrefix" Core..=) Core.<$> errorOutputPrefix,
                  ("Prefix" Core..=) Core.<$> prefix])
