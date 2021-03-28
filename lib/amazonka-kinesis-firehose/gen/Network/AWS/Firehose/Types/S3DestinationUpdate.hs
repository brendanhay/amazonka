{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.S3DestinationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.S3DestinationUpdate
  ( S3DestinationUpdate (..)
  -- * Smart constructor
  , mkS3DestinationUpdate
  -- * Lenses
  , sBucketARN
  , sBufferingHints
  , sCloudWatchLoggingOptions
  , sCompressionFormat
  , sEncryptionConfiguration
  , sErrorOutputPrefix
  , sPrefix
  , sRoleARN
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

-- | Describes an update for a destination in Amazon S3.
--
-- /See:/ 'mkS3DestinationUpdate' smart constructor.
data S3DestinationUpdate = S3DestinationUpdate'
  { bucketARN :: Core.Maybe Types.BucketARN
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
  , roleARN :: Core.Maybe Types.RoleARN
    -- ^ The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3DestinationUpdate' value with any optional fields omitted.
mkS3DestinationUpdate
    :: S3DestinationUpdate
mkS3DestinationUpdate
  = S3DestinationUpdate'{bucketARN = Core.Nothing,
                         bufferingHints = Core.Nothing,
                         cloudWatchLoggingOptions = Core.Nothing,
                         compressionFormat = Core.Nothing,
                         encryptionConfiguration = Core.Nothing,
                         errorOutputPrefix = Core.Nothing, prefix = Core.Nothing,
                         roleARN = Core.Nothing}

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sBucketARN :: Lens.Lens' S3DestinationUpdate (Core.Maybe Types.BucketARN)
sBucketARN = Lens.field @"bucketARN"
{-# INLINEABLE sBucketARN #-}
{-# DEPRECATED bucketARN "Use generic-lens or generic-optics with 'bucketARN' instead"  #-}

-- | The buffering option. If no value is specified, @BufferingHints@ object default values are used.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sBufferingHints :: Lens.Lens' S3DestinationUpdate (Core.Maybe Types.BufferingHints)
sBufferingHints = Lens.field @"bufferingHints"
{-# INLINEABLE sBufferingHints #-}
{-# DEPRECATED bufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead"  #-}

-- | The CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCloudWatchLoggingOptions :: Lens.Lens' S3DestinationUpdate (Core.Maybe Types.CloudWatchLoggingOptions)
sCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# INLINEABLE sCloudWatchLoggingOptions #-}
{-# DEPRECATED cloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead"  #-}

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon Redshift destinations because they are not supported by the Amazon Redshift @COPY@ operation that reads from the S3 bucket.
--
-- /Note:/ Consider using 'compressionFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCompressionFormat :: Lens.Lens' S3DestinationUpdate (Core.Maybe Types.CompressionFormat)
sCompressionFormat = Lens.field @"compressionFormat"
{-# INLINEABLE sCompressionFormat #-}
{-# DEPRECATED compressionFormat "Use generic-lens or generic-optics with 'compressionFormat' instead"  #-}

-- | The encryption configuration. If no value is specified, the default is no encryption.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEncryptionConfiguration :: Lens.Lens' S3DestinationUpdate (Core.Maybe Types.EncryptionConfiguration)
sEncryptionConfiguration = Lens.field @"encryptionConfiguration"
{-# INLINEABLE sEncryptionConfiguration #-}
{-# DEPRECATED encryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead"  #-}

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'errorOutputPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sErrorOutputPrefix :: Lens.Lens' S3DestinationUpdate (Core.Maybe Types.ErrorOutputPrefix)
sErrorOutputPrefix = Lens.field @"errorOutputPrefix"
{-# INLINEABLE sErrorOutputPrefix #-}
{-# DEPRECATED errorOutputPrefix "Use generic-lens or generic-optics with 'errorOutputPrefix' instead"  #-}

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPrefix :: Lens.Lens' S3DestinationUpdate (Core.Maybe Types.Prefix)
sPrefix = Lens.field @"prefix"
{-# INLINEABLE sPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRoleARN :: Lens.Lens' S3DestinationUpdate (Core.Maybe Types.RoleARN)
sRoleARN = Lens.field @"roleARN"
{-# INLINEABLE sRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

instance Core.FromJSON S3DestinationUpdate where
        toJSON S3DestinationUpdate{..}
          = Core.object
              (Core.catMaybes
                 [("BucketARN" Core..=) Core.<$> bucketARN,
                  ("BufferingHints" Core..=) Core.<$> bufferingHints,
                  ("CloudWatchLoggingOptions" Core..=) Core.<$>
                    cloudWatchLoggingOptions,
                  ("CompressionFormat" Core..=) Core.<$> compressionFormat,
                  ("EncryptionConfiguration" Core..=) Core.<$>
                    encryptionConfiguration,
                  ("ErrorOutputPrefix" Core..=) Core.<$> errorOutputPrefix,
                  ("Prefix" Core..=) Core.<$> prefix,
                  ("RoleARN" Core..=) Core.<$> roleARN])
