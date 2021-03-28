{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ExtendedS3DestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.ExtendedS3DestinationConfiguration
  ( ExtendedS3DestinationConfiguration (..)
  -- * Smart constructor
  , mkExtendedS3DestinationConfiguration
  -- * Lenses
  , esdcRoleARN
  , esdcBucketARN
  , esdcBufferingHints
  , esdcCloudWatchLoggingOptions
  , esdcCompressionFormat
  , esdcDataFormatConversionConfiguration
  , esdcEncryptionConfiguration
  , esdcErrorOutputPrefix
  , esdcPrefix
  , esdcProcessingConfiguration
  , esdcS3BackupConfiguration
  , esdcS3BackupMode
  ) where

import qualified Network.AWS.Firehose.Types.BucketARN as Types
import qualified Network.AWS.Firehose.Types.BufferingHints as Types
import qualified Network.AWS.Firehose.Types.CloudWatchLoggingOptions as Types
import qualified Network.AWS.Firehose.Types.CompressionFormat as Types
import qualified Network.AWS.Firehose.Types.DataFormatConversionConfiguration as Types
import qualified Network.AWS.Firehose.Types.EncryptionConfiguration as Types
import qualified Network.AWS.Firehose.Types.ErrorOutputPrefix as Types
import qualified Network.AWS.Firehose.Types.Prefix as Types
import qualified Network.AWS.Firehose.Types.ProcessingConfiguration as Types
import qualified Network.AWS.Firehose.Types.RoleARN as Types
import qualified Network.AWS.Firehose.Types.S3BackupMode as Types
import qualified Network.AWS.Firehose.Types.S3DestinationConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the configuration of a destination in Amazon S3.
--
-- /See:/ 'mkExtendedS3DestinationConfiguration' smart constructor.
data ExtendedS3DestinationConfiguration = ExtendedS3DestinationConfiguration'
  { roleARN :: Types.RoleARN
    -- ^ The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
  , bucketARN :: Types.BucketARN
    -- ^ The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
  , bufferingHints :: Core.Maybe Types.BufferingHints
    -- ^ The buffering option.
  , cloudWatchLoggingOptions :: Core.Maybe Types.CloudWatchLoggingOptions
    -- ^ The Amazon CloudWatch logging options for your delivery stream.
  , compressionFormat :: Core.Maybe Types.CompressionFormat
    -- ^ The compression format. If no value is specified, the default is UNCOMPRESSED.
  , dataFormatConversionConfiguration :: Core.Maybe Types.DataFormatConversionConfiguration
    -- ^ The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
  , encryptionConfiguration :: Core.Maybe Types.EncryptionConfiguration
    -- ^ The encryption configuration. If no value is specified, the default is no encryption.
  , errorOutputPrefix :: Core.Maybe Types.ErrorOutputPrefix
    -- ^ A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
  , prefix :: Core.Maybe Types.Prefix
    -- ^ The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
  , processingConfiguration :: Core.Maybe Types.ProcessingConfiguration
    -- ^ The data processing configuration.
  , s3BackupConfiguration :: Core.Maybe Types.S3DestinationConfiguration
    -- ^ The configuration for backup in Amazon S3.
  , s3BackupMode :: Core.Maybe Types.S3BackupMode
    -- ^ The Amazon S3 backup mode. After you create a delivery stream, you can update it to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExtendedS3DestinationConfiguration' value with any optional fields omitted.
mkExtendedS3DestinationConfiguration
    :: Types.RoleARN -- ^ 'roleARN'
    -> Types.BucketARN -- ^ 'bucketARN'
    -> ExtendedS3DestinationConfiguration
mkExtendedS3DestinationConfiguration roleARN bucketARN
  = ExtendedS3DestinationConfiguration'{roleARN, bucketARN,
                                        bufferingHints = Core.Nothing,
                                        cloudWatchLoggingOptions = Core.Nothing,
                                        compressionFormat = Core.Nothing,
                                        dataFormatConversionConfiguration = Core.Nothing,
                                        encryptionConfiguration = Core.Nothing,
                                        errorOutputPrefix = Core.Nothing, prefix = Core.Nothing,
                                        processingConfiguration = Core.Nothing,
                                        s3BackupConfiguration = Core.Nothing,
                                        s3BackupMode = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcRoleARN :: Lens.Lens' ExtendedS3DestinationConfiguration Types.RoleARN
esdcRoleARN = Lens.field @"roleARN"
{-# INLINEABLE esdcRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcBucketARN :: Lens.Lens' ExtendedS3DestinationConfiguration Types.BucketARN
esdcBucketARN = Lens.field @"bucketARN"
{-# INLINEABLE esdcBucketARN #-}
{-# DEPRECATED bucketARN "Use generic-lens or generic-optics with 'bucketARN' instead"  #-}

-- | The buffering option.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcBufferingHints :: Lens.Lens' ExtendedS3DestinationConfiguration (Core.Maybe Types.BufferingHints)
esdcBufferingHints = Lens.field @"bufferingHints"
{-# INLINEABLE esdcBufferingHints #-}
{-# DEPRECATED bufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead"  #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcCloudWatchLoggingOptions :: Lens.Lens' ExtendedS3DestinationConfiguration (Core.Maybe Types.CloudWatchLoggingOptions)
esdcCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# INLINEABLE esdcCloudWatchLoggingOptions #-}
{-# DEPRECATED cloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead"  #-}

-- | The compression format. If no value is specified, the default is UNCOMPRESSED.
--
-- /Note:/ Consider using 'compressionFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcCompressionFormat :: Lens.Lens' ExtendedS3DestinationConfiguration (Core.Maybe Types.CompressionFormat)
esdcCompressionFormat = Lens.field @"compressionFormat"
{-# INLINEABLE esdcCompressionFormat #-}
{-# DEPRECATED compressionFormat "Use generic-lens or generic-optics with 'compressionFormat' instead"  #-}

-- | The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
--
-- /Note:/ Consider using 'dataFormatConversionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcDataFormatConversionConfiguration :: Lens.Lens' ExtendedS3DestinationConfiguration (Core.Maybe Types.DataFormatConversionConfiguration)
esdcDataFormatConversionConfiguration = Lens.field @"dataFormatConversionConfiguration"
{-# INLINEABLE esdcDataFormatConversionConfiguration #-}
{-# DEPRECATED dataFormatConversionConfiguration "Use generic-lens or generic-optics with 'dataFormatConversionConfiguration' instead"  #-}

-- | The encryption configuration. If no value is specified, the default is no encryption.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcEncryptionConfiguration :: Lens.Lens' ExtendedS3DestinationConfiguration (Core.Maybe Types.EncryptionConfiguration)
esdcEncryptionConfiguration = Lens.field @"encryptionConfiguration"
{-# INLINEABLE esdcEncryptionConfiguration #-}
{-# DEPRECATED encryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead"  #-}

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'errorOutputPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcErrorOutputPrefix :: Lens.Lens' ExtendedS3DestinationConfiguration (Core.Maybe Types.ErrorOutputPrefix)
esdcErrorOutputPrefix = Lens.field @"errorOutputPrefix"
{-# INLINEABLE esdcErrorOutputPrefix #-}
{-# DEPRECATED errorOutputPrefix "Use generic-lens or generic-optics with 'errorOutputPrefix' instead"  #-}

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcPrefix :: Lens.Lens' ExtendedS3DestinationConfiguration (Core.Maybe Types.Prefix)
esdcPrefix = Lens.field @"prefix"
{-# INLINEABLE esdcPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcProcessingConfiguration :: Lens.Lens' ExtendedS3DestinationConfiguration (Core.Maybe Types.ProcessingConfiguration)
esdcProcessingConfiguration = Lens.field @"processingConfiguration"
{-# INLINEABLE esdcProcessingConfiguration #-}
{-# DEPRECATED processingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead"  #-}

-- | The configuration for backup in Amazon S3.
--
-- /Note:/ Consider using 's3BackupConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcS3BackupConfiguration :: Lens.Lens' ExtendedS3DestinationConfiguration (Core.Maybe Types.S3DestinationConfiguration)
esdcS3BackupConfiguration = Lens.field @"s3BackupConfiguration"
{-# INLINEABLE esdcS3BackupConfiguration #-}
{-# DEPRECATED s3BackupConfiguration "Use generic-lens or generic-optics with 's3BackupConfiguration' instead"  #-}

-- | The Amazon S3 backup mode. After you create a delivery stream, you can update it to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it. 
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esdcS3BackupMode :: Lens.Lens' ExtendedS3DestinationConfiguration (Core.Maybe Types.S3BackupMode)
esdcS3BackupMode = Lens.field @"s3BackupMode"
{-# INLINEABLE esdcS3BackupMode #-}
{-# DEPRECATED s3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead"  #-}

instance Core.FromJSON ExtendedS3DestinationConfiguration where
        toJSON ExtendedS3DestinationConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RoleARN" Core..= roleARN),
                  Core.Just ("BucketARN" Core..= bucketARN),
                  ("BufferingHints" Core..=) Core.<$> bufferingHints,
                  ("CloudWatchLoggingOptions" Core..=) Core.<$>
                    cloudWatchLoggingOptions,
                  ("CompressionFormat" Core..=) Core.<$> compressionFormat,
                  ("DataFormatConversionConfiguration" Core..=) Core.<$>
                    dataFormatConversionConfiguration,
                  ("EncryptionConfiguration" Core..=) Core.<$>
                    encryptionConfiguration,
                  ("ErrorOutputPrefix" Core..=) Core.<$> errorOutputPrefix,
                  ("Prefix" Core..=) Core.<$> prefix,
                  ("ProcessingConfiguration" Core..=) Core.<$>
                    processingConfiguration,
                  ("S3BackupConfiguration" Core..=) Core.<$> s3BackupConfiguration,
                  ("S3BackupMode" Core..=) Core.<$> s3BackupMode])
