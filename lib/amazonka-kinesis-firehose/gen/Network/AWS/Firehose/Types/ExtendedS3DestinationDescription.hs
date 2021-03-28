{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ExtendedS3DestinationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.ExtendedS3DestinationDescription
  ( ExtendedS3DestinationDescription (..)
  -- * Smart constructor
  , mkExtendedS3DestinationDescription
  -- * Lenses
  , esddRoleARN
  , esddBucketARN
  , esddBufferingHints
  , esddCompressionFormat
  , esddEncryptionConfiguration
  , esddCloudWatchLoggingOptions
  , esddDataFormatConversionConfiguration
  , esddErrorOutputPrefix
  , esddPrefix
  , esddProcessingConfiguration
  , esddS3BackupDescription
  , esddS3BackupMode
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
import qualified Network.AWS.Firehose.Types.S3DestinationDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a destination in Amazon S3.
--
-- /See:/ 'mkExtendedS3DestinationDescription' smart constructor.
data ExtendedS3DestinationDescription = ExtendedS3DestinationDescription'
  { roleARN :: Types.RoleARN
    -- ^ The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
  , bucketARN :: Types.BucketARN
    -- ^ The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
  , bufferingHints :: Types.BufferingHints
    -- ^ The buffering option.
  , compressionFormat :: Types.CompressionFormat
    -- ^ The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
  , encryptionConfiguration :: Types.EncryptionConfiguration
    -- ^ The encryption configuration. If no value is specified, the default is no encryption.
  , cloudWatchLoggingOptions :: Core.Maybe Types.CloudWatchLoggingOptions
    -- ^ The Amazon CloudWatch logging options for your delivery stream.
  , dataFormatConversionConfiguration :: Core.Maybe Types.DataFormatConversionConfiguration
    -- ^ The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
  , errorOutputPrefix :: Core.Maybe Types.ErrorOutputPrefix
    -- ^ A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
  , prefix :: Core.Maybe Types.Prefix
    -- ^ The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
  , processingConfiguration :: Core.Maybe Types.ProcessingConfiguration
    -- ^ The data processing configuration.
  , s3BackupDescription :: Core.Maybe Types.S3DestinationDescription
    -- ^ The configuration for backup in Amazon S3.
  , s3BackupMode :: Core.Maybe Types.S3BackupMode
    -- ^ The Amazon S3 backup mode.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExtendedS3DestinationDescription' value with any optional fields omitted.
mkExtendedS3DestinationDescription
    :: Types.RoleARN -- ^ 'roleARN'
    -> Types.BucketARN -- ^ 'bucketARN'
    -> Types.BufferingHints -- ^ 'bufferingHints'
    -> Types.CompressionFormat -- ^ 'compressionFormat'
    -> Types.EncryptionConfiguration -- ^ 'encryptionConfiguration'
    -> ExtendedS3DestinationDescription
mkExtendedS3DestinationDescription roleARN bucketARN bufferingHints
  compressionFormat encryptionConfiguration
  = ExtendedS3DestinationDescription'{roleARN, bucketARN,
                                      bufferingHints, compressionFormat, encryptionConfiguration,
                                      cloudWatchLoggingOptions = Core.Nothing,
                                      dataFormatConversionConfiguration = Core.Nothing,
                                      errorOutputPrefix = Core.Nothing, prefix = Core.Nothing,
                                      processingConfiguration = Core.Nothing,
                                      s3BackupDescription = Core.Nothing,
                                      s3BackupMode = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddRoleARN :: Lens.Lens' ExtendedS3DestinationDescription Types.RoleARN
esddRoleARN = Lens.field @"roleARN"
{-# INLINEABLE esddRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddBucketARN :: Lens.Lens' ExtendedS3DestinationDescription Types.BucketARN
esddBucketARN = Lens.field @"bucketARN"
{-# INLINEABLE esddBucketARN #-}
{-# DEPRECATED bucketARN "Use generic-lens or generic-optics with 'bucketARN' instead"  #-}

-- | The buffering option.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddBufferingHints :: Lens.Lens' ExtendedS3DestinationDescription Types.BufferingHints
esddBufferingHints = Lens.field @"bufferingHints"
{-# INLINEABLE esddBufferingHints #-}
{-# DEPRECATED bufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead"  #-}

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
--
-- /Note:/ Consider using 'compressionFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddCompressionFormat :: Lens.Lens' ExtendedS3DestinationDescription Types.CompressionFormat
esddCompressionFormat = Lens.field @"compressionFormat"
{-# INLINEABLE esddCompressionFormat #-}
{-# DEPRECATED compressionFormat "Use generic-lens or generic-optics with 'compressionFormat' instead"  #-}

-- | The encryption configuration. If no value is specified, the default is no encryption.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddEncryptionConfiguration :: Lens.Lens' ExtendedS3DestinationDescription Types.EncryptionConfiguration
esddEncryptionConfiguration = Lens.field @"encryptionConfiguration"
{-# INLINEABLE esddEncryptionConfiguration #-}
{-# DEPRECATED encryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead"  #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddCloudWatchLoggingOptions :: Lens.Lens' ExtendedS3DestinationDescription (Core.Maybe Types.CloudWatchLoggingOptions)
esddCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# INLINEABLE esddCloudWatchLoggingOptions #-}
{-# DEPRECATED cloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead"  #-}

-- | The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
--
-- /Note:/ Consider using 'dataFormatConversionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddDataFormatConversionConfiguration :: Lens.Lens' ExtendedS3DestinationDescription (Core.Maybe Types.DataFormatConversionConfiguration)
esddDataFormatConversionConfiguration = Lens.field @"dataFormatConversionConfiguration"
{-# INLINEABLE esddDataFormatConversionConfiguration #-}
{-# DEPRECATED dataFormatConversionConfiguration "Use generic-lens or generic-optics with 'dataFormatConversionConfiguration' instead"  #-}

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'errorOutputPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddErrorOutputPrefix :: Lens.Lens' ExtendedS3DestinationDescription (Core.Maybe Types.ErrorOutputPrefix)
esddErrorOutputPrefix = Lens.field @"errorOutputPrefix"
{-# INLINEABLE esddErrorOutputPrefix #-}
{-# DEPRECATED errorOutputPrefix "Use generic-lens or generic-optics with 'errorOutputPrefix' instead"  #-}

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddPrefix :: Lens.Lens' ExtendedS3DestinationDescription (Core.Maybe Types.Prefix)
esddPrefix = Lens.field @"prefix"
{-# INLINEABLE esddPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddProcessingConfiguration :: Lens.Lens' ExtendedS3DestinationDescription (Core.Maybe Types.ProcessingConfiguration)
esddProcessingConfiguration = Lens.field @"processingConfiguration"
{-# INLINEABLE esddProcessingConfiguration #-}
{-# DEPRECATED processingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead"  #-}

-- | The configuration for backup in Amazon S3.
--
-- /Note:/ Consider using 's3BackupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddS3BackupDescription :: Lens.Lens' ExtendedS3DestinationDescription (Core.Maybe Types.S3DestinationDescription)
esddS3BackupDescription = Lens.field @"s3BackupDescription"
{-# INLINEABLE esddS3BackupDescription #-}
{-# DEPRECATED s3BackupDescription "Use generic-lens or generic-optics with 's3BackupDescription' instead"  #-}

-- | The Amazon S3 backup mode.
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esddS3BackupMode :: Lens.Lens' ExtendedS3DestinationDescription (Core.Maybe Types.S3BackupMode)
esddS3BackupMode = Lens.field @"s3BackupMode"
{-# INLINEABLE esddS3BackupMode #-}
{-# DEPRECATED s3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead"  #-}

instance Core.FromJSON ExtendedS3DestinationDescription where
        parseJSON
          = Core.withObject "ExtendedS3DestinationDescription" Core.$
              \ x ->
                ExtendedS3DestinationDescription' Core.<$>
                  (x Core..: "RoleARN") Core.<*> x Core..: "BucketARN" Core.<*>
                    x Core..: "BufferingHints"
                    Core.<*> x Core..: "CompressionFormat"
                    Core.<*> x Core..: "EncryptionConfiguration"
                    Core.<*> x Core..:? "CloudWatchLoggingOptions"
                    Core.<*> x Core..:? "DataFormatConversionConfiguration"
                    Core.<*> x Core..:? "ErrorOutputPrefix"
                    Core.<*> x Core..:? "Prefix"
                    Core.<*> x Core..:? "ProcessingConfiguration"
                    Core.<*> x Core..:? "S3BackupDescription"
                    Core.<*> x Core..:? "S3BackupMode"
