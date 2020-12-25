{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ExtendedS3DestinationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ExtendedS3DestinationUpdate
  ( ExtendedS3DestinationUpdate (..),

    -- * Smart constructor
    mkExtendedS3DestinationUpdate,

    -- * Lenses
    esduBucketARN,
    esduBufferingHints,
    esduCloudWatchLoggingOptions,
    esduCompressionFormat,
    esduDataFormatConversionConfiguration,
    esduEncryptionConfiguration,
    esduErrorOutputPrefix,
    esduPrefix,
    esduProcessingConfiguration,
    esduRoleARN,
    esduS3BackupMode,
    esduS3BackupUpdate,
  )
where

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
import qualified Network.AWS.Firehose.Types.S3DestinationUpdate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an update for a destination in Amazon S3.
--
-- /See:/ 'mkExtendedS3DestinationUpdate' smart constructor.
data ExtendedS3DestinationUpdate = ExtendedS3DestinationUpdate'
  { -- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    bucketARN :: Core.Maybe Types.BucketARN,
    -- | The buffering option.
    bufferingHints :: Core.Maybe Types.BufferingHints,
    -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Core.Maybe Types.CloudWatchLoggingOptions,
    -- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
    compressionFormat :: Core.Maybe Types.CompressionFormat,
    -- | The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
    dataFormatConversionConfiguration :: Core.Maybe Types.DataFormatConversionConfiguration,
    -- | The encryption configuration. If no value is specified, the default is no encryption.
    encryptionConfiguration :: Core.Maybe Types.EncryptionConfiguration,
    -- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
    errorOutputPrefix :: Core.Maybe Types.ErrorOutputPrefix,
    -- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
    prefix :: Core.Maybe Types.Prefix,
    -- | The data processing configuration.
    processingConfiguration :: Core.Maybe Types.ProcessingConfiguration,
    -- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    roleARN :: Core.Maybe Types.RoleARN,
    -- | You can update a delivery stream to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it.
    s3BackupMode :: Core.Maybe Types.S3BackupMode,
    -- | The Amazon S3 destination for backup.
    s3BackupUpdate :: Core.Maybe Types.S3DestinationUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExtendedS3DestinationUpdate' value with any optional fields omitted.
mkExtendedS3DestinationUpdate ::
  ExtendedS3DestinationUpdate
mkExtendedS3DestinationUpdate =
  ExtendedS3DestinationUpdate'
    { bucketARN = Core.Nothing,
      bufferingHints = Core.Nothing,
      cloudWatchLoggingOptions = Core.Nothing,
      compressionFormat = Core.Nothing,
      dataFormatConversionConfiguration = Core.Nothing,
      encryptionConfiguration = Core.Nothing,
      errorOutputPrefix = Core.Nothing,
      prefix = Core.Nothing,
      processingConfiguration = Core.Nothing,
      roleARN = Core.Nothing,
      s3BackupMode = Core.Nothing,
      s3BackupUpdate = Core.Nothing
    }

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduBucketARN :: Lens.Lens' ExtendedS3DestinationUpdate (Core.Maybe Types.BucketARN)
esduBucketARN = Lens.field @"bucketARN"
{-# DEPRECATED esduBucketARN "Use generic-lens or generic-optics with 'bucketARN' instead." #-}

-- | The buffering option.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduBufferingHints :: Lens.Lens' ExtendedS3DestinationUpdate (Core.Maybe Types.BufferingHints)
esduBufferingHints = Lens.field @"bufferingHints"
{-# DEPRECATED esduBufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead." #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduCloudWatchLoggingOptions :: Lens.Lens' ExtendedS3DestinationUpdate (Core.Maybe Types.CloudWatchLoggingOptions)
esduCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# DEPRECATED esduCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
--
-- /Note:/ Consider using 'compressionFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduCompressionFormat :: Lens.Lens' ExtendedS3DestinationUpdate (Core.Maybe Types.CompressionFormat)
esduCompressionFormat = Lens.field @"compressionFormat"
{-# DEPRECATED esduCompressionFormat "Use generic-lens or generic-optics with 'compressionFormat' instead." #-}

-- | The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
--
-- /Note:/ Consider using 'dataFormatConversionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduDataFormatConversionConfiguration :: Lens.Lens' ExtendedS3DestinationUpdate (Core.Maybe Types.DataFormatConversionConfiguration)
esduDataFormatConversionConfiguration = Lens.field @"dataFormatConversionConfiguration"
{-# DEPRECATED esduDataFormatConversionConfiguration "Use generic-lens or generic-optics with 'dataFormatConversionConfiguration' instead." #-}

-- | The encryption configuration. If no value is specified, the default is no encryption.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduEncryptionConfiguration :: Lens.Lens' ExtendedS3DestinationUpdate (Core.Maybe Types.EncryptionConfiguration)
esduEncryptionConfiguration = Lens.field @"encryptionConfiguration"
{-# DEPRECATED esduEncryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead." #-}

-- | A prefix that Kinesis Data Firehose evaluates and adds to failed records before writing them to S3. This prefix appears immediately following the bucket name. For information about how to specify this prefix, see <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'errorOutputPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduErrorOutputPrefix :: Lens.Lens' ExtendedS3DestinationUpdate (Core.Maybe Types.ErrorOutputPrefix)
esduErrorOutputPrefix = Lens.field @"errorOutputPrefix"
{-# DEPRECATED esduErrorOutputPrefix "Use generic-lens or generic-optics with 'errorOutputPrefix' instead." #-}

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can also specify a custom prefix, as described in <https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html Custom Prefixes for Amazon S3 Objects> .
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduPrefix :: Lens.Lens' ExtendedS3DestinationUpdate (Core.Maybe Types.Prefix)
esduPrefix = Lens.field @"prefix"
{-# DEPRECATED esduPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduProcessingConfiguration :: Lens.Lens' ExtendedS3DestinationUpdate (Core.Maybe Types.ProcessingConfiguration)
esduProcessingConfiguration = Lens.field @"processingConfiguration"
{-# DEPRECATED esduProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduRoleARN :: Lens.Lens' ExtendedS3DestinationUpdate (Core.Maybe Types.RoleARN)
esduRoleARN = Lens.field @"roleARN"
{-# DEPRECATED esduRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | You can update a delivery stream to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it.
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduS3BackupMode :: Lens.Lens' ExtendedS3DestinationUpdate (Core.Maybe Types.S3BackupMode)
esduS3BackupMode = Lens.field @"s3BackupMode"
{-# DEPRECATED esduS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | The Amazon S3 destination for backup.
--
-- /Note:/ Consider using 's3BackupUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esduS3BackupUpdate :: Lens.Lens' ExtendedS3DestinationUpdate (Core.Maybe Types.S3DestinationUpdate)
esduS3BackupUpdate = Lens.field @"s3BackupUpdate"
{-# DEPRECATED esduS3BackupUpdate "Use generic-lens or generic-optics with 's3BackupUpdate' instead." #-}

instance Core.FromJSON ExtendedS3DestinationUpdate where
  toJSON ExtendedS3DestinationUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ ("BucketARN" Core..=) Core.<$> bucketARN,
            ("BufferingHints" Core..=) Core.<$> bufferingHints,
            ("CloudWatchLoggingOptions" Core..=)
              Core.<$> cloudWatchLoggingOptions,
            ("CompressionFormat" Core..=) Core.<$> compressionFormat,
            ("DataFormatConversionConfiguration" Core..=)
              Core.<$> dataFormatConversionConfiguration,
            ("EncryptionConfiguration" Core..=)
              Core.<$> encryptionConfiguration,
            ("ErrorOutputPrefix" Core..=) Core.<$> errorOutputPrefix,
            ("Prefix" Core..=) Core.<$> prefix,
            ("ProcessingConfiguration" Core..=)
              Core.<$> processingConfiguration,
            ("RoleARN" Core..=) Core.<$> roleARN,
            ("S3BackupMode" Core..=) Core.<$> s3BackupMode,
            ("S3BackupUpdate" Core..=) Core.<$> s3BackupUpdate
          ]
      )
