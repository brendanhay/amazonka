{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HttpEndpointDestinationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.HttpEndpointDestinationDescription
  ( HttpEndpointDestinationDescription (..)
  -- * Smart constructor
  , mkHttpEndpointDestinationDescription
  -- * Lenses
  , heddBufferingHints
  , heddCloudWatchLoggingOptions
  , heddEndpointConfiguration
  , heddProcessingConfiguration
  , heddRequestConfiguration
  , heddRetryOptions
  , heddRoleARN
  , heddS3BackupMode
  , heddS3DestinationDescription
  ) where

import qualified Network.AWS.Firehose.Types.CloudWatchLoggingOptions as Types
import qualified Network.AWS.Firehose.Types.HttpEndpointBufferingHints as Types
import qualified Network.AWS.Firehose.Types.HttpEndpointDescription as Types
import qualified Network.AWS.Firehose.Types.HttpEndpointRequestConfiguration as Types
import qualified Network.AWS.Firehose.Types.HttpEndpointRetryOptions as Types
import qualified Network.AWS.Firehose.Types.HttpEndpointS3BackupMode as Types
import qualified Network.AWS.Firehose.Types.ProcessingConfiguration as Types
import qualified Network.AWS.Firehose.Types.RoleARN as Types
import qualified Network.AWS.Firehose.Types.S3DestinationDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the HTTP endpoint destination.
--
-- /See:/ 'mkHttpEndpointDestinationDescription' smart constructor.
data HttpEndpointDestinationDescription = HttpEndpointDestinationDescription'
  { bufferingHints :: Core.Maybe Types.HttpEndpointBufferingHints
    -- ^ Describes buffering options that can be applied to the data before it is delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other. 
  , cloudWatchLoggingOptions :: Core.Maybe Types.CloudWatchLoggingOptions
  , endpointConfiguration :: Core.Maybe Types.HttpEndpointDescription
    -- ^ The configuration of the specified HTTP endpoint destination.
  , processingConfiguration :: Core.Maybe Types.ProcessingConfiguration
  , requestConfiguration :: Core.Maybe Types.HttpEndpointRequestConfiguration
    -- ^ The configuration of request sent to the HTTP endpoint specified as the destination.
  , retryOptions :: Core.Maybe Types.HttpEndpointRetryOptions
    -- ^ Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
  , roleARN :: Core.Maybe Types.RoleARN
    -- ^ Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
  , s3BackupMode :: Core.Maybe Types.HttpEndpointS3BackupMode
    -- ^ Describes the S3 bucket backup options for the data that Kinesis Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
  , s3DestinationDescription :: Core.Maybe Types.S3DestinationDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HttpEndpointDestinationDescription' value with any optional fields omitted.
mkHttpEndpointDestinationDescription
    :: HttpEndpointDestinationDescription
mkHttpEndpointDestinationDescription
  = HttpEndpointDestinationDescription'{bufferingHints =
                                          Core.Nothing,
                                        cloudWatchLoggingOptions = Core.Nothing,
                                        endpointConfiguration = Core.Nothing,
                                        processingConfiguration = Core.Nothing,
                                        requestConfiguration = Core.Nothing,
                                        retryOptions = Core.Nothing, roleARN = Core.Nothing,
                                        s3BackupMode = Core.Nothing,
                                        s3DestinationDescription = Core.Nothing}

-- | Describes buffering options that can be applied to the data before it is delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other. 
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heddBufferingHints :: Lens.Lens' HttpEndpointDestinationDescription (Core.Maybe Types.HttpEndpointBufferingHints)
heddBufferingHints = Lens.field @"bufferingHints"
{-# INLINEABLE heddBufferingHints #-}
{-# DEPRECATED bufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heddCloudWatchLoggingOptions :: Lens.Lens' HttpEndpointDestinationDescription (Core.Maybe Types.CloudWatchLoggingOptions)
heddCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# INLINEABLE heddCloudWatchLoggingOptions #-}
{-# DEPRECATED cloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead"  #-}

-- | The configuration of the specified HTTP endpoint destination.
--
-- /Note:/ Consider using 'endpointConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heddEndpointConfiguration :: Lens.Lens' HttpEndpointDestinationDescription (Core.Maybe Types.HttpEndpointDescription)
heddEndpointConfiguration = Lens.field @"endpointConfiguration"
{-# INLINEABLE heddEndpointConfiguration #-}
{-# DEPRECATED endpointConfiguration "Use generic-lens or generic-optics with 'endpointConfiguration' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heddProcessingConfiguration :: Lens.Lens' HttpEndpointDestinationDescription (Core.Maybe Types.ProcessingConfiguration)
heddProcessingConfiguration = Lens.field @"processingConfiguration"
{-# INLINEABLE heddProcessingConfiguration #-}
{-# DEPRECATED processingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead"  #-}

-- | The configuration of request sent to the HTTP endpoint specified as the destination.
--
-- /Note:/ Consider using 'requestConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heddRequestConfiguration :: Lens.Lens' HttpEndpointDestinationDescription (Core.Maybe Types.HttpEndpointRequestConfiguration)
heddRequestConfiguration = Lens.field @"requestConfiguration"
{-# INLINEABLE heddRequestConfiguration #-}
{-# DEPRECATED requestConfiguration "Use generic-lens or generic-optics with 'requestConfiguration' instead"  #-}

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heddRetryOptions :: Lens.Lens' HttpEndpointDestinationDescription (Core.Maybe Types.HttpEndpointRetryOptions)
heddRetryOptions = Lens.field @"retryOptions"
{-# INLINEABLE heddRetryOptions #-}
{-# DEPRECATED retryOptions "Use generic-lens or generic-optics with 'retryOptions' instead"  #-}

-- | Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heddRoleARN :: Lens.Lens' HttpEndpointDestinationDescription (Core.Maybe Types.RoleARN)
heddRoleARN = Lens.field @"roleARN"
{-# INLINEABLE heddRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | Describes the S3 bucket backup options for the data that Kinesis Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heddS3BackupMode :: Lens.Lens' HttpEndpointDestinationDescription (Core.Maybe Types.HttpEndpointS3BackupMode)
heddS3BackupMode = Lens.field @"s3BackupMode"
{-# INLINEABLE heddS3BackupMode #-}
{-# DEPRECATED s3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3DestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heddS3DestinationDescription :: Lens.Lens' HttpEndpointDestinationDescription (Core.Maybe Types.S3DestinationDescription)
heddS3DestinationDescription = Lens.field @"s3DestinationDescription"
{-# INLINEABLE heddS3DestinationDescription #-}
{-# DEPRECATED s3DestinationDescription "Use generic-lens or generic-optics with 's3DestinationDescription' instead"  #-}

instance Core.FromJSON HttpEndpointDestinationDescription where
        parseJSON
          = Core.withObject "HttpEndpointDestinationDescription" Core.$
              \ x ->
                HttpEndpointDestinationDescription' Core.<$>
                  (x Core..:? "BufferingHints") Core.<*>
                    x Core..:? "CloudWatchLoggingOptions"
                    Core.<*> x Core..:? "EndpointConfiguration"
                    Core.<*> x Core..:? "ProcessingConfiguration"
                    Core.<*> x Core..:? "RequestConfiguration"
                    Core.<*> x Core..:? "RetryOptions"
                    Core.<*> x Core..:? "RoleARN"
                    Core.<*> x Core..:? "S3BackupMode"
                    Core.<*> x Core..:? "S3DestinationDescription"
