{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HttpEndpointDestinationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.HttpEndpointDestinationUpdate
  ( HttpEndpointDestinationUpdate (..)
  -- * Smart constructor
  , mkHttpEndpointDestinationUpdate
  -- * Lenses
  , heduBufferingHints
  , heduCloudWatchLoggingOptions
  , heduEndpointConfiguration
  , heduProcessingConfiguration
  , heduRequestConfiguration
  , heduRetryOptions
  , heduRoleARN
  , heduS3BackupMode
  , heduS3Update
  ) where

import qualified Network.AWS.Firehose.Types.CloudWatchLoggingOptions as Types
import qualified Network.AWS.Firehose.Types.HttpEndpointBufferingHints as Types
import qualified Network.AWS.Firehose.Types.HttpEndpointConfiguration as Types
import qualified Network.AWS.Firehose.Types.HttpEndpointRequestConfiguration as Types
import qualified Network.AWS.Firehose.Types.HttpEndpointRetryOptions as Types
import qualified Network.AWS.Firehose.Types.HttpEndpointS3BackupMode as Types
import qualified Network.AWS.Firehose.Types.ProcessingConfiguration as Types
import qualified Network.AWS.Firehose.Types.RoleARN as Types
import qualified Network.AWS.Firehose.Types.S3DestinationUpdate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Updates the specified HTTP endpoint destination.
--
-- /See:/ 'mkHttpEndpointDestinationUpdate' smart constructor.
data HttpEndpointDestinationUpdate = HttpEndpointDestinationUpdate'
  { bufferingHints :: Core.Maybe Types.HttpEndpointBufferingHints
    -- ^ Describes buffering options that can be applied to the data before it is delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other. 
  , cloudWatchLoggingOptions :: Core.Maybe Types.CloudWatchLoggingOptions
  , endpointConfiguration :: Core.Maybe Types.HttpEndpointConfiguration
    -- ^ Describes the configuration of the HTTP endpoint destination.
  , processingConfiguration :: Core.Maybe Types.ProcessingConfiguration
  , requestConfiguration :: Core.Maybe Types.HttpEndpointRequestConfiguration
    -- ^ The configuration of the request sent to the HTTP endpoint specified as the destination.
  , retryOptions :: Core.Maybe Types.HttpEndpointRetryOptions
    -- ^ Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
  , roleARN :: Core.Maybe Types.RoleARN
    -- ^ Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
  , s3BackupMode :: Core.Maybe Types.HttpEndpointS3BackupMode
    -- ^ Describes the S3 bucket backup options for the data that Kinesis Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
  , s3Update :: Core.Maybe Types.S3DestinationUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HttpEndpointDestinationUpdate' value with any optional fields omitted.
mkHttpEndpointDestinationUpdate
    :: HttpEndpointDestinationUpdate
mkHttpEndpointDestinationUpdate
  = HttpEndpointDestinationUpdate'{bufferingHints = Core.Nothing,
                                   cloudWatchLoggingOptions = Core.Nothing,
                                   endpointConfiguration = Core.Nothing,
                                   processingConfiguration = Core.Nothing,
                                   requestConfiguration = Core.Nothing, retryOptions = Core.Nothing,
                                   roleARN = Core.Nothing, s3BackupMode = Core.Nothing,
                                   s3Update = Core.Nothing}

-- | Describes buffering options that can be applied to the data before it is delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other. 
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heduBufferingHints :: Lens.Lens' HttpEndpointDestinationUpdate (Core.Maybe Types.HttpEndpointBufferingHints)
heduBufferingHints = Lens.field @"bufferingHints"
{-# INLINEABLE heduBufferingHints #-}
{-# DEPRECATED bufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heduCloudWatchLoggingOptions :: Lens.Lens' HttpEndpointDestinationUpdate (Core.Maybe Types.CloudWatchLoggingOptions)
heduCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# INLINEABLE heduCloudWatchLoggingOptions #-}
{-# DEPRECATED cloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead"  #-}

-- | Describes the configuration of the HTTP endpoint destination.
--
-- /Note:/ Consider using 'endpointConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heduEndpointConfiguration :: Lens.Lens' HttpEndpointDestinationUpdate (Core.Maybe Types.HttpEndpointConfiguration)
heduEndpointConfiguration = Lens.field @"endpointConfiguration"
{-# INLINEABLE heduEndpointConfiguration #-}
{-# DEPRECATED endpointConfiguration "Use generic-lens or generic-optics with 'endpointConfiguration' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heduProcessingConfiguration :: Lens.Lens' HttpEndpointDestinationUpdate (Core.Maybe Types.ProcessingConfiguration)
heduProcessingConfiguration = Lens.field @"processingConfiguration"
{-# INLINEABLE heduProcessingConfiguration #-}
{-# DEPRECATED processingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead"  #-}

-- | The configuration of the request sent to the HTTP endpoint specified as the destination.
--
-- /Note:/ Consider using 'requestConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heduRequestConfiguration :: Lens.Lens' HttpEndpointDestinationUpdate (Core.Maybe Types.HttpEndpointRequestConfiguration)
heduRequestConfiguration = Lens.field @"requestConfiguration"
{-# INLINEABLE heduRequestConfiguration #-}
{-# DEPRECATED requestConfiguration "Use generic-lens or generic-optics with 'requestConfiguration' instead"  #-}

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heduRetryOptions :: Lens.Lens' HttpEndpointDestinationUpdate (Core.Maybe Types.HttpEndpointRetryOptions)
heduRetryOptions = Lens.field @"retryOptions"
{-# INLINEABLE heduRetryOptions #-}
{-# DEPRECATED retryOptions "Use generic-lens or generic-optics with 'retryOptions' instead"  #-}

-- | Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heduRoleARN :: Lens.Lens' HttpEndpointDestinationUpdate (Core.Maybe Types.RoleARN)
heduRoleARN = Lens.field @"roleARN"
{-# INLINEABLE heduRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | Describes the S3 bucket backup options for the data that Kinesis Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heduS3BackupMode :: Lens.Lens' HttpEndpointDestinationUpdate (Core.Maybe Types.HttpEndpointS3BackupMode)
heduS3BackupMode = Lens.field @"s3BackupMode"
{-# INLINEABLE heduS3BackupMode #-}
{-# DEPRECATED s3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Update' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heduS3Update :: Lens.Lens' HttpEndpointDestinationUpdate (Core.Maybe Types.S3DestinationUpdate)
heduS3Update = Lens.field @"s3Update"
{-# INLINEABLE heduS3Update #-}
{-# DEPRECATED s3Update "Use generic-lens or generic-optics with 's3Update' instead"  #-}

instance Core.FromJSON HttpEndpointDestinationUpdate where
        toJSON HttpEndpointDestinationUpdate{..}
          = Core.object
              (Core.catMaybes
                 [("BufferingHints" Core..=) Core.<$> bufferingHints,
                  ("CloudWatchLoggingOptions" Core..=) Core.<$>
                    cloudWatchLoggingOptions,
                  ("EndpointConfiguration" Core..=) Core.<$> endpointConfiguration,
                  ("ProcessingConfiguration" Core..=) Core.<$>
                    processingConfiguration,
                  ("RequestConfiguration" Core..=) Core.<$> requestConfiguration,
                  ("RetryOptions" Core..=) Core.<$> retryOptions,
                  ("RoleARN" Core..=) Core.<$> roleARN,
                  ("S3BackupMode" Core..=) Core.<$> s3BackupMode,
                  ("S3Update" Core..=) Core.<$> s3Update])
