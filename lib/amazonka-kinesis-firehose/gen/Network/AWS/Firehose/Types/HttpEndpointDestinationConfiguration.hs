{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HttpEndpointDestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.HttpEndpointDestinationConfiguration
  ( HttpEndpointDestinationConfiguration (..)
  -- * Smart constructor
  , mkHttpEndpointDestinationConfiguration
  -- * Lenses
  , hedcEndpointConfiguration
  , hedcS3Configuration
  , hedcBufferingHints
  , hedcCloudWatchLoggingOptions
  , hedcProcessingConfiguration
  , hedcRequestConfiguration
  , hedcRetryOptions
  , hedcRoleARN
  , hedcS3BackupMode
  ) where

import qualified Network.AWS.Firehose.Types.CloudWatchLoggingOptions as Types
import qualified Network.AWS.Firehose.Types.HttpEndpointBufferingHints as Types
import qualified Network.AWS.Firehose.Types.HttpEndpointConfiguration as Types
import qualified Network.AWS.Firehose.Types.HttpEndpointRequestConfiguration as Types
import qualified Network.AWS.Firehose.Types.HttpEndpointRetryOptions as Types
import qualified Network.AWS.Firehose.Types.HttpEndpointS3BackupMode as Types
import qualified Network.AWS.Firehose.Types.ProcessingConfiguration as Types
import qualified Network.AWS.Firehose.Types.RoleARN as Types
import qualified Network.AWS.Firehose.Types.S3DestinationConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the configuration of the HTTP endpoint destination.
--
-- /See:/ 'mkHttpEndpointDestinationConfiguration' smart constructor.
data HttpEndpointDestinationConfiguration = HttpEndpointDestinationConfiguration'
  { endpointConfiguration :: Types.HttpEndpointConfiguration
    -- ^ The configuration of the HTTP endpoint selected as the destination.
  , s3Configuration :: Types.S3DestinationConfiguration
  , bufferingHints :: Core.Maybe Types.HttpEndpointBufferingHints
    -- ^ The buffering options that can be used before data is delivered to the specified destination. Kinesis Data Firehose treats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if you specify a value for one of them, you must also provide a value for the other. 
  , cloudWatchLoggingOptions :: Core.Maybe Types.CloudWatchLoggingOptions
  , processingConfiguration :: Core.Maybe Types.ProcessingConfiguration
  , requestConfiguration :: Core.Maybe Types.HttpEndpointRequestConfiguration
    -- ^ The configuration of the requeste sent to the HTTP endpoint specified as the destination.
  , retryOptions :: Core.Maybe Types.HttpEndpointRetryOptions
    -- ^ Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
  , roleARN :: Core.Maybe Types.RoleARN
    -- ^ Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
  , s3BackupMode :: Core.Maybe Types.HttpEndpointS3BackupMode
    -- ^ Describes the S3 bucket backup options for the data that Kinesis Data Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HttpEndpointDestinationConfiguration' value with any optional fields omitted.
mkHttpEndpointDestinationConfiguration
    :: Types.HttpEndpointConfiguration -- ^ 'endpointConfiguration'
    -> Types.S3DestinationConfiguration -- ^ 's3Configuration'
    -> HttpEndpointDestinationConfiguration
mkHttpEndpointDestinationConfiguration endpointConfiguration
  s3Configuration
  = HttpEndpointDestinationConfiguration'{endpointConfiguration,
                                          s3Configuration, bufferingHints = Core.Nothing,
                                          cloudWatchLoggingOptions = Core.Nothing,
                                          processingConfiguration = Core.Nothing,
                                          requestConfiguration = Core.Nothing,
                                          retryOptions = Core.Nothing, roleARN = Core.Nothing,
                                          s3BackupMode = Core.Nothing}

-- | The configuration of the HTTP endpoint selected as the destination.
--
-- /Note:/ Consider using 'endpointConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hedcEndpointConfiguration :: Lens.Lens' HttpEndpointDestinationConfiguration Types.HttpEndpointConfiguration
hedcEndpointConfiguration = Lens.field @"endpointConfiguration"
{-# INLINEABLE hedcEndpointConfiguration #-}
{-# DEPRECATED endpointConfiguration "Use generic-lens or generic-optics with 'endpointConfiguration' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hedcS3Configuration :: Lens.Lens' HttpEndpointDestinationConfiguration Types.S3DestinationConfiguration
hedcS3Configuration = Lens.field @"s3Configuration"
{-# INLINEABLE hedcS3Configuration #-}
{-# DEPRECATED s3Configuration "Use generic-lens or generic-optics with 's3Configuration' instead"  #-}

-- | The buffering options that can be used before data is delivered to the specified destination. Kinesis Data Firehose treats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if you specify a value for one of them, you must also provide a value for the other. 
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hedcBufferingHints :: Lens.Lens' HttpEndpointDestinationConfiguration (Core.Maybe Types.HttpEndpointBufferingHints)
hedcBufferingHints = Lens.field @"bufferingHints"
{-# INLINEABLE hedcBufferingHints #-}
{-# DEPRECATED bufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hedcCloudWatchLoggingOptions :: Lens.Lens' HttpEndpointDestinationConfiguration (Core.Maybe Types.CloudWatchLoggingOptions)
hedcCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# INLINEABLE hedcCloudWatchLoggingOptions #-}
{-# DEPRECATED cloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hedcProcessingConfiguration :: Lens.Lens' HttpEndpointDestinationConfiguration (Core.Maybe Types.ProcessingConfiguration)
hedcProcessingConfiguration = Lens.field @"processingConfiguration"
{-# INLINEABLE hedcProcessingConfiguration #-}
{-# DEPRECATED processingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead"  #-}

-- | The configuration of the requeste sent to the HTTP endpoint specified as the destination.
--
-- /Note:/ Consider using 'requestConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hedcRequestConfiguration :: Lens.Lens' HttpEndpointDestinationConfiguration (Core.Maybe Types.HttpEndpointRequestConfiguration)
hedcRequestConfiguration = Lens.field @"requestConfiguration"
{-# INLINEABLE hedcRequestConfiguration #-}
{-# DEPRECATED requestConfiguration "Use generic-lens or generic-optics with 'requestConfiguration' instead"  #-}

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hedcRetryOptions :: Lens.Lens' HttpEndpointDestinationConfiguration (Core.Maybe Types.HttpEndpointRetryOptions)
hedcRetryOptions = Lens.field @"retryOptions"
{-# INLINEABLE hedcRetryOptions #-}
{-# DEPRECATED retryOptions "Use generic-lens or generic-optics with 'retryOptions' instead"  #-}

-- | Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hedcRoleARN :: Lens.Lens' HttpEndpointDestinationConfiguration (Core.Maybe Types.RoleARN)
hedcRoleARN = Lens.field @"roleARN"
{-# INLINEABLE hedcRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | Describes the S3 bucket backup options for the data that Kinesis Data Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hedcS3BackupMode :: Lens.Lens' HttpEndpointDestinationConfiguration (Core.Maybe Types.HttpEndpointS3BackupMode)
hedcS3BackupMode = Lens.field @"s3BackupMode"
{-# INLINEABLE hedcS3BackupMode #-}
{-# DEPRECATED s3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead"  #-}

instance Core.FromJSON HttpEndpointDestinationConfiguration where
        toJSON HttpEndpointDestinationConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EndpointConfiguration" Core..= endpointConfiguration),
                  Core.Just ("S3Configuration" Core..= s3Configuration),
                  ("BufferingHints" Core..=) Core.<$> bufferingHints,
                  ("CloudWatchLoggingOptions" Core..=) Core.<$>
                    cloudWatchLoggingOptions,
                  ("ProcessingConfiguration" Core..=) Core.<$>
                    processingConfiguration,
                  ("RequestConfiguration" Core..=) Core.<$> requestConfiguration,
                  ("RetryOptions" Core..=) Core.<$> retryOptions,
                  ("RoleARN" Core..=) Core.<$> roleARN,
                  ("S3BackupMode" Core..=) Core.<$> s3BackupMode])
