{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SplunkDestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.SplunkDestinationConfiguration
  ( SplunkDestinationConfiguration (..),

    -- * Smart constructor
    mkSplunkDestinationConfiguration,

    -- * Lenses
    sdcfHECEndpoint,
    sdcfHECEndpointType,
    sdcfHECToken,
    sdcfS3Configuration,
    sdcfCloudWatchLoggingOptions,
    sdcfHECAcknowledgmentTimeoutInSeconds,
    sdcfProcessingConfiguration,
    sdcfRetryOptions,
    sdcfS3BackupMode,
  )
where

import qualified Network.AWS.Firehose.Types.CloudWatchLoggingOptions as Types
import qualified Network.AWS.Firehose.Types.HECEndpoint as Types
import qualified Network.AWS.Firehose.Types.HECEndpointType as Types
import qualified Network.AWS.Firehose.Types.HECToken as Types
import qualified Network.AWS.Firehose.Types.ProcessingConfiguration as Types
import qualified Network.AWS.Firehose.Types.S3DestinationConfiguration as Types
import qualified Network.AWS.Firehose.Types.SplunkRetryOptions as Types
import qualified Network.AWS.Firehose.Types.SplunkS3BackupMode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the configuration of a destination in Splunk.
--
-- /See:/ 'mkSplunkDestinationConfiguration' smart constructor.
data SplunkDestinationConfiguration = SplunkDestinationConfiguration'
  { -- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
    hECEndpoint :: Types.HECEndpoint,
    -- | This type can be either "Raw" or "Event."
    hECEndpointType :: Types.HECEndpointType,
    -- | This is a GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
    hECToken :: Types.HECToken,
    -- | The configuration for the backup Amazon S3 location.
    s3Configuration :: Types.S3DestinationConfiguration,
    -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Core.Maybe Types.CloudWatchLoggingOptions,
    -- | The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
    hECAcknowledgmentTimeoutInSeconds :: Core.Maybe Core.Natural,
    -- | The data processing configuration.
    processingConfiguration :: Core.Maybe Types.ProcessingConfiguration,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk, or if it doesn't receive an acknowledgment of receipt from Splunk.
    retryOptions :: Core.Maybe Types.SplunkRetryOptions,
    -- | Defines how documents should be delivered to Amazon S3. When set to @FailedEventsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllEvents@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. The default value is @FailedEventsOnly@ .
    --
    -- You can update this backup mode from @FailedEventsOnly@ to @AllEvents@ . You can't update it from @AllEvents@ to @FailedEventsOnly@ .
    s3BackupMode :: Core.Maybe Types.SplunkS3BackupMode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SplunkDestinationConfiguration' value with any optional fields omitted.
mkSplunkDestinationConfiguration ::
  -- | 'hECEndpoint'
  Types.HECEndpoint ->
  -- | 'hECEndpointType'
  Types.HECEndpointType ->
  -- | 'hECToken'
  Types.HECToken ->
  -- | 's3Configuration'
  Types.S3DestinationConfiguration ->
  SplunkDestinationConfiguration
mkSplunkDestinationConfiguration
  hECEndpoint
  hECEndpointType
  hECToken
  s3Configuration =
    SplunkDestinationConfiguration'
      { hECEndpoint,
        hECEndpointType,
        hECToken,
        s3Configuration,
        cloudWatchLoggingOptions = Core.Nothing,
        hECAcknowledgmentTimeoutInSeconds = Core.Nothing,
        processingConfiguration = Core.Nothing,
        retryOptions = Core.Nothing,
        s3BackupMode = Core.Nothing
      }

-- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
--
-- /Note:/ Consider using 'hECEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcfHECEndpoint :: Lens.Lens' SplunkDestinationConfiguration Types.HECEndpoint
sdcfHECEndpoint = Lens.field @"hECEndpoint"
{-# DEPRECATED sdcfHECEndpoint "Use generic-lens or generic-optics with 'hECEndpoint' instead." #-}

-- | This type can be either "Raw" or "Event."
--
-- /Note:/ Consider using 'hECEndpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcfHECEndpointType :: Lens.Lens' SplunkDestinationConfiguration Types.HECEndpointType
sdcfHECEndpointType = Lens.field @"hECEndpointType"
{-# DEPRECATED sdcfHECEndpointType "Use generic-lens or generic-optics with 'hECEndpointType' instead." #-}

-- | This is a GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
--
-- /Note:/ Consider using 'hECToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcfHECToken :: Lens.Lens' SplunkDestinationConfiguration Types.HECToken
sdcfHECToken = Lens.field @"hECToken"
{-# DEPRECATED sdcfHECToken "Use generic-lens or generic-optics with 'hECToken' instead." #-}

-- | The configuration for the backup Amazon S3 location.
--
-- /Note:/ Consider using 's3Configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcfS3Configuration :: Lens.Lens' SplunkDestinationConfiguration Types.S3DestinationConfiguration
sdcfS3Configuration = Lens.field @"s3Configuration"
{-# DEPRECATED sdcfS3Configuration "Use generic-lens or generic-optics with 's3Configuration' instead." #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcfCloudWatchLoggingOptions :: Lens.Lens' SplunkDestinationConfiguration (Core.Maybe Types.CloudWatchLoggingOptions)
sdcfCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# DEPRECATED sdcfCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
--
-- /Note:/ Consider using 'hECAcknowledgmentTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcfHECAcknowledgmentTimeoutInSeconds :: Lens.Lens' SplunkDestinationConfiguration (Core.Maybe Core.Natural)
sdcfHECAcknowledgmentTimeoutInSeconds = Lens.field @"hECAcknowledgmentTimeoutInSeconds"
{-# DEPRECATED sdcfHECAcknowledgmentTimeoutInSeconds "Use generic-lens or generic-optics with 'hECAcknowledgmentTimeoutInSeconds' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcfProcessingConfiguration :: Lens.Lens' SplunkDestinationConfiguration (Core.Maybe Types.ProcessingConfiguration)
sdcfProcessingConfiguration = Lens.field @"processingConfiguration"
{-# DEPRECATED sdcfProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk, or if it doesn't receive an acknowledgment of receipt from Splunk.
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcfRetryOptions :: Lens.Lens' SplunkDestinationConfiguration (Core.Maybe Types.SplunkRetryOptions)
sdcfRetryOptions = Lens.field @"retryOptions"
{-# DEPRECATED sdcfRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | Defines how documents should be delivered to Amazon S3. When set to @FailedEventsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllEvents@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. The default value is @FailedEventsOnly@ .
--
-- You can update this backup mode from @FailedEventsOnly@ to @AllEvents@ . You can't update it from @AllEvents@ to @FailedEventsOnly@ .
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcfS3BackupMode :: Lens.Lens' SplunkDestinationConfiguration (Core.Maybe Types.SplunkS3BackupMode)
sdcfS3BackupMode = Lens.field @"s3BackupMode"
{-# DEPRECATED sdcfS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

instance Core.FromJSON SplunkDestinationConfiguration where
  toJSON SplunkDestinationConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("HECEndpoint" Core..= hECEndpoint),
            Core.Just ("HECEndpointType" Core..= hECEndpointType),
            Core.Just ("HECToken" Core..= hECToken),
            Core.Just ("S3Configuration" Core..= s3Configuration),
            ("CloudWatchLoggingOptions" Core..=)
              Core.<$> cloudWatchLoggingOptions,
            ("HECAcknowledgmentTimeoutInSeconds" Core..=)
              Core.<$> hECAcknowledgmentTimeoutInSeconds,
            ("ProcessingConfiguration" Core..=)
              Core.<$> processingConfiguration,
            ("RetryOptions" Core..=) Core.<$> retryOptions,
            ("S3BackupMode" Core..=) Core.<$> s3BackupMode
          ]
      )
