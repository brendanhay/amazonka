{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SplunkDestinationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.SplunkDestinationUpdate
  ( SplunkDestinationUpdate (..),

    -- * Smart constructor
    mkSplunkDestinationUpdate,

    -- * Lenses
    sduCloudWatchLoggingOptions,
    sduHECAcknowledgmentTimeoutInSeconds,
    sduHECEndpoint,
    sduHECEndpointType,
    sduHECToken,
    sduProcessingConfiguration,
    sduRetryOptions,
    sduS3BackupMode,
    sduS3Update,
  )
where

import qualified Network.AWS.Firehose.Types.CloudWatchLoggingOptions as Types
import qualified Network.AWS.Firehose.Types.HECEndpoint as Types
import qualified Network.AWS.Firehose.Types.HECEndpointType as Types
import qualified Network.AWS.Firehose.Types.HECToken as Types
import qualified Network.AWS.Firehose.Types.ProcessingConfiguration as Types
import qualified Network.AWS.Firehose.Types.S3DestinationUpdate as Types
import qualified Network.AWS.Firehose.Types.SplunkRetryOptions as Types
import qualified Network.AWS.Firehose.Types.SplunkS3BackupMode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an update for a destination in Splunk.
--
-- /See:/ 'mkSplunkDestinationUpdate' smart constructor.
data SplunkDestinationUpdate = SplunkDestinationUpdate'
  { -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Core.Maybe Types.CloudWatchLoggingOptions,
    -- | The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
    hECAcknowledgmentTimeoutInSeconds :: Core.Maybe Core.Natural,
    -- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
    hECEndpoint :: Core.Maybe Types.HECEndpoint,
    -- | This type can be either "Raw" or "Event."
    hECEndpointType :: Core.Maybe Types.HECEndpointType,
    -- | A GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
    hECToken :: Core.Maybe Types.HECToken,
    -- | The data processing configuration.
    processingConfiguration :: Core.Maybe Types.ProcessingConfiguration,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk or if it doesn't receive an acknowledgment of receipt from Splunk.
    retryOptions :: Core.Maybe Types.SplunkRetryOptions,
    -- | Specifies how you want Kinesis Data Firehose to back up documents to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllEvents@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. The default value is @FailedEventsOnly@ .
    --
    -- You can update this backup mode from @FailedEventsOnly@ to @AllEvents@ . You can't update it from @AllEvents@ to @FailedEventsOnly@ .
    s3BackupMode :: Core.Maybe Types.SplunkS3BackupMode,
    -- | Your update to the configuration of the backup Amazon S3 location.
    s3Update :: Core.Maybe Types.S3DestinationUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SplunkDestinationUpdate' value with any optional fields omitted.
mkSplunkDestinationUpdate ::
  SplunkDestinationUpdate
mkSplunkDestinationUpdate =
  SplunkDestinationUpdate'
    { cloudWatchLoggingOptions = Core.Nothing,
      hECAcknowledgmentTimeoutInSeconds = Core.Nothing,
      hECEndpoint = Core.Nothing,
      hECEndpointType = Core.Nothing,
      hECToken = Core.Nothing,
      processingConfiguration = Core.Nothing,
      retryOptions = Core.Nothing,
      s3BackupMode = Core.Nothing,
      s3Update = Core.Nothing
    }

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sduCloudWatchLoggingOptions :: Lens.Lens' SplunkDestinationUpdate (Core.Maybe Types.CloudWatchLoggingOptions)
sduCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# DEPRECATED sduCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
--
-- /Note:/ Consider using 'hECAcknowledgmentTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sduHECAcknowledgmentTimeoutInSeconds :: Lens.Lens' SplunkDestinationUpdate (Core.Maybe Core.Natural)
sduHECAcknowledgmentTimeoutInSeconds = Lens.field @"hECAcknowledgmentTimeoutInSeconds"
{-# DEPRECATED sduHECAcknowledgmentTimeoutInSeconds "Use generic-lens or generic-optics with 'hECAcknowledgmentTimeoutInSeconds' instead." #-}

-- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
--
-- /Note:/ Consider using 'hECEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sduHECEndpoint :: Lens.Lens' SplunkDestinationUpdate (Core.Maybe Types.HECEndpoint)
sduHECEndpoint = Lens.field @"hECEndpoint"
{-# DEPRECATED sduHECEndpoint "Use generic-lens or generic-optics with 'hECEndpoint' instead." #-}

-- | This type can be either "Raw" or "Event."
--
-- /Note:/ Consider using 'hECEndpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sduHECEndpointType :: Lens.Lens' SplunkDestinationUpdate (Core.Maybe Types.HECEndpointType)
sduHECEndpointType = Lens.field @"hECEndpointType"
{-# DEPRECATED sduHECEndpointType "Use generic-lens or generic-optics with 'hECEndpointType' instead." #-}

-- | A GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
--
-- /Note:/ Consider using 'hECToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sduHECToken :: Lens.Lens' SplunkDestinationUpdate (Core.Maybe Types.HECToken)
sduHECToken = Lens.field @"hECToken"
{-# DEPRECATED sduHECToken "Use generic-lens or generic-optics with 'hECToken' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sduProcessingConfiguration :: Lens.Lens' SplunkDestinationUpdate (Core.Maybe Types.ProcessingConfiguration)
sduProcessingConfiguration = Lens.field @"processingConfiguration"
{-# DEPRECATED sduProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk or if it doesn't receive an acknowledgment of receipt from Splunk.
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sduRetryOptions :: Lens.Lens' SplunkDestinationUpdate (Core.Maybe Types.SplunkRetryOptions)
sduRetryOptions = Lens.field @"retryOptions"
{-# DEPRECATED sduRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | Specifies how you want Kinesis Data Firehose to back up documents to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllEvents@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. The default value is @FailedEventsOnly@ .
--
-- You can update this backup mode from @FailedEventsOnly@ to @AllEvents@ . You can't update it from @AllEvents@ to @FailedEventsOnly@ .
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sduS3BackupMode :: Lens.Lens' SplunkDestinationUpdate (Core.Maybe Types.SplunkS3BackupMode)
sduS3BackupMode = Lens.field @"s3BackupMode"
{-# DEPRECATED sduS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | Your update to the configuration of the backup Amazon S3 location.
--
-- /Note:/ Consider using 's3Update' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sduS3Update :: Lens.Lens' SplunkDestinationUpdate (Core.Maybe Types.S3DestinationUpdate)
sduS3Update = Lens.field @"s3Update"
{-# DEPRECATED sduS3Update "Use generic-lens or generic-optics with 's3Update' instead." #-}

instance Core.FromJSON SplunkDestinationUpdate where
  toJSON SplunkDestinationUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ ("CloudWatchLoggingOptions" Core..=)
              Core.<$> cloudWatchLoggingOptions,
            ("HECAcknowledgmentTimeoutInSeconds" Core..=)
              Core.<$> hECAcknowledgmentTimeoutInSeconds,
            ("HECEndpoint" Core..=) Core.<$> hECEndpoint,
            ("HECEndpointType" Core..=) Core.<$> hECEndpointType,
            ("HECToken" Core..=) Core.<$> hECToken,
            ("ProcessingConfiguration" Core..=)
              Core.<$> processingConfiguration,
            ("RetryOptions" Core..=) Core.<$> retryOptions,
            ("S3BackupMode" Core..=) Core.<$> s3BackupMode,
            ("S3Update" Core..=) Core.<$> s3Update
          ]
      )
