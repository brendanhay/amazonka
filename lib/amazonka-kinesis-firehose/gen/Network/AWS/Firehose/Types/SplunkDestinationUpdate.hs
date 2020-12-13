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
    sduS3BackupMode,
    sduHECToken,
    sduHECEndpointType,
    sduCloudWatchLoggingOptions,
    sduHECAcknowledgmentTimeoutInSeconds,
    sduS3Update,
    sduHECEndpoint,
    sduRetryOptions,
    sduProcessingConfiguration,
  )
where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.HECEndpointType
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationUpdate
import Network.AWS.Firehose.Types.SplunkRetryOptions
import Network.AWS.Firehose.Types.SplunkS3BackupMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an update for a destination in Splunk.
--
-- /See:/ 'mkSplunkDestinationUpdate' smart constructor.
data SplunkDestinationUpdate = SplunkDestinationUpdate'
  { -- | Specifies how you want Kinesis Data Firehose to back up documents to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllEvents@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. The default value is @FailedEventsOnly@ .
    --
    -- You can update this backup mode from @FailedEventsOnly@ to @AllEvents@ . You can't update it from @AllEvents@ to @FailedEventsOnly@ .
    s3BackupMode :: Lude.Maybe SplunkS3BackupMode,
    -- | A GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
    hECToken :: Lude.Maybe Lude.Text,
    -- | This type can be either "Raw" or "Event."
    hECEndpointType :: Lude.Maybe HECEndpointType,
    -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Lude.Maybe CloudWatchLoggingOptions,
    -- | The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
    hECAcknowledgmentTimeoutInSeconds :: Lude.Maybe Lude.Natural,
    -- | Your update to the configuration of the backup Amazon S3 location.
    s3Update :: Lude.Maybe S3DestinationUpdate,
    -- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
    hECEndpoint :: Lude.Maybe Lude.Text,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk or if it doesn't receive an acknowledgment of receipt from Splunk.
    retryOptions :: Lude.Maybe SplunkRetryOptions,
    -- | The data processing configuration.
    processingConfiguration :: Lude.Maybe ProcessingConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SplunkDestinationUpdate' with the minimum fields required to make a request.
--
-- * 's3BackupMode' - Specifies how you want Kinesis Data Firehose to back up documents to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllEvents@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. The default value is @FailedEventsOnly@ .
--
-- You can update this backup mode from @FailedEventsOnly@ to @AllEvents@ . You can't update it from @AllEvents@ to @FailedEventsOnly@ .
-- * 'hECToken' - A GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
-- * 'hECEndpointType' - This type can be either "Raw" or "Event."
-- * 'cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
-- * 'hECAcknowledgmentTimeoutInSeconds' - The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
-- * 's3Update' - Your update to the configuration of the backup Amazon S3 location.
-- * 'hECEndpoint' - The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
-- * 'retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk or if it doesn't receive an acknowledgment of receipt from Splunk.
-- * 'processingConfiguration' - The data processing configuration.
mkSplunkDestinationUpdate ::
  SplunkDestinationUpdate
mkSplunkDestinationUpdate =
  SplunkDestinationUpdate'
    { s3BackupMode = Lude.Nothing,
      hECToken = Lude.Nothing,
      hECEndpointType = Lude.Nothing,
      cloudWatchLoggingOptions = Lude.Nothing,
      hECAcknowledgmentTimeoutInSeconds = Lude.Nothing,
      s3Update = Lude.Nothing,
      hECEndpoint = Lude.Nothing,
      retryOptions = Lude.Nothing,
      processingConfiguration = Lude.Nothing
    }

-- | Specifies how you want Kinesis Data Firehose to back up documents to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllEvents@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. The default value is @FailedEventsOnly@ .
--
-- You can update this backup mode from @FailedEventsOnly@ to @AllEvents@ . You can't update it from @AllEvents@ to @FailedEventsOnly@ .
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sduS3BackupMode :: Lens.Lens' SplunkDestinationUpdate (Lude.Maybe SplunkS3BackupMode)
sduS3BackupMode = Lens.lens (s3BackupMode :: SplunkDestinationUpdate -> Lude.Maybe SplunkS3BackupMode) (\s a -> s {s3BackupMode = a} :: SplunkDestinationUpdate)
{-# DEPRECATED sduS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | A GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
--
-- /Note:/ Consider using 'hECToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sduHECToken :: Lens.Lens' SplunkDestinationUpdate (Lude.Maybe Lude.Text)
sduHECToken = Lens.lens (hECToken :: SplunkDestinationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {hECToken = a} :: SplunkDestinationUpdate)
{-# DEPRECATED sduHECToken "Use generic-lens or generic-optics with 'hECToken' instead." #-}

-- | This type can be either "Raw" or "Event."
--
-- /Note:/ Consider using 'hECEndpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sduHECEndpointType :: Lens.Lens' SplunkDestinationUpdate (Lude.Maybe HECEndpointType)
sduHECEndpointType = Lens.lens (hECEndpointType :: SplunkDestinationUpdate -> Lude.Maybe HECEndpointType) (\s a -> s {hECEndpointType = a} :: SplunkDestinationUpdate)
{-# DEPRECATED sduHECEndpointType "Use generic-lens or generic-optics with 'hECEndpointType' instead." #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sduCloudWatchLoggingOptions :: Lens.Lens' SplunkDestinationUpdate (Lude.Maybe CloudWatchLoggingOptions)
sduCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: SplunkDestinationUpdate -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: SplunkDestinationUpdate)
{-# DEPRECATED sduCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
--
-- /Note:/ Consider using 'hECAcknowledgmentTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sduHECAcknowledgmentTimeoutInSeconds :: Lens.Lens' SplunkDestinationUpdate (Lude.Maybe Lude.Natural)
sduHECAcknowledgmentTimeoutInSeconds = Lens.lens (hECAcknowledgmentTimeoutInSeconds :: SplunkDestinationUpdate -> Lude.Maybe Lude.Natural) (\s a -> s {hECAcknowledgmentTimeoutInSeconds = a} :: SplunkDestinationUpdate)
{-# DEPRECATED sduHECAcknowledgmentTimeoutInSeconds "Use generic-lens or generic-optics with 'hECAcknowledgmentTimeoutInSeconds' instead." #-}

-- | Your update to the configuration of the backup Amazon S3 location.
--
-- /Note:/ Consider using 's3Update' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sduS3Update :: Lens.Lens' SplunkDestinationUpdate (Lude.Maybe S3DestinationUpdate)
sduS3Update = Lens.lens (s3Update :: SplunkDestinationUpdate -> Lude.Maybe S3DestinationUpdate) (\s a -> s {s3Update = a} :: SplunkDestinationUpdate)
{-# DEPRECATED sduS3Update "Use generic-lens or generic-optics with 's3Update' instead." #-}

-- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
--
-- /Note:/ Consider using 'hECEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sduHECEndpoint :: Lens.Lens' SplunkDestinationUpdate (Lude.Maybe Lude.Text)
sduHECEndpoint = Lens.lens (hECEndpoint :: SplunkDestinationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {hECEndpoint = a} :: SplunkDestinationUpdate)
{-# DEPRECATED sduHECEndpoint "Use generic-lens or generic-optics with 'hECEndpoint' instead." #-}

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk or if it doesn't receive an acknowledgment of receipt from Splunk.
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sduRetryOptions :: Lens.Lens' SplunkDestinationUpdate (Lude.Maybe SplunkRetryOptions)
sduRetryOptions = Lens.lens (retryOptions :: SplunkDestinationUpdate -> Lude.Maybe SplunkRetryOptions) (\s a -> s {retryOptions = a} :: SplunkDestinationUpdate)
{-# DEPRECATED sduRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sduProcessingConfiguration :: Lens.Lens' SplunkDestinationUpdate (Lude.Maybe ProcessingConfiguration)
sduProcessingConfiguration = Lens.lens (processingConfiguration :: SplunkDestinationUpdate -> Lude.Maybe ProcessingConfiguration) (\s a -> s {processingConfiguration = a} :: SplunkDestinationUpdate)
{-# DEPRECATED sduProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

instance Lude.ToJSON SplunkDestinationUpdate where
  toJSON SplunkDestinationUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3BackupMode" Lude..=) Lude.<$> s3BackupMode,
            ("HECToken" Lude..=) Lude.<$> hECToken,
            ("HECEndpointType" Lude..=) Lude.<$> hECEndpointType,
            ("CloudWatchLoggingOptions" Lude..=)
              Lude.<$> cloudWatchLoggingOptions,
            ("HECAcknowledgmentTimeoutInSeconds" Lude..=)
              Lude.<$> hECAcknowledgmentTimeoutInSeconds,
            ("S3Update" Lude..=) Lude.<$> s3Update,
            ("HECEndpoint" Lude..=) Lude.<$> hECEndpoint,
            ("RetryOptions" Lude..=) Lude.<$> retryOptions,
            ("ProcessingConfiguration" Lude..=)
              Lude.<$> processingConfiguration
          ]
      )
