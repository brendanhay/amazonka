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
    sS3BackupMode,
    sHECToken,
    sHECEndpointType,
    sS3Configuration,
    sCloudWatchLoggingOptions,
    sHECAcknowledgmentTimeoutInSeconds,
    sHECEndpoint,
    sRetryOptions,
    sProcessingConfiguration,
  )
where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.HECEndpointType
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import Network.AWS.Firehose.Types.SplunkRetryOptions
import Network.AWS.Firehose.Types.SplunkS3BackupMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the configuration of a destination in Splunk.
--
-- /See:/ 'mkSplunkDestinationConfiguration' smart constructor.
data SplunkDestinationConfiguration = SplunkDestinationConfiguration'
  { -- | Defines how documents should be delivered to Amazon S3. When set to @FailedEventsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllEvents@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. The default value is @FailedEventsOnly@ .
    --
    -- You can update this backup mode from @FailedEventsOnly@ to @AllEvents@ . You can't update it from @AllEvents@ to @FailedEventsOnly@ .
    s3BackupMode :: Lude.Maybe SplunkS3BackupMode,
    -- | This is a GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
    hECToken :: Lude.Text,
    -- | This type can be either "Raw" or "Event."
    hECEndpointType :: HECEndpointType,
    -- | The configuration for the backup Amazon S3 location.
    s3Configuration :: S3DestinationConfiguration,
    -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Lude.Maybe CloudWatchLoggingOptions,
    -- | The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
    hECAcknowledgmentTimeoutInSeconds :: Lude.Maybe Lude.Natural,
    -- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
    hECEndpoint :: Lude.Text,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk, or if it doesn't receive an acknowledgment of receipt from Splunk.
    retryOptions :: Lude.Maybe SplunkRetryOptions,
    -- | The data processing configuration.
    processingConfiguration :: Lude.Maybe ProcessingConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SplunkDestinationConfiguration' with the minimum fields required to make a request.
--
-- * 's3BackupMode' - Defines how documents should be delivered to Amazon S3. When set to @FailedEventsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllEvents@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. The default value is @FailedEventsOnly@ .
--
-- You can update this backup mode from @FailedEventsOnly@ to @AllEvents@ . You can't update it from @AllEvents@ to @FailedEventsOnly@ .
-- * 'hECToken' - This is a GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
-- * 'hECEndpointType' - This type can be either "Raw" or "Event."
-- * 's3Configuration' - The configuration for the backup Amazon S3 location.
-- * 'cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
-- * 'hECAcknowledgmentTimeoutInSeconds' - The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
-- * 'hECEndpoint' - The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
-- * 'retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk, or if it doesn't receive an acknowledgment of receipt from Splunk.
-- * 'processingConfiguration' - The data processing configuration.
mkSplunkDestinationConfiguration ::
  -- | 'hECToken'
  Lude.Text ->
  -- | 'hECEndpointType'
  HECEndpointType ->
  -- | 's3Configuration'
  S3DestinationConfiguration ->
  -- | 'hECEndpoint'
  Lude.Text ->
  SplunkDestinationConfiguration
mkSplunkDestinationConfiguration
  pHECToken_
  pHECEndpointType_
  pS3Configuration_
  pHECEndpoint_ =
    SplunkDestinationConfiguration'
      { s3BackupMode = Lude.Nothing,
        hECToken = pHECToken_,
        hECEndpointType = pHECEndpointType_,
        s3Configuration = pS3Configuration_,
        cloudWatchLoggingOptions = Lude.Nothing,
        hECAcknowledgmentTimeoutInSeconds = Lude.Nothing,
        hECEndpoint = pHECEndpoint_,
        retryOptions = Lude.Nothing,
        processingConfiguration = Lude.Nothing
      }

-- | Defines how documents should be delivered to Amazon S3. When set to @FailedEventsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllEvents@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. The default value is @FailedEventsOnly@ .
--
-- You can update this backup mode from @FailedEventsOnly@ to @AllEvents@ . You can't update it from @AllEvents@ to @FailedEventsOnly@ .
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sS3BackupMode :: Lens.Lens' SplunkDestinationConfiguration (Lude.Maybe SplunkS3BackupMode)
sS3BackupMode = Lens.lens (s3BackupMode :: SplunkDestinationConfiguration -> Lude.Maybe SplunkS3BackupMode) (\s a -> s {s3BackupMode = a} :: SplunkDestinationConfiguration)
{-# DEPRECATED sS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | This is a GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
--
-- /Note:/ Consider using 'hECToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sHECToken :: Lens.Lens' SplunkDestinationConfiguration Lude.Text
sHECToken = Lens.lens (hECToken :: SplunkDestinationConfiguration -> Lude.Text) (\s a -> s {hECToken = a} :: SplunkDestinationConfiguration)
{-# DEPRECATED sHECToken "Use generic-lens or generic-optics with 'hECToken' instead." #-}

-- | This type can be either "Raw" or "Event."
--
-- /Note:/ Consider using 'hECEndpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sHECEndpointType :: Lens.Lens' SplunkDestinationConfiguration HECEndpointType
sHECEndpointType = Lens.lens (hECEndpointType :: SplunkDestinationConfiguration -> HECEndpointType) (\s a -> s {hECEndpointType = a} :: SplunkDestinationConfiguration)
{-# DEPRECATED sHECEndpointType "Use generic-lens or generic-optics with 'hECEndpointType' instead." #-}

-- | The configuration for the backup Amazon S3 location.
--
-- /Note:/ Consider using 's3Configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sS3Configuration :: Lens.Lens' SplunkDestinationConfiguration S3DestinationConfiguration
sS3Configuration = Lens.lens (s3Configuration :: SplunkDestinationConfiguration -> S3DestinationConfiguration) (\s a -> s {s3Configuration = a} :: SplunkDestinationConfiguration)
{-# DEPRECATED sS3Configuration "Use generic-lens or generic-optics with 's3Configuration' instead." #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCloudWatchLoggingOptions :: Lens.Lens' SplunkDestinationConfiguration (Lude.Maybe CloudWatchLoggingOptions)
sCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: SplunkDestinationConfiguration -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: SplunkDestinationConfiguration)
{-# DEPRECATED sCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
--
-- /Note:/ Consider using 'hECAcknowledgmentTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sHECAcknowledgmentTimeoutInSeconds :: Lens.Lens' SplunkDestinationConfiguration (Lude.Maybe Lude.Natural)
sHECAcknowledgmentTimeoutInSeconds = Lens.lens (hECAcknowledgmentTimeoutInSeconds :: SplunkDestinationConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {hECAcknowledgmentTimeoutInSeconds = a} :: SplunkDestinationConfiguration)
{-# DEPRECATED sHECAcknowledgmentTimeoutInSeconds "Use generic-lens or generic-optics with 'hECAcknowledgmentTimeoutInSeconds' instead." #-}

-- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
--
-- /Note:/ Consider using 'hECEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sHECEndpoint :: Lens.Lens' SplunkDestinationConfiguration Lude.Text
sHECEndpoint = Lens.lens (hECEndpoint :: SplunkDestinationConfiguration -> Lude.Text) (\s a -> s {hECEndpoint = a} :: SplunkDestinationConfiguration)
{-# DEPRECATED sHECEndpoint "Use generic-lens or generic-optics with 'hECEndpoint' instead." #-}

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk, or if it doesn't receive an acknowledgment of receipt from Splunk.
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRetryOptions :: Lens.Lens' SplunkDestinationConfiguration (Lude.Maybe SplunkRetryOptions)
sRetryOptions = Lens.lens (retryOptions :: SplunkDestinationConfiguration -> Lude.Maybe SplunkRetryOptions) (\s a -> s {retryOptions = a} :: SplunkDestinationConfiguration)
{-# DEPRECATED sRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sProcessingConfiguration :: Lens.Lens' SplunkDestinationConfiguration (Lude.Maybe ProcessingConfiguration)
sProcessingConfiguration = Lens.lens (processingConfiguration :: SplunkDestinationConfiguration -> Lude.Maybe ProcessingConfiguration) (\s a -> s {processingConfiguration = a} :: SplunkDestinationConfiguration)
{-# DEPRECATED sProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

instance Lude.ToJSON SplunkDestinationConfiguration where
  toJSON SplunkDestinationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3BackupMode" Lude..=) Lude.<$> s3BackupMode,
            Lude.Just ("HECToken" Lude..= hECToken),
            Lude.Just ("HECEndpointType" Lude..= hECEndpointType),
            Lude.Just ("S3Configuration" Lude..= s3Configuration),
            ("CloudWatchLoggingOptions" Lude..=)
              Lude.<$> cloudWatchLoggingOptions,
            ("HECAcknowledgmentTimeoutInSeconds" Lude..=)
              Lude.<$> hECAcknowledgmentTimeoutInSeconds,
            Lude.Just ("HECEndpoint" Lude..= hECEndpoint),
            ("RetryOptions" Lude..=) Lude.<$> retryOptions,
            ("ProcessingConfiguration" Lude..=)
              Lude.<$> processingConfiguration
          ]
      )
