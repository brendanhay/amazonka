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
    splS3BackupMode,
    splCloudWatchLoggingOptions,
    splHECAcknowledgmentTimeoutInSeconds,
    splRetryOptions,
    splProcessingConfiguration,
    splHECEndpoint,
    splHECEndpointType,
    splHECToken,
    splS3Configuration,
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
  { s3BackupMode ::
      Lude.Maybe SplunkS3BackupMode,
    cloudWatchLoggingOptions ::
      Lude.Maybe
        CloudWatchLoggingOptions,
    hECAcknowledgmentTimeoutInSeconds ::
      Lude.Maybe Lude.Natural,
    retryOptions ::
      Lude.Maybe SplunkRetryOptions,
    processingConfiguration ::
      Lude.Maybe
        ProcessingConfiguration,
    hECEndpoint :: Lude.Text,
    hECEndpointType ::
      HECEndpointType,
    hECToken :: Lude.Text,
    s3Configuration ::
      S3DestinationConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SplunkDestinationConfiguration' with the minimum fields required to make a request.
--
-- * 'cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
-- * 'hECAcknowledgmentTimeoutInSeconds' - The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
-- * 'hECEndpoint' - The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
-- * 'hECEndpointType' - This type can be either "Raw" or "Event."
-- * 'hECToken' - This is a GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
-- * 'processingConfiguration' - The data processing configuration.
-- * 'retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk, or if it doesn't receive an acknowledgment of receipt from Splunk.
-- * 's3BackupMode' - Defines how documents should be delivered to Amazon S3. When set to @FailedEventsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllEvents@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. The default value is @FailedEventsOnly@ .
--
-- You can update this backup mode from @FailedEventsOnly@ to @AllEvents@ . You can't update it from @AllEvents@ to @FailedEventsOnly@ .
-- * 's3Configuration' - The configuration for the backup Amazon S3 location.
mkSplunkDestinationConfiguration ::
  -- | 'hECEndpoint'
  Lude.Text ->
  -- | 'hECEndpointType'
  HECEndpointType ->
  -- | 'hECToken'
  Lude.Text ->
  -- | 's3Configuration'
  S3DestinationConfiguration ->
  SplunkDestinationConfiguration
mkSplunkDestinationConfiguration
  pHECEndpoint_
  pHECEndpointType_
  pHECToken_
  pS3Configuration_ =
    SplunkDestinationConfiguration'
      { s3BackupMode = Lude.Nothing,
        cloudWatchLoggingOptions = Lude.Nothing,
        hECAcknowledgmentTimeoutInSeconds = Lude.Nothing,
        retryOptions = Lude.Nothing,
        processingConfiguration = Lude.Nothing,
        hECEndpoint = pHECEndpoint_,
        hECEndpointType = pHECEndpointType_,
        hECToken = pHECToken_,
        s3Configuration = pS3Configuration_
      }

-- | Defines how documents should be delivered to Amazon S3. When set to @FailedEventsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllEvents@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. The default value is @FailedEventsOnly@ .
--
-- You can update this backup mode from @FailedEventsOnly@ to @AllEvents@ . You can't update it from @AllEvents@ to @FailedEventsOnly@ .
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
splS3BackupMode :: Lens.Lens' SplunkDestinationConfiguration (Lude.Maybe SplunkS3BackupMode)
splS3BackupMode = Lens.lens (s3BackupMode :: SplunkDestinationConfiguration -> Lude.Maybe SplunkS3BackupMode) (\s a -> s {s3BackupMode = a} :: SplunkDestinationConfiguration)
{-# DEPRECATED splS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
splCloudWatchLoggingOptions :: Lens.Lens' SplunkDestinationConfiguration (Lude.Maybe CloudWatchLoggingOptions)
splCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: SplunkDestinationConfiguration -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: SplunkDestinationConfiguration)
{-# DEPRECATED splCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
--
-- /Note:/ Consider using 'hECAcknowledgmentTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
splHECAcknowledgmentTimeoutInSeconds :: Lens.Lens' SplunkDestinationConfiguration (Lude.Maybe Lude.Natural)
splHECAcknowledgmentTimeoutInSeconds = Lens.lens (hECAcknowledgmentTimeoutInSeconds :: SplunkDestinationConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {hECAcknowledgmentTimeoutInSeconds = a} :: SplunkDestinationConfiguration)
{-# DEPRECATED splHECAcknowledgmentTimeoutInSeconds "Use generic-lens or generic-optics with 'hECAcknowledgmentTimeoutInSeconds' instead." #-}

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk, or if it doesn't receive an acknowledgment of receipt from Splunk.
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
splRetryOptions :: Lens.Lens' SplunkDestinationConfiguration (Lude.Maybe SplunkRetryOptions)
splRetryOptions = Lens.lens (retryOptions :: SplunkDestinationConfiguration -> Lude.Maybe SplunkRetryOptions) (\s a -> s {retryOptions = a} :: SplunkDestinationConfiguration)
{-# DEPRECATED splRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
splProcessingConfiguration :: Lens.Lens' SplunkDestinationConfiguration (Lude.Maybe ProcessingConfiguration)
splProcessingConfiguration = Lens.lens (processingConfiguration :: SplunkDestinationConfiguration -> Lude.Maybe ProcessingConfiguration) (\s a -> s {processingConfiguration = a} :: SplunkDestinationConfiguration)
{-# DEPRECATED splProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
--
-- /Note:/ Consider using 'hECEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
splHECEndpoint :: Lens.Lens' SplunkDestinationConfiguration Lude.Text
splHECEndpoint = Lens.lens (hECEndpoint :: SplunkDestinationConfiguration -> Lude.Text) (\s a -> s {hECEndpoint = a} :: SplunkDestinationConfiguration)
{-# DEPRECATED splHECEndpoint "Use generic-lens or generic-optics with 'hECEndpoint' instead." #-}

-- | This type can be either "Raw" or "Event."
--
-- /Note:/ Consider using 'hECEndpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
splHECEndpointType :: Lens.Lens' SplunkDestinationConfiguration HECEndpointType
splHECEndpointType = Lens.lens (hECEndpointType :: SplunkDestinationConfiguration -> HECEndpointType) (\s a -> s {hECEndpointType = a} :: SplunkDestinationConfiguration)
{-# DEPRECATED splHECEndpointType "Use generic-lens or generic-optics with 'hECEndpointType' instead." #-}

-- | This is a GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
--
-- /Note:/ Consider using 'hECToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
splHECToken :: Lens.Lens' SplunkDestinationConfiguration Lude.Text
splHECToken = Lens.lens (hECToken :: SplunkDestinationConfiguration -> Lude.Text) (\s a -> s {hECToken = a} :: SplunkDestinationConfiguration)
{-# DEPRECATED splHECToken "Use generic-lens or generic-optics with 'hECToken' instead." #-}

-- | The configuration for the backup Amazon S3 location.
--
-- /Note:/ Consider using 's3Configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
splS3Configuration :: Lens.Lens' SplunkDestinationConfiguration S3DestinationConfiguration
splS3Configuration = Lens.lens (s3Configuration :: SplunkDestinationConfiguration -> S3DestinationConfiguration) (\s a -> s {s3Configuration = a} :: SplunkDestinationConfiguration)
{-# DEPRECATED splS3Configuration "Use generic-lens or generic-optics with 's3Configuration' instead." #-}

instance Lude.ToJSON SplunkDestinationConfiguration where
  toJSON SplunkDestinationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3BackupMode" Lude..=) Lude.<$> s3BackupMode,
            ("CloudWatchLoggingOptions" Lude..=)
              Lude.<$> cloudWatchLoggingOptions,
            ("HECAcknowledgmentTimeoutInSeconds" Lude..=)
              Lude.<$> hECAcknowledgmentTimeoutInSeconds,
            ("RetryOptions" Lude..=) Lude.<$> retryOptions,
            ("ProcessingConfiguration" Lude..=)
              Lude.<$> processingConfiguration,
            Lude.Just ("HECEndpoint" Lude..= hECEndpoint),
            Lude.Just ("HECEndpointType" Lude..= hECEndpointType),
            Lude.Just ("HECToken" Lude..= hECToken),
            Lude.Just ("S3Configuration" Lude..= s3Configuration)
          ]
      )
