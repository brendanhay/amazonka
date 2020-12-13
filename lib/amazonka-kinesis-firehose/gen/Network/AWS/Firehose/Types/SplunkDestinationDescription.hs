{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SplunkDestinationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.SplunkDestinationDescription
  ( SplunkDestinationDescription (..),

    -- * Smart constructor
    mkSplunkDestinationDescription,

    -- * Lenses
    sddS3BackupMode,
    sddHECToken,
    sddHECEndpointType,
    sddCloudWatchLoggingOptions,
    sddHECAcknowledgmentTimeoutInSeconds,
    sddS3DestinationDescription,
    sddHECEndpoint,
    sddRetryOptions,
    sddProcessingConfiguration,
  )
where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.HECEndpointType
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationDescription
import Network.AWS.Firehose.Types.SplunkRetryOptions
import Network.AWS.Firehose.Types.SplunkS3BackupMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a destination in Splunk.
--
-- /See:/ 'mkSplunkDestinationDescription' smart constructor.
data SplunkDestinationDescription = SplunkDestinationDescription'
  { -- | Defines how documents should be delivered to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. Default value is @FailedDocumentsOnly@ .
    s3BackupMode :: Lude.Maybe SplunkS3BackupMode,
    -- | A GUID you obtain from your Splunk cluster when you create a new HEC endpoint.
    hECToken :: Lude.Maybe Lude.Text,
    -- | This type can be either "Raw" or "Event."
    hECEndpointType :: Lude.Maybe HECEndpointType,
    -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Lude.Maybe CloudWatchLoggingOptions,
    -- | The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
    hECAcknowledgmentTimeoutInSeconds :: Lude.Maybe Lude.Natural,
    -- | The Amazon S3 destination.>
    s3DestinationDescription :: Lude.Maybe S3DestinationDescription,
    -- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
    hECEndpoint :: Lude.Maybe Lude.Text,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk or if it doesn't receive an acknowledgment of receipt from Splunk.
    retryOptions :: Lude.Maybe SplunkRetryOptions,
    -- | The data processing configuration.
    processingConfiguration :: Lude.Maybe ProcessingConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SplunkDestinationDescription' with the minimum fields required to make a request.
--
-- * 's3BackupMode' - Defines how documents should be delivered to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. Default value is @FailedDocumentsOnly@ .
-- * 'hECToken' - A GUID you obtain from your Splunk cluster when you create a new HEC endpoint.
-- * 'hECEndpointType' - This type can be either "Raw" or "Event."
-- * 'cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
-- * 'hECAcknowledgmentTimeoutInSeconds' - The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
-- * 's3DestinationDescription' - The Amazon S3 destination.>
-- * 'hECEndpoint' - The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
-- * 'retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk or if it doesn't receive an acknowledgment of receipt from Splunk.
-- * 'processingConfiguration' - The data processing configuration.
mkSplunkDestinationDescription ::
  SplunkDestinationDescription
mkSplunkDestinationDescription =
  SplunkDestinationDescription'
    { s3BackupMode = Lude.Nothing,
      hECToken = Lude.Nothing,
      hECEndpointType = Lude.Nothing,
      cloudWatchLoggingOptions = Lude.Nothing,
      hECAcknowledgmentTimeoutInSeconds = Lude.Nothing,
      s3DestinationDescription = Lude.Nothing,
      hECEndpoint = Lude.Nothing,
      retryOptions = Lude.Nothing,
      processingConfiguration = Lude.Nothing
    }

-- | Defines how documents should be delivered to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. Default value is @FailedDocumentsOnly@ .
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddS3BackupMode :: Lens.Lens' SplunkDestinationDescription (Lude.Maybe SplunkS3BackupMode)
sddS3BackupMode = Lens.lens (s3BackupMode :: SplunkDestinationDescription -> Lude.Maybe SplunkS3BackupMode) (\s a -> s {s3BackupMode = a} :: SplunkDestinationDescription)
{-# DEPRECATED sddS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | A GUID you obtain from your Splunk cluster when you create a new HEC endpoint.
--
-- /Note:/ Consider using 'hECToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddHECToken :: Lens.Lens' SplunkDestinationDescription (Lude.Maybe Lude.Text)
sddHECToken = Lens.lens (hECToken :: SplunkDestinationDescription -> Lude.Maybe Lude.Text) (\s a -> s {hECToken = a} :: SplunkDestinationDescription)
{-# DEPRECATED sddHECToken "Use generic-lens or generic-optics with 'hECToken' instead." #-}

-- | This type can be either "Raw" or "Event."
--
-- /Note:/ Consider using 'hECEndpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddHECEndpointType :: Lens.Lens' SplunkDestinationDescription (Lude.Maybe HECEndpointType)
sddHECEndpointType = Lens.lens (hECEndpointType :: SplunkDestinationDescription -> Lude.Maybe HECEndpointType) (\s a -> s {hECEndpointType = a} :: SplunkDestinationDescription)
{-# DEPRECATED sddHECEndpointType "Use generic-lens or generic-optics with 'hECEndpointType' instead." #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddCloudWatchLoggingOptions :: Lens.Lens' SplunkDestinationDescription (Lude.Maybe CloudWatchLoggingOptions)
sddCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: SplunkDestinationDescription -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: SplunkDestinationDescription)
{-# DEPRECATED sddCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
--
-- /Note:/ Consider using 'hECAcknowledgmentTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddHECAcknowledgmentTimeoutInSeconds :: Lens.Lens' SplunkDestinationDescription (Lude.Maybe Lude.Natural)
sddHECAcknowledgmentTimeoutInSeconds = Lens.lens (hECAcknowledgmentTimeoutInSeconds :: SplunkDestinationDescription -> Lude.Maybe Lude.Natural) (\s a -> s {hECAcknowledgmentTimeoutInSeconds = a} :: SplunkDestinationDescription)
{-# DEPRECATED sddHECAcknowledgmentTimeoutInSeconds "Use generic-lens or generic-optics with 'hECAcknowledgmentTimeoutInSeconds' instead." #-}

-- | The Amazon S3 destination.>
--
-- /Note:/ Consider using 's3DestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddS3DestinationDescription :: Lens.Lens' SplunkDestinationDescription (Lude.Maybe S3DestinationDescription)
sddS3DestinationDescription = Lens.lens (s3DestinationDescription :: SplunkDestinationDescription -> Lude.Maybe S3DestinationDescription) (\s a -> s {s3DestinationDescription = a} :: SplunkDestinationDescription)
{-# DEPRECATED sddS3DestinationDescription "Use generic-lens or generic-optics with 's3DestinationDescription' instead." #-}

-- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
--
-- /Note:/ Consider using 'hECEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddHECEndpoint :: Lens.Lens' SplunkDestinationDescription (Lude.Maybe Lude.Text)
sddHECEndpoint = Lens.lens (hECEndpoint :: SplunkDestinationDescription -> Lude.Maybe Lude.Text) (\s a -> s {hECEndpoint = a} :: SplunkDestinationDescription)
{-# DEPRECATED sddHECEndpoint "Use generic-lens or generic-optics with 'hECEndpoint' instead." #-}

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk or if it doesn't receive an acknowledgment of receipt from Splunk.
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddRetryOptions :: Lens.Lens' SplunkDestinationDescription (Lude.Maybe SplunkRetryOptions)
sddRetryOptions = Lens.lens (retryOptions :: SplunkDestinationDescription -> Lude.Maybe SplunkRetryOptions) (\s a -> s {retryOptions = a} :: SplunkDestinationDescription)
{-# DEPRECATED sddRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddProcessingConfiguration :: Lens.Lens' SplunkDestinationDescription (Lude.Maybe ProcessingConfiguration)
sddProcessingConfiguration = Lens.lens (processingConfiguration :: SplunkDestinationDescription -> Lude.Maybe ProcessingConfiguration) (\s a -> s {processingConfiguration = a} :: SplunkDestinationDescription)
{-# DEPRECATED sddProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

instance Lude.FromJSON SplunkDestinationDescription where
  parseJSON =
    Lude.withObject
      "SplunkDestinationDescription"
      ( \x ->
          SplunkDestinationDescription'
            Lude.<$> (x Lude..:? "S3BackupMode")
            Lude.<*> (x Lude..:? "HECToken")
            Lude.<*> (x Lude..:? "HECEndpointType")
            Lude.<*> (x Lude..:? "CloudWatchLoggingOptions")
            Lude.<*> (x Lude..:? "HECAcknowledgmentTimeoutInSeconds")
            Lude.<*> (x Lude..:? "S3DestinationDescription")
            Lude.<*> (x Lude..:? "HECEndpoint")
            Lude.<*> (x Lude..:? "RetryOptions")
            Lude.<*> (x Lude..:? "ProcessingConfiguration")
      )
