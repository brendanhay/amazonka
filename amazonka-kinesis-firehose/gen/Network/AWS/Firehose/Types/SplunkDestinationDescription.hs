{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SplunkDestinationDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.SplunkDestinationDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.HECEndpointType
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationDescription
import Network.AWS.Firehose.Types.SplunkRetryOptions
import Network.AWS.Firehose.Types.SplunkS3BackupMode
import qualified Network.AWS.Lens as Lens

-- | Describes a destination in Splunk.
--
-- /See:/ 'newSplunkDestinationDescription' smart constructor.
data SplunkDestinationDescription = SplunkDestinationDescription'
  { -- | The amount of time that Kinesis Data Firehose waits to receive an
    -- acknowledgment from Splunk after it sends it data. At the end of the
    -- timeout period, Kinesis Data Firehose either tries to send the data
    -- again or considers it an error, based on your retry settings.
    hECAcknowledgmentTimeoutInSeconds :: Core.Maybe Core.Natural,
    -- | The data processing configuration.
    processingConfiguration :: Core.Maybe ProcessingConfiguration,
    -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Core.Maybe CloudWatchLoggingOptions,
    -- | This type can be either \"Raw\" or \"Event.\"
    hECEndpointType :: Core.Maybe HECEndpointType,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver
    -- data to Splunk or if it doesn\'t receive an acknowledgment of receipt
    -- from Splunk.
    retryOptions :: Core.Maybe SplunkRetryOptions,
    -- | Defines how documents should be delivered to Amazon S3. When set to
    -- @FailedDocumentsOnly@, Kinesis Data Firehose writes any data that could
    -- not be indexed to the configured Amazon S3 destination. When set to
    -- @AllDocuments@, Kinesis Data Firehose delivers all incoming records to
    -- Amazon S3, and also writes failed documents to Amazon S3. Default value
    -- is @FailedDocumentsOnly@.
    s3BackupMode :: Core.Maybe SplunkS3BackupMode,
    -- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose
    -- sends your data.
    hECEndpoint :: Core.Maybe Core.Text,
    -- | A GUID you obtain from your Splunk cluster when you create a new HEC
    -- endpoint.
    hECToken :: Core.Maybe Core.Text,
    -- | The Amazon S3 destination.>
    s3DestinationDescription :: Core.Maybe S3DestinationDescription
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SplunkDestinationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hECAcknowledgmentTimeoutInSeconds', 'splunkDestinationDescription_hECAcknowledgmentTimeoutInSeconds' - The amount of time that Kinesis Data Firehose waits to receive an
-- acknowledgment from Splunk after it sends it data. At the end of the
-- timeout period, Kinesis Data Firehose either tries to send the data
-- again or considers it an error, based on your retry settings.
--
-- 'processingConfiguration', 'splunkDestinationDescription_processingConfiguration' - The data processing configuration.
--
-- 'cloudWatchLoggingOptions', 'splunkDestinationDescription_cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- 'hECEndpointType', 'splunkDestinationDescription_hECEndpointType' - This type can be either \"Raw\" or \"Event.\"
--
-- 'retryOptions', 'splunkDestinationDescription_retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver
-- data to Splunk or if it doesn\'t receive an acknowledgment of receipt
-- from Splunk.
--
-- 's3BackupMode', 'splunkDestinationDescription_s3BackupMode' - Defines how documents should be delivered to Amazon S3. When set to
-- @FailedDocumentsOnly@, Kinesis Data Firehose writes any data that could
-- not be indexed to the configured Amazon S3 destination. When set to
-- @AllDocuments@, Kinesis Data Firehose delivers all incoming records to
-- Amazon S3, and also writes failed documents to Amazon S3. Default value
-- is @FailedDocumentsOnly@.
--
-- 'hECEndpoint', 'splunkDestinationDescription_hECEndpoint' - The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose
-- sends your data.
--
-- 'hECToken', 'splunkDestinationDescription_hECToken' - A GUID you obtain from your Splunk cluster when you create a new HEC
-- endpoint.
--
-- 's3DestinationDescription', 'splunkDestinationDescription_s3DestinationDescription' - The Amazon S3 destination.>
newSplunkDestinationDescription ::
  SplunkDestinationDescription
newSplunkDestinationDescription =
  SplunkDestinationDescription'
    { hECAcknowledgmentTimeoutInSeconds =
        Core.Nothing,
      processingConfiguration = Core.Nothing,
      cloudWatchLoggingOptions = Core.Nothing,
      hECEndpointType = Core.Nothing,
      retryOptions = Core.Nothing,
      s3BackupMode = Core.Nothing,
      hECEndpoint = Core.Nothing,
      hECToken = Core.Nothing,
      s3DestinationDescription = Core.Nothing
    }

-- | The amount of time that Kinesis Data Firehose waits to receive an
-- acknowledgment from Splunk after it sends it data. At the end of the
-- timeout period, Kinesis Data Firehose either tries to send the data
-- again or considers it an error, based on your retry settings.
splunkDestinationDescription_hECAcknowledgmentTimeoutInSeconds :: Lens.Lens' SplunkDestinationDescription (Core.Maybe Core.Natural)
splunkDestinationDescription_hECAcknowledgmentTimeoutInSeconds = Lens.lens (\SplunkDestinationDescription' {hECAcknowledgmentTimeoutInSeconds} -> hECAcknowledgmentTimeoutInSeconds) (\s@SplunkDestinationDescription' {} a -> s {hECAcknowledgmentTimeoutInSeconds = a} :: SplunkDestinationDescription)

-- | The data processing configuration.
splunkDestinationDescription_processingConfiguration :: Lens.Lens' SplunkDestinationDescription (Core.Maybe ProcessingConfiguration)
splunkDestinationDescription_processingConfiguration = Lens.lens (\SplunkDestinationDescription' {processingConfiguration} -> processingConfiguration) (\s@SplunkDestinationDescription' {} a -> s {processingConfiguration = a} :: SplunkDestinationDescription)

-- | The Amazon CloudWatch logging options for your delivery stream.
splunkDestinationDescription_cloudWatchLoggingOptions :: Lens.Lens' SplunkDestinationDescription (Core.Maybe CloudWatchLoggingOptions)
splunkDestinationDescription_cloudWatchLoggingOptions = Lens.lens (\SplunkDestinationDescription' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@SplunkDestinationDescription' {} a -> s {cloudWatchLoggingOptions = a} :: SplunkDestinationDescription)

-- | This type can be either \"Raw\" or \"Event.\"
splunkDestinationDescription_hECEndpointType :: Lens.Lens' SplunkDestinationDescription (Core.Maybe HECEndpointType)
splunkDestinationDescription_hECEndpointType = Lens.lens (\SplunkDestinationDescription' {hECEndpointType} -> hECEndpointType) (\s@SplunkDestinationDescription' {} a -> s {hECEndpointType = a} :: SplunkDestinationDescription)

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- data to Splunk or if it doesn\'t receive an acknowledgment of receipt
-- from Splunk.
splunkDestinationDescription_retryOptions :: Lens.Lens' SplunkDestinationDescription (Core.Maybe SplunkRetryOptions)
splunkDestinationDescription_retryOptions = Lens.lens (\SplunkDestinationDescription' {retryOptions} -> retryOptions) (\s@SplunkDestinationDescription' {} a -> s {retryOptions = a} :: SplunkDestinationDescription)

-- | Defines how documents should be delivered to Amazon S3. When set to
-- @FailedDocumentsOnly@, Kinesis Data Firehose writes any data that could
-- not be indexed to the configured Amazon S3 destination. When set to
-- @AllDocuments@, Kinesis Data Firehose delivers all incoming records to
-- Amazon S3, and also writes failed documents to Amazon S3. Default value
-- is @FailedDocumentsOnly@.
splunkDestinationDescription_s3BackupMode :: Lens.Lens' SplunkDestinationDescription (Core.Maybe SplunkS3BackupMode)
splunkDestinationDescription_s3BackupMode = Lens.lens (\SplunkDestinationDescription' {s3BackupMode} -> s3BackupMode) (\s@SplunkDestinationDescription' {} a -> s {s3BackupMode = a} :: SplunkDestinationDescription)

-- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose
-- sends your data.
splunkDestinationDescription_hECEndpoint :: Lens.Lens' SplunkDestinationDescription (Core.Maybe Core.Text)
splunkDestinationDescription_hECEndpoint = Lens.lens (\SplunkDestinationDescription' {hECEndpoint} -> hECEndpoint) (\s@SplunkDestinationDescription' {} a -> s {hECEndpoint = a} :: SplunkDestinationDescription)

-- | A GUID you obtain from your Splunk cluster when you create a new HEC
-- endpoint.
splunkDestinationDescription_hECToken :: Lens.Lens' SplunkDestinationDescription (Core.Maybe Core.Text)
splunkDestinationDescription_hECToken = Lens.lens (\SplunkDestinationDescription' {hECToken} -> hECToken) (\s@SplunkDestinationDescription' {} a -> s {hECToken = a} :: SplunkDestinationDescription)

-- | The Amazon S3 destination.>
splunkDestinationDescription_s3DestinationDescription :: Lens.Lens' SplunkDestinationDescription (Core.Maybe S3DestinationDescription)
splunkDestinationDescription_s3DestinationDescription = Lens.lens (\SplunkDestinationDescription' {s3DestinationDescription} -> s3DestinationDescription) (\s@SplunkDestinationDescription' {} a -> s {s3DestinationDescription = a} :: SplunkDestinationDescription)

instance Core.FromJSON SplunkDestinationDescription where
  parseJSON =
    Core.withObject
      "SplunkDestinationDescription"
      ( \x ->
          SplunkDestinationDescription'
            Core.<$> (x Core..:? "HECAcknowledgmentTimeoutInSeconds")
            Core.<*> (x Core..:? "ProcessingConfiguration")
            Core.<*> (x Core..:? "CloudWatchLoggingOptions")
            Core.<*> (x Core..:? "HECEndpointType")
            Core.<*> (x Core..:? "RetryOptions")
            Core.<*> (x Core..:? "S3BackupMode")
            Core.<*> (x Core..:? "HECEndpoint")
            Core.<*> (x Core..:? "HECToken")
            Core.<*> (x Core..:? "S3DestinationDescription")
      )

instance Core.Hashable SplunkDestinationDescription

instance Core.NFData SplunkDestinationDescription
