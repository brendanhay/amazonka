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
-- Module      : Network.AWS.Firehose.Types.SplunkDestinationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.SplunkDestinationConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.HECEndpointType
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import Network.AWS.Firehose.Types.SplunkRetryOptions
import Network.AWS.Firehose.Types.SplunkS3BackupMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the configuration of a destination in Splunk.
--
-- /See:/ 'newSplunkDestinationConfiguration' smart constructor.
data SplunkDestinationConfiguration = SplunkDestinationConfiguration'
  { -- | Defines how documents should be delivered to Amazon S3. When set to
    -- @FailedEventsOnly@, Kinesis Data Firehose writes any data that could not
    -- be indexed to the configured Amazon S3 destination. When set to
    -- @AllEvents@, Kinesis Data Firehose delivers all incoming records to
    -- Amazon S3, and also writes failed documents to Amazon S3. The default
    -- value is @FailedEventsOnly@.
    --
    -- You can update this backup mode from @FailedEventsOnly@ to @AllEvents@.
    -- You can\'t update it from @AllEvents@ to @FailedEventsOnly@.
    s3BackupMode :: Prelude.Maybe SplunkS3BackupMode,
    -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The amount of time that Kinesis Data Firehose waits to receive an
    -- acknowledgment from Splunk after it sends it data. At the end of the
    -- timeout period, Kinesis Data Firehose either tries to send the data
    -- again or considers it an error, based on your retry settings.
    hECAcknowledgmentTimeoutInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver
    -- data to Splunk, or if it doesn\'t receive an acknowledgment of receipt
    -- from Splunk.
    retryOptions :: Prelude.Maybe SplunkRetryOptions,
    -- | The data processing configuration.
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose
    -- sends your data.
    hECEndpoint :: Prelude.Text,
    -- | This type can be either \"Raw\" or \"Event.\"
    hECEndpointType :: HECEndpointType,
    -- | This is a GUID that you obtain from your Splunk cluster when you create
    -- a new HEC endpoint.
    hECToken :: Prelude.Text,
    -- | The configuration for the backup Amazon S3 location.
    s3Configuration :: S3DestinationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SplunkDestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3BackupMode', 'splunkDestinationConfiguration_s3BackupMode' - Defines how documents should be delivered to Amazon S3. When set to
-- @FailedEventsOnly@, Kinesis Data Firehose writes any data that could not
-- be indexed to the configured Amazon S3 destination. When set to
-- @AllEvents@, Kinesis Data Firehose delivers all incoming records to
-- Amazon S3, and also writes failed documents to Amazon S3. The default
-- value is @FailedEventsOnly@.
--
-- You can update this backup mode from @FailedEventsOnly@ to @AllEvents@.
-- You can\'t update it from @AllEvents@ to @FailedEventsOnly@.
--
-- 'cloudWatchLoggingOptions', 'splunkDestinationConfiguration_cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- 'hECAcknowledgmentTimeoutInSeconds', 'splunkDestinationConfiguration_hECAcknowledgmentTimeoutInSeconds' - The amount of time that Kinesis Data Firehose waits to receive an
-- acknowledgment from Splunk after it sends it data. At the end of the
-- timeout period, Kinesis Data Firehose either tries to send the data
-- again or considers it an error, based on your retry settings.
--
-- 'retryOptions', 'splunkDestinationConfiguration_retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver
-- data to Splunk, or if it doesn\'t receive an acknowledgment of receipt
-- from Splunk.
--
-- 'processingConfiguration', 'splunkDestinationConfiguration_processingConfiguration' - The data processing configuration.
--
-- 'hECEndpoint', 'splunkDestinationConfiguration_hECEndpoint' - The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose
-- sends your data.
--
-- 'hECEndpointType', 'splunkDestinationConfiguration_hECEndpointType' - This type can be either \"Raw\" or \"Event.\"
--
-- 'hECToken', 'splunkDestinationConfiguration_hECToken' - This is a GUID that you obtain from your Splunk cluster when you create
-- a new HEC endpoint.
--
-- 's3Configuration', 'splunkDestinationConfiguration_s3Configuration' - The configuration for the backup Amazon S3 location.
newSplunkDestinationConfiguration ::
  -- | 'hECEndpoint'
  Prelude.Text ->
  -- | 'hECEndpointType'
  HECEndpointType ->
  -- | 'hECToken'
  Prelude.Text ->
  -- | 's3Configuration'
  S3DestinationConfiguration ->
  SplunkDestinationConfiguration
newSplunkDestinationConfiguration
  pHECEndpoint_
  pHECEndpointType_
  pHECToken_
  pS3Configuration_ =
    SplunkDestinationConfiguration'
      { s3BackupMode =
          Prelude.Nothing,
        cloudWatchLoggingOptions = Prelude.Nothing,
        hECAcknowledgmentTimeoutInSeconds =
          Prelude.Nothing,
        retryOptions = Prelude.Nothing,
        processingConfiguration = Prelude.Nothing,
        hECEndpoint = pHECEndpoint_,
        hECEndpointType = pHECEndpointType_,
        hECToken = pHECToken_,
        s3Configuration = pS3Configuration_
      }

-- | Defines how documents should be delivered to Amazon S3. When set to
-- @FailedEventsOnly@, Kinesis Data Firehose writes any data that could not
-- be indexed to the configured Amazon S3 destination. When set to
-- @AllEvents@, Kinesis Data Firehose delivers all incoming records to
-- Amazon S3, and also writes failed documents to Amazon S3. The default
-- value is @FailedEventsOnly@.
--
-- You can update this backup mode from @FailedEventsOnly@ to @AllEvents@.
-- You can\'t update it from @AllEvents@ to @FailedEventsOnly@.
splunkDestinationConfiguration_s3BackupMode :: Lens.Lens' SplunkDestinationConfiguration (Prelude.Maybe SplunkS3BackupMode)
splunkDestinationConfiguration_s3BackupMode = Lens.lens (\SplunkDestinationConfiguration' {s3BackupMode} -> s3BackupMode) (\s@SplunkDestinationConfiguration' {} a -> s {s3BackupMode = a} :: SplunkDestinationConfiguration)

-- | The Amazon CloudWatch logging options for your delivery stream.
splunkDestinationConfiguration_cloudWatchLoggingOptions :: Lens.Lens' SplunkDestinationConfiguration (Prelude.Maybe CloudWatchLoggingOptions)
splunkDestinationConfiguration_cloudWatchLoggingOptions = Lens.lens (\SplunkDestinationConfiguration' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@SplunkDestinationConfiguration' {} a -> s {cloudWatchLoggingOptions = a} :: SplunkDestinationConfiguration)

-- | The amount of time that Kinesis Data Firehose waits to receive an
-- acknowledgment from Splunk after it sends it data. At the end of the
-- timeout period, Kinesis Data Firehose either tries to send the data
-- again or considers it an error, based on your retry settings.
splunkDestinationConfiguration_hECAcknowledgmentTimeoutInSeconds :: Lens.Lens' SplunkDestinationConfiguration (Prelude.Maybe Prelude.Natural)
splunkDestinationConfiguration_hECAcknowledgmentTimeoutInSeconds = Lens.lens (\SplunkDestinationConfiguration' {hECAcknowledgmentTimeoutInSeconds} -> hECAcknowledgmentTimeoutInSeconds) (\s@SplunkDestinationConfiguration' {} a -> s {hECAcknowledgmentTimeoutInSeconds = a} :: SplunkDestinationConfiguration)

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- data to Splunk, or if it doesn\'t receive an acknowledgment of receipt
-- from Splunk.
splunkDestinationConfiguration_retryOptions :: Lens.Lens' SplunkDestinationConfiguration (Prelude.Maybe SplunkRetryOptions)
splunkDestinationConfiguration_retryOptions = Lens.lens (\SplunkDestinationConfiguration' {retryOptions} -> retryOptions) (\s@SplunkDestinationConfiguration' {} a -> s {retryOptions = a} :: SplunkDestinationConfiguration)

-- | The data processing configuration.
splunkDestinationConfiguration_processingConfiguration :: Lens.Lens' SplunkDestinationConfiguration (Prelude.Maybe ProcessingConfiguration)
splunkDestinationConfiguration_processingConfiguration = Lens.lens (\SplunkDestinationConfiguration' {processingConfiguration} -> processingConfiguration) (\s@SplunkDestinationConfiguration' {} a -> s {processingConfiguration = a} :: SplunkDestinationConfiguration)

-- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose
-- sends your data.
splunkDestinationConfiguration_hECEndpoint :: Lens.Lens' SplunkDestinationConfiguration Prelude.Text
splunkDestinationConfiguration_hECEndpoint = Lens.lens (\SplunkDestinationConfiguration' {hECEndpoint} -> hECEndpoint) (\s@SplunkDestinationConfiguration' {} a -> s {hECEndpoint = a} :: SplunkDestinationConfiguration)

-- | This type can be either \"Raw\" or \"Event.\"
splunkDestinationConfiguration_hECEndpointType :: Lens.Lens' SplunkDestinationConfiguration HECEndpointType
splunkDestinationConfiguration_hECEndpointType = Lens.lens (\SplunkDestinationConfiguration' {hECEndpointType} -> hECEndpointType) (\s@SplunkDestinationConfiguration' {} a -> s {hECEndpointType = a} :: SplunkDestinationConfiguration)

-- | This is a GUID that you obtain from your Splunk cluster when you create
-- a new HEC endpoint.
splunkDestinationConfiguration_hECToken :: Lens.Lens' SplunkDestinationConfiguration Prelude.Text
splunkDestinationConfiguration_hECToken = Lens.lens (\SplunkDestinationConfiguration' {hECToken} -> hECToken) (\s@SplunkDestinationConfiguration' {} a -> s {hECToken = a} :: SplunkDestinationConfiguration)

-- | The configuration for the backup Amazon S3 location.
splunkDestinationConfiguration_s3Configuration :: Lens.Lens' SplunkDestinationConfiguration S3DestinationConfiguration
splunkDestinationConfiguration_s3Configuration = Lens.lens (\SplunkDestinationConfiguration' {s3Configuration} -> s3Configuration) (\s@SplunkDestinationConfiguration' {} a -> s {s3Configuration = a} :: SplunkDestinationConfiguration)

instance
  Prelude.Hashable
    SplunkDestinationConfiguration

instance
  Prelude.NFData
    SplunkDestinationConfiguration

instance Core.ToJSON SplunkDestinationConfiguration where
  toJSON SplunkDestinationConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("S3BackupMode" Core..=) Prelude.<$> s3BackupMode,
            ("CloudWatchLoggingOptions" Core..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("HECAcknowledgmentTimeoutInSeconds" Core..=)
              Prelude.<$> hECAcknowledgmentTimeoutInSeconds,
            ("RetryOptions" Core..=) Prelude.<$> retryOptions,
            ("ProcessingConfiguration" Core..=)
              Prelude.<$> processingConfiguration,
            Prelude.Just ("HECEndpoint" Core..= hECEndpoint),
            Prelude.Just
              ("HECEndpointType" Core..= hECEndpointType),
            Prelude.Just ("HECToken" Core..= hECToken),
            Prelude.Just
              ("S3Configuration" Core..= s3Configuration)
          ]
      )
