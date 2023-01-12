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
-- Module      : Amazonka.Firehose.Types.SplunkDestinationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.SplunkDestinationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.HECEndpointType
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.S3DestinationUpdate
import Amazonka.Firehose.Types.SplunkRetryOptions
import Amazonka.Firehose.Types.SplunkS3BackupMode
import qualified Amazonka.Prelude as Prelude

-- | Describes an update for a destination in Splunk.
--
-- /See:/ 'newSplunkDestinationUpdate' smart constructor.
data SplunkDestinationUpdate = SplunkDestinationUpdate'
  { -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The amount of time that Kinesis Data Firehose waits to receive an
    -- acknowledgment from Splunk after it sends data. At the end of the
    -- timeout period, Kinesis Data Firehose either tries to send the data
    -- again or considers it an error, based on your retry settings.
    hECAcknowledgmentTimeoutInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose
    -- sends your data.
    hECEndpoint :: Prelude.Maybe Prelude.Text,
    -- | This type can be either \"Raw\" or \"Event.\"
    hECEndpointType :: Prelude.Maybe HECEndpointType,
    -- | A GUID that you obtain from your Splunk cluster when you create a new
    -- HEC endpoint.
    hECToken :: Prelude.Maybe Prelude.Text,
    -- | The data processing configuration.
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver
    -- data to Splunk or if it doesn\'t receive an acknowledgment of receipt
    -- from Splunk.
    retryOptions :: Prelude.Maybe SplunkRetryOptions,
    -- | Specifies how you want Kinesis Data Firehose to back up documents to
    -- Amazon S3. When set to @FailedDocumentsOnly@, Kinesis Data Firehose
    -- writes any data that could not be indexed to the configured Amazon S3
    -- destination. When set to @AllEvents@, Kinesis Data Firehose delivers all
    -- incoming records to Amazon S3, and also writes failed documents to
    -- Amazon S3. The default value is @FailedEventsOnly@.
    --
    -- You can update this backup mode from @FailedEventsOnly@ to @AllEvents@.
    -- You can\'t update it from @AllEvents@ to @FailedEventsOnly@.
    s3BackupMode :: Prelude.Maybe SplunkS3BackupMode,
    -- | Your update to the configuration of the backup Amazon S3 location.
    s3Update :: Prelude.Maybe S3DestinationUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SplunkDestinationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLoggingOptions', 'splunkDestinationUpdate_cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- 'hECAcknowledgmentTimeoutInSeconds', 'splunkDestinationUpdate_hECAcknowledgmentTimeoutInSeconds' - The amount of time that Kinesis Data Firehose waits to receive an
-- acknowledgment from Splunk after it sends data. At the end of the
-- timeout period, Kinesis Data Firehose either tries to send the data
-- again or considers it an error, based on your retry settings.
--
-- 'hECEndpoint', 'splunkDestinationUpdate_hECEndpoint' - The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose
-- sends your data.
--
-- 'hECEndpointType', 'splunkDestinationUpdate_hECEndpointType' - This type can be either \"Raw\" or \"Event.\"
--
-- 'hECToken', 'splunkDestinationUpdate_hECToken' - A GUID that you obtain from your Splunk cluster when you create a new
-- HEC endpoint.
--
-- 'processingConfiguration', 'splunkDestinationUpdate_processingConfiguration' - The data processing configuration.
--
-- 'retryOptions', 'splunkDestinationUpdate_retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver
-- data to Splunk or if it doesn\'t receive an acknowledgment of receipt
-- from Splunk.
--
-- 's3BackupMode', 'splunkDestinationUpdate_s3BackupMode' - Specifies how you want Kinesis Data Firehose to back up documents to
-- Amazon S3. When set to @FailedDocumentsOnly@, Kinesis Data Firehose
-- writes any data that could not be indexed to the configured Amazon S3
-- destination. When set to @AllEvents@, Kinesis Data Firehose delivers all
-- incoming records to Amazon S3, and also writes failed documents to
-- Amazon S3. The default value is @FailedEventsOnly@.
--
-- You can update this backup mode from @FailedEventsOnly@ to @AllEvents@.
-- You can\'t update it from @AllEvents@ to @FailedEventsOnly@.
--
-- 's3Update', 'splunkDestinationUpdate_s3Update' - Your update to the configuration of the backup Amazon S3 location.
newSplunkDestinationUpdate ::
  SplunkDestinationUpdate
newSplunkDestinationUpdate =
  SplunkDestinationUpdate'
    { cloudWatchLoggingOptions =
        Prelude.Nothing,
      hECAcknowledgmentTimeoutInSeconds =
        Prelude.Nothing,
      hECEndpoint = Prelude.Nothing,
      hECEndpointType = Prelude.Nothing,
      hECToken = Prelude.Nothing,
      processingConfiguration = Prelude.Nothing,
      retryOptions = Prelude.Nothing,
      s3BackupMode = Prelude.Nothing,
      s3Update = Prelude.Nothing
    }

-- | The Amazon CloudWatch logging options for your delivery stream.
splunkDestinationUpdate_cloudWatchLoggingOptions :: Lens.Lens' SplunkDestinationUpdate (Prelude.Maybe CloudWatchLoggingOptions)
splunkDestinationUpdate_cloudWatchLoggingOptions = Lens.lens (\SplunkDestinationUpdate' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@SplunkDestinationUpdate' {} a -> s {cloudWatchLoggingOptions = a} :: SplunkDestinationUpdate)

-- | The amount of time that Kinesis Data Firehose waits to receive an
-- acknowledgment from Splunk after it sends data. At the end of the
-- timeout period, Kinesis Data Firehose either tries to send the data
-- again or considers it an error, based on your retry settings.
splunkDestinationUpdate_hECAcknowledgmentTimeoutInSeconds :: Lens.Lens' SplunkDestinationUpdate (Prelude.Maybe Prelude.Natural)
splunkDestinationUpdate_hECAcknowledgmentTimeoutInSeconds = Lens.lens (\SplunkDestinationUpdate' {hECAcknowledgmentTimeoutInSeconds} -> hECAcknowledgmentTimeoutInSeconds) (\s@SplunkDestinationUpdate' {} a -> s {hECAcknowledgmentTimeoutInSeconds = a} :: SplunkDestinationUpdate)

-- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose
-- sends your data.
splunkDestinationUpdate_hECEndpoint :: Lens.Lens' SplunkDestinationUpdate (Prelude.Maybe Prelude.Text)
splunkDestinationUpdate_hECEndpoint = Lens.lens (\SplunkDestinationUpdate' {hECEndpoint} -> hECEndpoint) (\s@SplunkDestinationUpdate' {} a -> s {hECEndpoint = a} :: SplunkDestinationUpdate)

-- | This type can be either \"Raw\" or \"Event.\"
splunkDestinationUpdate_hECEndpointType :: Lens.Lens' SplunkDestinationUpdate (Prelude.Maybe HECEndpointType)
splunkDestinationUpdate_hECEndpointType = Lens.lens (\SplunkDestinationUpdate' {hECEndpointType} -> hECEndpointType) (\s@SplunkDestinationUpdate' {} a -> s {hECEndpointType = a} :: SplunkDestinationUpdate)

-- | A GUID that you obtain from your Splunk cluster when you create a new
-- HEC endpoint.
splunkDestinationUpdate_hECToken :: Lens.Lens' SplunkDestinationUpdate (Prelude.Maybe Prelude.Text)
splunkDestinationUpdate_hECToken = Lens.lens (\SplunkDestinationUpdate' {hECToken} -> hECToken) (\s@SplunkDestinationUpdate' {} a -> s {hECToken = a} :: SplunkDestinationUpdate)

-- | The data processing configuration.
splunkDestinationUpdate_processingConfiguration :: Lens.Lens' SplunkDestinationUpdate (Prelude.Maybe ProcessingConfiguration)
splunkDestinationUpdate_processingConfiguration = Lens.lens (\SplunkDestinationUpdate' {processingConfiguration} -> processingConfiguration) (\s@SplunkDestinationUpdate' {} a -> s {processingConfiguration = a} :: SplunkDestinationUpdate)

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- data to Splunk or if it doesn\'t receive an acknowledgment of receipt
-- from Splunk.
splunkDestinationUpdate_retryOptions :: Lens.Lens' SplunkDestinationUpdate (Prelude.Maybe SplunkRetryOptions)
splunkDestinationUpdate_retryOptions = Lens.lens (\SplunkDestinationUpdate' {retryOptions} -> retryOptions) (\s@SplunkDestinationUpdate' {} a -> s {retryOptions = a} :: SplunkDestinationUpdate)

-- | Specifies how you want Kinesis Data Firehose to back up documents to
-- Amazon S3. When set to @FailedDocumentsOnly@, Kinesis Data Firehose
-- writes any data that could not be indexed to the configured Amazon S3
-- destination. When set to @AllEvents@, Kinesis Data Firehose delivers all
-- incoming records to Amazon S3, and also writes failed documents to
-- Amazon S3. The default value is @FailedEventsOnly@.
--
-- You can update this backup mode from @FailedEventsOnly@ to @AllEvents@.
-- You can\'t update it from @AllEvents@ to @FailedEventsOnly@.
splunkDestinationUpdate_s3BackupMode :: Lens.Lens' SplunkDestinationUpdate (Prelude.Maybe SplunkS3BackupMode)
splunkDestinationUpdate_s3BackupMode = Lens.lens (\SplunkDestinationUpdate' {s3BackupMode} -> s3BackupMode) (\s@SplunkDestinationUpdate' {} a -> s {s3BackupMode = a} :: SplunkDestinationUpdate)

-- | Your update to the configuration of the backup Amazon S3 location.
splunkDestinationUpdate_s3Update :: Lens.Lens' SplunkDestinationUpdate (Prelude.Maybe S3DestinationUpdate)
splunkDestinationUpdate_s3Update = Lens.lens (\SplunkDestinationUpdate' {s3Update} -> s3Update) (\s@SplunkDestinationUpdate' {} a -> s {s3Update = a} :: SplunkDestinationUpdate)

instance Prelude.Hashable SplunkDestinationUpdate where
  hashWithSalt _salt SplunkDestinationUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLoggingOptions
      `Prelude.hashWithSalt` hECAcknowledgmentTimeoutInSeconds
      `Prelude.hashWithSalt` hECEndpoint
      `Prelude.hashWithSalt` hECEndpointType
      `Prelude.hashWithSalt` hECToken
      `Prelude.hashWithSalt` processingConfiguration
      `Prelude.hashWithSalt` retryOptions
      `Prelude.hashWithSalt` s3BackupMode
      `Prelude.hashWithSalt` s3Update

instance Prelude.NFData SplunkDestinationUpdate where
  rnf SplunkDestinationUpdate' {..} =
    Prelude.rnf cloudWatchLoggingOptions
      `Prelude.seq` Prelude.rnf hECAcknowledgmentTimeoutInSeconds
      `Prelude.seq` Prelude.rnf hECEndpoint
      `Prelude.seq` Prelude.rnf hECEndpointType
      `Prelude.seq` Prelude.rnf hECToken
      `Prelude.seq` Prelude.rnf processingConfiguration
      `Prelude.seq` Prelude.rnf retryOptions
      `Prelude.seq` Prelude.rnf s3BackupMode
      `Prelude.seq` Prelude.rnf s3Update

instance Data.ToJSON SplunkDestinationUpdate where
  toJSON SplunkDestinationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudWatchLoggingOptions" Data..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("HECAcknowledgmentTimeoutInSeconds" Data..=)
              Prelude.<$> hECAcknowledgmentTimeoutInSeconds,
            ("HECEndpoint" Data..=) Prelude.<$> hECEndpoint,
            ("HECEndpointType" Data..=)
              Prelude.<$> hECEndpointType,
            ("HECToken" Data..=) Prelude.<$> hECToken,
            ("ProcessingConfiguration" Data..=)
              Prelude.<$> processingConfiguration,
            ("RetryOptions" Data..=) Prelude.<$> retryOptions,
            ("S3BackupMode" Data..=) Prelude.<$> s3BackupMode,
            ("S3Update" Data..=) Prelude.<$> s3Update
          ]
      )
