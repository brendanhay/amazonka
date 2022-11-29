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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.SplunkDestinationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
  { -- | A GUID that you obtain from your Splunk cluster when you create a new
    -- HEC endpoint.
    hECToken :: Prelude.Maybe Prelude.Text,
    -- | The data processing configuration.
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | Your update to the configuration of the backup Amazon S3 location.
    s3Update :: Prelude.Maybe S3DestinationUpdate,
    -- | This type can be either \"Raw\" or \"Event.\"
    hECEndpointType :: Prelude.Maybe HECEndpointType,
    -- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose
    -- sends your data.
    hECEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
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
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver
    -- data to Splunk or if it doesn\'t receive an acknowledgment of receipt
    -- from Splunk.
    retryOptions :: Prelude.Maybe SplunkRetryOptions,
    -- | The amount of time that Kinesis Data Firehose waits to receive an
    -- acknowledgment from Splunk after it sends data. At the end of the
    -- timeout period, Kinesis Data Firehose either tries to send the data
    -- again or considers it an error, based on your retry settings.
    hECAcknowledgmentTimeoutInSeconds :: Prelude.Maybe Prelude.Natural
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
-- 'hECToken', 'splunkDestinationUpdate_hECToken' - A GUID that you obtain from your Splunk cluster when you create a new
-- HEC endpoint.
--
-- 'processingConfiguration', 'splunkDestinationUpdate_processingConfiguration' - The data processing configuration.
--
-- 's3Update', 'splunkDestinationUpdate_s3Update' - Your update to the configuration of the backup Amazon S3 location.
--
-- 'hECEndpointType', 'splunkDestinationUpdate_hECEndpointType' - This type can be either \"Raw\" or \"Event.\"
--
-- 'hECEndpoint', 'splunkDestinationUpdate_hECEndpoint' - The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose
-- sends your data.
--
-- 'cloudWatchLoggingOptions', 'splunkDestinationUpdate_cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
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
-- 'retryOptions', 'splunkDestinationUpdate_retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver
-- data to Splunk or if it doesn\'t receive an acknowledgment of receipt
-- from Splunk.
--
-- 'hECAcknowledgmentTimeoutInSeconds', 'splunkDestinationUpdate_hECAcknowledgmentTimeoutInSeconds' - The amount of time that Kinesis Data Firehose waits to receive an
-- acknowledgment from Splunk after it sends data. At the end of the
-- timeout period, Kinesis Data Firehose either tries to send the data
-- again or considers it an error, based on your retry settings.
newSplunkDestinationUpdate ::
  SplunkDestinationUpdate
newSplunkDestinationUpdate =
  SplunkDestinationUpdate'
    { hECToken =
        Prelude.Nothing,
      processingConfiguration = Prelude.Nothing,
      s3Update = Prelude.Nothing,
      hECEndpointType = Prelude.Nothing,
      hECEndpoint = Prelude.Nothing,
      cloudWatchLoggingOptions = Prelude.Nothing,
      s3BackupMode = Prelude.Nothing,
      retryOptions = Prelude.Nothing,
      hECAcknowledgmentTimeoutInSeconds =
        Prelude.Nothing
    }

-- | A GUID that you obtain from your Splunk cluster when you create a new
-- HEC endpoint.
splunkDestinationUpdate_hECToken :: Lens.Lens' SplunkDestinationUpdate (Prelude.Maybe Prelude.Text)
splunkDestinationUpdate_hECToken = Lens.lens (\SplunkDestinationUpdate' {hECToken} -> hECToken) (\s@SplunkDestinationUpdate' {} a -> s {hECToken = a} :: SplunkDestinationUpdate)

-- | The data processing configuration.
splunkDestinationUpdate_processingConfiguration :: Lens.Lens' SplunkDestinationUpdate (Prelude.Maybe ProcessingConfiguration)
splunkDestinationUpdate_processingConfiguration = Lens.lens (\SplunkDestinationUpdate' {processingConfiguration} -> processingConfiguration) (\s@SplunkDestinationUpdate' {} a -> s {processingConfiguration = a} :: SplunkDestinationUpdate)

-- | Your update to the configuration of the backup Amazon S3 location.
splunkDestinationUpdate_s3Update :: Lens.Lens' SplunkDestinationUpdate (Prelude.Maybe S3DestinationUpdate)
splunkDestinationUpdate_s3Update = Lens.lens (\SplunkDestinationUpdate' {s3Update} -> s3Update) (\s@SplunkDestinationUpdate' {} a -> s {s3Update = a} :: SplunkDestinationUpdate)

-- | This type can be either \"Raw\" or \"Event.\"
splunkDestinationUpdate_hECEndpointType :: Lens.Lens' SplunkDestinationUpdate (Prelude.Maybe HECEndpointType)
splunkDestinationUpdate_hECEndpointType = Lens.lens (\SplunkDestinationUpdate' {hECEndpointType} -> hECEndpointType) (\s@SplunkDestinationUpdate' {} a -> s {hECEndpointType = a} :: SplunkDestinationUpdate)

-- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose
-- sends your data.
splunkDestinationUpdate_hECEndpoint :: Lens.Lens' SplunkDestinationUpdate (Prelude.Maybe Prelude.Text)
splunkDestinationUpdate_hECEndpoint = Lens.lens (\SplunkDestinationUpdate' {hECEndpoint} -> hECEndpoint) (\s@SplunkDestinationUpdate' {} a -> s {hECEndpoint = a} :: SplunkDestinationUpdate)

-- | The Amazon CloudWatch logging options for your delivery stream.
splunkDestinationUpdate_cloudWatchLoggingOptions :: Lens.Lens' SplunkDestinationUpdate (Prelude.Maybe CloudWatchLoggingOptions)
splunkDestinationUpdate_cloudWatchLoggingOptions = Lens.lens (\SplunkDestinationUpdate' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@SplunkDestinationUpdate' {} a -> s {cloudWatchLoggingOptions = a} :: SplunkDestinationUpdate)

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

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- data to Splunk or if it doesn\'t receive an acknowledgment of receipt
-- from Splunk.
splunkDestinationUpdate_retryOptions :: Lens.Lens' SplunkDestinationUpdate (Prelude.Maybe SplunkRetryOptions)
splunkDestinationUpdate_retryOptions = Lens.lens (\SplunkDestinationUpdate' {retryOptions} -> retryOptions) (\s@SplunkDestinationUpdate' {} a -> s {retryOptions = a} :: SplunkDestinationUpdate)

-- | The amount of time that Kinesis Data Firehose waits to receive an
-- acknowledgment from Splunk after it sends data. At the end of the
-- timeout period, Kinesis Data Firehose either tries to send the data
-- again or considers it an error, based on your retry settings.
splunkDestinationUpdate_hECAcknowledgmentTimeoutInSeconds :: Lens.Lens' SplunkDestinationUpdate (Prelude.Maybe Prelude.Natural)
splunkDestinationUpdate_hECAcknowledgmentTimeoutInSeconds = Lens.lens (\SplunkDestinationUpdate' {hECAcknowledgmentTimeoutInSeconds} -> hECAcknowledgmentTimeoutInSeconds) (\s@SplunkDestinationUpdate' {} a -> s {hECAcknowledgmentTimeoutInSeconds = a} :: SplunkDestinationUpdate)

instance Prelude.Hashable SplunkDestinationUpdate where
  hashWithSalt _salt SplunkDestinationUpdate' {..} =
    _salt `Prelude.hashWithSalt` hECToken
      `Prelude.hashWithSalt` processingConfiguration
      `Prelude.hashWithSalt` s3Update
      `Prelude.hashWithSalt` hECEndpointType
      `Prelude.hashWithSalt` hECEndpoint
      `Prelude.hashWithSalt` cloudWatchLoggingOptions
      `Prelude.hashWithSalt` s3BackupMode
      `Prelude.hashWithSalt` retryOptions
      `Prelude.hashWithSalt` hECAcknowledgmentTimeoutInSeconds

instance Prelude.NFData SplunkDestinationUpdate where
  rnf SplunkDestinationUpdate' {..} =
    Prelude.rnf hECToken
      `Prelude.seq` Prelude.rnf processingConfiguration
      `Prelude.seq` Prelude.rnf s3Update
      `Prelude.seq` Prelude.rnf hECEndpointType
      `Prelude.seq` Prelude.rnf hECEndpoint
      `Prelude.seq` Prelude.rnf cloudWatchLoggingOptions
      `Prelude.seq` Prelude.rnf s3BackupMode
      `Prelude.seq` Prelude.rnf retryOptions
      `Prelude.seq` Prelude.rnf hECAcknowledgmentTimeoutInSeconds

instance Core.ToJSON SplunkDestinationUpdate where
  toJSON SplunkDestinationUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("HECToken" Core..=) Prelude.<$> hECToken,
            ("ProcessingConfiguration" Core..=)
              Prelude.<$> processingConfiguration,
            ("S3Update" Core..=) Prelude.<$> s3Update,
            ("HECEndpointType" Core..=)
              Prelude.<$> hECEndpointType,
            ("HECEndpoint" Core..=) Prelude.<$> hECEndpoint,
            ("CloudWatchLoggingOptions" Core..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("S3BackupMode" Core..=) Prelude.<$> s3BackupMode,
            ("RetryOptions" Core..=) Prelude.<$> retryOptions,
            ("HECAcknowledgmentTimeoutInSeconds" Core..=)
              Prelude.<$> hECAcknowledgmentTimeoutInSeconds
          ]
      )
