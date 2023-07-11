{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.PinpointSmsVoiceV2.UpdateEventDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing event destination in a configuration set. You can
-- update the IAM role ARN for CloudWatch Logs and Kinesis Data Firehose.
-- You can also enable or disable the event destination.
--
-- You may want to update an event destination to change its matching event
-- types or updating the destination resource ARN. You can\'t change an
-- event destination\'s type between CloudWatch Logs, Kinesis Data
-- Firehose, and Amazon SNS.
module Amazonka.PinpointSmsVoiceV2.UpdateEventDestination
  ( -- * Creating a Request
    UpdateEventDestination (..),
    newUpdateEventDestination,

    -- * Request Lenses
    updateEventDestination_cloudWatchLogsDestination,
    updateEventDestination_enabled,
    updateEventDestination_kinesisFirehoseDestination,
    updateEventDestination_matchingEventTypes,
    updateEventDestination_snsDestination,
    updateEventDestination_configurationSetName,
    updateEventDestination_eventDestinationName,

    -- * Destructuring the Response
    UpdateEventDestinationResponse (..),
    newUpdateEventDestinationResponse,

    -- * Response Lenses
    updateEventDestinationResponse_configurationSetArn,
    updateEventDestinationResponse_configurationSetName,
    updateEventDestinationResponse_eventDestination,
    updateEventDestinationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEventDestination' smart constructor.
data UpdateEventDestination = UpdateEventDestination'
  { -- | An object that contains information about an event destination that
    -- sends data to CloudWatch Logs.
    cloudWatchLogsDestination :: Prelude.Maybe CloudWatchLogsDestination,
    -- | When set to true logging is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | An object that contains information about an event destination for
    -- logging to Kinesis Data Firehose.
    kinesisFirehoseDestination :: Prelude.Maybe KinesisFirehoseDestination,
    -- | An array of event types that determine which events to log.
    matchingEventTypes :: Prelude.Maybe (Prelude.NonEmpty EventType),
    -- | An object that contains information about an event destination that
    -- sends data to Amazon SNS.
    snsDestination :: Prelude.Maybe SnsDestination,
    -- | The configuration set to update with the new event destination. Valid
    -- values for this can be the ConfigurationSetName or ConfigurationSetArn.
    configurationSetName :: Prelude.Text,
    -- | The name to use for the event destination.
    eventDestinationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEventDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogsDestination', 'updateEventDestination_cloudWatchLogsDestination' - An object that contains information about an event destination that
-- sends data to CloudWatch Logs.
--
-- 'enabled', 'updateEventDestination_enabled' - When set to true logging is enabled.
--
-- 'kinesisFirehoseDestination', 'updateEventDestination_kinesisFirehoseDestination' - An object that contains information about an event destination for
-- logging to Kinesis Data Firehose.
--
-- 'matchingEventTypes', 'updateEventDestination_matchingEventTypes' - An array of event types that determine which events to log.
--
-- 'snsDestination', 'updateEventDestination_snsDestination' - An object that contains information about an event destination that
-- sends data to Amazon SNS.
--
-- 'configurationSetName', 'updateEventDestination_configurationSetName' - The configuration set to update with the new event destination. Valid
-- values for this can be the ConfigurationSetName or ConfigurationSetArn.
--
-- 'eventDestinationName', 'updateEventDestination_eventDestinationName' - The name to use for the event destination.
newUpdateEventDestination ::
  -- | 'configurationSetName'
  Prelude.Text ->
  -- | 'eventDestinationName'
  Prelude.Text ->
  UpdateEventDestination
newUpdateEventDestination
  pConfigurationSetName_
  pEventDestinationName_ =
    UpdateEventDestination'
      { cloudWatchLogsDestination =
          Prelude.Nothing,
        enabled = Prelude.Nothing,
        kinesisFirehoseDestination = Prelude.Nothing,
        matchingEventTypes = Prelude.Nothing,
        snsDestination = Prelude.Nothing,
        configurationSetName = pConfigurationSetName_,
        eventDestinationName = pEventDestinationName_
      }

-- | An object that contains information about an event destination that
-- sends data to CloudWatch Logs.
updateEventDestination_cloudWatchLogsDestination :: Lens.Lens' UpdateEventDestination (Prelude.Maybe CloudWatchLogsDestination)
updateEventDestination_cloudWatchLogsDestination = Lens.lens (\UpdateEventDestination' {cloudWatchLogsDestination} -> cloudWatchLogsDestination) (\s@UpdateEventDestination' {} a -> s {cloudWatchLogsDestination = a} :: UpdateEventDestination)

-- | When set to true logging is enabled.
updateEventDestination_enabled :: Lens.Lens' UpdateEventDestination (Prelude.Maybe Prelude.Bool)
updateEventDestination_enabled = Lens.lens (\UpdateEventDestination' {enabled} -> enabled) (\s@UpdateEventDestination' {} a -> s {enabled = a} :: UpdateEventDestination)

-- | An object that contains information about an event destination for
-- logging to Kinesis Data Firehose.
updateEventDestination_kinesisFirehoseDestination :: Lens.Lens' UpdateEventDestination (Prelude.Maybe KinesisFirehoseDestination)
updateEventDestination_kinesisFirehoseDestination = Lens.lens (\UpdateEventDestination' {kinesisFirehoseDestination} -> kinesisFirehoseDestination) (\s@UpdateEventDestination' {} a -> s {kinesisFirehoseDestination = a} :: UpdateEventDestination)

-- | An array of event types that determine which events to log.
updateEventDestination_matchingEventTypes :: Lens.Lens' UpdateEventDestination (Prelude.Maybe (Prelude.NonEmpty EventType))
updateEventDestination_matchingEventTypes = Lens.lens (\UpdateEventDestination' {matchingEventTypes} -> matchingEventTypes) (\s@UpdateEventDestination' {} a -> s {matchingEventTypes = a} :: UpdateEventDestination) Prelude.. Lens.mapping Lens.coerced

-- | An object that contains information about an event destination that
-- sends data to Amazon SNS.
updateEventDestination_snsDestination :: Lens.Lens' UpdateEventDestination (Prelude.Maybe SnsDestination)
updateEventDestination_snsDestination = Lens.lens (\UpdateEventDestination' {snsDestination} -> snsDestination) (\s@UpdateEventDestination' {} a -> s {snsDestination = a} :: UpdateEventDestination)

-- | The configuration set to update with the new event destination. Valid
-- values for this can be the ConfigurationSetName or ConfigurationSetArn.
updateEventDestination_configurationSetName :: Lens.Lens' UpdateEventDestination Prelude.Text
updateEventDestination_configurationSetName = Lens.lens (\UpdateEventDestination' {configurationSetName} -> configurationSetName) (\s@UpdateEventDestination' {} a -> s {configurationSetName = a} :: UpdateEventDestination)

-- | The name to use for the event destination.
updateEventDestination_eventDestinationName :: Lens.Lens' UpdateEventDestination Prelude.Text
updateEventDestination_eventDestinationName = Lens.lens (\UpdateEventDestination' {eventDestinationName} -> eventDestinationName) (\s@UpdateEventDestination' {} a -> s {eventDestinationName = a} :: UpdateEventDestination)

instance Core.AWSRequest UpdateEventDestination where
  type
    AWSResponse UpdateEventDestination =
      UpdateEventDestinationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEventDestinationResponse'
            Prelude.<$> (x Data..?> "ConfigurationSetArn")
            Prelude.<*> (x Data..?> "ConfigurationSetName")
            Prelude.<*> (x Data..?> "EventDestination")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEventDestination where
  hashWithSalt _salt UpdateEventDestination' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogsDestination
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` kinesisFirehoseDestination
      `Prelude.hashWithSalt` matchingEventTypes
      `Prelude.hashWithSalt` snsDestination
      `Prelude.hashWithSalt` configurationSetName
      `Prelude.hashWithSalt` eventDestinationName

instance Prelude.NFData UpdateEventDestination where
  rnf UpdateEventDestination' {..} =
    Prelude.rnf cloudWatchLogsDestination
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf kinesisFirehoseDestination
      `Prelude.seq` Prelude.rnf matchingEventTypes
      `Prelude.seq` Prelude.rnf snsDestination
      `Prelude.seq` Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf eventDestinationName

instance Data.ToHeaders UpdateEventDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.UpdateEventDestination" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateEventDestination where
  toJSON UpdateEventDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudWatchLogsDestination" Data..=)
              Prelude.<$> cloudWatchLogsDestination,
            ("Enabled" Data..=) Prelude.<$> enabled,
            ("KinesisFirehoseDestination" Data..=)
              Prelude.<$> kinesisFirehoseDestination,
            ("MatchingEventTypes" Data..=)
              Prelude.<$> matchingEventTypes,
            ("SnsDestination" Data..=)
              Prelude.<$> snsDestination,
            Prelude.Just
              ( "ConfigurationSetName"
                  Data..= configurationSetName
              ),
            Prelude.Just
              ( "EventDestinationName"
                  Data..= eventDestinationName
              )
          ]
      )

instance Data.ToPath UpdateEventDestination where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateEventDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEventDestinationResponse' smart constructor.
data UpdateEventDestinationResponse = UpdateEventDestinationResponse'
  { -- | The Amazon Resource Name (ARN) for the ConfigurationSet that was
    -- updated.
    configurationSetArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration set.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | An EventDestination object containing the details of where events will
    -- be logged.
    eventDestination :: Prelude.Maybe EventDestination,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEventDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetArn', 'updateEventDestinationResponse_configurationSetArn' - The Amazon Resource Name (ARN) for the ConfigurationSet that was
-- updated.
--
-- 'configurationSetName', 'updateEventDestinationResponse_configurationSetName' - The name of the configuration set.
--
-- 'eventDestination', 'updateEventDestinationResponse_eventDestination' - An EventDestination object containing the details of where events will
-- be logged.
--
-- 'httpStatus', 'updateEventDestinationResponse_httpStatus' - The response's http status code.
newUpdateEventDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEventDestinationResponse
newUpdateEventDestinationResponse pHttpStatus_ =
  UpdateEventDestinationResponse'
    { configurationSetArn =
        Prelude.Nothing,
      configurationSetName = Prelude.Nothing,
      eventDestination = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the ConfigurationSet that was
-- updated.
updateEventDestinationResponse_configurationSetArn :: Lens.Lens' UpdateEventDestinationResponse (Prelude.Maybe Prelude.Text)
updateEventDestinationResponse_configurationSetArn = Lens.lens (\UpdateEventDestinationResponse' {configurationSetArn} -> configurationSetArn) (\s@UpdateEventDestinationResponse' {} a -> s {configurationSetArn = a} :: UpdateEventDestinationResponse)

-- | The name of the configuration set.
updateEventDestinationResponse_configurationSetName :: Lens.Lens' UpdateEventDestinationResponse (Prelude.Maybe Prelude.Text)
updateEventDestinationResponse_configurationSetName = Lens.lens (\UpdateEventDestinationResponse' {configurationSetName} -> configurationSetName) (\s@UpdateEventDestinationResponse' {} a -> s {configurationSetName = a} :: UpdateEventDestinationResponse)

-- | An EventDestination object containing the details of where events will
-- be logged.
updateEventDestinationResponse_eventDestination :: Lens.Lens' UpdateEventDestinationResponse (Prelude.Maybe EventDestination)
updateEventDestinationResponse_eventDestination = Lens.lens (\UpdateEventDestinationResponse' {eventDestination} -> eventDestination) (\s@UpdateEventDestinationResponse' {} a -> s {eventDestination = a} :: UpdateEventDestinationResponse)

-- | The response's http status code.
updateEventDestinationResponse_httpStatus :: Lens.Lens' UpdateEventDestinationResponse Prelude.Int
updateEventDestinationResponse_httpStatus = Lens.lens (\UpdateEventDestinationResponse' {httpStatus} -> httpStatus) (\s@UpdateEventDestinationResponse' {} a -> s {httpStatus = a} :: UpdateEventDestinationResponse)

instance
  Prelude.NFData
    UpdateEventDestinationResponse
  where
  rnf UpdateEventDestinationResponse' {..} =
    Prelude.rnf configurationSetArn
      `Prelude.seq` Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf eventDestination
      `Prelude.seq` Prelude.rnf httpStatus
