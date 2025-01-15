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
-- Module      : Amazonka.FraudDetector.SendEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stores events in Amazon Fraud Detector without generating fraud
-- predictions for those events. For example, you can use @SendEvent@ to
-- upload a historical dataset, which you can then later use to train a
-- model.
module Amazonka.FraudDetector.SendEvent
  ( -- * Creating a Request
    SendEvent (..),
    newSendEvent,

    -- * Request Lenses
    sendEvent_assignedLabel,
    sendEvent_labelTimestamp,
    sendEvent_eventId,
    sendEvent_eventTypeName,
    sendEvent_eventTimestamp,
    sendEvent_eventVariables,
    sendEvent_entities,

    -- * Destructuring the Response
    SendEventResponse (..),
    newSendEventResponse,

    -- * Response Lenses
    sendEventResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendEvent' smart constructor.
data SendEvent = SendEvent'
  { -- | The label to associate with the event. Required if specifying
    -- @labelTimestamp@.
    assignedLabel :: Prelude.Maybe Prelude.Text,
    -- | The timestamp associated with the label. Required if specifying
    -- @assignedLabel@.
    labelTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The event ID to upload.
    eventId :: Prelude.Text,
    -- | The event type name of the event.
    eventTypeName :: Prelude.Text,
    -- | The timestamp that defines when the event under evaluation occurred. The
    -- timestamp must be specified using ISO 8601 standard in UTC.
    eventTimestamp :: Prelude.Text,
    -- | Names of the event type\'s variables you defined in Amazon Fraud
    -- Detector to represent data elements and their corresponding values for
    -- the event you are sending for evaluation.
    eventVariables :: Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text),
    -- | An array of entities.
    entities :: [Data.Sensitive Entity]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignedLabel', 'sendEvent_assignedLabel' - The label to associate with the event. Required if specifying
-- @labelTimestamp@.
--
-- 'labelTimestamp', 'sendEvent_labelTimestamp' - The timestamp associated with the label. Required if specifying
-- @assignedLabel@.
--
-- 'eventId', 'sendEvent_eventId' - The event ID to upload.
--
-- 'eventTypeName', 'sendEvent_eventTypeName' - The event type name of the event.
--
-- 'eventTimestamp', 'sendEvent_eventTimestamp' - The timestamp that defines when the event under evaluation occurred. The
-- timestamp must be specified using ISO 8601 standard in UTC.
--
-- 'eventVariables', 'sendEvent_eventVariables' - Names of the event type\'s variables you defined in Amazon Fraud
-- Detector to represent data elements and their corresponding values for
-- the event you are sending for evaluation.
--
-- 'entities', 'sendEvent_entities' - An array of entities.
newSendEvent ::
  -- | 'eventId'
  Prelude.Text ->
  -- | 'eventTypeName'
  Prelude.Text ->
  -- | 'eventTimestamp'
  Prelude.Text ->
  SendEvent
newSendEvent
  pEventId_
  pEventTypeName_
  pEventTimestamp_ =
    SendEvent'
      { assignedLabel = Prelude.Nothing,
        labelTimestamp = Prelude.Nothing,
        eventId = pEventId_,
        eventTypeName = pEventTypeName_,
        eventTimestamp = pEventTimestamp_,
        eventVariables = Prelude.mempty,
        entities = Prelude.mempty
      }

-- | The label to associate with the event. Required if specifying
-- @labelTimestamp@.
sendEvent_assignedLabel :: Lens.Lens' SendEvent (Prelude.Maybe Prelude.Text)
sendEvent_assignedLabel = Lens.lens (\SendEvent' {assignedLabel} -> assignedLabel) (\s@SendEvent' {} a -> s {assignedLabel = a} :: SendEvent)

-- | The timestamp associated with the label. Required if specifying
-- @assignedLabel@.
sendEvent_labelTimestamp :: Lens.Lens' SendEvent (Prelude.Maybe Prelude.Text)
sendEvent_labelTimestamp = Lens.lens (\SendEvent' {labelTimestamp} -> labelTimestamp) (\s@SendEvent' {} a -> s {labelTimestamp = a} :: SendEvent)

-- | The event ID to upload.
sendEvent_eventId :: Lens.Lens' SendEvent Prelude.Text
sendEvent_eventId = Lens.lens (\SendEvent' {eventId} -> eventId) (\s@SendEvent' {} a -> s {eventId = a} :: SendEvent)

-- | The event type name of the event.
sendEvent_eventTypeName :: Lens.Lens' SendEvent Prelude.Text
sendEvent_eventTypeName = Lens.lens (\SendEvent' {eventTypeName} -> eventTypeName) (\s@SendEvent' {} a -> s {eventTypeName = a} :: SendEvent)

-- | The timestamp that defines when the event under evaluation occurred. The
-- timestamp must be specified using ISO 8601 standard in UTC.
sendEvent_eventTimestamp :: Lens.Lens' SendEvent Prelude.Text
sendEvent_eventTimestamp = Lens.lens (\SendEvent' {eventTimestamp} -> eventTimestamp) (\s@SendEvent' {} a -> s {eventTimestamp = a} :: SendEvent)

-- | Names of the event type\'s variables you defined in Amazon Fraud
-- Detector to represent data elements and their corresponding values for
-- the event you are sending for evaluation.
sendEvent_eventVariables :: Lens.Lens' SendEvent (Prelude.HashMap Prelude.Text Prelude.Text)
sendEvent_eventVariables = Lens.lens (\SendEvent' {eventVariables} -> eventVariables) (\s@SendEvent' {} a -> s {eventVariables = a} :: SendEvent) Prelude.. Lens.coerced

-- | An array of entities.
sendEvent_entities :: Lens.Lens' SendEvent [Entity]
sendEvent_entities = Lens.lens (\SendEvent' {entities} -> entities) (\s@SendEvent' {} a -> s {entities = a} :: SendEvent) Prelude.. Lens.coerced

instance Core.AWSRequest SendEvent where
  type AWSResponse SendEvent = SendEventResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          SendEventResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendEvent where
  hashWithSalt _salt SendEvent' {..} =
    _salt
      `Prelude.hashWithSalt` assignedLabel
      `Prelude.hashWithSalt` labelTimestamp
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` eventTypeName
      `Prelude.hashWithSalt` eventTimestamp
      `Prelude.hashWithSalt` eventVariables
      `Prelude.hashWithSalt` entities

instance Prelude.NFData SendEvent where
  rnf SendEvent' {..} =
    Prelude.rnf assignedLabel `Prelude.seq`
      Prelude.rnf labelTimestamp `Prelude.seq`
        Prelude.rnf eventId `Prelude.seq`
          Prelude.rnf eventTypeName `Prelude.seq`
            Prelude.rnf eventTimestamp `Prelude.seq`
              Prelude.rnf eventVariables `Prelude.seq`
                Prelude.rnf entities

instance Data.ToHeaders SendEvent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.SendEvent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SendEvent where
  toJSON SendEvent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("assignedLabel" Data..=) Prelude.<$> assignedLabel,
            ("labelTimestamp" Data..=)
              Prelude.<$> labelTimestamp,
            Prelude.Just ("eventId" Data..= eventId),
            Prelude.Just ("eventTypeName" Data..= eventTypeName),
            Prelude.Just
              ("eventTimestamp" Data..= eventTimestamp),
            Prelude.Just
              ("eventVariables" Data..= eventVariables),
            Prelude.Just ("entities" Data..= entities)
          ]
      )

instance Data.ToPath SendEvent where
  toPath = Prelude.const "/"

instance Data.ToQuery SendEvent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendEventResponse' smart constructor.
data SendEventResponse = SendEventResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendEventResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'sendEventResponse_httpStatus' - The response's http status code.
newSendEventResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendEventResponse
newSendEventResponse pHttpStatus_ =
  SendEventResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
sendEventResponse_httpStatus :: Lens.Lens' SendEventResponse Prelude.Int
sendEventResponse_httpStatus = Lens.lens (\SendEventResponse' {httpStatus} -> httpStatus) (\s@SendEventResponse' {} a -> s {httpStatus = a} :: SendEventResponse)

instance Prelude.NFData SendEventResponse where
  rnf SendEventResponse' {..} = Prelude.rnf httpStatus
