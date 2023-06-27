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
-- Module      : Amazonka.SSMIncidents.UpdateTimelineEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a timeline event. You can update events of type @Custom Event@.
module Amazonka.SSMIncidents.UpdateTimelineEvent
  ( -- * Creating a Request
    UpdateTimelineEvent (..),
    newUpdateTimelineEvent,

    -- * Request Lenses
    updateTimelineEvent_clientToken,
    updateTimelineEvent_eventData,
    updateTimelineEvent_eventReferences,
    updateTimelineEvent_eventTime,
    updateTimelineEvent_eventType,
    updateTimelineEvent_eventId,
    updateTimelineEvent_incidentRecordArn,

    -- * Destructuring the Response
    UpdateTimelineEventResponse (..),
    newUpdateTimelineEventResponse,

    -- * Response Lenses
    updateTimelineEventResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newUpdateTimelineEvent' smart constructor.
data UpdateTimelineEvent = UpdateTimelineEvent'
  { -- | A token that ensures that a client calls the operation only once with
    -- the specified details.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A short description of the event.
    eventData :: Prelude.Maybe Prelude.Text,
    -- | Updates all existing references in a @TimelineEvent@. A reference is an
    -- Amazon Web Services resource involved or associated with the incident.
    -- To specify a reference, enter its Amazon Resource Name (ARN). You can
    -- also specify a related item associated with that resource. For example,
    -- to specify an Amazon DynamoDB (DynamoDB) table as a resource, use its
    -- ARN. You can also specify an Amazon CloudWatch metric associated with
    -- the DynamoDB table as a related item.
    --
    -- This update action overrides all existing references. If you want to
    -- keep existing references, you must specify them in the call. If you
    -- don\'t, this action removes any existing references and enters only new
    -- references.
    eventReferences :: Prelude.Maybe [EventReference],
    -- | The time that the event occurred.
    eventTime :: Prelude.Maybe Data.POSIX,
    -- | The type of event. You can update events of type @Custom Event@.
    eventType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the event to update. You can use @ListTimelineEvents@ to find
    -- an event\'s ID.
    eventId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the incident that includes the
    -- timeline event.
    incidentRecordArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTimelineEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateTimelineEvent_clientToken' - A token that ensures that a client calls the operation only once with
-- the specified details.
--
-- 'eventData', 'updateTimelineEvent_eventData' - A short description of the event.
--
-- 'eventReferences', 'updateTimelineEvent_eventReferences' - Updates all existing references in a @TimelineEvent@. A reference is an
-- Amazon Web Services resource involved or associated with the incident.
-- To specify a reference, enter its Amazon Resource Name (ARN). You can
-- also specify a related item associated with that resource. For example,
-- to specify an Amazon DynamoDB (DynamoDB) table as a resource, use its
-- ARN. You can also specify an Amazon CloudWatch metric associated with
-- the DynamoDB table as a related item.
--
-- This update action overrides all existing references. If you want to
-- keep existing references, you must specify them in the call. If you
-- don\'t, this action removes any existing references and enters only new
-- references.
--
-- 'eventTime', 'updateTimelineEvent_eventTime' - The time that the event occurred.
--
-- 'eventType', 'updateTimelineEvent_eventType' - The type of event. You can update events of type @Custom Event@.
--
-- 'eventId', 'updateTimelineEvent_eventId' - The ID of the event to update. You can use @ListTimelineEvents@ to find
-- an event\'s ID.
--
-- 'incidentRecordArn', 'updateTimelineEvent_incidentRecordArn' - The Amazon Resource Name (ARN) of the incident that includes the
-- timeline event.
newUpdateTimelineEvent ::
  -- | 'eventId'
  Prelude.Text ->
  -- | 'incidentRecordArn'
  Prelude.Text ->
  UpdateTimelineEvent
newUpdateTimelineEvent pEventId_ pIncidentRecordArn_ =
  UpdateTimelineEvent'
    { clientToken = Prelude.Nothing,
      eventData = Prelude.Nothing,
      eventReferences = Prelude.Nothing,
      eventTime = Prelude.Nothing,
      eventType = Prelude.Nothing,
      eventId = pEventId_,
      incidentRecordArn = pIncidentRecordArn_
    }

-- | A token that ensures that a client calls the operation only once with
-- the specified details.
updateTimelineEvent_clientToken :: Lens.Lens' UpdateTimelineEvent (Prelude.Maybe Prelude.Text)
updateTimelineEvent_clientToken = Lens.lens (\UpdateTimelineEvent' {clientToken} -> clientToken) (\s@UpdateTimelineEvent' {} a -> s {clientToken = a} :: UpdateTimelineEvent)

-- | A short description of the event.
updateTimelineEvent_eventData :: Lens.Lens' UpdateTimelineEvent (Prelude.Maybe Prelude.Text)
updateTimelineEvent_eventData = Lens.lens (\UpdateTimelineEvent' {eventData} -> eventData) (\s@UpdateTimelineEvent' {} a -> s {eventData = a} :: UpdateTimelineEvent)

-- | Updates all existing references in a @TimelineEvent@. A reference is an
-- Amazon Web Services resource involved or associated with the incident.
-- To specify a reference, enter its Amazon Resource Name (ARN). You can
-- also specify a related item associated with that resource. For example,
-- to specify an Amazon DynamoDB (DynamoDB) table as a resource, use its
-- ARN. You can also specify an Amazon CloudWatch metric associated with
-- the DynamoDB table as a related item.
--
-- This update action overrides all existing references. If you want to
-- keep existing references, you must specify them in the call. If you
-- don\'t, this action removes any existing references and enters only new
-- references.
updateTimelineEvent_eventReferences :: Lens.Lens' UpdateTimelineEvent (Prelude.Maybe [EventReference])
updateTimelineEvent_eventReferences = Lens.lens (\UpdateTimelineEvent' {eventReferences} -> eventReferences) (\s@UpdateTimelineEvent' {} a -> s {eventReferences = a} :: UpdateTimelineEvent) Prelude.. Lens.mapping Lens.coerced

-- | The time that the event occurred.
updateTimelineEvent_eventTime :: Lens.Lens' UpdateTimelineEvent (Prelude.Maybe Prelude.UTCTime)
updateTimelineEvent_eventTime = Lens.lens (\UpdateTimelineEvent' {eventTime} -> eventTime) (\s@UpdateTimelineEvent' {} a -> s {eventTime = a} :: UpdateTimelineEvent) Prelude.. Lens.mapping Data._Time

-- | The type of event. You can update events of type @Custom Event@.
updateTimelineEvent_eventType :: Lens.Lens' UpdateTimelineEvent (Prelude.Maybe Prelude.Text)
updateTimelineEvent_eventType = Lens.lens (\UpdateTimelineEvent' {eventType} -> eventType) (\s@UpdateTimelineEvent' {} a -> s {eventType = a} :: UpdateTimelineEvent)

-- | The ID of the event to update. You can use @ListTimelineEvents@ to find
-- an event\'s ID.
updateTimelineEvent_eventId :: Lens.Lens' UpdateTimelineEvent Prelude.Text
updateTimelineEvent_eventId = Lens.lens (\UpdateTimelineEvent' {eventId} -> eventId) (\s@UpdateTimelineEvent' {} a -> s {eventId = a} :: UpdateTimelineEvent)

-- | The Amazon Resource Name (ARN) of the incident that includes the
-- timeline event.
updateTimelineEvent_incidentRecordArn :: Lens.Lens' UpdateTimelineEvent Prelude.Text
updateTimelineEvent_incidentRecordArn = Lens.lens (\UpdateTimelineEvent' {incidentRecordArn} -> incidentRecordArn) (\s@UpdateTimelineEvent' {} a -> s {incidentRecordArn = a} :: UpdateTimelineEvent)

instance Core.AWSRequest UpdateTimelineEvent where
  type
    AWSResponse UpdateTimelineEvent =
      UpdateTimelineEventResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateTimelineEventResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTimelineEvent where
  hashWithSalt _salt UpdateTimelineEvent' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` eventData
      `Prelude.hashWithSalt` eventReferences
      `Prelude.hashWithSalt` eventTime
      `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` incidentRecordArn

instance Prelude.NFData UpdateTimelineEvent where
  rnf UpdateTimelineEvent' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf eventData
      `Prelude.seq` Prelude.rnf eventReferences
      `Prelude.seq` Prelude.rnf eventTime
      `Prelude.seq` Prelude.rnf eventType
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf incidentRecordArn

instance Data.ToHeaders UpdateTimelineEvent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTimelineEvent where
  toJSON UpdateTimelineEvent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("eventData" Data..=) Prelude.<$> eventData,
            ("eventReferences" Data..=)
              Prelude.<$> eventReferences,
            ("eventTime" Data..=) Prelude.<$> eventTime,
            ("eventType" Data..=) Prelude.<$> eventType,
            Prelude.Just ("eventId" Data..= eventId),
            Prelude.Just
              ("incidentRecordArn" Data..= incidentRecordArn)
          ]
      )

instance Data.ToPath UpdateTimelineEvent where
  toPath = Prelude.const "/updateTimelineEvent"

instance Data.ToQuery UpdateTimelineEvent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTimelineEventResponse' smart constructor.
data UpdateTimelineEventResponse = UpdateTimelineEventResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTimelineEventResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateTimelineEventResponse_httpStatus' - The response's http status code.
newUpdateTimelineEventResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTimelineEventResponse
newUpdateTimelineEventResponse pHttpStatus_ =
  UpdateTimelineEventResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateTimelineEventResponse_httpStatus :: Lens.Lens' UpdateTimelineEventResponse Prelude.Int
updateTimelineEventResponse_httpStatus = Lens.lens (\UpdateTimelineEventResponse' {httpStatus} -> httpStatus) (\s@UpdateTimelineEventResponse' {} a -> s {httpStatus = a} :: UpdateTimelineEventResponse)

instance Prelude.NFData UpdateTimelineEventResponse where
  rnf UpdateTimelineEventResponse' {..} =
    Prelude.rnf httpStatus
