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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    updateTimelineEvent_eventData,
    updateTimelineEvent_clientToken,
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newUpdateTimelineEvent' smart constructor.
data UpdateTimelineEvent = UpdateTimelineEvent'
  { -- | A short description of the event.
    eventData :: Prelude.Maybe Prelude.Text,
    -- | A token ensuring that the action is called only once with the specified
    -- details.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The time that the event occurred.
    eventTime :: Prelude.Maybe Core.POSIX,
    -- | The type of the event. You can update events of type @Custom Event@.
    eventType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the event you are updating. You can find this by using
    -- @ListTimelineEvents@.
    eventId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the incident that the timeline event
    -- is part of.
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
-- 'eventData', 'updateTimelineEvent_eventData' - A short description of the event.
--
-- 'clientToken', 'updateTimelineEvent_clientToken' - A token ensuring that the action is called only once with the specified
-- details.
--
-- 'eventTime', 'updateTimelineEvent_eventTime' - The time that the event occurred.
--
-- 'eventType', 'updateTimelineEvent_eventType' - The type of the event. You can update events of type @Custom Event@.
--
-- 'eventId', 'updateTimelineEvent_eventId' - The ID of the event you are updating. You can find this by using
-- @ListTimelineEvents@.
--
-- 'incidentRecordArn', 'updateTimelineEvent_incidentRecordArn' - The Amazon Resource Name (ARN) of the incident that the timeline event
-- is part of.
newUpdateTimelineEvent ::
  -- | 'eventId'
  Prelude.Text ->
  -- | 'incidentRecordArn'
  Prelude.Text ->
  UpdateTimelineEvent
newUpdateTimelineEvent pEventId_ pIncidentRecordArn_ =
  UpdateTimelineEvent'
    { eventData = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      eventTime = Prelude.Nothing,
      eventType = Prelude.Nothing,
      eventId = pEventId_,
      incidentRecordArn = pIncidentRecordArn_
    }

-- | A short description of the event.
updateTimelineEvent_eventData :: Lens.Lens' UpdateTimelineEvent (Prelude.Maybe Prelude.Text)
updateTimelineEvent_eventData = Lens.lens (\UpdateTimelineEvent' {eventData} -> eventData) (\s@UpdateTimelineEvent' {} a -> s {eventData = a} :: UpdateTimelineEvent)

-- | A token ensuring that the action is called only once with the specified
-- details.
updateTimelineEvent_clientToken :: Lens.Lens' UpdateTimelineEvent (Prelude.Maybe Prelude.Text)
updateTimelineEvent_clientToken = Lens.lens (\UpdateTimelineEvent' {clientToken} -> clientToken) (\s@UpdateTimelineEvent' {} a -> s {clientToken = a} :: UpdateTimelineEvent)

-- | The time that the event occurred.
updateTimelineEvent_eventTime :: Lens.Lens' UpdateTimelineEvent (Prelude.Maybe Prelude.UTCTime)
updateTimelineEvent_eventTime = Lens.lens (\UpdateTimelineEvent' {eventTime} -> eventTime) (\s@UpdateTimelineEvent' {} a -> s {eventTime = a} :: UpdateTimelineEvent) Prelude.. Lens.mapping Core._Time

-- | The type of the event. You can update events of type @Custom Event@.
updateTimelineEvent_eventType :: Lens.Lens' UpdateTimelineEvent (Prelude.Maybe Prelude.Text)
updateTimelineEvent_eventType = Lens.lens (\UpdateTimelineEvent' {eventType} -> eventType) (\s@UpdateTimelineEvent' {} a -> s {eventType = a} :: UpdateTimelineEvent)

-- | The ID of the event you are updating. You can find this by using
-- @ListTimelineEvents@.
updateTimelineEvent_eventId :: Lens.Lens' UpdateTimelineEvent Prelude.Text
updateTimelineEvent_eventId = Lens.lens (\UpdateTimelineEvent' {eventId} -> eventId) (\s@UpdateTimelineEvent' {} a -> s {eventId = a} :: UpdateTimelineEvent)

-- | The Amazon Resource Name (ARN) of the incident that the timeline event
-- is part of.
updateTimelineEvent_incidentRecordArn :: Lens.Lens' UpdateTimelineEvent Prelude.Text
updateTimelineEvent_incidentRecordArn = Lens.lens (\UpdateTimelineEvent' {incidentRecordArn} -> incidentRecordArn) (\s@UpdateTimelineEvent' {} a -> s {incidentRecordArn = a} :: UpdateTimelineEvent)

instance Core.AWSRequest UpdateTimelineEvent where
  type
    AWSResponse UpdateTimelineEvent =
      UpdateTimelineEventResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateTimelineEventResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTimelineEvent where
  hashWithSalt salt' UpdateTimelineEvent' {..} =
    salt' `Prelude.hashWithSalt` incidentRecordArn
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` eventTime
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` eventData

instance Prelude.NFData UpdateTimelineEvent where
  rnf UpdateTimelineEvent' {..} =
    Prelude.rnf eventData
      `Prelude.seq` Prelude.rnf incidentRecordArn
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf eventType
      `Prelude.seq` Prelude.rnf eventTime
      `Prelude.seq` Prelude.rnf clientToken

instance Core.ToHeaders UpdateTimelineEvent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateTimelineEvent where
  toJSON UpdateTimelineEvent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("eventData" Core..=) Prelude.<$> eventData,
            ("clientToken" Core..=) Prelude.<$> clientToken,
            ("eventTime" Core..=) Prelude.<$> eventTime,
            ("eventType" Core..=) Prelude.<$> eventType,
            Prelude.Just ("eventId" Core..= eventId),
            Prelude.Just
              ("incidentRecordArn" Core..= incidentRecordArn)
          ]
      )

instance Core.ToPath UpdateTimelineEvent where
  toPath = Prelude.const "/updateTimelineEvent"

instance Core.ToQuery UpdateTimelineEvent where
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
