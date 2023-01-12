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
-- Module      : Amazonka.SSMIncidents.GetTimelineEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a timeline event based on its ID and incident record.
module Amazonka.SSMIncidents.GetTimelineEvent
  ( -- * Creating a Request
    GetTimelineEvent (..),
    newGetTimelineEvent,

    -- * Request Lenses
    getTimelineEvent_eventId,
    getTimelineEvent_incidentRecordArn,

    -- * Destructuring the Response
    GetTimelineEventResponse (..),
    newGetTimelineEventResponse,

    -- * Response Lenses
    getTimelineEventResponse_httpStatus,
    getTimelineEventResponse_event,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newGetTimelineEvent' smart constructor.
data GetTimelineEvent = GetTimelineEvent'
  { -- | The ID of the event. You can get an event\'s ID when you create it, or
    -- by using @ListTimelineEvents@.
    eventId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the incident that includes the
    -- timeline event.
    incidentRecordArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTimelineEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventId', 'getTimelineEvent_eventId' - The ID of the event. You can get an event\'s ID when you create it, or
-- by using @ListTimelineEvents@.
--
-- 'incidentRecordArn', 'getTimelineEvent_incidentRecordArn' - The Amazon Resource Name (ARN) of the incident that includes the
-- timeline event.
newGetTimelineEvent ::
  -- | 'eventId'
  Prelude.Text ->
  -- | 'incidentRecordArn'
  Prelude.Text ->
  GetTimelineEvent
newGetTimelineEvent pEventId_ pIncidentRecordArn_ =
  GetTimelineEvent'
    { eventId = pEventId_,
      incidentRecordArn = pIncidentRecordArn_
    }

-- | The ID of the event. You can get an event\'s ID when you create it, or
-- by using @ListTimelineEvents@.
getTimelineEvent_eventId :: Lens.Lens' GetTimelineEvent Prelude.Text
getTimelineEvent_eventId = Lens.lens (\GetTimelineEvent' {eventId} -> eventId) (\s@GetTimelineEvent' {} a -> s {eventId = a} :: GetTimelineEvent)

-- | The Amazon Resource Name (ARN) of the incident that includes the
-- timeline event.
getTimelineEvent_incidentRecordArn :: Lens.Lens' GetTimelineEvent Prelude.Text
getTimelineEvent_incidentRecordArn = Lens.lens (\GetTimelineEvent' {incidentRecordArn} -> incidentRecordArn) (\s@GetTimelineEvent' {} a -> s {incidentRecordArn = a} :: GetTimelineEvent)

instance Core.AWSRequest GetTimelineEvent where
  type
    AWSResponse GetTimelineEvent =
      GetTimelineEventResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTimelineEventResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "event")
      )

instance Prelude.Hashable GetTimelineEvent where
  hashWithSalt _salt GetTimelineEvent' {..} =
    _salt `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` incidentRecordArn

instance Prelude.NFData GetTimelineEvent where
  rnf GetTimelineEvent' {..} =
    Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf incidentRecordArn

instance Data.ToHeaders GetTimelineEvent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetTimelineEvent where
  toPath = Prelude.const "/getTimelineEvent"

instance Data.ToQuery GetTimelineEvent where
  toQuery GetTimelineEvent' {..} =
    Prelude.mconcat
      [ "eventId" Data.=: eventId,
        "incidentRecordArn" Data.=: incidentRecordArn
      ]

-- | /See:/ 'newGetTimelineEventResponse' smart constructor.
data GetTimelineEventResponse = GetTimelineEventResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Details about the timeline event.
    event :: TimelineEvent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTimelineEventResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getTimelineEventResponse_httpStatus' - The response's http status code.
--
-- 'event', 'getTimelineEventResponse_event' - Details about the timeline event.
newGetTimelineEventResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'event'
  TimelineEvent ->
  GetTimelineEventResponse
newGetTimelineEventResponse pHttpStatus_ pEvent_ =
  GetTimelineEventResponse'
    { httpStatus =
        pHttpStatus_,
      event = pEvent_
    }

-- | The response's http status code.
getTimelineEventResponse_httpStatus :: Lens.Lens' GetTimelineEventResponse Prelude.Int
getTimelineEventResponse_httpStatus = Lens.lens (\GetTimelineEventResponse' {httpStatus} -> httpStatus) (\s@GetTimelineEventResponse' {} a -> s {httpStatus = a} :: GetTimelineEventResponse)

-- | Details about the timeline event.
getTimelineEventResponse_event :: Lens.Lens' GetTimelineEventResponse TimelineEvent
getTimelineEventResponse_event = Lens.lens (\GetTimelineEventResponse' {event} -> event) (\s@GetTimelineEventResponse' {} a -> s {event = a} :: GetTimelineEventResponse)

instance Prelude.NFData GetTimelineEventResponse where
  rnf GetTimelineEventResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf event
