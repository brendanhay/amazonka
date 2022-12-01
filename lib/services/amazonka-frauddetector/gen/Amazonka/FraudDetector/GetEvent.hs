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
-- Module      : Amazonka.FraudDetector.GetEvent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details of events stored with Amazon Fraud Detector. This
-- action does not retrieve prediction results.
module Amazonka.FraudDetector.GetEvent
  ( -- * Creating a Request
    GetEvent (..),
    newGetEvent,

    -- * Request Lenses
    getEvent_eventId,
    getEvent_eventTypeName,

    -- * Destructuring the Response
    GetEventResponse (..),
    newGetEventResponse,

    -- * Response Lenses
    getEventResponse_event,
    getEventResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEvent' smart constructor.
data GetEvent = GetEvent'
  { -- | The ID of the event to retrieve.
    eventId :: Prelude.Text,
    -- | The event type of the event to retrieve.
    eventTypeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventId', 'getEvent_eventId' - The ID of the event to retrieve.
--
-- 'eventTypeName', 'getEvent_eventTypeName' - The event type of the event to retrieve.
newGetEvent ::
  -- | 'eventId'
  Prelude.Text ->
  -- | 'eventTypeName'
  Prelude.Text ->
  GetEvent
newGetEvent pEventId_ pEventTypeName_ =
  GetEvent'
    { eventId = pEventId_,
      eventTypeName = pEventTypeName_
    }

-- | The ID of the event to retrieve.
getEvent_eventId :: Lens.Lens' GetEvent Prelude.Text
getEvent_eventId = Lens.lens (\GetEvent' {eventId} -> eventId) (\s@GetEvent' {} a -> s {eventId = a} :: GetEvent)

-- | The event type of the event to retrieve.
getEvent_eventTypeName :: Lens.Lens' GetEvent Prelude.Text
getEvent_eventTypeName = Lens.lens (\GetEvent' {eventTypeName} -> eventTypeName) (\s@GetEvent' {} a -> s {eventTypeName = a} :: GetEvent)

instance Core.AWSRequest GetEvent where
  type AWSResponse GetEvent = GetEventResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEventResponse'
            Prelude.<$> (x Core..?> "event")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEvent where
  hashWithSalt _salt GetEvent' {..} =
    _salt `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` eventTypeName

instance Prelude.NFData GetEvent where
  rnf GetEvent' {..} =
    Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf eventTypeName

instance Core.ToHeaders GetEvent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHawksNestServiceFacade.GetEvent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetEvent where
  toJSON GetEvent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("eventId" Core..= eventId),
            Prelude.Just
              ("eventTypeName" Core..= eventTypeName)
          ]
      )

instance Core.ToPath GetEvent where
  toPath = Prelude.const "/"

instance Core.ToQuery GetEvent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEventResponse' smart constructor.
data GetEventResponse = GetEventResponse'
  { -- | The details of the event.
    event :: Prelude.Maybe Event,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'event', 'getEventResponse_event' - The details of the event.
--
-- 'httpStatus', 'getEventResponse_httpStatus' - The response's http status code.
newGetEventResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEventResponse
newGetEventResponse pHttpStatus_ =
  GetEventResponse'
    { event = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the event.
getEventResponse_event :: Lens.Lens' GetEventResponse (Prelude.Maybe Event)
getEventResponse_event = Lens.lens (\GetEventResponse' {event} -> event) (\s@GetEventResponse' {} a -> s {event = a} :: GetEventResponse)

-- | The response's http status code.
getEventResponse_httpStatus :: Lens.Lens' GetEventResponse Prelude.Int
getEventResponse_httpStatus = Lens.lens (\GetEventResponse' {httpStatus} -> httpStatus) (\s@GetEventResponse' {} a -> s {httpStatus = a} :: GetEventResponse)

instance Prelude.NFData GetEventResponse where
  rnf GetEventResponse' {..} =
    Prelude.rnf event
      `Prelude.seq` Prelude.rnf httpStatus
