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
-- Module      : Amazonka.DataExchange.GetEventAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation retrieves information about an event action.
module Amazonka.DataExchange.GetEventAction
  ( -- * Creating a Request
    GetEventAction (..),
    newGetEventAction,

    -- * Request Lenses
    getEventAction_eventActionId,

    -- * Destructuring the Response
    GetEventActionResponse (..),
    newGetEventActionResponse,

    -- * Response Lenses
    getEventActionResponse_action,
    getEventActionResponse_arn,
    getEventActionResponse_createdAt,
    getEventActionResponse_event,
    getEventActionResponse_id,
    getEventActionResponse_updatedAt,
    getEventActionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEventAction' smart constructor.
data GetEventAction = GetEventAction'
  { -- | The unique identifier for the event action.
    eventActionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventActionId', 'getEventAction_eventActionId' - The unique identifier for the event action.
newGetEventAction ::
  -- | 'eventActionId'
  Prelude.Text ->
  GetEventAction
newGetEventAction pEventActionId_ =
  GetEventAction' {eventActionId = pEventActionId_}

-- | The unique identifier for the event action.
getEventAction_eventActionId :: Lens.Lens' GetEventAction Prelude.Text
getEventAction_eventActionId = Lens.lens (\GetEventAction' {eventActionId} -> eventActionId) (\s@GetEventAction' {} a -> s {eventActionId = a} :: GetEventAction)

instance Core.AWSRequest GetEventAction where
  type
    AWSResponse GetEventAction =
      GetEventActionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEventActionResponse'
            Prelude.<$> (x Data..?> "Action")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "Event")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "UpdatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEventAction where
  hashWithSalt _salt GetEventAction' {..} =
    _salt `Prelude.hashWithSalt` eventActionId

instance Prelude.NFData GetEventAction where
  rnf GetEventAction' {..} = Prelude.rnf eventActionId

instance Data.ToHeaders GetEventAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetEventAction where
  toPath GetEventAction' {..} =
    Prelude.mconcat
      ["/v1/event-actions/", Data.toBS eventActionId]

instance Data.ToQuery GetEventAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEventActionResponse' smart constructor.
data GetEventActionResponse = GetEventActionResponse'
  { -- | What occurs after a certain event.
    action :: Prelude.Maybe Action,
    -- | The ARN for the event action.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the event action was created, in ISO 8601 format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | What occurs to start an action.
    event :: Prelude.Maybe Event,
    -- | The unique identifier for the event action.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the event action was last updated, in ISO 8601
    -- format.
    updatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'getEventActionResponse_action' - What occurs after a certain event.
--
-- 'arn', 'getEventActionResponse_arn' - The ARN for the event action.
--
-- 'createdAt', 'getEventActionResponse_createdAt' - The date and time that the event action was created, in ISO 8601 format.
--
-- 'event', 'getEventActionResponse_event' - What occurs to start an action.
--
-- 'id', 'getEventActionResponse_id' - The unique identifier for the event action.
--
-- 'updatedAt', 'getEventActionResponse_updatedAt' - The date and time that the event action was last updated, in ISO 8601
-- format.
--
-- 'httpStatus', 'getEventActionResponse_httpStatus' - The response's http status code.
newGetEventActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEventActionResponse
newGetEventActionResponse pHttpStatus_ =
  GetEventActionResponse'
    { action = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      event = Prelude.Nothing,
      id = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | What occurs after a certain event.
getEventActionResponse_action :: Lens.Lens' GetEventActionResponse (Prelude.Maybe Action)
getEventActionResponse_action = Lens.lens (\GetEventActionResponse' {action} -> action) (\s@GetEventActionResponse' {} a -> s {action = a} :: GetEventActionResponse)

-- | The ARN for the event action.
getEventActionResponse_arn :: Lens.Lens' GetEventActionResponse (Prelude.Maybe Prelude.Text)
getEventActionResponse_arn = Lens.lens (\GetEventActionResponse' {arn} -> arn) (\s@GetEventActionResponse' {} a -> s {arn = a} :: GetEventActionResponse)

-- | The date and time that the event action was created, in ISO 8601 format.
getEventActionResponse_createdAt :: Lens.Lens' GetEventActionResponse (Prelude.Maybe Prelude.UTCTime)
getEventActionResponse_createdAt = Lens.lens (\GetEventActionResponse' {createdAt} -> createdAt) (\s@GetEventActionResponse' {} a -> s {createdAt = a} :: GetEventActionResponse) Prelude.. Lens.mapping Data._Time

-- | What occurs to start an action.
getEventActionResponse_event :: Lens.Lens' GetEventActionResponse (Prelude.Maybe Event)
getEventActionResponse_event = Lens.lens (\GetEventActionResponse' {event} -> event) (\s@GetEventActionResponse' {} a -> s {event = a} :: GetEventActionResponse)

-- | The unique identifier for the event action.
getEventActionResponse_id :: Lens.Lens' GetEventActionResponse (Prelude.Maybe Prelude.Text)
getEventActionResponse_id = Lens.lens (\GetEventActionResponse' {id} -> id) (\s@GetEventActionResponse' {} a -> s {id = a} :: GetEventActionResponse)

-- | The date and time that the event action was last updated, in ISO 8601
-- format.
getEventActionResponse_updatedAt :: Lens.Lens' GetEventActionResponse (Prelude.Maybe Prelude.UTCTime)
getEventActionResponse_updatedAt = Lens.lens (\GetEventActionResponse' {updatedAt} -> updatedAt) (\s@GetEventActionResponse' {} a -> s {updatedAt = a} :: GetEventActionResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getEventActionResponse_httpStatus :: Lens.Lens' GetEventActionResponse Prelude.Int
getEventActionResponse_httpStatus = Lens.lens (\GetEventActionResponse' {httpStatus} -> httpStatus) (\s@GetEventActionResponse' {} a -> s {httpStatus = a} :: GetEventActionResponse)

instance Prelude.NFData GetEventActionResponse where
  rnf GetEventActionResponse' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf event
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf httpStatus
