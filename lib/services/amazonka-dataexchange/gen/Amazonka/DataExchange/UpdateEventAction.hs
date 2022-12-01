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
-- Module      : Amazonka.DataExchange.UpdateEventAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation updates the event action.
module Amazonka.DataExchange.UpdateEventAction
  ( -- * Creating a Request
    UpdateEventAction (..),
    newUpdateEventAction,

    -- * Request Lenses
    updateEventAction_action,
    updateEventAction_eventActionId,

    -- * Destructuring the Response
    UpdateEventActionResponse (..),
    newUpdateEventActionResponse,

    -- * Response Lenses
    updateEventActionResponse_arn,
    updateEventActionResponse_id,
    updateEventActionResponse_event,
    updateEventActionResponse_action,
    updateEventActionResponse_createdAt,
    updateEventActionResponse_updatedAt,
    updateEventActionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEventAction' smart constructor.
data UpdateEventAction = UpdateEventAction'
  { -- | What occurs after a certain event.
    action :: Prelude.Maybe Action,
    -- | The unique identifier for the event action.
    eventActionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEventAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'updateEventAction_action' - What occurs after a certain event.
--
-- 'eventActionId', 'updateEventAction_eventActionId' - The unique identifier for the event action.
newUpdateEventAction ::
  -- | 'eventActionId'
  Prelude.Text ->
  UpdateEventAction
newUpdateEventAction pEventActionId_ =
  UpdateEventAction'
    { action = Prelude.Nothing,
      eventActionId = pEventActionId_
    }

-- | What occurs after a certain event.
updateEventAction_action :: Lens.Lens' UpdateEventAction (Prelude.Maybe Action)
updateEventAction_action = Lens.lens (\UpdateEventAction' {action} -> action) (\s@UpdateEventAction' {} a -> s {action = a} :: UpdateEventAction)

-- | The unique identifier for the event action.
updateEventAction_eventActionId :: Lens.Lens' UpdateEventAction Prelude.Text
updateEventAction_eventActionId = Lens.lens (\UpdateEventAction' {eventActionId} -> eventActionId) (\s@UpdateEventAction' {} a -> s {eventActionId = a} :: UpdateEventAction)

instance Core.AWSRequest UpdateEventAction where
  type
    AWSResponse UpdateEventAction =
      UpdateEventActionResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEventActionResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Event")
            Prelude.<*> (x Core..?> "Action")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "UpdatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEventAction where
  hashWithSalt _salt UpdateEventAction' {..} =
    _salt `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` eventActionId

instance Prelude.NFData UpdateEventAction where
  rnf UpdateEventAction' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf eventActionId

instance Core.ToHeaders UpdateEventAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateEventAction where
  toJSON UpdateEventAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Action" Core..=) Prelude.<$> action]
      )

instance Core.ToPath UpdateEventAction where
  toPath UpdateEventAction' {..} =
    Prelude.mconcat
      ["/v1/event-actions/", Core.toBS eventActionId]

instance Core.ToQuery UpdateEventAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEventActionResponse' smart constructor.
data UpdateEventActionResponse = UpdateEventActionResponse'
  { -- | The ARN for the event action.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the event action.
    id :: Prelude.Maybe Prelude.Text,
    -- | What occurs to start an action.
    event :: Prelude.Maybe Event,
    -- | What occurs after a certain event.
    action :: Prelude.Maybe Action,
    -- | The date and time that the event action was created, in ISO 8601 format.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The date and time that the event action was last updated, in ISO 8601
    -- format.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEventActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateEventActionResponse_arn' - The ARN for the event action.
--
-- 'id', 'updateEventActionResponse_id' - The unique identifier for the event action.
--
-- 'event', 'updateEventActionResponse_event' - What occurs to start an action.
--
-- 'action', 'updateEventActionResponse_action' - What occurs after a certain event.
--
-- 'createdAt', 'updateEventActionResponse_createdAt' - The date and time that the event action was created, in ISO 8601 format.
--
-- 'updatedAt', 'updateEventActionResponse_updatedAt' - The date and time that the event action was last updated, in ISO 8601
-- format.
--
-- 'httpStatus', 'updateEventActionResponse_httpStatus' - The response's http status code.
newUpdateEventActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEventActionResponse
newUpdateEventActionResponse pHttpStatus_ =
  UpdateEventActionResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      event = Prelude.Nothing,
      action = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN for the event action.
updateEventActionResponse_arn :: Lens.Lens' UpdateEventActionResponse (Prelude.Maybe Prelude.Text)
updateEventActionResponse_arn = Lens.lens (\UpdateEventActionResponse' {arn} -> arn) (\s@UpdateEventActionResponse' {} a -> s {arn = a} :: UpdateEventActionResponse)

-- | The unique identifier for the event action.
updateEventActionResponse_id :: Lens.Lens' UpdateEventActionResponse (Prelude.Maybe Prelude.Text)
updateEventActionResponse_id = Lens.lens (\UpdateEventActionResponse' {id} -> id) (\s@UpdateEventActionResponse' {} a -> s {id = a} :: UpdateEventActionResponse)

-- | What occurs to start an action.
updateEventActionResponse_event :: Lens.Lens' UpdateEventActionResponse (Prelude.Maybe Event)
updateEventActionResponse_event = Lens.lens (\UpdateEventActionResponse' {event} -> event) (\s@UpdateEventActionResponse' {} a -> s {event = a} :: UpdateEventActionResponse)

-- | What occurs after a certain event.
updateEventActionResponse_action :: Lens.Lens' UpdateEventActionResponse (Prelude.Maybe Action)
updateEventActionResponse_action = Lens.lens (\UpdateEventActionResponse' {action} -> action) (\s@UpdateEventActionResponse' {} a -> s {action = a} :: UpdateEventActionResponse)

-- | The date and time that the event action was created, in ISO 8601 format.
updateEventActionResponse_createdAt :: Lens.Lens' UpdateEventActionResponse (Prelude.Maybe Prelude.UTCTime)
updateEventActionResponse_createdAt = Lens.lens (\UpdateEventActionResponse' {createdAt} -> createdAt) (\s@UpdateEventActionResponse' {} a -> s {createdAt = a} :: UpdateEventActionResponse) Prelude.. Lens.mapping Core._Time

-- | The date and time that the event action was last updated, in ISO 8601
-- format.
updateEventActionResponse_updatedAt :: Lens.Lens' UpdateEventActionResponse (Prelude.Maybe Prelude.UTCTime)
updateEventActionResponse_updatedAt = Lens.lens (\UpdateEventActionResponse' {updatedAt} -> updatedAt) (\s@UpdateEventActionResponse' {} a -> s {updatedAt = a} :: UpdateEventActionResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
updateEventActionResponse_httpStatus :: Lens.Lens' UpdateEventActionResponse Prelude.Int
updateEventActionResponse_httpStatus = Lens.lens (\UpdateEventActionResponse' {httpStatus} -> httpStatus) (\s@UpdateEventActionResponse' {} a -> s {httpStatus = a} :: UpdateEventActionResponse)

instance Prelude.NFData UpdateEventActionResponse where
  rnf UpdateEventActionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf event
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf httpStatus
