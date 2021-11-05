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
-- Module      : Amazonka.DataExchange.CreateEventAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation creates an event action.
module Amazonka.DataExchange.CreateEventAction
  ( -- * Creating a Request
    CreateEventAction (..),
    newCreateEventAction,

    -- * Request Lenses
    createEventAction_action,
    createEventAction_event,

    -- * Destructuring the Response
    CreateEventActionResponse (..),
    newCreateEventActionResponse,

    -- * Response Lenses
    createEventActionResponse_event,
    createEventActionResponse_arn,
    createEventActionResponse_createdAt,
    createEventActionResponse_action,
    createEventActionResponse_id,
    createEventActionResponse_updatedAt,
    createEventActionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DataExchange.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for CreateEventAction.
--
-- /See:/ 'newCreateEventAction' smart constructor.
data CreateEventAction = CreateEventAction'
  { -- | What occurs after a certain event.
    action :: Action,
    -- | What occurs to start an action.
    event :: Event
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEventAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'createEventAction_action' - What occurs after a certain event.
--
-- 'event', 'createEventAction_event' - What occurs to start an action.
newCreateEventAction ::
  -- | 'action'
  Action ->
  -- | 'event'
  Event ->
  CreateEventAction
newCreateEventAction pAction_ pEvent_ =
  CreateEventAction'
    { action = pAction_,
      event = pEvent_
    }

-- | What occurs after a certain event.
createEventAction_action :: Lens.Lens' CreateEventAction Action
createEventAction_action = Lens.lens (\CreateEventAction' {action} -> action) (\s@CreateEventAction' {} a -> s {action = a} :: CreateEventAction)

-- | What occurs to start an action.
createEventAction_event :: Lens.Lens' CreateEventAction Event
createEventAction_event = Lens.lens (\CreateEventAction' {event} -> event) (\s@CreateEventAction' {} a -> s {event = a} :: CreateEventAction)

instance Core.AWSRequest CreateEventAction where
  type
    AWSResponse CreateEventAction =
      CreateEventActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEventActionResponse'
            Prelude.<$> (x Core..?> "Event")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "Action")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "UpdatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEventAction

instance Prelude.NFData CreateEventAction

instance Core.ToHeaders CreateEventAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateEventAction where
  toJSON CreateEventAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Action" Core..= action),
            Prelude.Just ("Event" Core..= event)
          ]
      )

instance Core.ToPath CreateEventAction where
  toPath = Prelude.const "/v1/event-actions"

instance Core.ToQuery CreateEventAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEventActionResponse' smart constructor.
data CreateEventActionResponse = CreateEventActionResponse'
  { -- | What occurs to start an action.
    event :: Prelude.Maybe Event,
    -- | The ARN for the event action.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the event action was created, in ISO 8601 format.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | What occurs after a certain event.
    action :: Prelude.Maybe Action,
    -- | The unique identifier for the event action.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the event action was last updated, in ISO 8601
    -- format.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEventActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'event', 'createEventActionResponse_event' - What occurs to start an action.
--
-- 'arn', 'createEventActionResponse_arn' - The ARN for the event action.
--
-- 'createdAt', 'createEventActionResponse_createdAt' - The date and time that the event action was created, in ISO 8601 format.
--
-- 'action', 'createEventActionResponse_action' - What occurs after a certain event.
--
-- 'id', 'createEventActionResponse_id' - The unique identifier for the event action.
--
-- 'updatedAt', 'createEventActionResponse_updatedAt' - The date and time that the event action was last updated, in ISO 8601
-- format.
--
-- 'httpStatus', 'createEventActionResponse_httpStatus' - The response's http status code.
newCreateEventActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEventActionResponse
newCreateEventActionResponse pHttpStatus_ =
  CreateEventActionResponse'
    { event = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      action = Prelude.Nothing,
      id = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | What occurs to start an action.
createEventActionResponse_event :: Lens.Lens' CreateEventActionResponse (Prelude.Maybe Event)
createEventActionResponse_event = Lens.lens (\CreateEventActionResponse' {event} -> event) (\s@CreateEventActionResponse' {} a -> s {event = a} :: CreateEventActionResponse)

-- | The ARN for the event action.
createEventActionResponse_arn :: Lens.Lens' CreateEventActionResponse (Prelude.Maybe Prelude.Text)
createEventActionResponse_arn = Lens.lens (\CreateEventActionResponse' {arn} -> arn) (\s@CreateEventActionResponse' {} a -> s {arn = a} :: CreateEventActionResponse)

-- | The date and time that the event action was created, in ISO 8601 format.
createEventActionResponse_createdAt :: Lens.Lens' CreateEventActionResponse (Prelude.Maybe Prelude.UTCTime)
createEventActionResponse_createdAt = Lens.lens (\CreateEventActionResponse' {createdAt} -> createdAt) (\s@CreateEventActionResponse' {} a -> s {createdAt = a} :: CreateEventActionResponse) Prelude.. Lens.mapping Core._Time

-- | What occurs after a certain event.
createEventActionResponse_action :: Lens.Lens' CreateEventActionResponse (Prelude.Maybe Action)
createEventActionResponse_action = Lens.lens (\CreateEventActionResponse' {action} -> action) (\s@CreateEventActionResponse' {} a -> s {action = a} :: CreateEventActionResponse)

-- | The unique identifier for the event action.
createEventActionResponse_id :: Lens.Lens' CreateEventActionResponse (Prelude.Maybe Prelude.Text)
createEventActionResponse_id = Lens.lens (\CreateEventActionResponse' {id} -> id) (\s@CreateEventActionResponse' {} a -> s {id = a} :: CreateEventActionResponse)

-- | The date and time that the event action was last updated, in ISO 8601
-- format.
createEventActionResponse_updatedAt :: Lens.Lens' CreateEventActionResponse (Prelude.Maybe Prelude.UTCTime)
createEventActionResponse_updatedAt = Lens.lens (\CreateEventActionResponse' {updatedAt} -> updatedAt) (\s@CreateEventActionResponse' {} a -> s {updatedAt = a} :: CreateEventActionResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
createEventActionResponse_httpStatus :: Lens.Lens' CreateEventActionResponse Prelude.Int
createEventActionResponse_httpStatus = Lens.lens (\CreateEventActionResponse' {httpStatus} -> httpStatus) (\s@CreateEventActionResponse' {} a -> s {httpStatus = a} :: CreateEventActionResponse)

instance Prelude.NFData CreateEventActionResponse
