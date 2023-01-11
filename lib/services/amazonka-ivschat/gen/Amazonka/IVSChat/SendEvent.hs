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
-- Module      : Amazonka.IVSChat.SendEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends an event to a room. Use this within your application’s business
-- logic to send events to clients of a room; e.g., to notify clients to
-- change the way the chat UI is rendered.
module Amazonka.IVSChat.SendEvent
  ( -- * Creating a Request
    SendEvent (..),
    newSendEvent,

    -- * Request Lenses
    sendEvent_attributes,
    sendEvent_eventName,
    sendEvent_roomIdentifier,

    -- * Destructuring the Response
    SendEventResponse (..),
    newSendEventResponse,

    -- * Response Lenses
    sendEventResponse_id,
    sendEventResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSChat.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendEvent' smart constructor.
data SendEvent = SendEvent'
  { -- | Application-defined metadata to attach to the event sent to clients. The
    -- maximum length of the metadata is 1 KB total.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Application-defined name of the event to send to clients.
    eventName :: Prelude.Text,
    -- | Identifier of the room to which the event will be sent. Currently this
    -- must be an ARN.
    roomIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'sendEvent_attributes' - Application-defined metadata to attach to the event sent to clients. The
-- maximum length of the metadata is 1 KB total.
--
-- 'eventName', 'sendEvent_eventName' - Application-defined name of the event to send to clients.
--
-- 'roomIdentifier', 'sendEvent_roomIdentifier' - Identifier of the room to which the event will be sent. Currently this
-- must be an ARN.
newSendEvent ::
  -- | 'eventName'
  Prelude.Text ->
  -- | 'roomIdentifier'
  Prelude.Text ->
  SendEvent
newSendEvent pEventName_ pRoomIdentifier_ =
  SendEvent'
    { attributes = Prelude.Nothing,
      eventName = pEventName_,
      roomIdentifier = pRoomIdentifier_
    }

-- | Application-defined metadata to attach to the event sent to clients. The
-- maximum length of the metadata is 1 KB total.
sendEvent_attributes :: Lens.Lens' SendEvent (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
sendEvent_attributes = Lens.lens (\SendEvent' {attributes} -> attributes) (\s@SendEvent' {} a -> s {attributes = a} :: SendEvent) Prelude.. Lens.mapping Lens.coerced

-- | Application-defined name of the event to send to clients.
sendEvent_eventName :: Lens.Lens' SendEvent Prelude.Text
sendEvent_eventName = Lens.lens (\SendEvent' {eventName} -> eventName) (\s@SendEvent' {} a -> s {eventName = a} :: SendEvent)

-- | Identifier of the room to which the event will be sent. Currently this
-- must be an ARN.
sendEvent_roomIdentifier :: Lens.Lens' SendEvent Prelude.Text
sendEvent_roomIdentifier = Lens.lens (\SendEvent' {roomIdentifier} -> roomIdentifier) (\s@SendEvent' {} a -> s {roomIdentifier = a} :: SendEvent)

instance Core.AWSRequest SendEvent where
  type AWSResponse SendEvent = SendEventResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SendEventResponse'
            Prelude.<$> (x Data..?> "id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendEvent where
  hashWithSalt _salt SendEvent' {..} =
    _salt `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` eventName
      `Prelude.hashWithSalt` roomIdentifier

instance Prelude.NFData SendEvent where
  rnf SendEvent' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf eventName
      `Prelude.seq` Prelude.rnf roomIdentifier

instance Data.ToHeaders SendEvent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SendEvent where
  toJSON SendEvent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributes" Data..=) Prelude.<$> attributes,
            Prelude.Just ("eventName" Data..= eventName),
            Prelude.Just
              ("roomIdentifier" Data..= roomIdentifier)
          ]
      )

instance Data.ToPath SendEvent where
  toPath = Prelude.const "/SendEvent"

instance Data.ToQuery SendEvent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendEventResponse' smart constructor.
data SendEventResponse = SendEventResponse'
  { -- | An identifier generated by Amazon IVS Chat. This identifier must be used
    -- in subsequent operations for this message, such as DeleteMessage.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
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
-- 'id', 'sendEventResponse_id' - An identifier generated by Amazon IVS Chat. This identifier must be used
-- in subsequent operations for this message, such as DeleteMessage.
--
-- 'httpStatus', 'sendEventResponse_httpStatus' - The response's http status code.
newSendEventResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendEventResponse
newSendEventResponse pHttpStatus_ =
  SendEventResponse'
    { id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier generated by Amazon IVS Chat. This identifier must be used
-- in subsequent operations for this message, such as DeleteMessage.
sendEventResponse_id :: Lens.Lens' SendEventResponse (Prelude.Maybe Prelude.Text)
sendEventResponse_id = Lens.lens (\SendEventResponse' {id} -> id) (\s@SendEventResponse' {} a -> s {id = a} :: SendEventResponse)

-- | The response's http status code.
sendEventResponse_httpStatus :: Lens.Lens' SendEventResponse Prelude.Int
sendEventResponse_httpStatus = Lens.lens (\SendEventResponse' {httpStatus} -> httpStatus) (\s@SendEventResponse' {} a -> s {httpStatus = a} :: SendEventResponse)

instance Prelude.NFData SendEventResponse where
  rnf SendEventResponse' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf httpStatus
