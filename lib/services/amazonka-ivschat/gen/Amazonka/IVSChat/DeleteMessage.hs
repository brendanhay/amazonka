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
-- Module      : Amazonka.IVSChat.DeleteMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends an event to a specific room which directs clients to delete a
-- specific message; that is, unrender it from view and delete it from the
-- client’s chat history. This event’s @EventName@ is @aws:DELETE_MESSAGE@.
-- This replicates the
-- <https://docs.aws.amazon.com/ivs/latest/chatmsgapireference/actions-deletemessage-publish.html DeleteMessage>
-- WebSocket operation in the Amazon IVS Chat Messaging API.
module Amazonka.IVSChat.DeleteMessage
  ( -- * Creating a Request
    DeleteMessage (..),
    newDeleteMessage,

    -- * Request Lenses
    deleteMessage_reason,
    deleteMessage_id,
    deleteMessage_roomIdentifier,

    -- * Destructuring the Response
    DeleteMessageResponse (..),
    newDeleteMessageResponse,

    -- * Response Lenses
    deleteMessageResponse_id,
    deleteMessageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSChat.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMessage' smart constructor.
data DeleteMessage = DeleteMessage'
  { -- | Reason for deleting the message.
    reason :: Prelude.Maybe Prelude.Text,
    -- | ID of the message to be deleted. This is the @Id@ field in the received
    -- message (see
    -- <https://docs.aws.amazon.com/ivs/latest/chatmsgapireference/actions-message-subscribe.html Message (Subscribe)>
    -- in the Chat Messaging API).
    id :: Prelude.Text,
    -- | Identifier of the room where the message should be deleted. Currently
    -- this must be an ARN.
    roomIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'deleteMessage_reason' - Reason for deleting the message.
--
-- 'id', 'deleteMessage_id' - ID of the message to be deleted. This is the @Id@ field in the received
-- message (see
-- <https://docs.aws.amazon.com/ivs/latest/chatmsgapireference/actions-message-subscribe.html Message (Subscribe)>
-- in the Chat Messaging API).
--
-- 'roomIdentifier', 'deleteMessage_roomIdentifier' - Identifier of the room where the message should be deleted. Currently
-- this must be an ARN.
newDeleteMessage ::
  -- | 'id'
  Prelude.Text ->
  -- | 'roomIdentifier'
  Prelude.Text ->
  DeleteMessage
newDeleteMessage pId_ pRoomIdentifier_ =
  DeleteMessage'
    { reason = Prelude.Nothing,
      id = pId_,
      roomIdentifier = pRoomIdentifier_
    }

-- | Reason for deleting the message.
deleteMessage_reason :: Lens.Lens' DeleteMessage (Prelude.Maybe Prelude.Text)
deleteMessage_reason = Lens.lens (\DeleteMessage' {reason} -> reason) (\s@DeleteMessage' {} a -> s {reason = a} :: DeleteMessage)

-- | ID of the message to be deleted. This is the @Id@ field in the received
-- message (see
-- <https://docs.aws.amazon.com/ivs/latest/chatmsgapireference/actions-message-subscribe.html Message (Subscribe)>
-- in the Chat Messaging API).
deleteMessage_id :: Lens.Lens' DeleteMessage Prelude.Text
deleteMessage_id = Lens.lens (\DeleteMessage' {id} -> id) (\s@DeleteMessage' {} a -> s {id = a} :: DeleteMessage)

-- | Identifier of the room where the message should be deleted. Currently
-- this must be an ARN.
deleteMessage_roomIdentifier :: Lens.Lens' DeleteMessage Prelude.Text
deleteMessage_roomIdentifier = Lens.lens (\DeleteMessage' {roomIdentifier} -> roomIdentifier) (\s@DeleteMessage' {} a -> s {roomIdentifier = a} :: DeleteMessage)

instance Core.AWSRequest DeleteMessage where
  type
    AWSResponse DeleteMessage =
      DeleteMessageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteMessageResponse'
            Prelude.<$> (x Data..?> "id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMessage where
  hashWithSalt _salt DeleteMessage' {..} =
    _salt
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` roomIdentifier

instance Prelude.NFData DeleteMessage where
  rnf DeleteMessage' {..} =
    Prelude.rnf reason
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf roomIdentifier

instance Data.ToHeaders DeleteMessage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteMessage where
  toJSON DeleteMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("reason" Data..=) Prelude.<$> reason,
            Prelude.Just ("id" Data..= id),
            Prelude.Just
              ("roomIdentifier" Data..= roomIdentifier)
          ]
      )

instance Data.ToPath DeleteMessage where
  toPath = Prelude.const "/DeleteMessage"

instance Data.ToQuery DeleteMessage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMessageResponse' smart constructor.
data DeleteMessageResponse = DeleteMessageResponse'
  { -- | Operation identifier, generated by Amazon IVS Chat.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteMessageResponse_id' - Operation identifier, generated by Amazon IVS Chat.
--
-- 'httpStatus', 'deleteMessageResponse_httpStatus' - The response's http status code.
newDeleteMessageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMessageResponse
newDeleteMessageResponse pHttpStatus_ =
  DeleteMessageResponse'
    { id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Operation identifier, generated by Amazon IVS Chat.
deleteMessageResponse_id :: Lens.Lens' DeleteMessageResponse (Prelude.Maybe Prelude.Text)
deleteMessageResponse_id = Lens.lens (\DeleteMessageResponse' {id} -> id) (\s@DeleteMessageResponse' {} a -> s {id = a} :: DeleteMessageResponse)

-- | The response's http status code.
deleteMessageResponse_httpStatus :: Lens.Lens' DeleteMessageResponse Prelude.Int
deleteMessageResponse_httpStatus = Lens.lens (\DeleteMessageResponse' {httpStatus} -> httpStatus) (\s@DeleteMessageResponse' {} a -> s {httpStatus = a} :: DeleteMessageResponse)

instance Prelude.NFData DeleteMessageResponse where
  rnf DeleteMessageResponse' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf httpStatus
