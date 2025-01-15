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
-- Module      : Amazonka.Chime.RedactRoomMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Redacts the specified message from the specified Amazon Chime channel.
module Amazonka.Chime.RedactRoomMessage
  ( -- * Creating a Request
    RedactRoomMessage (..),
    newRedactRoomMessage,

    -- * Request Lenses
    redactRoomMessage_accountId,
    redactRoomMessage_roomId,
    redactRoomMessage_messageId,

    -- * Destructuring the Response
    RedactRoomMessageResponse (..),
    newRedactRoomMessageResponse,

    -- * Response Lenses
    redactRoomMessageResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRedactRoomMessage' smart constructor.
data RedactRoomMessage = RedactRoomMessage'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The room ID.
    roomId :: Prelude.Text,
    -- | The message ID.
    messageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedactRoomMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'redactRoomMessage_accountId' - The Amazon Chime account ID.
--
-- 'roomId', 'redactRoomMessage_roomId' - The room ID.
--
-- 'messageId', 'redactRoomMessage_messageId' - The message ID.
newRedactRoomMessage ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'roomId'
  Prelude.Text ->
  -- | 'messageId'
  Prelude.Text ->
  RedactRoomMessage
newRedactRoomMessage pAccountId_ pRoomId_ pMessageId_ =
  RedactRoomMessage'
    { accountId = pAccountId_,
      roomId = pRoomId_,
      messageId = pMessageId_
    }

-- | The Amazon Chime account ID.
redactRoomMessage_accountId :: Lens.Lens' RedactRoomMessage Prelude.Text
redactRoomMessage_accountId = Lens.lens (\RedactRoomMessage' {accountId} -> accountId) (\s@RedactRoomMessage' {} a -> s {accountId = a} :: RedactRoomMessage)

-- | The room ID.
redactRoomMessage_roomId :: Lens.Lens' RedactRoomMessage Prelude.Text
redactRoomMessage_roomId = Lens.lens (\RedactRoomMessage' {roomId} -> roomId) (\s@RedactRoomMessage' {} a -> s {roomId = a} :: RedactRoomMessage)

-- | The message ID.
redactRoomMessage_messageId :: Lens.Lens' RedactRoomMessage Prelude.Text
redactRoomMessage_messageId = Lens.lens (\RedactRoomMessage' {messageId} -> messageId) (\s@RedactRoomMessage' {} a -> s {messageId = a} :: RedactRoomMessage)

instance Core.AWSRequest RedactRoomMessage where
  type
    AWSResponse RedactRoomMessage =
      RedactRoomMessageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RedactRoomMessageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RedactRoomMessage where
  hashWithSalt _salt RedactRoomMessage' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` roomId
      `Prelude.hashWithSalt` messageId

instance Prelude.NFData RedactRoomMessage where
  rnf RedactRoomMessage' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf roomId `Prelude.seq`
        Prelude.rnf messageId

instance Data.ToHeaders RedactRoomMessage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON RedactRoomMessage where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath RedactRoomMessage where
  toPath RedactRoomMessage' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS accountId,
        "/rooms/",
        Data.toBS roomId,
        "/messages/",
        Data.toBS messageId
      ]

instance Data.ToQuery RedactRoomMessage where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["operation=redact"])

-- | /See:/ 'newRedactRoomMessageResponse' smart constructor.
data RedactRoomMessageResponse = RedactRoomMessageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedactRoomMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'redactRoomMessageResponse_httpStatus' - The response's http status code.
newRedactRoomMessageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RedactRoomMessageResponse
newRedactRoomMessageResponse pHttpStatus_ =
  RedactRoomMessageResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
redactRoomMessageResponse_httpStatus :: Lens.Lens' RedactRoomMessageResponse Prelude.Int
redactRoomMessageResponse_httpStatus = Lens.lens (\RedactRoomMessageResponse' {httpStatus} -> httpStatus) (\s@RedactRoomMessageResponse' {} a -> s {httpStatus = a} :: RedactRoomMessageResponse)

instance Prelude.NFData RedactRoomMessageResponse where
  rnf RedactRoomMessageResponse' {..} =
    Prelude.rnf httpStatus
