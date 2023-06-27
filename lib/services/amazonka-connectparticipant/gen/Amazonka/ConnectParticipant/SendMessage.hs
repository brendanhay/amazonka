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
-- Module      : Amazonka.ConnectParticipant.SendMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a message.
--
-- @ConnectionToken@ is used for invoking this API instead of
-- @ParticipantToken@.
--
-- The Amazon Connect Participant Service APIs do not use
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 authentication>.
module Amazonka.ConnectParticipant.SendMessage
  ( -- * Creating a Request
    SendMessage (..),
    newSendMessage,

    -- * Request Lenses
    sendMessage_clientToken,
    sendMessage_contentType,
    sendMessage_content,
    sendMessage_connectionToken,

    -- * Destructuring the Response
    SendMessageResponse (..),
    newSendMessageResponse,

    -- * Response Lenses
    sendMessageResponse_absoluteTime,
    sendMessageResponse_id,
    sendMessageResponse_httpStatus,
  )
where

import Amazonka.ConnectParticipant.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendMessage' smart constructor.
data SendMessage = SendMessage'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The type of the content. Supported types are @text\/plain@,
    -- @text\/markdown@, @application\/json@, and
    -- @application\/vnd.amazonaws.connect.message.interactive.response@.
    contentType :: Prelude.Text,
    -- | The content of the message.
    --
    -- -   For @text\/plain@ and @text\/markdown@, the Length Constraints are
    --     Minimum of 1, Maximum of 1024.
    --
    -- -   For @application\/json@, the Length Constraints are Minimum of 1,
    --     Maximum of 12000.
    --
    -- -   For
    --     @application\/vnd.amazonaws.connect.message.interactive.response@,
    --     the Length Constraints are Minimum of 1, Maximum of 12288.
    content :: Prelude.Text,
    -- | The authentication token associated with the connection.
    connectionToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'sendMessage_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'contentType', 'sendMessage_contentType' - The type of the content. Supported types are @text\/plain@,
-- @text\/markdown@, @application\/json@, and
-- @application\/vnd.amazonaws.connect.message.interactive.response@.
--
-- 'content', 'sendMessage_content' - The content of the message.
--
-- -   For @text\/plain@ and @text\/markdown@, the Length Constraints are
--     Minimum of 1, Maximum of 1024.
--
-- -   For @application\/json@, the Length Constraints are Minimum of 1,
--     Maximum of 12000.
--
-- -   For
--     @application\/vnd.amazonaws.connect.message.interactive.response@,
--     the Length Constraints are Minimum of 1, Maximum of 12288.
--
-- 'connectionToken', 'sendMessage_connectionToken' - The authentication token associated with the connection.
newSendMessage ::
  -- | 'contentType'
  Prelude.Text ->
  -- | 'content'
  Prelude.Text ->
  -- | 'connectionToken'
  Prelude.Text ->
  SendMessage
newSendMessage
  pContentType_
  pContent_
  pConnectionToken_ =
    SendMessage'
      { clientToken = Prelude.Nothing,
        contentType = pContentType_,
        content = pContent_,
        connectionToken = pConnectionToken_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
sendMessage_clientToken :: Lens.Lens' SendMessage (Prelude.Maybe Prelude.Text)
sendMessage_clientToken = Lens.lens (\SendMessage' {clientToken} -> clientToken) (\s@SendMessage' {} a -> s {clientToken = a} :: SendMessage)

-- | The type of the content. Supported types are @text\/plain@,
-- @text\/markdown@, @application\/json@, and
-- @application\/vnd.amazonaws.connect.message.interactive.response@.
sendMessage_contentType :: Lens.Lens' SendMessage Prelude.Text
sendMessage_contentType = Lens.lens (\SendMessage' {contentType} -> contentType) (\s@SendMessage' {} a -> s {contentType = a} :: SendMessage)

-- | The content of the message.
--
-- -   For @text\/plain@ and @text\/markdown@, the Length Constraints are
--     Minimum of 1, Maximum of 1024.
--
-- -   For @application\/json@, the Length Constraints are Minimum of 1,
--     Maximum of 12000.
--
-- -   For
--     @application\/vnd.amazonaws.connect.message.interactive.response@,
--     the Length Constraints are Minimum of 1, Maximum of 12288.
sendMessage_content :: Lens.Lens' SendMessage Prelude.Text
sendMessage_content = Lens.lens (\SendMessage' {content} -> content) (\s@SendMessage' {} a -> s {content = a} :: SendMessage)

-- | The authentication token associated with the connection.
sendMessage_connectionToken :: Lens.Lens' SendMessage Prelude.Text
sendMessage_connectionToken = Lens.lens (\SendMessage' {connectionToken} -> connectionToken) (\s@SendMessage' {} a -> s {connectionToken = a} :: SendMessage)

instance Core.AWSRequest SendMessage where
  type AWSResponse SendMessage = SendMessageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SendMessageResponse'
            Prelude.<$> (x Data..?> "AbsoluteTime")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendMessage where
  hashWithSalt _salt SendMessage' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` connectionToken

instance Prelude.NFData SendMessage where
  rnf SendMessage' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf connectionToken

instance Data.ToHeaders SendMessage where
  toHeaders SendMessage' {..} =
    Prelude.mconcat
      [ "X-Amz-Bearer" Data.=# connectionToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON SendMessage where
  toJSON SendMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("ContentType" Data..= contentType),
            Prelude.Just ("Content" Data..= content)
          ]
      )

instance Data.ToPath SendMessage where
  toPath = Prelude.const "/participant/message"

instance Data.ToQuery SendMessage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendMessageResponse' smart constructor.
data SendMessageResponse = SendMessageResponse'
  { -- | The time when the message was sent.
    --
    -- It\'s specified in ISO 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For
    -- example, 2019-11-08T02:41:28.172Z.
    absoluteTime :: Prelude.Maybe Prelude.Text,
    -- | The ID of the message.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'absoluteTime', 'sendMessageResponse_absoluteTime' - The time when the message was sent.
--
-- It\'s specified in ISO 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For
-- example, 2019-11-08T02:41:28.172Z.
--
-- 'id', 'sendMessageResponse_id' - The ID of the message.
--
-- 'httpStatus', 'sendMessageResponse_httpStatus' - The response's http status code.
newSendMessageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendMessageResponse
newSendMessageResponse pHttpStatus_ =
  SendMessageResponse'
    { absoluteTime =
        Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time when the message was sent.
--
-- It\'s specified in ISO 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For
-- example, 2019-11-08T02:41:28.172Z.
sendMessageResponse_absoluteTime :: Lens.Lens' SendMessageResponse (Prelude.Maybe Prelude.Text)
sendMessageResponse_absoluteTime = Lens.lens (\SendMessageResponse' {absoluteTime} -> absoluteTime) (\s@SendMessageResponse' {} a -> s {absoluteTime = a} :: SendMessageResponse)

-- | The ID of the message.
sendMessageResponse_id :: Lens.Lens' SendMessageResponse (Prelude.Maybe Prelude.Text)
sendMessageResponse_id = Lens.lens (\SendMessageResponse' {id} -> id) (\s@SendMessageResponse' {} a -> s {id = a} :: SendMessageResponse)

-- | The response's http status code.
sendMessageResponse_httpStatus :: Lens.Lens' SendMessageResponse Prelude.Int
sendMessageResponse_httpStatus = Lens.lens (\SendMessageResponse' {httpStatus} -> httpStatus) (\s@SendMessageResponse' {} a -> s {httpStatus = a} :: SendMessageResponse)

instance Prelude.NFData SendMessageResponse where
  rnf SendMessageResponse' {..} =
    Prelude.rnf absoluteTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus
