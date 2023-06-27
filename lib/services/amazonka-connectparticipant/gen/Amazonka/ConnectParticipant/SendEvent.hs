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
-- Module      : Amazonka.ConnectParticipant.SendEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends an event.
--
-- @ConnectionToken@ is used for invoking this API instead of
-- @ParticipantToken@.
--
-- The Amazon Connect Participant Service APIs do not use
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 authentication>.
module Amazonka.ConnectParticipant.SendEvent
  ( -- * Creating a Request
    SendEvent (..),
    newSendEvent,

    -- * Request Lenses
    sendEvent_clientToken,
    sendEvent_content,
    sendEvent_contentType,
    sendEvent_connectionToken,

    -- * Destructuring the Response
    SendEventResponse (..),
    newSendEventResponse,

    -- * Response Lenses
    sendEventResponse_absoluteTime,
    sendEventResponse_id,
    sendEventResponse_httpStatus,
  )
where

import Amazonka.ConnectParticipant.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendEvent' smart constructor.
data SendEvent = SendEvent'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The content of the event to be sent (for example, message text). For
    -- content related to message receipts, this is supported in the form of a
    -- JSON string.
    --
    -- Sample Content:
    -- \"{\\\"messageId\\\":\\\"11111111-aaaa-bbbb-cccc-EXAMPLE01234\\\"}\"
    content :: Prelude.Maybe Prelude.Text,
    -- | The content type of the request. Supported types are:
    --
    -- -   application\/vnd.amazonaws.connect.event.typing
    --
    -- -   application\/vnd.amazonaws.connect.event.connection.acknowledged
    --
    -- -   application\/vnd.amazonaws.connect.event.message.delivered
    --
    -- -   application\/vnd.amazonaws.connect.event.message.read
    contentType :: Prelude.Text,
    -- | The authentication token associated with the participant\'s connection.
    connectionToken :: Prelude.Text
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
-- 'clientToken', 'sendEvent_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'content', 'sendEvent_content' - The content of the event to be sent (for example, message text). For
-- content related to message receipts, this is supported in the form of a
-- JSON string.
--
-- Sample Content:
-- \"{\\\"messageId\\\":\\\"11111111-aaaa-bbbb-cccc-EXAMPLE01234\\\"}\"
--
-- 'contentType', 'sendEvent_contentType' - The content type of the request. Supported types are:
--
-- -   application\/vnd.amazonaws.connect.event.typing
--
-- -   application\/vnd.amazonaws.connect.event.connection.acknowledged
--
-- -   application\/vnd.amazonaws.connect.event.message.delivered
--
-- -   application\/vnd.amazonaws.connect.event.message.read
--
-- 'connectionToken', 'sendEvent_connectionToken' - The authentication token associated with the participant\'s connection.
newSendEvent ::
  -- | 'contentType'
  Prelude.Text ->
  -- | 'connectionToken'
  Prelude.Text ->
  SendEvent
newSendEvent pContentType_ pConnectionToken_ =
  SendEvent'
    { clientToken = Prelude.Nothing,
      content = Prelude.Nothing,
      contentType = pContentType_,
      connectionToken = pConnectionToken_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
sendEvent_clientToken :: Lens.Lens' SendEvent (Prelude.Maybe Prelude.Text)
sendEvent_clientToken = Lens.lens (\SendEvent' {clientToken} -> clientToken) (\s@SendEvent' {} a -> s {clientToken = a} :: SendEvent)

-- | The content of the event to be sent (for example, message text). For
-- content related to message receipts, this is supported in the form of a
-- JSON string.
--
-- Sample Content:
-- \"{\\\"messageId\\\":\\\"11111111-aaaa-bbbb-cccc-EXAMPLE01234\\\"}\"
sendEvent_content :: Lens.Lens' SendEvent (Prelude.Maybe Prelude.Text)
sendEvent_content = Lens.lens (\SendEvent' {content} -> content) (\s@SendEvent' {} a -> s {content = a} :: SendEvent)

-- | The content type of the request. Supported types are:
--
-- -   application\/vnd.amazonaws.connect.event.typing
--
-- -   application\/vnd.amazonaws.connect.event.connection.acknowledged
--
-- -   application\/vnd.amazonaws.connect.event.message.delivered
--
-- -   application\/vnd.amazonaws.connect.event.message.read
sendEvent_contentType :: Lens.Lens' SendEvent Prelude.Text
sendEvent_contentType = Lens.lens (\SendEvent' {contentType} -> contentType) (\s@SendEvent' {} a -> s {contentType = a} :: SendEvent)

-- | The authentication token associated with the participant\'s connection.
sendEvent_connectionToken :: Lens.Lens' SendEvent Prelude.Text
sendEvent_connectionToken = Lens.lens (\SendEvent' {connectionToken} -> connectionToken) (\s@SendEvent' {} a -> s {connectionToken = a} :: SendEvent)

instance Core.AWSRequest SendEvent where
  type AWSResponse SendEvent = SendEventResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SendEventResponse'
            Prelude.<$> (x Data..?> "AbsoluteTime")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendEvent where
  hashWithSalt _salt SendEvent' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` connectionToken

instance Prelude.NFData SendEvent where
  rnf SendEvent' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf connectionToken

instance Data.ToHeaders SendEvent where
  toHeaders SendEvent' {..} =
    Prelude.mconcat
      [ "X-Amz-Bearer" Data.=# connectionToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON SendEvent where
  toJSON SendEvent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Content" Data..=) Prelude.<$> content,
            Prelude.Just ("ContentType" Data..= contentType)
          ]
      )

instance Data.ToPath SendEvent where
  toPath = Prelude.const "/participant/event"

instance Data.ToQuery SendEvent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendEventResponse' smart constructor.
data SendEventResponse = SendEventResponse'
  { -- | The time when the event was sent.
    --
    -- It\'s specified in ISO 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For
    -- example, 2019-11-08T02:41:28.172Z.
    absoluteTime :: Prelude.Maybe Prelude.Text,
    -- | The ID of the response.
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
-- 'absoluteTime', 'sendEventResponse_absoluteTime' - The time when the event was sent.
--
-- It\'s specified in ISO 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For
-- example, 2019-11-08T02:41:28.172Z.
--
-- 'id', 'sendEventResponse_id' - The ID of the response.
--
-- 'httpStatus', 'sendEventResponse_httpStatus' - The response's http status code.
newSendEventResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendEventResponse
newSendEventResponse pHttpStatus_ =
  SendEventResponse'
    { absoluteTime = Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time when the event was sent.
--
-- It\'s specified in ISO 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For
-- example, 2019-11-08T02:41:28.172Z.
sendEventResponse_absoluteTime :: Lens.Lens' SendEventResponse (Prelude.Maybe Prelude.Text)
sendEventResponse_absoluteTime = Lens.lens (\SendEventResponse' {absoluteTime} -> absoluteTime) (\s@SendEventResponse' {} a -> s {absoluteTime = a} :: SendEventResponse)

-- | The ID of the response.
sendEventResponse_id :: Lens.Lens' SendEventResponse (Prelude.Maybe Prelude.Text)
sendEventResponse_id = Lens.lens (\SendEventResponse' {id} -> id) (\s@SendEventResponse' {} a -> s {id = a} :: SendEventResponse)

-- | The response's http status code.
sendEventResponse_httpStatus :: Lens.Lens' SendEventResponse Prelude.Int
sendEventResponse_httpStatus = Lens.lens (\SendEventResponse' {httpStatus} -> httpStatus) (\s@SendEventResponse' {} a -> s {httpStatus = a} :: SendEventResponse)

instance Prelude.NFData SendEventResponse where
  rnf SendEventResponse' {..} =
    Prelude.rnf absoluteTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus
