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
-- Module      : Amazonka.Chime.SendChannelMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a message to a particular channel that the member is a part of.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
--
-- Also, @STANDARD@ messages can contain 4KB of data and the 1KB of
-- metadata. @CONTROL@ messages can contain 30 bytes of data and no
-- metadata.
module Amazonka.Chime.SendChannelMessage
  ( -- * Creating a Request
    SendChannelMessage (..),
    newSendChannelMessage,

    -- * Request Lenses
    sendChannelMessage_chimeBearer,
    sendChannelMessage_metadata,
    sendChannelMessage_channelArn,
    sendChannelMessage_content,
    sendChannelMessage_type,
    sendChannelMessage_persistence,
    sendChannelMessage_clientRequestToken,

    -- * Destructuring the Response
    SendChannelMessageResponse (..),
    newSendChannelMessageResponse,

    -- * Response Lenses
    sendChannelMessageResponse_channelArn,
    sendChannelMessageResponse_messageId,
    sendChannelMessageResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendChannelMessage' smart constructor.
data SendChannelMessage = SendChannelMessage'
  { -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Maybe Prelude.Text,
    -- | The optional metadata for each message.
    metadata :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ARN of the channel.
    channelArn :: Prelude.Text,
    -- | The content of the message.
    content :: Data.Sensitive Prelude.Text,
    -- | The type of message, @STANDARD@ or @CONTROL@.
    type' :: ChannelMessageType,
    -- | Boolean that controls whether the message is persisted on the back end.
    -- Required.
    persistence :: ChannelMessagePersistenceType,
    -- | The @Idempotency@ token for each client request.
    clientRequestToken :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendChannelMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chimeBearer', 'sendChannelMessage_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
--
-- 'metadata', 'sendChannelMessage_metadata' - The optional metadata for each message.
--
-- 'channelArn', 'sendChannelMessage_channelArn' - The ARN of the channel.
--
-- 'content', 'sendChannelMessage_content' - The content of the message.
--
-- 'type'', 'sendChannelMessage_type' - The type of message, @STANDARD@ or @CONTROL@.
--
-- 'persistence', 'sendChannelMessage_persistence' - Boolean that controls whether the message is persisted on the back end.
-- Required.
--
-- 'clientRequestToken', 'sendChannelMessage_clientRequestToken' - The @Idempotency@ token for each client request.
newSendChannelMessage ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'content'
  Prelude.Text ->
  -- | 'type''
  ChannelMessageType ->
  -- | 'persistence'
  ChannelMessagePersistenceType ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  SendChannelMessage
newSendChannelMessage
  pChannelArn_
  pContent_
  pType_
  pPersistence_
  pClientRequestToken_ =
    SendChannelMessage'
      { chimeBearer = Prelude.Nothing,
        metadata = Prelude.Nothing,
        channelArn = pChannelArn_,
        content = Data._Sensitive Lens.# pContent_,
        type' = pType_,
        persistence = pPersistence_,
        clientRequestToken =
          Data._Sensitive Lens.# pClientRequestToken_
      }

-- | The @AppInstanceUserArn@ of the user that makes the API call.
sendChannelMessage_chimeBearer :: Lens.Lens' SendChannelMessage (Prelude.Maybe Prelude.Text)
sendChannelMessage_chimeBearer = Lens.lens (\SendChannelMessage' {chimeBearer} -> chimeBearer) (\s@SendChannelMessage' {} a -> s {chimeBearer = a} :: SendChannelMessage)

-- | The optional metadata for each message.
sendChannelMessage_metadata :: Lens.Lens' SendChannelMessage (Prelude.Maybe Prelude.Text)
sendChannelMessage_metadata = Lens.lens (\SendChannelMessage' {metadata} -> metadata) (\s@SendChannelMessage' {} a -> s {metadata = a} :: SendChannelMessage) Prelude.. Lens.mapping Data._Sensitive

-- | The ARN of the channel.
sendChannelMessage_channelArn :: Lens.Lens' SendChannelMessage Prelude.Text
sendChannelMessage_channelArn = Lens.lens (\SendChannelMessage' {channelArn} -> channelArn) (\s@SendChannelMessage' {} a -> s {channelArn = a} :: SendChannelMessage)

-- | The content of the message.
sendChannelMessage_content :: Lens.Lens' SendChannelMessage Prelude.Text
sendChannelMessage_content = Lens.lens (\SendChannelMessage' {content} -> content) (\s@SendChannelMessage' {} a -> s {content = a} :: SendChannelMessage) Prelude.. Data._Sensitive

-- | The type of message, @STANDARD@ or @CONTROL@.
sendChannelMessage_type :: Lens.Lens' SendChannelMessage ChannelMessageType
sendChannelMessage_type = Lens.lens (\SendChannelMessage' {type'} -> type') (\s@SendChannelMessage' {} a -> s {type' = a} :: SendChannelMessage)

-- | Boolean that controls whether the message is persisted on the back end.
-- Required.
sendChannelMessage_persistence :: Lens.Lens' SendChannelMessage ChannelMessagePersistenceType
sendChannelMessage_persistence = Lens.lens (\SendChannelMessage' {persistence} -> persistence) (\s@SendChannelMessage' {} a -> s {persistence = a} :: SendChannelMessage)

-- | The @Idempotency@ token for each client request.
sendChannelMessage_clientRequestToken :: Lens.Lens' SendChannelMessage Prelude.Text
sendChannelMessage_clientRequestToken = Lens.lens (\SendChannelMessage' {clientRequestToken} -> clientRequestToken) (\s@SendChannelMessage' {} a -> s {clientRequestToken = a} :: SendChannelMessage) Prelude.. Data._Sensitive

instance Core.AWSRequest SendChannelMessage where
  type
    AWSResponse SendChannelMessage =
      SendChannelMessageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SendChannelMessageResponse'
            Prelude.<$> (x Data..?> "ChannelArn")
            Prelude.<*> (x Data..?> "MessageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendChannelMessage where
  hashWithSalt _salt SendChannelMessage' {..} =
    _salt
      `Prelude.hashWithSalt` chimeBearer
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` persistence
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData SendChannelMessage where
  rnf SendChannelMessage' {..} =
    Prelude.rnf chimeBearer
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf persistence
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders SendChannelMessage where
  toHeaders SendChannelMessage' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToJSON SendChannelMessage where
  toJSON SendChannelMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Metadata" Data..=) Prelude.<$> metadata,
            Prelude.Just ("Content" Data..= content),
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Persistence" Data..= persistence),
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath SendChannelMessage where
  toPath SendChannelMessage' {..} =
    Prelude.mconcat
      ["/channels/", Data.toBS channelArn, "/messages"]

instance Data.ToQuery SendChannelMessage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendChannelMessageResponse' smart constructor.
data SendChannelMessageResponse = SendChannelMessageResponse'
  { -- | The ARN of the channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The ID string assigned to each message.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendChannelMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'sendChannelMessageResponse_channelArn' - The ARN of the channel.
--
-- 'messageId', 'sendChannelMessageResponse_messageId' - The ID string assigned to each message.
--
-- 'httpStatus', 'sendChannelMessageResponse_httpStatus' - The response's http status code.
newSendChannelMessageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendChannelMessageResponse
newSendChannelMessageResponse pHttpStatus_ =
  SendChannelMessageResponse'
    { channelArn =
        Prelude.Nothing,
      messageId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the channel.
sendChannelMessageResponse_channelArn :: Lens.Lens' SendChannelMessageResponse (Prelude.Maybe Prelude.Text)
sendChannelMessageResponse_channelArn = Lens.lens (\SendChannelMessageResponse' {channelArn} -> channelArn) (\s@SendChannelMessageResponse' {} a -> s {channelArn = a} :: SendChannelMessageResponse)

-- | The ID string assigned to each message.
sendChannelMessageResponse_messageId :: Lens.Lens' SendChannelMessageResponse (Prelude.Maybe Prelude.Text)
sendChannelMessageResponse_messageId = Lens.lens (\SendChannelMessageResponse' {messageId} -> messageId) (\s@SendChannelMessageResponse' {} a -> s {messageId = a} :: SendChannelMessageResponse)

-- | The response's http status code.
sendChannelMessageResponse_httpStatus :: Lens.Lens' SendChannelMessageResponse Prelude.Int
sendChannelMessageResponse_httpStatus = Lens.lens (\SendChannelMessageResponse' {httpStatus} -> httpStatus) (\s@SendChannelMessageResponse' {} a -> s {httpStatus = a} :: SendChannelMessageResponse)

instance Prelude.NFData SendChannelMessageResponse where
  rnf SendChannelMessageResponse' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf httpStatus
