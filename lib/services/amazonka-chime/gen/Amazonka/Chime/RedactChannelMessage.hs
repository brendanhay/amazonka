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
-- Module      : Amazonka.Chime.RedactChannelMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Redacts message content, but not metadata. The message exists in the
-- back end, but the action returns null content, and the state shows as
-- redacted.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.Chime.RedactChannelMessage
  ( -- * Creating a Request
    RedactChannelMessage (..),
    newRedactChannelMessage,

    -- * Request Lenses
    redactChannelMessage_chimeBearer,
    redactChannelMessage_channelArn,
    redactChannelMessage_messageId,

    -- * Destructuring the Response
    RedactChannelMessageResponse (..),
    newRedactChannelMessageResponse,

    -- * Response Lenses
    redactChannelMessageResponse_channelArn,
    redactChannelMessageResponse_messageId,
    redactChannelMessageResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRedactChannelMessage' smart constructor.
data RedactChannelMessage = RedactChannelMessage'
  { -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the channel containing the messages that you want to redact.
    channelArn :: Prelude.Text,
    -- | The ID of the message being redacted.
    messageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedactChannelMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chimeBearer', 'redactChannelMessage_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
--
-- 'channelArn', 'redactChannelMessage_channelArn' - The ARN of the channel containing the messages that you want to redact.
--
-- 'messageId', 'redactChannelMessage_messageId' - The ID of the message being redacted.
newRedactChannelMessage ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'messageId'
  Prelude.Text ->
  RedactChannelMessage
newRedactChannelMessage pChannelArn_ pMessageId_ =
  RedactChannelMessage'
    { chimeBearer =
        Prelude.Nothing,
      channelArn = pChannelArn_,
      messageId = pMessageId_
    }

-- | The @AppInstanceUserArn@ of the user that makes the API call.
redactChannelMessage_chimeBearer :: Lens.Lens' RedactChannelMessage (Prelude.Maybe Prelude.Text)
redactChannelMessage_chimeBearer = Lens.lens (\RedactChannelMessage' {chimeBearer} -> chimeBearer) (\s@RedactChannelMessage' {} a -> s {chimeBearer = a} :: RedactChannelMessage)

-- | The ARN of the channel containing the messages that you want to redact.
redactChannelMessage_channelArn :: Lens.Lens' RedactChannelMessage Prelude.Text
redactChannelMessage_channelArn = Lens.lens (\RedactChannelMessage' {channelArn} -> channelArn) (\s@RedactChannelMessage' {} a -> s {channelArn = a} :: RedactChannelMessage)

-- | The ID of the message being redacted.
redactChannelMessage_messageId :: Lens.Lens' RedactChannelMessage Prelude.Text
redactChannelMessage_messageId = Lens.lens (\RedactChannelMessage' {messageId} -> messageId) (\s@RedactChannelMessage' {} a -> s {messageId = a} :: RedactChannelMessage)

instance Core.AWSRequest RedactChannelMessage where
  type
    AWSResponse RedactChannelMessage =
      RedactChannelMessageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RedactChannelMessageResponse'
            Prelude.<$> (x Core..?> "ChannelArn")
            Prelude.<*> (x Core..?> "MessageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RedactChannelMessage where
  hashWithSalt _salt RedactChannelMessage' {..} =
    _salt `Prelude.hashWithSalt` chimeBearer
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` messageId

instance Prelude.NFData RedactChannelMessage where
  rnf RedactChannelMessage' {..} =
    Prelude.rnf chimeBearer
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf messageId

instance Core.ToHeaders RedactChannelMessage where
  toHeaders RedactChannelMessage' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Core.=# chimeBearer]

instance Core.ToJSON RedactChannelMessage where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath RedactChannelMessage where
  toPath RedactChannelMessage' {..} =
    Prelude.mconcat
      [ "/channels/",
        Core.toBS channelArn,
        "/messages/",
        Core.toBS messageId
      ]

instance Core.ToQuery RedactChannelMessage where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["operation=redact"])

-- | /See:/ 'newRedactChannelMessageResponse' smart constructor.
data RedactChannelMessageResponse = RedactChannelMessageResponse'
  { -- | The ARN of the channel containing the messages that you want to redact.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the message being redacted.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedactChannelMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'redactChannelMessageResponse_channelArn' - The ARN of the channel containing the messages that you want to redact.
--
-- 'messageId', 'redactChannelMessageResponse_messageId' - The ID of the message being redacted.
--
-- 'httpStatus', 'redactChannelMessageResponse_httpStatus' - The response's http status code.
newRedactChannelMessageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RedactChannelMessageResponse
newRedactChannelMessageResponse pHttpStatus_ =
  RedactChannelMessageResponse'
    { channelArn =
        Prelude.Nothing,
      messageId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the channel containing the messages that you want to redact.
redactChannelMessageResponse_channelArn :: Lens.Lens' RedactChannelMessageResponse (Prelude.Maybe Prelude.Text)
redactChannelMessageResponse_channelArn = Lens.lens (\RedactChannelMessageResponse' {channelArn} -> channelArn) (\s@RedactChannelMessageResponse' {} a -> s {channelArn = a} :: RedactChannelMessageResponse)

-- | The ID of the message being redacted.
redactChannelMessageResponse_messageId :: Lens.Lens' RedactChannelMessageResponse (Prelude.Maybe Prelude.Text)
redactChannelMessageResponse_messageId = Lens.lens (\RedactChannelMessageResponse' {messageId} -> messageId) (\s@RedactChannelMessageResponse' {} a -> s {messageId = a} :: RedactChannelMessageResponse)

-- | The response's http status code.
redactChannelMessageResponse_httpStatus :: Lens.Lens' RedactChannelMessageResponse Prelude.Int
redactChannelMessageResponse_httpStatus = Lens.lens (\RedactChannelMessageResponse' {httpStatus} -> httpStatus) (\s@RedactChannelMessageResponse' {} a -> s {httpStatus = a} :: RedactChannelMessageResponse)

instance Prelude.NFData RedactChannelMessageResponse where
  rnf RedactChannelMessageResponse' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf httpStatus
