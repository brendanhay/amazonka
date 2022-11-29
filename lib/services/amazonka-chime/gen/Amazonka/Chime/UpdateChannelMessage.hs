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
-- Module      : Amazonka.Chime.UpdateChannelMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the content of a message.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.Chime.UpdateChannelMessage
  ( -- * Creating a Request
    UpdateChannelMessage (..),
    newUpdateChannelMessage,

    -- * Request Lenses
    updateChannelMessage_chimeBearer,
    updateChannelMessage_metadata,
    updateChannelMessage_content,
    updateChannelMessage_channelArn,
    updateChannelMessage_messageId,

    -- * Destructuring the Response
    UpdateChannelMessageResponse (..),
    newUpdateChannelMessageResponse,

    -- * Response Lenses
    updateChannelMessageResponse_channelArn,
    updateChannelMessageResponse_messageId,
    updateChannelMessageResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateChannelMessage' smart constructor.
data UpdateChannelMessage = UpdateChannelMessage'
  { -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Maybe Prelude.Text,
    -- | The metadata of the message being updated.
    metadata :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The content of the message being updated.
    content :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ARN of the channel.
    channelArn :: Prelude.Text,
    -- | The ID string of the message being updated.
    messageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateChannelMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chimeBearer', 'updateChannelMessage_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
--
-- 'metadata', 'updateChannelMessage_metadata' - The metadata of the message being updated.
--
-- 'content', 'updateChannelMessage_content' - The content of the message being updated.
--
-- 'channelArn', 'updateChannelMessage_channelArn' - The ARN of the channel.
--
-- 'messageId', 'updateChannelMessage_messageId' - The ID string of the message being updated.
newUpdateChannelMessage ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'messageId'
  Prelude.Text ->
  UpdateChannelMessage
newUpdateChannelMessage pChannelArn_ pMessageId_ =
  UpdateChannelMessage'
    { chimeBearer =
        Prelude.Nothing,
      metadata = Prelude.Nothing,
      content = Prelude.Nothing,
      channelArn = pChannelArn_,
      messageId = pMessageId_
    }

-- | The @AppInstanceUserArn@ of the user that makes the API call.
updateChannelMessage_chimeBearer :: Lens.Lens' UpdateChannelMessage (Prelude.Maybe Prelude.Text)
updateChannelMessage_chimeBearer = Lens.lens (\UpdateChannelMessage' {chimeBearer} -> chimeBearer) (\s@UpdateChannelMessage' {} a -> s {chimeBearer = a} :: UpdateChannelMessage)

-- | The metadata of the message being updated.
updateChannelMessage_metadata :: Lens.Lens' UpdateChannelMessage (Prelude.Maybe Prelude.Text)
updateChannelMessage_metadata = Lens.lens (\UpdateChannelMessage' {metadata} -> metadata) (\s@UpdateChannelMessage' {} a -> s {metadata = a} :: UpdateChannelMessage) Prelude.. Lens.mapping Core._Sensitive

-- | The content of the message being updated.
updateChannelMessage_content :: Lens.Lens' UpdateChannelMessage (Prelude.Maybe Prelude.Text)
updateChannelMessage_content = Lens.lens (\UpdateChannelMessage' {content} -> content) (\s@UpdateChannelMessage' {} a -> s {content = a} :: UpdateChannelMessage) Prelude.. Lens.mapping Core._Sensitive

-- | The ARN of the channel.
updateChannelMessage_channelArn :: Lens.Lens' UpdateChannelMessage Prelude.Text
updateChannelMessage_channelArn = Lens.lens (\UpdateChannelMessage' {channelArn} -> channelArn) (\s@UpdateChannelMessage' {} a -> s {channelArn = a} :: UpdateChannelMessage)

-- | The ID string of the message being updated.
updateChannelMessage_messageId :: Lens.Lens' UpdateChannelMessage Prelude.Text
updateChannelMessage_messageId = Lens.lens (\UpdateChannelMessage' {messageId} -> messageId) (\s@UpdateChannelMessage' {} a -> s {messageId = a} :: UpdateChannelMessage)

instance Core.AWSRequest UpdateChannelMessage where
  type
    AWSResponse UpdateChannelMessage =
      UpdateChannelMessageResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateChannelMessageResponse'
            Prelude.<$> (x Core..?> "ChannelArn")
            Prelude.<*> (x Core..?> "MessageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateChannelMessage where
  hashWithSalt _salt UpdateChannelMessage' {..} =
    _salt `Prelude.hashWithSalt` chimeBearer
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` messageId

instance Prelude.NFData UpdateChannelMessage where
  rnf UpdateChannelMessage' {..} =
    Prelude.rnf chimeBearer
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf messageId

instance Core.ToHeaders UpdateChannelMessage where
  toHeaders UpdateChannelMessage' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Core.=# chimeBearer]

instance Core.ToJSON UpdateChannelMessage where
  toJSON UpdateChannelMessage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Metadata" Core..=) Prelude.<$> metadata,
            ("Content" Core..=) Prelude.<$> content
          ]
      )

instance Core.ToPath UpdateChannelMessage where
  toPath UpdateChannelMessage' {..} =
    Prelude.mconcat
      [ "/channels/",
        Core.toBS channelArn,
        "/messages/",
        Core.toBS messageId
      ]

instance Core.ToQuery UpdateChannelMessage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateChannelMessageResponse' smart constructor.
data UpdateChannelMessageResponse = UpdateChannelMessageResponse'
  { -- | The ARN of the channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The ID string of the message being updated.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateChannelMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'updateChannelMessageResponse_channelArn' - The ARN of the channel.
--
-- 'messageId', 'updateChannelMessageResponse_messageId' - The ID string of the message being updated.
--
-- 'httpStatus', 'updateChannelMessageResponse_httpStatus' - The response's http status code.
newUpdateChannelMessageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateChannelMessageResponse
newUpdateChannelMessageResponse pHttpStatus_ =
  UpdateChannelMessageResponse'
    { channelArn =
        Prelude.Nothing,
      messageId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the channel.
updateChannelMessageResponse_channelArn :: Lens.Lens' UpdateChannelMessageResponse (Prelude.Maybe Prelude.Text)
updateChannelMessageResponse_channelArn = Lens.lens (\UpdateChannelMessageResponse' {channelArn} -> channelArn) (\s@UpdateChannelMessageResponse' {} a -> s {channelArn = a} :: UpdateChannelMessageResponse)

-- | The ID string of the message being updated.
updateChannelMessageResponse_messageId :: Lens.Lens' UpdateChannelMessageResponse (Prelude.Maybe Prelude.Text)
updateChannelMessageResponse_messageId = Lens.lens (\UpdateChannelMessageResponse' {messageId} -> messageId) (\s@UpdateChannelMessageResponse' {} a -> s {messageId = a} :: UpdateChannelMessageResponse)

-- | The response's http status code.
updateChannelMessageResponse_httpStatus :: Lens.Lens' UpdateChannelMessageResponse Prelude.Int
updateChannelMessageResponse_httpStatus = Lens.lens (\UpdateChannelMessageResponse' {httpStatus} -> httpStatus) (\s@UpdateChannelMessageResponse' {} a -> s {httpStatus = a} :: UpdateChannelMessageResponse)

instance Prelude.NFData UpdateChannelMessageResponse where
  rnf UpdateChannelMessageResponse' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf httpStatus
