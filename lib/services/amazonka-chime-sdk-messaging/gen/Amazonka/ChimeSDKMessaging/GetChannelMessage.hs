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
-- Module      : Amazonka.ChimeSDKMessaging.GetChannelMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the full details of a channel message.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the ARN of the
-- @AppInstanceUser@ or @AppInstanceBot@ that makes the API call as the
-- value in the header.
module Amazonka.ChimeSDKMessaging.GetChannelMessage
  ( -- * Creating a Request
    GetChannelMessage (..),
    newGetChannelMessage,

    -- * Request Lenses
    getChannelMessage_subChannelId,
    getChannelMessage_channelArn,
    getChannelMessage_messageId,
    getChannelMessage_chimeBearer,

    -- * Destructuring the Response
    GetChannelMessageResponse (..),
    newGetChannelMessageResponse,

    -- * Response Lenses
    getChannelMessageResponse_channelMessage,
    getChannelMessageResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetChannelMessage' smart constructor.
data GetChannelMessage = GetChannelMessage'
  { -- | The ID of the SubChannel in the request.
    --
    -- Only required when getting messages in a SubChannel that the user
    -- belongs to.
    subChannelId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the channel.
    channelArn :: Prelude.Text,
    -- | The ID of the message.
    messageId :: Prelude.Text,
    -- | The ARN of the @AppInstanceUser@ or @AppInstanceBot@ that makes the API
    -- call.
    chimeBearer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChannelMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subChannelId', 'getChannelMessage_subChannelId' - The ID of the SubChannel in the request.
--
-- Only required when getting messages in a SubChannel that the user
-- belongs to.
--
-- 'channelArn', 'getChannelMessage_channelArn' - The ARN of the channel.
--
-- 'messageId', 'getChannelMessage_messageId' - The ID of the message.
--
-- 'chimeBearer', 'getChannelMessage_chimeBearer' - The ARN of the @AppInstanceUser@ or @AppInstanceBot@ that makes the API
-- call.
newGetChannelMessage ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'messageId'
  Prelude.Text ->
  -- | 'chimeBearer'
  Prelude.Text ->
  GetChannelMessage
newGetChannelMessage
  pChannelArn_
  pMessageId_
  pChimeBearer_ =
    GetChannelMessage'
      { subChannelId = Prelude.Nothing,
        channelArn = pChannelArn_,
        messageId = pMessageId_,
        chimeBearer = pChimeBearer_
      }

-- | The ID of the SubChannel in the request.
--
-- Only required when getting messages in a SubChannel that the user
-- belongs to.
getChannelMessage_subChannelId :: Lens.Lens' GetChannelMessage (Prelude.Maybe Prelude.Text)
getChannelMessage_subChannelId = Lens.lens (\GetChannelMessage' {subChannelId} -> subChannelId) (\s@GetChannelMessage' {} a -> s {subChannelId = a} :: GetChannelMessage)

-- | The ARN of the channel.
getChannelMessage_channelArn :: Lens.Lens' GetChannelMessage Prelude.Text
getChannelMessage_channelArn = Lens.lens (\GetChannelMessage' {channelArn} -> channelArn) (\s@GetChannelMessage' {} a -> s {channelArn = a} :: GetChannelMessage)

-- | The ID of the message.
getChannelMessage_messageId :: Lens.Lens' GetChannelMessage Prelude.Text
getChannelMessage_messageId = Lens.lens (\GetChannelMessage' {messageId} -> messageId) (\s@GetChannelMessage' {} a -> s {messageId = a} :: GetChannelMessage)

-- | The ARN of the @AppInstanceUser@ or @AppInstanceBot@ that makes the API
-- call.
getChannelMessage_chimeBearer :: Lens.Lens' GetChannelMessage Prelude.Text
getChannelMessage_chimeBearer = Lens.lens (\GetChannelMessage' {chimeBearer} -> chimeBearer) (\s@GetChannelMessage' {} a -> s {chimeBearer = a} :: GetChannelMessage)

instance Core.AWSRequest GetChannelMessage where
  type
    AWSResponse GetChannelMessage =
      GetChannelMessageResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChannelMessageResponse'
            Prelude.<$> (x Data..?> "ChannelMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetChannelMessage where
  hashWithSalt _salt GetChannelMessage' {..} =
    _salt
      `Prelude.hashWithSalt` subChannelId
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` messageId
      `Prelude.hashWithSalt` chimeBearer

instance Prelude.NFData GetChannelMessage where
  rnf GetChannelMessage' {..} =
    Prelude.rnf subChannelId
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf chimeBearer

instance Data.ToHeaders GetChannelMessage where
  toHeaders GetChannelMessage' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToPath GetChannelMessage where
  toPath GetChannelMessage' {..} =
    Prelude.mconcat
      [ "/channels/",
        Data.toBS channelArn,
        "/messages/",
        Data.toBS messageId
      ]

instance Data.ToQuery GetChannelMessage where
  toQuery GetChannelMessage' {..} =
    Prelude.mconcat
      ["sub-channel-id" Data.=: subChannelId]

-- | /See:/ 'newGetChannelMessageResponse' smart constructor.
data GetChannelMessageResponse = GetChannelMessageResponse'
  { -- | The details of and content in the message.
    channelMessage :: Prelude.Maybe ChannelMessage,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChannelMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelMessage', 'getChannelMessageResponse_channelMessage' - The details of and content in the message.
--
-- 'httpStatus', 'getChannelMessageResponse_httpStatus' - The response's http status code.
newGetChannelMessageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetChannelMessageResponse
newGetChannelMessageResponse pHttpStatus_ =
  GetChannelMessageResponse'
    { channelMessage =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of and content in the message.
getChannelMessageResponse_channelMessage :: Lens.Lens' GetChannelMessageResponse (Prelude.Maybe ChannelMessage)
getChannelMessageResponse_channelMessage = Lens.lens (\GetChannelMessageResponse' {channelMessage} -> channelMessage) (\s@GetChannelMessageResponse' {} a -> s {channelMessage = a} :: GetChannelMessageResponse)

-- | The response's http status code.
getChannelMessageResponse_httpStatus :: Lens.Lens' GetChannelMessageResponse Prelude.Int
getChannelMessageResponse_httpStatus = Lens.lens (\GetChannelMessageResponse' {httpStatus} -> httpStatus) (\s@GetChannelMessageResponse' {} a -> s {httpStatus = a} :: GetChannelMessageResponse)

instance Prelude.NFData GetChannelMessageResponse where
  rnf GetChannelMessageResponse' {..} =
    Prelude.rnf channelMessage
      `Prelude.seq` Prelude.rnf httpStatus
