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
-- Module      : Amazonka.Chime.DeleteChannelMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a channel message. Only admins can perform this action. Deletion
-- makes messages inaccessible immediately. A background process deletes
-- any revisions created by @UpdateChannelMessage@.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.Chime.DeleteChannelMessage
  ( -- * Creating a Request
    DeleteChannelMessage (..),
    newDeleteChannelMessage,

    -- * Request Lenses
    deleteChannelMessage_chimeBearer,
    deleteChannelMessage_channelArn,
    deleteChannelMessage_messageId,

    -- * Destructuring the Response
    DeleteChannelMessageResponse (..),
    newDeleteChannelMessageResponse,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteChannelMessage' smart constructor.
data DeleteChannelMessage = DeleteChannelMessage'
  { -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the channel.
    channelArn :: Prelude.Text,
    -- | The ID of the message being deleted.
    messageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannelMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chimeBearer', 'deleteChannelMessage_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
--
-- 'channelArn', 'deleteChannelMessage_channelArn' - The ARN of the channel.
--
-- 'messageId', 'deleteChannelMessage_messageId' - The ID of the message being deleted.
newDeleteChannelMessage ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'messageId'
  Prelude.Text ->
  DeleteChannelMessage
newDeleteChannelMessage pChannelArn_ pMessageId_ =
  DeleteChannelMessage'
    { chimeBearer =
        Prelude.Nothing,
      channelArn = pChannelArn_,
      messageId = pMessageId_
    }

-- | The @AppInstanceUserArn@ of the user that makes the API call.
deleteChannelMessage_chimeBearer :: Lens.Lens' DeleteChannelMessage (Prelude.Maybe Prelude.Text)
deleteChannelMessage_chimeBearer = Lens.lens (\DeleteChannelMessage' {chimeBearer} -> chimeBearer) (\s@DeleteChannelMessage' {} a -> s {chimeBearer = a} :: DeleteChannelMessage)

-- | The ARN of the channel.
deleteChannelMessage_channelArn :: Lens.Lens' DeleteChannelMessage Prelude.Text
deleteChannelMessage_channelArn = Lens.lens (\DeleteChannelMessage' {channelArn} -> channelArn) (\s@DeleteChannelMessage' {} a -> s {channelArn = a} :: DeleteChannelMessage)

-- | The ID of the message being deleted.
deleteChannelMessage_messageId :: Lens.Lens' DeleteChannelMessage Prelude.Text
deleteChannelMessage_messageId = Lens.lens (\DeleteChannelMessage' {messageId} -> messageId) (\s@DeleteChannelMessage' {} a -> s {messageId = a} :: DeleteChannelMessage)

instance Core.AWSRequest DeleteChannelMessage where
  type
    AWSResponse DeleteChannelMessage =
      DeleteChannelMessageResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteChannelMessageResponse'

instance Prelude.Hashable DeleteChannelMessage where
  hashWithSalt _salt DeleteChannelMessage' {..} =
    _salt `Prelude.hashWithSalt` chimeBearer
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` messageId

instance Prelude.NFData DeleteChannelMessage where
  rnf DeleteChannelMessage' {..} =
    Prelude.rnf chimeBearer
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf messageId

instance Data.ToHeaders DeleteChannelMessage where
  toHeaders DeleteChannelMessage' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToPath DeleteChannelMessage where
  toPath DeleteChannelMessage' {..} =
    Prelude.mconcat
      [ "/channels/",
        Data.toBS channelArn,
        "/messages/",
        Data.toBS messageId
      ]

instance Data.ToQuery DeleteChannelMessage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteChannelMessageResponse' smart constructor.
data DeleteChannelMessageResponse = DeleteChannelMessageResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannelMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteChannelMessageResponse ::
  DeleteChannelMessageResponse
newDeleteChannelMessageResponse =
  DeleteChannelMessageResponse'

instance Prelude.NFData DeleteChannelMessageResponse where
  rnf _ = ()
