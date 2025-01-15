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
-- Module      : Amazonka.Chime.RedactConversationMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Redacts the specified message from the specified Amazon Chime
-- conversation.
module Amazonka.Chime.RedactConversationMessage
  ( -- * Creating a Request
    RedactConversationMessage (..),
    newRedactConversationMessage,

    -- * Request Lenses
    redactConversationMessage_accountId,
    redactConversationMessage_conversationId,
    redactConversationMessage_messageId,

    -- * Destructuring the Response
    RedactConversationMessageResponse (..),
    newRedactConversationMessageResponse,

    -- * Response Lenses
    redactConversationMessageResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRedactConversationMessage' smart constructor.
data RedactConversationMessage = RedactConversationMessage'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The conversation ID.
    conversationId :: Prelude.Text,
    -- | The message ID.
    messageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedactConversationMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'redactConversationMessage_accountId' - The Amazon Chime account ID.
--
-- 'conversationId', 'redactConversationMessage_conversationId' - The conversation ID.
--
-- 'messageId', 'redactConversationMessage_messageId' - The message ID.
newRedactConversationMessage ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'conversationId'
  Prelude.Text ->
  -- | 'messageId'
  Prelude.Text ->
  RedactConversationMessage
newRedactConversationMessage
  pAccountId_
  pConversationId_
  pMessageId_ =
    RedactConversationMessage'
      { accountId = pAccountId_,
        conversationId = pConversationId_,
        messageId = pMessageId_
      }

-- | The Amazon Chime account ID.
redactConversationMessage_accountId :: Lens.Lens' RedactConversationMessage Prelude.Text
redactConversationMessage_accountId = Lens.lens (\RedactConversationMessage' {accountId} -> accountId) (\s@RedactConversationMessage' {} a -> s {accountId = a} :: RedactConversationMessage)

-- | The conversation ID.
redactConversationMessage_conversationId :: Lens.Lens' RedactConversationMessage Prelude.Text
redactConversationMessage_conversationId = Lens.lens (\RedactConversationMessage' {conversationId} -> conversationId) (\s@RedactConversationMessage' {} a -> s {conversationId = a} :: RedactConversationMessage)

-- | The message ID.
redactConversationMessage_messageId :: Lens.Lens' RedactConversationMessage Prelude.Text
redactConversationMessage_messageId = Lens.lens (\RedactConversationMessage' {messageId} -> messageId) (\s@RedactConversationMessage' {} a -> s {messageId = a} :: RedactConversationMessage)

instance Core.AWSRequest RedactConversationMessage where
  type
    AWSResponse RedactConversationMessage =
      RedactConversationMessageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RedactConversationMessageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RedactConversationMessage where
  hashWithSalt _salt RedactConversationMessage' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` conversationId
      `Prelude.hashWithSalt` messageId

instance Prelude.NFData RedactConversationMessage where
  rnf RedactConversationMessage' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf conversationId `Prelude.seq`
        Prelude.rnf messageId

instance Data.ToHeaders RedactConversationMessage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON RedactConversationMessage where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath RedactConversationMessage where
  toPath RedactConversationMessage' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS accountId,
        "/conversations/",
        Data.toBS conversationId,
        "/messages/",
        Data.toBS messageId
      ]

instance Data.ToQuery RedactConversationMessage where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["operation=redact"])

-- | /See:/ 'newRedactConversationMessageResponse' smart constructor.
data RedactConversationMessageResponse = RedactConversationMessageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedactConversationMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'redactConversationMessageResponse_httpStatus' - The response's http status code.
newRedactConversationMessageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RedactConversationMessageResponse
newRedactConversationMessageResponse pHttpStatus_ =
  RedactConversationMessageResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
redactConversationMessageResponse_httpStatus :: Lens.Lens' RedactConversationMessageResponse Prelude.Int
redactConversationMessageResponse_httpStatus = Lens.lens (\RedactConversationMessageResponse' {httpStatus} -> httpStatus) (\s@RedactConversationMessageResponse' {} a -> s {httpStatus = a} :: RedactConversationMessageResponse)

instance
  Prelude.NFData
    RedactConversationMessageResponse
  where
  rnf RedactConversationMessageResponse' {..} =
    Prelude.rnf httpStatus
