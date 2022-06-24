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
-- Module      : Amazonka.Pinpoint.SendMessages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and sends a direct message.
module Amazonka.Pinpoint.SendMessages
  ( -- * Creating a Request
    SendMessages (..),
    newSendMessages,

    -- * Request Lenses
    sendMessages_applicationId,
    sendMessages_messageRequest,

    -- * Destructuring the Response
    SendMessagesResponse (..),
    newSendMessagesResponse,

    -- * Response Lenses
    sendMessagesResponse_httpStatus,
    sendMessagesResponse_messageResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendMessages' smart constructor.
data SendMessages = SendMessages'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    messageRequest :: MessageRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendMessages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'sendMessages_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'messageRequest', 'sendMessages_messageRequest' - Undocumented member.
newSendMessages ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'messageRequest'
  MessageRequest ->
  SendMessages
newSendMessages pApplicationId_ pMessageRequest_ =
  SendMessages'
    { applicationId = pApplicationId_,
      messageRequest = pMessageRequest_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
sendMessages_applicationId :: Lens.Lens' SendMessages Prelude.Text
sendMessages_applicationId = Lens.lens (\SendMessages' {applicationId} -> applicationId) (\s@SendMessages' {} a -> s {applicationId = a} :: SendMessages)

-- | Undocumented member.
sendMessages_messageRequest :: Lens.Lens' SendMessages MessageRequest
sendMessages_messageRequest = Lens.lens (\SendMessages' {messageRequest} -> messageRequest) (\s@SendMessages' {} a -> s {messageRequest = a} :: SendMessages)

instance Core.AWSRequest SendMessages where
  type AWSResponse SendMessages = SendMessagesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SendMessagesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable SendMessages where
  hashWithSalt _salt SendMessages' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` messageRequest

instance Prelude.NFData SendMessages where
  rnf SendMessages' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf messageRequest

instance Core.ToHeaders SendMessages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SendMessages where
  toJSON SendMessages' {..} = Core.toJSON messageRequest

instance Core.ToPath SendMessages where
  toPath SendMessages' {..} =
    Prelude.mconcat
      ["/v1/apps/", Core.toBS applicationId, "/messages"]

instance Core.ToQuery SendMessages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendMessagesResponse' smart constructor.
data SendMessagesResponse = SendMessagesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    messageResponse :: MessageResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendMessagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'sendMessagesResponse_httpStatus' - The response's http status code.
--
-- 'messageResponse', 'sendMessagesResponse_messageResponse' - Undocumented member.
newSendMessagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'messageResponse'
  MessageResponse ->
  SendMessagesResponse
newSendMessagesResponse
  pHttpStatus_
  pMessageResponse_ =
    SendMessagesResponse'
      { httpStatus = pHttpStatus_,
        messageResponse = pMessageResponse_
      }

-- | The response's http status code.
sendMessagesResponse_httpStatus :: Lens.Lens' SendMessagesResponse Prelude.Int
sendMessagesResponse_httpStatus = Lens.lens (\SendMessagesResponse' {httpStatus} -> httpStatus) (\s@SendMessagesResponse' {} a -> s {httpStatus = a} :: SendMessagesResponse)

-- | Undocumented member.
sendMessagesResponse_messageResponse :: Lens.Lens' SendMessagesResponse MessageResponse
sendMessagesResponse_messageResponse = Lens.lens (\SendMessagesResponse' {messageResponse} -> messageResponse) (\s@SendMessagesResponse' {} a -> s {messageResponse = a} :: SendMessagesResponse)

instance Prelude.NFData SendMessagesResponse where
  rnf SendMessagesResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf messageResponse
