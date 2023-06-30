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
-- Module      : Amazonka.WorkMailMessageFlow.PutRawMessageContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the raw content of an in-transit email message, in MIME format.
--
-- This example describes how to update in-transit email message. For more
-- information and examples for using this API, see
-- <https://docs.aws.amazon.com/workmail/latest/adminguide/update-with-lambda.html Updating message content with AWS Lambda>.
--
-- Updates to an in-transit message only appear when you call
-- @PutRawMessageContent@ from an AWS Lambda function configured with a
-- synchronous
-- <https://docs.aws.amazon.com/workmail/latest/adminguide/lambda.html#synchronous-rules Run Lambda>
-- rule. If you call @PutRawMessageContent@ on a delivered or sent message,
-- the message remains unchanged, even though
-- <https://docs.aws.amazon.com/workmail/latest/APIReference/API_messageflow_GetRawMessageContent.html GetRawMessageContent>
-- returns an updated message.
module Amazonka.WorkMailMessageFlow.PutRawMessageContent
  ( -- * Creating a Request
    PutRawMessageContent (..),
    newPutRawMessageContent,

    -- * Request Lenses
    putRawMessageContent_messageId,
    putRawMessageContent_content,

    -- * Destructuring the Response
    PutRawMessageContentResponse (..),
    newPutRawMessageContentResponse,

    -- * Response Lenses
    putRawMessageContentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMailMessageFlow.Types

-- | /See:/ 'newPutRawMessageContent' smart constructor.
data PutRawMessageContent = PutRawMessageContent'
  { -- | The identifier of the email message being updated.
    messageId :: Prelude.Text,
    -- | Describes the raw message content of the updated email message.
    content :: RawMessageContent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRawMessageContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageId', 'putRawMessageContent_messageId' - The identifier of the email message being updated.
--
-- 'content', 'putRawMessageContent_content' - Describes the raw message content of the updated email message.
newPutRawMessageContent ::
  -- | 'messageId'
  Prelude.Text ->
  -- | 'content'
  RawMessageContent ->
  PutRawMessageContent
newPutRawMessageContent pMessageId_ pContent_ =
  PutRawMessageContent'
    { messageId = pMessageId_,
      content = pContent_
    }

-- | The identifier of the email message being updated.
putRawMessageContent_messageId :: Lens.Lens' PutRawMessageContent Prelude.Text
putRawMessageContent_messageId = Lens.lens (\PutRawMessageContent' {messageId} -> messageId) (\s@PutRawMessageContent' {} a -> s {messageId = a} :: PutRawMessageContent)

-- | Describes the raw message content of the updated email message.
putRawMessageContent_content :: Lens.Lens' PutRawMessageContent RawMessageContent
putRawMessageContent_content = Lens.lens (\PutRawMessageContent' {content} -> content) (\s@PutRawMessageContent' {} a -> s {content = a} :: PutRawMessageContent)

instance Core.AWSRequest PutRawMessageContent where
  type
    AWSResponse PutRawMessageContent =
      PutRawMessageContentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutRawMessageContentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutRawMessageContent where
  hashWithSalt _salt PutRawMessageContent' {..} =
    _salt
      `Prelude.hashWithSalt` messageId
      `Prelude.hashWithSalt` content

instance Prelude.NFData PutRawMessageContent where
  rnf PutRawMessageContent' {..} =
    Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf content

instance Data.ToHeaders PutRawMessageContent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutRawMessageContent where
  toJSON PutRawMessageContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("content" Data..= content)]
      )

instance Data.ToPath PutRawMessageContent where
  toPath PutRawMessageContent' {..} =
    Prelude.mconcat ["/messages/", Data.toBS messageId]

instance Data.ToQuery PutRawMessageContent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRawMessageContentResponse' smart constructor.
data PutRawMessageContentResponse = PutRawMessageContentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRawMessageContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putRawMessageContentResponse_httpStatus' - The response's http status code.
newPutRawMessageContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutRawMessageContentResponse
newPutRawMessageContentResponse pHttpStatus_ =
  PutRawMessageContentResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putRawMessageContentResponse_httpStatus :: Lens.Lens' PutRawMessageContentResponse Prelude.Int
putRawMessageContentResponse_httpStatus = Lens.lens (\PutRawMessageContentResponse' {httpStatus} -> httpStatus) (\s@PutRawMessageContentResponse' {} a -> s {httpStatus = a} :: PutRawMessageContentResponse)

instance Prelude.NFData PutRawMessageContentResponse where
  rnf PutRawMessageContentResponse' {..} =
    Prelude.rnf httpStatus
