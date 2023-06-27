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
-- Module      : Amazonka.SQS.GetQueueUrl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the URL of an existing Amazon SQS queue.
--
-- To access a queue that belongs to another AWS account, use the
-- @QueueOwnerAWSAccountId@ parameter to specify the account ID of the
-- queue\'s owner. The queue\'s owner must grant you permission to access
-- the queue. For more information about shared queue access, see
-- @ @@AddPermission@@ @ or see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-writing-an-sqs-policy.html#write-messages-to-shared-queue Allow Developers to Write Messages to a Shared Queue>
-- in the /Amazon SQS Developer Guide/.
module Amazonka.SQS.GetQueueUrl
  ( -- * Creating a Request
    GetQueueUrl (..),
    newGetQueueUrl,

    -- * Request Lenses
    getQueueUrl_queueOwnerAWSAccountId,
    getQueueUrl_queueName,

    -- * Destructuring the Response
    GetQueueUrlResponse (..),
    newGetQueueUrlResponse,

    -- * Response Lenses
    getQueueUrlResponse_httpStatus,
    getQueueUrlResponse_queueUrl,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SQS.Types

-- |
--
-- /See:/ 'newGetQueueUrl' smart constructor.
data GetQueueUrl = GetQueueUrl'
  { -- | The Amazon Web Services account ID of the account that created the
    -- queue.
    queueOwnerAWSAccountId :: Prelude.Maybe Prelude.Text,
    -- | The name of the queue whose URL must be fetched. Maximum 80 characters.
    -- Valid values: alphanumeric characters, hyphens (@-@), and underscores
    -- (@_@).
    --
    -- Queue URLs and names are case-sensitive.
    queueName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQueueUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueOwnerAWSAccountId', 'getQueueUrl_queueOwnerAWSAccountId' - The Amazon Web Services account ID of the account that created the
-- queue.
--
-- 'queueName', 'getQueueUrl_queueName' - The name of the queue whose URL must be fetched. Maximum 80 characters.
-- Valid values: alphanumeric characters, hyphens (@-@), and underscores
-- (@_@).
--
-- Queue URLs and names are case-sensitive.
newGetQueueUrl ::
  -- | 'queueName'
  Prelude.Text ->
  GetQueueUrl
newGetQueueUrl pQueueName_ =
  GetQueueUrl'
    { queueOwnerAWSAccountId =
        Prelude.Nothing,
      queueName = pQueueName_
    }

-- | The Amazon Web Services account ID of the account that created the
-- queue.
getQueueUrl_queueOwnerAWSAccountId :: Lens.Lens' GetQueueUrl (Prelude.Maybe Prelude.Text)
getQueueUrl_queueOwnerAWSAccountId = Lens.lens (\GetQueueUrl' {queueOwnerAWSAccountId} -> queueOwnerAWSAccountId) (\s@GetQueueUrl' {} a -> s {queueOwnerAWSAccountId = a} :: GetQueueUrl)

-- | The name of the queue whose URL must be fetched. Maximum 80 characters.
-- Valid values: alphanumeric characters, hyphens (@-@), and underscores
-- (@_@).
--
-- Queue URLs and names are case-sensitive.
getQueueUrl_queueName :: Lens.Lens' GetQueueUrl Prelude.Text
getQueueUrl_queueName = Lens.lens (\GetQueueUrl' {queueName} -> queueName) (\s@GetQueueUrl' {} a -> s {queueName = a} :: GetQueueUrl)

instance Core.AWSRequest GetQueueUrl where
  type AWSResponse GetQueueUrl = GetQueueUrlResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetQueueUrlResult"
      ( \s h x ->
          GetQueueUrlResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "QueueUrl")
      )

instance Prelude.Hashable GetQueueUrl where
  hashWithSalt _salt GetQueueUrl' {..} =
    _salt
      `Prelude.hashWithSalt` queueOwnerAWSAccountId
      `Prelude.hashWithSalt` queueName

instance Prelude.NFData GetQueueUrl where
  rnf GetQueueUrl' {..} =
    Prelude.rnf queueOwnerAWSAccountId
      `Prelude.seq` Prelude.rnf queueName

instance Data.ToHeaders GetQueueUrl where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetQueueUrl where
  toPath = Prelude.const "/"

instance Data.ToQuery GetQueueUrl where
  toQuery GetQueueUrl' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetQueueUrl" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-11-05" :: Prelude.ByteString),
        "QueueOwnerAWSAccountId"
          Data.=: queueOwnerAWSAccountId,
        "QueueName" Data.=: queueName
      ]

-- | For more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-api-responses.html Interpreting Responses>
-- in the /Amazon SQS Developer Guide/.
--
-- /See:/ 'newGetQueueUrlResponse' smart constructor.
data GetQueueUrlResponse = GetQueueUrlResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The URL of the queue.
    queueUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQueueUrlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getQueueUrlResponse_httpStatus' - The response's http status code.
--
-- 'queueUrl', 'getQueueUrlResponse_queueUrl' - The URL of the queue.
newGetQueueUrlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'queueUrl'
  Prelude.Text ->
  GetQueueUrlResponse
newGetQueueUrlResponse pHttpStatus_ pQueueUrl_ =
  GetQueueUrlResponse'
    { httpStatus = pHttpStatus_,
      queueUrl = pQueueUrl_
    }

-- | The response's http status code.
getQueueUrlResponse_httpStatus :: Lens.Lens' GetQueueUrlResponse Prelude.Int
getQueueUrlResponse_httpStatus = Lens.lens (\GetQueueUrlResponse' {httpStatus} -> httpStatus) (\s@GetQueueUrlResponse' {} a -> s {httpStatus = a} :: GetQueueUrlResponse)

-- | The URL of the queue.
getQueueUrlResponse_queueUrl :: Lens.Lens' GetQueueUrlResponse Prelude.Text
getQueueUrlResponse_queueUrl = Lens.lens (\GetQueueUrlResponse' {queueUrl} -> queueUrl) (\s@GetQueueUrlResponse' {} a -> s {queueUrl = a} :: GetQueueUrlResponse)

instance Prelude.NFData GetQueueUrlResponse where
  rnf GetQueueUrlResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf queueUrl
