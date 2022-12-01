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
-- Module      : Amazonka.SQS.ChangeMessageVisibilityBatch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the visibility timeout of multiple messages. This is a batch
-- version of @ ChangeMessageVisibility.@ The result of the action on each
-- message is reported individually in the response. You can send up to 10
-- @ ChangeMessageVisibility @ requests with each
-- @ChangeMessageVisibilityBatch@ action.
--
-- Because the batch request can result in a combination of successful and
-- unsuccessful actions, you should check for batch errors even when the
-- call returns an HTTP status code of @200@.
--
-- Some actions take lists of parameters. These lists are specified using
-- the @param.n@ notation. Values of @n@ are integers starting from 1. For
-- example, a parameter list with two elements looks like this:
--
-- @&AttributeName.1=first@
--
-- @&AttributeName.2=second@
module Amazonka.SQS.ChangeMessageVisibilityBatch
  ( -- * Creating a Request
    ChangeMessageVisibilityBatch (..),
    newChangeMessageVisibilityBatch,

    -- * Request Lenses
    changeMessageVisibilityBatch_queueUrl,
    changeMessageVisibilityBatch_entries,

    -- * Destructuring the Response
    ChangeMessageVisibilityBatchResponse (..),
    newChangeMessageVisibilityBatchResponse,

    -- * Response Lenses
    changeMessageVisibilityBatchResponse_httpStatus,
    changeMessageVisibilityBatchResponse_successful,
    changeMessageVisibilityBatchResponse_failed,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SQS.Types

-- |
--
-- /See:/ 'newChangeMessageVisibilityBatch' smart constructor.
data ChangeMessageVisibilityBatch = ChangeMessageVisibilityBatch'
  { -- | The URL of the Amazon SQS queue whose messages\' visibility is changed.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Prelude.Text,
    -- | A list of receipt handles of the messages for which the visibility
    -- timeout must be changed.
    entries :: [ChangeMessageVisibilityBatchRequestEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeMessageVisibilityBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueUrl', 'changeMessageVisibilityBatch_queueUrl' - The URL of the Amazon SQS queue whose messages\' visibility is changed.
--
-- Queue URLs and names are case-sensitive.
--
-- 'entries', 'changeMessageVisibilityBatch_entries' - A list of receipt handles of the messages for which the visibility
-- timeout must be changed.
newChangeMessageVisibilityBatch ::
  -- | 'queueUrl'
  Prelude.Text ->
  ChangeMessageVisibilityBatch
newChangeMessageVisibilityBatch pQueueUrl_ =
  ChangeMessageVisibilityBatch'
    { queueUrl =
        pQueueUrl_,
      entries = Prelude.mempty
    }

-- | The URL of the Amazon SQS queue whose messages\' visibility is changed.
--
-- Queue URLs and names are case-sensitive.
changeMessageVisibilityBatch_queueUrl :: Lens.Lens' ChangeMessageVisibilityBatch Prelude.Text
changeMessageVisibilityBatch_queueUrl = Lens.lens (\ChangeMessageVisibilityBatch' {queueUrl} -> queueUrl) (\s@ChangeMessageVisibilityBatch' {} a -> s {queueUrl = a} :: ChangeMessageVisibilityBatch)

-- | A list of receipt handles of the messages for which the visibility
-- timeout must be changed.
changeMessageVisibilityBatch_entries :: Lens.Lens' ChangeMessageVisibilityBatch [ChangeMessageVisibilityBatchRequestEntry]
changeMessageVisibilityBatch_entries = Lens.lens (\ChangeMessageVisibilityBatch' {entries} -> entries) (\s@ChangeMessageVisibilityBatch' {} a -> s {entries = a} :: ChangeMessageVisibilityBatch) Prelude.. Lens.coerced

instance Core.AWSRequest ChangeMessageVisibilityBatch where
  type
    AWSResponse ChangeMessageVisibilityBatch =
      ChangeMessageVisibilityBatchResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ChangeMessageVisibilityBatchResult"
      ( \s h x ->
          ChangeMessageVisibilityBatchResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( Core.parseXMLList
                            "ChangeMessageVisibilityBatchResultEntry"
                            x
                        )
            Prelude.<*> (Core.parseXMLList "BatchResultErrorEntry" x)
      )

instance
  Prelude.Hashable
    ChangeMessageVisibilityBatch
  where
  hashWithSalt _salt ChangeMessageVisibilityBatch' {..} =
    _salt `Prelude.hashWithSalt` queueUrl
      `Prelude.hashWithSalt` entries

instance Prelude.NFData ChangeMessageVisibilityBatch where
  rnf ChangeMessageVisibilityBatch' {..} =
    Prelude.rnf queueUrl
      `Prelude.seq` Prelude.rnf entries

instance Core.ToHeaders ChangeMessageVisibilityBatch where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ChangeMessageVisibilityBatch where
  toPath = Prelude.const "/"

instance Core.ToQuery ChangeMessageVisibilityBatch where
  toQuery ChangeMessageVisibilityBatch' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ChangeMessageVisibilityBatch" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-11-05" :: Prelude.ByteString),
        "QueueUrl" Core.=: queueUrl,
        Core.toQueryList
          "ChangeMessageVisibilityBatchRequestEntry"
          entries
      ]

-- | For each message in the batch, the response contains a
-- @ ChangeMessageVisibilityBatchResultEntry @ tag if the message succeeds
-- or a @ BatchResultErrorEntry @ tag if the message fails.
--
-- /See:/ 'newChangeMessageVisibilityBatchResponse' smart constructor.
data ChangeMessageVisibilityBatchResponse = ChangeMessageVisibilityBatchResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of @ ChangeMessageVisibilityBatchResultEntry @ items.
    successful :: [ChangeMessageVisibilityBatchResultEntry],
    -- | A list of @ BatchResultErrorEntry @ items.
    failed :: [BatchResultErrorEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeMessageVisibilityBatchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'changeMessageVisibilityBatchResponse_httpStatus' - The response's http status code.
--
-- 'successful', 'changeMessageVisibilityBatchResponse_successful' - A list of @ ChangeMessageVisibilityBatchResultEntry @ items.
--
-- 'failed', 'changeMessageVisibilityBatchResponse_failed' - A list of @ BatchResultErrorEntry @ items.
newChangeMessageVisibilityBatchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ChangeMessageVisibilityBatchResponse
newChangeMessageVisibilityBatchResponse pHttpStatus_ =
  ChangeMessageVisibilityBatchResponse'
    { httpStatus =
        pHttpStatus_,
      successful = Prelude.mempty,
      failed = Prelude.mempty
    }

-- | The response's http status code.
changeMessageVisibilityBatchResponse_httpStatus :: Lens.Lens' ChangeMessageVisibilityBatchResponse Prelude.Int
changeMessageVisibilityBatchResponse_httpStatus = Lens.lens (\ChangeMessageVisibilityBatchResponse' {httpStatus} -> httpStatus) (\s@ChangeMessageVisibilityBatchResponse' {} a -> s {httpStatus = a} :: ChangeMessageVisibilityBatchResponse)

-- | A list of @ ChangeMessageVisibilityBatchResultEntry @ items.
changeMessageVisibilityBatchResponse_successful :: Lens.Lens' ChangeMessageVisibilityBatchResponse [ChangeMessageVisibilityBatchResultEntry]
changeMessageVisibilityBatchResponse_successful = Lens.lens (\ChangeMessageVisibilityBatchResponse' {successful} -> successful) (\s@ChangeMessageVisibilityBatchResponse' {} a -> s {successful = a} :: ChangeMessageVisibilityBatchResponse) Prelude.. Lens.coerced

-- | A list of @ BatchResultErrorEntry @ items.
changeMessageVisibilityBatchResponse_failed :: Lens.Lens' ChangeMessageVisibilityBatchResponse [BatchResultErrorEntry]
changeMessageVisibilityBatchResponse_failed = Lens.lens (\ChangeMessageVisibilityBatchResponse' {failed} -> failed) (\s@ChangeMessageVisibilityBatchResponse' {} a -> s {failed = a} :: ChangeMessageVisibilityBatchResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ChangeMessageVisibilityBatchResponse
  where
  rnf ChangeMessageVisibilityBatchResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf successful
      `Prelude.seq` Prelude.rnf failed
