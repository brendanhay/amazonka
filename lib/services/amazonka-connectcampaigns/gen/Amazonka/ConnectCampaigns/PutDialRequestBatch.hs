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
-- Module      : Amazonka.ConnectCampaigns.PutDialRequestBatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates dials requests for the specified campaign Amazon Connect
-- account. This API is idempotent.
module Amazonka.ConnectCampaigns.PutDialRequestBatch
  ( -- * Creating a Request
    PutDialRequestBatch (..),
    newPutDialRequestBatch,

    -- * Request Lenses
    putDialRequestBatch_dialRequests,
    putDialRequestBatch_id,

    -- * Destructuring the Response
    PutDialRequestBatchResponse (..),
    newPutDialRequestBatchResponse,

    -- * Response Lenses
    putDialRequestBatchResponse_failedRequests,
    putDialRequestBatchResponse_successfulRequests,
    putDialRequestBatchResponse_httpStatus,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | PutDialRequestBatchRequest
--
-- /See:/ 'newPutDialRequestBatch' smart constructor.
data PutDialRequestBatch = PutDialRequestBatch'
  { dialRequests :: Prelude.NonEmpty DialRequest,
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDialRequestBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dialRequests', 'putDialRequestBatch_dialRequests' - Undocumented member.
--
-- 'id', 'putDialRequestBatch_id' - Undocumented member.
newPutDialRequestBatch ::
  -- | 'dialRequests'
  Prelude.NonEmpty DialRequest ->
  -- | 'id'
  Prelude.Text ->
  PutDialRequestBatch
newPutDialRequestBatch pDialRequests_ pId_ =
  PutDialRequestBatch'
    { dialRequests =
        Lens.coerced Lens.# pDialRequests_,
      id = pId_
    }

-- | Undocumented member.
putDialRequestBatch_dialRequests :: Lens.Lens' PutDialRequestBatch (Prelude.NonEmpty DialRequest)
putDialRequestBatch_dialRequests = Lens.lens (\PutDialRequestBatch' {dialRequests} -> dialRequests) (\s@PutDialRequestBatch' {} a -> s {dialRequests = a} :: PutDialRequestBatch) Prelude.. Lens.coerced

-- | Undocumented member.
putDialRequestBatch_id :: Lens.Lens' PutDialRequestBatch Prelude.Text
putDialRequestBatch_id = Lens.lens (\PutDialRequestBatch' {id} -> id) (\s@PutDialRequestBatch' {} a -> s {id = a} :: PutDialRequestBatch)

instance Core.AWSRequest PutDialRequestBatch where
  type
    AWSResponse PutDialRequestBatch =
      PutDialRequestBatchResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutDialRequestBatchResponse'
            Prelude.<$> (x Data..?> "failedRequests" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "successfulRequests"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutDialRequestBatch where
  hashWithSalt _salt PutDialRequestBatch' {..} =
    _salt
      `Prelude.hashWithSalt` dialRequests
      `Prelude.hashWithSalt` id

instance Prelude.NFData PutDialRequestBatch where
  rnf PutDialRequestBatch' {..} =
    Prelude.rnf dialRequests
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders PutDialRequestBatch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutDialRequestBatch where
  toJSON PutDialRequestBatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("dialRequests" Data..= dialRequests)]
      )

instance Data.ToPath PutDialRequestBatch where
  toPath PutDialRequestBatch' {..} =
    Prelude.mconcat
      ["/campaigns/", Data.toBS id, "/dial-requests"]

instance Data.ToQuery PutDialRequestBatch where
  toQuery = Prelude.const Prelude.mempty

-- | PutDialRequestBatchResponse
--
-- /See:/ 'newPutDialRequestBatchResponse' smart constructor.
data PutDialRequestBatchResponse = PutDialRequestBatchResponse'
  { failedRequests :: Prelude.Maybe [FailedRequest],
    successfulRequests :: Prelude.Maybe [SuccessfulRequest],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDialRequestBatchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedRequests', 'putDialRequestBatchResponse_failedRequests' - Undocumented member.
--
-- 'successfulRequests', 'putDialRequestBatchResponse_successfulRequests' - Undocumented member.
--
-- 'httpStatus', 'putDialRequestBatchResponse_httpStatus' - The response's http status code.
newPutDialRequestBatchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutDialRequestBatchResponse
newPutDialRequestBatchResponse pHttpStatus_ =
  PutDialRequestBatchResponse'
    { failedRequests =
        Prelude.Nothing,
      successfulRequests = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
putDialRequestBatchResponse_failedRequests :: Lens.Lens' PutDialRequestBatchResponse (Prelude.Maybe [FailedRequest])
putDialRequestBatchResponse_failedRequests = Lens.lens (\PutDialRequestBatchResponse' {failedRequests} -> failedRequests) (\s@PutDialRequestBatchResponse' {} a -> s {failedRequests = a} :: PutDialRequestBatchResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
putDialRequestBatchResponse_successfulRequests :: Lens.Lens' PutDialRequestBatchResponse (Prelude.Maybe [SuccessfulRequest])
putDialRequestBatchResponse_successfulRequests = Lens.lens (\PutDialRequestBatchResponse' {successfulRequests} -> successfulRequests) (\s@PutDialRequestBatchResponse' {} a -> s {successfulRequests = a} :: PutDialRequestBatchResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
putDialRequestBatchResponse_httpStatus :: Lens.Lens' PutDialRequestBatchResponse Prelude.Int
putDialRequestBatchResponse_httpStatus = Lens.lens (\PutDialRequestBatchResponse' {httpStatus} -> httpStatus) (\s@PutDialRequestBatchResponse' {} a -> s {httpStatus = a} :: PutDialRequestBatchResponse)

instance Prelude.NFData PutDialRequestBatchResponse where
  rnf PutDialRequestBatchResponse' {..} =
    Prelude.rnf failedRequests
      `Prelude.seq` Prelude.rnf successfulRequests
      `Prelude.seq` Prelude.rnf httpStatus
