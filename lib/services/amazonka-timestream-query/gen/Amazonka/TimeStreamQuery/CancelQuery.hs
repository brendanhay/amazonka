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
-- Module      : Amazonka.TimeStreamQuery.CancelQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a query that has been issued. Cancellation is provided only if
-- the query has not completed running before the cancellation request was
-- issued. Because cancellation is an idempotent operation, subsequent
-- cancellation requests will return a @CancellationMessage@, indicating
-- that the query has already been canceled. See
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/code-samples.cancel-query.html code sample>
-- for details.
module Amazonka.TimeStreamQuery.CancelQuery
  ( -- * Creating a Request
    CancelQuery (..),
    newCancelQuery,

    -- * Request Lenses
    cancelQuery_queryId,

    -- * Destructuring the Response
    CancelQueryResponse (..),
    newCancelQueryResponse,

    -- * Response Lenses
    cancelQueryResponse_cancellationMessage,
    cancelQueryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamQuery.Types

-- | /See:/ 'newCancelQuery' smart constructor.
data CancelQuery = CancelQuery'
  { -- | The ID of the query that needs to be cancelled. @QueryID@ is returned as
    -- part of the query result.
    queryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryId', 'cancelQuery_queryId' - The ID of the query that needs to be cancelled. @QueryID@ is returned as
-- part of the query result.
newCancelQuery ::
  -- | 'queryId'
  Prelude.Text ->
  CancelQuery
newCancelQuery pQueryId_ =
  CancelQuery' {queryId = pQueryId_}

-- | The ID of the query that needs to be cancelled. @QueryID@ is returned as
-- part of the query result.
cancelQuery_queryId :: Lens.Lens' CancelQuery Prelude.Text
cancelQuery_queryId = Lens.lens (\CancelQuery' {queryId} -> queryId) (\s@CancelQuery' {} a -> s {queryId = a} :: CancelQuery)

instance Core.AWSRequest CancelQuery where
  type AWSResponse CancelQuery = CancelQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelQueryResponse'
            Prelude.<$> (x Data..?> "CancellationMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelQuery where
  hashWithSalt _salt CancelQuery' {..} =
    _salt `Prelude.hashWithSalt` queryId

instance Prelude.NFData CancelQuery where
  rnf CancelQuery' {..} = Prelude.rnf queryId

instance Data.ToHeaders CancelQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.CancelQuery" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelQuery where
  toJSON CancelQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("QueryId" Data..= queryId)]
      )

instance Data.ToPath CancelQuery where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelQueryResponse' smart constructor.
data CancelQueryResponse = CancelQueryResponse'
  { -- | A @CancellationMessage@ is returned when a @CancelQuery@ request for the
    -- query specified by @QueryId@ has already been issued.
    cancellationMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cancellationMessage', 'cancelQueryResponse_cancellationMessage' - A @CancellationMessage@ is returned when a @CancelQuery@ request for the
-- query specified by @QueryId@ has already been issued.
--
-- 'httpStatus', 'cancelQueryResponse_httpStatus' - The response's http status code.
newCancelQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelQueryResponse
newCancelQueryResponse pHttpStatus_ =
  CancelQueryResponse'
    { cancellationMessage =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A @CancellationMessage@ is returned when a @CancelQuery@ request for the
-- query specified by @QueryId@ has already been issued.
cancelQueryResponse_cancellationMessage :: Lens.Lens' CancelQueryResponse (Prelude.Maybe Prelude.Text)
cancelQueryResponse_cancellationMessage = Lens.lens (\CancelQueryResponse' {cancellationMessage} -> cancellationMessage) (\s@CancelQueryResponse' {} a -> s {cancellationMessage = a} :: CancelQueryResponse)

-- | The response's http status code.
cancelQueryResponse_httpStatus :: Lens.Lens' CancelQueryResponse Prelude.Int
cancelQueryResponse_httpStatus = Lens.lens (\CancelQueryResponse' {httpStatus} -> httpStatus) (\s@CancelQueryResponse' {} a -> s {httpStatus = a} :: CancelQueryResponse)

instance Prelude.NFData CancelQueryResponse where
  rnf CancelQueryResponse' {..} =
    Prelude.rnf cancellationMessage
      `Prelude.seq` Prelude.rnf httpStatus
