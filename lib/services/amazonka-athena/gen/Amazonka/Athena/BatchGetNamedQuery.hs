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
-- Module      : Amazonka.Athena.BatchGetNamedQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of a single named query or a list of up to 50
-- queries, which you provide as an array of query ID strings. Requires you
-- to have access to the workgroup in which the queries were saved. Use
-- ListNamedQueriesInput to get the list of named query IDs in the
-- specified workgroup. If information could not be retrieved for a
-- submitted query ID, information about the query ID submitted is listed
-- under UnprocessedNamedQueryId. Named queries differ from executed
-- queries. Use BatchGetQueryExecutionInput to get details about each
-- unique query execution, and ListQueryExecutionsInput to get a list of
-- query execution IDs.
module Amazonka.Athena.BatchGetNamedQuery
  ( -- * Creating a Request
    BatchGetNamedQuery (..),
    newBatchGetNamedQuery,

    -- * Request Lenses
    batchGetNamedQuery_namedQueryIds,

    -- * Destructuring the Response
    BatchGetNamedQueryResponse (..),
    newBatchGetNamedQueryResponse,

    -- * Response Lenses
    batchGetNamedQueryResponse_namedQueries,
    batchGetNamedQueryResponse_unprocessedNamedQueryIds,
    batchGetNamedQueryResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains an array of named query IDs.
--
-- /See:/ 'newBatchGetNamedQuery' smart constructor.
data BatchGetNamedQuery = BatchGetNamedQuery'
  { -- | An array of query IDs.
    namedQueryIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetNamedQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namedQueryIds', 'batchGetNamedQuery_namedQueryIds' - An array of query IDs.
newBatchGetNamedQuery ::
  -- | 'namedQueryIds'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetNamedQuery
newBatchGetNamedQuery pNamedQueryIds_ =
  BatchGetNamedQuery'
    { namedQueryIds =
        Lens.coerced Lens.# pNamedQueryIds_
    }

-- | An array of query IDs.
batchGetNamedQuery_namedQueryIds :: Lens.Lens' BatchGetNamedQuery (Prelude.NonEmpty Prelude.Text)
batchGetNamedQuery_namedQueryIds = Lens.lens (\BatchGetNamedQuery' {namedQueryIds} -> namedQueryIds) (\s@BatchGetNamedQuery' {} a -> s {namedQueryIds = a} :: BatchGetNamedQuery) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetNamedQuery where
  type
    AWSResponse BatchGetNamedQuery =
      BatchGetNamedQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetNamedQueryResponse'
            Prelude.<$> (x Data..?> "NamedQueries" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "UnprocessedNamedQueryIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetNamedQuery where
  hashWithSalt _salt BatchGetNamedQuery' {..} =
    _salt `Prelude.hashWithSalt` namedQueryIds

instance Prelude.NFData BatchGetNamedQuery where
  rnf BatchGetNamedQuery' {..} =
    Prelude.rnf namedQueryIds

instance Data.ToHeaders BatchGetNamedQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.BatchGetNamedQuery" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetNamedQuery where
  toJSON BatchGetNamedQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("NamedQueryIds" Data..= namedQueryIds)
          ]
      )

instance Data.ToPath BatchGetNamedQuery where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetNamedQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetNamedQueryResponse' smart constructor.
data BatchGetNamedQueryResponse = BatchGetNamedQueryResponse'
  { -- | Information about the named query IDs submitted.
    namedQueries :: Prelude.Maybe [NamedQuery],
    -- | Information about provided query IDs.
    unprocessedNamedQueryIds :: Prelude.Maybe [UnprocessedNamedQueryId],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetNamedQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namedQueries', 'batchGetNamedQueryResponse_namedQueries' - Information about the named query IDs submitted.
--
-- 'unprocessedNamedQueryIds', 'batchGetNamedQueryResponse_unprocessedNamedQueryIds' - Information about provided query IDs.
--
-- 'httpStatus', 'batchGetNamedQueryResponse_httpStatus' - The response's http status code.
newBatchGetNamedQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetNamedQueryResponse
newBatchGetNamedQueryResponse pHttpStatus_ =
  BatchGetNamedQueryResponse'
    { namedQueries =
        Prelude.Nothing,
      unprocessedNamedQueryIds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the named query IDs submitted.
batchGetNamedQueryResponse_namedQueries :: Lens.Lens' BatchGetNamedQueryResponse (Prelude.Maybe [NamedQuery])
batchGetNamedQueryResponse_namedQueries = Lens.lens (\BatchGetNamedQueryResponse' {namedQueries} -> namedQueries) (\s@BatchGetNamedQueryResponse' {} a -> s {namedQueries = a} :: BatchGetNamedQueryResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about provided query IDs.
batchGetNamedQueryResponse_unprocessedNamedQueryIds :: Lens.Lens' BatchGetNamedQueryResponse (Prelude.Maybe [UnprocessedNamedQueryId])
batchGetNamedQueryResponse_unprocessedNamedQueryIds = Lens.lens (\BatchGetNamedQueryResponse' {unprocessedNamedQueryIds} -> unprocessedNamedQueryIds) (\s@BatchGetNamedQueryResponse' {} a -> s {unprocessedNamedQueryIds = a} :: BatchGetNamedQueryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetNamedQueryResponse_httpStatus :: Lens.Lens' BatchGetNamedQueryResponse Prelude.Int
batchGetNamedQueryResponse_httpStatus = Lens.lens (\BatchGetNamedQueryResponse' {httpStatus} -> httpStatus) (\s@BatchGetNamedQueryResponse' {} a -> s {httpStatus = a} :: BatchGetNamedQueryResponse)

instance Prelude.NFData BatchGetNamedQueryResponse where
  rnf BatchGetNamedQueryResponse' {..} =
    Prelude.rnf namedQueries
      `Prelude.seq` Prelude.rnf unprocessedNamedQueryIds
      `Prelude.seq` Prelude.rnf httpStatus
