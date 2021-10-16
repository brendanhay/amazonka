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
-- Module      : Network.AWS.Athena.BatchGetNamedQuery
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Athena.BatchGetNamedQuery
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

import Network.AWS.Athena.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetNamedQuery' smart constructor.
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
        Lens._Coerce Lens.# pNamedQueryIds_
    }

-- | An array of query IDs.
batchGetNamedQuery_namedQueryIds :: Lens.Lens' BatchGetNamedQuery (Prelude.NonEmpty Prelude.Text)
batchGetNamedQuery_namedQueryIds = Lens.lens (\BatchGetNamedQuery' {namedQueryIds} -> namedQueryIds) (\s@BatchGetNamedQuery' {} a -> s {namedQueryIds = a} :: BatchGetNamedQuery) Prelude.. Lens._Coerce

instance Core.AWSRequest BatchGetNamedQuery where
  type
    AWSResponse BatchGetNamedQuery =
      BatchGetNamedQueryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetNamedQueryResponse'
            Prelude.<$> (x Core..?> "NamedQueries" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "UnprocessedNamedQueryIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetNamedQuery

instance Prelude.NFData BatchGetNamedQuery

instance Core.ToHeaders BatchGetNamedQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonAthena.BatchGetNamedQuery" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchGetNamedQuery where
  toJSON BatchGetNamedQuery' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("NamedQueryIds" Core..= namedQueryIds)
          ]
      )

instance Core.ToPath BatchGetNamedQuery where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchGetNamedQuery where
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
batchGetNamedQueryResponse_namedQueries = Lens.lens (\BatchGetNamedQueryResponse' {namedQueries} -> namedQueries) (\s@BatchGetNamedQueryResponse' {} a -> s {namedQueries = a} :: BatchGetNamedQueryResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Information about provided query IDs.
batchGetNamedQueryResponse_unprocessedNamedQueryIds :: Lens.Lens' BatchGetNamedQueryResponse (Prelude.Maybe [UnprocessedNamedQueryId])
batchGetNamedQueryResponse_unprocessedNamedQueryIds = Lens.lens (\BatchGetNamedQueryResponse' {unprocessedNamedQueryIds} -> unprocessedNamedQueryIds) (\s@BatchGetNamedQueryResponse' {} a -> s {unprocessedNamedQueryIds = a} :: BatchGetNamedQueryResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetNamedQueryResponse_httpStatus :: Lens.Lens' BatchGetNamedQueryResponse Prelude.Int
batchGetNamedQueryResponse_httpStatus = Lens.lens (\BatchGetNamedQueryResponse' {httpStatus} -> httpStatus) (\s@BatchGetNamedQueryResponse' {} a -> s {httpStatus = a} :: BatchGetNamedQueryResponse)

instance Prelude.NFData BatchGetNamedQueryResponse
