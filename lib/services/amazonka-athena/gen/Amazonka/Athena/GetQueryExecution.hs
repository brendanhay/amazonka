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
-- Module      : Amazonka.Athena.GetQueryExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a single execution of a query if you have
-- access to the workgroup in which the query ran. Each time a query
-- executes, information about the query execution is saved with a unique
-- ID.
module Amazonka.Athena.GetQueryExecution
  ( -- * Creating a Request
    GetQueryExecution (..),
    newGetQueryExecution,

    -- * Request Lenses
    getQueryExecution_queryExecutionId,

    -- * Destructuring the Response
    GetQueryExecutionResponse (..),
    newGetQueryExecutionResponse,

    -- * Response Lenses
    getQueryExecutionResponse_queryExecution,
    getQueryExecutionResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetQueryExecution' smart constructor.
data GetQueryExecution = GetQueryExecution'
  { -- | The unique ID of the query execution.
    queryExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQueryExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryExecutionId', 'getQueryExecution_queryExecutionId' - The unique ID of the query execution.
newGetQueryExecution ::
  -- | 'queryExecutionId'
  Prelude.Text ->
  GetQueryExecution
newGetQueryExecution pQueryExecutionId_ =
  GetQueryExecution'
    { queryExecutionId =
        pQueryExecutionId_
    }

-- | The unique ID of the query execution.
getQueryExecution_queryExecutionId :: Lens.Lens' GetQueryExecution Prelude.Text
getQueryExecution_queryExecutionId = Lens.lens (\GetQueryExecution' {queryExecutionId} -> queryExecutionId) (\s@GetQueryExecution' {} a -> s {queryExecutionId = a} :: GetQueryExecution)

instance Core.AWSRequest GetQueryExecution where
  type
    AWSResponse GetQueryExecution =
      GetQueryExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetQueryExecutionResponse'
            Prelude.<$> (x Data..?> "QueryExecution")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetQueryExecution where
  hashWithSalt _salt GetQueryExecution' {..} =
    _salt `Prelude.hashWithSalt` queryExecutionId

instance Prelude.NFData GetQueryExecution where
  rnf GetQueryExecution' {..} =
    Prelude.rnf queryExecutionId

instance Data.ToHeaders GetQueryExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.GetQueryExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetQueryExecution where
  toJSON GetQueryExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("QueryExecutionId" Data..= queryExecutionId)
          ]
      )

instance Data.ToPath GetQueryExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery GetQueryExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetQueryExecutionResponse' smart constructor.
data GetQueryExecutionResponse = GetQueryExecutionResponse'
  { -- | Information about the query execution.
    queryExecution :: Prelude.Maybe QueryExecution,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQueryExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryExecution', 'getQueryExecutionResponse_queryExecution' - Information about the query execution.
--
-- 'httpStatus', 'getQueryExecutionResponse_httpStatus' - The response's http status code.
newGetQueryExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetQueryExecutionResponse
newGetQueryExecutionResponse pHttpStatus_ =
  GetQueryExecutionResponse'
    { queryExecution =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the query execution.
getQueryExecutionResponse_queryExecution :: Lens.Lens' GetQueryExecutionResponse (Prelude.Maybe QueryExecution)
getQueryExecutionResponse_queryExecution = Lens.lens (\GetQueryExecutionResponse' {queryExecution} -> queryExecution) (\s@GetQueryExecutionResponse' {} a -> s {queryExecution = a} :: GetQueryExecutionResponse)

-- | The response's http status code.
getQueryExecutionResponse_httpStatus :: Lens.Lens' GetQueryExecutionResponse Prelude.Int
getQueryExecutionResponse_httpStatus = Lens.lens (\GetQueryExecutionResponse' {httpStatus} -> httpStatus) (\s@GetQueryExecutionResponse' {} a -> s {httpStatus = a} :: GetQueryExecutionResponse)

instance Prelude.NFData GetQueryExecutionResponse where
  rnf GetQueryExecutionResponse' {..} =
    Prelude.rnf queryExecution
      `Prelude.seq` Prelude.rnf httpStatus
