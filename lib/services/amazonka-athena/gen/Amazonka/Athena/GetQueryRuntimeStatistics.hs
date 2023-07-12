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
-- Module      : Amazonka.Athena.GetQueryRuntimeStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns query execution runtime statistics related to a single execution
-- of a query if you have access to the workgroup in which the query ran.
-- Query execution runtime statistics are returned only when
-- QueryExecutionStatus$State is in a SUCCEEDED or FAILED state.
-- Stage-level input and output row count and data size statistics are not
-- shown when a query has row-level filters defined in Lake Formation.
module Amazonka.Athena.GetQueryRuntimeStatistics
  ( -- * Creating a Request
    GetQueryRuntimeStatistics (..),
    newGetQueryRuntimeStatistics,

    -- * Request Lenses
    getQueryRuntimeStatistics_queryExecutionId,

    -- * Destructuring the Response
    GetQueryRuntimeStatisticsResponse (..),
    newGetQueryRuntimeStatisticsResponse,

    -- * Response Lenses
    getQueryRuntimeStatisticsResponse_queryRuntimeStatistics,
    getQueryRuntimeStatisticsResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetQueryRuntimeStatistics' smart constructor.
data GetQueryRuntimeStatistics = GetQueryRuntimeStatistics'
  { -- | The unique ID of the query execution.
    queryExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQueryRuntimeStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryExecutionId', 'getQueryRuntimeStatistics_queryExecutionId' - The unique ID of the query execution.
newGetQueryRuntimeStatistics ::
  -- | 'queryExecutionId'
  Prelude.Text ->
  GetQueryRuntimeStatistics
newGetQueryRuntimeStatistics pQueryExecutionId_ =
  GetQueryRuntimeStatistics'
    { queryExecutionId =
        pQueryExecutionId_
    }

-- | The unique ID of the query execution.
getQueryRuntimeStatistics_queryExecutionId :: Lens.Lens' GetQueryRuntimeStatistics Prelude.Text
getQueryRuntimeStatistics_queryExecutionId = Lens.lens (\GetQueryRuntimeStatistics' {queryExecutionId} -> queryExecutionId) (\s@GetQueryRuntimeStatistics' {} a -> s {queryExecutionId = a} :: GetQueryRuntimeStatistics)

instance Core.AWSRequest GetQueryRuntimeStatistics where
  type
    AWSResponse GetQueryRuntimeStatistics =
      GetQueryRuntimeStatisticsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetQueryRuntimeStatisticsResponse'
            Prelude.<$> (x Data..?> "QueryRuntimeStatistics")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetQueryRuntimeStatistics where
  hashWithSalt _salt GetQueryRuntimeStatistics' {..} =
    _salt `Prelude.hashWithSalt` queryExecutionId

instance Prelude.NFData GetQueryRuntimeStatistics where
  rnf GetQueryRuntimeStatistics' {..} =
    Prelude.rnf queryExecutionId

instance Data.ToHeaders GetQueryRuntimeStatistics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.GetQueryRuntimeStatistics" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetQueryRuntimeStatistics where
  toJSON GetQueryRuntimeStatistics' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("QueryExecutionId" Data..= queryExecutionId)
          ]
      )

instance Data.ToPath GetQueryRuntimeStatistics where
  toPath = Prelude.const "/"

instance Data.ToQuery GetQueryRuntimeStatistics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetQueryRuntimeStatisticsResponse' smart constructor.
data GetQueryRuntimeStatisticsResponse = GetQueryRuntimeStatisticsResponse'
  { -- | Runtime statistics about the query execution.
    queryRuntimeStatistics :: Prelude.Maybe QueryRuntimeStatistics,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQueryRuntimeStatisticsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryRuntimeStatistics', 'getQueryRuntimeStatisticsResponse_queryRuntimeStatistics' - Runtime statistics about the query execution.
--
-- 'httpStatus', 'getQueryRuntimeStatisticsResponse_httpStatus' - The response's http status code.
newGetQueryRuntimeStatisticsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetQueryRuntimeStatisticsResponse
newGetQueryRuntimeStatisticsResponse pHttpStatus_ =
  GetQueryRuntimeStatisticsResponse'
    { queryRuntimeStatistics =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Runtime statistics about the query execution.
getQueryRuntimeStatisticsResponse_queryRuntimeStatistics :: Lens.Lens' GetQueryRuntimeStatisticsResponse (Prelude.Maybe QueryRuntimeStatistics)
getQueryRuntimeStatisticsResponse_queryRuntimeStatistics = Lens.lens (\GetQueryRuntimeStatisticsResponse' {queryRuntimeStatistics} -> queryRuntimeStatistics) (\s@GetQueryRuntimeStatisticsResponse' {} a -> s {queryRuntimeStatistics = a} :: GetQueryRuntimeStatisticsResponse)

-- | The response's http status code.
getQueryRuntimeStatisticsResponse_httpStatus :: Lens.Lens' GetQueryRuntimeStatisticsResponse Prelude.Int
getQueryRuntimeStatisticsResponse_httpStatus = Lens.lens (\GetQueryRuntimeStatisticsResponse' {httpStatus} -> httpStatus) (\s@GetQueryRuntimeStatisticsResponse' {} a -> s {httpStatus = a} :: GetQueryRuntimeStatisticsResponse)

instance
  Prelude.NFData
    GetQueryRuntimeStatisticsResponse
  where
  rnf GetQueryRuntimeStatisticsResponse' {..} =
    Prelude.rnf queryRuntimeStatistics
      `Prelude.seq` Prelude.rnf httpStatus
