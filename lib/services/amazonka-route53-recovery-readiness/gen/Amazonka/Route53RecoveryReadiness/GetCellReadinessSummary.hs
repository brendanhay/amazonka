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
-- Module      : Amazonka.Route53RecoveryReadiness.GetCellReadinessSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets readiness for a cell. Aggregates the readiness of all the resources
-- that are associated with the cell into a single value.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryReadiness.GetCellReadinessSummary
  ( -- * Creating a Request
    GetCellReadinessSummary (..),
    newGetCellReadinessSummary,

    -- * Request Lenses
    getCellReadinessSummary_maxResults,
    getCellReadinessSummary_nextToken,
    getCellReadinessSummary_cellName,

    -- * Destructuring the Response
    GetCellReadinessSummaryResponse (..),
    newGetCellReadinessSummaryResponse,

    -- * Response Lenses
    getCellReadinessSummaryResponse_nextToken,
    getCellReadinessSummaryResponse_readiness,
    getCellReadinessSummaryResponse_readinessChecks,
    getCellReadinessSummaryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newGetCellReadinessSummary' smart constructor.
data GetCellReadinessSummary = GetCellReadinessSummary'
  { -- | The number of objects that you want to return with this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the cell.
    cellName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCellReadinessSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getCellReadinessSummary_maxResults' - The number of objects that you want to return with this call.
--
-- 'nextToken', 'getCellReadinessSummary_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'cellName', 'getCellReadinessSummary_cellName' - The name of the cell.
newGetCellReadinessSummary ::
  -- | 'cellName'
  Prelude.Text ->
  GetCellReadinessSummary
newGetCellReadinessSummary pCellName_ =
  GetCellReadinessSummary'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      cellName = pCellName_
    }

-- | The number of objects that you want to return with this call.
getCellReadinessSummary_maxResults :: Lens.Lens' GetCellReadinessSummary (Prelude.Maybe Prelude.Natural)
getCellReadinessSummary_maxResults = Lens.lens (\GetCellReadinessSummary' {maxResults} -> maxResults) (\s@GetCellReadinessSummary' {} a -> s {maxResults = a} :: GetCellReadinessSummary)

-- | The token that identifies which batch of results you want to see.
getCellReadinessSummary_nextToken :: Lens.Lens' GetCellReadinessSummary (Prelude.Maybe Prelude.Text)
getCellReadinessSummary_nextToken = Lens.lens (\GetCellReadinessSummary' {nextToken} -> nextToken) (\s@GetCellReadinessSummary' {} a -> s {nextToken = a} :: GetCellReadinessSummary)

-- | The name of the cell.
getCellReadinessSummary_cellName :: Lens.Lens' GetCellReadinessSummary Prelude.Text
getCellReadinessSummary_cellName = Lens.lens (\GetCellReadinessSummary' {cellName} -> cellName) (\s@GetCellReadinessSummary' {} a -> s {cellName = a} :: GetCellReadinessSummary)

instance Core.AWSPager GetCellReadinessSummary where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getCellReadinessSummaryResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getCellReadinessSummaryResponse_readinessChecks
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getCellReadinessSummary_nextToken
          Lens..~ rs
          Lens.^? getCellReadinessSummaryResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetCellReadinessSummary where
  type
    AWSResponse GetCellReadinessSummary =
      GetCellReadinessSummaryResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCellReadinessSummaryResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "readiness")
            Prelude.<*> ( x Data..?> "readinessChecks"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCellReadinessSummary where
  hashWithSalt _salt GetCellReadinessSummary' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` cellName

instance Prelude.NFData GetCellReadinessSummary where
  rnf GetCellReadinessSummary' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf cellName

instance Data.ToHeaders GetCellReadinessSummary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCellReadinessSummary where
  toPath GetCellReadinessSummary' {..} =
    Prelude.mconcat
      ["/cellreadiness/", Data.toBS cellName]

instance Data.ToQuery GetCellReadinessSummary where
  toQuery GetCellReadinessSummary' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetCellReadinessSummaryResponse' smart constructor.
data GetCellReadinessSummaryResponse = GetCellReadinessSummaryResponse'
  { -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The readiness at a cell level.
    readiness :: Prelude.Maybe Readiness,
    -- | Summaries for the readiness checks that make up the cell.
    readinessChecks :: Prelude.Maybe [ReadinessCheckSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCellReadinessSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getCellReadinessSummaryResponse_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'readiness', 'getCellReadinessSummaryResponse_readiness' - The readiness at a cell level.
--
-- 'readinessChecks', 'getCellReadinessSummaryResponse_readinessChecks' - Summaries for the readiness checks that make up the cell.
--
-- 'httpStatus', 'getCellReadinessSummaryResponse_httpStatus' - The response's http status code.
newGetCellReadinessSummaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCellReadinessSummaryResponse
newGetCellReadinessSummaryResponse pHttpStatus_ =
  GetCellReadinessSummaryResponse'
    { nextToken =
        Prelude.Nothing,
      readiness = Prelude.Nothing,
      readinessChecks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that identifies which batch of results you want to see.
getCellReadinessSummaryResponse_nextToken :: Lens.Lens' GetCellReadinessSummaryResponse (Prelude.Maybe Prelude.Text)
getCellReadinessSummaryResponse_nextToken = Lens.lens (\GetCellReadinessSummaryResponse' {nextToken} -> nextToken) (\s@GetCellReadinessSummaryResponse' {} a -> s {nextToken = a} :: GetCellReadinessSummaryResponse)

-- | The readiness at a cell level.
getCellReadinessSummaryResponse_readiness :: Lens.Lens' GetCellReadinessSummaryResponse (Prelude.Maybe Readiness)
getCellReadinessSummaryResponse_readiness = Lens.lens (\GetCellReadinessSummaryResponse' {readiness} -> readiness) (\s@GetCellReadinessSummaryResponse' {} a -> s {readiness = a} :: GetCellReadinessSummaryResponse)

-- | Summaries for the readiness checks that make up the cell.
getCellReadinessSummaryResponse_readinessChecks :: Lens.Lens' GetCellReadinessSummaryResponse (Prelude.Maybe [ReadinessCheckSummary])
getCellReadinessSummaryResponse_readinessChecks = Lens.lens (\GetCellReadinessSummaryResponse' {readinessChecks} -> readinessChecks) (\s@GetCellReadinessSummaryResponse' {} a -> s {readinessChecks = a} :: GetCellReadinessSummaryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getCellReadinessSummaryResponse_httpStatus :: Lens.Lens' GetCellReadinessSummaryResponse Prelude.Int
getCellReadinessSummaryResponse_httpStatus = Lens.lens (\GetCellReadinessSummaryResponse' {httpStatus} -> httpStatus) (\s@GetCellReadinessSummaryResponse' {} a -> s {httpStatus = a} :: GetCellReadinessSummaryResponse)

instance
  Prelude.NFData
    GetCellReadinessSummaryResponse
  where
  rnf GetCellReadinessSummaryResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf readiness
      `Prelude.seq` Prelude.rnf readinessChecks
      `Prelude.seq` Prelude.rnf httpStatus
