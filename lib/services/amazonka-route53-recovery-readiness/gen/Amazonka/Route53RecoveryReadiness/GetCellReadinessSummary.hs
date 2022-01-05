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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about readiness of a Cell.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryReadiness.GetCellReadinessSummary
  ( -- * Creating a Request
    GetCellReadinessSummary (..),
    newGetCellReadinessSummary,

    -- * Request Lenses
    getCellReadinessSummary_nextToken,
    getCellReadinessSummary_maxResults,
    getCellReadinessSummary_cellName,

    -- * Destructuring the Response
    GetCellReadinessSummaryResponse (..),
    newGetCellReadinessSummaryResponse,

    -- * Response Lenses
    getCellReadinessSummaryResponse_readinessChecks,
    getCellReadinessSummaryResponse_readiness,
    getCellReadinessSummaryResponse_nextToken,
    getCellReadinessSummaryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newGetCellReadinessSummary' smart constructor.
data GetCellReadinessSummary = GetCellReadinessSummary'
  { -- | A token used to resume pagination from the end of a previous request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Upper bound on number of records to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the Cell
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
-- 'nextToken', 'getCellReadinessSummary_nextToken' - A token used to resume pagination from the end of a previous request.
--
-- 'maxResults', 'getCellReadinessSummary_maxResults' - Upper bound on number of records to return.
--
-- 'cellName', 'getCellReadinessSummary_cellName' - The name of the Cell
newGetCellReadinessSummary ::
  -- | 'cellName'
  Prelude.Text ->
  GetCellReadinessSummary
newGetCellReadinessSummary pCellName_ =
  GetCellReadinessSummary'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      cellName = pCellName_
    }

-- | A token used to resume pagination from the end of a previous request.
getCellReadinessSummary_nextToken :: Lens.Lens' GetCellReadinessSummary (Prelude.Maybe Prelude.Text)
getCellReadinessSummary_nextToken = Lens.lens (\GetCellReadinessSummary' {nextToken} -> nextToken) (\s@GetCellReadinessSummary' {} a -> s {nextToken = a} :: GetCellReadinessSummary)

-- | Upper bound on number of records to return.
getCellReadinessSummary_maxResults :: Lens.Lens' GetCellReadinessSummary (Prelude.Maybe Prelude.Natural)
getCellReadinessSummary_maxResults = Lens.lens (\GetCellReadinessSummary' {maxResults} -> maxResults) (\s@GetCellReadinessSummary' {} a -> s {maxResults = a} :: GetCellReadinessSummary)

-- | The name of the Cell
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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCellReadinessSummaryResponse'
            Prelude.<$> ( x Core..?> "readinessChecks"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "readiness")
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCellReadinessSummary where
  hashWithSalt _salt GetCellReadinessSummary' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` cellName

instance Prelude.NFData GetCellReadinessSummary where
  rnf GetCellReadinessSummary' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf cellName

instance Core.ToHeaders GetCellReadinessSummary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetCellReadinessSummary where
  toPath GetCellReadinessSummary' {..} =
    Prelude.mconcat
      ["/cellreadiness/", Core.toBS cellName]

instance Core.ToQuery GetCellReadinessSummary where
  toQuery GetCellReadinessSummary' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetCellReadinessSummaryResponse' smart constructor.
data GetCellReadinessSummaryResponse = GetCellReadinessSummaryResponse'
  { -- | Summaries for the ReadinessChecks making up the Cell
    readinessChecks :: Prelude.Maybe [ReadinessCheckSummary],
    -- | The readiness at Cell level.
    readiness :: Prelude.Maybe Readiness,
    -- | A token that can be used to resume pagination from the end of the
    -- collection.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'readinessChecks', 'getCellReadinessSummaryResponse_readinessChecks' - Summaries for the ReadinessChecks making up the Cell
--
-- 'readiness', 'getCellReadinessSummaryResponse_readiness' - The readiness at Cell level.
--
-- 'nextToken', 'getCellReadinessSummaryResponse_nextToken' - A token that can be used to resume pagination from the end of the
-- collection.
--
-- 'httpStatus', 'getCellReadinessSummaryResponse_httpStatus' - The response's http status code.
newGetCellReadinessSummaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCellReadinessSummaryResponse
newGetCellReadinessSummaryResponse pHttpStatus_ =
  GetCellReadinessSummaryResponse'
    { readinessChecks =
        Prelude.Nothing,
      readiness = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Summaries for the ReadinessChecks making up the Cell
getCellReadinessSummaryResponse_readinessChecks :: Lens.Lens' GetCellReadinessSummaryResponse (Prelude.Maybe [ReadinessCheckSummary])
getCellReadinessSummaryResponse_readinessChecks = Lens.lens (\GetCellReadinessSummaryResponse' {readinessChecks} -> readinessChecks) (\s@GetCellReadinessSummaryResponse' {} a -> s {readinessChecks = a} :: GetCellReadinessSummaryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The readiness at Cell level.
getCellReadinessSummaryResponse_readiness :: Lens.Lens' GetCellReadinessSummaryResponse (Prelude.Maybe Readiness)
getCellReadinessSummaryResponse_readiness = Lens.lens (\GetCellReadinessSummaryResponse' {readiness} -> readiness) (\s@GetCellReadinessSummaryResponse' {} a -> s {readiness = a} :: GetCellReadinessSummaryResponse)

-- | A token that can be used to resume pagination from the end of the
-- collection.
getCellReadinessSummaryResponse_nextToken :: Lens.Lens' GetCellReadinessSummaryResponse (Prelude.Maybe Prelude.Text)
getCellReadinessSummaryResponse_nextToken = Lens.lens (\GetCellReadinessSummaryResponse' {nextToken} -> nextToken) (\s@GetCellReadinessSummaryResponse' {} a -> s {nextToken = a} :: GetCellReadinessSummaryResponse)

-- | The response's http status code.
getCellReadinessSummaryResponse_httpStatus :: Lens.Lens' GetCellReadinessSummaryResponse Prelude.Int
getCellReadinessSummaryResponse_httpStatus = Lens.lens (\GetCellReadinessSummaryResponse' {httpStatus} -> httpStatus) (\s@GetCellReadinessSummaryResponse' {} a -> s {httpStatus = a} :: GetCellReadinessSummaryResponse)

instance
  Prelude.NFData
    GetCellReadinessSummaryResponse
  where
  rnf GetCellReadinessSummaryResponse' {..} =
    Prelude.rnf readinessChecks
      `Prelude.seq` Prelude.rnf readiness
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
