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
-- Module      : Amazonka.Route53RecoveryReadiness.GetRecoveryGroupReadinessSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a Recovery Group.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryReadiness.GetRecoveryGroupReadinessSummary
  ( -- * Creating a Request
    GetRecoveryGroupReadinessSummary (..),
    newGetRecoveryGroupReadinessSummary,

    -- * Request Lenses
    getRecoveryGroupReadinessSummary_nextToken,
    getRecoveryGroupReadinessSummary_maxResults,
    getRecoveryGroupReadinessSummary_recoveryGroupName,

    -- * Destructuring the Response
    GetRecoveryGroupReadinessSummaryResponse (..),
    newGetRecoveryGroupReadinessSummaryResponse,

    -- * Response Lenses
    getRecoveryGroupReadinessSummaryResponse_readinessChecks,
    getRecoveryGroupReadinessSummaryResponse_readiness,
    getRecoveryGroupReadinessSummaryResponse_nextToken,
    getRecoveryGroupReadinessSummaryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newGetRecoveryGroupReadinessSummary' smart constructor.
data GetRecoveryGroupReadinessSummary = GetRecoveryGroupReadinessSummary'
  { -- | A token used to resume pagination from the end of a previous request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Upper bound on number of records to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the RecoveryGroup
    recoveryGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecoveryGroupReadinessSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getRecoveryGroupReadinessSummary_nextToken' - A token used to resume pagination from the end of a previous request.
--
-- 'maxResults', 'getRecoveryGroupReadinessSummary_maxResults' - Upper bound on number of records to return.
--
-- 'recoveryGroupName', 'getRecoveryGroupReadinessSummary_recoveryGroupName' - The name of the RecoveryGroup
newGetRecoveryGroupReadinessSummary ::
  -- | 'recoveryGroupName'
  Prelude.Text ->
  GetRecoveryGroupReadinessSummary
newGetRecoveryGroupReadinessSummary
  pRecoveryGroupName_ =
    GetRecoveryGroupReadinessSummary'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        recoveryGroupName = pRecoveryGroupName_
      }

-- | A token used to resume pagination from the end of a previous request.
getRecoveryGroupReadinessSummary_nextToken :: Lens.Lens' GetRecoveryGroupReadinessSummary (Prelude.Maybe Prelude.Text)
getRecoveryGroupReadinessSummary_nextToken = Lens.lens (\GetRecoveryGroupReadinessSummary' {nextToken} -> nextToken) (\s@GetRecoveryGroupReadinessSummary' {} a -> s {nextToken = a} :: GetRecoveryGroupReadinessSummary)

-- | Upper bound on number of records to return.
getRecoveryGroupReadinessSummary_maxResults :: Lens.Lens' GetRecoveryGroupReadinessSummary (Prelude.Maybe Prelude.Natural)
getRecoveryGroupReadinessSummary_maxResults = Lens.lens (\GetRecoveryGroupReadinessSummary' {maxResults} -> maxResults) (\s@GetRecoveryGroupReadinessSummary' {} a -> s {maxResults = a} :: GetRecoveryGroupReadinessSummary)

-- | The name of the RecoveryGroup
getRecoveryGroupReadinessSummary_recoveryGroupName :: Lens.Lens' GetRecoveryGroupReadinessSummary Prelude.Text
getRecoveryGroupReadinessSummary_recoveryGroupName = Lens.lens (\GetRecoveryGroupReadinessSummary' {recoveryGroupName} -> recoveryGroupName) (\s@GetRecoveryGroupReadinessSummary' {} a -> s {recoveryGroupName = a} :: GetRecoveryGroupReadinessSummary)

instance
  Core.AWSPager
    GetRecoveryGroupReadinessSummary
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getRecoveryGroupReadinessSummaryResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getRecoveryGroupReadinessSummaryResponse_readinessChecks
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getRecoveryGroupReadinessSummary_nextToken
          Lens..~ rs
          Lens.^? getRecoveryGroupReadinessSummaryResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetRecoveryGroupReadinessSummary
  where
  type
    AWSResponse GetRecoveryGroupReadinessSummary =
      GetRecoveryGroupReadinessSummaryResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRecoveryGroupReadinessSummaryResponse'
            Prelude.<$> ( x Core..?> "readinessChecks"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "readiness")
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRecoveryGroupReadinessSummary
  where
  hashWithSalt
    _salt
    GetRecoveryGroupReadinessSummary' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` recoveryGroupName

instance
  Prelude.NFData
    GetRecoveryGroupReadinessSummary
  where
  rnf GetRecoveryGroupReadinessSummary' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf recoveryGroupName

instance
  Core.ToHeaders
    GetRecoveryGroupReadinessSummary
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetRecoveryGroupReadinessSummary where
  toPath GetRecoveryGroupReadinessSummary' {..} =
    Prelude.mconcat
      [ "/recoverygroupreadiness/",
        Core.toBS recoveryGroupName
      ]

instance
  Core.ToQuery
    GetRecoveryGroupReadinessSummary
  where
  toQuery GetRecoveryGroupReadinessSummary' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetRecoveryGroupReadinessSummaryResponse' smart constructor.
data GetRecoveryGroupReadinessSummaryResponse = GetRecoveryGroupReadinessSummaryResponse'
  { -- | Summaries for the ReadinessChecks making up the RecoveryGroup
    readinessChecks :: Prelude.Maybe [ReadinessCheckSummary],
    -- | The readiness at RecoveryGroup level.
    readiness :: Prelude.Maybe Readiness,
    -- | A token that can be used to resume pagination from the end of the
    -- collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecoveryGroupReadinessSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readinessChecks', 'getRecoveryGroupReadinessSummaryResponse_readinessChecks' - Summaries for the ReadinessChecks making up the RecoveryGroup
--
-- 'readiness', 'getRecoveryGroupReadinessSummaryResponse_readiness' - The readiness at RecoveryGroup level.
--
-- 'nextToken', 'getRecoveryGroupReadinessSummaryResponse_nextToken' - A token that can be used to resume pagination from the end of the
-- collection.
--
-- 'httpStatus', 'getRecoveryGroupReadinessSummaryResponse_httpStatus' - The response's http status code.
newGetRecoveryGroupReadinessSummaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRecoveryGroupReadinessSummaryResponse
newGetRecoveryGroupReadinessSummaryResponse
  pHttpStatus_ =
    GetRecoveryGroupReadinessSummaryResponse'
      { readinessChecks =
          Prelude.Nothing,
        readiness = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Summaries for the ReadinessChecks making up the RecoveryGroup
getRecoveryGroupReadinessSummaryResponse_readinessChecks :: Lens.Lens' GetRecoveryGroupReadinessSummaryResponse (Prelude.Maybe [ReadinessCheckSummary])
getRecoveryGroupReadinessSummaryResponse_readinessChecks = Lens.lens (\GetRecoveryGroupReadinessSummaryResponse' {readinessChecks} -> readinessChecks) (\s@GetRecoveryGroupReadinessSummaryResponse' {} a -> s {readinessChecks = a} :: GetRecoveryGroupReadinessSummaryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The readiness at RecoveryGroup level.
getRecoveryGroupReadinessSummaryResponse_readiness :: Lens.Lens' GetRecoveryGroupReadinessSummaryResponse (Prelude.Maybe Readiness)
getRecoveryGroupReadinessSummaryResponse_readiness = Lens.lens (\GetRecoveryGroupReadinessSummaryResponse' {readiness} -> readiness) (\s@GetRecoveryGroupReadinessSummaryResponse' {} a -> s {readiness = a} :: GetRecoveryGroupReadinessSummaryResponse)

-- | A token that can be used to resume pagination from the end of the
-- collection.
getRecoveryGroupReadinessSummaryResponse_nextToken :: Lens.Lens' GetRecoveryGroupReadinessSummaryResponse (Prelude.Maybe Prelude.Text)
getRecoveryGroupReadinessSummaryResponse_nextToken = Lens.lens (\GetRecoveryGroupReadinessSummaryResponse' {nextToken} -> nextToken) (\s@GetRecoveryGroupReadinessSummaryResponse' {} a -> s {nextToken = a} :: GetRecoveryGroupReadinessSummaryResponse)

-- | The response's http status code.
getRecoveryGroupReadinessSummaryResponse_httpStatus :: Lens.Lens' GetRecoveryGroupReadinessSummaryResponse Prelude.Int
getRecoveryGroupReadinessSummaryResponse_httpStatus = Lens.lens (\GetRecoveryGroupReadinessSummaryResponse' {httpStatus} -> httpStatus) (\s@GetRecoveryGroupReadinessSummaryResponse' {} a -> s {httpStatus = a} :: GetRecoveryGroupReadinessSummaryResponse)

instance
  Prelude.NFData
    GetRecoveryGroupReadinessSummaryResponse
  where
  rnf GetRecoveryGroupReadinessSummaryResponse' {..} =
    Prelude.rnf readinessChecks
      `Prelude.seq` Prelude.rnf readiness
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
