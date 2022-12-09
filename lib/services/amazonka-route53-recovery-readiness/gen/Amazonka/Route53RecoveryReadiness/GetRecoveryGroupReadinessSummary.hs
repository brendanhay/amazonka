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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays a summary of information about a recovery group\'s readiness
-- status. Includes the readiness checks for resources in the recovery
-- group and the readiness status of each one.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryReadiness.GetRecoveryGroupReadinessSummary
  ( -- * Creating a Request
    GetRecoveryGroupReadinessSummary (..),
    newGetRecoveryGroupReadinessSummary,

    -- * Request Lenses
    getRecoveryGroupReadinessSummary_maxResults,
    getRecoveryGroupReadinessSummary_nextToken,
    getRecoveryGroupReadinessSummary_recoveryGroupName,

    -- * Destructuring the Response
    GetRecoveryGroupReadinessSummaryResponse (..),
    newGetRecoveryGroupReadinessSummaryResponse,

    -- * Response Lenses
    getRecoveryGroupReadinessSummaryResponse_nextToken,
    getRecoveryGroupReadinessSummaryResponse_readiness,
    getRecoveryGroupReadinessSummaryResponse_readinessChecks,
    getRecoveryGroupReadinessSummaryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newGetRecoveryGroupReadinessSummary' smart constructor.
data GetRecoveryGroupReadinessSummary = GetRecoveryGroupReadinessSummary'
  { -- | The number of objects that you want to return with this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of a recovery group.
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
-- 'maxResults', 'getRecoveryGroupReadinessSummary_maxResults' - The number of objects that you want to return with this call.
--
-- 'nextToken', 'getRecoveryGroupReadinessSummary_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'recoveryGroupName', 'getRecoveryGroupReadinessSummary_recoveryGroupName' - The name of a recovery group.
newGetRecoveryGroupReadinessSummary ::
  -- | 'recoveryGroupName'
  Prelude.Text ->
  GetRecoveryGroupReadinessSummary
newGetRecoveryGroupReadinessSummary
  pRecoveryGroupName_ =
    GetRecoveryGroupReadinessSummary'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        recoveryGroupName = pRecoveryGroupName_
      }

-- | The number of objects that you want to return with this call.
getRecoveryGroupReadinessSummary_maxResults :: Lens.Lens' GetRecoveryGroupReadinessSummary (Prelude.Maybe Prelude.Natural)
getRecoveryGroupReadinessSummary_maxResults = Lens.lens (\GetRecoveryGroupReadinessSummary' {maxResults} -> maxResults) (\s@GetRecoveryGroupReadinessSummary' {} a -> s {maxResults = a} :: GetRecoveryGroupReadinessSummary)

-- | The token that identifies which batch of results you want to see.
getRecoveryGroupReadinessSummary_nextToken :: Lens.Lens' GetRecoveryGroupReadinessSummary (Prelude.Maybe Prelude.Text)
getRecoveryGroupReadinessSummary_nextToken = Lens.lens (\GetRecoveryGroupReadinessSummary' {nextToken} -> nextToken) (\s@GetRecoveryGroupReadinessSummary' {} a -> s {nextToken = a} :: GetRecoveryGroupReadinessSummary)

-- | The name of a recovery group.
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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRecoveryGroupReadinessSummaryResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "readiness")
            Prelude.<*> ( x Data..?> "readinessChecks"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRecoveryGroupReadinessSummary
  where
  hashWithSalt
    _salt
    GetRecoveryGroupReadinessSummary' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` recoveryGroupName

instance
  Prelude.NFData
    GetRecoveryGroupReadinessSummary
  where
  rnf GetRecoveryGroupReadinessSummary' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recoveryGroupName

instance
  Data.ToHeaders
    GetRecoveryGroupReadinessSummary
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetRecoveryGroupReadinessSummary where
  toPath GetRecoveryGroupReadinessSummary' {..} =
    Prelude.mconcat
      [ "/recoverygroupreadiness/",
        Data.toBS recoveryGroupName
      ]

instance
  Data.ToQuery
    GetRecoveryGroupReadinessSummary
  where
  toQuery GetRecoveryGroupReadinessSummary' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetRecoveryGroupReadinessSummaryResponse' smart constructor.
data GetRecoveryGroupReadinessSummaryResponse = GetRecoveryGroupReadinessSummaryResponse'
  { -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The readiness status at a recovery group level.
    readiness :: Prelude.Maybe Readiness,
    -- | Summaries of the readiness checks for the recovery group.
    readinessChecks :: Prelude.Maybe [ReadinessCheckSummary],
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
-- 'nextToken', 'getRecoveryGroupReadinessSummaryResponse_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'readiness', 'getRecoveryGroupReadinessSummaryResponse_readiness' - The readiness status at a recovery group level.
--
-- 'readinessChecks', 'getRecoveryGroupReadinessSummaryResponse_readinessChecks' - Summaries of the readiness checks for the recovery group.
--
-- 'httpStatus', 'getRecoveryGroupReadinessSummaryResponse_httpStatus' - The response's http status code.
newGetRecoveryGroupReadinessSummaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRecoveryGroupReadinessSummaryResponse
newGetRecoveryGroupReadinessSummaryResponse
  pHttpStatus_ =
    GetRecoveryGroupReadinessSummaryResponse'
      { nextToken =
          Prelude.Nothing,
        readiness = Prelude.Nothing,
        readinessChecks = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token that identifies which batch of results you want to see.
getRecoveryGroupReadinessSummaryResponse_nextToken :: Lens.Lens' GetRecoveryGroupReadinessSummaryResponse (Prelude.Maybe Prelude.Text)
getRecoveryGroupReadinessSummaryResponse_nextToken = Lens.lens (\GetRecoveryGroupReadinessSummaryResponse' {nextToken} -> nextToken) (\s@GetRecoveryGroupReadinessSummaryResponse' {} a -> s {nextToken = a} :: GetRecoveryGroupReadinessSummaryResponse)

-- | The readiness status at a recovery group level.
getRecoveryGroupReadinessSummaryResponse_readiness :: Lens.Lens' GetRecoveryGroupReadinessSummaryResponse (Prelude.Maybe Readiness)
getRecoveryGroupReadinessSummaryResponse_readiness = Lens.lens (\GetRecoveryGroupReadinessSummaryResponse' {readiness} -> readiness) (\s@GetRecoveryGroupReadinessSummaryResponse' {} a -> s {readiness = a} :: GetRecoveryGroupReadinessSummaryResponse)

-- | Summaries of the readiness checks for the recovery group.
getRecoveryGroupReadinessSummaryResponse_readinessChecks :: Lens.Lens' GetRecoveryGroupReadinessSummaryResponse (Prelude.Maybe [ReadinessCheckSummary])
getRecoveryGroupReadinessSummaryResponse_readinessChecks = Lens.lens (\GetRecoveryGroupReadinessSummaryResponse' {readinessChecks} -> readinessChecks) (\s@GetRecoveryGroupReadinessSummaryResponse' {} a -> s {readinessChecks = a} :: GetRecoveryGroupReadinessSummaryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRecoveryGroupReadinessSummaryResponse_httpStatus :: Lens.Lens' GetRecoveryGroupReadinessSummaryResponse Prelude.Int
getRecoveryGroupReadinessSummaryResponse_httpStatus = Lens.lens (\GetRecoveryGroupReadinessSummaryResponse' {httpStatus} -> httpStatus) (\s@GetRecoveryGroupReadinessSummaryResponse' {} a -> s {httpStatus = a} :: GetRecoveryGroupReadinessSummaryResponse)

instance
  Prelude.NFData
    GetRecoveryGroupReadinessSummaryResponse
  where
  rnf GetRecoveryGroupReadinessSummaryResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf readiness
      `Prelude.seq` Prelude.rnf readinessChecks
      `Prelude.seq` Prelude.rnf httpStatus
