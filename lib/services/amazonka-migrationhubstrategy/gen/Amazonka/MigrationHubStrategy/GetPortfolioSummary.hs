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
-- Module      : Amazonka.MigrationHubStrategy.GetPortfolioSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves overall summary including the number of servers to rehost and
-- the overall number of anti-patterns.
module Amazonka.MigrationHubStrategy.GetPortfolioSummary
  ( -- * Creating a Request
    GetPortfolioSummary (..),
    newGetPortfolioSummary,

    -- * Destructuring the Response
    GetPortfolioSummaryResponse (..),
    newGetPortfolioSummaryResponse,

    -- * Response Lenses
    getPortfolioSummaryResponse_assessmentSummary,
    getPortfolioSummaryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubStrategy.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPortfolioSummary' smart constructor.
data GetPortfolioSummary = GetPortfolioSummary'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPortfolioSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetPortfolioSummary ::
  GetPortfolioSummary
newGetPortfolioSummary = GetPortfolioSummary'

instance Core.AWSRequest GetPortfolioSummary where
  type
    AWSResponse GetPortfolioSummary =
      GetPortfolioSummaryResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPortfolioSummaryResponse'
            Prelude.<$> (x Core..?> "assessmentSummary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPortfolioSummary where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetPortfolioSummary where
  rnf _ = ()

instance Core.ToHeaders GetPortfolioSummary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetPortfolioSummary where
  toPath = Prelude.const "/get-portfolio-summary"

instance Core.ToQuery GetPortfolioSummary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPortfolioSummaryResponse' smart constructor.
data GetPortfolioSummaryResponse = GetPortfolioSummaryResponse'
  { -- | An assessment summary for the portfolio including the number of servers
    -- to rehost and the overall number of anti-patterns.
    assessmentSummary :: Prelude.Maybe AssessmentSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPortfolioSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentSummary', 'getPortfolioSummaryResponse_assessmentSummary' - An assessment summary for the portfolio including the number of servers
-- to rehost and the overall number of anti-patterns.
--
-- 'httpStatus', 'getPortfolioSummaryResponse_httpStatus' - The response's http status code.
newGetPortfolioSummaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPortfolioSummaryResponse
newGetPortfolioSummaryResponse pHttpStatus_ =
  GetPortfolioSummaryResponse'
    { assessmentSummary =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An assessment summary for the portfolio including the number of servers
-- to rehost and the overall number of anti-patterns.
getPortfolioSummaryResponse_assessmentSummary :: Lens.Lens' GetPortfolioSummaryResponse (Prelude.Maybe AssessmentSummary)
getPortfolioSummaryResponse_assessmentSummary = Lens.lens (\GetPortfolioSummaryResponse' {assessmentSummary} -> assessmentSummary) (\s@GetPortfolioSummaryResponse' {} a -> s {assessmentSummary = a} :: GetPortfolioSummaryResponse)

-- | The response's http status code.
getPortfolioSummaryResponse_httpStatus :: Lens.Lens' GetPortfolioSummaryResponse Prelude.Int
getPortfolioSummaryResponse_httpStatus = Lens.lens (\GetPortfolioSummaryResponse' {httpStatus} -> httpStatus) (\s@GetPortfolioSummaryResponse' {} a -> s {httpStatus = a} :: GetPortfolioSummaryResponse)

instance Prelude.NFData GetPortfolioSummaryResponse where
  rnf GetPortfolioSummaryResponse' {..} =
    Prelude.rnf assessmentSummary
      `Prelude.seq` Prelude.rnf httpStatus
