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
-- Module      : Amazonka.AuditManager.GetInsights
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the latest analytics data for all your current active assessments.
module Amazonka.AuditManager.GetInsights
  ( -- * Creating a Request
    GetInsights (..),
    newGetInsights,

    -- * Destructuring the Response
    GetInsightsResponse (..),
    newGetInsightsResponse,

    -- * Response Lenses
    getInsightsResponse_insights,
    getInsightsResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetInsights' smart constructor.
data GetInsights = GetInsights'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInsights' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetInsights ::
  GetInsights
newGetInsights = GetInsights'

instance Core.AWSRequest GetInsights where
  type AWSResponse GetInsights = GetInsightsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInsightsResponse'
            Prelude.<$> (x Data..?> "insights")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInsights where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetInsights where
  rnf _ = ()

instance Data.ToHeaders GetInsights where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetInsights where
  toPath = Prelude.const "/insights"

instance Data.ToQuery GetInsights where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInsightsResponse' smart constructor.
data GetInsightsResponse = GetInsightsResponse'
  { -- | The analytics data that the @GetInsights@ API returned.
    insights :: Prelude.Maybe Insights,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInsightsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insights', 'getInsightsResponse_insights' - The analytics data that the @GetInsights@ API returned.
--
-- 'httpStatus', 'getInsightsResponse_httpStatus' - The response's http status code.
newGetInsightsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInsightsResponse
newGetInsightsResponse pHttpStatus_ =
  GetInsightsResponse'
    { insights = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The analytics data that the @GetInsights@ API returned.
getInsightsResponse_insights :: Lens.Lens' GetInsightsResponse (Prelude.Maybe Insights)
getInsightsResponse_insights = Lens.lens (\GetInsightsResponse' {insights} -> insights) (\s@GetInsightsResponse' {} a -> s {insights = a} :: GetInsightsResponse)

-- | The response's http status code.
getInsightsResponse_httpStatus :: Lens.Lens' GetInsightsResponse Prelude.Int
getInsightsResponse_httpStatus = Lens.lens (\GetInsightsResponse' {httpStatus} -> httpStatus) (\s@GetInsightsResponse' {} a -> s {httpStatus = a} :: GetInsightsResponse)

instance Prelude.NFData GetInsightsResponse where
  rnf GetInsightsResponse' {..} =
    Prelude.rnf insights
      `Prelude.seq` Prelude.rnf httpStatus
