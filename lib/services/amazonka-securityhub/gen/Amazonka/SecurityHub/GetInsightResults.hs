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
-- Module      : Amazonka.SecurityHub.GetInsightResults
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the results of the Security Hub insight specified by the insight
-- ARN.
module Amazonka.SecurityHub.GetInsightResults
  ( -- * Creating a Request
    GetInsightResults (..),
    newGetInsightResults,

    -- * Request Lenses
    getInsightResults_insightArn,

    -- * Destructuring the Response
    GetInsightResultsResponse (..),
    newGetInsightResultsResponse,

    -- * Response Lenses
    getInsightResultsResponse_httpStatus,
    getInsightResultsResponse_insightResults,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newGetInsightResults' smart constructor.
data GetInsightResults = GetInsightResults'
  { -- | The ARN of the insight for which to return results.
    insightArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInsightResults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insightArn', 'getInsightResults_insightArn' - The ARN of the insight for which to return results.
newGetInsightResults ::
  -- | 'insightArn'
  Prelude.Text ->
  GetInsightResults
newGetInsightResults pInsightArn_ =
  GetInsightResults' {insightArn = pInsightArn_}

-- | The ARN of the insight for which to return results.
getInsightResults_insightArn :: Lens.Lens' GetInsightResults Prelude.Text
getInsightResults_insightArn = Lens.lens (\GetInsightResults' {insightArn} -> insightArn) (\s@GetInsightResults' {} a -> s {insightArn = a} :: GetInsightResults)

instance Core.AWSRequest GetInsightResults where
  type
    AWSResponse GetInsightResults =
      GetInsightResultsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInsightResultsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "InsightResults")
      )

instance Prelude.Hashable GetInsightResults where
  hashWithSalt _salt GetInsightResults' {..} =
    _salt `Prelude.hashWithSalt` insightArn

instance Prelude.NFData GetInsightResults where
  rnf GetInsightResults' {..} = Prelude.rnf insightArn

instance Data.ToHeaders GetInsightResults where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetInsightResults where
  toPath GetInsightResults' {..} =
    Prelude.mconcat
      ["/insights/results/", Data.toBS insightArn]

instance Data.ToQuery GetInsightResults where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInsightResultsResponse' smart constructor.
data GetInsightResultsResponse = GetInsightResultsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The insight results returned by the operation.
    insightResults :: InsightResults
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInsightResultsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getInsightResultsResponse_httpStatus' - The response's http status code.
--
-- 'insightResults', 'getInsightResultsResponse_insightResults' - The insight results returned by the operation.
newGetInsightResultsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'insightResults'
  InsightResults ->
  GetInsightResultsResponse
newGetInsightResultsResponse
  pHttpStatus_
  pInsightResults_ =
    GetInsightResultsResponse'
      { httpStatus =
          pHttpStatus_,
        insightResults = pInsightResults_
      }

-- | The response's http status code.
getInsightResultsResponse_httpStatus :: Lens.Lens' GetInsightResultsResponse Prelude.Int
getInsightResultsResponse_httpStatus = Lens.lens (\GetInsightResultsResponse' {httpStatus} -> httpStatus) (\s@GetInsightResultsResponse' {} a -> s {httpStatus = a} :: GetInsightResultsResponse)

-- | The insight results returned by the operation.
getInsightResultsResponse_insightResults :: Lens.Lens' GetInsightResultsResponse InsightResults
getInsightResultsResponse_insightResults = Lens.lens (\GetInsightResultsResponse' {insightResults} -> insightResults) (\s@GetInsightResultsResponse' {} a -> s {insightResults = a} :: GetInsightResultsResponse)

instance Prelude.NFData GetInsightResultsResponse where
  rnf GetInsightResultsResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf insightResults
