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
-- Module      : Amazonka.XRay.GetSamplingStatisticSummaries
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about recent sampling results for all sampling
-- rules.
--
-- This operation returns paginated results.
module Amazonka.XRay.GetSamplingStatisticSummaries
  ( -- * Creating a Request
    GetSamplingStatisticSummaries (..),
    newGetSamplingStatisticSummaries,

    -- * Request Lenses
    getSamplingStatisticSummaries_nextToken,

    -- * Destructuring the Response
    GetSamplingStatisticSummariesResponse (..),
    newGetSamplingStatisticSummariesResponse,

    -- * Response Lenses
    getSamplingStatisticSummariesResponse_samplingStatisticSummaries,
    getSamplingStatisticSummariesResponse_nextToken,
    getSamplingStatisticSummariesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newGetSamplingStatisticSummaries' smart constructor.
data GetSamplingStatisticSummaries = GetSamplingStatisticSummaries'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSamplingStatisticSummaries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSamplingStatisticSummaries_nextToken' - Pagination token.
newGetSamplingStatisticSummaries ::
  GetSamplingStatisticSummaries
newGetSamplingStatisticSummaries =
  GetSamplingStatisticSummaries'
    { nextToken =
        Prelude.Nothing
    }

-- | Pagination token.
getSamplingStatisticSummaries_nextToken :: Lens.Lens' GetSamplingStatisticSummaries (Prelude.Maybe Prelude.Text)
getSamplingStatisticSummaries_nextToken = Lens.lens (\GetSamplingStatisticSummaries' {nextToken} -> nextToken) (\s@GetSamplingStatisticSummaries' {} a -> s {nextToken = a} :: GetSamplingStatisticSummaries)

instance Core.AWSPager GetSamplingStatisticSummaries where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getSamplingStatisticSummariesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getSamplingStatisticSummariesResponse_samplingStatisticSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getSamplingStatisticSummaries_nextToken
          Lens..~ rs
          Lens.^? getSamplingStatisticSummariesResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetSamplingStatisticSummaries
  where
  type
    AWSResponse GetSamplingStatisticSummaries =
      GetSamplingStatisticSummariesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSamplingStatisticSummariesResponse'
            Prelude.<$> ( x Data..?> "SamplingStatisticSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetSamplingStatisticSummaries
  where
  hashWithSalt _salt GetSamplingStatisticSummaries' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetSamplingStatisticSummaries where
  rnf GetSamplingStatisticSummaries' {..} =
    Prelude.rnf nextToken

instance Data.ToHeaders GetSamplingStatisticSummaries where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetSamplingStatisticSummaries where
  toJSON GetSamplingStatisticSummaries' {..} =
    Data.object
      ( Prelude.catMaybes
          [("NextToken" Data..=) Prelude.<$> nextToken]
      )

instance Data.ToPath GetSamplingStatisticSummaries where
  toPath = Prelude.const "/SamplingStatisticSummaries"

instance Data.ToQuery GetSamplingStatisticSummaries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSamplingStatisticSummariesResponse' smart constructor.
data GetSamplingStatisticSummariesResponse = GetSamplingStatisticSummariesResponse'
  { -- | Information about the number of requests instrumented for each sampling
    -- rule.
    samplingStatisticSummaries :: Prelude.Maybe [SamplingStatisticSummary],
    -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSamplingStatisticSummariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'samplingStatisticSummaries', 'getSamplingStatisticSummariesResponse_samplingStatisticSummaries' - Information about the number of requests instrumented for each sampling
-- rule.
--
-- 'nextToken', 'getSamplingStatisticSummariesResponse_nextToken' - Pagination token.
--
-- 'httpStatus', 'getSamplingStatisticSummariesResponse_httpStatus' - The response's http status code.
newGetSamplingStatisticSummariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSamplingStatisticSummariesResponse
newGetSamplingStatisticSummariesResponse pHttpStatus_ =
  GetSamplingStatisticSummariesResponse'
    { samplingStatisticSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the number of requests instrumented for each sampling
-- rule.
getSamplingStatisticSummariesResponse_samplingStatisticSummaries :: Lens.Lens' GetSamplingStatisticSummariesResponse (Prelude.Maybe [SamplingStatisticSummary])
getSamplingStatisticSummariesResponse_samplingStatisticSummaries = Lens.lens (\GetSamplingStatisticSummariesResponse' {samplingStatisticSummaries} -> samplingStatisticSummaries) (\s@GetSamplingStatisticSummariesResponse' {} a -> s {samplingStatisticSummaries = a} :: GetSamplingStatisticSummariesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Pagination token.
getSamplingStatisticSummariesResponse_nextToken :: Lens.Lens' GetSamplingStatisticSummariesResponse (Prelude.Maybe Prelude.Text)
getSamplingStatisticSummariesResponse_nextToken = Lens.lens (\GetSamplingStatisticSummariesResponse' {nextToken} -> nextToken) (\s@GetSamplingStatisticSummariesResponse' {} a -> s {nextToken = a} :: GetSamplingStatisticSummariesResponse)

-- | The response's http status code.
getSamplingStatisticSummariesResponse_httpStatus :: Lens.Lens' GetSamplingStatisticSummariesResponse Prelude.Int
getSamplingStatisticSummariesResponse_httpStatus = Lens.lens (\GetSamplingStatisticSummariesResponse' {httpStatus} -> httpStatus) (\s@GetSamplingStatisticSummariesResponse' {} a -> s {httpStatus = a} :: GetSamplingStatisticSummariesResponse)

instance
  Prelude.NFData
    GetSamplingStatisticSummariesResponse
  where
  rnf GetSamplingStatisticSummariesResponse' {..} =
    Prelude.rnf samplingStatisticSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
