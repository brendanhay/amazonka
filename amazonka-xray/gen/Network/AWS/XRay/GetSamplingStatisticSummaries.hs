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
-- Module      : Network.AWS.XRay.GetSamplingStatisticSummaries
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about recent sampling results for all sampling
-- rules.
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetSamplingStatisticSummaries
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newGetSamplingStatisticSummaries' smart constructor.
data GetSamplingStatisticSummaries = GetSamplingStatisticSummaries'
  { -- | Pagination token.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | Pagination token.
getSamplingStatisticSummaries_nextToken :: Lens.Lens' GetSamplingStatisticSummaries (Core.Maybe Core.Text)
getSamplingStatisticSummaries_nextToken = Lens.lens (\GetSamplingStatisticSummaries' {nextToken} -> nextToken) (\s@GetSamplingStatisticSummaries' {} a -> s {nextToken = a} :: GetSamplingStatisticSummaries)

instance Core.AWSPager GetSamplingStatisticSummaries where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getSamplingStatisticSummariesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getSamplingStatisticSummariesResponse_samplingStatisticSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getSamplingStatisticSummaries_nextToken
          Lens..~ rs
          Lens.^? getSamplingStatisticSummariesResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    GetSamplingStatisticSummaries
  where
  type
    AWSResponse GetSamplingStatisticSummaries =
      GetSamplingStatisticSummariesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSamplingStatisticSummariesResponse'
            Core.<$> ( x Core..?> "SamplingStatisticSummaries"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSamplingStatisticSummaries

instance Core.NFData GetSamplingStatisticSummaries

instance Core.ToHeaders GetSamplingStatisticSummaries where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON GetSamplingStatisticSummaries where
  toJSON GetSamplingStatisticSummaries' {..} =
    Core.object
      ( Core.catMaybes
          [("NextToken" Core..=) Core.<$> nextToken]
      )

instance Core.ToPath GetSamplingStatisticSummaries where
  toPath = Core.const "/SamplingStatisticSummaries"

instance Core.ToQuery GetSamplingStatisticSummaries where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSamplingStatisticSummariesResponse' smart constructor.
data GetSamplingStatisticSummariesResponse = GetSamplingStatisticSummariesResponse'
  { -- | Information about the number of requests instrumented for each sampling
    -- rule.
    samplingStatisticSummaries :: Core.Maybe [SamplingStatisticSummary],
    -- | Pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetSamplingStatisticSummariesResponse
newGetSamplingStatisticSummariesResponse pHttpStatus_ =
  GetSamplingStatisticSummariesResponse'
    { samplingStatisticSummaries =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the number of requests instrumented for each sampling
-- rule.
getSamplingStatisticSummariesResponse_samplingStatisticSummaries :: Lens.Lens' GetSamplingStatisticSummariesResponse (Core.Maybe [SamplingStatisticSummary])
getSamplingStatisticSummariesResponse_samplingStatisticSummaries = Lens.lens (\GetSamplingStatisticSummariesResponse' {samplingStatisticSummaries} -> samplingStatisticSummaries) (\s@GetSamplingStatisticSummariesResponse' {} a -> s {samplingStatisticSummaries = a} :: GetSamplingStatisticSummariesResponse) Core.. Lens.mapping Lens._Coerce

-- | Pagination token.
getSamplingStatisticSummariesResponse_nextToken :: Lens.Lens' GetSamplingStatisticSummariesResponse (Core.Maybe Core.Text)
getSamplingStatisticSummariesResponse_nextToken = Lens.lens (\GetSamplingStatisticSummariesResponse' {nextToken} -> nextToken) (\s@GetSamplingStatisticSummariesResponse' {} a -> s {nextToken = a} :: GetSamplingStatisticSummariesResponse)

-- | The response's http status code.
getSamplingStatisticSummariesResponse_httpStatus :: Lens.Lens' GetSamplingStatisticSummariesResponse Core.Int
getSamplingStatisticSummariesResponse_httpStatus = Lens.lens (\GetSamplingStatisticSummariesResponse' {httpStatus} -> httpStatus) (\s@GetSamplingStatisticSummariesResponse' {} a -> s {httpStatus = a} :: GetSamplingStatisticSummariesResponse)

instance
  Core.NFData
    GetSamplingStatisticSummariesResponse
