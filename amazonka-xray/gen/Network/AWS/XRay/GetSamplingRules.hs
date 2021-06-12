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
-- Module      : Network.AWS.XRay.GetSamplingRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all sampling rules.
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetSamplingRules
  ( -- * Creating a Request
    GetSamplingRules (..),
    newGetSamplingRules,

    -- * Request Lenses
    getSamplingRules_nextToken,

    -- * Destructuring the Response
    GetSamplingRulesResponse (..),
    newGetSamplingRulesResponse,

    -- * Response Lenses
    getSamplingRulesResponse_nextToken,
    getSamplingRulesResponse_samplingRuleRecords,
    getSamplingRulesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newGetSamplingRules' smart constructor.
data GetSamplingRules = GetSamplingRules'
  { -- | Pagination token.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSamplingRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSamplingRules_nextToken' - Pagination token.
newGetSamplingRules ::
  GetSamplingRules
newGetSamplingRules =
  GetSamplingRules' {nextToken = Core.Nothing}

-- | Pagination token.
getSamplingRules_nextToken :: Lens.Lens' GetSamplingRules (Core.Maybe Core.Text)
getSamplingRules_nextToken = Lens.lens (\GetSamplingRules' {nextToken} -> nextToken) (\s@GetSamplingRules' {} a -> s {nextToken = a} :: GetSamplingRules)

instance Core.AWSPager GetSamplingRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getSamplingRulesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getSamplingRulesResponse_samplingRuleRecords
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getSamplingRules_nextToken
          Lens..~ rs
          Lens.^? getSamplingRulesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetSamplingRules where
  type
    AWSResponse GetSamplingRules =
      GetSamplingRulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSamplingRulesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "SamplingRuleRecords"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSamplingRules

instance Core.NFData GetSamplingRules

instance Core.ToHeaders GetSamplingRules where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON GetSamplingRules where
  toJSON GetSamplingRules' {..} =
    Core.object
      ( Core.catMaybes
          [("NextToken" Core..=) Core.<$> nextToken]
      )

instance Core.ToPath GetSamplingRules where
  toPath = Core.const "/GetSamplingRules"

instance Core.ToQuery GetSamplingRules where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSamplingRulesResponse' smart constructor.
data GetSamplingRulesResponse = GetSamplingRulesResponse'
  { -- | Pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | Rule definitions and metadata.
    samplingRuleRecords :: Core.Maybe [SamplingRuleRecord],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSamplingRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSamplingRulesResponse_nextToken' - Pagination token.
--
-- 'samplingRuleRecords', 'getSamplingRulesResponse_samplingRuleRecords' - Rule definitions and metadata.
--
-- 'httpStatus', 'getSamplingRulesResponse_httpStatus' - The response's http status code.
newGetSamplingRulesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSamplingRulesResponse
newGetSamplingRulesResponse pHttpStatus_ =
  GetSamplingRulesResponse'
    { nextToken = Core.Nothing,
      samplingRuleRecords = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token.
getSamplingRulesResponse_nextToken :: Lens.Lens' GetSamplingRulesResponse (Core.Maybe Core.Text)
getSamplingRulesResponse_nextToken = Lens.lens (\GetSamplingRulesResponse' {nextToken} -> nextToken) (\s@GetSamplingRulesResponse' {} a -> s {nextToken = a} :: GetSamplingRulesResponse)

-- | Rule definitions and metadata.
getSamplingRulesResponse_samplingRuleRecords :: Lens.Lens' GetSamplingRulesResponse (Core.Maybe [SamplingRuleRecord])
getSamplingRulesResponse_samplingRuleRecords = Lens.lens (\GetSamplingRulesResponse' {samplingRuleRecords} -> samplingRuleRecords) (\s@GetSamplingRulesResponse' {} a -> s {samplingRuleRecords = a} :: GetSamplingRulesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getSamplingRulesResponse_httpStatus :: Lens.Lens' GetSamplingRulesResponse Core.Int
getSamplingRulesResponse_httpStatus = Lens.lens (\GetSamplingRulesResponse' {httpStatus} -> httpStatus) (\s@GetSamplingRulesResponse' {} a -> s {httpStatus = a} :: GetSamplingRulesResponse)

instance Core.NFData GetSamplingRulesResponse
