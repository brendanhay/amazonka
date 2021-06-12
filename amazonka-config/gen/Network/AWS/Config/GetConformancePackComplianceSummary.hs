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
-- Module      : Network.AWS.Config.GetConformancePackComplianceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns compliance details for the conformance pack based on the
-- cumulative compliance results of all the rules in that conformance pack.
module Network.AWS.Config.GetConformancePackComplianceSummary
  ( -- * Creating a Request
    GetConformancePackComplianceSummary (..),
    newGetConformancePackComplianceSummary,

    -- * Request Lenses
    getConformancePackComplianceSummary_nextToken,
    getConformancePackComplianceSummary_limit,
    getConformancePackComplianceSummary_conformancePackNames,

    -- * Destructuring the Response
    GetConformancePackComplianceSummaryResponse (..),
    newGetConformancePackComplianceSummaryResponse,

    -- * Response Lenses
    getConformancePackComplianceSummaryResponse_conformancePackComplianceSummaryList,
    getConformancePackComplianceSummaryResponse_nextToken,
    getConformancePackComplianceSummaryResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetConformancePackComplianceSummary' smart constructor.
data GetConformancePackComplianceSummary = GetConformancePackComplianceSummary'
  { -- | The nextToken string returned on a previous page that you use to get the
    -- next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of conformance packs returned on each page.
    limit :: Core.Maybe Core.Natural,
    -- | Names of conformance packs.
    conformancePackNames :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConformancePackComplianceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getConformancePackComplianceSummary_nextToken' - The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
--
-- 'limit', 'getConformancePackComplianceSummary_limit' - The maximum number of conformance packs returned on each page.
--
-- 'conformancePackNames', 'getConformancePackComplianceSummary_conformancePackNames' - Names of conformance packs.
newGetConformancePackComplianceSummary ::
  -- | 'conformancePackNames'
  Core.NonEmpty Core.Text ->
  GetConformancePackComplianceSummary
newGetConformancePackComplianceSummary
  pConformancePackNames_ =
    GetConformancePackComplianceSummary'
      { nextToken =
          Core.Nothing,
        limit = Core.Nothing,
        conformancePackNames =
          Lens._Coerce
            Lens.# pConformancePackNames_
      }

-- | The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
getConformancePackComplianceSummary_nextToken :: Lens.Lens' GetConformancePackComplianceSummary (Core.Maybe Core.Text)
getConformancePackComplianceSummary_nextToken = Lens.lens (\GetConformancePackComplianceSummary' {nextToken} -> nextToken) (\s@GetConformancePackComplianceSummary' {} a -> s {nextToken = a} :: GetConformancePackComplianceSummary)

-- | The maximum number of conformance packs returned on each page.
getConformancePackComplianceSummary_limit :: Lens.Lens' GetConformancePackComplianceSummary (Core.Maybe Core.Natural)
getConformancePackComplianceSummary_limit = Lens.lens (\GetConformancePackComplianceSummary' {limit} -> limit) (\s@GetConformancePackComplianceSummary' {} a -> s {limit = a} :: GetConformancePackComplianceSummary)

-- | Names of conformance packs.
getConformancePackComplianceSummary_conformancePackNames :: Lens.Lens' GetConformancePackComplianceSummary (Core.NonEmpty Core.Text)
getConformancePackComplianceSummary_conformancePackNames = Lens.lens (\GetConformancePackComplianceSummary' {conformancePackNames} -> conformancePackNames) (\s@GetConformancePackComplianceSummary' {} a -> s {conformancePackNames = a} :: GetConformancePackComplianceSummary) Core.. Lens._Coerce

instance
  Core.AWSRequest
    GetConformancePackComplianceSummary
  where
  type
    AWSResponse GetConformancePackComplianceSummary =
      GetConformancePackComplianceSummaryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConformancePackComplianceSummaryResponse'
            Core.<$> (x Core..?> "ConformancePackComplianceSummaryList")
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetConformancePackComplianceSummary

instance
  Core.NFData
    GetConformancePackComplianceSummary

instance
  Core.ToHeaders
    GetConformancePackComplianceSummary
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.GetConformancePackComplianceSummary" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    GetConformancePackComplianceSummary
  where
  toJSON GetConformancePackComplianceSummary' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just
              ( "ConformancePackNames"
                  Core..= conformancePackNames
              )
          ]
      )

instance
  Core.ToPath
    GetConformancePackComplianceSummary
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetConformancePackComplianceSummary
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetConformancePackComplianceSummaryResponse' smart constructor.
data GetConformancePackComplianceSummaryResponse = GetConformancePackComplianceSummaryResponse'
  { -- | A list of @ConformancePackComplianceSummary@ objects.
    conformancePackComplianceSummaryList :: Core.Maybe (Core.NonEmpty ConformancePackComplianceSummary),
    -- | The nextToken string returned on a previous page that you use to get the
    -- next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConformancePackComplianceSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conformancePackComplianceSummaryList', 'getConformancePackComplianceSummaryResponse_conformancePackComplianceSummaryList' - A list of @ConformancePackComplianceSummary@ objects.
--
-- 'nextToken', 'getConformancePackComplianceSummaryResponse_nextToken' - The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
--
-- 'httpStatus', 'getConformancePackComplianceSummaryResponse_httpStatus' - The response's http status code.
newGetConformancePackComplianceSummaryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetConformancePackComplianceSummaryResponse
newGetConformancePackComplianceSummaryResponse
  pHttpStatus_ =
    GetConformancePackComplianceSummaryResponse'
      { conformancePackComplianceSummaryList =
          Core.Nothing,
        nextToken = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of @ConformancePackComplianceSummary@ objects.
getConformancePackComplianceSummaryResponse_conformancePackComplianceSummaryList :: Lens.Lens' GetConformancePackComplianceSummaryResponse (Core.Maybe (Core.NonEmpty ConformancePackComplianceSummary))
getConformancePackComplianceSummaryResponse_conformancePackComplianceSummaryList = Lens.lens (\GetConformancePackComplianceSummaryResponse' {conformancePackComplianceSummaryList} -> conformancePackComplianceSummaryList) (\s@GetConformancePackComplianceSummaryResponse' {} a -> s {conformancePackComplianceSummaryList = a} :: GetConformancePackComplianceSummaryResponse) Core.. Lens.mapping Lens._Coerce

-- | The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
getConformancePackComplianceSummaryResponse_nextToken :: Lens.Lens' GetConformancePackComplianceSummaryResponse (Core.Maybe Core.Text)
getConformancePackComplianceSummaryResponse_nextToken = Lens.lens (\GetConformancePackComplianceSummaryResponse' {nextToken} -> nextToken) (\s@GetConformancePackComplianceSummaryResponse' {} a -> s {nextToken = a} :: GetConformancePackComplianceSummaryResponse)

-- | The response's http status code.
getConformancePackComplianceSummaryResponse_httpStatus :: Lens.Lens' GetConformancePackComplianceSummaryResponse Core.Int
getConformancePackComplianceSummaryResponse_httpStatus = Lens.lens (\GetConformancePackComplianceSummaryResponse' {httpStatus} -> httpStatus) (\s@GetConformancePackComplianceSummaryResponse' {} a -> s {httpStatus = a} :: GetConformancePackComplianceSummaryResponse)

instance
  Core.NFData
    GetConformancePackComplianceSummaryResponse
