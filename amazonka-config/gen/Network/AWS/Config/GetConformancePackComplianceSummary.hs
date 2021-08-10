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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetConformancePackComplianceSummary' smart constructor.
data GetConformancePackComplianceSummary = GetConformancePackComplianceSummary'
  { -- | The nextToken string returned on a previous page that you use to get the
    -- next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of conformance packs returned on each page.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Names of conformance packs.
    conformancePackNames :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.NonEmpty Prelude.Text ->
  GetConformancePackComplianceSummary
newGetConformancePackComplianceSummary
  pConformancePackNames_ =
    GetConformancePackComplianceSummary'
      { nextToken =
          Prelude.Nothing,
        limit = Prelude.Nothing,
        conformancePackNames =
          Lens._Coerce
            Lens.# pConformancePackNames_
      }

-- | The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
getConformancePackComplianceSummary_nextToken :: Lens.Lens' GetConformancePackComplianceSummary (Prelude.Maybe Prelude.Text)
getConformancePackComplianceSummary_nextToken = Lens.lens (\GetConformancePackComplianceSummary' {nextToken} -> nextToken) (\s@GetConformancePackComplianceSummary' {} a -> s {nextToken = a} :: GetConformancePackComplianceSummary)

-- | The maximum number of conformance packs returned on each page.
getConformancePackComplianceSummary_limit :: Lens.Lens' GetConformancePackComplianceSummary (Prelude.Maybe Prelude.Natural)
getConformancePackComplianceSummary_limit = Lens.lens (\GetConformancePackComplianceSummary' {limit} -> limit) (\s@GetConformancePackComplianceSummary' {} a -> s {limit = a} :: GetConformancePackComplianceSummary)

-- | Names of conformance packs.
getConformancePackComplianceSummary_conformancePackNames :: Lens.Lens' GetConformancePackComplianceSummary (Prelude.NonEmpty Prelude.Text)
getConformancePackComplianceSummary_conformancePackNames = Lens.lens (\GetConformancePackComplianceSummary' {conformancePackNames} -> conformancePackNames) (\s@GetConformancePackComplianceSummary' {} a -> s {conformancePackNames = a} :: GetConformancePackComplianceSummary) Prelude.. Lens._Coerce

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
            Prelude.<$> (x Core..?> "ConformancePackComplianceSummaryList")
              Prelude.<*> (x Core..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetConformancePackComplianceSummary

instance
  Prelude.NFData
    GetConformancePackComplianceSummary

instance
  Core.ToHeaders
    GetConformancePackComplianceSummary
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.GetConformancePackComplianceSummary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    GetConformancePackComplianceSummary
  where
  toJSON GetConformancePackComplianceSummary' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just
              ( "ConformancePackNames"
                  Core..= conformancePackNames
              )
          ]
      )

instance
  Core.ToPath
    GetConformancePackComplianceSummary
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetConformancePackComplianceSummary
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConformancePackComplianceSummaryResponse' smart constructor.
data GetConformancePackComplianceSummaryResponse = GetConformancePackComplianceSummaryResponse'
  { -- | A list of @ConformancePackComplianceSummary@ objects.
    conformancePackComplianceSummaryList :: Prelude.Maybe (Prelude.NonEmpty ConformancePackComplianceSummary),
    -- | The nextToken string returned on a previous page that you use to get the
    -- next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetConformancePackComplianceSummaryResponse
newGetConformancePackComplianceSummaryResponse
  pHttpStatus_ =
    GetConformancePackComplianceSummaryResponse'
      { conformancePackComplianceSummaryList =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of @ConformancePackComplianceSummary@ objects.
getConformancePackComplianceSummaryResponse_conformancePackComplianceSummaryList :: Lens.Lens' GetConformancePackComplianceSummaryResponse (Prelude.Maybe (Prelude.NonEmpty ConformancePackComplianceSummary))
getConformancePackComplianceSummaryResponse_conformancePackComplianceSummaryList = Lens.lens (\GetConformancePackComplianceSummaryResponse' {conformancePackComplianceSummaryList} -> conformancePackComplianceSummaryList) (\s@GetConformancePackComplianceSummaryResponse' {} a -> s {conformancePackComplianceSummaryList = a} :: GetConformancePackComplianceSummaryResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
getConformancePackComplianceSummaryResponse_nextToken :: Lens.Lens' GetConformancePackComplianceSummaryResponse (Prelude.Maybe Prelude.Text)
getConformancePackComplianceSummaryResponse_nextToken = Lens.lens (\GetConformancePackComplianceSummaryResponse' {nextToken} -> nextToken) (\s@GetConformancePackComplianceSummaryResponse' {} a -> s {nextToken = a} :: GetConformancePackComplianceSummaryResponse)

-- | The response's http status code.
getConformancePackComplianceSummaryResponse_httpStatus :: Lens.Lens' GetConformancePackComplianceSummaryResponse Prelude.Int
getConformancePackComplianceSummaryResponse_httpStatus = Lens.lens (\GetConformancePackComplianceSummaryResponse' {httpStatus} -> httpStatus) (\s@GetConformancePackComplianceSummaryResponse' {} a -> s {httpStatus = a} :: GetConformancePackComplianceSummaryResponse)

instance
  Prelude.NFData
    GetConformancePackComplianceSummaryResponse
