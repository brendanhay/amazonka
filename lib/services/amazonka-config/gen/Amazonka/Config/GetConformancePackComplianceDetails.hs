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
-- Module      : Amazonka.Config.GetConformancePackComplianceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns compliance details of a conformance pack for all Amazon Web
-- Services resources that are monitered by conformance pack.
module Amazonka.Config.GetConformancePackComplianceDetails
  ( -- * Creating a Request
    GetConformancePackComplianceDetails (..),
    newGetConformancePackComplianceDetails,

    -- * Request Lenses
    getConformancePackComplianceDetails_filters,
    getConformancePackComplianceDetails_limit,
    getConformancePackComplianceDetails_nextToken,
    getConformancePackComplianceDetails_conformancePackName,

    -- * Destructuring the Response
    GetConformancePackComplianceDetailsResponse (..),
    newGetConformancePackComplianceDetailsResponse,

    -- * Response Lenses
    getConformancePackComplianceDetailsResponse_conformancePackRuleEvaluationResults,
    getConformancePackComplianceDetailsResponse_nextToken,
    getConformancePackComplianceDetailsResponse_httpStatus,
    getConformancePackComplianceDetailsResponse_conformancePackName,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetConformancePackComplianceDetails' smart constructor.
data GetConformancePackComplianceDetails = GetConformancePackComplianceDetails'
  { -- | A @ConformancePackEvaluationFilters@ object.
    filters :: Prelude.Maybe ConformancePackEvaluationFilters,
    -- | The maximum number of evaluation results returned on each page. If you
    -- do no specify a number, Config uses the default. The default is 100.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Name of the conformance pack.
    conformancePackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConformancePackComplianceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'getConformancePackComplianceDetails_filters' - A @ConformancePackEvaluationFilters@ object.
--
-- 'limit', 'getConformancePackComplianceDetails_limit' - The maximum number of evaluation results returned on each page. If you
-- do no specify a number, Config uses the default. The default is 100.
--
-- 'nextToken', 'getConformancePackComplianceDetails_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'conformancePackName', 'getConformancePackComplianceDetails_conformancePackName' - Name of the conformance pack.
newGetConformancePackComplianceDetails ::
  -- | 'conformancePackName'
  Prelude.Text ->
  GetConformancePackComplianceDetails
newGetConformancePackComplianceDetails
  pConformancePackName_ =
    GetConformancePackComplianceDetails'
      { filters =
          Prelude.Nothing,
        limit = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        conformancePackName =
          pConformancePackName_
      }

-- | A @ConformancePackEvaluationFilters@ object.
getConformancePackComplianceDetails_filters :: Lens.Lens' GetConformancePackComplianceDetails (Prelude.Maybe ConformancePackEvaluationFilters)
getConformancePackComplianceDetails_filters = Lens.lens (\GetConformancePackComplianceDetails' {filters} -> filters) (\s@GetConformancePackComplianceDetails' {} a -> s {filters = a} :: GetConformancePackComplianceDetails)

-- | The maximum number of evaluation results returned on each page. If you
-- do no specify a number, Config uses the default. The default is 100.
getConformancePackComplianceDetails_limit :: Lens.Lens' GetConformancePackComplianceDetails (Prelude.Maybe Prelude.Natural)
getConformancePackComplianceDetails_limit = Lens.lens (\GetConformancePackComplianceDetails' {limit} -> limit) (\s@GetConformancePackComplianceDetails' {} a -> s {limit = a} :: GetConformancePackComplianceDetails)

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
getConformancePackComplianceDetails_nextToken :: Lens.Lens' GetConformancePackComplianceDetails (Prelude.Maybe Prelude.Text)
getConformancePackComplianceDetails_nextToken = Lens.lens (\GetConformancePackComplianceDetails' {nextToken} -> nextToken) (\s@GetConformancePackComplianceDetails' {} a -> s {nextToken = a} :: GetConformancePackComplianceDetails)

-- | Name of the conformance pack.
getConformancePackComplianceDetails_conformancePackName :: Lens.Lens' GetConformancePackComplianceDetails Prelude.Text
getConformancePackComplianceDetails_conformancePackName = Lens.lens (\GetConformancePackComplianceDetails' {conformancePackName} -> conformancePackName) (\s@GetConformancePackComplianceDetails' {} a -> s {conformancePackName = a} :: GetConformancePackComplianceDetails)

instance
  Core.AWSRequest
    GetConformancePackComplianceDetails
  where
  type
    AWSResponse GetConformancePackComplianceDetails =
      GetConformancePackComplianceDetailsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConformancePackComplianceDetailsResponse'
            Prelude.<$> ( x
                            Data..?> "ConformancePackRuleEvaluationResults"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ConformancePackName")
      )

instance
  Prelude.Hashable
    GetConformancePackComplianceDetails
  where
  hashWithSalt
    _salt
    GetConformancePackComplianceDetails' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` conformancePackName

instance
  Prelude.NFData
    GetConformancePackComplianceDetails
  where
  rnf GetConformancePackComplianceDetails' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf conformancePackName

instance
  Data.ToHeaders
    GetConformancePackComplianceDetails
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.GetConformancePackComplianceDetails" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    GetConformancePackComplianceDetails
  where
  toJSON GetConformancePackComplianceDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("ConformancePackName" Data..= conformancePackName)
          ]
      )

instance
  Data.ToPath
    GetConformancePackComplianceDetails
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetConformancePackComplianceDetails
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConformancePackComplianceDetailsResponse' smart constructor.
data GetConformancePackComplianceDetailsResponse = GetConformancePackComplianceDetailsResponse'
  { -- | Returns a list of @ConformancePackEvaluationResult@ objects.
    conformancePackRuleEvaluationResults :: Prelude.Maybe [ConformancePackEvaluationResult],
    -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Name of the conformance pack.
    conformancePackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConformancePackComplianceDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conformancePackRuleEvaluationResults', 'getConformancePackComplianceDetailsResponse_conformancePackRuleEvaluationResults' - Returns a list of @ConformancePackEvaluationResult@ objects.
--
-- 'nextToken', 'getConformancePackComplianceDetailsResponse_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'httpStatus', 'getConformancePackComplianceDetailsResponse_httpStatus' - The response's http status code.
--
-- 'conformancePackName', 'getConformancePackComplianceDetailsResponse_conformancePackName' - Name of the conformance pack.
newGetConformancePackComplianceDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'conformancePackName'
  Prelude.Text ->
  GetConformancePackComplianceDetailsResponse
newGetConformancePackComplianceDetailsResponse
  pHttpStatus_
  pConformancePackName_ =
    GetConformancePackComplianceDetailsResponse'
      { conformancePackRuleEvaluationResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        conformancePackName =
          pConformancePackName_
      }

-- | Returns a list of @ConformancePackEvaluationResult@ objects.
getConformancePackComplianceDetailsResponse_conformancePackRuleEvaluationResults :: Lens.Lens' GetConformancePackComplianceDetailsResponse (Prelude.Maybe [ConformancePackEvaluationResult])
getConformancePackComplianceDetailsResponse_conformancePackRuleEvaluationResults = Lens.lens (\GetConformancePackComplianceDetailsResponse' {conformancePackRuleEvaluationResults} -> conformancePackRuleEvaluationResults) (\s@GetConformancePackComplianceDetailsResponse' {} a -> s {conformancePackRuleEvaluationResults = a} :: GetConformancePackComplianceDetailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
getConformancePackComplianceDetailsResponse_nextToken :: Lens.Lens' GetConformancePackComplianceDetailsResponse (Prelude.Maybe Prelude.Text)
getConformancePackComplianceDetailsResponse_nextToken = Lens.lens (\GetConformancePackComplianceDetailsResponse' {nextToken} -> nextToken) (\s@GetConformancePackComplianceDetailsResponse' {} a -> s {nextToken = a} :: GetConformancePackComplianceDetailsResponse)

-- | The response's http status code.
getConformancePackComplianceDetailsResponse_httpStatus :: Lens.Lens' GetConformancePackComplianceDetailsResponse Prelude.Int
getConformancePackComplianceDetailsResponse_httpStatus = Lens.lens (\GetConformancePackComplianceDetailsResponse' {httpStatus} -> httpStatus) (\s@GetConformancePackComplianceDetailsResponse' {} a -> s {httpStatus = a} :: GetConformancePackComplianceDetailsResponse)

-- | Name of the conformance pack.
getConformancePackComplianceDetailsResponse_conformancePackName :: Lens.Lens' GetConformancePackComplianceDetailsResponse Prelude.Text
getConformancePackComplianceDetailsResponse_conformancePackName = Lens.lens (\GetConformancePackComplianceDetailsResponse' {conformancePackName} -> conformancePackName) (\s@GetConformancePackComplianceDetailsResponse' {} a -> s {conformancePackName = a} :: GetConformancePackComplianceDetailsResponse)

instance
  Prelude.NFData
    GetConformancePackComplianceDetailsResponse
  where
  rnf GetConformancePackComplianceDetailsResponse' {..} =
    Prelude.rnf conformancePackRuleEvaluationResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf conformancePackName
