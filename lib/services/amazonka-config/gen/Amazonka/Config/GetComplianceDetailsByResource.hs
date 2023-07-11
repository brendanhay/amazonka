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
-- Module      : Amazonka.Config.GetComplianceDetailsByResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the evaluation results for the specified Amazon Web Services
-- resource. The results indicate which Config rules were used to evaluate
-- the resource, when each rule was last invoked, and whether the resource
-- complies with each rule.
--
-- This operation returns paginated results.
module Amazonka.Config.GetComplianceDetailsByResource
  ( -- * Creating a Request
    GetComplianceDetailsByResource (..),
    newGetComplianceDetailsByResource,

    -- * Request Lenses
    getComplianceDetailsByResource_complianceTypes,
    getComplianceDetailsByResource_nextToken,
    getComplianceDetailsByResource_resourceEvaluationId,
    getComplianceDetailsByResource_resourceId,
    getComplianceDetailsByResource_resourceType,

    -- * Destructuring the Response
    GetComplianceDetailsByResourceResponse (..),
    newGetComplianceDetailsByResourceResponse,

    -- * Response Lenses
    getComplianceDetailsByResourceResponse_evaluationResults,
    getComplianceDetailsByResourceResponse_nextToken,
    getComplianceDetailsByResourceResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newGetComplianceDetailsByResource' smart constructor.
data GetComplianceDetailsByResource = GetComplianceDetailsByResource'
  { -- | Filters the results by compliance.
    --
    -- The allowed values are @COMPLIANT@, @NON_COMPLIANT@, and
    -- @NOT_APPLICABLE@.
    complianceTypes :: Prelude.Maybe [ComplianceType],
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of Amazon Web Services resource execution for which you
    -- want to retrieve evaluation results.
    --
    -- You need to only provide either a @ResourceEvaluationID@ or a
    -- @ResourceID @and @ResourceType@.
    resourceEvaluationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services resource for which you want compliance
    -- information.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of the Amazon Web Services resource for which you want
    -- compliance information.
    resourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComplianceDetailsByResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceTypes', 'getComplianceDetailsByResource_complianceTypes' - Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@, @NON_COMPLIANT@, and
-- @NOT_APPLICABLE@.
--
-- 'nextToken', 'getComplianceDetailsByResource_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'resourceEvaluationId', 'getComplianceDetailsByResource_resourceEvaluationId' - The unique ID of Amazon Web Services resource execution for which you
-- want to retrieve evaluation results.
--
-- You need to only provide either a @ResourceEvaluationID@ or a
-- @ResourceID @and @ResourceType@.
--
-- 'resourceId', 'getComplianceDetailsByResource_resourceId' - The ID of the Amazon Web Services resource for which you want compliance
-- information.
--
-- 'resourceType', 'getComplianceDetailsByResource_resourceType' - The type of the Amazon Web Services resource for which you want
-- compliance information.
newGetComplianceDetailsByResource ::
  GetComplianceDetailsByResource
newGetComplianceDetailsByResource =
  GetComplianceDetailsByResource'
    { complianceTypes =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceEvaluationId = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@, @NON_COMPLIANT@, and
-- @NOT_APPLICABLE@.
getComplianceDetailsByResource_complianceTypes :: Lens.Lens' GetComplianceDetailsByResource (Prelude.Maybe [ComplianceType])
getComplianceDetailsByResource_complianceTypes = Lens.lens (\GetComplianceDetailsByResource' {complianceTypes} -> complianceTypes) (\s@GetComplianceDetailsByResource' {} a -> s {complianceTypes = a} :: GetComplianceDetailsByResource) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getComplianceDetailsByResource_nextToken :: Lens.Lens' GetComplianceDetailsByResource (Prelude.Maybe Prelude.Text)
getComplianceDetailsByResource_nextToken = Lens.lens (\GetComplianceDetailsByResource' {nextToken} -> nextToken) (\s@GetComplianceDetailsByResource' {} a -> s {nextToken = a} :: GetComplianceDetailsByResource)

-- | The unique ID of Amazon Web Services resource execution for which you
-- want to retrieve evaluation results.
--
-- You need to only provide either a @ResourceEvaluationID@ or a
-- @ResourceID @and @ResourceType@.
getComplianceDetailsByResource_resourceEvaluationId :: Lens.Lens' GetComplianceDetailsByResource (Prelude.Maybe Prelude.Text)
getComplianceDetailsByResource_resourceEvaluationId = Lens.lens (\GetComplianceDetailsByResource' {resourceEvaluationId} -> resourceEvaluationId) (\s@GetComplianceDetailsByResource' {} a -> s {resourceEvaluationId = a} :: GetComplianceDetailsByResource)

-- | The ID of the Amazon Web Services resource for which you want compliance
-- information.
getComplianceDetailsByResource_resourceId :: Lens.Lens' GetComplianceDetailsByResource (Prelude.Maybe Prelude.Text)
getComplianceDetailsByResource_resourceId = Lens.lens (\GetComplianceDetailsByResource' {resourceId} -> resourceId) (\s@GetComplianceDetailsByResource' {} a -> s {resourceId = a} :: GetComplianceDetailsByResource)

-- | The type of the Amazon Web Services resource for which you want
-- compliance information.
getComplianceDetailsByResource_resourceType :: Lens.Lens' GetComplianceDetailsByResource (Prelude.Maybe Prelude.Text)
getComplianceDetailsByResource_resourceType = Lens.lens (\GetComplianceDetailsByResource' {resourceType} -> resourceType) (\s@GetComplianceDetailsByResource' {} a -> s {resourceType = a} :: GetComplianceDetailsByResource)

instance Core.AWSPager GetComplianceDetailsByResource where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getComplianceDetailsByResourceResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getComplianceDetailsByResourceResponse_evaluationResults
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getComplianceDetailsByResource_nextToken
          Lens..~ rs
          Lens.^? getComplianceDetailsByResourceResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetComplianceDetailsByResource
  where
  type
    AWSResponse GetComplianceDetailsByResource =
      GetComplianceDetailsByResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComplianceDetailsByResourceResponse'
            Prelude.<$> ( x
                            Data..?> "EvaluationResults"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetComplianceDetailsByResource
  where
  hashWithSalt
    _salt
    GetComplianceDetailsByResource' {..} =
      _salt
        `Prelude.hashWithSalt` complianceTypes
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` resourceEvaluationId
        `Prelude.hashWithSalt` resourceId
        `Prelude.hashWithSalt` resourceType

instance
  Prelude.NFData
    GetComplianceDetailsByResource
  where
  rnf GetComplianceDetailsByResource' {..} =
    Prelude.rnf complianceTypes
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceEvaluationId
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType

instance
  Data.ToHeaders
    GetComplianceDetailsByResource
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.GetComplianceDetailsByResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetComplianceDetailsByResource where
  toJSON GetComplianceDetailsByResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ComplianceTypes" Data..=)
              Prelude.<$> complianceTypes,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ResourceEvaluationId" Data..=)
              Prelude.<$> resourceEvaluationId,
            ("ResourceId" Data..=) Prelude.<$> resourceId,
            ("ResourceType" Data..=) Prelude.<$> resourceType
          ]
      )

instance Data.ToPath GetComplianceDetailsByResource where
  toPath = Prelude.const "/"

instance Data.ToQuery GetComplianceDetailsByResource where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newGetComplianceDetailsByResourceResponse' smart constructor.
data GetComplianceDetailsByResourceResponse = GetComplianceDetailsByResourceResponse'
  { -- | Indicates whether the specified Amazon Web Services resource complies
    -- each Config rule.
    evaluationResults :: Prelude.Maybe [EvaluationResult],
    -- | The string that you use in a subsequent request to get the next page of
    -- results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComplianceDetailsByResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationResults', 'getComplianceDetailsByResourceResponse_evaluationResults' - Indicates whether the specified Amazon Web Services resource complies
-- each Config rule.
--
-- 'nextToken', 'getComplianceDetailsByResourceResponse_nextToken' - The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
--
-- 'httpStatus', 'getComplianceDetailsByResourceResponse_httpStatus' - The response's http status code.
newGetComplianceDetailsByResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetComplianceDetailsByResourceResponse
newGetComplianceDetailsByResourceResponse
  pHttpStatus_ =
    GetComplianceDetailsByResourceResponse'
      { evaluationResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Indicates whether the specified Amazon Web Services resource complies
-- each Config rule.
getComplianceDetailsByResourceResponse_evaluationResults :: Lens.Lens' GetComplianceDetailsByResourceResponse (Prelude.Maybe [EvaluationResult])
getComplianceDetailsByResourceResponse_evaluationResults = Lens.lens (\GetComplianceDetailsByResourceResponse' {evaluationResults} -> evaluationResults) (\s@GetComplianceDetailsByResourceResponse' {} a -> s {evaluationResults = a} :: GetComplianceDetailsByResourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
getComplianceDetailsByResourceResponse_nextToken :: Lens.Lens' GetComplianceDetailsByResourceResponse (Prelude.Maybe Prelude.Text)
getComplianceDetailsByResourceResponse_nextToken = Lens.lens (\GetComplianceDetailsByResourceResponse' {nextToken} -> nextToken) (\s@GetComplianceDetailsByResourceResponse' {} a -> s {nextToken = a} :: GetComplianceDetailsByResourceResponse)

-- | The response's http status code.
getComplianceDetailsByResourceResponse_httpStatus :: Lens.Lens' GetComplianceDetailsByResourceResponse Prelude.Int
getComplianceDetailsByResourceResponse_httpStatus = Lens.lens (\GetComplianceDetailsByResourceResponse' {httpStatus} -> httpStatus) (\s@GetComplianceDetailsByResourceResponse' {} a -> s {httpStatus = a} :: GetComplianceDetailsByResourceResponse)

instance
  Prelude.NFData
    GetComplianceDetailsByResourceResponse
  where
  rnf GetComplianceDetailsByResourceResponse' {..} =
    Prelude.rnf evaluationResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
