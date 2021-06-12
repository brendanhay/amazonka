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
-- Module      : Network.AWS.Config.GetComplianceDetailsByResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the evaluation results for the specified AWS resource. The
-- results indicate which AWS Config rules were used to evaluate the
-- resource, when each rule was last used, and whether the resource
-- complies with each rule.
--
-- This operation returns paginated results.
module Network.AWS.Config.GetComplianceDetailsByResource
  ( -- * Creating a Request
    GetComplianceDetailsByResource (..),
    newGetComplianceDetailsByResource,

    -- * Request Lenses
    getComplianceDetailsByResource_nextToken,
    getComplianceDetailsByResource_complianceTypes,
    getComplianceDetailsByResource_resourceType,
    getComplianceDetailsByResource_resourceId,

    -- * Destructuring the Response
    GetComplianceDetailsByResourceResponse (..),
    newGetComplianceDetailsByResourceResponse,

    -- * Response Lenses
    getComplianceDetailsByResourceResponse_nextToken,
    getComplianceDetailsByResourceResponse_evaluationResults,
    getComplianceDetailsByResourceResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newGetComplianceDetailsByResource' smart constructor.
data GetComplianceDetailsByResource = GetComplianceDetailsByResource'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Filters the results by compliance.
    --
    -- The allowed values are @COMPLIANT@, @NON_COMPLIANT@, and
    -- @NOT_APPLICABLE@.
    complianceTypes :: Core.Maybe [ComplianceType],
    -- | The type of the AWS resource for which you want compliance information.
    resourceType :: Core.Text,
    -- | The ID of the AWS resource for which you want compliance information.
    resourceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetComplianceDetailsByResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getComplianceDetailsByResource_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'complianceTypes', 'getComplianceDetailsByResource_complianceTypes' - Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@, @NON_COMPLIANT@, and
-- @NOT_APPLICABLE@.
--
-- 'resourceType', 'getComplianceDetailsByResource_resourceType' - The type of the AWS resource for which you want compliance information.
--
-- 'resourceId', 'getComplianceDetailsByResource_resourceId' - The ID of the AWS resource for which you want compliance information.
newGetComplianceDetailsByResource ::
  -- | 'resourceType'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  GetComplianceDetailsByResource
newGetComplianceDetailsByResource
  pResourceType_
  pResourceId_ =
    GetComplianceDetailsByResource'
      { nextToken =
          Core.Nothing,
        complianceTypes = Core.Nothing,
        resourceType = pResourceType_,
        resourceId = pResourceId_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getComplianceDetailsByResource_nextToken :: Lens.Lens' GetComplianceDetailsByResource (Core.Maybe Core.Text)
getComplianceDetailsByResource_nextToken = Lens.lens (\GetComplianceDetailsByResource' {nextToken} -> nextToken) (\s@GetComplianceDetailsByResource' {} a -> s {nextToken = a} :: GetComplianceDetailsByResource)

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@, @NON_COMPLIANT@, and
-- @NOT_APPLICABLE@.
getComplianceDetailsByResource_complianceTypes :: Lens.Lens' GetComplianceDetailsByResource (Core.Maybe [ComplianceType])
getComplianceDetailsByResource_complianceTypes = Lens.lens (\GetComplianceDetailsByResource' {complianceTypes} -> complianceTypes) (\s@GetComplianceDetailsByResource' {} a -> s {complianceTypes = a} :: GetComplianceDetailsByResource) Core.. Lens.mapping Lens._Coerce

-- | The type of the AWS resource for which you want compliance information.
getComplianceDetailsByResource_resourceType :: Lens.Lens' GetComplianceDetailsByResource Core.Text
getComplianceDetailsByResource_resourceType = Lens.lens (\GetComplianceDetailsByResource' {resourceType} -> resourceType) (\s@GetComplianceDetailsByResource' {} a -> s {resourceType = a} :: GetComplianceDetailsByResource)

-- | The ID of the AWS resource for which you want compliance information.
getComplianceDetailsByResource_resourceId :: Lens.Lens' GetComplianceDetailsByResource Core.Text
getComplianceDetailsByResource_resourceId = Lens.lens (\GetComplianceDetailsByResource' {resourceId} -> resourceId) (\s@GetComplianceDetailsByResource' {} a -> s {resourceId = a} :: GetComplianceDetailsByResource)

instance Core.AWSPager GetComplianceDetailsByResource where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getComplianceDetailsByResourceResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getComplianceDetailsByResourceResponse_evaluationResults
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getComplianceDetailsByResource_nextToken
          Lens..~ rs
          Lens.^? getComplianceDetailsByResourceResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    GetComplianceDetailsByResource
  where
  type
    AWSResponse GetComplianceDetailsByResource =
      GetComplianceDetailsByResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComplianceDetailsByResourceResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "EvaluationResults" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetComplianceDetailsByResource

instance Core.NFData GetComplianceDetailsByResource

instance
  Core.ToHeaders
    GetComplianceDetailsByResource
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.GetComplianceDetailsByResource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetComplianceDetailsByResource where
  toJSON GetComplianceDetailsByResource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("ComplianceTypes" Core..=) Core.<$> complianceTypes,
            Core.Just ("ResourceType" Core..= resourceType),
            Core.Just ("ResourceId" Core..= resourceId)
          ]
      )

instance Core.ToPath GetComplianceDetailsByResource where
  toPath = Core.const "/"

instance Core.ToQuery GetComplianceDetailsByResource where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newGetComplianceDetailsByResourceResponse' smart constructor.
data GetComplianceDetailsByResourceResponse = GetComplianceDetailsByResourceResponse'
  { -- | The string that you use in a subsequent request to get the next page of
    -- results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Indicates whether the specified AWS resource complies each AWS Config
    -- rule.
    evaluationResults :: Core.Maybe [EvaluationResult],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetComplianceDetailsByResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getComplianceDetailsByResourceResponse_nextToken' - The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
--
-- 'evaluationResults', 'getComplianceDetailsByResourceResponse_evaluationResults' - Indicates whether the specified AWS resource complies each AWS Config
-- rule.
--
-- 'httpStatus', 'getComplianceDetailsByResourceResponse_httpStatus' - The response's http status code.
newGetComplianceDetailsByResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetComplianceDetailsByResourceResponse
newGetComplianceDetailsByResourceResponse
  pHttpStatus_ =
    GetComplianceDetailsByResourceResponse'
      { nextToken =
          Core.Nothing,
        evaluationResults = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
getComplianceDetailsByResourceResponse_nextToken :: Lens.Lens' GetComplianceDetailsByResourceResponse (Core.Maybe Core.Text)
getComplianceDetailsByResourceResponse_nextToken = Lens.lens (\GetComplianceDetailsByResourceResponse' {nextToken} -> nextToken) (\s@GetComplianceDetailsByResourceResponse' {} a -> s {nextToken = a} :: GetComplianceDetailsByResourceResponse)

-- | Indicates whether the specified AWS resource complies each AWS Config
-- rule.
getComplianceDetailsByResourceResponse_evaluationResults :: Lens.Lens' GetComplianceDetailsByResourceResponse (Core.Maybe [EvaluationResult])
getComplianceDetailsByResourceResponse_evaluationResults = Lens.lens (\GetComplianceDetailsByResourceResponse' {evaluationResults} -> evaluationResults) (\s@GetComplianceDetailsByResourceResponse' {} a -> s {evaluationResults = a} :: GetComplianceDetailsByResourceResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getComplianceDetailsByResourceResponse_httpStatus :: Lens.Lens' GetComplianceDetailsByResourceResponse Core.Int
getComplianceDetailsByResourceResponse_httpStatus = Lens.lens (\GetComplianceDetailsByResourceResponse' {httpStatus} -> httpStatus) (\s@GetComplianceDetailsByResourceResponse' {} a -> s {httpStatus = a} :: GetComplianceDetailsByResourceResponse)

instance
  Core.NFData
    GetComplianceDetailsByResourceResponse
