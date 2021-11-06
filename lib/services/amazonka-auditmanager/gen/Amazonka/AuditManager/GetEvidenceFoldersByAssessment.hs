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
-- Module      : Amazonka.AuditManager.GetEvidenceFoldersByAssessment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the evidence folders from a specified assessment in Audit
-- Manager.
module Amazonka.AuditManager.GetEvidenceFoldersByAssessment
  ( -- * Creating a Request
    GetEvidenceFoldersByAssessment (..),
    newGetEvidenceFoldersByAssessment,

    -- * Request Lenses
    getEvidenceFoldersByAssessment_nextToken,
    getEvidenceFoldersByAssessment_maxResults,
    getEvidenceFoldersByAssessment_assessmentId,

    -- * Destructuring the Response
    GetEvidenceFoldersByAssessmentResponse (..),
    newGetEvidenceFoldersByAssessmentResponse,

    -- * Response Lenses
    getEvidenceFoldersByAssessmentResponse_nextToken,
    getEvidenceFoldersByAssessmentResponse_evidenceFolders,
    getEvidenceFoldersByAssessmentResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEvidenceFoldersByAssessment' smart constructor.
data GetEvidenceFoldersByAssessment = GetEvidenceFoldersByAssessment'
  { -- | The pagination token used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Represents the maximum number of results per page, or per API request
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier for the specified assessment.
    assessmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEvidenceFoldersByAssessment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getEvidenceFoldersByAssessment_nextToken' - The pagination token used to fetch the next set of results.
--
-- 'maxResults', 'getEvidenceFoldersByAssessment_maxResults' - Represents the maximum number of results per page, or per API request
-- call.
--
-- 'assessmentId', 'getEvidenceFoldersByAssessment_assessmentId' - The identifier for the specified assessment.
newGetEvidenceFoldersByAssessment ::
  -- | 'assessmentId'
  Prelude.Text ->
  GetEvidenceFoldersByAssessment
newGetEvidenceFoldersByAssessment pAssessmentId_ =
  GetEvidenceFoldersByAssessment'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      assessmentId = pAssessmentId_
    }

-- | The pagination token used to fetch the next set of results.
getEvidenceFoldersByAssessment_nextToken :: Lens.Lens' GetEvidenceFoldersByAssessment (Prelude.Maybe Prelude.Text)
getEvidenceFoldersByAssessment_nextToken = Lens.lens (\GetEvidenceFoldersByAssessment' {nextToken} -> nextToken) (\s@GetEvidenceFoldersByAssessment' {} a -> s {nextToken = a} :: GetEvidenceFoldersByAssessment)

-- | Represents the maximum number of results per page, or per API request
-- call.
getEvidenceFoldersByAssessment_maxResults :: Lens.Lens' GetEvidenceFoldersByAssessment (Prelude.Maybe Prelude.Natural)
getEvidenceFoldersByAssessment_maxResults = Lens.lens (\GetEvidenceFoldersByAssessment' {maxResults} -> maxResults) (\s@GetEvidenceFoldersByAssessment' {} a -> s {maxResults = a} :: GetEvidenceFoldersByAssessment)

-- | The identifier for the specified assessment.
getEvidenceFoldersByAssessment_assessmentId :: Lens.Lens' GetEvidenceFoldersByAssessment Prelude.Text
getEvidenceFoldersByAssessment_assessmentId = Lens.lens (\GetEvidenceFoldersByAssessment' {assessmentId} -> assessmentId) (\s@GetEvidenceFoldersByAssessment' {} a -> s {assessmentId = a} :: GetEvidenceFoldersByAssessment)

instance
  Core.AWSRequest
    GetEvidenceFoldersByAssessment
  where
  type
    AWSResponse GetEvidenceFoldersByAssessment =
      GetEvidenceFoldersByAssessmentResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEvidenceFoldersByAssessmentResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "evidenceFolders"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetEvidenceFoldersByAssessment

instance
  Prelude.NFData
    GetEvidenceFoldersByAssessment

instance
  Core.ToHeaders
    GetEvidenceFoldersByAssessment
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetEvidenceFoldersByAssessment where
  toPath GetEvidenceFoldersByAssessment' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Core.toBS assessmentId,
        "/evidenceFolders"
      ]

instance Core.ToQuery GetEvidenceFoldersByAssessment where
  toQuery GetEvidenceFoldersByAssessment' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetEvidenceFoldersByAssessmentResponse' smart constructor.
data GetEvidenceFoldersByAssessmentResponse = GetEvidenceFoldersByAssessmentResponse'
  { -- | The pagination token used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of evidence folders returned by the
    -- @GetEvidenceFoldersByAssessment@ API.
    evidenceFolders :: Prelude.Maybe [AssessmentEvidenceFolder],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEvidenceFoldersByAssessmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getEvidenceFoldersByAssessmentResponse_nextToken' - The pagination token used to fetch the next set of results.
--
-- 'evidenceFolders', 'getEvidenceFoldersByAssessmentResponse_evidenceFolders' - The list of evidence folders returned by the
-- @GetEvidenceFoldersByAssessment@ API.
--
-- 'httpStatus', 'getEvidenceFoldersByAssessmentResponse_httpStatus' - The response's http status code.
newGetEvidenceFoldersByAssessmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEvidenceFoldersByAssessmentResponse
newGetEvidenceFoldersByAssessmentResponse
  pHttpStatus_ =
    GetEvidenceFoldersByAssessmentResponse'
      { nextToken =
          Prelude.Nothing,
        evidenceFolders = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The pagination token used to fetch the next set of results.
getEvidenceFoldersByAssessmentResponse_nextToken :: Lens.Lens' GetEvidenceFoldersByAssessmentResponse (Prelude.Maybe Prelude.Text)
getEvidenceFoldersByAssessmentResponse_nextToken = Lens.lens (\GetEvidenceFoldersByAssessmentResponse' {nextToken} -> nextToken) (\s@GetEvidenceFoldersByAssessmentResponse' {} a -> s {nextToken = a} :: GetEvidenceFoldersByAssessmentResponse)

-- | The list of evidence folders returned by the
-- @GetEvidenceFoldersByAssessment@ API.
getEvidenceFoldersByAssessmentResponse_evidenceFolders :: Lens.Lens' GetEvidenceFoldersByAssessmentResponse (Prelude.Maybe [AssessmentEvidenceFolder])
getEvidenceFoldersByAssessmentResponse_evidenceFolders = Lens.lens (\GetEvidenceFoldersByAssessmentResponse' {evidenceFolders} -> evidenceFolders) (\s@GetEvidenceFoldersByAssessmentResponse' {} a -> s {evidenceFolders = a} :: GetEvidenceFoldersByAssessmentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getEvidenceFoldersByAssessmentResponse_httpStatus :: Lens.Lens' GetEvidenceFoldersByAssessmentResponse Prelude.Int
getEvidenceFoldersByAssessmentResponse_httpStatus = Lens.lens (\GetEvidenceFoldersByAssessmentResponse' {httpStatus} -> httpStatus) (\s@GetEvidenceFoldersByAssessmentResponse' {} a -> s {httpStatus = a} :: GetEvidenceFoldersByAssessmentResponse)

instance
  Prelude.NFData
    GetEvidenceFoldersByAssessmentResponse
