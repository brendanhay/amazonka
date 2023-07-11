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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    getEvidenceFoldersByAssessment_maxResults,
    getEvidenceFoldersByAssessment_nextToken,
    getEvidenceFoldersByAssessment_assessmentId,

    -- * Destructuring the Response
    GetEvidenceFoldersByAssessmentResponse (..),
    newGetEvidenceFoldersByAssessmentResponse,

    -- * Response Lenses
    getEvidenceFoldersByAssessmentResponse_evidenceFolders,
    getEvidenceFoldersByAssessmentResponse_nextToken,
    getEvidenceFoldersByAssessmentResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEvidenceFoldersByAssessment' smart constructor.
data GetEvidenceFoldersByAssessment = GetEvidenceFoldersByAssessment'
  { -- | Represents the maximum number of results on a page or for an API request
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the assessment.
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
-- 'maxResults', 'getEvidenceFoldersByAssessment_maxResults' - Represents the maximum number of results on a page or for an API request
-- call.
--
-- 'nextToken', 'getEvidenceFoldersByAssessment_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'assessmentId', 'getEvidenceFoldersByAssessment_assessmentId' - The unique identifier for the assessment.
newGetEvidenceFoldersByAssessment ::
  -- | 'assessmentId'
  Prelude.Text ->
  GetEvidenceFoldersByAssessment
newGetEvidenceFoldersByAssessment pAssessmentId_ =
  GetEvidenceFoldersByAssessment'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      assessmentId = pAssessmentId_
    }

-- | Represents the maximum number of results on a page or for an API request
-- call.
getEvidenceFoldersByAssessment_maxResults :: Lens.Lens' GetEvidenceFoldersByAssessment (Prelude.Maybe Prelude.Natural)
getEvidenceFoldersByAssessment_maxResults = Lens.lens (\GetEvidenceFoldersByAssessment' {maxResults} -> maxResults) (\s@GetEvidenceFoldersByAssessment' {} a -> s {maxResults = a} :: GetEvidenceFoldersByAssessment)

-- | The pagination token that\'s used to fetch the next set of results.
getEvidenceFoldersByAssessment_nextToken :: Lens.Lens' GetEvidenceFoldersByAssessment (Prelude.Maybe Prelude.Text)
getEvidenceFoldersByAssessment_nextToken = Lens.lens (\GetEvidenceFoldersByAssessment' {nextToken} -> nextToken) (\s@GetEvidenceFoldersByAssessment' {} a -> s {nextToken = a} :: GetEvidenceFoldersByAssessment)

-- | The unique identifier for the assessment.
getEvidenceFoldersByAssessment_assessmentId :: Lens.Lens' GetEvidenceFoldersByAssessment Prelude.Text
getEvidenceFoldersByAssessment_assessmentId = Lens.lens (\GetEvidenceFoldersByAssessment' {assessmentId} -> assessmentId) (\s@GetEvidenceFoldersByAssessment' {} a -> s {assessmentId = a} :: GetEvidenceFoldersByAssessment)

instance
  Core.AWSRequest
    GetEvidenceFoldersByAssessment
  where
  type
    AWSResponse GetEvidenceFoldersByAssessment =
      GetEvidenceFoldersByAssessmentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEvidenceFoldersByAssessmentResponse'
            Prelude.<$> ( x
                            Data..?> "evidenceFolders"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetEvidenceFoldersByAssessment
  where
  hashWithSalt
    _salt
    GetEvidenceFoldersByAssessment' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` assessmentId

instance
  Prelude.NFData
    GetEvidenceFoldersByAssessment
  where
  rnf GetEvidenceFoldersByAssessment' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf assessmentId

instance
  Data.ToHeaders
    GetEvidenceFoldersByAssessment
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetEvidenceFoldersByAssessment where
  toPath GetEvidenceFoldersByAssessment' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Data.toBS assessmentId,
        "/evidenceFolders"
      ]

instance Data.ToQuery GetEvidenceFoldersByAssessment where
  toQuery GetEvidenceFoldersByAssessment' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetEvidenceFoldersByAssessmentResponse' smart constructor.
data GetEvidenceFoldersByAssessmentResponse = GetEvidenceFoldersByAssessmentResponse'
  { -- | The list of evidence folders that the @GetEvidenceFoldersByAssessment@
    -- API returned.
    evidenceFolders :: Prelude.Maybe [AssessmentEvidenceFolder],
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'evidenceFolders', 'getEvidenceFoldersByAssessmentResponse_evidenceFolders' - The list of evidence folders that the @GetEvidenceFoldersByAssessment@
-- API returned.
--
-- 'nextToken', 'getEvidenceFoldersByAssessmentResponse_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'httpStatus', 'getEvidenceFoldersByAssessmentResponse_httpStatus' - The response's http status code.
newGetEvidenceFoldersByAssessmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEvidenceFoldersByAssessmentResponse
newGetEvidenceFoldersByAssessmentResponse
  pHttpStatus_ =
    GetEvidenceFoldersByAssessmentResponse'
      { evidenceFolders =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of evidence folders that the @GetEvidenceFoldersByAssessment@
-- API returned.
getEvidenceFoldersByAssessmentResponse_evidenceFolders :: Lens.Lens' GetEvidenceFoldersByAssessmentResponse (Prelude.Maybe [AssessmentEvidenceFolder])
getEvidenceFoldersByAssessmentResponse_evidenceFolders = Lens.lens (\GetEvidenceFoldersByAssessmentResponse' {evidenceFolders} -> evidenceFolders) (\s@GetEvidenceFoldersByAssessmentResponse' {} a -> s {evidenceFolders = a} :: GetEvidenceFoldersByAssessmentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that\'s used to fetch the next set of results.
getEvidenceFoldersByAssessmentResponse_nextToken :: Lens.Lens' GetEvidenceFoldersByAssessmentResponse (Prelude.Maybe Prelude.Text)
getEvidenceFoldersByAssessmentResponse_nextToken = Lens.lens (\GetEvidenceFoldersByAssessmentResponse' {nextToken} -> nextToken) (\s@GetEvidenceFoldersByAssessmentResponse' {} a -> s {nextToken = a} :: GetEvidenceFoldersByAssessmentResponse)

-- | The response's http status code.
getEvidenceFoldersByAssessmentResponse_httpStatus :: Lens.Lens' GetEvidenceFoldersByAssessmentResponse Prelude.Int
getEvidenceFoldersByAssessmentResponse_httpStatus = Lens.lens (\GetEvidenceFoldersByAssessmentResponse' {httpStatus} -> httpStatus) (\s@GetEvidenceFoldersByAssessmentResponse' {} a -> s {httpStatus = a} :: GetEvidenceFoldersByAssessmentResponse)

instance
  Prelude.NFData
    GetEvidenceFoldersByAssessmentResponse
  where
  rnf GetEvidenceFoldersByAssessmentResponse' {..} =
    Prelude.rnf evidenceFolders
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
