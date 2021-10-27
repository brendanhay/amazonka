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
-- Module      : Network.AWS.AuditManager.GetEvidenceFoldersByAssessmentControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of evidence folders associated with a specified control
-- of an assessment in Audit Manager.
module Network.AWS.AuditManager.GetEvidenceFoldersByAssessmentControl
  ( -- * Creating a Request
    GetEvidenceFoldersByAssessmentControl (..),
    newGetEvidenceFoldersByAssessmentControl,

    -- * Request Lenses
    getEvidenceFoldersByAssessmentControl_nextToken,
    getEvidenceFoldersByAssessmentControl_maxResults,
    getEvidenceFoldersByAssessmentControl_assessmentId,
    getEvidenceFoldersByAssessmentControl_controlSetId,
    getEvidenceFoldersByAssessmentControl_controlId,

    -- * Destructuring the Response
    GetEvidenceFoldersByAssessmentControlResponse (..),
    newGetEvidenceFoldersByAssessmentControlResponse,

    -- * Response Lenses
    getEvidenceFoldersByAssessmentControlResponse_nextToken,
    getEvidenceFoldersByAssessmentControlResponse_evidenceFolders,
    getEvidenceFoldersByAssessmentControlResponse_httpStatus,
  )
where

import Network.AWS.AuditManager.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetEvidenceFoldersByAssessmentControl' smart constructor.
data GetEvidenceFoldersByAssessmentControl = GetEvidenceFoldersByAssessmentControl'
  { -- | The pagination token used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Represents the maximum number of results per page, or per API request
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier for the specified assessment.
    assessmentId :: Prelude.Text,
    -- | The identifier for the specified control set.
    controlSetId :: Prelude.Text,
    -- | The identifier for the specified control.
    controlId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEvidenceFoldersByAssessmentControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getEvidenceFoldersByAssessmentControl_nextToken' - The pagination token used to fetch the next set of results.
--
-- 'maxResults', 'getEvidenceFoldersByAssessmentControl_maxResults' - Represents the maximum number of results per page, or per API request
-- call.
--
-- 'assessmentId', 'getEvidenceFoldersByAssessmentControl_assessmentId' - The identifier for the specified assessment.
--
-- 'controlSetId', 'getEvidenceFoldersByAssessmentControl_controlSetId' - The identifier for the specified control set.
--
-- 'controlId', 'getEvidenceFoldersByAssessmentControl_controlId' - The identifier for the specified control.
newGetEvidenceFoldersByAssessmentControl ::
  -- | 'assessmentId'
  Prelude.Text ->
  -- | 'controlSetId'
  Prelude.Text ->
  -- | 'controlId'
  Prelude.Text ->
  GetEvidenceFoldersByAssessmentControl
newGetEvidenceFoldersByAssessmentControl
  pAssessmentId_
  pControlSetId_
  pControlId_ =
    GetEvidenceFoldersByAssessmentControl'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        assessmentId = pAssessmentId_,
        controlSetId = pControlSetId_,
        controlId = pControlId_
      }

-- | The pagination token used to fetch the next set of results.
getEvidenceFoldersByAssessmentControl_nextToken :: Lens.Lens' GetEvidenceFoldersByAssessmentControl (Prelude.Maybe Prelude.Text)
getEvidenceFoldersByAssessmentControl_nextToken = Lens.lens (\GetEvidenceFoldersByAssessmentControl' {nextToken} -> nextToken) (\s@GetEvidenceFoldersByAssessmentControl' {} a -> s {nextToken = a} :: GetEvidenceFoldersByAssessmentControl)

-- | Represents the maximum number of results per page, or per API request
-- call.
getEvidenceFoldersByAssessmentControl_maxResults :: Lens.Lens' GetEvidenceFoldersByAssessmentControl (Prelude.Maybe Prelude.Natural)
getEvidenceFoldersByAssessmentControl_maxResults = Lens.lens (\GetEvidenceFoldersByAssessmentControl' {maxResults} -> maxResults) (\s@GetEvidenceFoldersByAssessmentControl' {} a -> s {maxResults = a} :: GetEvidenceFoldersByAssessmentControl)

-- | The identifier for the specified assessment.
getEvidenceFoldersByAssessmentControl_assessmentId :: Lens.Lens' GetEvidenceFoldersByAssessmentControl Prelude.Text
getEvidenceFoldersByAssessmentControl_assessmentId = Lens.lens (\GetEvidenceFoldersByAssessmentControl' {assessmentId} -> assessmentId) (\s@GetEvidenceFoldersByAssessmentControl' {} a -> s {assessmentId = a} :: GetEvidenceFoldersByAssessmentControl)

-- | The identifier for the specified control set.
getEvidenceFoldersByAssessmentControl_controlSetId :: Lens.Lens' GetEvidenceFoldersByAssessmentControl Prelude.Text
getEvidenceFoldersByAssessmentControl_controlSetId = Lens.lens (\GetEvidenceFoldersByAssessmentControl' {controlSetId} -> controlSetId) (\s@GetEvidenceFoldersByAssessmentControl' {} a -> s {controlSetId = a} :: GetEvidenceFoldersByAssessmentControl)

-- | The identifier for the specified control.
getEvidenceFoldersByAssessmentControl_controlId :: Lens.Lens' GetEvidenceFoldersByAssessmentControl Prelude.Text
getEvidenceFoldersByAssessmentControl_controlId = Lens.lens (\GetEvidenceFoldersByAssessmentControl' {controlId} -> controlId) (\s@GetEvidenceFoldersByAssessmentControl' {} a -> s {controlId = a} :: GetEvidenceFoldersByAssessmentControl)

instance
  Core.AWSRequest
    GetEvidenceFoldersByAssessmentControl
  where
  type
    AWSResponse
      GetEvidenceFoldersByAssessmentControl =
      GetEvidenceFoldersByAssessmentControlResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEvidenceFoldersByAssessmentControlResponse'
            Prelude.<$> (x Core..?> "nextToken")
              Prelude.<*> ( x Core..?> "evidenceFolders"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetEvidenceFoldersByAssessmentControl

instance
  Prelude.NFData
    GetEvidenceFoldersByAssessmentControl

instance
  Core.ToHeaders
    GetEvidenceFoldersByAssessmentControl
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

instance
  Core.ToPath
    GetEvidenceFoldersByAssessmentControl
  where
  toPath GetEvidenceFoldersByAssessmentControl' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Core.toBS assessmentId,
        "/evidenceFolders-by-assessment-control/",
        Core.toBS controlSetId,
        "/",
        Core.toBS controlId
      ]

instance
  Core.ToQuery
    GetEvidenceFoldersByAssessmentControl
  where
  toQuery GetEvidenceFoldersByAssessmentControl' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetEvidenceFoldersByAssessmentControlResponse' smart constructor.
data GetEvidenceFoldersByAssessmentControlResponse = GetEvidenceFoldersByAssessmentControlResponse'
  { -- | The pagination token used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of evidence folders returned by the
    -- @GetEvidenceFoldersByAssessmentControl@ API.
    evidenceFolders :: Prelude.Maybe [AssessmentEvidenceFolder],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEvidenceFoldersByAssessmentControlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getEvidenceFoldersByAssessmentControlResponse_nextToken' - The pagination token used to fetch the next set of results.
--
-- 'evidenceFolders', 'getEvidenceFoldersByAssessmentControlResponse_evidenceFolders' - The list of evidence folders returned by the
-- @GetEvidenceFoldersByAssessmentControl@ API.
--
-- 'httpStatus', 'getEvidenceFoldersByAssessmentControlResponse_httpStatus' - The response's http status code.
newGetEvidenceFoldersByAssessmentControlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEvidenceFoldersByAssessmentControlResponse
newGetEvidenceFoldersByAssessmentControlResponse
  pHttpStatus_ =
    GetEvidenceFoldersByAssessmentControlResponse'
      { nextToken =
          Prelude.Nothing,
        evidenceFolders =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The pagination token used to fetch the next set of results.
getEvidenceFoldersByAssessmentControlResponse_nextToken :: Lens.Lens' GetEvidenceFoldersByAssessmentControlResponse (Prelude.Maybe Prelude.Text)
getEvidenceFoldersByAssessmentControlResponse_nextToken = Lens.lens (\GetEvidenceFoldersByAssessmentControlResponse' {nextToken} -> nextToken) (\s@GetEvidenceFoldersByAssessmentControlResponse' {} a -> s {nextToken = a} :: GetEvidenceFoldersByAssessmentControlResponse)

-- | The list of evidence folders returned by the
-- @GetEvidenceFoldersByAssessmentControl@ API.
getEvidenceFoldersByAssessmentControlResponse_evidenceFolders :: Lens.Lens' GetEvidenceFoldersByAssessmentControlResponse (Prelude.Maybe [AssessmentEvidenceFolder])
getEvidenceFoldersByAssessmentControlResponse_evidenceFolders = Lens.lens (\GetEvidenceFoldersByAssessmentControlResponse' {evidenceFolders} -> evidenceFolders) (\s@GetEvidenceFoldersByAssessmentControlResponse' {} a -> s {evidenceFolders = a} :: GetEvidenceFoldersByAssessmentControlResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getEvidenceFoldersByAssessmentControlResponse_httpStatus :: Lens.Lens' GetEvidenceFoldersByAssessmentControlResponse Prelude.Int
getEvidenceFoldersByAssessmentControlResponse_httpStatus = Lens.lens (\GetEvidenceFoldersByAssessmentControlResponse' {httpStatus} -> httpStatus) (\s@GetEvidenceFoldersByAssessmentControlResponse' {} a -> s {httpStatus = a} :: GetEvidenceFoldersByAssessmentControlResponse)

instance
  Prelude.NFData
    GetEvidenceFoldersByAssessmentControlResponse
