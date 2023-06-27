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
-- Module      : Amazonka.AuditManager.GetEvidenceFoldersByAssessmentControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of evidence folders that are associated with a specified
-- control in an Audit Manager assessment.
module Amazonka.AuditManager.GetEvidenceFoldersByAssessmentControl
  ( -- * Creating a Request
    GetEvidenceFoldersByAssessmentControl (..),
    newGetEvidenceFoldersByAssessmentControl,

    -- * Request Lenses
    getEvidenceFoldersByAssessmentControl_maxResults,
    getEvidenceFoldersByAssessmentControl_nextToken,
    getEvidenceFoldersByAssessmentControl_assessmentId,
    getEvidenceFoldersByAssessmentControl_controlSetId,
    getEvidenceFoldersByAssessmentControl_controlId,

    -- * Destructuring the Response
    GetEvidenceFoldersByAssessmentControlResponse (..),
    newGetEvidenceFoldersByAssessmentControlResponse,

    -- * Response Lenses
    getEvidenceFoldersByAssessmentControlResponse_evidenceFolders,
    getEvidenceFoldersByAssessmentControlResponse_nextToken,
    getEvidenceFoldersByAssessmentControlResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEvidenceFoldersByAssessmentControl' smart constructor.
data GetEvidenceFoldersByAssessmentControl = GetEvidenceFoldersByAssessmentControl'
  { -- | Represents the maximum number of results on a page or for an API request
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the assessment.
    assessmentId :: Prelude.Text,
    -- | The identifier for the control set.
    controlSetId :: Prelude.Text,
    -- | The identifier for the control.
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
-- 'maxResults', 'getEvidenceFoldersByAssessmentControl_maxResults' - Represents the maximum number of results on a page or for an API request
-- call.
--
-- 'nextToken', 'getEvidenceFoldersByAssessmentControl_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'assessmentId', 'getEvidenceFoldersByAssessmentControl_assessmentId' - The identifier for the assessment.
--
-- 'controlSetId', 'getEvidenceFoldersByAssessmentControl_controlSetId' - The identifier for the control set.
--
-- 'controlId', 'getEvidenceFoldersByAssessmentControl_controlId' - The identifier for the control.
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
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        assessmentId = pAssessmentId_,
        controlSetId = pControlSetId_,
        controlId = pControlId_
      }

-- | Represents the maximum number of results on a page or for an API request
-- call.
getEvidenceFoldersByAssessmentControl_maxResults :: Lens.Lens' GetEvidenceFoldersByAssessmentControl (Prelude.Maybe Prelude.Natural)
getEvidenceFoldersByAssessmentControl_maxResults = Lens.lens (\GetEvidenceFoldersByAssessmentControl' {maxResults} -> maxResults) (\s@GetEvidenceFoldersByAssessmentControl' {} a -> s {maxResults = a} :: GetEvidenceFoldersByAssessmentControl)

-- | The pagination token that\'s used to fetch the next set of results.
getEvidenceFoldersByAssessmentControl_nextToken :: Lens.Lens' GetEvidenceFoldersByAssessmentControl (Prelude.Maybe Prelude.Text)
getEvidenceFoldersByAssessmentControl_nextToken = Lens.lens (\GetEvidenceFoldersByAssessmentControl' {nextToken} -> nextToken) (\s@GetEvidenceFoldersByAssessmentControl' {} a -> s {nextToken = a} :: GetEvidenceFoldersByAssessmentControl)

-- | The identifier for the assessment.
getEvidenceFoldersByAssessmentControl_assessmentId :: Lens.Lens' GetEvidenceFoldersByAssessmentControl Prelude.Text
getEvidenceFoldersByAssessmentControl_assessmentId = Lens.lens (\GetEvidenceFoldersByAssessmentControl' {assessmentId} -> assessmentId) (\s@GetEvidenceFoldersByAssessmentControl' {} a -> s {assessmentId = a} :: GetEvidenceFoldersByAssessmentControl)

-- | The identifier for the control set.
getEvidenceFoldersByAssessmentControl_controlSetId :: Lens.Lens' GetEvidenceFoldersByAssessmentControl Prelude.Text
getEvidenceFoldersByAssessmentControl_controlSetId = Lens.lens (\GetEvidenceFoldersByAssessmentControl' {controlSetId} -> controlSetId) (\s@GetEvidenceFoldersByAssessmentControl' {} a -> s {controlSetId = a} :: GetEvidenceFoldersByAssessmentControl)

-- | The identifier for the control.
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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEvidenceFoldersByAssessmentControlResponse'
            Prelude.<$> ( x
                            Data..?> "evidenceFolders"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetEvidenceFoldersByAssessmentControl
  where
  hashWithSalt
    _salt
    GetEvidenceFoldersByAssessmentControl' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` assessmentId
        `Prelude.hashWithSalt` controlSetId
        `Prelude.hashWithSalt` controlId

instance
  Prelude.NFData
    GetEvidenceFoldersByAssessmentControl
  where
  rnf GetEvidenceFoldersByAssessmentControl' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf controlSetId
      `Prelude.seq` Prelude.rnf controlId

instance
  Data.ToHeaders
    GetEvidenceFoldersByAssessmentControl
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

instance
  Data.ToPath
    GetEvidenceFoldersByAssessmentControl
  where
  toPath GetEvidenceFoldersByAssessmentControl' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Data.toBS assessmentId,
        "/evidenceFolders-by-assessment-control/",
        Data.toBS controlSetId,
        "/",
        Data.toBS controlId
      ]

instance
  Data.ToQuery
    GetEvidenceFoldersByAssessmentControl
  where
  toQuery GetEvidenceFoldersByAssessmentControl' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetEvidenceFoldersByAssessmentControlResponse' smart constructor.
data GetEvidenceFoldersByAssessmentControlResponse = GetEvidenceFoldersByAssessmentControlResponse'
  { -- | The list of evidence folders that the
    -- @GetEvidenceFoldersByAssessmentControl@ API returned.
    evidenceFolders :: Prelude.Maybe [AssessmentEvidenceFolder],
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'evidenceFolders', 'getEvidenceFoldersByAssessmentControlResponse_evidenceFolders' - The list of evidence folders that the
-- @GetEvidenceFoldersByAssessmentControl@ API returned.
--
-- 'nextToken', 'getEvidenceFoldersByAssessmentControlResponse_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'httpStatus', 'getEvidenceFoldersByAssessmentControlResponse_httpStatus' - The response's http status code.
newGetEvidenceFoldersByAssessmentControlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEvidenceFoldersByAssessmentControlResponse
newGetEvidenceFoldersByAssessmentControlResponse
  pHttpStatus_ =
    GetEvidenceFoldersByAssessmentControlResponse'
      { evidenceFolders =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of evidence folders that the
-- @GetEvidenceFoldersByAssessmentControl@ API returned.
getEvidenceFoldersByAssessmentControlResponse_evidenceFolders :: Lens.Lens' GetEvidenceFoldersByAssessmentControlResponse (Prelude.Maybe [AssessmentEvidenceFolder])
getEvidenceFoldersByAssessmentControlResponse_evidenceFolders = Lens.lens (\GetEvidenceFoldersByAssessmentControlResponse' {evidenceFolders} -> evidenceFolders) (\s@GetEvidenceFoldersByAssessmentControlResponse' {} a -> s {evidenceFolders = a} :: GetEvidenceFoldersByAssessmentControlResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that\'s used to fetch the next set of results.
getEvidenceFoldersByAssessmentControlResponse_nextToken :: Lens.Lens' GetEvidenceFoldersByAssessmentControlResponse (Prelude.Maybe Prelude.Text)
getEvidenceFoldersByAssessmentControlResponse_nextToken = Lens.lens (\GetEvidenceFoldersByAssessmentControlResponse' {nextToken} -> nextToken) (\s@GetEvidenceFoldersByAssessmentControlResponse' {} a -> s {nextToken = a} :: GetEvidenceFoldersByAssessmentControlResponse)

-- | The response's http status code.
getEvidenceFoldersByAssessmentControlResponse_httpStatus :: Lens.Lens' GetEvidenceFoldersByAssessmentControlResponse Prelude.Int
getEvidenceFoldersByAssessmentControlResponse_httpStatus = Lens.lens (\GetEvidenceFoldersByAssessmentControlResponse' {httpStatus} -> httpStatus) (\s@GetEvidenceFoldersByAssessmentControlResponse' {} a -> s {httpStatus = a} :: GetEvidenceFoldersByAssessmentControlResponse)

instance
  Prelude.NFData
    GetEvidenceFoldersByAssessmentControlResponse
  where
  rnf
    GetEvidenceFoldersByAssessmentControlResponse' {..} =
      Prelude.rnf evidenceFolders
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
