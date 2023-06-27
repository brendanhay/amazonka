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
-- Module      : Amazonka.AuditManager.BatchAssociateAssessmentReportEvidence
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a list of evidence to an assessment report in an Audit
-- Manager assessment.
module Amazonka.AuditManager.BatchAssociateAssessmentReportEvidence
  ( -- * Creating a Request
    BatchAssociateAssessmentReportEvidence (..),
    newBatchAssociateAssessmentReportEvidence,

    -- * Request Lenses
    batchAssociateAssessmentReportEvidence_assessmentId,
    batchAssociateAssessmentReportEvidence_evidenceFolderId,
    batchAssociateAssessmentReportEvidence_evidenceIds,

    -- * Destructuring the Response
    BatchAssociateAssessmentReportEvidenceResponse (..),
    newBatchAssociateAssessmentReportEvidenceResponse,

    -- * Response Lenses
    batchAssociateAssessmentReportEvidenceResponse_errors,
    batchAssociateAssessmentReportEvidenceResponse_evidenceIds,
    batchAssociateAssessmentReportEvidenceResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchAssociateAssessmentReportEvidence' smart constructor.
data BatchAssociateAssessmentReportEvidence = BatchAssociateAssessmentReportEvidence'
  { -- | The identifier for the assessment.
    assessmentId :: Prelude.Text,
    -- | The identifier for the folder that the evidence is stored in.
    evidenceFolderId :: Prelude.Text,
    -- | The list of evidence identifiers.
    evidenceIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAssociateAssessmentReportEvidence' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentId', 'batchAssociateAssessmentReportEvidence_assessmentId' - The identifier for the assessment.
--
-- 'evidenceFolderId', 'batchAssociateAssessmentReportEvidence_evidenceFolderId' - The identifier for the folder that the evidence is stored in.
--
-- 'evidenceIds', 'batchAssociateAssessmentReportEvidence_evidenceIds' - The list of evidence identifiers.
newBatchAssociateAssessmentReportEvidence ::
  -- | 'assessmentId'
  Prelude.Text ->
  -- | 'evidenceFolderId'
  Prelude.Text ->
  BatchAssociateAssessmentReportEvidence
newBatchAssociateAssessmentReportEvidence
  pAssessmentId_
  pEvidenceFolderId_ =
    BatchAssociateAssessmentReportEvidence'
      { assessmentId =
          pAssessmentId_,
        evidenceFolderId =
          pEvidenceFolderId_,
        evidenceIds = Prelude.mempty
      }

-- | The identifier for the assessment.
batchAssociateAssessmentReportEvidence_assessmentId :: Lens.Lens' BatchAssociateAssessmentReportEvidence Prelude.Text
batchAssociateAssessmentReportEvidence_assessmentId = Lens.lens (\BatchAssociateAssessmentReportEvidence' {assessmentId} -> assessmentId) (\s@BatchAssociateAssessmentReportEvidence' {} a -> s {assessmentId = a} :: BatchAssociateAssessmentReportEvidence)

-- | The identifier for the folder that the evidence is stored in.
batchAssociateAssessmentReportEvidence_evidenceFolderId :: Lens.Lens' BatchAssociateAssessmentReportEvidence Prelude.Text
batchAssociateAssessmentReportEvidence_evidenceFolderId = Lens.lens (\BatchAssociateAssessmentReportEvidence' {evidenceFolderId} -> evidenceFolderId) (\s@BatchAssociateAssessmentReportEvidence' {} a -> s {evidenceFolderId = a} :: BatchAssociateAssessmentReportEvidence)

-- | The list of evidence identifiers.
batchAssociateAssessmentReportEvidence_evidenceIds :: Lens.Lens' BatchAssociateAssessmentReportEvidence [Prelude.Text]
batchAssociateAssessmentReportEvidence_evidenceIds = Lens.lens (\BatchAssociateAssessmentReportEvidence' {evidenceIds} -> evidenceIds) (\s@BatchAssociateAssessmentReportEvidence' {} a -> s {evidenceIds = a} :: BatchAssociateAssessmentReportEvidence) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchAssociateAssessmentReportEvidence
  where
  type
    AWSResponse
      BatchAssociateAssessmentReportEvidence =
      BatchAssociateAssessmentReportEvidenceResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchAssociateAssessmentReportEvidenceResponse'
            Prelude.<$> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "evidenceIds" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchAssociateAssessmentReportEvidence
  where
  hashWithSalt
    _salt
    BatchAssociateAssessmentReportEvidence' {..} =
      _salt
        `Prelude.hashWithSalt` assessmentId
        `Prelude.hashWithSalt` evidenceFolderId
        `Prelude.hashWithSalt` evidenceIds

instance
  Prelude.NFData
    BatchAssociateAssessmentReportEvidence
  where
  rnf BatchAssociateAssessmentReportEvidence' {..} =
    Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf evidenceFolderId
      `Prelude.seq` Prelude.rnf evidenceIds

instance
  Data.ToHeaders
    BatchAssociateAssessmentReportEvidence
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
  Data.ToJSON
    BatchAssociateAssessmentReportEvidence
  where
  toJSON BatchAssociateAssessmentReportEvidence' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("evidenceFolderId" Data..= evidenceFolderId),
            Prelude.Just ("evidenceIds" Data..= evidenceIds)
          ]
      )

instance
  Data.ToPath
    BatchAssociateAssessmentReportEvidence
  where
  toPath BatchAssociateAssessmentReportEvidence' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Data.toBS assessmentId,
        "/batchAssociateToAssessmentReport"
      ]

instance
  Data.ToQuery
    BatchAssociateAssessmentReportEvidence
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchAssociateAssessmentReportEvidenceResponse' smart constructor.
data BatchAssociateAssessmentReportEvidenceResponse = BatchAssociateAssessmentReportEvidenceResponse'
  { -- | A list of errors that the @BatchAssociateAssessmentReportEvidence@ API
    -- returned.
    errors :: Prelude.Maybe [AssessmentReportEvidenceError],
    -- | The list of evidence identifiers.
    evidenceIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAssociateAssessmentReportEvidenceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchAssociateAssessmentReportEvidenceResponse_errors' - A list of errors that the @BatchAssociateAssessmentReportEvidence@ API
-- returned.
--
-- 'evidenceIds', 'batchAssociateAssessmentReportEvidenceResponse_evidenceIds' - The list of evidence identifiers.
--
-- 'httpStatus', 'batchAssociateAssessmentReportEvidenceResponse_httpStatus' - The response's http status code.
newBatchAssociateAssessmentReportEvidenceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchAssociateAssessmentReportEvidenceResponse
newBatchAssociateAssessmentReportEvidenceResponse
  pHttpStatus_ =
    BatchAssociateAssessmentReportEvidenceResponse'
      { errors =
          Prelude.Nothing,
        evidenceIds =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of errors that the @BatchAssociateAssessmentReportEvidence@ API
-- returned.
batchAssociateAssessmentReportEvidenceResponse_errors :: Lens.Lens' BatchAssociateAssessmentReportEvidenceResponse (Prelude.Maybe [AssessmentReportEvidenceError])
batchAssociateAssessmentReportEvidenceResponse_errors = Lens.lens (\BatchAssociateAssessmentReportEvidenceResponse' {errors} -> errors) (\s@BatchAssociateAssessmentReportEvidenceResponse' {} a -> s {errors = a} :: BatchAssociateAssessmentReportEvidenceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of evidence identifiers.
batchAssociateAssessmentReportEvidenceResponse_evidenceIds :: Lens.Lens' BatchAssociateAssessmentReportEvidenceResponse (Prelude.Maybe [Prelude.Text])
batchAssociateAssessmentReportEvidenceResponse_evidenceIds = Lens.lens (\BatchAssociateAssessmentReportEvidenceResponse' {evidenceIds} -> evidenceIds) (\s@BatchAssociateAssessmentReportEvidenceResponse' {} a -> s {evidenceIds = a} :: BatchAssociateAssessmentReportEvidenceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchAssociateAssessmentReportEvidenceResponse_httpStatus :: Lens.Lens' BatchAssociateAssessmentReportEvidenceResponse Prelude.Int
batchAssociateAssessmentReportEvidenceResponse_httpStatus = Lens.lens (\BatchAssociateAssessmentReportEvidenceResponse' {httpStatus} -> httpStatus) (\s@BatchAssociateAssessmentReportEvidenceResponse' {} a -> s {httpStatus = a} :: BatchAssociateAssessmentReportEvidenceResponse)

instance
  Prelude.NFData
    BatchAssociateAssessmentReportEvidenceResponse
  where
  rnf
    BatchAssociateAssessmentReportEvidenceResponse' {..} =
      Prelude.rnf errors
        `Prelude.seq` Prelude.rnf evidenceIds
        `Prelude.seq` Prelude.rnf httpStatus
