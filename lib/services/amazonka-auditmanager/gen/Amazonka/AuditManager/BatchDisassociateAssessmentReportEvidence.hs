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
-- Module      : Amazonka.AuditManager.BatchDisassociateAssessmentReportEvidence
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a list of evidence from an assessment report in Audit
-- Manager.
module Amazonka.AuditManager.BatchDisassociateAssessmentReportEvidence
  ( -- * Creating a Request
    BatchDisassociateAssessmentReportEvidence (..),
    newBatchDisassociateAssessmentReportEvidence,

    -- * Request Lenses
    batchDisassociateAssessmentReportEvidence_assessmentId,
    batchDisassociateAssessmentReportEvidence_evidenceFolderId,
    batchDisassociateAssessmentReportEvidence_evidenceIds,

    -- * Destructuring the Response
    BatchDisassociateAssessmentReportEvidenceResponse (..),
    newBatchDisassociateAssessmentReportEvidenceResponse,

    -- * Response Lenses
    batchDisassociateAssessmentReportEvidenceResponse_errors,
    batchDisassociateAssessmentReportEvidenceResponse_evidenceIds,
    batchDisassociateAssessmentReportEvidenceResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDisassociateAssessmentReportEvidence' smart constructor.
data BatchDisassociateAssessmentReportEvidence = BatchDisassociateAssessmentReportEvidence'
  { -- | The identifier for the assessment.
    assessmentId :: Prelude.Text,
    -- | The identifier for the folder that the evidence is stored in.
    evidenceFolderId :: Prelude.Text,
    -- | The list of evidence identifiers.
    evidenceIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDisassociateAssessmentReportEvidence' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentId', 'batchDisassociateAssessmentReportEvidence_assessmentId' - The identifier for the assessment.
--
-- 'evidenceFolderId', 'batchDisassociateAssessmentReportEvidence_evidenceFolderId' - The identifier for the folder that the evidence is stored in.
--
-- 'evidenceIds', 'batchDisassociateAssessmentReportEvidence_evidenceIds' - The list of evidence identifiers.
newBatchDisassociateAssessmentReportEvidence ::
  -- | 'assessmentId'
  Prelude.Text ->
  -- | 'evidenceFolderId'
  Prelude.Text ->
  BatchDisassociateAssessmentReportEvidence
newBatchDisassociateAssessmentReportEvidence
  pAssessmentId_
  pEvidenceFolderId_ =
    BatchDisassociateAssessmentReportEvidence'
      { assessmentId =
          pAssessmentId_,
        evidenceFolderId =
          pEvidenceFolderId_,
        evidenceIds = Prelude.mempty
      }

-- | The identifier for the assessment.
batchDisassociateAssessmentReportEvidence_assessmentId :: Lens.Lens' BatchDisassociateAssessmentReportEvidence Prelude.Text
batchDisassociateAssessmentReportEvidence_assessmentId = Lens.lens (\BatchDisassociateAssessmentReportEvidence' {assessmentId} -> assessmentId) (\s@BatchDisassociateAssessmentReportEvidence' {} a -> s {assessmentId = a} :: BatchDisassociateAssessmentReportEvidence)

-- | The identifier for the folder that the evidence is stored in.
batchDisassociateAssessmentReportEvidence_evidenceFolderId :: Lens.Lens' BatchDisassociateAssessmentReportEvidence Prelude.Text
batchDisassociateAssessmentReportEvidence_evidenceFolderId = Lens.lens (\BatchDisassociateAssessmentReportEvidence' {evidenceFolderId} -> evidenceFolderId) (\s@BatchDisassociateAssessmentReportEvidence' {} a -> s {evidenceFolderId = a} :: BatchDisassociateAssessmentReportEvidence)

-- | The list of evidence identifiers.
batchDisassociateAssessmentReportEvidence_evidenceIds :: Lens.Lens' BatchDisassociateAssessmentReportEvidence [Prelude.Text]
batchDisassociateAssessmentReportEvidence_evidenceIds = Lens.lens (\BatchDisassociateAssessmentReportEvidence' {evidenceIds} -> evidenceIds) (\s@BatchDisassociateAssessmentReportEvidence' {} a -> s {evidenceIds = a} :: BatchDisassociateAssessmentReportEvidence) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchDisassociateAssessmentReportEvidence
  where
  type
    AWSResponse
      BatchDisassociateAssessmentReportEvidence =
      BatchDisassociateAssessmentReportEvidenceResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDisassociateAssessmentReportEvidenceResponse'
            Prelude.<$> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "evidenceIds" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchDisassociateAssessmentReportEvidence
  where
  hashWithSalt
    _salt
    BatchDisassociateAssessmentReportEvidence' {..} =
      _salt
        `Prelude.hashWithSalt` assessmentId
        `Prelude.hashWithSalt` evidenceFolderId
        `Prelude.hashWithSalt` evidenceIds

instance
  Prelude.NFData
    BatchDisassociateAssessmentReportEvidence
  where
  rnf BatchDisassociateAssessmentReportEvidence' {..} =
    Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf evidenceFolderId
      `Prelude.seq` Prelude.rnf evidenceIds

instance
  Data.ToHeaders
    BatchDisassociateAssessmentReportEvidence
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
    BatchDisassociateAssessmentReportEvidence
  where
  toJSON BatchDisassociateAssessmentReportEvidence' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("evidenceFolderId" Data..= evidenceFolderId),
            Prelude.Just ("evidenceIds" Data..= evidenceIds)
          ]
      )

instance
  Data.ToPath
    BatchDisassociateAssessmentReportEvidence
  where
  toPath BatchDisassociateAssessmentReportEvidence' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Data.toBS assessmentId,
        "/batchDisassociateFromAssessmentReport"
      ]

instance
  Data.ToQuery
    BatchDisassociateAssessmentReportEvidence
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDisassociateAssessmentReportEvidenceResponse' smart constructor.
data BatchDisassociateAssessmentReportEvidenceResponse = BatchDisassociateAssessmentReportEvidenceResponse'
  { -- | A list of errors that the @BatchDisassociateAssessmentReportEvidence@
    -- API returned.
    errors :: Prelude.Maybe [AssessmentReportEvidenceError],
    -- | The identifier for the evidence.
    evidenceIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDisassociateAssessmentReportEvidenceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchDisassociateAssessmentReportEvidenceResponse_errors' - A list of errors that the @BatchDisassociateAssessmentReportEvidence@
-- API returned.
--
-- 'evidenceIds', 'batchDisassociateAssessmentReportEvidenceResponse_evidenceIds' - The identifier for the evidence.
--
-- 'httpStatus', 'batchDisassociateAssessmentReportEvidenceResponse_httpStatus' - The response's http status code.
newBatchDisassociateAssessmentReportEvidenceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDisassociateAssessmentReportEvidenceResponse
newBatchDisassociateAssessmentReportEvidenceResponse
  pHttpStatus_ =
    BatchDisassociateAssessmentReportEvidenceResponse'
      { errors =
          Prelude.Nothing,
        evidenceIds =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | A list of errors that the @BatchDisassociateAssessmentReportEvidence@
-- API returned.
batchDisassociateAssessmentReportEvidenceResponse_errors :: Lens.Lens' BatchDisassociateAssessmentReportEvidenceResponse (Prelude.Maybe [AssessmentReportEvidenceError])
batchDisassociateAssessmentReportEvidenceResponse_errors = Lens.lens (\BatchDisassociateAssessmentReportEvidenceResponse' {errors} -> errors) (\s@BatchDisassociateAssessmentReportEvidenceResponse' {} a -> s {errors = a} :: BatchDisassociateAssessmentReportEvidenceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the evidence.
batchDisassociateAssessmentReportEvidenceResponse_evidenceIds :: Lens.Lens' BatchDisassociateAssessmentReportEvidenceResponse (Prelude.Maybe [Prelude.Text])
batchDisassociateAssessmentReportEvidenceResponse_evidenceIds = Lens.lens (\BatchDisassociateAssessmentReportEvidenceResponse' {evidenceIds} -> evidenceIds) (\s@BatchDisassociateAssessmentReportEvidenceResponse' {} a -> s {evidenceIds = a} :: BatchDisassociateAssessmentReportEvidenceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDisassociateAssessmentReportEvidenceResponse_httpStatus :: Lens.Lens' BatchDisassociateAssessmentReportEvidenceResponse Prelude.Int
batchDisassociateAssessmentReportEvidenceResponse_httpStatus = Lens.lens (\BatchDisassociateAssessmentReportEvidenceResponse' {httpStatus} -> httpStatus) (\s@BatchDisassociateAssessmentReportEvidenceResponse' {} a -> s {httpStatus = a} :: BatchDisassociateAssessmentReportEvidenceResponse)

instance
  Prelude.NFData
    BatchDisassociateAssessmentReportEvidenceResponse
  where
  rnf
    BatchDisassociateAssessmentReportEvidenceResponse' {..} =
      Prelude.rnf errors
        `Prelude.seq` Prelude.rnf evidenceIds
        `Prelude.seq` Prelude.rnf httpStatus
