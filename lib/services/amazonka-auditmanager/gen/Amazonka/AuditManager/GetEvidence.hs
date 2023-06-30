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
-- Module      : Amazonka.AuditManager.GetEvidence
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns evidence from Audit Manager.
module Amazonka.AuditManager.GetEvidence
  ( -- * Creating a Request
    GetEvidence (..),
    newGetEvidence,

    -- * Request Lenses
    getEvidence_assessmentId,
    getEvidence_controlSetId,
    getEvidence_evidenceFolderId,
    getEvidence_evidenceId,

    -- * Destructuring the Response
    GetEvidenceResponse (..),
    newGetEvidenceResponse,

    -- * Response Lenses
    getEvidenceResponse_evidence,
    getEvidenceResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEvidence' smart constructor.
data GetEvidence = GetEvidence'
  { -- | The unique identifier for the assessment.
    assessmentId :: Prelude.Text,
    -- | The unique identifier for the control set.
    controlSetId :: Prelude.Text,
    -- | The unique identifier for the folder that the evidence is stored in.
    evidenceFolderId :: Prelude.Text,
    -- | The unique identifier for the evidence.
    evidenceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEvidence' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentId', 'getEvidence_assessmentId' - The unique identifier for the assessment.
--
-- 'controlSetId', 'getEvidence_controlSetId' - The unique identifier for the control set.
--
-- 'evidenceFolderId', 'getEvidence_evidenceFolderId' - The unique identifier for the folder that the evidence is stored in.
--
-- 'evidenceId', 'getEvidence_evidenceId' - The unique identifier for the evidence.
newGetEvidence ::
  -- | 'assessmentId'
  Prelude.Text ->
  -- | 'controlSetId'
  Prelude.Text ->
  -- | 'evidenceFolderId'
  Prelude.Text ->
  -- | 'evidenceId'
  Prelude.Text ->
  GetEvidence
newGetEvidence
  pAssessmentId_
  pControlSetId_
  pEvidenceFolderId_
  pEvidenceId_ =
    GetEvidence'
      { assessmentId = pAssessmentId_,
        controlSetId = pControlSetId_,
        evidenceFolderId = pEvidenceFolderId_,
        evidenceId = pEvidenceId_
      }

-- | The unique identifier for the assessment.
getEvidence_assessmentId :: Lens.Lens' GetEvidence Prelude.Text
getEvidence_assessmentId = Lens.lens (\GetEvidence' {assessmentId} -> assessmentId) (\s@GetEvidence' {} a -> s {assessmentId = a} :: GetEvidence)

-- | The unique identifier for the control set.
getEvidence_controlSetId :: Lens.Lens' GetEvidence Prelude.Text
getEvidence_controlSetId = Lens.lens (\GetEvidence' {controlSetId} -> controlSetId) (\s@GetEvidence' {} a -> s {controlSetId = a} :: GetEvidence)

-- | The unique identifier for the folder that the evidence is stored in.
getEvidence_evidenceFolderId :: Lens.Lens' GetEvidence Prelude.Text
getEvidence_evidenceFolderId = Lens.lens (\GetEvidence' {evidenceFolderId} -> evidenceFolderId) (\s@GetEvidence' {} a -> s {evidenceFolderId = a} :: GetEvidence)

-- | The unique identifier for the evidence.
getEvidence_evidenceId :: Lens.Lens' GetEvidence Prelude.Text
getEvidence_evidenceId = Lens.lens (\GetEvidence' {evidenceId} -> evidenceId) (\s@GetEvidence' {} a -> s {evidenceId = a} :: GetEvidence)

instance Core.AWSRequest GetEvidence where
  type AWSResponse GetEvidence = GetEvidenceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEvidenceResponse'
            Prelude.<$> (x Data..?> "evidence")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEvidence where
  hashWithSalt _salt GetEvidence' {..} =
    _salt
      `Prelude.hashWithSalt` assessmentId
      `Prelude.hashWithSalt` controlSetId
      `Prelude.hashWithSalt` evidenceFolderId
      `Prelude.hashWithSalt` evidenceId

instance Prelude.NFData GetEvidence where
  rnf GetEvidence' {..} =
    Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf controlSetId
      `Prelude.seq` Prelude.rnf evidenceFolderId
      `Prelude.seq` Prelude.rnf evidenceId

instance Data.ToHeaders GetEvidence where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetEvidence where
  toPath GetEvidence' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Data.toBS assessmentId,
        "/controlSets/",
        Data.toBS controlSetId,
        "/evidenceFolders/",
        Data.toBS evidenceFolderId,
        "/evidence/",
        Data.toBS evidenceId
      ]

instance Data.ToQuery GetEvidence where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEvidenceResponse' smart constructor.
data GetEvidenceResponse = GetEvidenceResponse'
  { -- | The evidence that the @GetEvidence@ API returned.
    evidence :: Prelude.Maybe Evidence,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEvidenceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evidence', 'getEvidenceResponse_evidence' - The evidence that the @GetEvidence@ API returned.
--
-- 'httpStatus', 'getEvidenceResponse_httpStatus' - The response's http status code.
newGetEvidenceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEvidenceResponse
newGetEvidenceResponse pHttpStatus_ =
  GetEvidenceResponse'
    { evidence = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The evidence that the @GetEvidence@ API returned.
getEvidenceResponse_evidence :: Lens.Lens' GetEvidenceResponse (Prelude.Maybe Evidence)
getEvidenceResponse_evidence = Lens.lens (\GetEvidenceResponse' {evidence} -> evidence) (\s@GetEvidenceResponse' {} a -> s {evidence = a} :: GetEvidenceResponse)

-- | The response's http status code.
getEvidenceResponse_httpStatus :: Lens.Lens' GetEvidenceResponse Prelude.Int
getEvidenceResponse_httpStatus = Lens.lens (\GetEvidenceResponse' {httpStatus} -> httpStatus) (\s@GetEvidenceResponse' {} a -> s {httpStatus = a} :: GetEvidenceResponse)

instance Prelude.NFData GetEvidenceResponse where
  rnf GetEvidenceResponse' {..} =
    Prelude.rnf evidence
      `Prelude.seq` Prelude.rnf httpStatus
