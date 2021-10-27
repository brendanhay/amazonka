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
-- Module      : Network.AWS.AuditManager.GetEvidence
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns evidence from Audit Manager.
module Network.AWS.AuditManager.GetEvidence
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

import Network.AWS.AuditManager.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetEvidence' smart constructor.
data GetEvidence = GetEvidence'
  { -- | The identifier for the specified assessment.
    assessmentId :: Prelude.Text,
    -- | The identifier for the specified control set.
    controlSetId :: Prelude.Text,
    -- | The identifier for the folder in which the evidence is stored.
    evidenceFolderId :: Prelude.Text,
    -- | The identifier for the evidence.
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
-- 'assessmentId', 'getEvidence_assessmentId' - The identifier for the specified assessment.
--
-- 'controlSetId', 'getEvidence_controlSetId' - The identifier for the specified control set.
--
-- 'evidenceFolderId', 'getEvidence_evidenceFolderId' - The identifier for the folder in which the evidence is stored.
--
-- 'evidenceId', 'getEvidence_evidenceId' - The identifier for the evidence.
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

-- | The identifier for the specified assessment.
getEvidence_assessmentId :: Lens.Lens' GetEvidence Prelude.Text
getEvidence_assessmentId = Lens.lens (\GetEvidence' {assessmentId} -> assessmentId) (\s@GetEvidence' {} a -> s {assessmentId = a} :: GetEvidence)

-- | The identifier for the specified control set.
getEvidence_controlSetId :: Lens.Lens' GetEvidence Prelude.Text
getEvidence_controlSetId = Lens.lens (\GetEvidence' {controlSetId} -> controlSetId) (\s@GetEvidence' {} a -> s {controlSetId = a} :: GetEvidence)

-- | The identifier for the folder in which the evidence is stored.
getEvidence_evidenceFolderId :: Lens.Lens' GetEvidence Prelude.Text
getEvidence_evidenceFolderId = Lens.lens (\GetEvidence' {evidenceFolderId} -> evidenceFolderId) (\s@GetEvidence' {} a -> s {evidenceFolderId = a} :: GetEvidence)

-- | The identifier for the evidence.
getEvidence_evidenceId :: Lens.Lens' GetEvidence Prelude.Text
getEvidence_evidenceId = Lens.lens (\GetEvidence' {evidenceId} -> evidenceId) (\s@GetEvidence' {} a -> s {evidenceId = a} :: GetEvidence)

instance Core.AWSRequest GetEvidence where
  type AWSResponse GetEvidence = GetEvidenceResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEvidenceResponse'
            Prelude.<$> (x Core..?> "evidence")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEvidence

instance Prelude.NFData GetEvidence

instance Core.ToHeaders GetEvidence where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetEvidence where
  toPath GetEvidence' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Core.toBS assessmentId,
        "/controlSets/",
        Core.toBS controlSetId,
        "/evidenceFolders/",
        Core.toBS evidenceFolderId,
        "/evidence/",
        Core.toBS evidenceId
      ]

instance Core.ToQuery GetEvidence where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEvidenceResponse' smart constructor.
data GetEvidenceResponse = GetEvidenceResponse'
  { -- | The evidence returned by the @GetEvidenceResponse@ API.
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
-- 'evidence', 'getEvidenceResponse_evidence' - The evidence returned by the @GetEvidenceResponse@ API.
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

-- | The evidence returned by the @GetEvidenceResponse@ API.
getEvidenceResponse_evidence :: Lens.Lens' GetEvidenceResponse (Prelude.Maybe Evidence)
getEvidenceResponse_evidence = Lens.lens (\GetEvidenceResponse' {evidence} -> evidence) (\s@GetEvidenceResponse' {} a -> s {evidence = a} :: GetEvidenceResponse)

-- | The response's http status code.
getEvidenceResponse_httpStatus :: Lens.Lens' GetEvidenceResponse Prelude.Int
getEvidenceResponse_httpStatus = Lens.lens (\GetEvidenceResponse' {httpStatus} -> httpStatus) (\s@GetEvidenceResponse' {} a -> s {httpStatus = a} :: GetEvidenceResponse)

instance Prelude.NFData GetEvidenceResponse
