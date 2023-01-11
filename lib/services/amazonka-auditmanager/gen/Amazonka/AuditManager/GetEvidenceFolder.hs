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
-- Module      : Amazonka.AuditManager.GetEvidenceFolder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an evidence folder from the specified assessment in Audit
-- Manager.
module Amazonka.AuditManager.GetEvidenceFolder
  ( -- * Creating a Request
    GetEvidenceFolder (..),
    newGetEvidenceFolder,

    -- * Request Lenses
    getEvidenceFolder_assessmentId,
    getEvidenceFolder_controlSetId,
    getEvidenceFolder_evidenceFolderId,

    -- * Destructuring the Response
    GetEvidenceFolderResponse (..),
    newGetEvidenceFolderResponse,

    -- * Response Lenses
    getEvidenceFolderResponse_evidenceFolder,
    getEvidenceFolderResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEvidenceFolder' smart constructor.
data GetEvidenceFolder = GetEvidenceFolder'
  { -- | The unique identifier for the assessment.
    assessmentId :: Prelude.Text,
    -- | The unique identifier for the control set.
    controlSetId :: Prelude.Text,
    -- | The unique identifier for the folder that the evidence is stored in.
    evidenceFolderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEvidenceFolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentId', 'getEvidenceFolder_assessmentId' - The unique identifier for the assessment.
--
-- 'controlSetId', 'getEvidenceFolder_controlSetId' - The unique identifier for the control set.
--
-- 'evidenceFolderId', 'getEvidenceFolder_evidenceFolderId' - The unique identifier for the folder that the evidence is stored in.
newGetEvidenceFolder ::
  -- | 'assessmentId'
  Prelude.Text ->
  -- | 'controlSetId'
  Prelude.Text ->
  -- | 'evidenceFolderId'
  Prelude.Text ->
  GetEvidenceFolder
newGetEvidenceFolder
  pAssessmentId_
  pControlSetId_
  pEvidenceFolderId_ =
    GetEvidenceFolder'
      { assessmentId = pAssessmentId_,
        controlSetId = pControlSetId_,
        evidenceFolderId = pEvidenceFolderId_
      }

-- | The unique identifier for the assessment.
getEvidenceFolder_assessmentId :: Lens.Lens' GetEvidenceFolder Prelude.Text
getEvidenceFolder_assessmentId = Lens.lens (\GetEvidenceFolder' {assessmentId} -> assessmentId) (\s@GetEvidenceFolder' {} a -> s {assessmentId = a} :: GetEvidenceFolder)

-- | The unique identifier for the control set.
getEvidenceFolder_controlSetId :: Lens.Lens' GetEvidenceFolder Prelude.Text
getEvidenceFolder_controlSetId = Lens.lens (\GetEvidenceFolder' {controlSetId} -> controlSetId) (\s@GetEvidenceFolder' {} a -> s {controlSetId = a} :: GetEvidenceFolder)

-- | The unique identifier for the folder that the evidence is stored in.
getEvidenceFolder_evidenceFolderId :: Lens.Lens' GetEvidenceFolder Prelude.Text
getEvidenceFolder_evidenceFolderId = Lens.lens (\GetEvidenceFolder' {evidenceFolderId} -> evidenceFolderId) (\s@GetEvidenceFolder' {} a -> s {evidenceFolderId = a} :: GetEvidenceFolder)

instance Core.AWSRequest GetEvidenceFolder where
  type
    AWSResponse GetEvidenceFolder =
      GetEvidenceFolderResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEvidenceFolderResponse'
            Prelude.<$> (x Data..?> "evidenceFolder")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEvidenceFolder where
  hashWithSalt _salt GetEvidenceFolder' {..} =
    _salt `Prelude.hashWithSalt` assessmentId
      `Prelude.hashWithSalt` controlSetId
      `Prelude.hashWithSalt` evidenceFolderId

instance Prelude.NFData GetEvidenceFolder where
  rnf GetEvidenceFolder' {..} =
    Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf controlSetId
      `Prelude.seq` Prelude.rnf evidenceFolderId

instance Data.ToHeaders GetEvidenceFolder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetEvidenceFolder where
  toPath GetEvidenceFolder' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Data.toBS assessmentId,
        "/controlSets/",
        Data.toBS controlSetId,
        "/evidenceFolders/",
        Data.toBS evidenceFolderId
      ]

instance Data.ToQuery GetEvidenceFolder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEvidenceFolderResponse' smart constructor.
data GetEvidenceFolderResponse = GetEvidenceFolderResponse'
  { -- | The folder that the evidence is stored in.
    evidenceFolder :: Prelude.Maybe AssessmentEvidenceFolder,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEvidenceFolderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evidenceFolder', 'getEvidenceFolderResponse_evidenceFolder' - The folder that the evidence is stored in.
--
-- 'httpStatus', 'getEvidenceFolderResponse_httpStatus' - The response's http status code.
newGetEvidenceFolderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEvidenceFolderResponse
newGetEvidenceFolderResponse pHttpStatus_ =
  GetEvidenceFolderResponse'
    { evidenceFolder =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The folder that the evidence is stored in.
getEvidenceFolderResponse_evidenceFolder :: Lens.Lens' GetEvidenceFolderResponse (Prelude.Maybe AssessmentEvidenceFolder)
getEvidenceFolderResponse_evidenceFolder = Lens.lens (\GetEvidenceFolderResponse' {evidenceFolder} -> evidenceFolder) (\s@GetEvidenceFolderResponse' {} a -> s {evidenceFolder = a} :: GetEvidenceFolderResponse)

-- | The response's http status code.
getEvidenceFolderResponse_httpStatus :: Lens.Lens' GetEvidenceFolderResponse Prelude.Int
getEvidenceFolderResponse_httpStatus = Lens.lens (\GetEvidenceFolderResponse' {httpStatus} -> httpStatus) (\s@GetEvidenceFolderResponse' {} a -> s {httpStatus = a} :: GetEvidenceFolderResponse)

instance Prelude.NFData GetEvidenceFolderResponse where
  rnf GetEvidenceFolderResponse' {..} =
    Prelude.rnf evidenceFolder
      `Prelude.seq` Prelude.rnf httpStatus
