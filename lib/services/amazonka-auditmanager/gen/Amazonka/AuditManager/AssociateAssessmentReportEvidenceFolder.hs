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
-- Module      : Amazonka.AuditManager.AssociateAssessmentReportEvidenceFolder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an evidence folder to an assessment report in an Audit
-- Manager assessment.
module Amazonka.AuditManager.AssociateAssessmentReportEvidenceFolder
  ( -- * Creating a Request
    AssociateAssessmentReportEvidenceFolder (..),
    newAssociateAssessmentReportEvidenceFolder,

    -- * Request Lenses
    associateAssessmentReportEvidenceFolder_assessmentId,
    associateAssessmentReportEvidenceFolder_evidenceFolderId,

    -- * Destructuring the Response
    AssociateAssessmentReportEvidenceFolderResponse (..),
    newAssociateAssessmentReportEvidenceFolderResponse,

    -- * Response Lenses
    associateAssessmentReportEvidenceFolderResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateAssessmentReportEvidenceFolder' smart constructor.
data AssociateAssessmentReportEvidenceFolder = AssociateAssessmentReportEvidenceFolder'
  { -- | The identifier for the assessment.
    assessmentId :: Prelude.Text,
    -- | The identifier for the folder that the evidence is stored in.
    evidenceFolderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateAssessmentReportEvidenceFolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentId', 'associateAssessmentReportEvidenceFolder_assessmentId' - The identifier for the assessment.
--
-- 'evidenceFolderId', 'associateAssessmentReportEvidenceFolder_evidenceFolderId' - The identifier for the folder that the evidence is stored in.
newAssociateAssessmentReportEvidenceFolder ::
  -- | 'assessmentId'
  Prelude.Text ->
  -- | 'evidenceFolderId'
  Prelude.Text ->
  AssociateAssessmentReportEvidenceFolder
newAssociateAssessmentReportEvidenceFolder
  pAssessmentId_
  pEvidenceFolderId_ =
    AssociateAssessmentReportEvidenceFolder'
      { assessmentId =
          pAssessmentId_,
        evidenceFolderId =
          pEvidenceFolderId_
      }

-- | The identifier for the assessment.
associateAssessmentReportEvidenceFolder_assessmentId :: Lens.Lens' AssociateAssessmentReportEvidenceFolder Prelude.Text
associateAssessmentReportEvidenceFolder_assessmentId = Lens.lens (\AssociateAssessmentReportEvidenceFolder' {assessmentId} -> assessmentId) (\s@AssociateAssessmentReportEvidenceFolder' {} a -> s {assessmentId = a} :: AssociateAssessmentReportEvidenceFolder)

-- | The identifier for the folder that the evidence is stored in.
associateAssessmentReportEvidenceFolder_evidenceFolderId :: Lens.Lens' AssociateAssessmentReportEvidenceFolder Prelude.Text
associateAssessmentReportEvidenceFolder_evidenceFolderId = Lens.lens (\AssociateAssessmentReportEvidenceFolder' {evidenceFolderId} -> evidenceFolderId) (\s@AssociateAssessmentReportEvidenceFolder' {} a -> s {evidenceFolderId = a} :: AssociateAssessmentReportEvidenceFolder)

instance
  Core.AWSRequest
    AssociateAssessmentReportEvidenceFolder
  where
  type
    AWSResponse
      AssociateAssessmentReportEvidenceFolder =
      AssociateAssessmentReportEvidenceFolderResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateAssessmentReportEvidenceFolderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateAssessmentReportEvidenceFolder
  where
  hashWithSalt
    _salt
    AssociateAssessmentReportEvidenceFolder' {..} =
      _salt
        `Prelude.hashWithSalt` assessmentId
        `Prelude.hashWithSalt` evidenceFolderId

instance
  Prelude.NFData
    AssociateAssessmentReportEvidenceFolder
  where
  rnf AssociateAssessmentReportEvidenceFolder' {..} =
    Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf evidenceFolderId

instance
  Data.ToHeaders
    AssociateAssessmentReportEvidenceFolder
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
    AssociateAssessmentReportEvidenceFolder
  where
  toJSON AssociateAssessmentReportEvidenceFolder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("evidenceFolderId" Data..= evidenceFolderId)
          ]
      )

instance
  Data.ToPath
    AssociateAssessmentReportEvidenceFolder
  where
  toPath AssociateAssessmentReportEvidenceFolder' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Data.toBS assessmentId,
        "/associateToAssessmentReport"
      ]

instance
  Data.ToQuery
    AssociateAssessmentReportEvidenceFolder
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateAssessmentReportEvidenceFolderResponse' smart constructor.
data AssociateAssessmentReportEvidenceFolderResponse = AssociateAssessmentReportEvidenceFolderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateAssessmentReportEvidenceFolderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateAssessmentReportEvidenceFolderResponse_httpStatus' - The response's http status code.
newAssociateAssessmentReportEvidenceFolderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateAssessmentReportEvidenceFolderResponse
newAssociateAssessmentReportEvidenceFolderResponse
  pHttpStatus_ =
    AssociateAssessmentReportEvidenceFolderResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associateAssessmentReportEvidenceFolderResponse_httpStatus :: Lens.Lens' AssociateAssessmentReportEvidenceFolderResponse Prelude.Int
associateAssessmentReportEvidenceFolderResponse_httpStatus = Lens.lens (\AssociateAssessmentReportEvidenceFolderResponse' {httpStatus} -> httpStatus) (\s@AssociateAssessmentReportEvidenceFolderResponse' {} a -> s {httpStatus = a} :: AssociateAssessmentReportEvidenceFolderResponse)

instance
  Prelude.NFData
    AssociateAssessmentReportEvidenceFolderResponse
  where
  rnf
    AssociateAssessmentReportEvidenceFolderResponse' {..} =
      Prelude.rnf httpStatus
