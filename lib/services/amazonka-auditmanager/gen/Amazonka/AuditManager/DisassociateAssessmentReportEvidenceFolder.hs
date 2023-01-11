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
-- Module      : Amazonka.AuditManager.DisassociateAssessmentReportEvidenceFolder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an evidence folder from the specified assessment report in
-- Audit Manager.
module Amazonka.AuditManager.DisassociateAssessmentReportEvidenceFolder
  ( -- * Creating a Request
    DisassociateAssessmentReportEvidenceFolder (..),
    newDisassociateAssessmentReportEvidenceFolder,

    -- * Request Lenses
    disassociateAssessmentReportEvidenceFolder_assessmentId,
    disassociateAssessmentReportEvidenceFolder_evidenceFolderId,

    -- * Destructuring the Response
    DisassociateAssessmentReportEvidenceFolderResponse (..),
    newDisassociateAssessmentReportEvidenceFolderResponse,

    -- * Response Lenses
    disassociateAssessmentReportEvidenceFolderResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateAssessmentReportEvidenceFolder' smart constructor.
data DisassociateAssessmentReportEvidenceFolder = DisassociateAssessmentReportEvidenceFolder'
  { -- | The unique identifier for the assessment.
    assessmentId :: Prelude.Text,
    -- | The unique identifier for the folder that the evidence is stored in.
    evidenceFolderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateAssessmentReportEvidenceFolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentId', 'disassociateAssessmentReportEvidenceFolder_assessmentId' - The unique identifier for the assessment.
--
-- 'evidenceFolderId', 'disassociateAssessmentReportEvidenceFolder_evidenceFolderId' - The unique identifier for the folder that the evidence is stored in.
newDisassociateAssessmentReportEvidenceFolder ::
  -- | 'assessmentId'
  Prelude.Text ->
  -- | 'evidenceFolderId'
  Prelude.Text ->
  DisassociateAssessmentReportEvidenceFolder
newDisassociateAssessmentReportEvidenceFolder
  pAssessmentId_
  pEvidenceFolderId_ =
    DisassociateAssessmentReportEvidenceFolder'
      { assessmentId =
          pAssessmentId_,
        evidenceFolderId =
          pEvidenceFolderId_
      }

-- | The unique identifier for the assessment.
disassociateAssessmentReportEvidenceFolder_assessmentId :: Lens.Lens' DisassociateAssessmentReportEvidenceFolder Prelude.Text
disassociateAssessmentReportEvidenceFolder_assessmentId = Lens.lens (\DisassociateAssessmentReportEvidenceFolder' {assessmentId} -> assessmentId) (\s@DisassociateAssessmentReportEvidenceFolder' {} a -> s {assessmentId = a} :: DisassociateAssessmentReportEvidenceFolder)

-- | The unique identifier for the folder that the evidence is stored in.
disassociateAssessmentReportEvidenceFolder_evidenceFolderId :: Lens.Lens' DisassociateAssessmentReportEvidenceFolder Prelude.Text
disassociateAssessmentReportEvidenceFolder_evidenceFolderId = Lens.lens (\DisassociateAssessmentReportEvidenceFolder' {evidenceFolderId} -> evidenceFolderId) (\s@DisassociateAssessmentReportEvidenceFolder' {} a -> s {evidenceFolderId = a} :: DisassociateAssessmentReportEvidenceFolder)

instance
  Core.AWSRequest
    DisassociateAssessmentReportEvidenceFolder
  where
  type
    AWSResponse
      DisassociateAssessmentReportEvidenceFolder =
      DisassociateAssessmentReportEvidenceFolderResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateAssessmentReportEvidenceFolderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateAssessmentReportEvidenceFolder
  where
  hashWithSalt
    _salt
    DisassociateAssessmentReportEvidenceFolder' {..} =
      _salt `Prelude.hashWithSalt` assessmentId
        `Prelude.hashWithSalt` evidenceFolderId

instance
  Prelude.NFData
    DisassociateAssessmentReportEvidenceFolder
  where
  rnf DisassociateAssessmentReportEvidenceFolder' {..} =
    Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf evidenceFolderId

instance
  Data.ToHeaders
    DisassociateAssessmentReportEvidenceFolder
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
    DisassociateAssessmentReportEvidenceFolder
  where
  toJSON
    DisassociateAssessmentReportEvidenceFolder' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("evidenceFolderId" Data..= evidenceFolderId)
            ]
        )

instance
  Data.ToPath
    DisassociateAssessmentReportEvidenceFolder
  where
  toPath
    DisassociateAssessmentReportEvidenceFolder' {..} =
      Prelude.mconcat
        [ "/assessments/",
          Data.toBS assessmentId,
          "/disassociateFromAssessmentReport"
        ]

instance
  Data.ToQuery
    DisassociateAssessmentReportEvidenceFolder
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateAssessmentReportEvidenceFolderResponse' smart constructor.
data DisassociateAssessmentReportEvidenceFolderResponse = DisassociateAssessmentReportEvidenceFolderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateAssessmentReportEvidenceFolderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateAssessmentReportEvidenceFolderResponse_httpStatus' - The response's http status code.
newDisassociateAssessmentReportEvidenceFolderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateAssessmentReportEvidenceFolderResponse
newDisassociateAssessmentReportEvidenceFolderResponse
  pHttpStatus_ =
    DisassociateAssessmentReportEvidenceFolderResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateAssessmentReportEvidenceFolderResponse_httpStatus :: Lens.Lens' DisassociateAssessmentReportEvidenceFolderResponse Prelude.Int
disassociateAssessmentReportEvidenceFolderResponse_httpStatus = Lens.lens (\DisassociateAssessmentReportEvidenceFolderResponse' {httpStatus} -> httpStatus) (\s@DisassociateAssessmentReportEvidenceFolderResponse' {} a -> s {httpStatus = a} :: DisassociateAssessmentReportEvidenceFolderResponse)

instance
  Prelude.NFData
    DisassociateAssessmentReportEvidenceFolderResponse
  where
  rnf
    DisassociateAssessmentReportEvidenceFolderResponse' {..} =
      Prelude.rnf httpStatus
