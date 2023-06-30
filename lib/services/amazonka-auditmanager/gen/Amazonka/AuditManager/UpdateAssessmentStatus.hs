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
-- Module      : Amazonka.AuditManager.UpdateAssessmentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of an assessment in Audit Manager.
module Amazonka.AuditManager.UpdateAssessmentStatus
  ( -- * Creating a Request
    UpdateAssessmentStatus (..),
    newUpdateAssessmentStatus,

    -- * Request Lenses
    updateAssessmentStatus_assessmentId,
    updateAssessmentStatus_status,

    -- * Destructuring the Response
    UpdateAssessmentStatusResponse (..),
    newUpdateAssessmentStatusResponse,

    -- * Response Lenses
    updateAssessmentStatusResponse_assessment,
    updateAssessmentStatusResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAssessmentStatus' smart constructor.
data UpdateAssessmentStatus = UpdateAssessmentStatus'
  { -- | The unique identifier for the assessment.
    assessmentId :: Prelude.Text,
    -- | The current status of the assessment.
    status :: AssessmentStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssessmentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentId', 'updateAssessmentStatus_assessmentId' - The unique identifier for the assessment.
--
-- 'status', 'updateAssessmentStatus_status' - The current status of the assessment.
newUpdateAssessmentStatus ::
  -- | 'assessmentId'
  Prelude.Text ->
  -- | 'status'
  AssessmentStatus ->
  UpdateAssessmentStatus
newUpdateAssessmentStatus pAssessmentId_ pStatus_ =
  UpdateAssessmentStatus'
    { assessmentId =
        pAssessmentId_,
      status = pStatus_
    }

-- | The unique identifier for the assessment.
updateAssessmentStatus_assessmentId :: Lens.Lens' UpdateAssessmentStatus Prelude.Text
updateAssessmentStatus_assessmentId = Lens.lens (\UpdateAssessmentStatus' {assessmentId} -> assessmentId) (\s@UpdateAssessmentStatus' {} a -> s {assessmentId = a} :: UpdateAssessmentStatus)

-- | The current status of the assessment.
updateAssessmentStatus_status :: Lens.Lens' UpdateAssessmentStatus AssessmentStatus
updateAssessmentStatus_status = Lens.lens (\UpdateAssessmentStatus' {status} -> status) (\s@UpdateAssessmentStatus' {} a -> s {status = a} :: UpdateAssessmentStatus)

instance Core.AWSRequest UpdateAssessmentStatus where
  type
    AWSResponse UpdateAssessmentStatus =
      UpdateAssessmentStatusResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAssessmentStatusResponse'
            Prelude.<$> (x Data..?> "assessment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAssessmentStatus where
  hashWithSalt _salt UpdateAssessmentStatus' {..} =
    _salt
      `Prelude.hashWithSalt` assessmentId
      `Prelude.hashWithSalt` status

instance Prelude.NFData UpdateAssessmentStatus where
  rnf UpdateAssessmentStatus' {..} =
    Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders UpdateAssessmentStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAssessmentStatus where
  toJSON UpdateAssessmentStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("status" Data..= status)]
      )

instance Data.ToPath UpdateAssessmentStatus where
  toPath UpdateAssessmentStatus' {..} =
    Prelude.mconcat
      ["/assessments/", Data.toBS assessmentId, "/status"]

instance Data.ToQuery UpdateAssessmentStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAssessmentStatusResponse' smart constructor.
data UpdateAssessmentStatusResponse = UpdateAssessmentStatusResponse'
  { -- | The name of the updated assessment that the @UpdateAssessmentStatus@ API
    -- returned.
    assessment :: Prelude.Maybe Assessment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssessmentStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessment', 'updateAssessmentStatusResponse_assessment' - The name of the updated assessment that the @UpdateAssessmentStatus@ API
-- returned.
--
-- 'httpStatus', 'updateAssessmentStatusResponse_httpStatus' - The response's http status code.
newUpdateAssessmentStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAssessmentStatusResponse
newUpdateAssessmentStatusResponse pHttpStatus_ =
  UpdateAssessmentStatusResponse'
    { assessment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the updated assessment that the @UpdateAssessmentStatus@ API
-- returned.
updateAssessmentStatusResponse_assessment :: Lens.Lens' UpdateAssessmentStatusResponse (Prelude.Maybe Assessment)
updateAssessmentStatusResponse_assessment = Lens.lens (\UpdateAssessmentStatusResponse' {assessment} -> assessment) (\s@UpdateAssessmentStatusResponse' {} a -> s {assessment = a} :: UpdateAssessmentStatusResponse)

-- | The response's http status code.
updateAssessmentStatusResponse_httpStatus :: Lens.Lens' UpdateAssessmentStatusResponse Prelude.Int
updateAssessmentStatusResponse_httpStatus = Lens.lens (\UpdateAssessmentStatusResponse' {httpStatus} -> httpStatus) (\s@UpdateAssessmentStatusResponse' {} a -> s {httpStatus = a} :: UpdateAssessmentStatusResponse)

instance
  Prelude.NFData
    UpdateAssessmentStatusResponse
  where
  rnf UpdateAssessmentStatusResponse' {..} =
    Prelude.rnf assessment
      `Prelude.seq` Prelude.rnf httpStatus
