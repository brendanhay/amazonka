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
-- Module      : Amazonka.AuditManager.UpdateAssessmentControlSetStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of a control set in an Audit Manager assessment.
module Amazonka.AuditManager.UpdateAssessmentControlSetStatus
  ( -- * Creating a Request
    UpdateAssessmentControlSetStatus (..),
    newUpdateAssessmentControlSetStatus,

    -- * Request Lenses
    updateAssessmentControlSetStatus_assessmentId,
    updateAssessmentControlSetStatus_controlSetId,
    updateAssessmentControlSetStatus_status,
    updateAssessmentControlSetStatus_comment,

    -- * Destructuring the Response
    UpdateAssessmentControlSetStatusResponse (..),
    newUpdateAssessmentControlSetStatusResponse,

    -- * Response Lenses
    updateAssessmentControlSetStatusResponse_controlSet,
    updateAssessmentControlSetStatusResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAssessmentControlSetStatus' smart constructor.
data UpdateAssessmentControlSetStatus = UpdateAssessmentControlSetStatus'
  { -- | The unique identifier for the assessment.
    assessmentId :: Prelude.Text,
    -- | The unique identifier for the control set.
    controlSetId :: Prelude.Text,
    -- | The status of the control set that\'s being updated.
    status :: ControlSetStatus,
    -- | The comment that\'s related to the status update.
    comment :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssessmentControlSetStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentId', 'updateAssessmentControlSetStatus_assessmentId' - The unique identifier for the assessment.
--
-- 'controlSetId', 'updateAssessmentControlSetStatus_controlSetId' - The unique identifier for the control set.
--
-- 'status', 'updateAssessmentControlSetStatus_status' - The status of the control set that\'s being updated.
--
-- 'comment', 'updateAssessmentControlSetStatus_comment' - The comment that\'s related to the status update.
newUpdateAssessmentControlSetStatus ::
  -- | 'assessmentId'
  Prelude.Text ->
  -- | 'controlSetId'
  Prelude.Text ->
  -- | 'status'
  ControlSetStatus ->
  -- | 'comment'
  Prelude.Text ->
  UpdateAssessmentControlSetStatus
newUpdateAssessmentControlSetStatus
  pAssessmentId_
  pControlSetId_
  pStatus_
  pComment_ =
    UpdateAssessmentControlSetStatus'
      { assessmentId =
          pAssessmentId_,
        controlSetId = pControlSetId_,
        status = pStatus_,
        comment = pComment_
      }

-- | The unique identifier for the assessment.
updateAssessmentControlSetStatus_assessmentId :: Lens.Lens' UpdateAssessmentControlSetStatus Prelude.Text
updateAssessmentControlSetStatus_assessmentId = Lens.lens (\UpdateAssessmentControlSetStatus' {assessmentId} -> assessmentId) (\s@UpdateAssessmentControlSetStatus' {} a -> s {assessmentId = a} :: UpdateAssessmentControlSetStatus)

-- | The unique identifier for the control set.
updateAssessmentControlSetStatus_controlSetId :: Lens.Lens' UpdateAssessmentControlSetStatus Prelude.Text
updateAssessmentControlSetStatus_controlSetId = Lens.lens (\UpdateAssessmentControlSetStatus' {controlSetId} -> controlSetId) (\s@UpdateAssessmentControlSetStatus' {} a -> s {controlSetId = a} :: UpdateAssessmentControlSetStatus)

-- | The status of the control set that\'s being updated.
updateAssessmentControlSetStatus_status :: Lens.Lens' UpdateAssessmentControlSetStatus ControlSetStatus
updateAssessmentControlSetStatus_status = Lens.lens (\UpdateAssessmentControlSetStatus' {status} -> status) (\s@UpdateAssessmentControlSetStatus' {} a -> s {status = a} :: UpdateAssessmentControlSetStatus)

-- | The comment that\'s related to the status update.
updateAssessmentControlSetStatus_comment :: Lens.Lens' UpdateAssessmentControlSetStatus Prelude.Text
updateAssessmentControlSetStatus_comment = Lens.lens (\UpdateAssessmentControlSetStatus' {comment} -> comment) (\s@UpdateAssessmentControlSetStatus' {} a -> s {comment = a} :: UpdateAssessmentControlSetStatus)

instance
  Core.AWSRequest
    UpdateAssessmentControlSetStatus
  where
  type
    AWSResponse UpdateAssessmentControlSetStatus =
      UpdateAssessmentControlSetStatusResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAssessmentControlSetStatusResponse'
            Prelude.<$> (x Core..?> "controlSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateAssessmentControlSetStatus
  where
  hashWithSalt
    _salt
    UpdateAssessmentControlSetStatus' {..} =
      _salt `Prelude.hashWithSalt` assessmentId
        `Prelude.hashWithSalt` controlSetId
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` comment

instance
  Prelude.NFData
    UpdateAssessmentControlSetStatus
  where
  rnf UpdateAssessmentControlSetStatus' {..} =
    Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf controlSetId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf comment

instance
  Core.ToHeaders
    UpdateAssessmentControlSetStatus
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

instance Core.ToJSON UpdateAssessmentControlSetStatus where
  toJSON UpdateAssessmentControlSetStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("status" Core..= status),
            Prelude.Just ("comment" Core..= comment)
          ]
      )

instance Core.ToPath UpdateAssessmentControlSetStatus where
  toPath UpdateAssessmentControlSetStatus' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Core.toBS assessmentId,
        "/controlSets/",
        Core.toBS controlSetId,
        "/status"
      ]

instance
  Core.ToQuery
    UpdateAssessmentControlSetStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAssessmentControlSetStatusResponse' smart constructor.
data UpdateAssessmentControlSetStatusResponse = UpdateAssessmentControlSetStatusResponse'
  { -- | The name of the updated control set that the
    -- @UpdateAssessmentControlSetStatus@ API returned.
    controlSet :: Prelude.Maybe AssessmentControlSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssessmentControlSetStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlSet', 'updateAssessmentControlSetStatusResponse_controlSet' - The name of the updated control set that the
-- @UpdateAssessmentControlSetStatus@ API returned.
--
-- 'httpStatus', 'updateAssessmentControlSetStatusResponse_httpStatus' - The response's http status code.
newUpdateAssessmentControlSetStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAssessmentControlSetStatusResponse
newUpdateAssessmentControlSetStatusResponse
  pHttpStatus_ =
    UpdateAssessmentControlSetStatusResponse'
      { controlSet =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The name of the updated control set that the
-- @UpdateAssessmentControlSetStatus@ API returned.
updateAssessmentControlSetStatusResponse_controlSet :: Lens.Lens' UpdateAssessmentControlSetStatusResponse (Prelude.Maybe AssessmentControlSet)
updateAssessmentControlSetStatusResponse_controlSet = Lens.lens (\UpdateAssessmentControlSetStatusResponse' {controlSet} -> controlSet) (\s@UpdateAssessmentControlSetStatusResponse' {} a -> s {controlSet = a} :: UpdateAssessmentControlSetStatusResponse)

-- | The response's http status code.
updateAssessmentControlSetStatusResponse_httpStatus :: Lens.Lens' UpdateAssessmentControlSetStatusResponse Prelude.Int
updateAssessmentControlSetStatusResponse_httpStatus = Lens.lens (\UpdateAssessmentControlSetStatusResponse' {httpStatus} -> httpStatus) (\s@UpdateAssessmentControlSetStatusResponse' {} a -> s {httpStatus = a} :: UpdateAssessmentControlSetStatusResponse)

instance
  Prelude.NFData
    UpdateAssessmentControlSetStatusResponse
  where
  rnf UpdateAssessmentControlSetStatusResponse' {..} =
    Prelude.rnf controlSet
      `Prelude.seq` Prelude.rnf httpStatus
