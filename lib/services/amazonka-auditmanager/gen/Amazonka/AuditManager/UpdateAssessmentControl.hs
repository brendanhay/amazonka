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
-- Module      : Amazonka.AuditManager.UpdateAssessmentControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a control within an assessment in Audit Manager.
module Amazonka.AuditManager.UpdateAssessmentControl
  ( -- * Creating a Request
    UpdateAssessmentControl (..),
    newUpdateAssessmentControl,

    -- * Request Lenses
    updateAssessmentControl_controlStatus,
    updateAssessmentControl_commentBody,
    updateAssessmentControl_assessmentId,
    updateAssessmentControl_controlSetId,
    updateAssessmentControl_controlId,

    -- * Destructuring the Response
    UpdateAssessmentControlResponse (..),
    newUpdateAssessmentControlResponse,

    -- * Response Lenses
    updateAssessmentControlResponse_control,
    updateAssessmentControlResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAssessmentControl' smart constructor.
data UpdateAssessmentControl = UpdateAssessmentControl'
  { -- | The status of the specified control.
    controlStatus :: Prelude.Maybe ControlStatus,
    -- | The comment body text for the specified control.
    commentBody :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the specified assessment.
    assessmentId :: Prelude.Text,
    -- | The identifier for the specified control set.
    controlSetId :: Prelude.Text,
    -- | The identifier for the specified control.
    controlId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssessmentControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlStatus', 'updateAssessmentControl_controlStatus' - The status of the specified control.
--
-- 'commentBody', 'updateAssessmentControl_commentBody' - The comment body text for the specified control.
--
-- 'assessmentId', 'updateAssessmentControl_assessmentId' - The identifier for the specified assessment.
--
-- 'controlSetId', 'updateAssessmentControl_controlSetId' - The identifier for the specified control set.
--
-- 'controlId', 'updateAssessmentControl_controlId' - The identifier for the specified control.
newUpdateAssessmentControl ::
  -- | 'assessmentId'
  Prelude.Text ->
  -- | 'controlSetId'
  Prelude.Text ->
  -- | 'controlId'
  Prelude.Text ->
  UpdateAssessmentControl
newUpdateAssessmentControl
  pAssessmentId_
  pControlSetId_
  pControlId_ =
    UpdateAssessmentControl'
      { controlStatus =
          Prelude.Nothing,
        commentBody = Prelude.Nothing,
        assessmentId = pAssessmentId_,
        controlSetId = pControlSetId_,
        controlId = pControlId_
      }

-- | The status of the specified control.
updateAssessmentControl_controlStatus :: Lens.Lens' UpdateAssessmentControl (Prelude.Maybe ControlStatus)
updateAssessmentControl_controlStatus = Lens.lens (\UpdateAssessmentControl' {controlStatus} -> controlStatus) (\s@UpdateAssessmentControl' {} a -> s {controlStatus = a} :: UpdateAssessmentControl)

-- | The comment body text for the specified control.
updateAssessmentControl_commentBody :: Lens.Lens' UpdateAssessmentControl (Prelude.Maybe Prelude.Text)
updateAssessmentControl_commentBody = Lens.lens (\UpdateAssessmentControl' {commentBody} -> commentBody) (\s@UpdateAssessmentControl' {} a -> s {commentBody = a} :: UpdateAssessmentControl)

-- | The identifier for the specified assessment.
updateAssessmentControl_assessmentId :: Lens.Lens' UpdateAssessmentControl Prelude.Text
updateAssessmentControl_assessmentId = Lens.lens (\UpdateAssessmentControl' {assessmentId} -> assessmentId) (\s@UpdateAssessmentControl' {} a -> s {assessmentId = a} :: UpdateAssessmentControl)

-- | The identifier for the specified control set.
updateAssessmentControl_controlSetId :: Lens.Lens' UpdateAssessmentControl Prelude.Text
updateAssessmentControl_controlSetId = Lens.lens (\UpdateAssessmentControl' {controlSetId} -> controlSetId) (\s@UpdateAssessmentControl' {} a -> s {controlSetId = a} :: UpdateAssessmentControl)

-- | The identifier for the specified control.
updateAssessmentControl_controlId :: Lens.Lens' UpdateAssessmentControl Prelude.Text
updateAssessmentControl_controlId = Lens.lens (\UpdateAssessmentControl' {controlId} -> controlId) (\s@UpdateAssessmentControl' {} a -> s {controlId = a} :: UpdateAssessmentControl)

instance Core.AWSRequest UpdateAssessmentControl where
  type
    AWSResponse UpdateAssessmentControl =
      UpdateAssessmentControlResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAssessmentControlResponse'
            Prelude.<$> (x Core..?> "control")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAssessmentControl

instance Prelude.NFData UpdateAssessmentControl

instance Core.ToHeaders UpdateAssessmentControl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateAssessmentControl where
  toJSON UpdateAssessmentControl' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("controlStatus" Core..=) Prelude.<$> controlStatus,
            ("commentBody" Core..=) Prelude.<$> commentBody
          ]
      )

instance Core.ToPath UpdateAssessmentControl where
  toPath UpdateAssessmentControl' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Core.toBS assessmentId,
        "/controlSets/",
        Core.toBS controlSetId,
        "/controls/",
        Core.toBS controlId
      ]

instance Core.ToQuery UpdateAssessmentControl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAssessmentControlResponse' smart constructor.
data UpdateAssessmentControlResponse = UpdateAssessmentControlResponse'
  { -- | The name of the updated control set returned by the
    -- @UpdateAssessmentControl@ API.
    control :: Prelude.Maybe AssessmentControl,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssessmentControlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'control', 'updateAssessmentControlResponse_control' - The name of the updated control set returned by the
-- @UpdateAssessmentControl@ API.
--
-- 'httpStatus', 'updateAssessmentControlResponse_httpStatus' - The response's http status code.
newUpdateAssessmentControlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAssessmentControlResponse
newUpdateAssessmentControlResponse pHttpStatus_ =
  UpdateAssessmentControlResponse'
    { control =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the updated control set returned by the
-- @UpdateAssessmentControl@ API.
updateAssessmentControlResponse_control :: Lens.Lens' UpdateAssessmentControlResponse (Prelude.Maybe AssessmentControl)
updateAssessmentControlResponse_control = Lens.lens (\UpdateAssessmentControlResponse' {control} -> control) (\s@UpdateAssessmentControlResponse' {} a -> s {control = a} :: UpdateAssessmentControlResponse)

-- | The response's http status code.
updateAssessmentControlResponse_httpStatus :: Lens.Lens' UpdateAssessmentControlResponse Prelude.Int
updateAssessmentControlResponse_httpStatus = Lens.lens (\UpdateAssessmentControlResponse' {httpStatus} -> httpStatus) (\s@UpdateAssessmentControlResponse' {} a -> s {httpStatus = a} :: UpdateAssessmentControlResponse)

instance
  Prelude.NFData
    UpdateAssessmentControlResponse
