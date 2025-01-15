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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a control within an assessment in Audit Manager.
module Amazonka.AuditManager.UpdateAssessmentControl
  ( -- * Creating a Request
    UpdateAssessmentControl (..),
    newUpdateAssessmentControl,

    -- * Request Lenses
    updateAssessmentControl_commentBody,
    updateAssessmentControl_controlStatus,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAssessmentControl' smart constructor.
data UpdateAssessmentControl = UpdateAssessmentControl'
  { -- | The comment body text for the control.
    commentBody :: Prelude.Maybe Prelude.Text,
    -- | The status of the control.
    controlStatus :: Prelude.Maybe ControlStatus,
    -- | The unique identifier for the assessment.
    assessmentId :: Prelude.Text,
    -- | The unique identifier for the control set.
    controlSetId :: Prelude.Text,
    -- | The unique identifier for the control.
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
-- 'commentBody', 'updateAssessmentControl_commentBody' - The comment body text for the control.
--
-- 'controlStatus', 'updateAssessmentControl_controlStatus' - The status of the control.
--
-- 'assessmentId', 'updateAssessmentControl_assessmentId' - The unique identifier for the assessment.
--
-- 'controlSetId', 'updateAssessmentControl_controlSetId' - The unique identifier for the control set.
--
-- 'controlId', 'updateAssessmentControl_controlId' - The unique identifier for the control.
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
      { commentBody =
          Prelude.Nothing,
        controlStatus = Prelude.Nothing,
        assessmentId = pAssessmentId_,
        controlSetId = pControlSetId_,
        controlId = pControlId_
      }

-- | The comment body text for the control.
updateAssessmentControl_commentBody :: Lens.Lens' UpdateAssessmentControl (Prelude.Maybe Prelude.Text)
updateAssessmentControl_commentBody = Lens.lens (\UpdateAssessmentControl' {commentBody} -> commentBody) (\s@UpdateAssessmentControl' {} a -> s {commentBody = a} :: UpdateAssessmentControl)

-- | The status of the control.
updateAssessmentControl_controlStatus :: Lens.Lens' UpdateAssessmentControl (Prelude.Maybe ControlStatus)
updateAssessmentControl_controlStatus = Lens.lens (\UpdateAssessmentControl' {controlStatus} -> controlStatus) (\s@UpdateAssessmentControl' {} a -> s {controlStatus = a} :: UpdateAssessmentControl)

-- | The unique identifier for the assessment.
updateAssessmentControl_assessmentId :: Lens.Lens' UpdateAssessmentControl Prelude.Text
updateAssessmentControl_assessmentId = Lens.lens (\UpdateAssessmentControl' {assessmentId} -> assessmentId) (\s@UpdateAssessmentControl' {} a -> s {assessmentId = a} :: UpdateAssessmentControl)

-- | The unique identifier for the control set.
updateAssessmentControl_controlSetId :: Lens.Lens' UpdateAssessmentControl Prelude.Text
updateAssessmentControl_controlSetId = Lens.lens (\UpdateAssessmentControl' {controlSetId} -> controlSetId) (\s@UpdateAssessmentControl' {} a -> s {controlSetId = a} :: UpdateAssessmentControl)

-- | The unique identifier for the control.
updateAssessmentControl_controlId :: Lens.Lens' UpdateAssessmentControl Prelude.Text
updateAssessmentControl_controlId = Lens.lens (\UpdateAssessmentControl' {controlId} -> controlId) (\s@UpdateAssessmentControl' {} a -> s {controlId = a} :: UpdateAssessmentControl)

instance Core.AWSRequest UpdateAssessmentControl where
  type
    AWSResponse UpdateAssessmentControl =
      UpdateAssessmentControlResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAssessmentControlResponse'
            Prelude.<$> (x Data..?> "control")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAssessmentControl where
  hashWithSalt _salt UpdateAssessmentControl' {..} =
    _salt
      `Prelude.hashWithSalt` commentBody
      `Prelude.hashWithSalt` controlStatus
      `Prelude.hashWithSalt` assessmentId
      `Prelude.hashWithSalt` controlSetId
      `Prelude.hashWithSalt` controlId

instance Prelude.NFData UpdateAssessmentControl where
  rnf UpdateAssessmentControl' {..} =
    Prelude.rnf commentBody `Prelude.seq`
      Prelude.rnf controlStatus `Prelude.seq`
        Prelude.rnf assessmentId `Prelude.seq`
          Prelude.rnf controlSetId `Prelude.seq`
            Prelude.rnf controlId

instance Data.ToHeaders UpdateAssessmentControl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAssessmentControl where
  toJSON UpdateAssessmentControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("commentBody" Data..=) Prelude.<$> commentBody,
            ("controlStatus" Data..=) Prelude.<$> controlStatus
          ]
      )

instance Data.ToPath UpdateAssessmentControl where
  toPath UpdateAssessmentControl' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Data.toBS assessmentId,
        "/controlSets/",
        Data.toBS controlSetId,
        "/controls/",
        Data.toBS controlId
      ]

instance Data.ToQuery UpdateAssessmentControl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAssessmentControlResponse' smart constructor.
data UpdateAssessmentControlResponse = UpdateAssessmentControlResponse'
  { -- | The name of the updated control set that the @UpdateAssessmentControl@
    -- API returned.
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
-- 'control', 'updateAssessmentControlResponse_control' - The name of the updated control set that the @UpdateAssessmentControl@
-- API returned.
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

-- | The name of the updated control set that the @UpdateAssessmentControl@
-- API returned.
updateAssessmentControlResponse_control :: Lens.Lens' UpdateAssessmentControlResponse (Prelude.Maybe AssessmentControl)
updateAssessmentControlResponse_control = Lens.lens (\UpdateAssessmentControlResponse' {control} -> control) (\s@UpdateAssessmentControlResponse' {} a -> s {control = a} :: UpdateAssessmentControlResponse)

-- | The response's http status code.
updateAssessmentControlResponse_httpStatus :: Lens.Lens' UpdateAssessmentControlResponse Prelude.Int
updateAssessmentControlResponse_httpStatus = Lens.lens (\UpdateAssessmentControlResponse' {httpStatus} -> httpStatus) (\s@UpdateAssessmentControlResponse' {} a -> s {httpStatus = a} :: UpdateAssessmentControlResponse)

instance
  Prelude.NFData
    UpdateAssessmentControlResponse
  where
  rnf UpdateAssessmentControlResponse' {..} =
    Prelude.rnf control `Prelude.seq`
      Prelude.rnf httpStatus
