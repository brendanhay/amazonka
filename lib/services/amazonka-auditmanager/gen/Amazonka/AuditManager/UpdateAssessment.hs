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
-- Module      : Amazonka.AuditManager.UpdateAssessment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Edits an Audit Manager assessment.
module Amazonka.AuditManager.UpdateAssessment
  ( -- * Creating a Request
    UpdateAssessment (..),
    newUpdateAssessment,

    -- * Request Lenses
    updateAssessment_roles,
    updateAssessment_assessmentDescription,
    updateAssessment_assessmentReportsDestination,
    updateAssessment_assessmentName,
    updateAssessment_assessmentId,
    updateAssessment_scope,

    -- * Destructuring the Response
    UpdateAssessmentResponse (..),
    newUpdateAssessmentResponse,

    -- * Response Lenses
    updateAssessmentResponse_assessment,
    updateAssessmentResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAssessment' smart constructor.
data UpdateAssessment = UpdateAssessment'
  { -- | The list of roles for the specified assessment.
    roles :: Prelude.Maybe [Role],
    -- | The description of the specified assessment.
    assessmentDescription :: Prelude.Maybe Prelude.Text,
    -- | The assessment report storage destination for the specified assessment
    -- that is being updated.
    assessmentReportsDestination :: Prelude.Maybe AssessmentReportsDestination,
    -- | The name of the specified assessment to be updated.
    assessmentName :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the specified assessment.
    assessmentId :: Prelude.Text,
    -- | The scope of the specified assessment.
    scope :: Scope
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssessment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roles', 'updateAssessment_roles' - The list of roles for the specified assessment.
--
-- 'assessmentDescription', 'updateAssessment_assessmentDescription' - The description of the specified assessment.
--
-- 'assessmentReportsDestination', 'updateAssessment_assessmentReportsDestination' - The assessment report storage destination for the specified assessment
-- that is being updated.
--
-- 'assessmentName', 'updateAssessment_assessmentName' - The name of the specified assessment to be updated.
--
-- 'assessmentId', 'updateAssessment_assessmentId' - The identifier for the specified assessment.
--
-- 'scope', 'updateAssessment_scope' - The scope of the specified assessment.
newUpdateAssessment ::
  -- | 'assessmentId'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  UpdateAssessment
newUpdateAssessment pAssessmentId_ pScope_ =
  UpdateAssessment'
    { roles = Prelude.Nothing,
      assessmentDescription = Prelude.Nothing,
      assessmentReportsDestination = Prelude.Nothing,
      assessmentName = Prelude.Nothing,
      assessmentId = pAssessmentId_,
      scope = pScope_
    }

-- | The list of roles for the specified assessment.
updateAssessment_roles :: Lens.Lens' UpdateAssessment (Prelude.Maybe [Role])
updateAssessment_roles = Lens.lens (\UpdateAssessment' {roles} -> roles) (\s@UpdateAssessment' {} a -> s {roles = a} :: UpdateAssessment) Prelude.. Lens.mapping Lens.coerced

-- | The description of the specified assessment.
updateAssessment_assessmentDescription :: Lens.Lens' UpdateAssessment (Prelude.Maybe Prelude.Text)
updateAssessment_assessmentDescription = Lens.lens (\UpdateAssessment' {assessmentDescription} -> assessmentDescription) (\s@UpdateAssessment' {} a -> s {assessmentDescription = a} :: UpdateAssessment)

-- | The assessment report storage destination for the specified assessment
-- that is being updated.
updateAssessment_assessmentReportsDestination :: Lens.Lens' UpdateAssessment (Prelude.Maybe AssessmentReportsDestination)
updateAssessment_assessmentReportsDestination = Lens.lens (\UpdateAssessment' {assessmentReportsDestination} -> assessmentReportsDestination) (\s@UpdateAssessment' {} a -> s {assessmentReportsDestination = a} :: UpdateAssessment)

-- | The name of the specified assessment to be updated.
updateAssessment_assessmentName :: Lens.Lens' UpdateAssessment (Prelude.Maybe Prelude.Text)
updateAssessment_assessmentName = Lens.lens (\UpdateAssessment' {assessmentName} -> assessmentName) (\s@UpdateAssessment' {} a -> s {assessmentName = a} :: UpdateAssessment)

-- | The identifier for the specified assessment.
updateAssessment_assessmentId :: Lens.Lens' UpdateAssessment Prelude.Text
updateAssessment_assessmentId = Lens.lens (\UpdateAssessment' {assessmentId} -> assessmentId) (\s@UpdateAssessment' {} a -> s {assessmentId = a} :: UpdateAssessment)

-- | The scope of the specified assessment.
updateAssessment_scope :: Lens.Lens' UpdateAssessment Scope
updateAssessment_scope = Lens.lens (\UpdateAssessment' {scope} -> scope) (\s@UpdateAssessment' {} a -> s {scope = a} :: UpdateAssessment)

instance Core.AWSRequest UpdateAssessment where
  type
    AWSResponse UpdateAssessment =
      UpdateAssessmentResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAssessmentResponse'
            Prelude.<$> (x Core..?> "assessment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAssessment where
  hashWithSalt salt' UpdateAssessment' {..} =
    salt' `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` assessmentId
      `Prelude.hashWithSalt` assessmentName
      `Prelude.hashWithSalt` assessmentReportsDestination
      `Prelude.hashWithSalt` assessmentDescription
      `Prelude.hashWithSalt` roles

instance Prelude.NFData UpdateAssessment where
  rnf UpdateAssessment' {..} =
    Prelude.rnf roles `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf assessmentName
      `Prelude.seq` Prelude.rnf assessmentReportsDestination
      `Prelude.seq` Prelude.rnf assessmentDescription

instance Core.ToHeaders UpdateAssessment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateAssessment where
  toJSON UpdateAssessment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("roles" Core..=) Prelude.<$> roles,
            ("assessmentDescription" Core..=)
              Prelude.<$> assessmentDescription,
            ("assessmentReportsDestination" Core..=)
              Prelude.<$> assessmentReportsDestination,
            ("assessmentName" Core..=)
              Prelude.<$> assessmentName,
            Prelude.Just ("scope" Core..= scope)
          ]
      )

instance Core.ToPath UpdateAssessment where
  toPath UpdateAssessment' {..} =
    Prelude.mconcat
      ["/assessments/", Core.toBS assessmentId]

instance Core.ToQuery UpdateAssessment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAssessmentResponse' smart constructor.
data UpdateAssessmentResponse = UpdateAssessmentResponse'
  { -- | The response object (name of the updated assessment) for the
    -- @UpdateAssessmentRequest@ API.
    assessment :: Prelude.Maybe Assessment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssessmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessment', 'updateAssessmentResponse_assessment' - The response object (name of the updated assessment) for the
-- @UpdateAssessmentRequest@ API.
--
-- 'httpStatus', 'updateAssessmentResponse_httpStatus' - The response's http status code.
newUpdateAssessmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAssessmentResponse
newUpdateAssessmentResponse pHttpStatus_ =
  UpdateAssessmentResponse'
    { assessment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The response object (name of the updated assessment) for the
-- @UpdateAssessmentRequest@ API.
updateAssessmentResponse_assessment :: Lens.Lens' UpdateAssessmentResponse (Prelude.Maybe Assessment)
updateAssessmentResponse_assessment = Lens.lens (\UpdateAssessmentResponse' {assessment} -> assessment) (\s@UpdateAssessmentResponse' {} a -> s {assessment = a} :: UpdateAssessmentResponse)

-- | The response's http status code.
updateAssessmentResponse_httpStatus :: Lens.Lens' UpdateAssessmentResponse Prelude.Int
updateAssessmentResponse_httpStatus = Lens.lens (\UpdateAssessmentResponse' {httpStatus} -> httpStatus) (\s@UpdateAssessmentResponse' {} a -> s {httpStatus = a} :: UpdateAssessmentResponse)

instance Prelude.NFData UpdateAssessmentResponse where
  rnf UpdateAssessmentResponse' {..} =
    Prelude.rnf assessment
      `Prelude.seq` Prelude.rnf httpStatus
