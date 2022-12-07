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
-- Module      : Amazonka.AuditManager.CreateAssessment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an assessment in Audit Manager.
module Amazonka.AuditManager.CreateAssessment
  ( -- * Creating a Request
    CreateAssessment (..),
    newCreateAssessment,

    -- * Request Lenses
    createAssessment_tags,
    createAssessment_description,
    createAssessment_name,
    createAssessment_assessmentReportsDestination,
    createAssessment_scope,
    createAssessment_roles,
    createAssessment_frameworkId,

    -- * Destructuring the Response
    CreateAssessmentResponse (..),
    newCreateAssessmentResponse,

    -- * Response Lenses
    createAssessmentResponse_assessment,
    createAssessmentResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAssessment' smart constructor.
data CreateAssessment = CreateAssessment'
  { -- | The tags that are associated with the assessment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The optional description of the assessment to be created.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the assessment to be created.
    name :: Prelude.Text,
    -- | The assessment report storage destination for the assessment that\'s
    -- being created.
    assessmentReportsDestination :: AssessmentReportsDestination,
    scope :: Scope,
    -- | The list of roles for the assessment.
    roles :: [Role],
    -- | The identifier for the framework that the assessment will be created
    -- from.
    frameworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssessment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createAssessment_tags' - The tags that are associated with the assessment.
--
-- 'description', 'createAssessment_description' - The optional description of the assessment to be created.
--
-- 'name', 'createAssessment_name' - The name of the assessment to be created.
--
-- 'assessmentReportsDestination', 'createAssessment_assessmentReportsDestination' - The assessment report storage destination for the assessment that\'s
-- being created.
--
-- 'scope', 'createAssessment_scope' - Undocumented member.
--
-- 'roles', 'createAssessment_roles' - The list of roles for the assessment.
--
-- 'frameworkId', 'createAssessment_frameworkId' - The identifier for the framework that the assessment will be created
-- from.
newCreateAssessment ::
  -- | 'name'
  Prelude.Text ->
  -- | 'assessmentReportsDestination'
  AssessmentReportsDestination ->
  -- | 'scope'
  Scope ->
  -- | 'frameworkId'
  Prelude.Text ->
  CreateAssessment
newCreateAssessment
  pName_
  pAssessmentReportsDestination_
  pScope_
  pFrameworkId_ =
    CreateAssessment'
      { tags = Prelude.Nothing,
        description = Prelude.Nothing,
        name = pName_,
        assessmentReportsDestination =
          pAssessmentReportsDestination_,
        scope = pScope_,
        roles = Prelude.mempty,
        frameworkId = pFrameworkId_
      }

-- | The tags that are associated with the assessment.
createAssessment_tags :: Lens.Lens' CreateAssessment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAssessment_tags = Lens.lens (\CreateAssessment' {tags} -> tags) (\s@CreateAssessment' {} a -> s {tags = a} :: CreateAssessment) Prelude.. Lens.mapping Lens.coerced

-- | The optional description of the assessment to be created.
createAssessment_description :: Lens.Lens' CreateAssessment (Prelude.Maybe Prelude.Text)
createAssessment_description = Lens.lens (\CreateAssessment' {description} -> description) (\s@CreateAssessment' {} a -> s {description = a} :: CreateAssessment)

-- | The name of the assessment to be created.
createAssessment_name :: Lens.Lens' CreateAssessment Prelude.Text
createAssessment_name = Lens.lens (\CreateAssessment' {name} -> name) (\s@CreateAssessment' {} a -> s {name = a} :: CreateAssessment)

-- | The assessment report storage destination for the assessment that\'s
-- being created.
createAssessment_assessmentReportsDestination :: Lens.Lens' CreateAssessment AssessmentReportsDestination
createAssessment_assessmentReportsDestination = Lens.lens (\CreateAssessment' {assessmentReportsDestination} -> assessmentReportsDestination) (\s@CreateAssessment' {} a -> s {assessmentReportsDestination = a} :: CreateAssessment)

-- | Undocumented member.
createAssessment_scope :: Lens.Lens' CreateAssessment Scope
createAssessment_scope = Lens.lens (\CreateAssessment' {scope} -> scope) (\s@CreateAssessment' {} a -> s {scope = a} :: CreateAssessment)

-- | The list of roles for the assessment.
createAssessment_roles :: Lens.Lens' CreateAssessment [Role]
createAssessment_roles = Lens.lens (\CreateAssessment' {roles} -> roles) (\s@CreateAssessment' {} a -> s {roles = a} :: CreateAssessment) Prelude.. Lens.coerced

-- | The identifier for the framework that the assessment will be created
-- from.
createAssessment_frameworkId :: Lens.Lens' CreateAssessment Prelude.Text
createAssessment_frameworkId = Lens.lens (\CreateAssessment' {frameworkId} -> frameworkId) (\s@CreateAssessment' {} a -> s {frameworkId = a} :: CreateAssessment)

instance Core.AWSRequest CreateAssessment where
  type
    AWSResponse CreateAssessment =
      CreateAssessmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAssessmentResponse'
            Prelude.<$> (x Data..?> "assessment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAssessment where
  hashWithSalt _salt CreateAssessment' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` assessmentReportsDestination
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` roles
      `Prelude.hashWithSalt` frameworkId

instance Prelude.NFData CreateAssessment where
  rnf CreateAssessment' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf assessmentReportsDestination
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf roles
      `Prelude.seq` Prelude.rnf frameworkId

instance Data.ToHeaders CreateAssessment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAssessment where
  toJSON CreateAssessment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ( "assessmentReportsDestination"
                  Data..= assessmentReportsDestination
              ),
            Prelude.Just ("scope" Data..= scope),
            Prelude.Just ("roles" Data..= roles),
            Prelude.Just ("frameworkId" Data..= frameworkId)
          ]
      )

instance Data.ToPath CreateAssessment where
  toPath = Prelude.const "/assessments"

instance Data.ToQuery CreateAssessment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAssessmentResponse' smart constructor.
data CreateAssessmentResponse = CreateAssessmentResponse'
  { assessment :: Prelude.Maybe Assessment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssessmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessment', 'createAssessmentResponse_assessment' - Undocumented member.
--
-- 'httpStatus', 'createAssessmentResponse_httpStatus' - The response's http status code.
newCreateAssessmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAssessmentResponse
newCreateAssessmentResponse pHttpStatus_ =
  CreateAssessmentResponse'
    { assessment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createAssessmentResponse_assessment :: Lens.Lens' CreateAssessmentResponse (Prelude.Maybe Assessment)
createAssessmentResponse_assessment = Lens.lens (\CreateAssessmentResponse' {assessment} -> assessment) (\s@CreateAssessmentResponse' {} a -> s {assessment = a} :: CreateAssessmentResponse)

-- | The response's http status code.
createAssessmentResponse_httpStatus :: Lens.Lens' CreateAssessmentResponse Prelude.Int
createAssessmentResponse_httpStatus = Lens.lens (\CreateAssessmentResponse' {httpStatus} -> httpStatus) (\s@CreateAssessmentResponse' {} a -> s {httpStatus = a} :: CreateAssessmentResponse)

instance Prelude.NFData CreateAssessmentResponse where
  rnf CreateAssessmentResponse' {..} =
    Prelude.rnf assessment
      `Prelude.seq` Prelude.rnf httpStatus
