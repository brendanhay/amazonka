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
-- Module      : Amazonka.Inspector.DeleteAssessmentTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assessment template that is specified by the ARN of the
-- assessment template.
module Amazonka.Inspector.DeleteAssessmentTemplate
  ( -- * Creating a Request
    DeleteAssessmentTemplate (..),
    newDeleteAssessmentTemplate,

    -- * Request Lenses
    deleteAssessmentTemplate_assessmentTemplateArn,

    -- * Destructuring the Response
    DeleteAssessmentTemplateResponse (..),
    newDeleteAssessmentTemplateResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAssessmentTemplate' smart constructor.
data DeleteAssessmentTemplate = DeleteAssessmentTemplate'
  { -- | The ARN that specifies the assessment template that you want to delete.
    assessmentTemplateArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssessmentTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentTemplateArn', 'deleteAssessmentTemplate_assessmentTemplateArn' - The ARN that specifies the assessment template that you want to delete.
newDeleteAssessmentTemplate ::
  -- | 'assessmentTemplateArn'
  Prelude.Text ->
  DeleteAssessmentTemplate
newDeleteAssessmentTemplate pAssessmentTemplateArn_ =
  DeleteAssessmentTemplate'
    { assessmentTemplateArn =
        pAssessmentTemplateArn_
    }

-- | The ARN that specifies the assessment template that you want to delete.
deleteAssessmentTemplate_assessmentTemplateArn :: Lens.Lens' DeleteAssessmentTemplate Prelude.Text
deleteAssessmentTemplate_assessmentTemplateArn = Lens.lens (\DeleteAssessmentTemplate' {assessmentTemplateArn} -> assessmentTemplateArn) (\s@DeleteAssessmentTemplate' {} a -> s {assessmentTemplateArn = a} :: DeleteAssessmentTemplate)

instance Core.AWSRequest DeleteAssessmentTemplate where
  type
    AWSResponse DeleteAssessmentTemplate =
      DeleteAssessmentTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteAssessmentTemplateResponse'

instance Prelude.Hashable DeleteAssessmentTemplate where
  hashWithSalt _salt DeleteAssessmentTemplate' {..} =
    _salt `Prelude.hashWithSalt` assessmentTemplateArn

instance Prelude.NFData DeleteAssessmentTemplate where
  rnf DeleteAssessmentTemplate' {..} =
    Prelude.rnf assessmentTemplateArn

instance Data.ToHeaders DeleteAssessmentTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.DeleteAssessmentTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAssessmentTemplate where
  toJSON DeleteAssessmentTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "assessmentTemplateArn"
                  Data..= assessmentTemplateArn
              )
          ]
      )

instance Data.ToPath DeleteAssessmentTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAssessmentTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAssessmentTemplateResponse' smart constructor.
data DeleteAssessmentTemplateResponse = DeleteAssessmentTemplateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssessmentTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAssessmentTemplateResponse ::
  DeleteAssessmentTemplateResponse
newDeleteAssessmentTemplateResponse =
  DeleteAssessmentTemplateResponse'

instance
  Prelude.NFData
    DeleteAssessmentTemplateResponse
  where
  rnf _ = ()
