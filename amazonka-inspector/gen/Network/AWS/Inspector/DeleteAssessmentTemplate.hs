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
-- Module      : Network.AWS.Inspector.DeleteAssessmentTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assessment template that is specified by the ARN of the
-- assessment template.
module Network.AWS.Inspector.DeleteAssessmentTemplate
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

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAssessmentTemplate' smart constructor.
data DeleteAssessmentTemplate = DeleteAssessmentTemplate'
  { -- | The ARN that specifies the assessment template that you want to delete.
    assessmentTemplateArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteAssessmentTemplate
newDeleteAssessmentTemplate pAssessmentTemplateArn_ =
  DeleteAssessmentTemplate'
    { assessmentTemplateArn =
        pAssessmentTemplateArn_
    }

-- | The ARN that specifies the assessment template that you want to delete.
deleteAssessmentTemplate_assessmentTemplateArn :: Lens.Lens' DeleteAssessmentTemplate Core.Text
deleteAssessmentTemplate_assessmentTemplateArn = Lens.lens (\DeleteAssessmentTemplate' {assessmentTemplateArn} -> assessmentTemplateArn) (\s@DeleteAssessmentTemplate' {} a -> s {assessmentTemplateArn = a} :: DeleteAssessmentTemplate)

instance Core.AWSRequest DeleteAssessmentTemplate where
  type
    AWSResponse DeleteAssessmentTemplate =
      DeleteAssessmentTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteAssessmentTemplateResponse'

instance Core.Hashable DeleteAssessmentTemplate

instance Core.NFData DeleteAssessmentTemplate

instance Core.ToHeaders DeleteAssessmentTemplate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.DeleteAssessmentTemplate" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteAssessmentTemplate where
  toJSON DeleteAssessmentTemplate' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "assessmentTemplateArn"
                  Core..= assessmentTemplateArn
              )
          ]
      )

instance Core.ToPath DeleteAssessmentTemplate where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAssessmentTemplate where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAssessmentTemplateResponse' smart constructor.
data DeleteAssessmentTemplateResponse = DeleteAssessmentTemplateResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAssessmentTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAssessmentTemplateResponse ::
  DeleteAssessmentTemplateResponse
newDeleteAssessmentTemplateResponse =
  DeleteAssessmentTemplateResponse'

instance Core.NFData DeleteAssessmentTemplateResponse
