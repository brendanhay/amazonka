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
-- Module      : Network.AWS.Inspector.DeleteAssessmentTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assessment target that is specified by the ARN of the
-- assessment target.
module Network.AWS.Inspector.DeleteAssessmentTarget
  ( -- * Creating a Request
    DeleteAssessmentTarget (..),
    newDeleteAssessmentTarget,

    -- * Request Lenses
    deleteAssessmentTarget_assessmentTargetArn,

    -- * Destructuring the Response
    DeleteAssessmentTargetResponse (..),
    newDeleteAssessmentTargetResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAssessmentTarget' smart constructor.
data DeleteAssessmentTarget = DeleteAssessmentTarget'
  { -- | The ARN that specifies the assessment target that you want to delete.
    assessmentTargetArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAssessmentTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentTargetArn', 'deleteAssessmentTarget_assessmentTargetArn' - The ARN that specifies the assessment target that you want to delete.
newDeleteAssessmentTarget ::
  -- | 'assessmentTargetArn'
  Core.Text ->
  DeleteAssessmentTarget
newDeleteAssessmentTarget pAssessmentTargetArn_ =
  DeleteAssessmentTarget'
    { assessmentTargetArn =
        pAssessmentTargetArn_
    }

-- | The ARN that specifies the assessment target that you want to delete.
deleteAssessmentTarget_assessmentTargetArn :: Lens.Lens' DeleteAssessmentTarget Core.Text
deleteAssessmentTarget_assessmentTargetArn = Lens.lens (\DeleteAssessmentTarget' {assessmentTargetArn} -> assessmentTargetArn) (\s@DeleteAssessmentTarget' {} a -> s {assessmentTargetArn = a} :: DeleteAssessmentTarget)

instance Core.AWSRequest DeleteAssessmentTarget where
  type
    AWSResponse DeleteAssessmentTarget =
      DeleteAssessmentTargetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteAssessmentTargetResponse'

instance Core.Hashable DeleteAssessmentTarget

instance Core.NFData DeleteAssessmentTarget

instance Core.ToHeaders DeleteAssessmentTarget where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.DeleteAssessmentTarget" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteAssessmentTarget where
  toJSON DeleteAssessmentTarget' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("assessmentTargetArn" Core..= assessmentTargetArn)
          ]
      )

instance Core.ToPath DeleteAssessmentTarget where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAssessmentTarget where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAssessmentTargetResponse' smart constructor.
data DeleteAssessmentTargetResponse = DeleteAssessmentTargetResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAssessmentTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAssessmentTargetResponse ::
  DeleteAssessmentTargetResponse
newDeleteAssessmentTargetResponse =
  DeleteAssessmentTargetResponse'

instance Core.NFData DeleteAssessmentTargetResponse
