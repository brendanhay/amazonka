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
-- Module      : Network.AWS.Inspector.DeleteAssessmentRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assessment run that is specified by the ARN of the
-- assessment run.
module Network.AWS.Inspector.DeleteAssessmentRun
  ( -- * Creating a Request
    DeleteAssessmentRun (..),
    newDeleteAssessmentRun,

    -- * Request Lenses
    deleteAssessmentRun_assessmentRunArn,

    -- * Destructuring the Response
    DeleteAssessmentRunResponse (..),
    newDeleteAssessmentRunResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAssessmentRun' smart constructor.
data DeleteAssessmentRun = DeleteAssessmentRun'
  { -- | The ARN that specifies the assessment run that you want to delete.
    assessmentRunArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAssessmentRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentRunArn', 'deleteAssessmentRun_assessmentRunArn' - The ARN that specifies the assessment run that you want to delete.
newDeleteAssessmentRun ::
  -- | 'assessmentRunArn'
  Core.Text ->
  DeleteAssessmentRun
newDeleteAssessmentRun pAssessmentRunArn_ =
  DeleteAssessmentRun'
    { assessmentRunArn =
        pAssessmentRunArn_
    }

-- | The ARN that specifies the assessment run that you want to delete.
deleteAssessmentRun_assessmentRunArn :: Lens.Lens' DeleteAssessmentRun Core.Text
deleteAssessmentRun_assessmentRunArn = Lens.lens (\DeleteAssessmentRun' {assessmentRunArn} -> assessmentRunArn) (\s@DeleteAssessmentRun' {} a -> s {assessmentRunArn = a} :: DeleteAssessmentRun)

instance Core.AWSRequest DeleteAssessmentRun where
  type
    AWSResponse DeleteAssessmentRun =
      DeleteAssessmentRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteAssessmentRunResponse'

instance Core.Hashable DeleteAssessmentRun

instance Core.NFData DeleteAssessmentRun

instance Core.ToHeaders DeleteAssessmentRun where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.DeleteAssessmentRun" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteAssessmentRun where
  toJSON DeleteAssessmentRun' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("assessmentRunArn" Core..= assessmentRunArn)
          ]
      )

instance Core.ToPath DeleteAssessmentRun where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAssessmentRun where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAssessmentRunResponse' smart constructor.
data DeleteAssessmentRunResponse = DeleteAssessmentRunResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAssessmentRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAssessmentRunResponse ::
  DeleteAssessmentRunResponse
newDeleteAssessmentRunResponse =
  DeleteAssessmentRunResponse'

instance Core.NFData DeleteAssessmentRunResponse
