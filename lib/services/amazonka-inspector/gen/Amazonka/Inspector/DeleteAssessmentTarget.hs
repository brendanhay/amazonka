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
-- Module      : Amazonka.Inspector.DeleteAssessmentTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assessment target that is specified by the ARN of the
-- assessment target.
module Amazonka.Inspector.DeleteAssessmentTarget
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAssessmentTarget' smart constructor.
data DeleteAssessmentTarget = DeleteAssessmentTarget'
  { -- | The ARN that specifies the assessment target that you want to delete.
    assessmentTargetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteAssessmentTarget
newDeleteAssessmentTarget pAssessmentTargetArn_ =
  DeleteAssessmentTarget'
    { assessmentTargetArn =
        pAssessmentTargetArn_
    }

-- | The ARN that specifies the assessment target that you want to delete.
deleteAssessmentTarget_assessmentTargetArn :: Lens.Lens' DeleteAssessmentTarget Prelude.Text
deleteAssessmentTarget_assessmentTargetArn = Lens.lens (\DeleteAssessmentTarget' {assessmentTargetArn} -> assessmentTargetArn) (\s@DeleteAssessmentTarget' {} a -> s {assessmentTargetArn = a} :: DeleteAssessmentTarget)

instance Core.AWSRequest DeleteAssessmentTarget where
  type
    AWSResponse DeleteAssessmentTarget =
      DeleteAssessmentTargetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteAssessmentTargetResponse'

instance Prelude.Hashable DeleteAssessmentTarget where
  hashWithSalt _salt DeleteAssessmentTarget' {..} =
    _salt `Prelude.hashWithSalt` assessmentTargetArn

instance Prelude.NFData DeleteAssessmentTarget where
  rnf DeleteAssessmentTarget' {..} =
    Prelude.rnf assessmentTargetArn

instance Data.ToHeaders DeleteAssessmentTarget where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.DeleteAssessmentTarget" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAssessmentTarget where
  toJSON DeleteAssessmentTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("assessmentTargetArn" Data..= assessmentTargetArn)
          ]
      )

instance Data.ToPath DeleteAssessmentTarget where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAssessmentTarget where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAssessmentTargetResponse' smart constructor.
data DeleteAssessmentTargetResponse = DeleteAssessmentTargetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssessmentTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAssessmentTargetResponse ::
  DeleteAssessmentTargetResponse
newDeleteAssessmentTargetResponse =
  DeleteAssessmentTargetResponse'

instance
  Prelude.NFData
    DeleteAssessmentTargetResponse
  where
  rnf _ = ()
