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
-- Module      : Amazonka.Inspector.DeleteAssessmentRun
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assessment run that is specified by the ARN of the
-- assessment run.
module Amazonka.Inspector.DeleteAssessmentRun
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAssessmentRun' smart constructor.
data DeleteAssessmentRun = DeleteAssessmentRun'
  { -- | The ARN that specifies the assessment run that you want to delete.
    assessmentRunArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteAssessmentRun
newDeleteAssessmentRun pAssessmentRunArn_ =
  DeleteAssessmentRun'
    { assessmentRunArn =
        pAssessmentRunArn_
    }

-- | The ARN that specifies the assessment run that you want to delete.
deleteAssessmentRun_assessmentRunArn :: Lens.Lens' DeleteAssessmentRun Prelude.Text
deleteAssessmentRun_assessmentRunArn = Lens.lens (\DeleteAssessmentRun' {assessmentRunArn} -> assessmentRunArn) (\s@DeleteAssessmentRun' {} a -> s {assessmentRunArn = a} :: DeleteAssessmentRun)

instance Core.AWSRequest DeleteAssessmentRun where
  type
    AWSResponse DeleteAssessmentRun =
      DeleteAssessmentRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteAssessmentRunResponse'

instance Prelude.Hashable DeleteAssessmentRun where
  hashWithSalt _salt DeleteAssessmentRun' {..} =
    _salt `Prelude.hashWithSalt` assessmentRunArn

instance Prelude.NFData DeleteAssessmentRun where
  rnf DeleteAssessmentRun' {..} =
    Prelude.rnf assessmentRunArn

instance Core.ToHeaders DeleteAssessmentRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.DeleteAssessmentRun" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteAssessmentRun where
  toJSON DeleteAssessmentRun' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("assessmentRunArn" Core..= assessmentRunArn)
          ]
      )

instance Core.ToPath DeleteAssessmentRun where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteAssessmentRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAssessmentRunResponse' smart constructor.
data DeleteAssessmentRunResponse = DeleteAssessmentRunResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssessmentRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAssessmentRunResponse ::
  DeleteAssessmentRunResponse
newDeleteAssessmentRunResponse =
  DeleteAssessmentRunResponse'

instance Prelude.NFData DeleteAssessmentRunResponse where
  rnf _ = ()
