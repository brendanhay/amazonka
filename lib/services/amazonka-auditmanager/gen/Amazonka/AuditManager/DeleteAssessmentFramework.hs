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
-- Module      : Amazonka.AuditManager.DeleteAssessmentFramework
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom framework in Audit Manager.
module Amazonka.AuditManager.DeleteAssessmentFramework
  ( -- * Creating a Request
    DeleteAssessmentFramework (..),
    newDeleteAssessmentFramework,

    -- * Request Lenses
    deleteAssessmentFramework_frameworkId,

    -- * Destructuring the Response
    DeleteAssessmentFrameworkResponse (..),
    newDeleteAssessmentFrameworkResponse,

    -- * Response Lenses
    deleteAssessmentFrameworkResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAssessmentFramework' smart constructor.
data DeleteAssessmentFramework = DeleteAssessmentFramework'
  { -- | The identifier for the custom framework.
    frameworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssessmentFramework' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frameworkId', 'deleteAssessmentFramework_frameworkId' - The identifier for the custom framework.
newDeleteAssessmentFramework ::
  -- | 'frameworkId'
  Prelude.Text ->
  DeleteAssessmentFramework
newDeleteAssessmentFramework pFrameworkId_ =
  DeleteAssessmentFramework'
    { frameworkId =
        pFrameworkId_
    }

-- | The identifier for the custom framework.
deleteAssessmentFramework_frameworkId :: Lens.Lens' DeleteAssessmentFramework Prelude.Text
deleteAssessmentFramework_frameworkId = Lens.lens (\DeleteAssessmentFramework' {frameworkId} -> frameworkId) (\s@DeleteAssessmentFramework' {} a -> s {frameworkId = a} :: DeleteAssessmentFramework)

instance Core.AWSRequest DeleteAssessmentFramework where
  type
    AWSResponse DeleteAssessmentFramework =
      DeleteAssessmentFrameworkResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAssessmentFrameworkResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAssessmentFramework where
  hashWithSalt _salt DeleteAssessmentFramework' {..} =
    _salt `Prelude.hashWithSalt` frameworkId

instance Prelude.NFData DeleteAssessmentFramework where
  rnf DeleteAssessmentFramework' {..} =
    Prelude.rnf frameworkId

instance Data.ToHeaders DeleteAssessmentFramework where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteAssessmentFramework where
  toPath DeleteAssessmentFramework' {..} =
    Prelude.mconcat
      ["/assessmentFrameworks/", Data.toBS frameworkId]

instance Data.ToQuery DeleteAssessmentFramework where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAssessmentFrameworkResponse' smart constructor.
data DeleteAssessmentFrameworkResponse = DeleteAssessmentFrameworkResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssessmentFrameworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAssessmentFrameworkResponse_httpStatus' - The response's http status code.
newDeleteAssessmentFrameworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAssessmentFrameworkResponse
newDeleteAssessmentFrameworkResponse pHttpStatus_ =
  DeleteAssessmentFrameworkResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAssessmentFrameworkResponse_httpStatus :: Lens.Lens' DeleteAssessmentFrameworkResponse Prelude.Int
deleteAssessmentFrameworkResponse_httpStatus = Lens.lens (\DeleteAssessmentFrameworkResponse' {httpStatus} -> httpStatus) (\s@DeleteAssessmentFrameworkResponse' {} a -> s {httpStatus = a} :: DeleteAssessmentFrameworkResponse)

instance
  Prelude.NFData
    DeleteAssessmentFrameworkResponse
  where
  rnf DeleteAssessmentFrameworkResponse' {..} =
    Prelude.rnf httpStatus
