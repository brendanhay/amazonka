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
-- Module      : Amazonka.EMRContainers.DeleteJobTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a job template. Job template stores values of StartJobRun API
-- request in a template and can be used to start a job run. Job template
-- allows two use cases: avoid repeating recurring StartJobRun API request
-- values, enforcing certain values in StartJobRun API request.
module Amazonka.EMRContainers.DeleteJobTemplate
  ( -- * Creating a Request
    DeleteJobTemplate (..),
    newDeleteJobTemplate,

    -- * Request Lenses
    deleteJobTemplate_id,

    -- * Destructuring the Response
    DeleteJobTemplateResponse (..),
    newDeleteJobTemplateResponse,

    -- * Response Lenses
    deleteJobTemplateResponse_id,
    deleteJobTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRContainers.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteJobTemplate' smart constructor.
data DeleteJobTemplate = DeleteJobTemplate'
  { -- | The ID of the job template that will be deleted.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteJobTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteJobTemplate_id' - The ID of the job template that will be deleted.
newDeleteJobTemplate ::
  -- | 'id'
  Prelude.Text ->
  DeleteJobTemplate
newDeleteJobTemplate pId_ =
  DeleteJobTemplate' {id = pId_}

-- | The ID of the job template that will be deleted.
deleteJobTemplate_id :: Lens.Lens' DeleteJobTemplate Prelude.Text
deleteJobTemplate_id = Lens.lens (\DeleteJobTemplate' {id} -> id) (\s@DeleteJobTemplate' {} a -> s {id = a} :: DeleteJobTemplate)

instance Core.AWSRequest DeleteJobTemplate where
  type
    AWSResponse DeleteJobTemplate =
      DeleteJobTemplateResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteJobTemplateResponse'
            Prelude.<$> (x Data..?> "id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteJobTemplate where
  hashWithSalt _salt DeleteJobTemplate' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteJobTemplate where
  rnf DeleteJobTemplate' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteJobTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteJobTemplate where
  toPath DeleteJobTemplate' {..} =
    Prelude.mconcat ["/jobtemplates/", Data.toBS id]

instance Data.ToQuery DeleteJobTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteJobTemplateResponse' smart constructor.
data DeleteJobTemplateResponse = DeleteJobTemplateResponse'
  { -- | This output contains the ID of the job template that was deleted.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteJobTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteJobTemplateResponse_id' - This output contains the ID of the job template that was deleted.
--
-- 'httpStatus', 'deleteJobTemplateResponse_httpStatus' - The response's http status code.
newDeleteJobTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteJobTemplateResponse
newDeleteJobTemplateResponse pHttpStatus_ =
  DeleteJobTemplateResponse'
    { id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This output contains the ID of the job template that was deleted.
deleteJobTemplateResponse_id :: Lens.Lens' DeleteJobTemplateResponse (Prelude.Maybe Prelude.Text)
deleteJobTemplateResponse_id = Lens.lens (\DeleteJobTemplateResponse' {id} -> id) (\s@DeleteJobTemplateResponse' {} a -> s {id = a} :: DeleteJobTemplateResponse)

-- | The response's http status code.
deleteJobTemplateResponse_httpStatus :: Lens.Lens' DeleteJobTemplateResponse Prelude.Int
deleteJobTemplateResponse_httpStatus = Lens.lens (\DeleteJobTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteJobTemplateResponse' {} a -> s {httpStatus = a} :: DeleteJobTemplateResponse)

instance Prelude.NFData DeleteJobTemplateResponse where
  rnf DeleteJobTemplateResponse' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf httpStatus
