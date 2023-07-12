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
-- Module      : Amazonka.IoT.DeleteJobTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified job template.
module Amazonka.IoT.DeleteJobTemplate
  ( -- * Creating a Request
    DeleteJobTemplate (..),
    newDeleteJobTemplate,

    -- * Request Lenses
    deleteJobTemplate_jobTemplateId,

    -- * Destructuring the Response
    DeleteJobTemplateResponse (..),
    newDeleteJobTemplateResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteJobTemplate' smart constructor.
data DeleteJobTemplate = DeleteJobTemplate'
  { -- | The unique identifier of the job template to delete.
    jobTemplateId :: Prelude.Text
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
-- 'jobTemplateId', 'deleteJobTemplate_jobTemplateId' - The unique identifier of the job template to delete.
newDeleteJobTemplate ::
  -- | 'jobTemplateId'
  Prelude.Text ->
  DeleteJobTemplate
newDeleteJobTemplate pJobTemplateId_ =
  DeleteJobTemplate' {jobTemplateId = pJobTemplateId_}

-- | The unique identifier of the job template to delete.
deleteJobTemplate_jobTemplateId :: Lens.Lens' DeleteJobTemplate Prelude.Text
deleteJobTemplate_jobTemplateId = Lens.lens (\DeleteJobTemplate' {jobTemplateId} -> jobTemplateId) (\s@DeleteJobTemplate' {} a -> s {jobTemplateId = a} :: DeleteJobTemplate)

instance Core.AWSRequest DeleteJobTemplate where
  type
    AWSResponse DeleteJobTemplate =
      DeleteJobTemplateResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteJobTemplateResponse'

instance Prelude.Hashable DeleteJobTemplate where
  hashWithSalt _salt DeleteJobTemplate' {..} =
    _salt `Prelude.hashWithSalt` jobTemplateId

instance Prelude.NFData DeleteJobTemplate where
  rnf DeleteJobTemplate' {..} =
    Prelude.rnf jobTemplateId

instance Data.ToHeaders DeleteJobTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteJobTemplate where
  toPath DeleteJobTemplate' {..} =
    Prelude.mconcat
      ["/job-templates/", Data.toBS jobTemplateId]

instance Data.ToQuery DeleteJobTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteJobTemplateResponse' smart constructor.
data DeleteJobTemplateResponse = DeleteJobTemplateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteJobTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteJobTemplateResponse ::
  DeleteJobTemplateResponse
newDeleteJobTemplateResponse =
  DeleteJobTemplateResponse'

instance Prelude.NFData DeleteJobTemplateResponse where
  rnf _ = ()
