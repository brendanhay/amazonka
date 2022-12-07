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
-- Module      : Amazonka.FIS.DeleteExperimentTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified experiment template.
module Amazonka.FIS.DeleteExperimentTemplate
  ( -- * Creating a Request
    DeleteExperimentTemplate (..),
    newDeleteExperimentTemplate,

    -- * Request Lenses
    deleteExperimentTemplate_id,

    -- * Destructuring the Response
    DeleteExperimentTemplateResponse (..),
    newDeleteExperimentTemplateResponse,

    -- * Response Lenses
    deleteExperimentTemplateResponse_experimentTemplate,
    deleteExperimentTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteExperimentTemplate' smart constructor.
data DeleteExperimentTemplate = DeleteExperimentTemplate'
  { -- | The ID of the experiment template.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteExperimentTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteExperimentTemplate_id' - The ID of the experiment template.
newDeleteExperimentTemplate ::
  -- | 'id'
  Prelude.Text ->
  DeleteExperimentTemplate
newDeleteExperimentTemplate pId_ =
  DeleteExperimentTemplate' {id = pId_}

-- | The ID of the experiment template.
deleteExperimentTemplate_id :: Lens.Lens' DeleteExperimentTemplate Prelude.Text
deleteExperimentTemplate_id = Lens.lens (\DeleteExperimentTemplate' {id} -> id) (\s@DeleteExperimentTemplate' {} a -> s {id = a} :: DeleteExperimentTemplate)

instance Core.AWSRequest DeleteExperimentTemplate where
  type
    AWSResponse DeleteExperimentTemplate =
      DeleteExperimentTemplateResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteExperimentTemplateResponse'
            Prelude.<$> (x Data..?> "experimentTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteExperimentTemplate where
  hashWithSalt _salt DeleteExperimentTemplate' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteExperimentTemplate where
  rnf DeleteExperimentTemplate' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteExperimentTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteExperimentTemplate where
  toPath DeleteExperimentTemplate' {..} =
    Prelude.mconcat
      ["/experimentTemplates/", Data.toBS id]

instance Data.ToQuery DeleteExperimentTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteExperimentTemplateResponse' smart constructor.
data DeleteExperimentTemplateResponse = DeleteExperimentTemplateResponse'
  { -- | Information about the experiment template.
    experimentTemplate :: Prelude.Maybe ExperimentTemplate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteExperimentTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experimentTemplate', 'deleteExperimentTemplateResponse_experimentTemplate' - Information about the experiment template.
--
-- 'httpStatus', 'deleteExperimentTemplateResponse_httpStatus' - The response's http status code.
newDeleteExperimentTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteExperimentTemplateResponse
newDeleteExperimentTemplateResponse pHttpStatus_ =
  DeleteExperimentTemplateResponse'
    { experimentTemplate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the experiment template.
deleteExperimentTemplateResponse_experimentTemplate :: Lens.Lens' DeleteExperimentTemplateResponse (Prelude.Maybe ExperimentTemplate)
deleteExperimentTemplateResponse_experimentTemplate = Lens.lens (\DeleteExperimentTemplateResponse' {experimentTemplate} -> experimentTemplate) (\s@DeleteExperimentTemplateResponse' {} a -> s {experimentTemplate = a} :: DeleteExperimentTemplateResponse)

-- | The response's http status code.
deleteExperimentTemplateResponse_httpStatus :: Lens.Lens' DeleteExperimentTemplateResponse Prelude.Int
deleteExperimentTemplateResponse_httpStatus = Lens.lens (\DeleteExperimentTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteExperimentTemplateResponse' {} a -> s {httpStatus = a} :: DeleteExperimentTemplateResponse)

instance
  Prelude.NFData
    DeleteExperimentTemplateResponse
  where
  rnf DeleteExperimentTemplateResponse' {..} =
    Prelude.rnf experimentTemplate
      `Prelude.seq` Prelude.rnf httpStatus
