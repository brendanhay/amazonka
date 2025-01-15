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
-- Module      : Amazonka.Connect.DeleteTaskTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the task template.
module Amazonka.Connect.DeleteTaskTemplate
  ( -- * Creating a Request
    DeleteTaskTemplate (..),
    newDeleteTaskTemplate,

    -- * Request Lenses
    deleteTaskTemplate_instanceId,
    deleteTaskTemplate_taskTemplateId,

    -- * Destructuring the Response
    DeleteTaskTemplateResponse (..),
    newDeleteTaskTemplateResponse,

    -- * Response Lenses
    deleteTaskTemplateResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTaskTemplate' smart constructor.
data DeleteTaskTemplate = DeleteTaskTemplate'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | A unique identifier for the task template.
    taskTemplateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTaskTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deleteTaskTemplate_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'taskTemplateId', 'deleteTaskTemplate_taskTemplateId' - A unique identifier for the task template.
newDeleteTaskTemplate ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'taskTemplateId'
  Prelude.Text ->
  DeleteTaskTemplate
newDeleteTaskTemplate pInstanceId_ pTaskTemplateId_ =
  DeleteTaskTemplate'
    { instanceId = pInstanceId_,
      taskTemplateId = pTaskTemplateId_
    }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
deleteTaskTemplate_instanceId :: Lens.Lens' DeleteTaskTemplate Prelude.Text
deleteTaskTemplate_instanceId = Lens.lens (\DeleteTaskTemplate' {instanceId} -> instanceId) (\s@DeleteTaskTemplate' {} a -> s {instanceId = a} :: DeleteTaskTemplate)

-- | A unique identifier for the task template.
deleteTaskTemplate_taskTemplateId :: Lens.Lens' DeleteTaskTemplate Prelude.Text
deleteTaskTemplate_taskTemplateId = Lens.lens (\DeleteTaskTemplate' {taskTemplateId} -> taskTemplateId) (\s@DeleteTaskTemplate' {} a -> s {taskTemplateId = a} :: DeleteTaskTemplate)

instance Core.AWSRequest DeleteTaskTemplate where
  type
    AWSResponse DeleteTaskTemplate =
      DeleteTaskTemplateResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTaskTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTaskTemplate where
  hashWithSalt _salt DeleteTaskTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` taskTemplateId

instance Prelude.NFData DeleteTaskTemplate where
  rnf DeleteTaskTemplate' {..} =
    Prelude.rnf instanceId `Prelude.seq`
      Prelude.rnf taskTemplateId

instance Data.ToHeaders DeleteTaskTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteTaskTemplate where
  toPath DeleteTaskTemplate' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/task/template/",
        Data.toBS taskTemplateId
      ]

instance Data.ToQuery DeleteTaskTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTaskTemplateResponse' smart constructor.
data DeleteTaskTemplateResponse = DeleteTaskTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTaskTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTaskTemplateResponse_httpStatus' - The response's http status code.
newDeleteTaskTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTaskTemplateResponse
newDeleteTaskTemplateResponse pHttpStatus_ =
  DeleteTaskTemplateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteTaskTemplateResponse_httpStatus :: Lens.Lens' DeleteTaskTemplateResponse Prelude.Int
deleteTaskTemplateResponse_httpStatus = Lens.lens (\DeleteTaskTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteTaskTemplateResponse' {} a -> s {httpStatus = a} :: DeleteTaskTemplateResponse)

instance Prelude.NFData DeleteTaskTemplateResponse where
  rnf DeleteTaskTemplateResponse' {..} =
    Prelude.rnf httpStatus
