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
-- Module      : Amazonka.M2.GetDataSetImportTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of a data set import task initiated with the
-- CreateDataSetImportTask operation.
module Amazonka.M2.GetDataSetImportTask
  ( -- * Creating a Request
    GetDataSetImportTask (..),
    newGetDataSetImportTask,

    -- * Request Lenses
    getDataSetImportTask_applicationId,
    getDataSetImportTask_taskId,

    -- * Destructuring the Response
    GetDataSetImportTaskResponse (..),
    newGetDataSetImportTaskResponse,

    -- * Response Lenses
    getDataSetImportTaskResponse_summary,
    getDataSetImportTaskResponse_httpStatus,
    getDataSetImportTaskResponse_status,
    getDataSetImportTaskResponse_taskId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDataSetImportTask' smart constructor.
data GetDataSetImportTask = GetDataSetImportTask'
  { -- | The application identifier.
    applicationId :: Prelude.Text,
    -- | The task identifier returned by the CreateDataSetImportTask operation.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataSetImportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getDataSetImportTask_applicationId' - The application identifier.
--
-- 'taskId', 'getDataSetImportTask_taskId' - The task identifier returned by the CreateDataSetImportTask operation.
newGetDataSetImportTask ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'taskId'
  Prelude.Text ->
  GetDataSetImportTask
newGetDataSetImportTask pApplicationId_ pTaskId_ =
  GetDataSetImportTask'
    { applicationId =
        pApplicationId_,
      taskId = pTaskId_
    }

-- | The application identifier.
getDataSetImportTask_applicationId :: Lens.Lens' GetDataSetImportTask Prelude.Text
getDataSetImportTask_applicationId = Lens.lens (\GetDataSetImportTask' {applicationId} -> applicationId) (\s@GetDataSetImportTask' {} a -> s {applicationId = a} :: GetDataSetImportTask)

-- | The task identifier returned by the CreateDataSetImportTask operation.
getDataSetImportTask_taskId :: Lens.Lens' GetDataSetImportTask Prelude.Text
getDataSetImportTask_taskId = Lens.lens (\GetDataSetImportTask' {taskId} -> taskId) (\s@GetDataSetImportTask' {} a -> s {taskId = a} :: GetDataSetImportTask)

instance Core.AWSRequest GetDataSetImportTask where
  type
    AWSResponse GetDataSetImportTask =
      GetDataSetImportTaskResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataSetImportTaskResponse'
            Prelude.<$> (x Data..?> "summary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "taskId")
      )

instance Prelude.Hashable GetDataSetImportTask where
  hashWithSalt _salt GetDataSetImportTask' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` taskId

instance Prelude.NFData GetDataSetImportTask where
  rnf GetDataSetImportTask' {..} =
    Prelude.rnf applicationId `Prelude.seq`
      Prelude.rnf taskId

instance Data.ToHeaders GetDataSetImportTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDataSetImportTask where
  toPath GetDataSetImportTask' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/dataset-import-tasks/",
        Data.toBS taskId
      ]

instance Data.ToQuery GetDataSetImportTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataSetImportTaskResponse' smart constructor.
data GetDataSetImportTaskResponse = GetDataSetImportTaskResponse'
  { -- | A summary of the status of the task.
    summary :: Prelude.Maybe DataSetImportSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the task.
    status :: DataSetTaskLifecycle,
    -- | The task identifier.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataSetImportTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'getDataSetImportTaskResponse_summary' - A summary of the status of the task.
--
-- 'httpStatus', 'getDataSetImportTaskResponse_httpStatus' - The response's http status code.
--
-- 'status', 'getDataSetImportTaskResponse_status' - The status of the task.
--
-- 'taskId', 'getDataSetImportTaskResponse_taskId' - The task identifier.
newGetDataSetImportTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'status'
  DataSetTaskLifecycle ->
  -- | 'taskId'
  Prelude.Text ->
  GetDataSetImportTaskResponse
newGetDataSetImportTaskResponse
  pHttpStatus_
  pStatus_
  pTaskId_ =
    GetDataSetImportTaskResponse'
      { summary =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        status = pStatus_,
        taskId = pTaskId_
      }

-- | A summary of the status of the task.
getDataSetImportTaskResponse_summary :: Lens.Lens' GetDataSetImportTaskResponse (Prelude.Maybe DataSetImportSummary)
getDataSetImportTaskResponse_summary = Lens.lens (\GetDataSetImportTaskResponse' {summary} -> summary) (\s@GetDataSetImportTaskResponse' {} a -> s {summary = a} :: GetDataSetImportTaskResponse)

-- | The response's http status code.
getDataSetImportTaskResponse_httpStatus :: Lens.Lens' GetDataSetImportTaskResponse Prelude.Int
getDataSetImportTaskResponse_httpStatus = Lens.lens (\GetDataSetImportTaskResponse' {httpStatus} -> httpStatus) (\s@GetDataSetImportTaskResponse' {} a -> s {httpStatus = a} :: GetDataSetImportTaskResponse)

-- | The status of the task.
getDataSetImportTaskResponse_status :: Lens.Lens' GetDataSetImportTaskResponse DataSetTaskLifecycle
getDataSetImportTaskResponse_status = Lens.lens (\GetDataSetImportTaskResponse' {status} -> status) (\s@GetDataSetImportTaskResponse' {} a -> s {status = a} :: GetDataSetImportTaskResponse)

-- | The task identifier.
getDataSetImportTaskResponse_taskId :: Lens.Lens' GetDataSetImportTaskResponse Prelude.Text
getDataSetImportTaskResponse_taskId = Lens.lens (\GetDataSetImportTaskResponse' {taskId} -> taskId) (\s@GetDataSetImportTaskResponse' {} a -> s {taskId = a} :: GetDataSetImportTaskResponse)

instance Prelude.NFData GetDataSetImportTaskResponse where
  rnf GetDataSetImportTaskResponse' {..} =
    Prelude.rnf summary `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf status `Prelude.seq`
          Prelude.rnf taskId
