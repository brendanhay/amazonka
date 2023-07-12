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
-- Module      : Amazonka.M2.CreateDataSetImportTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a data set import task for a specific application.
module Amazonka.M2.CreateDataSetImportTask
  ( -- * Creating a Request
    CreateDataSetImportTask (..),
    newCreateDataSetImportTask,

    -- * Request Lenses
    createDataSetImportTask_clientToken,
    createDataSetImportTask_applicationId,
    createDataSetImportTask_importConfig,

    -- * Destructuring the Response
    CreateDataSetImportTaskResponse (..),
    newCreateDataSetImportTaskResponse,

    -- * Response Lenses
    createDataSetImportTaskResponse_httpStatus,
    createDataSetImportTaskResponse_taskId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDataSetImportTask' smart constructor.
data CreateDataSetImportTask = CreateDataSetImportTask'
  { -- | Unique, case-sensitive identifier you provide to ensure the idempotency
    -- of the request to create a data set import. The service generates the
    -- clientToken when the API call is triggered. The token expires after one
    -- hour, so if you retry the API within this timeframe with the same
    -- clientToken, you will get the same response. The service also handles
    -- deleting the clientToken after it expires.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the application for which you want to import
    -- data sets.
    applicationId :: Prelude.Text,
    -- | The data set import task configuration.
    importConfig :: DataSetImportConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataSetImportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createDataSetImportTask_clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request to create a data set import. The service generates the
-- clientToken when the API call is triggered. The token expires after one
-- hour, so if you retry the API within this timeframe with the same
-- clientToken, you will get the same response. The service also handles
-- deleting the clientToken after it expires.
--
-- 'applicationId', 'createDataSetImportTask_applicationId' - The unique identifier of the application for which you want to import
-- data sets.
--
-- 'importConfig', 'createDataSetImportTask_importConfig' - The data set import task configuration.
newCreateDataSetImportTask ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'importConfig'
  DataSetImportConfig ->
  CreateDataSetImportTask
newCreateDataSetImportTask
  pApplicationId_
  pImportConfig_ =
    CreateDataSetImportTask'
      { clientToken =
          Prelude.Nothing,
        applicationId = pApplicationId_,
        importConfig = pImportConfig_
      }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request to create a data set import. The service generates the
-- clientToken when the API call is triggered. The token expires after one
-- hour, so if you retry the API within this timeframe with the same
-- clientToken, you will get the same response. The service also handles
-- deleting the clientToken after it expires.
createDataSetImportTask_clientToken :: Lens.Lens' CreateDataSetImportTask (Prelude.Maybe Prelude.Text)
createDataSetImportTask_clientToken = Lens.lens (\CreateDataSetImportTask' {clientToken} -> clientToken) (\s@CreateDataSetImportTask' {} a -> s {clientToken = a} :: CreateDataSetImportTask)

-- | The unique identifier of the application for which you want to import
-- data sets.
createDataSetImportTask_applicationId :: Lens.Lens' CreateDataSetImportTask Prelude.Text
createDataSetImportTask_applicationId = Lens.lens (\CreateDataSetImportTask' {applicationId} -> applicationId) (\s@CreateDataSetImportTask' {} a -> s {applicationId = a} :: CreateDataSetImportTask)

-- | The data set import task configuration.
createDataSetImportTask_importConfig :: Lens.Lens' CreateDataSetImportTask DataSetImportConfig
createDataSetImportTask_importConfig = Lens.lens (\CreateDataSetImportTask' {importConfig} -> importConfig) (\s@CreateDataSetImportTask' {} a -> s {importConfig = a} :: CreateDataSetImportTask)

instance Core.AWSRequest CreateDataSetImportTask where
  type
    AWSResponse CreateDataSetImportTask =
      CreateDataSetImportTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataSetImportTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "taskId")
      )

instance Prelude.Hashable CreateDataSetImportTask where
  hashWithSalt _salt CreateDataSetImportTask' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` importConfig

instance Prelude.NFData CreateDataSetImportTask where
  rnf CreateDataSetImportTask' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf importConfig

instance Data.ToHeaders CreateDataSetImportTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDataSetImportTask where
  toJSON CreateDataSetImportTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("importConfig" Data..= importConfig)
          ]
      )

instance Data.ToPath CreateDataSetImportTask where
  toPath CreateDataSetImportTask' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/dataset-import-task"
      ]

instance Data.ToQuery CreateDataSetImportTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDataSetImportTaskResponse' smart constructor.
data CreateDataSetImportTaskResponse = CreateDataSetImportTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The task identifier. This operation is asynchronous. Use this identifier
    -- with the GetDataSetImportTask operation to obtain the status of this
    -- task.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataSetImportTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createDataSetImportTaskResponse_httpStatus' - The response's http status code.
--
-- 'taskId', 'createDataSetImportTaskResponse_taskId' - The task identifier. This operation is asynchronous. Use this identifier
-- with the GetDataSetImportTask operation to obtain the status of this
-- task.
newCreateDataSetImportTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'taskId'
  Prelude.Text ->
  CreateDataSetImportTaskResponse
newCreateDataSetImportTaskResponse
  pHttpStatus_
  pTaskId_ =
    CreateDataSetImportTaskResponse'
      { httpStatus =
          pHttpStatus_,
        taskId = pTaskId_
      }

-- | The response's http status code.
createDataSetImportTaskResponse_httpStatus :: Lens.Lens' CreateDataSetImportTaskResponse Prelude.Int
createDataSetImportTaskResponse_httpStatus = Lens.lens (\CreateDataSetImportTaskResponse' {httpStatus} -> httpStatus) (\s@CreateDataSetImportTaskResponse' {} a -> s {httpStatus = a} :: CreateDataSetImportTaskResponse)

-- | The task identifier. This operation is asynchronous. Use this identifier
-- with the GetDataSetImportTask operation to obtain the status of this
-- task.
createDataSetImportTaskResponse_taskId :: Lens.Lens' CreateDataSetImportTaskResponse Prelude.Text
createDataSetImportTaskResponse_taskId = Lens.lens (\CreateDataSetImportTaskResponse' {taskId} -> taskId) (\s@CreateDataSetImportTaskResponse' {} a -> s {taskId = a} :: CreateDataSetImportTaskResponse)

instance
  Prelude.NFData
    CreateDataSetImportTaskResponse
  where
  rnf CreateDataSetImportTaskResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf taskId
