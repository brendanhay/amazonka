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
-- Module      : Amazonka.TimeStreamWrite.CreateBatchLoadTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Timestream batch load task. A batch load task processes
-- data from a CSV source in an S3 location and writes to a Timestream
-- table. A mapping from source to target is defined in a batch load task.
-- Errors and events are written to a report at an S3 location. For the
-- report, if the KMS key is not specified, the report will be encrypted
-- with an S3 managed key when @SSE_S3@ is the option. Otherwise an error
-- is thrown. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk Amazon Web Services managed keys>.
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/ts-limits.html Service quotas apply>.
-- For details, see
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/code-samples.create-batch-load.html code sample>.
module Amazonka.TimeStreamWrite.CreateBatchLoadTask
  ( -- * Creating a Request
    CreateBatchLoadTask (..),
    newCreateBatchLoadTask,

    -- * Request Lenses
    createBatchLoadTask_clientToken,
    createBatchLoadTask_dataModelConfiguration,
    createBatchLoadTask_recordVersion,
    createBatchLoadTask_dataSourceConfiguration,
    createBatchLoadTask_reportConfiguration,
    createBatchLoadTask_targetDatabaseName,
    createBatchLoadTask_targetTableName,

    -- * Destructuring the Response
    CreateBatchLoadTaskResponse (..),
    newCreateBatchLoadTaskResponse,

    -- * Response Lenses
    createBatchLoadTaskResponse_httpStatus,
    createBatchLoadTaskResponse_taskId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamWrite.Types

-- | /See:/ 'newCreateBatchLoadTask' smart constructor.
data CreateBatchLoadTask = CreateBatchLoadTask'
  { clientToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    dataModelConfiguration :: Prelude.Maybe DataModelConfiguration,
    recordVersion :: Prelude.Maybe Prelude.Integer,
    -- | Defines configuration details about the data source for a batch load
    -- task.
    dataSourceConfiguration :: DataSourceConfiguration,
    reportConfiguration :: ReportConfiguration,
    -- | Target Timestream database for a batch load task.
    targetDatabaseName :: Prelude.Text,
    -- | Target Timestream table for a batch load task.
    targetTableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBatchLoadTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createBatchLoadTask_clientToken' -
--
-- 'dataModelConfiguration', 'createBatchLoadTask_dataModelConfiguration' - Undocumented member.
--
-- 'recordVersion', 'createBatchLoadTask_recordVersion' -
--
-- 'dataSourceConfiguration', 'createBatchLoadTask_dataSourceConfiguration' - Defines configuration details about the data source for a batch load
-- task.
--
-- 'reportConfiguration', 'createBatchLoadTask_reportConfiguration' - Undocumented member.
--
-- 'targetDatabaseName', 'createBatchLoadTask_targetDatabaseName' - Target Timestream database for a batch load task.
--
-- 'targetTableName', 'createBatchLoadTask_targetTableName' - Target Timestream table for a batch load task.
newCreateBatchLoadTask ::
  -- | 'dataSourceConfiguration'
  DataSourceConfiguration ->
  -- | 'reportConfiguration'
  ReportConfiguration ->
  -- | 'targetDatabaseName'
  Prelude.Text ->
  -- | 'targetTableName'
  Prelude.Text ->
  CreateBatchLoadTask
newCreateBatchLoadTask
  pDataSourceConfiguration_
  pReportConfiguration_
  pTargetDatabaseName_
  pTargetTableName_ =
    CreateBatchLoadTask'
      { clientToken = Prelude.Nothing,
        dataModelConfiguration = Prelude.Nothing,
        recordVersion = Prelude.Nothing,
        dataSourceConfiguration = pDataSourceConfiguration_,
        reportConfiguration = pReportConfiguration_,
        targetDatabaseName = pTargetDatabaseName_,
        targetTableName = pTargetTableName_
      }

createBatchLoadTask_clientToken :: Lens.Lens' CreateBatchLoadTask (Prelude.Maybe Prelude.Text)
createBatchLoadTask_clientToken = Lens.lens (\CreateBatchLoadTask' {clientToken} -> clientToken) (\s@CreateBatchLoadTask' {} a -> s {clientToken = a} :: CreateBatchLoadTask) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
createBatchLoadTask_dataModelConfiguration :: Lens.Lens' CreateBatchLoadTask (Prelude.Maybe DataModelConfiguration)
createBatchLoadTask_dataModelConfiguration = Lens.lens (\CreateBatchLoadTask' {dataModelConfiguration} -> dataModelConfiguration) (\s@CreateBatchLoadTask' {} a -> s {dataModelConfiguration = a} :: CreateBatchLoadTask)

createBatchLoadTask_recordVersion :: Lens.Lens' CreateBatchLoadTask (Prelude.Maybe Prelude.Integer)
createBatchLoadTask_recordVersion = Lens.lens (\CreateBatchLoadTask' {recordVersion} -> recordVersion) (\s@CreateBatchLoadTask' {} a -> s {recordVersion = a} :: CreateBatchLoadTask)

-- | Defines configuration details about the data source for a batch load
-- task.
createBatchLoadTask_dataSourceConfiguration :: Lens.Lens' CreateBatchLoadTask DataSourceConfiguration
createBatchLoadTask_dataSourceConfiguration = Lens.lens (\CreateBatchLoadTask' {dataSourceConfiguration} -> dataSourceConfiguration) (\s@CreateBatchLoadTask' {} a -> s {dataSourceConfiguration = a} :: CreateBatchLoadTask)

-- | Undocumented member.
createBatchLoadTask_reportConfiguration :: Lens.Lens' CreateBatchLoadTask ReportConfiguration
createBatchLoadTask_reportConfiguration = Lens.lens (\CreateBatchLoadTask' {reportConfiguration} -> reportConfiguration) (\s@CreateBatchLoadTask' {} a -> s {reportConfiguration = a} :: CreateBatchLoadTask)

-- | Target Timestream database for a batch load task.
createBatchLoadTask_targetDatabaseName :: Lens.Lens' CreateBatchLoadTask Prelude.Text
createBatchLoadTask_targetDatabaseName = Lens.lens (\CreateBatchLoadTask' {targetDatabaseName} -> targetDatabaseName) (\s@CreateBatchLoadTask' {} a -> s {targetDatabaseName = a} :: CreateBatchLoadTask)

-- | Target Timestream table for a batch load task.
createBatchLoadTask_targetTableName :: Lens.Lens' CreateBatchLoadTask Prelude.Text
createBatchLoadTask_targetTableName = Lens.lens (\CreateBatchLoadTask' {targetTableName} -> targetTableName) (\s@CreateBatchLoadTask' {} a -> s {targetTableName = a} :: CreateBatchLoadTask)

instance Core.AWSRequest CreateBatchLoadTask where
  type
    AWSResponse CreateBatchLoadTask =
      CreateBatchLoadTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBatchLoadTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "TaskId")
      )

instance Prelude.Hashable CreateBatchLoadTask where
  hashWithSalt _salt CreateBatchLoadTask' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` dataModelConfiguration
      `Prelude.hashWithSalt` recordVersion
      `Prelude.hashWithSalt` dataSourceConfiguration
      `Prelude.hashWithSalt` reportConfiguration
      `Prelude.hashWithSalt` targetDatabaseName
      `Prelude.hashWithSalt` targetTableName

instance Prelude.NFData CreateBatchLoadTask where
  rnf CreateBatchLoadTask' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dataModelConfiguration
      `Prelude.seq` Prelude.rnf recordVersion
      `Prelude.seq` Prelude.rnf dataSourceConfiguration
      `Prelude.seq` Prelude.rnf reportConfiguration
      `Prelude.seq` Prelude.rnf targetDatabaseName
      `Prelude.seq` Prelude.rnf targetTableName

instance Data.ToHeaders CreateBatchLoadTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.CreateBatchLoadTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBatchLoadTask where
  toJSON CreateBatchLoadTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("DataModelConfiguration" Data..=)
              Prelude.<$> dataModelConfiguration,
            ("RecordVersion" Data..=) Prelude.<$> recordVersion,
            Prelude.Just
              ( "DataSourceConfiguration"
                  Data..= dataSourceConfiguration
              ),
            Prelude.Just
              ("ReportConfiguration" Data..= reportConfiguration),
            Prelude.Just
              ("TargetDatabaseName" Data..= targetDatabaseName),
            Prelude.Just
              ("TargetTableName" Data..= targetTableName)
          ]
      )

instance Data.ToPath CreateBatchLoadTask where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateBatchLoadTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBatchLoadTaskResponse' smart constructor.
data CreateBatchLoadTaskResponse = CreateBatchLoadTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the batch load task.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBatchLoadTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createBatchLoadTaskResponse_httpStatus' - The response's http status code.
--
-- 'taskId', 'createBatchLoadTaskResponse_taskId' - The ID of the batch load task.
newCreateBatchLoadTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'taskId'
  Prelude.Text ->
  CreateBatchLoadTaskResponse
newCreateBatchLoadTaskResponse pHttpStatus_ pTaskId_ =
  CreateBatchLoadTaskResponse'
    { httpStatus =
        pHttpStatus_,
      taskId = pTaskId_
    }

-- | The response's http status code.
createBatchLoadTaskResponse_httpStatus :: Lens.Lens' CreateBatchLoadTaskResponse Prelude.Int
createBatchLoadTaskResponse_httpStatus = Lens.lens (\CreateBatchLoadTaskResponse' {httpStatus} -> httpStatus) (\s@CreateBatchLoadTaskResponse' {} a -> s {httpStatus = a} :: CreateBatchLoadTaskResponse)

-- | The ID of the batch load task.
createBatchLoadTaskResponse_taskId :: Lens.Lens' CreateBatchLoadTaskResponse Prelude.Text
createBatchLoadTaskResponse_taskId = Lens.lens (\CreateBatchLoadTaskResponse' {taskId} -> taskId) (\s@CreateBatchLoadTaskResponse' {} a -> s {taskId = a} :: CreateBatchLoadTaskResponse)

instance Prelude.NFData CreateBatchLoadTaskResponse where
  rnf CreateBatchLoadTaskResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf taskId
