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
-- Module      : Amazonka.MigrationHubStrategy.GetImportFileTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details about a specific import task.
module Amazonka.MigrationHubStrategy.GetImportFileTask
  ( -- * Creating a Request
    GetImportFileTask (..),
    newGetImportFileTask,

    -- * Request Lenses
    getImportFileTask_id,

    -- * Destructuring the Response
    GetImportFileTaskResponse (..),
    newGetImportFileTaskResponse,

    -- * Response Lenses
    getImportFileTaskResponse_statusReportS3Bucket,
    getImportFileTaskResponse_status,
    getImportFileTaskResponse_completionTime,
    getImportFileTaskResponse_id,
    getImportFileTaskResponse_numberOfRecordsFailed,
    getImportFileTaskResponse_importName,
    getImportFileTaskResponse_inputS3Bucket,
    getImportFileTaskResponse_inputS3Key,
    getImportFileTaskResponse_numberOfRecordsSuccess,
    getImportFileTaskResponse_startTime,
    getImportFileTaskResponse_statusReportS3Key,
    getImportFileTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubStrategy.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetImportFileTask' smart constructor.
data GetImportFileTask = GetImportFileTask'
  { -- | The ID of the import file task. This ID is returned in the response of
    -- StartImportFileTask.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImportFileTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getImportFileTask_id' - The ID of the import file task. This ID is returned in the response of
-- StartImportFileTask.
newGetImportFileTask ::
  -- | 'id'
  Prelude.Text ->
  GetImportFileTask
newGetImportFileTask pId_ =
  GetImportFileTask' {id = pId_}

-- | The ID of the import file task. This ID is returned in the response of
-- StartImportFileTask.
getImportFileTask_id :: Lens.Lens' GetImportFileTask Prelude.Text
getImportFileTask_id = Lens.lens (\GetImportFileTask' {id} -> id) (\s@GetImportFileTask' {} a -> s {id = a} :: GetImportFileTask)

instance Core.AWSRequest GetImportFileTask where
  type
    AWSResponse GetImportFileTask =
      GetImportFileTaskResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetImportFileTaskResponse'
            Prelude.<$> (x Core..?> "statusReportS3Bucket")
            Prelude.<*> (x Core..?> "status")
            Prelude.<*> (x Core..?> "completionTime")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "numberOfRecordsFailed")
            Prelude.<*> (x Core..?> "importName")
            Prelude.<*> (x Core..?> "inputS3Bucket")
            Prelude.<*> (x Core..?> "inputS3Key")
            Prelude.<*> (x Core..?> "numberOfRecordsSuccess")
            Prelude.<*> (x Core..?> "startTime")
            Prelude.<*> (x Core..?> "statusReportS3Key")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetImportFileTask where
  hashWithSalt _salt GetImportFileTask' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetImportFileTask where
  rnf GetImportFileTask' {..} = Prelude.rnf id

instance Core.ToHeaders GetImportFileTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetImportFileTask where
  toPath GetImportFileTask' {..} =
    Prelude.mconcat
      ["/get-import-file-task/", Core.toBS id]

instance Core.ToQuery GetImportFileTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetImportFileTaskResponse' smart constructor.
data GetImportFileTaskResponse = GetImportFileTaskResponse'
  { -- | The S3 bucket name for status report of import task.
    statusReportS3Bucket :: Prelude.Maybe Prelude.Text,
    -- | Status of import file task.
    status :: Prelude.Maybe ImportFileTaskStatus,
    -- | The time that the import task completed.
    completionTime :: Prelude.Maybe Core.POSIX,
    -- | The import file task @id@ returned in the response of
    -- StartImportFileTask.
    id :: Prelude.Maybe Prelude.Text,
    -- | The number of records that failed to be imported.
    numberOfRecordsFailed :: Prelude.Maybe Prelude.Int,
    -- | The name of the import task given in StartImportFileTask.
    importName :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket where import file is located.
    inputS3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 key name of the import file.
    inputS3Key :: Prelude.Maybe Prelude.Text,
    -- | The number of records successfully imported.
    numberOfRecordsSuccess :: Prelude.Maybe Prelude.Int,
    -- | Start time of the import task.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon S3 key name for status report of import task. The report
    -- contains details about whether each record imported successfully or why
    -- it did not.
    statusReportS3Key :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImportFileTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusReportS3Bucket', 'getImportFileTaskResponse_statusReportS3Bucket' - The S3 bucket name for status report of import task.
--
-- 'status', 'getImportFileTaskResponse_status' - Status of import file task.
--
-- 'completionTime', 'getImportFileTaskResponse_completionTime' - The time that the import task completed.
--
-- 'id', 'getImportFileTaskResponse_id' - The import file task @id@ returned in the response of
-- StartImportFileTask.
--
-- 'numberOfRecordsFailed', 'getImportFileTaskResponse_numberOfRecordsFailed' - The number of records that failed to be imported.
--
-- 'importName', 'getImportFileTaskResponse_importName' - The name of the import task given in StartImportFileTask.
--
-- 'inputS3Bucket', 'getImportFileTaskResponse_inputS3Bucket' - The S3 bucket where import file is located.
--
-- 'inputS3Key', 'getImportFileTaskResponse_inputS3Key' - The Amazon S3 key name of the import file.
--
-- 'numberOfRecordsSuccess', 'getImportFileTaskResponse_numberOfRecordsSuccess' - The number of records successfully imported.
--
-- 'startTime', 'getImportFileTaskResponse_startTime' - Start time of the import task.
--
-- 'statusReportS3Key', 'getImportFileTaskResponse_statusReportS3Key' - The Amazon S3 key name for status report of import task. The report
-- contains details about whether each record imported successfully or why
-- it did not.
--
-- 'httpStatus', 'getImportFileTaskResponse_httpStatus' - The response's http status code.
newGetImportFileTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetImportFileTaskResponse
newGetImportFileTaskResponse pHttpStatus_ =
  GetImportFileTaskResponse'
    { statusReportS3Bucket =
        Prelude.Nothing,
      status = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      id = Prelude.Nothing,
      numberOfRecordsFailed = Prelude.Nothing,
      importName = Prelude.Nothing,
      inputS3Bucket = Prelude.Nothing,
      inputS3Key = Prelude.Nothing,
      numberOfRecordsSuccess = Prelude.Nothing,
      startTime = Prelude.Nothing,
      statusReportS3Key = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The S3 bucket name for status report of import task.
getImportFileTaskResponse_statusReportS3Bucket :: Lens.Lens' GetImportFileTaskResponse (Prelude.Maybe Prelude.Text)
getImportFileTaskResponse_statusReportS3Bucket = Lens.lens (\GetImportFileTaskResponse' {statusReportS3Bucket} -> statusReportS3Bucket) (\s@GetImportFileTaskResponse' {} a -> s {statusReportS3Bucket = a} :: GetImportFileTaskResponse)

-- | Status of import file task.
getImportFileTaskResponse_status :: Lens.Lens' GetImportFileTaskResponse (Prelude.Maybe ImportFileTaskStatus)
getImportFileTaskResponse_status = Lens.lens (\GetImportFileTaskResponse' {status} -> status) (\s@GetImportFileTaskResponse' {} a -> s {status = a} :: GetImportFileTaskResponse)

-- | The time that the import task completed.
getImportFileTaskResponse_completionTime :: Lens.Lens' GetImportFileTaskResponse (Prelude.Maybe Prelude.UTCTime)
getImportFileTaskResponse_completionTime = Lens.lens (\GetImportFileTaskResponse' {completionTime} -> completionTime) (\s@GetImportFileTaskResponse' {} a -> s {completionTime = a} :: GetImportFileTaskResponse) Prelude.. Lens.mapping Core._Time

-- | The import file task @id@ returned in the response of
-- StartImportFileTask.
getImportFileTaskResponse_id :: Lens.Lens' GetImportFileTaskResponse (Prelude.Maybe Prelude.Text)
getImportFileTaskResponse_id = Lens.lens (\GetImportFileTaskResponse' {id} -> id) (\s@GetImportFileTaskResponse' {} a -> s {id = a} :: GetImportFileTaskResponse)

-- | The number of records that failed to be imported.
getImportFileTaskResponse_numberOfRecordsFailed :: Lens.Lens' GetImportFileTaskResponse (Prelude.Maybe Prelude.Int)
getImportFileTaskResponse_numberOfRecordsFailed = Lens.lens (\GetImportFileTaskResponse' {numberOfRecordsFailed} -> numberOfRecordsFailed) (\s@GetImportFileTaskResponse' {} a -> s {numberOfRecordsFailed = a} :: GetImportFileTaskResponse)

-- | The name of the import task given in StartImportFileTask.
getImportFileTaskResponse_importName :: Lens.Lens' GetImportFileTaskResponse (Prelude.Maybe Prelude.Text)
getImportFileTaskResponse_importName = Lens.lens (\GetImportFileTaskResponse' {importName} -> importName) (\s@GetImportFileTaskResponse' {} a -> s {importName = a} :: GetImportFileTaskResponse)

-- | The S3 bucket where import file is located.
getImportFileTaskResponse_inputS3Bucket :: Lens.Lens' GetImportFileTaskResponse (Prelude.Maybe Prelude.Text)
getImportFileTaskResponse_inputS3Bucket = Lens.lens (\GetImportFileTaskResponse' {inputS3Bucket} -> inputS3Bucket) (\s@GetImportFileTaskResponse' {} a -> s {inputS3Bucket = a} :: GetImportFileTaskResponse)

-- | The Amazon S3 key name of the import file.
getImportFileTaskResponse_inputS3Key :: Lens.Lens' GetImportFileTaskResponse (Prelude.Maybe Prelude.Text)
getImportFileTaskResponse_inputS3Key = Lens.lens (\GetImportFileTaskResponse' {inputS3Key} -> inputS3Key) (\s@GetImportFileTaskResponse' {} a -> s {inputS3Key = a} :: GetImportFileTaskResponse)

-- | The number of records successfully imported.
getImportFileTaskResponse_numberOfRecordsSuccess :: Lens.Lens' GetImportFileTaskResponse (Prelude.Maybe Prelude.Int)
getImportFileTaskResponse_numberOfRecordsSuccess = Lens.lens (\GetImportFileTaskResponse' {numberOfRecordsSuccess} -> numberOfRecordsSuccess) (\s@GetImportFileTaskResponse' {} a -> s {numberOfRecordsSuccess = a} :: GetImportFileTaskResponse)

-- | Start time of the import task.
getImportFileTaskResponse_startTime :: Lens.Lens' GetImportFileTaskResponse (Prelude.Maybe Prelude.UTCTime)
getImportFileTaskResponse_startTime = Lens.lens (\GetImportFileTaskResponse' {startTime} -> startTime) (\s@GetImportFileTaskResponse' {} a -> s {startTime = a} :: GetImportFileTaskResponse) Prelude.. Lens.mapping Core._Time

-- | The Amazon S3 key name for status report of import task. The report
-- contains details about whether each record imported successfully or why
-- it did not.
getImportFileTaskResponse_statusReportS3Key :: Lens.Lens' GetImportFileTaskResponse (Prelude.Maybe Prelude.Text)
getImportFileTaskResponse_statusReportS3Key = Lens.lens (\GetImportFileTaskResponse' {statusReportS3Key} -> statusReportS3Key) (\s@GetImportFileTaskResponse' {} a -> s {statusReportS3Key = a} :: GetImportFileTaskResponse)

-- | The response's http status code.
getImportFileTaskResponse_httpStatus :: Lens.Lens' GetImportFileTaskResponse Prelude.Int
getImportFileTaskResponse_httpStatus = Lens.lens (\GetImportFileTaskResponse' {httpStatus} -> httpStatus) (\s@GetImportFileTaskResponse' {} a -> s {httpStatus = a} :: GetImportFileTaskResponse)

instance Prelude.NFData GetImportFileTaskResponse where
  rnf GetImportFileTaskResponse' {..} =
    Prelude.rnf statusReportS3Bucket
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf numberOfRecordsFailed
      `Prelude.seq` Prelude.rnf importName
      `Prelude.seq` Prelude.rnf inputS3Bucket
      `Prelude.seq` Prelude.rnf inputS3Key
      `Prelude.seq` Prelude.rnf numberOfRecordsSuccess
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf statusReportS3Key
      `Prelude.seq` Prelude.rnf httpStatus
