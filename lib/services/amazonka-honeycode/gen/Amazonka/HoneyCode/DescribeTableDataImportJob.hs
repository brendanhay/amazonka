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
-- Module      : Amazonka.HoneyCode.DescribeTableDataImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DescribeTableDataImportJob API allows you to retrieve the status and
-- details of a table data import job.
module Amazonka.HoneyCode.DescribeTableDataImportJob
  ( -- * Creating a Request
    DescribeTableDataImportJob (..),
    newDescribeTableDataImportJob,

    -- * Request Lenses
    describeTableDataImportJob_workbookId,
    describeTableDataImportJob_tableId,
    describeTableDataImportJob_jobId,

    -- * Destructuring the Response
    DescribeTableDataImportJobResponse (..),
    newDescribeTableDataImportJobResponse,

    -- * Response Lenses
    describeTableDataImportJobResponse_errorCode,
    describeTableDataImportJobResponse_httpStatus,
    describeTableDataImportJobResponse_jobStatus,
    describeTableDataImportJobResponse_message,
    describeTableDataImportJobResponse_jobMetadata,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTableDataImportJob' smart constructor.
data DescribeTableDataImportJob = DescribeTableDataImportJob'
  { -- | The ID of the workbook into which data was imported.
    --
    -- If a workbook with the specified id could not be found, this API throws
    -- ResourceNotFoundException.
    workbookId :: Prelude.Text,
    -- | The ID of the table into which data was imported.
    --
    -- If a table with the specified id could not be found, this API throws
    -- ResourceNotFoundException.
    tableId :: Prelude.Text,
    -- | The ID of the job that was returned by the StartTableDataImportJob
    -- request.
    --
    -- If a job with the specified id could not be found, this API throws
    -- ResourceNotFoundException.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTableDataImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workbookId', 'describeTableDataImportJob_workbookId' - The ID of the workbook into which data was imported.
--
-- If a workbook with the specified id could not be found, this API throws
-- ResourceNotFoundException.
--
-- 'tableId', 'describeTableDataImportJob_tableId' - The ID of the table into which data was imported.
--
-- If a table with the specified id could not be found, this API throws
-- ResourceNotFoundException.
--
-- 'jobId', 'describeTableDataImportJob_jobId' - The ID of the job that was returned by the StartTableDataImportJob
-- request.
--
-- If a job with the specified id could not be found, this API throws
-- ResourceNotFoundException.
newDescribeTableDataImportJob ::
  -- | 'workbookId'
  Prelude.Text ->
  -- | 'tableId'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  DescribeTableDataImportJob
newDescribeTableDataImportJob
  pWorkbookId_
  pTableId_
  pJobId_ =
    DescribeTableDataImportJob'
      { workbookId =
          pWorkbookId_,
        tableId = pTableId_,
        jobId = pJobId_
      }

-- | The ID of the workbook into which data was imported.
--
-- If a workbook with the specified id could not be found, this API throws
-- ResourceNotFoundException.
describeTableDataImportJob_workbookId :: Lens.Lens' DescribeTableDataImportJob Prelude.Text
describeTableDataImportJob_workbookId = Lens.lens (\DescribeTableDataImportJob' {workbookId} -> workbookId) (\s@DescribeTableDataImportJob' {} a -> s {workbookId = a} :: DescribeTableDataImportJob)

-- | The ID of the table into which data was imported.
--
-- If a table with the specified id could not be found, this API throws
-- ResourceNotFoundException.
describeTableDataImportJob_tableId :: Lens.Lens' DescribeTableDataImportJob Prelude.Text
describeTableDataImportJob_tableId = Lens.lens (\DescribeTableDataImportJob' {tableId} -> tableId) (\s@DescribeTableDataImportJob' {} a -> s {tableId = a} :: DescribeTableDataImportJob)

-- | The ID of the job that was returned by the StartTableDataImportJob
-- request.
--
-- If a job with the specified id could not be found, this API throws
-- ResourceNotFoundException.
describeTableDataImportJob_jobId :: Lens.Lens' DescribeTableDataImportJob Prelude.Text
describeTableDataImportJob_jobId = Lens.lens (\DescribeTableDataImportJob' {jobId} -> jobId) (\s@DescribeTableDataImportJob' {} a -> s {jobId = a} :: DescribeTableDataImportJob)

instance Core.AWSRequest DescribeTableDataImportJob where
  type
    AWSResponse DescribeTableDataImportJob =
      DescribeTableDataImportJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTableDataImportJobResponse'
            Prelude.<$> (x Data..?> "errorCode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "jobStatus")
            Prelude.<*> (x Data..:> "message")
            Prelude.<*> (x Data..:> "jobMetadata")
      )

instance Prelude.Hashable DescribeTableDataImportJob where
  hashWithSalt _salt DescribeTableDataImportJob' {..} =
    _salt
      `Prelude.hashWithSalt` workbookId
      `Prelude.hashWithSalt` tableId
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribeTableDataImportJob where
  rnf DescribeTableDataImportJob' {..} =
    Prelude.rnf workbookId
      `Prelude.seq` Prelude.rnf tableId
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders DescribeTableDataImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeTableDataImportJob where
  toPath DescribeTableDataImportJob' {..} =
    Prelude.mconcat
      [ "/workbooks/",
        Data.toBS workbookId,
        "/tables/",
        Data.toBS tableId,
        "/import/",
        Data.toBS jobId
      ]

instance Data.ToQuery DescribeTableDataImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTableDataImportJobResponse' smart constructor.
data DescribeTableDataImportJobResponse = DescribeTableDataImportJobResponse'
  { -- | If job status is failed, error code to understand reason for the
    -- failure.
    errorCode :: Prelude.Maybe ErrorCode,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The current status of the import job.
    jobStatus :: TableDataImportJobStatus,
    -- | A message providing more details about the current status of the import
    -- job.
    message :: Prelude.Text,
    -- | The metadata about the job that was submitted for import.
    jobMetadata :: TableDataImportJobMetadata
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTableDataImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'describeTableDataImportJobResponse_errorCode' - If job status is failed, error code to understand reason for the
-- failure.
--
-- 'httpStatus', 'describeTableDataImportJobResponse_httpStatus' - The response's http status code.
--
-- 'jobStatus', 'describeTableDataImportJobResponse_jobStatus' - The current status of the import job.
--
-- 'message', 'describeTableDataImportJobResponse_message' - A message providing more details about the current status of the import
-- job.
--
-- 'jobMetadata', 'describeTableDataImportJobResponse_jobMetadata' - The metadata about the job that was submitted for import.
newDescribeTableDataImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobStatus'
  TableDataImportJobStatus ->
  -- | 'message'
  Prelude.Text ->
  -- | 'jobMetadata'
  TableDataImportJobMetadata ->
  DescribeTableDataImportJobResponse
newDescribeTableDataImportJobResponse
  pHttpStatus_
  pJobStatus_
  pMessage_
  pJobMetadata_ =
    DescribeTableDataImportJobResponse'
      { errorCode =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        jobStatus = pJobStatus_,
        message = pMessage_,
        jobMetadata = pJobMetadata_
      }

-- | If job status is failed, error code to understand reason for the
-- failure.
describeTableDataImportJobResponse_errorCode :: Lens.Lens' DescribeTableDataImportJobResponse (Prelude.Maybe ErrorCode)
describeTableDataImportJobResponse_errorCode = Lens.lens (\DescribeTableDataImportJobResponse' {errorCode} -> errorCode) (\s@DescribeTableDataImportJobResponse' {} a -> s {errorCode = a} :: DescribeTableDataImportJobResponse)

-- | The response's http status code.
describeTableDataImportJobResponse_httpStatus :: Lens.Lens' DescribeTableDataImportJobResponse Prelude.Int
describeTableDataImportJobResponse_httpStatus = Lens.lens (\DescribeTableDataImportJobResponse' {httpStatus} -> httpStatus) (\s@DescribeTableDataImportJobResponse' {} a -> s {httpStatus = a} :: DescribeTableDataImportJobResponse)

-- | The current status of the import job.
describeTableDataImportJobResponse_jobStatus :: Lens.Lens' DescribeTableDataImportJobResponse TableDataImportJobStatus
describeTableDataImportJobResponse_jobStatus = Lens.lens (\DescribeTableDataImportJobResponse' {jobStatus} -> jobStatus) (\s@DescribeTableDataImportJobResponse' {} a -> s {jobStatus = a} :: DescribeTableDataImportJobResponse)

-- | A message providing more details about the current status of the import
-- job.
describeTableDataImportJobResponse_message :: Lens.Lens' DescribeTableDataImportJobResponse Prelude.Text
describeTableDataImportJobResponse_message = Lens.lens (\DescribeTableDataImportJobResponse' {message} -> message) (\s@DescribeTableDataImportJobResponse' {} a -> s {message = a} :: DescribeTableDataImportJobResponse)

-- | The metadata about the job that was submitted for import.
describeTableDataImportJobResponse_jobMetadata :: Lens.Lens' DescribeTableDataImportJobResponse TableDataImportJobMetadata
describeTableDataImportJobResponse_jobMetadata = Lens.lens (\DescribeTableDataImportJobResponse' {jobMetadata} -> jobMetadata) (\s@DescribeTableDataImportJobResponse' {} a -> s {jobMetadata = a} :: DescribeTableDataImportJobResponse)

instance
  Prelude.NFData
    DescribeTableDataImportJobResponse
  where
  rnf DescribeTableDataImportJobResponse' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf jobMetadata
