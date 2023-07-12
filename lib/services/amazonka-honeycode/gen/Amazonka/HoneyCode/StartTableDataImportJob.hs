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
-- Module      : Amazonka.HoneyCode.StartTableDataImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The StartTableDataImportJob API allows you to start an import job on a
-- table. This API will only return the id of the job that was started. To
-- find out the status of the import request, you need to call the
-- DescribeTableDataImportJob API.
module Amazonka.HoneyCode.StartTableDataImportJob
  ( -- * Creating a Request
    StartTableDataImportJob (..),
    newStartTableDataImportJob,

    -- * Request Lenses
    startTableDataImportJob_workbookId,
    startTableDataImportJob_dataSource,
    startTableDataImportJob_dataFormat,
    startTableDataImportJob_destinationTableId,
    startTableDataImportJob_importOptions,
    startTableDataImportJob_clientRequestToken,

    -- * Destructuring the Response
    StartTableDataImportJobResponse (..),
    newStartTableDataImportJobResponse,

    -- * Response Lenses
    startTableDataImportJobResponse_httpStatus,
    startTableDataImportJobResponse_jobId,
    startTableDataImportJobResponse_jobStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartTableDataImportJob' smart constructor.
data StartTableDataImportJob = StartTableDataImportJob'
  { -- | The ID of the workbook where the rows are being imported.
    --
    -- If a workbook with the specified id could not be found, this API throws
    -- ResourceNotFoundException.
    workbookId :: Prelude.Text,
    -- | The source of the data that is being imported. The size of source must
    -- be no larger than 100 MB. Source must have no more than 100,000 cells
    -- and no more than 1,000 rows.
    dataSource :: ImportDataSource,
    -- | The format of the data that is being imported. Currently the only option
    -- supported is \"DELIMITED_TEXT\".
    dataFormat :: ImportSourceDataFormat,
    -- | The ID of the table where the rows are being imported.
    --
    -- If a table with the specified id could not be found, this API throws
    -- ResourceNotFoundException.
    destinationTableId :: Prelude.Text,
    -- | The options for customizing this import request.
    importOptions :: ImportOptions,
    -- | The request token for performing the update action. Request tokens help
    -- to identify duplicate requests. If a call times out or fails due to a
    -- transient error like a failed network connection, you can retry the call
    -- with the same request token. The service ensures that if the first call
    -- using that request token is successfully performed, the second call will
    -- not perform the action again.
    --
    -- Note that request tokens are valid only for a few minutes. You cannot
    -- use request tokens to dedupe requests spanning hours or days.
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTableDataImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workbookId', 'startTableDataImportJob_workbookId' - The ID of the workbook where the rows are being imported.
--
-- If a workbook with the specified id could not be found, this API throws
-- ResourceNotFoundException.
--
-- 'dataSource', 'startTableDataImportJob_dataSource' - The source of the data that is being imported. The size of source must
-- be no larger than 100 MB. Source must have no more than 100,000 cells
-- and no more than 1,000 rows.
--
-- 'dataFormat', 'startTableDataImportJob_dataFormat' - The format of the data that is being imported. Currently the only option
-- supported is \"DELIMITED_TEXT\".
--
-- 'destinationTableId', 'startTableDataImportJob_destinationTableId' - The ID of the table where the rows are being imported.
--
-- If a table with the specified id could not be found, this API throws
-- ResourceNotFoundException.
--
-- 'importOptions', 'startTableDataImportJob_importOptions' - The options for customizing this import request.
--
-- 'clientRequestToken', 'startTableDataImportJob_clientRequestToken' - The request token for performing the update action. Request tokens help
-- to identify duplicate requests. If a call times out or fails due to a
-- transient error like a failed network connection, you can retry the call
-- with the same request token. The service ensures that if the first call
-- using that request token is successfully performed, the second call will
-- not perform the action again.
--
-- Note that request tokens are valid only for a few minutes. You cannot
-- use request tokens to dedupe requests spanning hours or days.
newStartTableDataImportJob ::
  -- | 'workbookId'
  Prelude.Text ->
  -- | 'dataSource'
  ImportDataSource ->
  -- | 'dataFormat'
  ImportSourceDataFormat ->
  -- | 'destinationTableId'
  Prelude.Text ->
  -- | 'importOptions'
  ImportOptions ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  StartTableDataImportJob
newStartTableDataImportJob
  pWorkbookId_
  pDataSource_
  pDataFormat_
  pDestinationTableId_
  pImportOptions_
  pClientRequestToken_ =
    StartTableDataImportJob'
      { workbookId = pWorkbookId_,
        dataSource = pDataSource_,
        dataFormat = pDataFormat_,
        destinationTableId = pDestinationTableId_,
        importOptions = pImportOptions_,
        clientRequestToken = pClientRequestToken_
      }

-- | The ID of the workbook where the rows are being imported.
--
-- If a workbook with the specified id could not be found, this API throws
-- ResourceNotFoundException.
startTableDataImportJob_workbookId :: Lens.Lens' StartTableDataImportJob Prelude.Text
startTableDataImportJob_workbookId = Lens.lens (\StartTableDataImportJob' {workbookId} -> workbookId) (\s@StartTableDataImportJob' {} a -> s {workbookId = a} :: StartTableDataImportJob)

-- | The source of the data that is being imported. The size of source must
-- be no larger than 100 MB. Source must have no more than 100,000 cells
-- and no more than 1,000 rows.
startTableDataImportJob_dataSource :: Lens.Lens' StartTableDataImportJob ImportDataSource
startTableDataImportJob_dataSource = Lens.lens (\StartTableDataImportJob' {dataSource} -> dataSource) (\s@StartTableDataImportJob' {} a -> s {dataSource = a} :: StartTableDataImportJob)

-- | The format of the data that is being imported. Currently the only option
-- supported is \"DELIMITED_TEXT\".
startTableDataImportJob_dataFormat :: Lens.Lens' StartTableDataImportJob ImportSourceDataFormat
startTableDataImportJob_dataFormat = Lens.lens (\StartTableDataImportJob' {dataFormat} -> dataFormat) (\s@StartTableDataImportJob' {} a -> s {dataFormat = a} :: StartTableDataImportJob)

-- | The ID of the table where the rows are being imported.
--
-- If a table with the specified id could not be found, this API throws
-- ResourceNotFoundException.
startTableDataImportJob_destinationTableId :: Lens.Lens' StartTableDataImportJob Prelude.Text
startTableDataImportJob_destinationTableId = Lens.lens (\StartTableDataImportJob' {destinationTableId} -> destinationTableId) (\s@StartTableDataImportJob' {} a -> s {destinationTableId = a} :: StartTableDataImportJob)

-- | The options for customizing this import request.
startTableDataImportJob_importOptions :: Lens.Lens' StartTableDataImportJob ImportOptions
startTableDataImportJob_importOptions = Lens.lens (\StartTableDataImportJob' {importOptions} -> importOptions) (\s@StartTableDataImportJob' {} a -> s {importOptions = a} :: StartTableDataImportJob)

-- | The request token for performing the update action. Request tokens help
-- to identify duplicate requests. If a call times out or fails due to a
-- transient error like a failed network connection, you can retry the call
-- with the same request token. The service ensures that if the first call
-- using that request token is successfully performed, the second call will
-- not perform the action again.
--
-- Note that request tokens are valid only for a few minutes. You cannot
-- use request tokens to dedupe requests spanning hours or days.
startTableDataImportJob_clientRequestToken :: Lens.Lens' StartTableDataImportJob Prelude.Text
startTableDataImportJob_clientRequestToken = Lens.lens (\StartTableDataImportJob' {clientRequestToken} -> clientRequestToken) (\s@StartTableDataImportJob' {} a -> s {clientRequestToken = a} :: StartTableDataImportJob)

instance Core.AWSRequest StartTableDataImportJob where
  type
    AWSResponse StartTableDataImportJob =
      StartTableDataImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTableDataImportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "jobId")
            Prelude.<*> (x Data..:> "jobStatus")
      )

instance Prelude.Hashable StartTableDataImportJob where
  hashWithSalt _salt StartTableDataImportJob' {..} =
    _salt
      `Prelude.hashWithSalt` workbookId
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` dataFormat
      `Prelude.hashWithSalt` destinationTableId
      `Prelude.hashWithSalt` importOptions
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData StartTableDataImportJob where
  rnf StartTableDataImportJob' {..} =
    Prelude.rnf workbookId
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf dataFormat
      `Prelude.seq` Prelude.rnf destinationTableId
      `Prelude.seq` Prelude.rnf importOptions
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders StartTableDataImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartTableDataImportJob where
  toJSON StartTableDataImportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("dataSource" Data..= dataSource),
            Prelude.Just ("dataFormat" Data..= dataFormat),
            Prelude.Just ("importOptions" Data..= importOptions),
            Prelude.Just
              ("clientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath StartTableDataImportJob where
  toPath StartTableDataImportJob' {..} =
    Prelude.mconcat
      [ "/workbooks/",
        Data.toBS workbookId,
        "/tables/",
        Data.toBS destinationTableId,
        "/import"
      ]

instance Data.ToQuery StartTableDataImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartTableDataImportJobResponse' smart constructor.
data StartTableDataImportJobResponse = StartTableDataImportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The id that is assigned to this import job. Future requests to find out
    -- the status of this import job need to send this id in the appropriate
    -- parameter in the request.
    jobId :: Prelude.Text,
    -- | The status of the import job immediately after submitting the request.
    jobStatus :: TableDataImportJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTableDataImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startTableDataImportJobResponse_httpStatus' - The response's http status code.
--
-- 'jobId', 'startTableDataImportJobResponse_jobId' - The id that is assigned to this import job. Future requests to find out
-- the status of this import job need to send this id in the appropriate
-- parameter in the request.
--
-- 'jobStatus', 'startTableDataImportJobResponse_jobStatus' - The status of the import job immediately after submitting the request.
newStartTableDataImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobId'
  Prelude.Text ->
  -- | 'jobStatus'
  TableDataImportJobStatus ->
  StartTableDataImportJobResponse
newStartTableDataImportJobResponse
  pHttpStatus_
  pJobId_
  pJobStatus_ =
    StartTableDataImportJobResponse'
      { httpStatus =
          pHttpStatus_,
        jobId = pJobId_,
        jobStatus = pJobStatus_
      }

-- | The response's http status code.
startTableDataImportJobResponse_httpStatus :: Lens.Lens' StartTableDataImportJobResponse Prelude.Int
startTableDataImportJobResponse_httpStatus = Lens.lens (\StartTableDataImportJobResponse' {httpStatus} -> httpStatus) (\s@StartTableDataImportJobResponse' {} a -> s {httpStatus = a} :: StartTableDataImportJobResponse)

-- | The id that is assigned to this import job. Future requests to find out
-- the status of this import job need to send this id in the appropriate
-- parameter in the request.
startTableDataImportJobResponse_jobId :: Lens.Lens' StartTableDataImportJobResponse Prelude.Text
startTableDataImportJobResponse_jobId = Lens.lens (\StartTableDataImportJobResponse' {jobId} -> jobId) (\s@StartTableDataImportJobResponse' {} a -> s {jobId = a} :: StartTableDataImportJobResponse)

-- | The status of the import job immediately after submitting the request.
startTableDataImportJobResponse_jobStatus :: Lens.Lens' StartTableDataImportJobResponse TableDataImportJobStatus
startTableDataImportJobResponse_jobStatus = Lens.lens (\StartTableDataImportJobResponse' {jobStatus} -> jobStatus) (\s@StartTableDataImportJobResponse' {} a -> s {jobStatus = a} :: StartTableDataImportJobResponse)

instance
  Prelude.NFData
    StartTableDataImportJobResponse
  where
  rnf StartTableDataImportJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobStatus
