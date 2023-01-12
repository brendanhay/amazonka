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
-- Module      : Amazonka.LookoutEquipment.StartDataIngestionJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a data ingestion job. Amazon Lookout for Equipment returns the
-- job status.
module Amazonka.LookoutEquipment.StartDataIngestionJob
  ( -- * Creating a Request
    StartDataIngestionJob (..),
    newStartDataIngestionJob,

    -- * Request Lenses
    startDataIngestionJob_datasetName,
    startDataIngestionJob_ingestionInputConfiguration,
    startDataIngestionJob_roleArn,
    startDataIngestionJob_clientToken,

    -- * Destructuring the Response
    StartDataIngestionJobResponse (..),
    newStartDataIngestionJobResponse,

    -- * Response Lenses
    startDataIngestionJobResponse_jobId,
    startDataIngestionJobResponse_status,
    startDataIngestionJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartDataIngestionJob' smart constructor.
data StartDataIngestionJob = StartDataIngestionJob'
  { -- | The name of the dataset being used by the data ingestion job.
    datasetName :: Prelude.Text,
    -- | Specifies information for the input data for the data ingestion job,
    -- including dataset S3 location.
    ingestionInputConfiguration :: IngestionInputConfiguration,
    -- | The Amazon Resource Name (ARN) of a role with permission to access the
    -- data source for the data ingestion job.
    roleArn :: Prelude.Text,
    -- | A unique identifier for the request. If you do not set the client
    -- request token, Amazon Lookout for Equipment generates one.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDataIngestionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetName', 'startDataIngestionJob_datasetName' - The name of the dataset being used by the data ingestion job.
--
-- 'ingestionInputConfiguration', 'startDataIngestionJob_ingestionInputConfiguration' - Specifies information for the input data for the data ingestion job,
-- including dataset S3 location.
--
-- 'roleArn', 'startDataIngestionJob_roleArn' - The Amazon Resource Name (ARN) of a role with permission to access the
-- data source for the data ingestion job.
--
-- 'clientToken', 'startDataIngestionJob_clientToken' - A unique identifier for the request. If you do not set the client
-- request token, Amazon Lookout for Equipment generates one.
newStartDataIngestionJob ::
  -- | 'datasetName'
  Prelude.Text ->
  -- | 'ingestionInputConfiguration'
  IngestionInputConfiguration ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  StartDataIngestionJob
newStartDataIngestionJob
  pDatasetName_
  pIngestionInputConfiguration_
  pRoleArn_
  pClientToken_ =
    StartDataIngestionJob'
      { datasetName = pDatasetName_,
        ingestionInputConfiguration =
          pIngestionInputConfiguration_,
        roleArn = pRoleArn_,
        clientToken = pClientToken_
      }

-- | The name of the dataset being used by the data ingestion job.
startDataIngestionJob_datasetName :: Lens.Lens' StartDataIngestionJob Prelude.Text
startDataIngestionJob_datasetName = Lens.lens (\StartDataIngestionJob' {datasetName} -> datasetName) (\s@StartDataIngestionJob' {} a -> s {datasetName = a} :: StartDataIngestionJob)

-- | Specifies information for the input data for the data ingestion job,
-- including dataset S3 location.
startDataIngestionJob_ingestionInputConfiguration :: Lens.Lens' StartDataIngestionJob IngestionInputConfiguration
startDataIngestionJob_ingestionInputConfiguration = Lens.lens (\StartDataIngestionJob' {ingestionInputConfiguration} -> ingestionInputConfiguration) (\s@StartDataIngestionJob' {} a -> s {ingestionInputConfiguration = a} :: StartDataIngestionJob)

-- | The Amazon Resource Name (ARN) of a role with permission to access the
-- data source for the data ingestion job.
startDataIngestionJob_roleArn :: Lens.Lens' StartDataIngestionJob Prelude.Text
startDataIngestionJob_roleArn = Lens.lens (\StartDataIngestionJob' {roleArn} -> roleArn) (\s@StartDataIngestionJob' {} a -> s {roleArn = a} :: StartDataIngestionJob)

-- | A unique identifier for the request. If you do not set the client
-- request token, Amazon Lookout for Equipment generates one.
startDataIngestionJob_clientToken :: Lens.Lens' StartDataIngestionJob Prelude.Text
startDataIngestionJob_clientToken = Lens.lens (\StartDataIngestionJob' {clientToken} -> clientToken) (\s@StartDataIngestionJob' {} a -> s {clientToken = a} :: StartDataIngestionJob)

instance Core.AWSRequest StartDataIngestionJob where
  type
    AWSResponse StartDataIngestionJob =
      StartDataIngestionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDataIngestionJobResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartDataIngestionJob where
  hashWithSalt _salt StartDataIngestionJob' {..} =
    _salt `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` ingestionInputConfiguration
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData StartDataIngestionJob where
  rnf StartDataIngestionJob' {..} =
    Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf ingestionInputConfiguration
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders StartDataIngestionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.StartDataIngestionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartDataIngestionJob where
  toJSON StartDataIngestionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DatasetName" Data..= datasetName),
            Prelude.Just
              ( "IngestionInputConfiguration"
                  Data..= ingestionInputConfiguration
              ),
            Prelude.Just ("RoleArn" Data..= roleArn),
            Prelude.Just ("ClientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath StartDataIngestionJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StartDataIngestionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartDataIngestionJobResponse' smart constructor.
data StartDataIngestionJobResponse = StartDataIngestionJobResponse'
  { -- | Indicates the job ID of the data ingestion job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | Indicates the status of the @StartDataIngestionJob@ operation.
    status :: Prelude.Maybe IngestionJobStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDataIngestionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startDataIngestionJobResponse_jobId' - Indicates the job ID of the data ingestion job.
--
-- 'status', 'startDataIngestionJobResponse_status' - Indicates the status of the @StartDataIngestionJob@ operation.
--
-- 'httpStatus', 'startDataIngestionJobResponse_httpStatus' - The response's http status code.
newStartDataIngestionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartDataIngestionJobResponse
newStartDataIngestionJobResponse pHttpStatus_ =
  StartDataIngestionJobResponse'
    { jobId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates the job ID of the data ingestion job.
startDataIngestionJobResponse_jobId :: Lens.Lens' StartDataIngestionJobResponse (Prelude.Maybe Prelude.Text)
startDataIngestionJobResponse_jobId = Lens.lens (\StartDataIngestionJobResponse' {jobId} -> jobId) (\s@StartDataIngestionJobResponse' {} a -> s {jobId = a} :: StartDataIngestionJobResponse)

-- | Indicates the status of the @StartDataIngestionJob@ operation.
startDataIngestionJobResponse_status :: Lens.Lens' StartDataIngestionJobResponse (Prelude.Maybe IngestionJobStatus)
startDataIngestionJobResponse_status = Lens.lens (\StartDataIngestionJobResponse' {status} -> status) (\s@StartDataIngestionJobResponse' {} a -> s {status = a} :: StartDataIngestionJobResponse)

-- | The response's http status code.
startDataIngestionJobResponse_httpStatus :: Lens.Lens' StartDataIngestionJobResponse Prelude.Int
startDataIngestionJobResponse_httpStatus = Lens.lens (\StartDataIngestionJobResponse' {httpStatus} -> httpStatus) (\s@StartDataIngestionJobResponse' {} a -> s {httpStatus = a} :: StartDataIngestionJobResponse)

instance Prelude.NFData StartDataIngestionJobResponse where
  rnf StartDataIngestionJobResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
