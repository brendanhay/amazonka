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
-- Module      : Amazonka.Omics.GetVariantImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a variant import job.
module Amazonka.Omics.GetVariantImportJob
  ( -- * Creating a Request
    GetVariantImportJob (..),
    newGetVariantImportJob,

    -- * Request Lenses
    getVariantImportJob_jobId,

    -- * Destructuring the Response
    GetVariantImportJobResponse (..),
    newGetVariantImportJobResponse,

    -- * Response Lenses
    getVariantImportJobResponse_completionTime,
    getVariantImportJobResponse_httpStatus,
    getVariantImportJobResponse_creationTime,
    getVariantImportJobResponse_destinationName,
    getVariantImportJobResponse_id,
    getVariantImportJobResponse_items,
    getVariantImportJobResponse_roleArn,
    getVariantImportJobResponse_runLeftNormalization,
    getVariantImportJobResponse_status,
    getVariantImportJobResponse_statusMessage,
    getVariantImportJobResponse_updateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVariantImportJob' smart constructor.
data GetVariantImportJob = GetVariantImportJob'
  { -- | The job\'s ID.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVariantImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'getVariantImportJob_jobId' - The job\'s ID.
newGetVariantImportJob ::
  -- | 'jobId'
  Prelude.Text ->
  GetVariantImportJob
newGetVariantImportJob pJobId_ =
  GetVariantImportJob' {jobId = pJobId_}

-- | The job\'s ID.
getVariantImportJob_jobId :: Lens.Lens' GetVariantImportJob Prelude.Text
getVariantImportJob_jobId = Lens.lens (\GetVariantImportJob' {jobId} -> jobId) (\s@GetVariantImportJob' {} a -> s {jobId = a} :: GetVariantImportJob)

instance Core.AWSRequest GetVariantImportJob where
  type
    AWSResponse GetVariantImportJob =
      GetVariantImportJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVariantImportJobResponse'
            Prelude.<$> (x Data..?> "completionTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "destinationName")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "items")
            Prelude.<*> (x Data..:> "roleArn")
            Prelude.<*> (x Data..:> "runLeftNormalization")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "statusMessage")
            Prelude.<*> (x Data..:> "updateTime")
      )

instance Prelude.Hashable GetVariantImportJob where
  hashWithSalt _salt GetVariantImportJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetVariantImportJob where
  rnf GetVariantImportJob' {..} = Prelude.rnf jobId

instance Data.ToHeaders GetVariantImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetVariantImportJob where
  toPath GetVariantImportJob' {..} =
    Prelude.mconcat
      ["/import/variant/", Data.toBS jobId]

instance Data.ToQuery GetVariantImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVariantImportJobResponse' smart constructor.
data GetVariantImportJobResponse = GetVariantImportJobResponse'
  { -- | When the job completed.
    completionTime :: Prelude.Maybe Data.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | When the job was created.
    creationTime :: Data.ISO8601,
    -- | The job\'s destination variant store.
    destinationName :: Prelude.Text,
    -- | The job\'s ID.
    id :: Prelude.Text,
    -- | The job\'s items.
    items :: Prelude.NonEmpty VariantImportItemDetail,
    -- | The job\'s service role ARN.
    roleArn :: Prelude.Text,
    -- | The job\'s left normalization setting.
    runLeftNormalization :: Prelude.Bool,
    -- | The job\'s status.
    status :: JobStatus,
    -- | The job\'s status message.
    statusMessage :: Prelude.Text,
    -- | When the job was updated.
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVariantImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'getVariantImportJobResponse_completionTime' - When the job completed.
--
-- 'httpStatus', 'getVariantImportJobResponse_httpStatus' - The response's http status code.
--
-- 'creationTime', 'getVariantImportJobResponse_creationTime' - When the job was created.
--
-- 'destinationName', 'getVariantImportJobResponse_destinationName' - The job\'s destination variant store.
--
-- 'id', 'getVariantImportJobResponse_id' - The job\'s ID.
--
-- 'items', 'getVariantImportJobResponse_items' - The job\'s items.
--
-- 'roleArn', 'getVariantImportJobResponse_roleArn' - The job\'s service role ARN.
--
-- 'runLeftNormalization', 'getVariantImportJobResponse_runLeftNormalization' - The job\'s left normalization setting.
--
-- 'status', 'getVariantImportJobResponse_status' - The job\'s status.
--
-- 'statusMessage', 'getVariantImportJobResponse_statusMessage' - The job\'s status message.
--
-- 'updateTime', 'getVariantImportJobResponse_updateTime' - When the job was updated.
newGetVariantImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'destinationName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'items'
  Prelude.NonEmpty VariantImportItemDetail ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'runLeftNormalization'
  Prelude.Bool ->
  -- | 'status'
  JobStatus ->
  -- | 'statusMessage'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  GetVariantImportJobResponse
newGetVariantImportJobResponse
  pHttpStatus_
  pCreationTime_
  pDestinationName_
  pId_
  pItems_
  pRoleArn_
  pRunLeftNormalization_
  pStatus_
  pStatusMessage_
  pUpdateTime_ =
    GetVariantImportJobResponse'
      { completionTime =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        destinationName = pDestinationName_,
        id = pId_,
        items = Lens.coerced Lens.# pItems_,
        roleArn = pRoleArn_,
        runLeftNormalization = pRunLeftNormalization_,
        status = pStatus_,
        statusMessage = pStatusMessage_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | When the job completed.
getVariantImportJobResponse_completionTime :: Lens.Lens' GetVariantImportJobResponse (Prelude.Maybe Prelude.UTCTime)
getVariantImportJobResponse_completionTime = Lens.lens (\GetVariantImportJobResponse' {completionTime} -> completionTime) (\s@GetVariantImportJobResponse' {} a -> s {completionTime = a} :: GetVariantImportJobResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getVariantImportJobResponse_httpStatus :: Lens.Lens' GetVariantImportJobResponse Prelude.Int
getVariantImportJobResponse_httpStatus = Lens.lens (\GetVariantImportJobResponse' {httpStatus} -> httpStatus) (\s@GetVariantImportJobResponse' {} a -> s {httpStatus = a} :: GetVariantImportJobResponse)

-- | When the job was created.
getVariantImportJobResponse_creationTime :: Lens.Lens' GetVariantImportJobResponse Prelude.UTCTime
getVariantImportJobResponse_creationTime = Lens.lens (\GetVariantImportJobResponse' {creationTime} -> creationTime) (\s@GetVariantImportJobResponse' {} a -> s {creationTime = a} :: GetVariantImportJobResponse) Prelude.. Data._Time

-- | The job\'s destination variant store.
getVariantImportJobResponse_destinationName :: Lens.Lens' GetVariantImportJobResponse Prelude.Text
getVariantImportJobResponse_destinationName = Lens.lens (\GetVariantImportJobResponse' {destinationName} -> destinationName) (\s@GetVariantImportJobResponse' {} a -> s {destinationName = a} :: GetVariantImportJobResponse)

-- | The job\'s ID.
getVariantImportJobResponse_id :: Lens.Lens' GetVariantImportJobResponse Prelude.Text
getVariantImportJobResponse_id = Lens.lens (\GetVariantImportJobResponse' {id} -> id) (\s@GetVariantImportJobResponse' {} a -> s {id = a} :: GetVariantImportJobResponse)

-- | The job\'s items.
getVariantImportJobResponse_items :: Lens.Lens' GetVariantImportJobResponse (Prelude.NonEmpty VariantImportItemDetail)
getVariantImportJobResponse_items = Lens.lens (\GetVariantImportJobResponse' {items} -> items) (\s@GetVariantImportJobResponse' {} a -> s {items = a} :: GetVariantImportJobResponse) Prelude.. Lens.coerced

-- | The job\'s service role ARN.
getVariantImportJobResponse_roleArn :: Lens.Lens' GetVariantImportJobResponse Prelude.Text
getVariantImportJobResponse_roleArn = Lens.lens (\GetVariantImportJobResponse' {roleArn} -> roleArn) (\s@GetVariantImportJobResponse' {} a -> s {roleArn = a} :: GetVariantImportJobResponse)

-- | The job\'s left normalization setting.
getVariantImportJobResponse_runLeftNormalization :: Lens.Lens' GetVariantImportJobResponse Prelude.Bool
getVariantImportJobResponse_runLeftNormalization = Lens.lens (\GetVariantImportJobResponse' {runLeftNormalization} -> runLeftNormalization) (\s@GetVariantImportJobResponse' {} a -> s {runLeftNormalization = a} :: GetVariantImportJobResponse)

-- | The job\'s status.
getVariantImportJobResponse_status :: Lens.Lens' GetVariantImportJobResponse JobStatus
getVariantImportJobResponse_status = Lens.lens (\GetVariantImportJobResponse' {status} -> status) (\s@GetVariantImportJobResponse' {} a -> s {status = a} :: GetVariantImportJobResponse)

-- | The job\'s status message.
getVariantImportJobResponse_statusMessage :: Lens.Lens' GetVariantImportJobResponse Prelude.Text
getVariantImportJobResponse_statusMessage = Lens.lens (\GetVariantImportJobResponse' {statusMessage} -> statusMessage) (\s@GetVariantImportJobResponse' {} a -> s {statusMessage = a} :: GetVariantImportJobResponse)

-- | When the job was updated.
getVariantImportJobResponse_updateTime :: Lens.Lens' GetVariantImportJobResponse Prelude.UTCTime
getVariantImportJobResponse_updateTime = Lens.lens (\GetVariantImportJobResponse' {updateTime} -> updateTime) (\s@GetVariantImportJobResponse' {} a -> s {updateTime = a} :: GetVariantImportJobResponse) Prelude.. Data._Time

instance Prelude.NFData GetVariantImportJobResponse where
  rnf GetVariantImportJobResponse' {..} =
    Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf destinationName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf items
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf runLeftNormalization
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf updateTime
