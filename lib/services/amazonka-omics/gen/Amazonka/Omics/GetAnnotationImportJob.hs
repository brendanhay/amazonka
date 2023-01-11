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
-- Module      : Amazonka.Omics.GetAnnotationImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an annotation import job.
module Amazonka.Omics.GetAnnotationImportJob
  ( -- * Creating a Request
    GetAnnotationImportJob (..),
    newGetAnnotationImportJob,

    -- * Request Lenses
    getAnnotationImportJob_jobId,

    -- * Destructuring the Response
    GetAnnotationImportJobResponse (..),
    newGetAnnotationImportJobResponse,

    -- * Response Lenses
    getAnnotationImportJobResponse_httpStatus,
    getAnnotationImportJobResponse_completionTime,
    getAnnotationImportJobResponse_creationTime,
    getAnnotationImportJobResponse_destinationName,
    getAnnotationImportJobResponse_formatOptions,
    getAnnotationImportJobResponse_id,
    getAnnotationImportJobResponse_items,
    getAnnotationImportJobResponse_roleArn,
    getAnnotationImportJobResponse_runLeftNormalization,
    getAnnotationImportJobResponse_status,
    getAnnotationImportJobResponse_statusMessage,
    getAnnotationImportJobResponse_updateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAnnotationImportJob' smart constructor.
data GetAnnotationImportJob = GetAnnotationImportJob'
  { -- | The job\'s ID.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAnnotationImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'getAnnotationImportJob_jobId' - The job\'s ID.
newGetAnnotationImportJob ::
  -- | 'jobId'
  Prelude.Text ->
  GetAnnotationImportJob
newGetAnnotationImportJob pJobId_ =
  GetAnnotationImportJob' {jobId = pJobId_}

-- | The job\'s ID.
getAnnotationImportJob_jobId :: Lens.Lens' GetAnnotationImportJob Prelude.Text
getAnnotationImportJob_jobId = Lens.lens (\GetAnnotationImportJob' {jobId} -> jobId) (\s@GetAnnotationImportJob' {} a -> s {jobId = a} :: GetAnnotationImportJob)

instance Core.AWSRequest GetAnnotationImportJob where
  type
    AWSResponse GetAnnotationImportJob =
      GetAnnotationImportJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAnnotationImportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "completionTime")
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "destinationName")
            Prelude.<*> (x Data..:> "formatOptions")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "items")
            Prelude.<*> (x Data..:> "roleArn")
            Prelude.<*> (x Data..:> "runLeftNormalization")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "statusMessage")
            Prelude.<*> (x Data..:> "updateTime")
      )

instance Prelude.Hashable GetAnnotationImportJob where
  hashWithSalt _salt GetAnnotationImportJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetAnnotationImportJob where
  rnf GetAnnotationImportJob' {..} = Prelude.rnf jobId

instance Data.ToHeaders GetAnnotationImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAnnotationImportJob where
  toPath GetAnnotationImportJob' {..} =
    Prelude.mconcat
      ["/import/annotation/", Data.toBS jobId]

instance Data.ToQuery GetAnnotationImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAnnotationImportJobResponse' smart constructor.
data GetAnnotationImportJobResponse = GetAnnotationImportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | When the job completed.
    completionTime :: Data.ISO8601,
    -- | When the job was created.
    creationTime :: Data.ISO8601,
    -- | The job\'s destination annotation store.
    destinationName :: Prelude.Text,
    formatOptions :: FormatOptions,
    -- | The job\'s ID.
    id :: Prelude.Text,
    -- | The job\'s imported items.
    items :: Prelude.NonEmpty AnnotationImportItemDetail,
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
-- Create a value of 'GetAnnotationImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getAnnotationImportJobResponse_httpStatus' - The response's http status code.
--
-- 'completionTime', 'getAnnotationImportJobResponse_completionTime' - When the job completed.
--
-- 'creationTime', 'getAnnotationImportJobResponse_creationTime' - When the job was created.
--
-- 'destinationName', 'getAnnotationImportJobResponse_destinationName' - The job\'s destination annotation store.
--
-- 'formatOptions', 'getAnnotationImportJobResponse_formatOptions' - Undocumented member.
--
-- 'id', 'getAnnotationImportJobResponse_id' - The job\'s ID.
--
-- 'items', 'getAnnotationImportJobResponse_items' - The job\'s imported items.
--
-- 'roleArn', 'getAnnotationImportJobResponse_roleArn' - The job\'s service role ARN.
--
-- 'runLeftNormalization', 'getAnnotationImportJobResponse_runLeftNormalization' - The job\'s left normalization setting.
--
-- 'status', 'getAnnotationImportJobResponse_status' - The job\'s status.
--
-- 'statusMessage', 'getAnnotationImportJobResponse_statusMessage' - The job\'s status message.
--
-- 'updateTime', 'getAnnotationImportJobResponse_updateTime' - When the job was updated.
newGetAnnotationImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'completionTime'
  Prelude.UTCTime ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'destinationName'
  Prelude.Text ->
  -- | 'formatOptions'
  FormatOptions ->
  -- | 'id'
  Prelude.Text ->
  -- | 'items'
  Prelude.NonEmpty AnnotationImportItemDetail ->
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
  GetAnnotationImportJobResponse
newGetAnnotationImportJobResponse
  pHttpStatus_
  pCompletionTime_
  pCreationTime_
  pDestinationName_
  pFormatOptions_
  pId_
  pItems_
  pRoleArn_
  pRunLeftNormalization_
  pStatus_
  pStatusMessage_
  pUpdateTime_ =
    GetAnnotationImportJobResponse'
      { httpStatus =
          pHttpStatus_,
        completionTime =
          Data._Time Lens.# pCompletionTime_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        destinationName = pDestinationName_,
        formatOptions = pFormatOptions_,
        id = pId_,
        items = Lens.coerced Lens.# pItems_,
        roleArn = pRoleArn_,
        runLeftNormalization =
          pRunLeftNormalization_,
        status = pStatus_,
        statusMessage = pStatusMessage_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | The response's http status code.
getAnnotationImportJobResponse_httpStatus :: Lens.Lens' GetAnnotationImportJobResponse Prelude.Int
getAnnotationImportJobResponse_httpStatus = Lens.lens (\GetAnnotationImportJobResponse' {httpStatus} -> httpStatus) (\s@GetAnnotationImportJobResponse' {} a -> s {httpStatus = a} :: GetAnnotationImportJobResponse)

-- | When the job completed.
getAnnotationImportJobResponse_completionTime :: Lens.Lens' GetAnnotationImportJobResponse Prelude.UTCTime
getAnnotationImportJobResponse_completionTime = Lens.lens (\GetAnnotationImportJobResponse' {completionTime} -> completionTime) (\s@GetAnnotationImportJobResponse' {} a -> s {completionTime = a} :: GetAnnotationImportJobResponse) Prelude.. Data._Time

-- | When the job was created.
getAnnotationImportJobResponse_creationTime :: Lens.Lens' GetAnnotationImportJobResponse Prelude.UTCTime
getAnnotationImportJobResponse_creationTime = Lens.lens (\GetAnnotationImportJobResponse' {creationTime} -> creationTime) (\s@GetAnnotationImportJobResponse' {} a -> s {creationTime = a} :: GetAnnotationImportJobResponse) Prelude.. Data._Time

-- | The job\'s destination annotation store.
getAnnotationImportJobResponse_destinationName :: Lens.Lens' GetAnnotationImportJobResponse Prelude.Text
getAnnotationImportJobResponse_destinationName = Lens.lens (\GetAnnotationImportJobResponse' {destinationName} -> destinationName) (\s@GetAnnotationImportJobResponse' {} a -> s {destinationName = a} :: GetAnnotationImportJobResponse)

-- | Undocumented member.
getAnnotationImportJobResponse_formatOptions :: Lens.Lens' GetAnnotationImportJobResponse FormatOptions
getAnnotationImportJobResponse_formatOptions = Lens.lens (\GetAnnotationImportJobResponse' {formatOptions} -> formatOptions) (\s@GetAnnotationImportJobResponse' {} a -> s {formatOptions = a} :: GetAnnotationImportJobResponse)

-- | The job\'s ID.
getAnnotationImportJobResponse_id :: Lens.Lens' GetAnnotationImportJobResponse Prelude.Text
getAnnotationImportJobResponse_id = Lens.lens (\GetAnnotationImportJobResponse' {id} -> id) (\s@GetAnnotationImportJobResponse' {} a -> s {id = a} :: GetAnnotationImportJobResponse)

-- | The job\'s imported items.
getAnnotationImportJobResponse_items :: Lens.Lens' GetAnnotationImportJobResponse (Prelude.NonEmpty AnnotationImportItemDetail)
getAnnotationImportJobResponse_items = Lens.lens (\GetAnnotationImportJobResponse' {items} -> items) (\s@GetAnnotationImportJobResponse' {} a -> s {items = a} :: GetAnnotationImportJobResponse) Prelude.. Lens.coerced

-- | The job\'s service role ARN.
getAnnotationImportJobResponse_roleArn :: Lens.Lens' GetAnnotationImportJobResponse Prelude.Text
getAnnotationImportJobResponse_roleArn = Lens.lens (\GetAnnotationImportJobResponse' {roleArn} -> roleArn) (\s@GetAnnotationImportJobResponse' {} a -> s {roleArn = a} :: GetAnnotationImportJobResponse)

-- | The job\'s left normalization setting.
getAnnotationImportJobResponse_runLeftNormalization :: Lens.Lens' GetAnnotationImportJobResponse Prelude.Bool
getAnnotationImportJobResponse_runLeftNormalization = Lens.lens (\GetAnnotationImportJobResponse' {runLeftNormalization} -> runLeftNormalization) (\s@GetAnnotationImportJobResponse' {} a -> s {runLeftNormalization = a} :: GetAnnotationImportJobResponse)

-- | The job\'s status.
getAnnotationImportJobResponse_status :: Lens.Lens' GetAnnotationImportJobResponse JobStatus
getAnnotationImportJobResponse_status = Lens.lens (\GetAnnotationImportJobResponse' {status} -> status) (\s@GetAnnotationImportJobResponse' {} a -> s {status = a} :: GetAnnotationImportJobResponse)

-- | The job\'s status message.
getAnnotationImportJobResponse_statusMessage :: Lens.Lens' GetAnnotationImportJobResponse Prelude.Text
getAnnotationImportJobResponse_statusMessage = Lens.lens (\GetAnnotationImportJobResponse' {statusMessage} -> statusMessage) (\s@GetAnnotationImportJobResponse' {} a -> s {statusMessage = a} :: GetAnnotationImportJobResponse)

-- | When the job was updated.
getAnnotationImportJobResponse_updateTime :: Lens.Lens' GetAnnotationImportJobResponse Prelude.UTCTime
getAnnotationImportJobResponse_updateTime = Lens.lens (\GetAnnotationImportJobResponse' {updateTime} -> updateTime) (\s@GetAnnotationImportJobResponse' {} a -> s {updateTime = a} :: GetAnnotationImportJobResponse) Prelude.. Data._Time

instance
  Prelude.NFData
    GetAnnotationImportJobResponse
  where
  rnf GetAnnotationImportJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf destinationName
      `Prelude.seq` Prelude.rnf formatOptions
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf items
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf runLeftNormalization
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf updateTime
