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
    getAnnotationImportJobResponse_annotationFields,
    getAnnotationImportJobResponse_httpStatus,
    getAnnotationImportJobResponse_id,
    getAnnotationImportJobResponse_destinationName,
    getAnnotationImportJobResponse_roleArn,
    getAnnotationImportJobResponse_status,
    getAnnotationImportJobResponse_statusMessage,
    getAnnotationImportJobResponse_creationTime,
    getAnnotationImportJobResponse_updateTime,
    getAnnotationImportJobResponse_completionTime,
    getAnnotationImportJobResponse_items,
    getAnnotationImportJobResponse_runLeftNormalization,
    getAnnotationImportJobResponse_formatOptions,
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
            Prelude.<$> ( x
                            Data..?> "annotationFields"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "destinationName")
            Prelude.<*> (x Data..:> "roleArn")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "statusMessage")
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "updateTime")
            Prelude.<*> (x Data..:> "completionTime")
            Prelude.<*> (x Data..:> "items")
            Prelude.<*> (x Data..:> "runLeftNormalization")
            Prelude.<*> (x Data..:> "formatOptions")
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
  { -- | The annotation schema generated by the parsed annotation data.
    annotationFields :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The job\'s ID.
    id :: Prelude.Text,
    -- | The job\'s destination annotation store.
    destinationName :: Prelude.Text,
    -- | The job\'s service role ARN.
    roleArn :: Prelude.Text,
    -- | The job\'s status.
    status :: JobStatus,
    -- | The job\'s status message.
    statusMessage :: Prelude.Text,
    -- | When the job was created.
    creationTime :: Data.ISO8601,
    -- | When the job was updated.
    updateTime :: Data.ISO8601,
    -- | When the job completed.
    completionTime :: Data.ISO8601,
    -- | The job\'s imported items.
    items :: Prelude.NonEmpty AnnotationImportItemDetail,
    -- | The job\'s left normalization setting.
    runLeftNormalization :: Prelude.Bool,
    formatOptions :: FormatOptions
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
-- 'annotationFields', 'getAnnotationImportJobResponse_annotationFields' - The annotation schema generated by the parsed annotation data.
--
-- 'httpStatus', 'getAnnotationImportJobResponse_httpStatus' - The response's http status code.
--
-- 'id', 'getAnnotationImportJobResponse_id' - The job\'s ID.
--
-- 'destinationName', 'getAnnotationImportJobResponse_destinationName' - The job\'s destination annotation store.
--
-- 'roleArn', 'getAnnotationImportJobResponse_roleArn' - The job\'s service role ARN.
--
-- 'status', 'getAnnotationImportJobResponse_status' - The job\'s status.
--
-- 'statusMessage', 'getAnnotationImportJobResponse_statusMessage' - The job\'s status message.
--
-- 'creationTime', 'getAnnotationImportJobResponse_creationTime' - When the job was created.
--
-- 'updateTime', 'getAnnotationImportJobResponse_updateTime' - When the job was updated.
--
-- 'completionTime', 'getAnnotationImportJobResponse_completionTime' - When the job completed.
--
-- 'items', 'getAnnotationImportJobResponse_items' - The job\'s imported items.
--
-- 'runLeftNormalization', 'getAnnotationImportJobResponse_runLeftNormalization' - The job\'s left normalization setting.
--
-- 'formatOptions', 'getAnnotationImportJobResponse_formatOptions' - Undocumented member.
newGetAnnotationImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  -- | 'destinationName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'status'
  JobStatus ->
  -- | 'statusMessage'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  -- | 'completionTime'
  Prelude.UTCTime ->
  -- | 'items'
  Prelude.NonEmpty AnnotationImportItemDetail ->
  -- | 'runLeftNormalization'
  Prelude.Bool ->
  -- | 'formatOptions'
  FormatOptions ->
  GetAnnotationImportJobResponse
newGetAnnotationImportJobResponse
  pHttpStatus_
  pId_
  pDestinationName_
  pRoleArn_
  pStatus_
  pStatusMessage_
  pCreationTime_
  pUpdateTime_
  pCompletionTime_
  pItems_
  pRunLeftNormalization_
  pFormatOptions_ =
    GetAnnotationImportJobResponse'
      { annotationFields =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        id = pId_,
        destinationName = pDestinationName_,
        roleArn = pRoleArn_,
        status = pStatus_,
        statusMessage = pStatusMessage_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        updateTime = Data._Time Lens.# pUpdateTime_,
        completionTime =
          Data._Time Lens.# pCompletionTime_,
        items = Lens.coerced Lens.# pItems_,
        runLeftNormalization =
          pRunLeftNormalization_,
        formatOptions = pFormatOptions_
      }

-- | The annotation schema generated by the parsed annotation data.
getAnnotationImportJobResponse_annotationFields :: Lens.Lens' GetAnnotationImportJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getAnnotationImportJobResponse_annotationFields = Lens.lens (\GetAnnotationImportJobResponse' {annotationFields} -> annotationFields) (\s@GetAnnotationImportJobResponse' {} a -> s {annotationFields = a} :: GetAnnotationImportJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getAnnotationImportJobResponse_httpStatus :: Lens.Lens' GetAnnotationImportJobResponse Prelude.Int
getAnnotationImportJobResponse_httpStatus = Lens.lens (\GetAnnotationImportJobResponse' {httpStatus} -> httpStatus) (\s@GetAnnotationImportJobResponse' {} a -> s {httpStatus = a} :: GetAnnotationImportJobResponse)

-- | The job\'s ID.
getAnnotationImportJobResponse_id :: Lens.Lens' GetAnnotationImportJobResponse Prelude.Text
getAnnotationImportJobResponse_id = Lens.lens (\GetAnnotationImportJobResponse' {id} -> id) (\s@GetAnnotationImportJobResponse' {} a -> s {id = a} :: GetAnnotationImportJobResponse)

-- | The job\'s destination annotation store.
getAnnotationImportJobResponse_destinationName :: Lens.Lens' GetAnnotationImportJobResponse Prelude.Text
getAnnotationImportJobResponse_destinationName = Lens.lens (\GetAnnotationImportJobResponse' {destinationName} -> destinationName) (\s@GetAnnotationImportJobResponse' {} a -> s {destinationName = a} :: GetAnnotationImportJobResponse)

-- | The job\'s service role ARN.
getAnnotationImportJobResponse_roleArn :: Lens.Lens' GetAnnotationImportJobResponse Prelude.Text
getAnnotationImportJobResponse_roleArn = Lens.lens (\GetAnnotationImportJobResponse' {roleArn} -> roleArn) (\s@GetAnnotationImportJobResponse' {} a -> s {roleArn = a} :: GetAnnotationImportJobResponse)

-- | The job\'s status.
getAnnotationImportJobResponse_status :: Lens.Lens' GetAnnotationImportJobResponse JobStatus
getAnnotationImportJobResponse_status = Lens.lens (\GetAnnotationImportJobResponse' {status} -> status) (\s@GetAnnotationImportJobResponse' {} a -> s {status = a} :: GetAnnotationImportJobResponse)

-- | The job\'s status message.
getAnnotationImportJobResponse_statusMessage :: Lens.Lens' GetAnnotationImportJobResponse Prelude.Text
getAnnotationImportJobResponse_statusMessage = Lens.lens (\GetAnnotationImportJobResponse' {statusMessage} -> statusMessage) (\s@GetAnnotationImportJobResponse' {} a -> s {statusMessage = a} :: GetAnnotationImportJobResponse)

-- | When the job was created.
getAnnotationImportJobResponse_creationTime :: Lens.Lens' GetAnnotationImportJobResponse Prelude.UTCTime
getAnnotationImportJobResponse_creationTime = Lens.lens (\GetAnnotationImportJobResponse' {creationTime} -> creationTime) (\s@GetAnnotationImportJobResponse' {} a -> s {creationTime = a} :: GetAnnotationImportJobResponse) Prelude.. Data._Time

-- | When the job was updated.
getAnnotationImportJobResponse_updateTime :: Lens.Lens' GetAnnotationImportJobResponse Prelude.UTCTime
getAnnotationImportJobResponse_updateTime = Lens.lens (\GetAnnotationImportJobResponse' {updateTime} -> updateTime) (\s@GetAnnotationImportJobResponse' {} a -> s {updateTime = a} :: GetAnnotationImportJobResponse) Prelude.. Data._Time

-- | When the job completed.
getAnnotationImportJobResponse_completionTime :: Lens.Lens' GetAnnotationImportJobResponse Prelude.UTCTime
getAnnotationImportJobResponse_completionTime = Lens.lens (\GetAnnotationImportJobResponse' {completionTime} -> completionTime) (\s@GetAnnotationImportJobResponse' {} a -> s {completionTime = a} :: GetAnnotationImportJobResponse) Prelude.. Data._Time

-- | The job\'s imported items.
getAnnotationImportJobResponse_items :: Lens.Lens' GetAnnotationImportJobResponse (Prelude.NonEmpty AnnotationImportItemDetail)
getAnnotationImportJobResponse_items = Lens.lens (\GetAnnotationImportJobResponse' {items} -> items) (\s@GetAnnotationImportJobResponse' {} a -> s {items = a} :: GetAnnotationImportJobResponse) Prelude.. Lens.coerced

-- | The job\'s left normalization setting.
getAnnotationImportJobResponse_runLeftNormalization :: Lens.Lens' GetAnnotationImportJobResponse Prelude.Bool
getAnnotationImportJobResponse_runLeftNormalization = Lens.lens (\GetAnnotationImportJobResponse' {runLeftNormalization} -> runLeftNormalization) (\s@GetAnnotationImportJobResponse' {} a -> s {runLeftNormalization = a} :: GetAnnotationImportJobResponse)

-- | Undocumented member.
getAnnotationImportJobResponse_formatOptions :: Lens.Lens' GetAnnotationImportJobResponse FormatOptions
getAnnotationImportJobResponse_formatOptions = Lens.lens (\GetAnnotationImportJobResponse' {formatOptions} -> formatOptions) (\s@GetAnnotationImportJobResponse' {} a -> s {formatOptions = a} :: GetAnnotationImportJobResponse)

instance
  Prelude.NFData
    GetAnnotationImportJobResponse
  where
  rnf GetAnnotationImportJobResponse' {..} =
    Prelude.rnf annotationFields
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf destinationName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf items
      `Prelude.seq` Prelude.rnf runLeftNormalization
      `Prelude.seq` Prelude.rnf formatOptions
