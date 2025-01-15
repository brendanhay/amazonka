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
-- Module      : Amazonka.Omics.GetReadSetImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a read set import job.
module Amazonka.Omics.GetReadSetImportJob
  ( -- * Creating a Request
    GetReadSetImportJob (..),
    newGetReadSetImportJob,

    -- * Request Lenses
    getReadSetImportJob_id,
    getReadSetImportJob_sequenceStoreId,

    -- * Destructuring the Response
    GetReadSetImportJobResponse (..),
    newGetReadSetImportJobResponse,

    -- * Response Lenses
    getReadSetImportJobResponse_completionTime,
    getReadSetImportJobResponse_statusMessage,
    getReadSetImportJobResponse_httpStatus,
    getReadSetImportJobResponse_creationTime,
    getReadSetImportJobResponse_id,
    getReadSetImportJobResponse_roleArn,
    getReadSetImportJobResponse_sequenceStoreId,
    getReadSetImportJobResponse_sources,
    getReadSetImportJobResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetReadSetImportJob' smart constructor.
data GetReadSetImportJob = GetReadSetImportJob'
  { -- | The job\'s ID.
    id :: Prelude.Text,
    -- | The job\'s sequence store ID.
    sequenceStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReadSetImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getReadSetImportJob_id' - The job\'s ID.
--
-- 'sequenceStoreId', 'getReadSetImportJob_sequenceStoreId' - The job\'s sequence store ID.
newGetReadSetImportJob ::
  -- | 'id'
  Prelude.Text ->
  -- | 'sequenceStoreId'
  Prelude.Text ->
  GetReadSetImportJob
newGetReadSetImportJob pId_ pSequenceStoreId_ =
  GetReadSetImportJob'
    { id = pId_,
      sequenceStoreId = pSequenceStoreId_
    }

-- | The job\'s ID.
getReadSetImportJob_id :: Lens.Lens' GetReadSetImportJob Prelude.Text
getReadSetImportJob_id = Lens.lens (\GetReadSetImportJob' {id} -> id) (\s@GetReadSetImportJob' {} a -> s {id = a} :: GetReadSetImportJob)

-- | The job\'s sequence store ID.
getReadSetImportJob_sequenceStoreId :: Lens.Lens' GetReadSetImportJob Prelude.Text
getReadSetImportJob_sequenceStoreId = Lens.lens (\GetReadSetImportJob' {sequenceStoreId} -> sequenceStoreId) (\s@GetReadSetImportJob' {} a -> s {sequenceStoreId = a} :: GetReadSetImportJob)

instance Core.AWSRequest GetReadSetImportJob where
  type
    AWSResponse GetReadSetImportJob =
      GetReadSetImportJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReadSetImportJobResponse'
            Prelude.<$> (x Data..?> "completionTime")
            Prelude.<*> (x Data..?> "statusMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "roleArn")
            Prelude.<*> (x Data..:> "sequenceStoreId")
            Prelude.<*> (x Data..?> "sources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable GetReadSetImportJob where
  hashWithSalt _salt GetReadSetImportJob' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` sequenceStoreId

instance Prelude.NFData GetReadSetImportJob where
  rnf GetReadSetImportJob' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf sequenceStoreId

instance Data.ToHeaders GetReadSetImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetReadSetImportJob where
  toPath GetReadSetImportJob' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/importjob/",
        Data.toBS id
      ]

instance Data.ToQuery GetReadSetImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetReadSetImportJobResponse' smart constructor.
data GetReadSetImportJobResponse = GetReadSetImportJobResponse'
  { -- | When the job completed.
    completionTime :: Prelude.Maybe Data.ISO8601,
    -- | The job\'s status message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | When the job was created.
    creationTime :: Data.ISO8601,
    -- | The job\'s ID.
    id :: Prelude.Text,
    -- | The job\'s service role ARN.
    roleArn :: Prelude.Text,
    -- | The job\'s sequence store ID.
    sequenceStoreId :: Prelude.Text,
    -- | The job\'s sources.
    sources :: [ImportReadSetSourceItem],
    -- | The job\'s status.
    status :: ReadSetImportJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReadSetImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'getReadSetImportJobResponse_completionTime' - When the job completed.
--
-- 'statusMessage', 'getReadSetImportJobResponse_statusMessage' - The job\'s status message.
--
-- 'httpStatus', 'getReadSetImportJobResponse_httpStatus' - The response's http status code.
--
-- 'creationTime', 'getReadSetImportJobResponse_creationTime' - When the job was created.
--
-- 'id', 'getReadSetImportJobResponse_id' - The job\'s ID.
--
-- 'roleArn', 'getReadSetImportJobResponse_roleArn' - The job\'s service role ARN.
--
-- 'sequenceStoreId', 'getReadSetImportJobResponse_sequenceStoreId' - The job\'s sequence store ID.
--
-- 'sources', 'getReadSetImportJobResponse_sources' - The job\'s sources.
--
-- 'status', 'getReadSetImportJobResponse_status' - The job\'s status.
newGetReadSetImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'id'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'status'
  ReadSetImportJobStatus ->
  GetReadSetImportJobResponse
newGetReadSetImportJobResponse
  pHttpStatus_
  pCreationTime_
  pId_
  pRoleArn_
  pSequenceStoreId_
  pStatus_ =
    GetReadSetImportJobResponse'
      { completionTime =
          Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        id = pId_,
        roleArn = pRoleArn_,
        sequenceStoreId = pSequenceStoreId_,
        sources = Prelude.mempty,
        status = pStatus_
      }

-- | When the job completed.
getReadSetImportJobResponse_completionTime :: Lens.Lens' GetReadSetImportJobResponse (Prelude.Maybe Prelude.UTCTime)
getReadSetImportJobResponse_completionTime = Lens.lens (\GetReadSetImportJobResponse' {completionTime} -> completionTime) (\s@GetReadSetImportJobResponse' {} a -> s {completionTime = a} :: GetReadSetImportJobResponse) Prelude.. Lens.mapping Data._Time

-- | The job\'s status message.
getReadSetImportJobResponse_statusMessage :: Lens.Lens' GetReadSetImportJobResponse (Prelude.Maybe Prelude.Text)
getReadSetImportJobResponse_statusMessage = Lens.lens (\GetReadSetImportJobResponse' {statusMessage} -> statusMessage) (\s@GetReadSetImportJobResponse' {} a -> s {statusMessage = a} :: GetReadSetImportJobResponse)

-- | The response's http status code.
getReadSetImportJobResponse_httpStatus :: Lens.Lens' GetReadSetImportJobResponse Prelude.Int
getReadSetImportJobResponse_httpStatus = Lens.lens (\GetReadSetImportJobResponse' {httpStatus} -> httpStatus) (\s@GetReadSetImportJobResponse' {} a -> s {httpStatus = a} :: GetReadSetImportJobResponse)

-- | When the job was created.
getReadSetImportJobResponse_creationTime :: Lens.Lens' GetReadSetImportJobResponse Prelude.UTCTime
getReadSetImportJobResponse_creationTime = Lens.lens (\GetReadSetImportJobResponse' {creationTime} -> creationTime) (\s@GetReadSetImportJobResponse' {} a -> s {creationTime = a} :: GetReadSetImportJobResponse) Prelude.. Data._Time

-- | The job\'s ID.
getReadSetImportJobResponse_id :: Lens.Lens' GetReadSetImportJobResponse Prelude.Text
getReadSetImportJobResponse_id = Lens.lens (\GetReadSetImportJobResponse' {id} -> id) (\s@GetReadSetImportJobResponse' {} a -> s {id = a} :: GetReadSetImportJobResponse)

-- | The job\'s service role ARN.
getReadSetImportJobResponse_roleArn :: Lens.Lens' GetReadSetImportJobResponse Prelude.Text
getReadSetImportJobResponse_roleArn = Lens.lens (\GetReadSetImportJobResponse' {roleArn} -> roleArn) (\s@GetReadSetImportJobResponse' {} a -> s {roleArn = a} :: GetReadSetImportJobResponse)

-- | The job\'s sequence store ID.
getReadSetImportJobResponse_sequenceStoreId :: Lens.Lens' GetReadSetImportJobResponse Prelude.Text
getReadSetImportJobResponse_sequenceStoreId = Lens.lens (\GetReadSetImportJobResponse' {sequenceStoreId} -> sequenceStoreId) (\s@GetReadSetImportJobResponse' {} a -> s {sequenceStoreId = a} :: GetReadSetImportJobResponse)

-- | The job\'s sources.
getReadSetImportJobResponse_sources :: Lens.Lens' GetReadSetImportJobResponse [ImportReadSetSourceItem]
getReadSetImportJobResponse_sources = Lens.lens (\GetReadSetImportJobResponse' {sources} -> sources) (\s@GetReadSetImportJobResponse' {} a -> s {sources = a} :: GetReadSetImportJobResponse) Prelude.. Lens.coerced

-- | The job\'s status.
getReadSetImportJobResponse_status :: Lens.Lens' GetReadSetImportJobResponse ReadSetImportJobStatus
getReadSetImportJobResponse_status = Lens.lens (\GetReadSetImportJobResponse' {status} -> status) (\s@GetReadSetImportJobResponse' {} a -> s {status = a} :: GetReadSetImportJobResponse)

instance Prelude.NFData GetReadSetImportJobResponse where
  rnf GetReadSetImportJobResponse' {..} =
    Prelude.rnf completionTime `Prelude.seq`
      Prelude.rnf statusMessage `Prelude.seq`
        Prelude.rnf httpStatus `Prelude.seq`
          Prelude.rnf creationTime `Prelude.seq`
            Prelude.rnf id `Prelude.seq`
              Prelude.rnf roleArn `Prelude.seq`
                Prelude.rnf sequenceStoreId `Prelude.seq`
                  Prelude.rnf sources `Prelude.seq`
                    Prelude.rnf status
