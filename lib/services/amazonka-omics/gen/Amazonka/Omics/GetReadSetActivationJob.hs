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
-- Module      : Amazonka.Omics.GetReadSetActivationJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a read set activation job.
module Amazonka.Omics.GetReadSetActivationJob
  ( -- * Creating a Request
    GetReadSetActivationJob (..),
    newGetReadSetActivationJob,

    -- * Request Lenses
    getReadSetActivationJob_id,
    getReadSetActivationJob_sequenceStoreId,

    -- * Destructuring the Response
    GetReadSetActivationJobResponse (..),
    newGetReadSetActivationJobResponse,

    -- * Response Lenses
    getReadSetActivationJobResponse_completionTime,
    getReadSetActivationJobResponse_sources,
    getReadSetActivationJobResponse_statusMessage,
    getReadSetActivationJobResponse_httpStatus,
    getReadSetActivationJobResponse_creationTime,
    getReadSetActivationJobResponse_id,
    getReadSetActivationJobResponse_sequenceStoreId,
    getReadSetActivationJobResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetReadSetActivationJob' smart constructor.
data GetReadSetActivationJob = GetReadSetActivationJob'
  { -- | The job\'s ID.
    id :: Prelude.Text,
    -- | The job\'s sequence store ID.
    sequenceStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReadSetActivationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getReadSetActivationJob_id' - The job\'s ID.
--
-- 'sequenceStoreId', 'getReadSetActivationJob_sequenceStoreId' - The job\'s sequence store ID.
newGetReadSetActivationJob ::
  -- | 'id'
  Prelude.Text ->
  -- | 'sequenceStoreId'
  Prelude.Text ->
  GetReadSetActivationJob
newGetReadSetActivationJob pId_ pSequenceStoreId_ =
  GetReadSetActivationJob'
    { id = pId_,
      sequenceStoreId = pSequenceStoreId_
    }

-- | The job\'s ID.
getReadSetActivationJob_id :: Lens.Lens' GetReadSetActivationJob Prelude.Text
getReadSetActivationJob_id = Lens.lens (\GetReadSetActivationJob' {id} -> id) (\s@GetReadSetActivationJob' {} a -> s {id = a} :: GetReadSetActivationJob)

-- | The job\'s sequence store ID.
getReadSetActivationJob_sequenceStoreId :: Lens.Lens' GetReadSetActivationJob Prelude.Text
getReadSetActivationJob_sequenceStoreId = Lens.lens (\GetReadSetActivationJob' {sequenceStoreId} -> sequenceStoreId) (\s@GetReadSetActivationJob' {} a -> s {sequenceStoreId = a} :: GetReadSetActivationJob)

instance Core.AWSRequest GetReadSetActivationJob where
  type
    AWSResponse GetReadSetActivationJob =
      GetReadSetActivationJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReadSetActivationJobResponse'
            Prelude.<$> (x Data..?> "completionTime")
            Prelude.<*> (x Data..?> "sources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "statusMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "sequenceStoreId")
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable GetReadSetActivationJob where
  hashWithSalt _salt GetReadSetActivationJob' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` sequenceStoreId

instance Prelude.NFData GetReadSetActivationJob where
  rnf GetReadSetActivationJob' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf sequenceStoreId

instance Data.ToHeaders GetReadSetActivationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetReadSetActivationJob where
  toPath GetReadSetActivationJob' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/activationjob/",
        Data.toBS id
      ]

instance Data.ToQuery GetReadSetActivationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetReadSetActivationJobResponse' smart constructor.
data GetReadSetActivationJobResponse = GetReadSetActivationJobResponse'
  { -- | When the job completed.
    completionTime :: Prelude.Maybe Data.POSIX,
    -- | The job\'s sources.
    sources :: Prelude.Maybe [ActivateReadSetSourceItem],
    -- | The job\'s status message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | When the job was created.
    creationTime :: Data.POSIX,
    -- | The job\'s ID.
    id :: Prelude.Text,
    -- | The job\'s sequence store ID.
    sequenceStoreId :: Prelude.Text,
    -- | The job\'s status.
    status :: ReadSetActivationJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReadSetActivationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'getReadSetActivationJobResponse_completionTime' - When the job completed.
--
-- 'sources', 'getReadSetActivationJobResponse_sources' - The job\'s sources.
--
-- 'statusMessage', 'getReadSetActivationJobResponse_statusMessage' - The job\'s status message.
--
-- 'httpStatus', 'getReadSetActivationJobResponse_httpStatus' - The response's http status code.
--
-- 'creationTime', 'getReadSetActivationJobResponse_creationTime' - When the job was created.
--
-- 'id', 'getReadSetActivationJobResponse_id' - The job\'s ID.
--
-- 'sequenceStoreId', 'getReadSetActivationJobResponse_sequenceStoreId' - The job\'s sequence store ID.
--
-- 'status', 'getReadSetActivationJobResponse_status' - The job\'s status.
newGetReadSetActivationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'id'
  Prelude.Text ->
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'status'
  ReadSetActivationJobStatus ->
  GetReadSetActivationJobResponse
newGetReadSetActivationJobResponse
  pHttpStatus_
  pCreationTime_
  pId_
  pSequenceStoreId_
  pStatus_ =
    GetReadSetActivationJobResponse'
      { completionTime =
          Prelude.Nothing,
        sources = Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        id = pId_,
        sequenceStoreId = pSequenceStoreId_,
        status = pStatus_
      }

-- | When the job completed.
getReadSetActivationJobResponse_completionTime :: Lens.Lens' GetReadSetActivationJobResponse (Prelude.Maybe Prelude.UTCTime)
getReadSetActivationJobResponse_completionTime = Lens.lens (\GetReadSetActivationJobResponse' {completionTime} -> completionTime) (\s@GetReadSetActivationJobResponse' {} a -> s {completionTime = a} :: GetReadSetActivationJobResponse) Prelude.. Lens.mapping Data._Time

-- | The job\'s sources.
getReadSetActivationJobResponse_sources :: Lens.Lens' GetReadSetActivationJobResponse (Prelude.Maybe [ActivateReadSetSourceItem])
getReadSetActivationJobResponse_sources = Lens.lens (\GetReadSetActivationJobResponse' {sources} -> sources) (\s@GetReadSetActivationJobResponse' {} a -> s {sources = a} :: GetReadSetActivationJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The job\'s status message.
getReadSetActivationJobResponse_statusMessage :: Lens.Lens' GetReadSetActivationJobResponse (Prelude.Maybe Prelude.Text)
getReadSetActivationJobResponse_statusMessage = Lens.lens (\GetReadSetActivationJobResponse' {statusMessage} -> statusMessage) (\s@GetReadSetActivationJobResponse' {} a -> s {statusMessage = a} :: GetReadSetActivationJobResponse)

-- | The response's http status code.
getReadSetActivationJobResponse_httpStatus :: Lens.Lens' GetReadSetActivationJobResponse Prelude.Int
getReadSetActivationJobResponse_httpStatus = Lens.lens (\GetReadSetActivationJobResponse' {httpStatus} -> httpStatus) (\s@GetReadSetActivationJobResponse' {} a -> s {httpStatus = a} :: GetReadSetActivationJobResponse)

-- | When the job was created.
getReadSetActivationJobResponse_creationTime :: Lens.Lens' GetReadSetActivationJobResponse Prelude.UTCTime
getReadSetActivationJobResponse_creationTime = Lens.lens (\GetReadSetActivationJobResponse' {creationTime} -> creationTime) (\s@GetReadSetActivationJobResponse' {} a -> s {creationTime = a} :: GetReadSetActivationJobResponse) Prelude.. Data._Time

-- | The job\'s ID.
getReadSetActivationJobResponse_id :: Lens.Lens' GetReadSetActivationJobResponse Prelude.Text
getReadSetActivationJobResponse_id = Lens.lens (\GetReadSetActivationJobResponse' {id} -> id) (\s@GetReadSetActivationJobResponse' {} a -> s {id = a} :: GetReadSetActivationJobResponse)

-- | The job\'s sequence store ID.
getReadSetActivationJobResponse_sequenceStoreId :: Lens.Lens' GetReadSetActivationJobResponse Prelude.Text
getReadSetActivationJobResponse_sequenceStoreId = Lens.lens (\GetReadSetActivationJobResponse' {sequenceStoreId} -> sequenceStoreId) (\s@GetReadSetActivationJobResponse' {} a -> s {sequenceStoreId = a} :: GetReadSetActivationJobResponse)

-- | The job\'s status.
getReadSetActivationJobResponse_status :: Lens.Lens' GetReadSetActivationJobResponse ReadSetActivationJobStatus
getReadSetActivationJobResponse_status = Lens.lens (\GetReadSetActivationJobResponse' {status} -> status) (\s@GetReadSetActivationJobResponse' {} a -> s {status = a} :: GetReadSetActivationJobResponse)

instance
  Prelude.NFData
    GetReadSetActivationJobResponse
  where
  rnf GetReadSetActivationJobResponse' {..} =
    Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf status
