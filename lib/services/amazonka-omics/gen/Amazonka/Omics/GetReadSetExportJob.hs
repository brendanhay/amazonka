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
-- Module      : Amazonka.Omics.GetReadSetExportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a read set export job.
module Amazonka.Omics.GetReadSetExportJob
  ( -- * Creating a Request
    GetReadSetExportJob (..),
    newGetReadSetExportJob,

    -- * Request Lenses
    getReadSetExportJob_sequenceStoreId,
    getReadSetExportJob_id,

    -- * Destructuring the Response
    GetReadSetExportJobResponse (..),
    newGetReadSetExportJobResponse,

    -- * Response Lenses
    getReadSetExportJobResponse_completionTime,
    getReadSetExportJobResponse_readSets,
    getReadSetExportJobResponse_statusMessage,
    getReadSetExportJobResponse_httpStatus,
    getReadSetExportJobResponse_id,
    getReadSetExportJobResponse_sequenceStoreId,
    getReadSetExportJobResponse_destination,
    getReadSetExportJobResponse_status,
    getReadSetExportJobResponse_creationTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetReadSetExportJob' smart constructor.
data GetReadSetExportJob = GetReadSetExportJob'
  { -- | The job\'s sequence store ID.
    sequenceStoreId :: Prelude.Text,
    -- | The job\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReadSetExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sequenceStoreId', 'getReadSetExportJob_sequenceStoreId' - The job\'s sequence store ID.
--
-- 'id', 'getReadSetExportJob_id' - The job\'s ID.
newGetReadSetExportJob ::
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  GetReadSetExportJob
newGetReadSetExportJob pSequenceStoreId_ pId_ =
  GetReadSetExportJob'
    { sequenceStoreId =
        pSequenceStoreId_,
      id = pId_
    }

-- | The job\'s sequence store ID.
getReadSetExportJob_sequenceStoreId :: Lens.Lens' GetReadSetExportJob Prelude.Text
getReadSetExportJob_sequenceStoreId = Lens.lens (\GetReadSetExportJob' {sequenceStoreId} -> sequenceStoreId) (\s@GetReadSetExportJob' {} a -> s {sequenceStoreId = a} :: GetReadSetExportJob)

-- | The job\'s ID.
getReadSetExportJob_id :: Lens.Lens' GetReadSetExportJob Prelude.Text
getReadSetExportJob_id = Lens.lens (\GetReadSetExportJob' {id} -> id) (\s@GetReadSetExportJob' {} a -> s {id = a} :: GetReadSetExportJob)

instance Core.AWSRequest GetReadSetExportJob where
  type
    AWSResponse GetReadSetExportJob =
      GetReadSetExportJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReadSetExportJobResponse'
            Prelude.<$> (x Data..?> "completionTime")
            Prelude.<*> (x Data..?> "readSets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "statusMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "sequenceStoreId")
            Prelude.<*> (x Data..:> "destination")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "creationTime")
      )

instance Prelude.Hashable GetReadSetExportJob where
  hashWithSalt _salt GetReadSetExportJob' {..} =
    _salt
      `Prelude.hashWithSalt` sequenceStoreId
      `Prelude.hashWithSalt` id

instance Prelude.NFData GetReadSetExportJob where
  rnf GetReadSetExportJob' {..} =
    Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders GetReadSetExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetReadSetExportJob where
  toPath GetReadSetExportJob' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/exportjob/",
        Data.toBS id
      ]

instance Data.ToQuery GetReadSetExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetReadSetExportJobResponse' smart constructor.
data GetReadSetExportJobResponse = GetReadSetExportJobResponse'
  { -- | When the job completed.
    completionTime :: Prelude.Maybe Data.ISO8601,
    -- | The job\'s read sets.
    readSets :: Prelude.Maybe [ExportReadSetDetail],
    -- | The job\'s status message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The job\'s ID.
    id :: Prelude.Text,
    -- | The job\'s sequence store ID.
    sequenceStoreId :: Prelude.Text,
    -- | The job\'s destination in Amazon S3.
    destination :: Prelude.Text,
    -- | The job\'s status.
    status :: ReadSetExportJobStatus,
    -- | When the job was created.
    creationTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReadSetExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'getReadSetExportJobResponse_completionTime' - When the job completed.
--
-- 'readSets', 'getReadSetExportJobResponse_readSets' - The job\'s read sets.
--
-- 'statusMessage', 'getReadSetExportJobResponse_statusMessage' - The job\'s status message.
--
-- 'httpStatus', 'getReadSetExportJobResponse_httpStatus' - The response's http status code.
--
-- 'id', 'getReadSetExportJobResponse_id' - The job\'s ID.
--
-- 'sequenceStoreId', 'getReadSetExportJobResponse_sequenceStoreId' - The job\'s sequence store ID.
--
-- 'destination', 'getReadSetExportJobResponse_destination' - The job\'s destination in Amazon S3.
--
-- 'status', 'getReadSetExportJobResponse_status' - The job\'s status.
--
-- 'creationTime', 'getReadSetExportJobResponse_creationTime' - When the job was created.
newGetReadSetExportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'destination'
  Prelude.Text ->
  -- | 'status'
  ReadSetExportJobStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  GetReadSetExportJobResponse
newGetReadSetExportJobResponse
  pHttpStatus_
  pId_
  pSequenceStoreId_
  pDestination_
  pStatus_
  pCreationTime_ =
    GetReadSetExportJobResponse'
      { completionTime =
          Prelude.Nothing,
        readSets = Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        id = pId_,
        sequenceStoreId = pSequenceStoreId_,
        destination = pDestination_,
        status = pStatus_,
        creationTime =
          Data._Time Lens.# pCreationTime_
      }

-- | When the job completed.
getReadSetExportJobResponse_completionTime :: Lens.Lens' GetReadSetExportJobResponse (Prelude.Maybe Prelude.UTCTime)
getReadSetExportJobResponse_completionTime = Lens.lens (\GetReadSetExportJobResponse' {completionTime} -> completionTime) (\s@GetReadSetExportJobResponse' {} a -> s {completionTime = a} :: GetReadSetExportJobResponse) Prelude.. Lens.mapping Data._Time

-- | The job\'s read sets.
getReadSetExportJobResponse_readSets :: Lens.Lens' GetReadSetExportJobResponse (Prelude.Maybe [ExportReadSetDetail])
getReadSetExportJobResponse_readSets = Lens.lens (\GetReadSetExportJobResponse' {readSets} -> readSets) (\s@GetReadSetExportJobResponse' {} a -> s {readSets = a} :: GetReadSetExportJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The job\'s status message.
getReadSetExportJobResponse_statusMessage :: Lens.Lens' GetReadSetExportJobResponse (Prelude.Maybe Prelude.Text)
getReadSetExportJobResponse_statusMessage = Lens.lens (\GetReadSetExportJobResponse' {statusMessage} -> statusMessage) (\s@GetReadSetExportJobResponse' {} a -> s {statusMessage = a} :: GetReadSetExportJobResponse)

-- | The response's http status code.
getReadSetExportJobResponse_httpStatus :: Lens.Lens' GetReadSetExportJobResponse Prelude.Int
getReadSetExportJobResponse_httpStatus = Lens.lens (\GetReadSetExportJobResponse' {httpStatus} -> httpStatus) (\s@GetReadSetExportJobResponse' {} a -> s {httpStatus = a} :: GetReadSetExportJobResponse)

-- | The job\'s ID.
getReadSetExportJobResponse_id :: Lens.Lens' GetReadSetExportJobResponse Prelude.Text
getReadSetExportJobResponse_id = Lens.lens (\GetReadSetExportJobResponse' {id} -> id) (\s@GetReadSetExportJobResponse' {} a -> s {id = a} :: GetReadSetExportJobResponse)

-- | The job\'s sequence store ID.
getReadSetExportJobResponse_sequenceStoreId :: Lens.Lens' GetReadSetExportJobResponse Prelude.Text
getReadSetExportJobResponse_sequenceStoreId = Lens.lens (\GetReadSetExportJobResponse' {sequenceStoreId} -> sequenceStoreId) (\s@GetReadSetExportJobResponse' {} a -> s {sequenceStoreId = a} :: GetReadSetExportJobResponse)

-- | The job\'s destination in Amazon S3.
getReadSetExportJobResponse_destination :: Lens.Lens' GetReadSetExportJobResponse Prelude.Text
getReadSetExportJobResponse_destination = Lens.lens (\GetReadSetExportJobResponse' {destination} -> destination) (\s@GetReadSetExportJobResponse' {} a -> s {destination = a} :: GetReadSetExportJobResponse)

-- | The job\'s status.
getReadSetExportJobResponse_status :: Lens.Lens' GetReadSetExportJobResponse ReadSetExportJobStatus
getReadSetExportJobResponse_status = Lens.lens (\GetReadSetExportJobResponse' {status} -> status) (\s@GetReadSetExportJobResponse' {} a -> s {status = a} :: GetReadSetExportJobResponse)

-- | When the job was created.
getReadSetExportJobResponse_creationTime :: Lens.Lens' GetReadSetExportJobResponse Prelude.UTCTime
getReadSetExportJobResponse_creationTime = Lens.lens (\GetReadSetExportJobResponse' {creationTime} -> creationTime) (\s@GetReadSetExportJobResponse' {} a -> s {creationTime = a} :: GetReadSetExportJobResponse) Prelude.. Data._Time

instance Prelude.NFData GetReadSetExportJobResponse where
  rnf GetReadSetExportJobResponse' {..} =
    Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf readSets
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf creationTime
