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
-- Module      : Amazonka.Omics.StartReadSetExportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a read set to Amazon S3.
module Amazonka.Omics.StartReadSetExportJob
  ( -- * Creating a Request
    StartReadSetExportJob (..),
    newStartReadSetExportJob,

    -- * Request Lenses
    startReadSetExportJob_clientToken,
    startReadSetExportJob_sequenceStoreId,
    startReadSetExportJob_destination,
    startReadSetExportJob_roleArn,
    startReadSetExportJob_sources,

    -- * Destructuring the Response
    StartReadSetExportJobResponse (..),
    newStartReadSetExportJobResponse,

    -- * Response Lenses
    startReadSetExportJobResponse_httpStatus,
    startReadSetExportJobResponse_id,
    startReadSetExportJobResponse_sequenceStoreId,
    startReadSetExportJobResponse_destination,
    startReadSetExportJobResponse_status,
    startReadSetExportJobResponse_creationTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartReadSetExportJob' smart constructor.
data StartReadSetExportJob = StartReadSetExportJob'
  { -- | To ensure that jobs don\'t run multiple times, specify a unique token
    -- for each job.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The read set\'s sequence store ID.
    sequenceStoreId :: Prelude.Text,
    -- | A location for exported files in Amazon S3.
    destination :: Prelude.Text,
    -- | A service role for the job.
    roleArn :: Prelude.Text,
    -- | The job\'s source files.
    sources :: Prelude.NonEmpty ExportReadSet
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReadSetExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startReadSetExportJob_clientToken' - To ensure that jobs don\'t run multiple times, specify a unique token
-- for each job.
--
-- 'sequenceStoreId', 'startReadSetExportJob_sequenceStoreId' - The read set\'s sequence store ID.
--
-- 'destination', 'startReadSetExportJob_destination' - A location for exported files in Amazon S3.
--
-- 'roleArn', 'startReadSetExportJob_roleArn' - A service role for the job.
--
-- 'sources', 'startReadSetExportJob_sources' - The job\'s source files.
newStartReadSetExportJob ::
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'destination'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'sources'
  Prelude.NonEmpty ExportReadSet ->
  StartReadSetExportJob
newStartReadSetExportJob
  pSequenceStoreId_
  pDestination_
  pRoleArn_
  pSources_ =
    StartReadSetExportJob'
      { clientToken =
          Prelude.Nothing,
        sequenceStoreId = pSequenceStoreId_,
        destination = pDestination_,
        roleArn = pRoleArn_,
        sources = Lens.coerced Lens.# pSources_
      }

-- | To ensure that jobs don\'t run multiple times, specify a unique token
-- for each job.
startReadSetExportJob_clientToken :: Lens.Lens' StartReadSetExportJob (Prelude.Maybe Prelude.Text)
startReadSetExportJob_clientToken = Lens.lens (\StartReadSetExportJob' {clientToken} -> clientToken) (\s@StartReadSetExportJob' {} a -> s {clientToken = a} :: StartReadSetExportJob)

-- | The read set\'s sequence store ID.
startReadSetExportJob_sequenceStoreId :: Lens.Lens' StartReadSetExportJob Prelude.Text
startReadSetExportJob_sequenceStoreId = Lens.lens (\StartReadSetExportJob' {sequenceStoreId} -> sequenceStoreId) (\s@StartReadSetExportJob' {} a -> s {sequenceStoreId = a} :: StartReadSetExportJob)

-- | A location for exported files in Amazon S3.
startReadSetExportJob_destination :: Lens.Lens' StartReadSetExportJob Prelude.Text
startReadSetExportJob_destination = Lens.lens (\StartReadSetExportJob' {destination} -> destination) (\s@StartReadSetExportJob' {} a -> s {destination = a} :: StartReadSetExportJob)

-- | A service role for the job.
startReadSetExportJob_roleArn :: Lens.Lens' StartReadSetExportJob Prelude.Text
startReadSetExportJob_roleArn = Lens.lens (\StartReadSetExportJob' {roleArn} -> roleArn) (\s@StartReadSetExportJob' {} a -> s {roleArn = a} :: StartReadSetExportJob)

-- | The job\'s source files.
startReadSetExportJob_sources :: Lens.Lens' StartReadSetExportJob (Prelude.NonEmpty ExportReadSet)
startReadSetExportJob_sources = Lens.lens (\StartReadSetExportJob' {sources} -> sources) (\s@StartReadSetExportJob' {} a -> s {sources = a} :: StartReadSetExportJob) Prelude.. Lens.coerced

instance Core.AWSRequest StartReadSetExportJob where
  type
    AWSResponse StartReadSetExportJob =
      StartReadSetExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartReadSetExportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "sequenceStoreId")
            Prelude.<*> (x Data..:> "destination")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "creationTime")
      )

instance Prelude.Hashable StartReadSetExportJob where
  hashWithSalt _salt StartReadSetExportJob' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` sequenceStoreId
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` sources

instance Prelude.NFData StartReadSetExportJob where
  rnf StartReadSetExportJob' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf sources

instance Data.ToHeaders StartReadSetExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartReadSetExportJob where
  toJSON StartReadSetExportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("destination" Data..= destination),
            Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("sources" Data..= sources)
          ]
      )

instance Data.ToPath StartReadSetExportJob where
  toPath StartReadSetExportJob' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/exportjob"
      ]

instance Data.ToQuery StartReadSetExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartReadSetExportJobResponse' smart constructor.
data StartReadSetExportJobResponse = StartReadSetExportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The job\'s ID.
    id :: Prelude.Text,
    -- | The read set\'s sequence store ID.
    sequenceStoreId :: Prelude.Text,
    -- | The job\'s output location.
    destination :: Prelude.Text,
    -- | The job\'s status.
    status :: ReadSetExportJobStatus,
    -- | When the job was created.
    creationTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReadSetExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startReadSetExportJobResponse_httpStatus' - The response's http status code.
--
-- 'id', 'startReadSetExportJobResponse_id' - The job\'s ID.
--
-- 'sequenceStoreId', 'startReadSetExportJobResponse_sequenceStoreId' - The read set\'s sequence store ID.
--
-- 'destination', 'startReadSetExportJobResponse_destination' - The job\'s output location.
--
-- 'status', 'startReadSetExportJobResponse_status' - The job\'s status.
--
-- 'creationTime', 'startReadSetExportJobResponse_creationTime' - When the job was created.
newStartReadSetExportJobResponse ::
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
  StartReadSetExportJobResponse
newStartReadSetExportJobResponse
  pHttpStatus_
  pId_
  pSequenceStoreId_
  pDestination_
  pStatus_
  pCreationTime_ =
    StartReadSetExportJobResponse'
      { httpStatus =
          pHttpStatus_,
        id = pId_,
        sequenceStoreId = pSequenceStoreId_,
        destination = pDestination_,
        status = pStatus_,
        creationTime =
          Data._Time Lens.# pCreationTime_
      }

-- | The response's http status code.
startReadSetExportJobResponse_httpStatus :: Lens.Lens' StartReadSetExportJobResponse Prelude.Int
startReadSetExportJobResponse_httpStatus = Lens.lens (\StartReadSetExportJobResponse' {httpStatus} -> httpStatus) (\s@StartReadSetExportJobResponse' {} a -> s {httpStatus = a} :: StartReadSetExportJobResponse)

-- | The job\'s ID.
startReadSetExportJobResponse_id :: Lens.Lens' StartReadSetExportJobResponse Prelude.Text
startReadSetExportJobResponse_id = Lens.lens (\StartReadSetExportJobResponse' {id} -> id) (\s@StartReadSetExportJobResponse' {} a -> s {id = a} :: StartReadSetExportJobResponse)

-- | The read set\'s sequence store ID.
startReadSetExportJobResponse_sequenceStoreId :: Lens.Lens' StartReadSetExportJobResponse Prelude.Text
startReadSetExportJobResponse_sequenceStoreId = Lens.lens (\StartReadSetExportJobResponse' {sequenceStoreId} -> sequenceStoreId) (\s@StartReadSetExportJobResponse' {} a -> s {sequenceStoreId = a} :: StartReadSetExportJobResponse)

-- | The job\'s output location.
startReadSetExportJobResponse_destination :: Lens.Lens' StartReadSetExportJobResponse Prelude.Text
startReadSetExportJobResponse_destination = Lens.lens (\StartReadSetExportJobResponse' {destination} -> destination) (\s@StartReadSetExportJobResponse' {} a -> s {destination = a} :: StartReadSetExportJobResponse)

-- | The job\'s status.
startReadSetExportJobResponse_status :: Lens.Lens' StartReadSetExportJobResponse ReadSetExportJobStatus
startReadSetExportJobResponse_status = Lens.lens (\StartReadSetExportJobResponse' {status} -> status) (\s@StartReadSetExportJobResponse' {} a -> s {status = a} :: StartReadSetExportJobResponse)

-- | When the job was created.
startReadSetExportJobResponse_creationTime :: Lens.Lens' StartReadSetExportJobResponse Prelude.UTCTime
startReadSetExportJobResponse_creationTime = Lens.lens (\StartReadSetExportJobResponse' {creationTime} -> creationTime) (\s@StartReadSetExportJobResponse' {} a -> s {creationTime = a} :: StartReadSetExportJobResponse) Prelude.. Data._Time

instance Prelude.NFData StartReadSetExportJobResponse where
  rnf StartReadSetExportJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf creationTime
