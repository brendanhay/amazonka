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
-- Module      : Amazonka.Omics.StartReadSetActivationJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a read set activation job.
module Amazonka.Omics.StartReadSetActivationJob
  ( -- * Creating a Request
    StartReadSetActivationJob (..),
    newStartReadSetActivationJob,

    -- * Request Lenses
    startReadSetActivationJob_clientToken,
    startReadSetActivationJob_sequenceStoreId,
    startReadSetActivationJob_sources,

    -- * Destructuring the Response
    StartReadSetActivationJobResponse (..),
    newStartReadSetActivationJobResponse,

    -- * Response Lenses
    startReadSetActivationJobResponse_httpStatus,
    startReadSetActivationJobResponse_creationTime,
    startReadSetActivationJobResponse_id,
    startReadSetActivationJobResponse_sequenceStoreId,
    startReadSetActivationJobResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartReadSetActivationJob' smart constructor.
data StartReadSetActivationJob = StartReadSetActivationJob'
  { -- | To ensure that jobs don\'t run multiple times, specify a unique token
    -- for each job.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The read set\'s sequence store ID.
    sequenceStoreId :: Prelude.Text,
    -- | The job\'s sources.
    sources :: Prelude.NonEmpty StartReadSetActivationJobSourceItem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReadSetActivationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startReadSetActivationJob_clientToken' - To ensure that jobs don\'t run multiple times, specify a unique token
-- for each job.
--
-- 'sequenceStoreId', 'startReadSetActivationJob_sequenceStoreId' - The read set\'s sequence store ID.
--
-- 'sources', 'startReadSetActivationJob_sources' - The job\'s sources.
newStartReadSetActivationJob ::
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'sources'
  Prelude.NonEmpty StartReadSetActivationJobSourceItem ->
  StartReadSetActivationJob
newStartReadSetActivationJob
  pSequenceStoreId_
  pSources_ =
    StartReadSetActivationJob'
      { clientToken =
          Prelude.Nothing,
        sequenceStoreId = pSequenceStoreId_,
        sources = Lens.coerced Lens.# pSources_
      }

-- | To ensure that jobs don\'t run multiple times, specify a unique token
-- for each job.
startReadSetActivationJob_clientToken :: Lens.Lens' StartReadSetActivationJob (Prelude.Maybe Prelude.Text)
startReadSetActivationJob_clientToken = Lens.lens (\StartReadSetActivationJob' {clientToken} -> clientToken) (\s@StartReadSetActivationJob' {} a -> s {clientToken = a} :: StartReadSetActivationJob)

-- | The read set\'s sequence store ID.
startReadSetActivationJob_sequenceStoreId :: Lens.Lens' StartReadSetActivationJob Prelude.Text
startReadSetActivationJob_sequenceStoreId = Lens.lens (\StartReadSetActivationJob' {sequenceStoreId} -> sequenceStoreId) (\s@StartReadSetActivationJob' {} a -> s {sequenceStoreId = a} :: StartReadSetActivationJob)

-- | The job\'s sources.
startReadSetActivationJob_sources :: Lens.Lens' StartReadSetActivationJob (Prelude.NonEmpty StartReadSetActivationJobSourceItem)
startReadSetActivationJob_sources = Lens.lens (\StartReadSetActivationJob' {sources} -> sources) (\s@StartReadSetActivationJob' {} a -> s {sources = a} :: StartReadSetActivationJob) Prelude.. Lens.coerced

instance Core.AWSRequest StartReadSetActivationJob where
  type
    AWSResponse StartReadSetActivationJob =
      StartReadSetActivationJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartReadSetActivationJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "sequenceStoreId")
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable StartReadSetActivationJob where
  hashWithSalt _salt StartReadSetActivationJob' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` sequenceStoreId
      `Prelude.hashWithSalt` sources

instance Prelude.NFData StartReadSetActivationJob where
  rnf StartReadSetActivationJob' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf sources

instance Data.ToHeaders StartReadSetActivationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartReadSetActivationJob where
  toJSON StartReadSetActivationJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("sources" Data..= sources)
          ]
      )

instance Data.ToPath StartReadSetActivationJob where
  toPath StartReadSetActivationJob' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/activationjob"
      ]

instance Data.ToQuery StartReadSetActivationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartReadSetActivationJobResponse' smart constructor.
data StartReadSetActivationJobResponse = StartReadSetActivationJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | When the job was created.
    creationTime :: Data.ISO8601,
    -- | The job\'s ID.
    id :: Prelude.Text,
    -- | The read set\'s sequence store ID.
    sequenceStoreId :: Prelude.Text,
    -- | The job\'s status.
    status :: ReadSetActivationJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReadSetActivationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startReadSetActivationJobResponse_httpStatus' - The response's http status code.
--
-- 'creationTime', 'startReadSetActivationJobResponse_creationTime' - When the job was created.
--
-- 'id', 'startReadSetActivationJobResponse_id' - The job\'s ID.
--
-- 'sequenceStoreId', 'startReadSetActivationJobResponse_sequenceStoreId' - The read set\'s sequence store ID.
--
-- 'status', 'startReadSetActivationJobResponse_status' - The job\'s status.
newStartReadSetActivationJobResponse ::
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
  StartReadSetActivationJobResponse
newStartReadSetActivationJobResponse
  pHttpStatus_
  pCreationTime_
  pId_
  pSequenceStoreId_
  pStatus_ =
    StartReadSetActivationJobResponse'
      { httpStatus =
          pHttpStatus_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        id = pId_,
        sequenceStoreId = pSequenceStoreId_,
        status = pStatus_
      }

-- | The response's http status code.
startReadSetActivationJobResponse_httpStatus :: Lens.Lens' StartReadSetActivationJobResponse Prelude.Int
startReadSetActivationJobResponse_httpStatus = Lens.lens (\StartReadSetActivationJobResponse' {httpStatus} -> httpStatus) (\s@StartReadSetActivationJobResponse' {} a -> s {httpStatus = a} :: StartReadSetActivationJobResponse)

-- | When the job was created.
startReadSetActivationJobResponse_creationTime :: Lens.Lens' StartReadSetActivationJobResponse Prelude.UTCTime
startReadSetActivationJobResponse_creationTime = Lens.lens (\StartReadSetActivationJobResponse' {creationTime} -> creationTime) (\s@StartReadSetActivationJobResponse' {} a -> s {creationTime = a} :: StartReadSetActivationJobResponse) Prelude.. Data._Time

-- | The job\'s ID.
startReadSetActivationJobResponse_id :: Lens.Lens' StartReadSetActivationJobResponse Prelude.Text
startReadSetActivationJobResponse_id = Lens.lens (\StartReadSetActivationJobResponse' {id} -> id) (\s@StartReadSetActivationJobResponse' {} a -> s {id = a} :: StartReadSetActivationJobResponse)

-- | The read set\'s sequence store ID.
startReadSetActivationJobResponse_sequenceStoreId :: Lens.Lens' StartReadSetActivationJobResponse Prelude.Text
startReadSetActivationJobResponse_sequenceStoreId = Lens.lens (\StartReadSetActivationJobResponse' {sequenceStoreId} -> sequenceStoreId) (\s@StartReadSetActivationJobResponse' {} a -> s {sequenceStoreId = a} :: StartReadSetActivationJobResponse)

-- | The job\'s status.
startReadSetActivationJobResponse_status :: Lens.Lens' StartReadSetActivationJobResponse ReadSetActivationJobStatus
startReadSetActivationJobResponse_status = Lens.lens (\StartReadSetActivationJobResponse' {status} -> status) (\s@StartReadSetActivationJobResponse' {} a -> s {status = a} :: StartReadSetActivationJobResponse)

instance
  Prelude.NFData
    StartReadSetActivationJobResponse
  where
  rnf StartReadSetActivationJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf status
