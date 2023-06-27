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
-- Module      : Amazonka.Omics.StartReadSetImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a read set import job.
module Amazonka.Omics.StartReadSetImportJob
  ( -- * Creating a Request
    StartReadSetImportJob (..),
    newStartReadSetImportJob,

    -- * Request Lenses
    startReadSetImportJob_clientToken,
    startReadSetImportJob_sequenceStoreId,
    startReadSetImportJob_roleArn,
    startReadSetImportJob_sources,

    -- * Destructuring the Response
    StartReadSetImportJobResponse (..),
    newStartReadSetImportJobResponse,

    -- * Response Lenses
    startReadSetImportJobResponse_httpStatus,
    startReadSetImportJobResponse_id,
    startReadSetImportJobResponse_sequenceStoreId,
    startReadSetImportJobResponse_roleArn,
    startReadSetImportJobResponse_status,
    startReadSetImportJobResponse_creationTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartReadSetImportJob' smart constructor.
data StartReadSetImportJob = StartReadSetImportJob'
  { -- | To ensure that jobs don\'t run multiple times, specify a unique token
    -- for each job.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The read set\'s sequence store ID.
    sequenceStoreId :: Prelude.Text,
    -- | A service role for the job.
    roleArn :: Prelude.Text,
    -- | The job\'s source files.
    sources :: Prelude.NonEmpty StartReadSetImportJobSourceItem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReadSetImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startReadSetImportJob_clientToken' - To ensure that jobs don\'t run multiple times, specify a unique token
-- for each job.
--
-- 'sequenceStoreId', 'startReadSetImportJob_sequenceStoreId' - The read set\'s sequence store ID.
--
-- 'roleArn', 'startReadSetImportJob_roleArn' - A service role for the job.
--
-- 'sources', 'startReadSetImportJob_sources' - The job\'s source files.
newStartReadSetImportJob ::
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'sources'
  Prelude.NonEmpty StartReadSetImportJobSourceItem ->
  StartReadSetImportJob
newStartReadSetImportJob
  pSequenceStoreId_
  pRoleArn_
  pSources_ =
    StartReadSetImportJob'
      { clientToken =
          Prelude.Nothing,
        sequenceStoreId = pSequenceStoreId_,
        roleArn = pRoleArn_,
        sources = Lens.coerced Lens.# pSources_
      }

-- | To ensure that jobs don\'t run multiple times, specify a unique token
-- for each job.
startReadSetImportJob_clientToken :: Lens.Lens' StartReadSetImportJob (Prelude.Maybe Prelude.Text)
startReadSetImportJob_clientToken = Lens.lens (\StartReadSetImportJob' {clientToken} -> clientToken) (\s@StartReadSetImportJob' {} a -> s {clientToken = a} :: StartReadSetImportJob)

-- | The read set\'s sequence store ID.
startReadSetImportJob_sequenceStoreId :: Lens.Lens' StartReadSetImportJob Prelude.Text
startReadSetImportJob_sequenceStoreId = Lens.lens (\StartReadSetImportJob' {sequenceStoreId} -> sequenceStoreId) (\s@StartReadSetImportJob' {} a -> s {sequenceStoreId = a} :: StartReadSetImportJob)

-- | A service role for the job.
startReadSetImportJob_roleArn :: Lens.Lens' StartReadSetImportJob Prelude.Text
startReadSetImportJob_roleArn = Lens.lens (\StartReadSetImportJob' {roleArn} -> roleArn) (\s@StartReadSetImportJob' {} a -> s {roleArn = a} :: StartReadSetImportJob)

-- | The job\'s source files.
startReadSetImportJob_sources :: Lens.Lens' StartReadSetImportJob (Prelude.NonEmpty StartReadSetImportJobSourceItem)
startReadSetImportJob_sources = Lens.lens (\StartReadSetImportJob' {sources} -> sources) (\s@StartReadSetImportJob' {} a -> s {sources = a} :: StartReadSetImportJob) Prelude.. Lens.coerced

instance Core.AWSRequest StartReadSetImportJob where
  type
    AWSResponse StartReadSetImportJob =
      StartReadSetImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartReadSetImportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "sequenceStoreId")
            Prelude.<*> (x Data..:> "roleArn")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "creationTime")
      )

instance Prelude.Hashable StartReadSetImportJob where
  hashWithSalt _salt StartReadSetImportJob' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` sequenceStoreId
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` sources

instance Prelude.NFData StartReadSetImportJob where
  rnf StartReadSetImportJob' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf sources

instance Data.ToHeaders StartReadSetImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartReadSetImportJob where
  toJSON StartReadSetImportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("sources" Data..= sources)
          ]
      )

instance Data.ToPath StartReadSetImportJob where
  toPath StartReadSetImportJob' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/importjob"
      ]

instance Data.ToQuery StartReadSetImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartReadSetImportJobResponse' smart constructor.
data StartReadSetImportJobResponse = StartReadSetImportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The job\'s ID.
    id :: Prelude.Text,
    -- | The read set\'s sequence store ID.
    sequenceStoreId :: Prelude.Text,
    -- | The job\'s service role ARN.
    roleArn :: Prelude.Text,
    -- | The job\'s status.
    status :: ReadSetImportJobStatus,
    -- | When the job was created.
    creationTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReadSetImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startReadSetImportJobResponse_httpStatus' - The response's http status code.
--
-- 'id', 'startReadSetImportJobResponse_id' - The job\'s ID.
--
-- 'sequenceStoreId', 'startReadSetImportJobResponse_sequenceStoreId' - The read set\'s sequence store ID.
--
-- 'roleArn', 'startReadSetImportJobResponse_roleArn' - The job\'s service role ARN.
--
-- 'status', 'startReadSetImportJobResponse_status' - The job\'s status.
--
-- 'creationTime', 'startReadSetImportJobResponse_creationTime' - When the job was created.
newStartReadSetImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'status'
  ReadSetImportJobStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  StartReadSetImportJobResponse
newStartReadSetImportJobResponse
  pHttpStatus_
  pId_
  pSequenceStoreId_
  pRoleArn_
  pStatus_
  pCreationTime_ =
    StartReadSetImportJobResponse'
      { httpStatus =
          pHttpStatus_,
        id = pId_,
        sequenceStoreId = pSequenceStoreId_,
        roleArn = pRoleArn_,
        status = pStatus_,
        creationTime =
          Data._Time Lens.# pCreationTime_
      }

-- | The response's http status code.
startReadSetImportJobResponse_httpStatus :: Lens.Lens' StartReadSetImportJobResponse Prelude.Int
startReadSetImportJobResponse_httpStatus = Lens.lens (\StartReadSetImportJobResponse' {httpStatus} -> httpStatus) (\s@StartReadSetImportJobResponse' {} a -> s {httpStatus = a} :: StartReadSetImportJobResponse)

-- | The job\'s ID.
startReadSetImportJobResponse_id :: Lens.Lens' StartReadSetImportJobResponse Prelude.Text
startReadSetImportJobResponse_id = Lens.lens (\StartReadSetImportJobResponse' {id} -> id) (\s@StartReadSetImportJobResponse' {} a -> s {id = a} :: StartReadSetImportJobResponse)

-- | The read set\'s sequence store ID.
startReadSetImportJobResponse_sequenceStoreId :: Lens.Lens' StartReadSetImportJobResponse Prelude.Text
startReadSetImportJobResponse_sequenceStoreId = Lens.lens (\StartReadSetImportJobResponse' {sequenceStoreId} -> sequenceStoreId) (\s@StartReadSetImportJobResponse' {} a -> s {sequenceStoreId = a} :: StartReadSetImportJobResponse)

-- | The job\'s service role ARN.
startReadSetImportJobResponse_roleArn :: Lens.Lens' StartReadSetImportJobResponse Prelude.Text
startReadSetImportJobResponse_roleArn = Lens.lens (\StartReadSetImportJobResponse' {roleArn} -> roleArn) (\s@StartReadSetImportJobResponse' {} a -> s {roleArn = a} :: StartReadSetImportJobResponse)

-- | The job\'s status.
startReadSetImportJobResponse_status :: Lens.Lens' StartReadSetImportJobResponse ReadSetImportJobStatus
startReadSetImportJobResponse_status = Lens.lens (\StartReadSetImportJobResponse' {status} -> status) (\s@StartReadSetImportJobResponse' {} a -> s {status = a} :: StartReadSetImportJobResponse)

-- | When the job was created.
startReadSetImportJobResponse_creationTime :: Lens.Lens' StartReadSetImportJobResponse Prelude.UTCTime
startReadSetImportJobResponse_creationTime = Lens.lens (\StartReadSetImportJobResponse' {creationTime} -> creationTime) (\s@StartReadSetImportJobResponse' {} a -> s {creationTime = a} :: StartReadSetImportJobResponse) Prelude.. Data._Time

instance Prelude.NFData StartReadSetImportJobResponse where
  rnf StartReadSetImportJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf creationTime
