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
-- Module      : Amazonka.Omics.StartReferenceImportJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a reference import job.
module Amazonka.Omics.StartReferenceImportJob
  ( -- * Creating a Request
    StartReferenceImportJob (..),
    newStartReferenceImportJob,

    -- * Request Lenses
    startReferenceImportJob_clientToken,
    startReferenceImportJob_referenceStoreId,
    startReferenceImportJob_roleArn,
    startReferenceImportJob_sources,

    -- * Destructuring the Response
    StartReferenceImportJobResponse (..),
    newStartReferenceImportJobResponse,

    -- * Response Lenses
    startReferenceImportJobResponse_httpStatus,
    startReferenceImportJobResponse_creationTime,
    startReferenceImportJobResponse_id,
    startReferenceImportJobResponse_referenceStoreId,
    startReferenceImportJobResponse_roleArn,
    startReferenceImportJobResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartReferenceImportJob' smart constructor.
data StartReferenceImportJob = StartReferenceImportJob'
  { -- | To ensure that jobs don\'t run multiple times, specify a unique token
    -- for each job.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The job\'s reference store ID.
    referenceStoreId :: Prelude.Text,
    -- | A service role for the job.
    roleArn :: Prelude.Text,
    -- | Sources for the job.
    sources :: Prelude.NonEmpty StartReferenceImportJobSourceItem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReferenceImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startReferenceImportJob_clientToken' - To ensure that jobs don\'t run multiple times, specify a unique token
-- for each job.
--
-- 'referenceStoreId', 'startReferenceImportJob_referenceStoreId' - The job\'s reference store ID.
--
-- 'roleArn', 'startReferenceImportJob_roleArn' - A service role for the job.
--
-- 'sources', 'startReferenceImportJob_sources' - Sources for the job.
newStartReferenceImportJob ::
  -- | 'referenceStoreId'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'sources'
  Prelude.NonEmpty StartReferenceImportJobSourceItem ->
  StartReferenceImportJob
newStartReferenceImportJob
  pReferenceStoreId_
  pRoleArn_
  pSources_ =
    StartReferenceImportJob'
      { clientToken =
          Prelude.Nothing,
        referenceStoreId = pReferenceStoreId_,
        roleArn = pRoleArn_,
        sources = Lens.coerced Lens.# pSources_
      }

-- | To ensure that jobs don\'t run multiple times, specify a unique token
-- for each job.
startReferenceImportJob_clientToken :: Lens.Lens' StartReferenceImportJob (Prelude.Maybe Prelude.Text)
startReferenceImportJob_clientToken = Lens.lens (\StartReferenceImportJob' {clientToken} -> clientToken) (\s@StartReferenceImportJob' {} a -> s {clientToken = a} :: StartReferenceImportJob)

-- | The job\'s reference store ID.
startReferenceImportJob_referenceStoreId :: Lens.Lens' StartReferenceImportJob Prelude.Text
startReferenceImportJob_referenceStoreId = Lens.lens (\StartReferenceImportJob' {referenceStoreId} -> referenceStoreId) (\s@StartReferenceImportJob' {} a -> s {referenceStoreId = a} :: StartReferenceImportJob)

-- | A service role for the job.
startReferenceImportJob_roleArn :: Lens.Lens' StartReferenceImportJob Prelude.Text
startReferenceImportJob_roleArn = Lens.lens (\StartReferenceImportJob' {roleArn} -> roleArn) (\s@StartReferenceImportJob' {} a -> s {roleArn = a} :: StartReferenceImportJob)

-- | Sources for the job.
startReferenceImportJob_sources :: Lens.Lens' StartReferenceImportJob (Prelude.NonEmpty StartReferenceImportJobSourceItem)
startReferenceImportJob_sources = Lens.lens (\StartReferenceImportJob' {sources} -> sources) (\s@StartReferenceImportJob' {} a -> s {sources = a} :: StartReferenceImportJob) Prelude.. Lens.coerced

instance Core.AWSRequest StartReferenceImportJob where
  type
    AWSResponse StartReferenceImportJob =
      StartReferenceImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartReferenceImportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "referenceStoreId")
            Prelude.<*> (x Data..:> "roleArn")
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable StartReferenceImportJob where
  hashWithSalt _salt StartReferenceImportJob' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` referenceStoreId
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` sources

instance Prelude.NFData StartReferenceImportJob where
  rnf StartReferenceImportJob' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf referenceStoreId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf sources

instance Data.ToHeaders StartReferenceImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartReferenceImportJob where
  toJSON StartReferenceImportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("sources" Data..= sources)
          ]
      )

instance Data.ToPath StartReferenceImportJob where
  toPath StartReferenceImportJob' {..} =
    Prelude.mconcat
      [ "/referencestore/",
        Data.toBS referenceStoreId,
        "/importjob"
      ]

instance Data.ToQuery StartReferenceImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartReferenceImportJobResponse' smart constructor.
data StartReferenceImportJobResponse = StartReferenceImportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | When the job was created.
    creationTime :: Data.POSIX,
    -- | The job\'s ID.
    id :: Prelude.Text,
    -- | The job\'s reference store ID.
    referenceStoreId :: Prelude.Text,
    -- | The job\'s service role ARN.
    roleArn :: Prelude.Text,
    -- | The job\'s status.
    status :: ReferenceImportJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReferenceImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startReferenceImportJobResponse_httpStatus' - The response's http status code.
--
-- 'creationTime', 'startReferenceImportJobResponse_creationTime' - When the job was created.
--
-- 'id', 'startReferenceImportJobResponse_id' - The job\'s ID.
--
-- 'referenceStoreId', 'startReferenceImportJobResponse_referenceStoreId' - The job\'s reference store ID.
--
-- 'roleArn', 'startReferenceImportJobResponse_roleArn' - The job\'s service role ARN.
--
-- 'status', 'startReferenceImportJobResponse_status' - The job\'s status.
newStartReferenceImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'id'
  Prelude.Text ->
  -- | 'referenceStoreId'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'status'
  ReferenceImportJobStatus ->
  StartReferenceImportJobResponse
newStartReferenceImportJobResponse
  pHttpStatus_
  pCreationTime_
  pId_
  pReferenceStoreId_
  pRoleArn_
  pStatus_ =
    StartReferenceImportJobResponse'
      { httpStatus =
          pHttpStatus_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        id = pId_,
        referenceStoreId = pReferenceStoreId_,
        roleArn = pRoleArn_,
        status = pStatus_
      }

-- | The response's http status code.
startReferenceImportJobResponse_httpStatus :: Lens.Lens' StartReferenceImportJobResponse Prelude.Int
startReferenceImportJobResponse_httpStatus = Lens.lens (\StartReferenceImportJobResponse' {httpStatus} -> httpStatus) (\s@StartReferenceImportJobResponse' {} a -> s {httpStatus = a} :: StartReferenceImportJobResponse)

-- | When the job was created.
startReferenceImportJobResponse_creationTime :: Lens.Lens' StartReferenceImportJobResponse Prelude.UTCTime
startReferenceImportJobResponse_creationTime = Lens.lens (\StartReferenceImportJobResponse' {creationTime} -> creationTime) (\s@StartReferenceImportJobResponse' {} a -> s {creationTime = a} :: StartReferenceImportJobResponse) Prelude.. Data._Time

-- | The job\'s ID.
startReferenceImportJobResponse_id :: Lens.Lens' StartReferenceImportJobResponse Prelude.Text
startReferenceImportJobResponse_id = Lens.lens (\StartReferenceImportJobResponse' {id} -> id) (\s@StartReferenceImportJobResponse' {} a -> s {id = a} :: StartReferenceImportJobResponse)

-- | The job\'s reference store ID.
startReferenceImportJobResponse_referenceStoreId :: Lens.Lens' StartReferenceImportJobResponse Prelude.Text
startReferenceImportJobResponse_referenceStoreId = Lens.lens (\StartReferenceImportJobResponse' {referenceStoreId} -> referenceStoreId) (\s@StartReferenceImportJobResponse' {} a -> s {referenceStoreId = a} :: StartReferenceImportJobResponse)

-- | The job\'s service role ARN.
startReferenceImportJobResponse_roleArn :: Lens.Lens' StartReferenceImportJobResponse Prelude.Text
startReferenceImportJobResponse_roleArn = Lens.lens (\StartReferenceImportJobResponse' {roleArn} -> roleArn) (\s@StartReferenceImportJobResponse' {} a -> s {roleArn = a} :: StartReferenceImportJobResponse)

-- | The job\'s status.
startReferenceImportJobResponse_status :: Lens.Lens' StartReferenceImportJobResponse ReferenceImportJobStatus
startReferenceImportJobResponse_status = Lens.lens (\StartReferenceImportJobResponse' {status} -> status) (\s@StartReferenceImportJobResponse' {} a -> s {status = a} :: StartReferenceImportJobResponse)

instance
  Prelude.NFData
    StartReferenceImportJobResponse
  where
  rnf StartReferenceImportJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf referenceStoreId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf status
