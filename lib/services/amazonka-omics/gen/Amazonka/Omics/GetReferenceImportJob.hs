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
-- Module      : Amazonka.Omics.GetReferenceImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a reference import job.
module Amazonka.Omics.GetReferenceImportJob
  ( -- * Creating a Request
    GetReferenceImportJob (..),
    newGetReferenceImportJob,

    -- * Request Lenses
    getReferenceImportJob_id,
    getReferenceImportJob_referenceStoreId,

    -- * Destructuring the Response
    GetReferenceImportJobResponse (..),
    newGetReferenceImportJobResponse,

    -- * Response Lenses
    getReferenceImportJobResponse_completionTime,
    getReferenceImportJobResponse_statusMessage,
    getReferenceImportJobResponse_httpStatus,
    getReferenceImportJobResponse_creationTime,
    getReferenceImportJobResponse_id,
    getReferenceImportJobResponse_referenceStoreId,
    getReferenceImportJobResponse_roleArn,
    getReferenceImportJobResponse_sources,
    getReferenceImportJobResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetReferenceImportJob' smart constructor.
data GetReferenceImportJob = GetReferenceImportJob'
  { -- | The job\'s ID.
    id :: Prelude.Text,
    -- | The job\'s reference store ID.
    referenceStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReferenceImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getReferenceImportJob_id' - The job\'s ID.
--
-- 'referenceStoreId', 'getReferenceImportJob_referenceStoreId' - The job\'s reference store ID.
newGetReferenceImportJob ::
  -- | 'id'
  Prelude.Text ->
  -- | 'referenceStoreId'
  Prelude.Text ->
  GetReferenceImportJob
newGetReferenceImportJob pId_ pReferenceStoreId_ =
  GetReferenceImportJob'
    { id = pId_,
      referenceStoreId = pReferenceStoreId_
    }

-- | The job\'s ID.
getReferenceImportJob_id :: Lens.Lens' GetReferenceImportJob Prelude.Text
getReferenceImportJob_id = Lens.lens (\GetReferenceImportJob' {id} -> id) (\s@GetReferenceImportJob' {} a -> s {id = a} :: GetReferenceImportJob)

-- | The job\'s reference store ID.
getReferenceImportJob_referenceStoreId :: Lens.Lens' GetReferenceImportJob Prelude.Text
getReferenceImportJob_referenceStoreId = Lens.lens (\GetReferenceImportJob' {referenceStoreId} -> referenceStoreId) (\s@GetReferenceImportJob' {} a -> s {referenceStoreId = a} :: GetReferenceImportJob)

instance Core.AWSRequest GetReferenceImportJob where
  type
    AWSResponse GetReferenceImportJob =
      GetReferenceImportJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReferenceImportJobResponse'
            Prelude.<$> (x Data..?> "completionTime")
            Prelude.<*> (x Data..?> "statusMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "referenceStoreId")
            Prelude.<*> (x Data..:> "roleArn")
            Prelude.<*> (x Data..?> "sources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable GetReferenceImportJob where
  hashWithSalt _salt GetReferenceImportJob' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` referenceStoreId

instance Prelude.NFData GetReferenceImportJob where
  rnf GetReferenceImportJob' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf referenceStoreId

instance Data.ToHeaders GetReferenceImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetReferenceImportJob where
  toPath GetReferenceImportJob' {..} =
    Prelude.mconcat
      [ "/referencestore/",
        Data.toBS referenceStoreId,
        "/importjob/",
        Data.toBS id
      ]

instance Data.ToQuery GetReferenceImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetReferenceImportJobResponse' smart constructor.
data GetReferenceImportJobResponse = GetReferenceImportJobResponse'
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
    -- | The job\'s reference store ID.
    referenceStoreId :: Prelude.Text,
    -- | The job\'s service role ARN.
    roleArn :: Prelude.Text,
    -- | The job\'s sources.
    sources :: [ImportReferenceSourceItem],
    -- | The job\'s status.
    status :: ReferenceImportJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReferenceImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'getReferenceImportJobResponse_completionTime' - When the job completed.
--
-- 'statusMessage', 'getReferenceImportJobResponse_statusMessage' - The job\'s status message.
--
-- 'httpStatus', 'getReferenceImportJobResponse_httpStatus' - The response's http status code.
--
-- 'creationTime', 'getReferenceImportJobResponse_creationTime' - When the job was created.
--
-- 'id', 'getReferenceImportJobResponse_id' - The job\'s ID.
--
-- 'referenceStoreId', 'getReferenceImportJobResponse_referenceStoreId' - The job\'s reference store ID.
--
-- 'roleArn', 'getReferenceImportJobResponse_roleArn' - The job\'s service role ARN.
--
-- 'sources', 'getReferenceImportJobResponse_sources' - The job\'s sources.
--
-- 'status', 'getReferenceImportJobResponse_status' - The job\'s status.
newGetReferenceImportJobResponse ::
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
  GetReferenceImportJobResponse
newGetReferenceImportJobResponse
  pHttpStatus_
  pCreationTime_
  pId_
  pReferenceStoreId_
  pRoleArn_
  pStatus_ =
    GetReferenceImportJobResponse'
      { completionTime =
          Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        id = pId_,
        referenceStoreId = pReferenceStoreId_,
        roleArn = pRoleArn_,
        sources = Prelude.mempty,
        status = pStatus_
      }

-- | When the job completed.
getReferenceImportJobResponse_completionTime :: Lens.Lens' GetReferenceImportJobResponse (Prelude.Maybe Prelude.UTCTime)
getReferenceImportJobResponse_completionTime = Lens.lens (\GetReferenceImportJobResponse' {completionTime} -> completionTime) (\s@GetReferenceImportJobResponse' {} a -> s {completionTime = a} :: GetReferenceImportJobResponse) Prelude.. Lens.mapping Data._Time

-- | The job\'s status message.
getReferenceImportJobResponse_statusMessage :: Lens.Lens' GetReferenceImportJobResponse (Prelude.Maybe Prelude.Text)
getReferenceImportJobResponse_statusMessage = Lens.lens (\GetReferenceImportJobResponse' {statusMessage} -> statusMessage) (\s@GetReferenceImportJobResponse' {} a -> s {statusMessage = a} :: GetReferenceImportJobResponse)

-- | The response's http status code.
getReferenceImportJobResponse_httpStatus :: Lens.Lens' GetReferenceImportJobResponse Prelude.Int
getReferenceImportJobResponse_httpStatus = Lens.lens (\GetReferenceImportJobResponse' {httpStatus} -> httpStatus) (\s@GetReferenceImportJobResponse' {} a -> s {httpStatus = a} :: GetReferenceImportJobResponse)

-- | When the job was created.
getReferenceImportJobResponse_creationTime :: Lens.Lens' GetReferenceImportJobResponse Prelude.UTCTime
getReferenceImportJobResponse_creationTime = Lens.lens (\GetReferenceImportJobResponse' {creationTime} -> creationTime) (\s@GetReferenceImportJobResponse' {} a -> s {creationTime = a} :: GetReferenceImportJobResponse) Prelude.. Data._Time

-- | The job\'s ID.
getReferenceImportJobResponse_id :: Lens.Lens' GetReferenceImportJobResponse Prelude.Text
getReferenceImportJobResponse_id = Lens.lens (\GetReferenceImportJobResponse' {id} -> id) (\s@GetReferenceImportJobResponse' {} a -> s {id = a} :: GetReferenceImportJobResponse)

-- | The job\'s reference store ID.
getReferenceImportJobResponse_referenceStoreId :: Lens.Lens' GetReferenceImportJobResponse Prelude.Text
getReferenceImportJobResponse_referenceStoreId = Lens.lens (\GetReferenceImportJobResponse' {referenceStoreId} -> referenceStoreId) (\s@GetReferenceImportJobResponse' {} a -> s {referenceStoreId = a} :: GetReferenceImportJobResponse)

-- | The job\'s service role ARN.
getReferenceImportJobResponse_roleArn :: Lens.Lens' GetReferenceImportJobResponse Prelude.Text
getReferenceImportJobResponse_roleArn = Lens.lens (\GetReferenceImportJobResponse' {roleArn} -> roleArn) (\s@GetReferenceImportJobResponse' {} a -> s {roleArn = a} :: GetReferenceImportJobResponse)

-- | The job\'s sources.
getReferenceImportJobResponse_sources :: Lens.Lens' GetReferenceImportJobResponse [ImportReferenceSourceItem]
getReferenceImportJobResponse_sources = Lens.lens (\GetReferenceImportJobResponse' {sources} -> sources) (\s@GetReferenceImportJobResponse' {} a -> s {sources = a} :: GetReferenceImportJobResponse) Prelude.. Lens.coerced

-- | The job\'s status.
getReferenceImportJobResponse_status :: Lens.Lens' GetReferenceImportJobResponse ReferenceImportJobStatus
getReferenceImportJobResponse_status = Lens.lens (\GetReferenceImportJobResponse' {status} -> status) (\s@GetReferenceImportJobResponse' {} a -> s {status = a} :: GetReferenceImportJobResponse)

instance Prelude.NFData GetReferenceImportJobResponse where
  rnf GetReferenceImportJobResponse' {..} =
    Prelude.rnf completionTime `Prelude.seq`
      Prelude.rnf statusMessage `Prelude.seq`
        Prelude.rnf httpStatus `Prelude.seq`
          Prelude.rnf creationTime `Prelude.seq`
            Prelude.rnf id `Prelude.seq`
              Prelude.rnf referenceStoreId `Prelude.seq`
                Prelude.rnf roleArn `Prelude.seq`
                  Prelude.rnf sources `Prelude.seq`
                    Prelude.rnf status
