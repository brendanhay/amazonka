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
-- Module      : Amazonka.IotTwinMaker.GetSyncJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the SyncJob.
module Amazonka.IotTwinMaker.GetSyncJob
  ( -- * Creating a Request
    GetSyncJob (..),
    newGetSyncJob,

    -- * Request Lenses
    getSyncJob_workspaceId,
    getSyncJob_syncSource,

    -- * Destructuring the Response
    GetSyncJobResponse (..),
    newGetSyncJobResponse,

    -- * Response Lenses
    getSyncJobResponse_httpStatus,
    getSyncJobResponse_arn,
    getSyncJobResponse_workspaceId,
    getSyncJobResponse_syncSource,
    getSyncJobResponse_syncRole,
    getSyncJobResponse_status,
    getSyncJobResponse_creationDateTime,
    getSyncJobResponse_updateDateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSyncJob' smart constructor.
data GetSyncJob = GetSyncJob'
  { -- | The workspace ID.
    workspaceId :: Prelude.Maybe Prelude.Text,
    -- | The sync source.
    --
    -- Currently the only supported syncSource is @SITEWISE @.
    syncSource :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSyncJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'getSyncJob_workspaceId' - The workspace ID.
--
-- 'syncSource', 'getSyncJob_syncSource' - The sync source.
--
-- Currently the only supported syncSource is @SITEWISE @.
newGetSyncJob ::
  -- | 'syncSource'
  Prelude.Text ->
  GetSyncJob
newGetSyncJob pSyncSource_ =
  GetSyncJob'
    { workspaceId = Prelude.Nothing,
      syncSource = pSyncSource_
    }

-- | The workspace ID.
getSyncJob_workspaceId :: Lens.Lens' GetSyncJob (Prelude.Maybe Prelude.Text)
getSyncJob_workspaceId = Lens.lens (\GetSyncJob' {workspaceId} -> workspaceId) (\s@GetSyncJob' {} a -> s {workspaceId = a} :: GetSyncJob)

-- | The sync source.
--
-- Currently the only supported syncSource is @SITEWISE @.
getSyncJob_syncSource :: Lens.Lens' GetSyncJob Prelude.Text
getSyncJob_syncSource = Lens.lens (\GetSyncJob' {syncSource} -> syncSource) (\s@GetSyncJob' {} a -> s {syncSource = a} :: GetSyncJob)

instance Core.AWSRequest GetSyncJob where
  type AWSResponse GetSyncJob = GetSyncJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSyncJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "workspaceId")
            Prelude.<*> (x Data..:> "syncSource")
            Prelude.<*> (x Data..:> "syncRole")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "creationDateTime")
            Prelude.<*> (x Data..:> "updateDateTime")
      )

instance Prelude.Hashable GetSyncJob where
  hashWithSalt _salt GetSyncJob' {..} =
    _salt
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` syncSource

instance Prelude.NFData GetSyncJob where
  rnf GetSyncJob' {..} =
    Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf syncSource

instance Data.ToHeaders GetSyncJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSyncJob where
  toPath GetSyncJob' {..} =
    Prelude.mconcat
      ["/sync-jobs/", Data.toBS syncSource]

instance Data.ToQuery GetSyncJob where
  toQuery GetSyncJob' {..} =
    Prelude.mconcat ["workspace" Data.=: workspaceId]

-- | /See:/ 'newGetSyncJobResponse' smart constructor.
data GetSyncJobResponse = GetSyncJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The sync job ARN.
    arn :: Prelude.Text,
    -- | The ID of the workspace that contains the sync job.
    workspaceId :: Prelude.Text,
    -- | The sync soucre.
    --
    -- Currently the only supported syncSource is @SITEWISE @.
    syncSource :: Prelude.Text,
    -- | The sync IAM role.
    syncRole :: Prelude.Text,
    -- | The SyncJob response status.
    status :: SyncJobStatus,
    -- | The creation date and time.
    creationDateTime :: Data.POSIX,
    -- | The update date and time.
    updateDateTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSyncJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getSyncJobResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getSyncJobResponse_arn' - The sync job ARN.
--
-- 'workspaceId', 'getSyncJobResponse_workspaceId' - The ID of the workspace that contains the sync job.
--
-- 'syncSource', 'getSyncJobResponse_syncSource' - The sync soucre.
--
-- Currently the only supported syncSource is @SITEWISE @.
--
-- 'syncRole', 'getSyncJobResponse_syncRole' - The sync IAM role.
--
-- 'status', 'getSyncJobResponse_status' - The SyncJob response status.
--
-- 'creationDateTime', 'getSyncJobResponse_creationDateTime' - The creation date and time.
--
-- 'updateDateTime', 'getSyncJobResponse_updateDateTime' - The update date and time.
newGetSyncJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'syncSource'
  Prelude.Text ->
  -- | 'syncRole'
  Prelude.Text ->
  -- | 'status'
  SyncJobStatus ->
  -- | 'creationDateTime'
  Prelude.UTCTime ->
  -- | 'updateDateTime'
  Prelude.UTCTime ->
  GetSyncJobResponse
newGetSyncJobResponse
  pHttpStatus_
  pArn_
  pWorkspaceId_
  pSyncSource_
  pSyncRole_
  pStatus_
  pCreationDateTime_
  pUpdateDateTime_ =
    GetSyncJobResponse'
      { httpStatus = pHttpStatus_,
        arn = pArn_,
        workspaceId = pWorkspaceId_,
        syncSource = pSyncSource_,
        syncRole = pSyncRole_,
        status = pStatus_,
        creationDateTime =
          Data._Time Lens.# pCreationDateTime_,
        updateDateTime = Data._Time Lens.# pUpdateDateTime_
      }

-- | The response's http status code.
getSyncJobResponse_httpStatus :: Lens.Lens' GetSyncJobResponse Prelude.Int
getSyncJobResponse_httpStatus = Lens.lens (\GetSyncJobResponse' {httpStatus} -> httpStatus) (\s@GetSyncJobResponse' {} a -> s {httpStatus = a} :: GetSyncJobResponse)

-- | The sync job ARN.
getSyncJobResponse_arn :: Lens.Lens' GetSyncJobResponse Prelude.Text
getSyncJobResponse_arn = Lens.lens (\GetSyncJobResponse' {arn} -> arn) (\s@GetSyncJobResponse' {} a -> s {arn = a} :: GetSyncJobResponse)

-- | The ID of the workspace that contains the sync job.
getSyncJobResponse_workspaceId :: Lens.Lens' GetSyncJobResponse Prelude.Text
getSyncJobResponse_workspaceId = Lens.lens (\GetSyncJobResponse' {workspaceId} -> workspaceId) (\s@GetSyncJobResponse' {} a -> s {workspaceId = a} :: GetSyncJobResponse)

-- | The sync soucre.
--
-- Currently the only supported syncSource is @SITEWISE @.
getSyncJobResponse_syncSource :: Lens.Lens' GetSyncJobResponse Prelude.Text
getSyncJobResponse_syncSource = Lens.lens (\GetSyncJobResponse' {syncSource} -> syncSource) (\s@GetSyncJobResponse' {} a -> s {syncSource = a} :: GetSyncJobResponse)

-- | The sync IAM role.
getSyncJobResponse_syncRole :: Lens.Lens' GetSyncJobResponse Prelude.Text
getSyncJobResponse_syncRole = Lens.lens (\GetSyncJobResponse' {syncRole} -> syncRole) (\s@GetSyncJobResponse' {} a -> s {syncRole = a} :: GetSyncJobResponse)

-- | The SyncJob response status.
getSyncJobResponse_status :: Lens.Lens' GetSyncJobResponse SyncJobStatus
getSyncJobResponse_status = Lens.lens (\GetSyncJobResponse' {status} -> status) (\s@GetSyncJobResponse' {} a -> s {status = a} :: GetSyncJobResponse)

-- | The creation date and time.
getSyncJobResponse_creationDateTime :: Lens.Lens' GetSyncJobResponse Prelude.UTCTime
getSyncJobResponse_creationDateTime = Lens.lens (\GetSyncJobResponse' {creationDateTime} -> creationDateTime) (\s@GetSyncJobResponse' {} a -> s {creationDateTime = a} :: GetSyncJobResponse) Prelude.. Data._Time

-- | The update date and time.
getSyncJobResponse_updateDateTime :: Lens.Lens' GetSyncJobResponse Prelude.UTCTime
getSyncJobResponse_updateDateTime = Lens.lens (\GetSyncJobResponse' {updateDateTime} -> updateDateTime) (\s@GetSyncJobResponse' {} a -> s {updateDateTime = a} :: GetSyncJobResponse) Prelude.. Data._Time

instance Prelude.NFData GetSyncJobResponse where
  rnf GetSyncJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf syncSource
      `Prelude.seq` Prelude.rnf syncRole
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf updateDateTime
