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
-- Module      : Amazonka.IotTwinMaker.DeleteSyncJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the SyncJob.
module Amazonka.IotTwinMaker.DeleteSyncJob
  ( -- * Creating a Request
    DeleteSyncJob (..),
    newDeleteSyncJob,

    -- * Request Lenses
    deleteSyncJob_workspaceId,
    deleteSyncJob_syncSource,

    -- * Destructuring the Response
    DeleteSyncJobResponse (..),
    newDeleteSyncJobResponse,

    -- * Response Lenses
    deleteSyncJobResponse_httpStatus,
    deleteSyncJobResponse_state,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSyncJob' smart constructor.
data DeleteSyncJob = DeleteSyncJob'
  { -- | The workspace Id.
    workspaceId :: Prelude.Text,
    -- | The sync source.
    --
    -- Currently the only supported syncSoucre is @SITEWISE @.
    syncSource :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSyncJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'deleteSyncJob_workspaceId' - The workspace Id.
--
-- 'syncSource', 'deleteSyncJob_syncSource' - The sync source.
--
-- Currently the only supported syncSoucre is @SITEWISE @.
newDeleteSyncJob ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'syncSource'
  Prelude.Text ->
  DeleteSyncJob
newDeleteSyncJob pWorkspaceId_ pSyncSource_ =
  DeleteSyncJob'
    { workspaceId = pWorkspaceId_,
      syncSource = pSyncSource_
    }

-- | The workspace Id.
deleteSyncJob_workspaceId :: Lens.Lens' DeleteSyncJob Prelude.Text
deleteSyncJob_workspaceId = Lens.lens (\DeleteSyncJob' {workspaceId} -> workspaceId) (\s@DeleteSyncJob' {} a -> s {workspaceId = a} :: DeleteSyncJob)

-- | The sync source.
--
-- Currently the only supported syncSoucre is @SITEWISE @.
deleteSyncJob_syncSource :: Lens.Lens' DeleteSyncJob Prelude.Text
deleteSyncJob_syncSource = Lens.lens (\DeleteSyncJob' {syncSource} -> syncSource) (\s@DeleteSyncJob' {} a -> s {syncSource = a} :: DeleteSyncJob)

instance Core.AWSRequest DeleteSyncJob where
  type
    AWSResponse DeleteSyncJob =
      DeleteSyncJobResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSyncJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "state")
      )

instance Prelude.Hashable DeleteSyncJob where
  hashWithSalt _salt DeleteSyncJob' {..} =
    _salt
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` syncSource

instance Prelude.NFData DeleteSyncJob where
  rnf DeleteSyncJob' {..} =
    Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf syncSource

instance Data.ToHeaders DeleteSyncJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSyncJob where
  toPath DeleteSyncJob' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/sync-jobs/",
        Data.toBS syncSource
      ]

instance Data.ToQuery DeleteSyncJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSyncJobResponse' smart constructor.
data DeleteSyncJobResponse = DeleteSyncJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The SyncJob response state.
    state :: SyncJobState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSyncJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSyncJobResponse_httpStatus' - The response's http status code.
--
-- 'state', 'deleteSyncJobResponse_state' - The SyncJob response state.
newDeleteSyncJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'state'
  SyncJobState ->
  DeleteSyncJobResponse
newDeleteSyncJobResponse pHttpStatus_ pState_ =
  DeleteSyncJobResponse'
    { httpStatus = pHttpStatus_,
      state = pState_
    }

-- | The response's http status code.
deleteSyncJobResponse_httpStatus :: Lens.Lens' DeleteSyncJobResponse Prelude.Int
deleteSyncJobResponse_httpStatus = Lens.lens (\DeleteSyncJobResponse' {httpStatus} -> httpStatus) (\s@DeleteSyncJobResponse' {} a -> s {httpStatus = a} :: DeleteSyncJobResponse)

-- | The SyncJob response state.
deleteSyncJobResponse_state :: Lens.Lens' DeleteSyncJobResponse SyncJobState
deleteSyncJobResponse_state = Lens.lens (\DeleteSyncJobResponse' {state} -> state) (\s@DeleteSyncJobResponse' {} a -> s {state = a} :: DeleteSyncJobResponse)

instance Prelude.NFData DeleteSyncJobResponse where
  rnf DeleteSyncJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf state
