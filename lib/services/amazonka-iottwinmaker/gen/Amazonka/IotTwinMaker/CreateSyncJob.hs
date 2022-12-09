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
-- Module      : Amazonka.IotTwinMaker.CreateSyncJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action creates a SyncJob.
module Amazonka.IotTwinMaker.CreateSyncJob
  ( -- * Creating a Request
    CreateSyncJob (..),
    newCreateSyncJob,

    -- * Request Lenses
    createSyncJob_tags,
    createSyncJob_workspaceId,
    createSyncJob_syncSource,
    createSyncJob_syncRole,

    -- * Destructuring the Response
    CreateSyncJobResponse (..),
    newCreateSyncJobResponse,

    -- * Response Lenses
    createSyncJobResponse_httpStatus,
    createSyncJobResponse_arn,
    createSyncJobResponse_creationDateTime,
    createSyncJobResponse_state,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSyncJob' smart constructor.
data CreateSyncJob = CreateSyncJob'
  { -- | The SyncJob tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The workspace Id.
    workspaceId :: Prelude.Text,
    -- | The sync source.
    --
    -- Currently the only supported syncSoucre is @SITEWISE @.
    syncSource :: Prelude.Text,
    -- | The SyncJob IAM role. This IAM role is used by the sync job to read from
    -- the syncSource, and create, update or delete the corresponding
    -- resources.
    syncRole :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSyncJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSyncJob_tags' - The SyncJob tags.
--
-- 'workspaceId', 'createSyncJob_workspaceId' - The workspace Id.
--
-- 'syncSource', 'createSyncJob_syncSource' - The sync source.
--
-- Currently the only supported syncSoucre is @SITEWISE @.
--
-- 'syncRole', 'createSyncJob_syncRole' - The SyncJob IAM role. This IAM role is used by the sync job to read from
-- the syncSource, and create, update or delete the corresponding
-- resources.
newCreateSyncJob ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'syncSource'
  Prelude.Text ->
  -- | 'syncRole'
  Prelude.Text ->
  CreateSyncJob
newCreateSyncJob
  pWorkspaceId_
  pSyncSource_
  pSyncRole_ =
    CreateSyncJob'
      { tags = Prelude.Nothing,
        workspaceId = pWorkspaceId_,
        syncSource = pSyncSource_,
        syncRole = pSyncRole_
      }

-- | The SyncJob tags.
createSyncJob_tags :: Lens.Lens' CreateSyncJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSyncJob_tags = Lens.lens (\CreateSyncJob' {tags} -> tags) (\s@CreateSyncJob' {} a -> s {tags = a} :: CreateSyncJob) Prelude.. Lens.mapping Lens.coerced

-- | The workspace Id.
createSyncJob_workspaceId :: Lens.Lens' CreateSyncJob Prelude.Text
createSyncJob_workspaceId = Lens.lens (\CreateSyncJob' {workspaceId} -> workspaceId) (\s@CreateSyncJob' {} a -> s {workspaceId = a} :: CreateSyncJob)

-- | The sync source.
--
-- Currently the only supported syncSoucre is @SITEWISE @.
createSyncJob_syncSource :: Lens.Lens' CreateSyncJob Prelude.Text
createSyncJob_syncSource = Lens.lens (\CreateSyncJob' {syncSource} -> syncSource) (\s@CreateSyncJob' {} a -> s {syncSource = a} :: CreateSyncJob)

-- | The SyncJob IAM role. This IAM role is used by the sync job to read from
-- the syncSource, and create, update or delete the corresponding
-- resources.
createSyncJob_syncRole :: Lens.Lens' CreateSyncJob Prelude.Text
createSyncJob_syncRole = Lens.lens (\CreateSyncJob' {syncRole} -> syncRole) (\s@CreateSyncJob' {} a -> s {syncRole = a} :: CreateSyncJob)

instance Core.AWSRequest CreateSyncJob where
  type
    AWSResponse CreateSyncJob =
      CreateSyncJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSyncJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "creationDateTime")
            Prelude.<*> (x Data..:> "state")
      )

instance Prelude.Hashable CreateSyncJob where
  hashWithSalt _salt CreateSyncJob' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` syncSource
      `Prelude.hashWithSalt` syncRole

instance Prelude.NFData CreateSyncJob where
  rnf CreateSyncJob' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf syncSource
      `Prelude.seq` Prelude.rnf syncRole

instance Data.ToHeaders CreateSyncJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSyncJob where
  toJSON CreateSyncJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("syncRole" Data..= syncRole)
          ]
      )

instance Data.ToPath CreateSyncJob where
  toPath CreateSyncJob' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/sync-jobs/",
        Data.toBS syncSource
      ]

instance Data.ToQuery CreateSyncJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSyncJobResponse' smart constructor.
data CreateSyncJobResponse = CreateSyncJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The SyncJob ARN.
    arn :: Prelude.Text,
    -- | The date and time for the SyncJob creation.
    creationDateTime :: Data.POSIX,
    -- | The SyncJob response state.
    state :: SyncJobState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSyncJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createSyncJobResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createSyncJobResponse_arn' - The SyncJob ARN.
--
-- 'creationDateTime', 'createSyncJobResponse_creationDateTime' - The date and time for the SyncJob creation.
--
-- 'state', 'createSyncJobResponse_state' - The SyncJob response state.
newCreateSyncJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationDateTime'
  Prelude.UTCTime ->
  -- | 'state'
  SyncJobState ->
  CreateSyncJobResponse
newCreateSyncJobResponse
  pHttpStatus_
  pArn_
  pCreationDateTime_
  pState_ =
    CreateSyncJobResponse'
      { httpStatus = pHttpStatus_,
        arn = pArn_,
        creationDateTime =
          Data._Time Lens.# pCreationDateTime_,
        state = pState_
      }

-- | The response's http status code.
createSyncJobResponse_httpStatus :: Lens.Lens' CreateSyncJobResponse Prelude.Int
createSyncJobResponse_httpStatus = Lens.lens (\CreateSyncJobResponse' {httpStatus} -> httpStatus) (\s@CreateSyncJobResponse' {} a -> s {httpStatus = a} :: CreateSyncJobResponse)

-- | The SyncJob ARN.
createSyncJobResponse_arn :: Lens.Lens' CreateSyncJobResponse Prelude.Text
createSyncJobResponse_arn = Lens.lens (\CreateSyncJobResponse' {arn} -> arn) (\s@CreateSyncJobResponse' {} a -> s {arn = a} :: CreateSyncJobResponse)

-- | The date and time for the SyncJob creation.
createSyncJobResponse_creationDateTime :: Lens.Lens' CreateSyncJobResponse Prelude.UTCTime
createSyncJobResponse_creationDateTime = Lens.lens (\CreateSyncJobResponse' {creationDateTime} -> creationDateTime) (\s@CreateSyncJobResponse' {} a -> s {creationDateTime = a} :: CreateSyncJobResponse) Prelude.. Data._Time

-- | The SyncJob response state.
createSyncJobResponse_state :: Lens.Lens' CreateSyncJobResponse SyncJobState
createSyncJobResponse_state = Lens.lens (\CreateSyncJobResponse' {state} -> state) (\s@CreateSyncJobResponse' {} a -> s {state = a} :: CreateSyncJobResponse)

instance Prelude.NFData CreateSyncJobResponse where
  rnf CreateSyncJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf state
