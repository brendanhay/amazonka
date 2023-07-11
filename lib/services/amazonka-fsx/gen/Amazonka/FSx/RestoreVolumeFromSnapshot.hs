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
-- Module      : Amazonka.FSx.RestoreVolumeFromSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an Amazon FSx for OpenZFS volume to the state saved by the
-- specified snapshot.
module Amazonka.FSx.RestoreVolumeFromSnapshot
  ( -- * Creating a Request
    RestoreVolumeFromSnapshot (..),
    newRestoreVolumeFromSnapshot,

    -- * Request Lenses
    restoreVolumeFromSnapshot_clientRequestToken,
    restoreVolumeFromSnapshot_options,
    restoreVolumeFromSnapshot_volumeId,
    restoreVolumeFromSnapshot_snapshotId,

    -- * Destructuring the Response
    RestoreVolumeFromSnapshotResponse (..),
    newRestoreVolumeFromSnapshotResponse,

    -- * Response Lenses
    restoreVolumeFromSnapshotResponse_administrativeActions,
    restoreVolumeFromSnapshotResponse_lifecycle,
    restoreVolumeFromSnapshotResponse_volumeId,
    restoreVolumeFromSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreVolumeFromSnapshot' smart constructor.
data RestoreVolumeFromSnapshot = RestoreVolumeFromSnapshot'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The settings used when restoring the specified volume from snapshot.
    --
    -- -   @DELETE_INTERMEDIATE_SNAPSHOTS@ - Deletes snapshots between the
    --     current state and the specified snapshot. If there are intermediate
    --     snapshots and this option isn\'t used, @RestoreVolumeFromSnapshot@
    --     fails.
    --
    -- -   @DELETE_CLONED_VOLUMES@ - Deletes any dependent clone volumes
    --     created from intermediate snapshots. If there are any dependent
    --     clone volumes and this option isn\'t used,
    --     @RestoreVolumeFromSnapshot@ fails.
    options :: Prelude.Maybe [RestoreOpenZFSVolumeOption],
    -- | The ID of the volume that you are restoring.
    volumeId :: Prelude.Text,
    -- | The ID of the source snapshot. Specifies the snapshot that you are
    -- restoring from.
    snapshotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreVolumeFromSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'restoreVolumeFromSnapshot_clientRequestToken' - Undocumented member.
--
-- 'options', 'restoreVolumeFromSnapshot_options' - The settings used when restoring the specified volume from snapshot.
--
-- -   @DELETE_INTERMEDIATE_SNAPSHOTS@ - Deletes snapshots between the
--     current state and the specified snapshot. If there are intermediate
--     snapshots and this option isn\'t used, @RestoreVolumeFromSnapshot@
--     fails.
--
-- -   @DELETE_CLONED_VOLUMES@ - Deletes any dependent clone volumes
--     created from intermediate snapshots. If there are any dependent
--     clone volumes and this option isn\'t used,
--     @RestoreVolumeFromSnapshot@ fails.
--
-- 'volumeId', 'restoreVolumeFromSnapshot_volumeId' - The ID of the volume that you are restoring.
--
-- 'snapshotId', 'restoreVolumeFromSnapshot_snapshotId' - The ID of the source snapshot. Specifies the snapshot that you are
-- restoring from.
newRestoreVolumeFromSnapshot ::
  -- | 'volumeId'
  Prelude.Text ->
  -- | 'snapshotId'
  Prelude.Text ->
  RestoreVolumeFromSnapshot
newRestoreVolumeFromSnapshot pVolumeId_ pSnapshotId_ =
  RestoreVolumeFromSnapshot'
    { clientRequestToken =
        Prelude.Nothing,
      options = Prelude.Nothing,
      volumeId = pVolumeId_,
      snapshotId = pSnapshotId_
    }

-- | Undocumented member.
restoreVolumeFromSnapshot_clientRequestToken :: Lens.Lens' RestoreVolumeFromSnapshot (Prelude.Maybe Prelude.Text)
restoreVolumeFromSnapshot_clientRequestToken = Lens.lens (\RestoreVolumeFromSnapshot' {clientRequestToken} -> clientRequestToken) (\s@RestoreVolumeFromSnapshot' {} a -> s {clientRequestToken = a} :: RestoreVolumeFromSnapshot)

-- | The settings used when restoring the specified volume from snapshot.
--
-- -   @DELETE_INTERMEDIATE_SNAPSHOTS@ - Deletes snapshots between the
--     current state and the specified snapshot. If there are intermediate
--     snapshots and this option isn\'t used, @RestoreVolumeFromSnapshot@
--     fails.
--
-- -   @DELETE_CLONED_VOLUMES@ - Deletes any dependent clone volumes
--     created from intermediate snapshots. If there are any dependent
--     clone volumes and this option isn\'t used,
--     @RestoreVolumeFromSnapshot@ fails.
restoreVolumeFromSnapshot_options :: Lens.Lens' RestoreVolumeFromSnapshot (Prelude.Maybe [RestoreOpenZFSVolumeOption])
restoreVolumeFromSnapshot_options = Lens.lens (\RestoreVolumeFromSnapshot' {options} -> options) (\s@RestoreVolumeFromSnapshot' {} a -> s {options = a} :: RestoreVolumeFromSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the volume that you are restoring.
restoreVolumeFromSnapshot_volumeId :: Lens.Lens' RestoreVolumeFromSnapshot Prelude.Text
restoreVolumeFromSnapshot_volumeId = Lens.lens (\RestoreVolumeFromSnapshot' {volumeId} -> volumeId) (\s@RestoreVolumeFromSnapshot' {} a -> s {volumeId = a} :: RestoreVolumeFromSnapshot)

-- | The ID of the source snapshot. Specifies the snapshot that you are
-- restoring from.
restoreVolumeFromSnapshot_snapshotId :: Lens.Lens' RestoreVolumeFromSnapshot Prelude.Text
restoreVolumeFromSnapshot_snapshotId = Lens.lens (\RestoreVolumeFromSnapshot' {snapshotId} -> snapshotId) (\s@RestoreVolumeFromSnapshot' {} a -> s {snapshotId = a} :: RestoreVolumeFromSnapshot)

instance Core.AWSRequest RestoreVolumeFromSnapshot where
  type
    AWSResponse RestoreVolumeFromSnapshot =
      RestoreVolumeFromSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreVolumeFromSnapshotResponse'
            Prelude.<$> ( x
                            Data..?> "AdministrativeActions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Lifecycle")
            Prelude.<*> (x Data..?> "VolumeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreVolumeFromSnapshot where
  hashWithSalt _salt RestoreVolumeFromSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` volumeId
      `Prelude.hashWithSalt` snapshotId

instance Prelude.NFData RestoreVolumeFromSnapshot where
  rnf RestoreVolumeFromSnapshot' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf volumeId
      `Prelude.seq` Prelude.rnf snapshotId

instance Data.ToHeaders RestoreVolumeFromSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.RestoreVolumeFromSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RestoreVolumeFromSnapshot where
  toJSON RestoreVolumeFromSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Options" Data..=) Prelude.<$> options,
            Prelude.Just ("VolumeId" Data..= volumeId),
            Prelude.Just ("SnapshotId" Data..= snapshotId)
          ]
      )

instance Data.ToPath RestoreVolumeFromSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreVolumeFromSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRestoreVolumeFromSnapshotResponse' smart constructor.
data RestoreVolumeFromSnapshotResponse = RestoreVolumeFromSnapshotResponse'
  { -- | A list of administrative actions for the file system that are in process
    -- or waiting to be processed. Administrative actions describe changes to
    -- the Amazon FSx system.
    administrativeActions :: Prelude.Maybe [AdministrativeAction],
    -- | The lifecycle state of the volume being restored.
    lifecycle :: Prelude.Maybe VolumeLifecycle,
    -- | The ID of the volume that you restored.
    volumeId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreVolumeFromSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'administrativeActions', 'restoreVolumeFromSnapshotResponse_administrativeActions' - A list of administrative actions for the file system that are in process
-- or waiting to be processed. Administrative actions describe changes to
-- the Amazon FSx system.
--
-- 'lifecycle', 'restoreVolumeFromSnapshotResponse_lifecycle' - The lifecycle state of the volume being restored.
--
-- 'volumeId', 'restoreVolumeFromSnapshotResponse_volumeId' - The ID of the volume that you restored.
--
-- 'httpStatus', 'restoreVolumeFromSnapshotResponse_httpStatus' - The response's http status code.
newRestoreVolumeFromSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreVolumeFromSnapshotResponse
newRestoreVolumeFromSnapshotResponse pHttpStatus_ =
  RestoreVolumeFromSnapshotResponse'
    { administrativeActions =
        Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      volumeId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of administrative actions for the file system that are in process
-- or waiting to be processed. Administrative actions describe changes to
-- the Amazon FSx system.
restoreVolumeFromSnapshotResponse_administrativeActions :: Lens.Lens' RestoreVolumeFromSnapshotResponse (Prelude.Maybe [AdministrativeAction])
restoreVolumeFromSnapshotResponse_administrativeActions = Lens.lens (\RestoreVolumeFromSnapshotResponse' {administrativeActions} -> administrativeActions) (\s@RestoreVolumeFromSnapshotResponse' {} a -> s {administrativeActions = a} :: RestoreVolumeFromSnapshotResponse) Prelude.. Lens.mapping Lens.coerced

-- | The lifecycle state of the volume being restored.
restoreVolumeFromSnapshotResponse_lifecycle :: Lens.Lens' RestoreVolumeFromSnapshotResponse (Prelude.Maybe VolumeLifecycle)
restoreVolumeFromSnapshotResponse_lifecycle = Lens.lens (\RestoreVolumeFromSnapshotResponse' {lifecycle} -> lifecycle) (\s@RestoreVolumeFromSnapshotResponse' {} a -> s {lifecycle = a} :: RestoreVolumeFromSnapshotResponse)

-- | The ID of the volume that you restored.
restoreVolumeFromSnapshotResponse_volumeId :: Lens.Lens' RestoreVolumeFromSnapshotResponse (Prelude.Maybe Prelude.Text)
restoreVolumeFromSnapshotResponse_volumeId = Lens.lens (\RestoreVolumeFromSnapshotResponse' {volumeId} -> volumeId) (\s@RestoreVolumeFromSnapshotResponse' {} a -> s {volumeId = a} :: RestoreVolumeFromSnapshotResponse)

-- | The response's http status code.
restoreVolumeFromSnapshotResponse_httpStatus :: Lens.Lens' RestoreVolumeFromSnapshotResponse Prelude.Int
restoreVolumeFromSnapshotResponse_httpStatus = Lens.lens (\RestoreVolumeFromSnapshotResponse' {httpStatus} -> httpStatus) (\s@RestoreVolumeFromSnapshotResponse' {} a -> s {httpStatus = a} :: RestoreVolumeFromSnapshotResponse)

instance
  Prelude.NFData
    RestoreVolumeFromSnapshotResponse
  where
  rnf RestoreVolumeFromSnapshotResponse' {..} =
    Prelude.rnf administrativeActions
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf volumeId
      `Prelude.seq` Prelude.rnf httpStatus
