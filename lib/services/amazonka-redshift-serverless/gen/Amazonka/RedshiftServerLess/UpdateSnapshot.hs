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
-- Module      : Amazonka.RedshiftServerLess.UpdateSnapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a snapshot.
module Amazonka.RedshiftServerLess.UpdateSnapshot
  ( -- * Creating a Request
    UpdateSnapshot (..),
    newUpdateSnapshot,

    -- * Request Lenses
    updateSnapshot_retentionPeriod,
    updateSnapshot_snapshotName,

    -- * Destructuring the Response
    UpdateSnapshotResponse (..),
    newUpdateSnapshotResponse,

    -- * Response Lenses
    updateSnapshotResponse_snapshot,
    updateSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSnapshot' smart constructor.
data UpdateSnapshot = UpdateSnapshot'
  { -- | The new retention period of the snapshot.
    retentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The name of the snapshot.
    snapshotName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retentionPeriod', 'updateSnapshot_retentionPeriod' - The new retention period of the snapshot.
--
-- 'snapshotName', 'updateSnapshot_snapshotName' - The name of the snapshot.
newUpdateSnapshot ::
  -- | 'snapshotName'
  Prelude.Text ->
  UpdateSnapshot
newUpdateSnapshot pSnapshotName_ =
  UpdateSnapshot'
    { retentionPeriod = Prelude.Nothing,
      snapshotName = pSnapshotName_
    }

-- | The new retention period of the snapshot.
updateSnapshot_retentionPeriod :: Lens.Lens' UpdateSnapshot (Prelude.Maybe Prelude.Int)
updateSnapshot_retentionPeriod = Lens.lens (\UpdateSnapshot' {retentionPeriod} -> retentionPeriod) (\s@UpdateSnapshot' {} a -> s {retentionPeriod = a} :: UpdateSnapshot)

-- | The name of the snapshot.
updateSnapshot_snapshotName :: Lens.Lens' UpdateSnapshot Prelude.Text
updateSnapshot_snapshotName = Lens.lens (\UpdateSnapshot' {snapshotName} -> snapshotName) (\s@UpdateSnapshot' {} a -> s {snapshotName = a} :: UpdateSnapshot)

instance Core.AWSRequest UpdateSnapshot where
  type
    AWSResponse UpdateSnapshot =
      UpdateSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSnapshotResponse'
            Prelude.<$> (x Data..?> "snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSnapshot where
  hashWithSalt _salt UpdateSnapshot' {..} =
    _salt `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` snapshotName

instance Prelude.NFData UpdateSnapshot where
  rnf UpdateSnapshot' {..} =
    Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf snapshotName

instance Data.ToHeaders UpdateSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.UpdateSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSnapshot where
  toJSON UpdateSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("retentionPeriod" Data..=)
              Prelude.<$> retentionPeriod,
            Prelude.Just ("snapshotName" Data..= snapshotName)
          ]
      )

instance Data.ToPath UpdateSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSnapshotResponse' smart constructor.
data UpdateSnapshotResponse = UpdateSnapshotResponse'
  { -- | The updated snapshot object.
    snapshot :: Prelude.Maybe Snapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshot', 'updateSnapshotResponse_snapshot' - The updated snapshot object.
--
-- 'httpStatus', 'updateSnapshotResponse_httpStatus' - The response's http status code.
newUpdateSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSnapshotResponse
newUpdateSnapshotResponse pHttpStatus_ =
  UpdateSnapshotResponse'
    { snapshot = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated snapshot object.
updateSnapshotResponse_snapshot :: Lens.Lens' UpdateSnapshotResponse (Prelude.Maybe Snapshot)
updateSnapshotResponse_snapshot = Lens.lens (\UpdateSnapshotResponse' {snapshot} -> snapshot) (\s@UpdateSnapshotResponse' {} a -> s {snapshot = a} :: UpdateSnapshotResponse)

-- | The response's http status code.
updateSnapshotResponse_httpStatus :: Lens.Lens' UpdateSnapshotResponse Prelude.Int
updateSnapshotResponse_httpStatus = Lens.lens (\UpdateSnapshotResponse' {httpStatus} -> httpStatus) (\s@UpdateSnapshotResponse' {} a -> s {httpStatus = a} :: UpdateSnapshotResponse)

instance Prelude.NFData UpdateSnapshotResponse where
  rnf UpdateSnapshotResponse' {..} =
    Prelude.rnf snapshot
      `Prelude.seq` Prelude.rnf httpStatus
