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
-- Module      : Amazonka.FSx.UpdateSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name of an Amazon FSx for OpenZFS snapshot.
module Amazonka.FSx.UpdateSnapshot
  ( -- * Creating a Request
    UpdateSnapshot (..),
    newUpdateSnapshot,

    -- * Request Lenses
    updateSnapshot_clientRequestToken,
    updateSnapshot_name,
    updateSnapshot_snapshotId,

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
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSnapshot' smart constructor.
data UpdateSnapshot = UpdateSnapshot'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the snapshot to update.
    name :: Prelude.Text,
    -- | The ID of the snapshot that you want to update, in the format
    -- @fsvolsnap-0123456789abcdef0@.
    snapshotId :: Prelude.Text
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
-- 'clientRequestToken', 'updateSnapshot_clientRequestToken' - Undocumented member.
--
-- 'name', 'updateSnapshot_name' - The name of the snapshot to update.
--
-- 'snapshotId', 'updateSnapshot_snapshotId' - The ID of the snapshot that you want to update, in the format
-- @fsvolsnap-0123456789abcdef0@.
newUpdateSnapshot ::
  -- | 'name'
  Prelude.Text ->
  -- | 'snapshotId'
  Prelude.Text ->
  UpdateSnapshot
newUpdateSnapshot pName_ pSnapshotId_ =
  UpdateSnapshot'
    { clientRequestToken =
        Prelude.Nothing,
      name = pName_,
      snapshotId = pSnapshotId_
    }

-- | Undocumented member.
updateSnapshot_clientRequestToken :: Lens.Lens' UpdateSnapshot (Prelude.Maybe Prelude.Text)
updateSnapshot_clientRequestToken = Lens.lens (\UpdateSnapshot' {clientRequestToken} -> clientRequestToken) (\s@UpdateSnapshot' {} a -> s {clientRequestToken = a} :: UpdateSnapshot)

-- | The name of the snapshot to update.
updateSnapshot_name :: Lens.Lens' UpdateSnapshot Prelude.Text
updateSnapshot_name = Lens.lens (\UpdateSnapshot' {name} -> name) (\s@UpdateSnapshot' {} a -> s {name = a} :: UpdateSnapshot)

-- | The ID of the snapshot that you want to update, in the format
-- @fsvolsnap-0123456789abcdef0@.
updateSnapshot_snapshotId :: Lens.Lens' UpdateSnapshot Prelude.Text
updateSnapshot_snapshotId = Lens.lens (\UpdateSnapshot' {snapshotId} -> snapshotId) (\s@UpdateSnapshot' {} a -> s {snapshotId = a} :: UpdateSnapshot)

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
            Prelude.<$> (x Data..?> "Snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSnapshot where
  hashWithSalt _salt UpdateSnapshot' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` snapshotId

instance Prelude.NFData UpdateSnapshot where
  rnf UpdateSnapshot' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf snapshotId

instance Data.ToHeaders UpdateSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.UpdateSnapshot" ::
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
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("SnapshotId" Data..= snapshotId)
          ]
      )

instance Data.ToPath UpdateSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSnapshotResponse' smart constructor.
data UpdateSnapshotResponse = UpdateSnapshotResponse'
  { -- | Returned after a successful @UpdateSnapshot@ operation, describing the
    -- snapshot that you updated.
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
-- 'snapshot', 'updateSnapshotResponse_snapshot' - Returned after a successful @UpdateSnapshot@ operation, describing the
-- snapshot that you updated.
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

-- | Returned after a successful @UpdateSnapshot@ operation, describing the
-- snapshot that you updated.
updateSnapshotResponse_snapshot :: Lens.Lens' UpdateSnapshotResponse (Prelude.Maybe Snapshot)
updateSnapshotResponse_snapshot = Lens.lens (\UpdateSnapshotResponse' {snapshot} -> snapshot) (\s@UpdateSnapshotResponse' {} a -> s {snapshot = a} :: UpdateSnapshotResponse)

-- | The response's http status code.
updateSnapshotResponse_httpStatus :: Lens.Lens' UpdateSnapshotResponse Prelude.Int
updateSnapshotResponse_httpStatus = Lens.lens (\UpdateSnapshotResponse' {httpStatus} -> httpStatus) (\s@UpdateSnapshotResponse' {} a -> s {httpStatus = a} :: UpdateSnapshotResponse)

instance Prelude.NFData UpdateSnapshotResponse where
  rnf UpdateSnapshotResponse' {..} =
    Prelude.rnf snapshot
      `Prelude.seq` Prelude.rnf httpStatus
