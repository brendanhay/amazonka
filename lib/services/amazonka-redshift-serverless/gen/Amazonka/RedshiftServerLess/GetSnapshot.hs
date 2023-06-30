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
-- Module      : Amazonka.RedshiftServerLess.GetSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific snapshot.
module Amazonka.RedshiftServerLess.GetSnapshot
  ( -- * Creating a Request
    GetSnapshot (..),
    newGetSnapshot,

    -- * Request Lenses
    getSnapshot_ownerAccount,
    getSnapshot_snapshotArn,
    getSnapshot_snapshotName,

    -- * Destructuring the Response
    GetSnapshotResponse (..),
    newGetSnapshotResponse,

    -- * Response Lenses
    getSnapshotResponse_snapshot,
    getSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSnapshot' smart constructor.
data GetSnapshot = GetSnapshot'
  { -- | The owner Amazon Web Services account of a snapshot shared with another
    -- user.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the snapshot to return.
    snapshotArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the snapshot to return.
    snapshotName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerAccount', 'getSnapshot_ownerAccount' - The owner Amazon Web Services account of a snapshot shared with another
-- user.
--
-- 'snapshotArn', 'getSnapshot_snapshotArn' - The Amazon Resource Name (ARN) of the snapshot to return.
--
-- 'snapshotName', 'getSnapshot_snapshotName' - The name of the snapshot to return.
newGetSnapshot ::
  GetSnapshot
newGetSnapshot =
  GetSnapshot'
    { ownerAccount = Prelude.Nothing,
      snapshotArn = Prelude.Nothing,
      snapshotName = Prelude.Nothing
    }

-- | The owner Amazon Web Services account of a snapshot shared with another
-- user.
getSnapshot_ownerAccount :: Lens.Lens' GetSnapshot (Prelude.Maybe Prelude.Text)
getSnapshot_ownerAccount = Lens.lens (\GetSnapshot' {ownerAccount} -> ownerAccount) (\s@GetSnapshot' {} a -> s {ownerAccount = a} :: GetSnapshot)

-- | The Amazon Resource Name (ARN) of the snapshot to return.
getSnapshot_snapshotArn :: Lens.Lens' GetSnapshot (Prelude.Maybe Prelude.Text)
getSnapshot_snapshotArn = Lens.lens (\GetSnapshot' {snapshotArn} -> snapshotArn) (\s@GetSnapshot' {} a -> s {snapshotArn = a} :: GetSnapshot)

-- | The name of the snapshot to return.
getSnapshot_snapshotName :: Lens.Lens' GetSnapshot (Prelude.Maybe Prelude.Text)
getSnapshot_snapshotName = Lens.lens (\GetSnapshot' {snapshotName} -> snapshotName) (\s@GetSnapshot' {} a -> s {snapshotName = a} :: GetSnapshot)

instance Core.AWSRequest GetSnapshot where
  type AWSResponse GetSnapshot = GetSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSnapshotResponse'
            Prelude.<$> (x Data..?> "snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSnapshot where
  hashWithSalt _salt GetSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` ownerAccount
      `Prelude.hashWithSalt` snapshotArn
      `Prelude.hashWithSalt` snapshotName

instance Prelude.NFData GetSnapshot where
  rnf GetSnapshot' {..} =
    Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf snapshotArn
      `Prelude.seq` Prelude.rnf snapshotName

instance Data.ToHeaders GetSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.GetSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSnapshot where
  toJSON GetSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ownerAccount" Data..=) Prelude.<$> ownerAccount,
            ("snapshotArn" Data..=) Prelude.<$> snapshotArn,
            ("snapshotName" Data..=) Prelude.<$> snapshotName
          ]
      )

instance Data.ToPath GetSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSnapshotResponse' smart constructor.
data GetSnapshotResponse = GetSnapshotResponse'
  { -- | The returned snapshot object.
    snapshot :: Prelude.Maybe Snapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshot', 'getSnapshotResponse_snapshot' - The returned snapshot object.
--
-- 'httpStatus', 'getSnapshotResponse_httpStatus' - The response's http status code.
newGetSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSnapshotResponse
newGetSnapshotResponse pHttpStatus_ =
  GetSnapshotResponse'
    { snapshot = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The returned snapshot object.
getSnapshotResponse_snapshot :: Lens.Lens' GetSnapshotResponse (Prelude.Maybe Snapshot)
getSnapshotResponse_snapshot = Lens.lens (\GetSnapshotResponse' {snapshot} -> snapshot) (\s@GetSnapshotResponse' {} a -> s {snapshot = a} :: GetSnapshotResponse)

-- | The response's http status code.
getSnapshotResponse_httpStatus :: Lens.Lens' GetSnapshotResponse Prelude.Int
getSnapshotResponse_httpStatus = Lens.lens (\GetSnapshotResponse' {httpStatus} -> httpStatus) (\s@GetSnapshotResponse' {} a -> s {httpStatus = a} :: GetSnapshotResponse)

instance Prelude.NFData GetSnapshotResponse where
  rnf GetSnapshotResponse' {..} =
    Prelude.rnf snapshot
      `Prelude.seq` Prelude.rnf httpStatus
