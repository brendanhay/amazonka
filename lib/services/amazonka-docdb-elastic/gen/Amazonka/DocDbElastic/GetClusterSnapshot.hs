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
-- Module      : Amazonka.DocDbElastic.GetClusterSnapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific Elastic DocumentDB snapshot
module Amazonka.DocDbElastic.GetClusterSnapshot
  ( -- * Creating a Request
    GetClusterSnapshot (..),
    newGetClusterSnapshot,

    -- * Request Lenses
    getClusterSnapshot_snapshotArn,

    -- * Destructuring the Response
    GetClusterSnapshotResponse (..),
    newGetClusterSnapshotResponse,

    -- * Response Lenses
    getClusterSnapshotResponse_httpStatus,
    getClusterSnapshotResponse_snapshot,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocDbElastic.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetClusterSnapshot' smart constructor.
data GetClusterSnapshot = GetClusterSnapshot'
  { -- | The arn of the Elastic DocumentDB snapshot.
    snapshotArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClusterSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotArn', 'getClusterSnapshot_snapshotArn' - The arn of the Elastic DocumentDB snapshot.
newGetClusterSnapshot ::
  -- | 'snapshotArn'
  Prelude.Text ->
  GetClusterSnapshot
newGetClusterSnapshot pSnapshotArn_ =
  GetClusterSnapshot' {snapshotArn = pSnapshotArn_}

-- | The arn of the Elastic DocumentDB snapshot.
getClusterSnapshot_snapshotArn :: Lens.Lens' GetClusterSnapshot Prelude.Text
getClusterSnapshot_snapshotArn = Lens.lens (\GetClusterSnapshot' {snapshotArn} -> snapshotArn) (\s@GetClusterSnapshot' {} a -> s {snapshotArn = a} :: GetClusterSnapshot)

instance Core.AWSRequest GetClusterSnapshot where
  type
    AWSResponse GetClusterSnapshot =
      GetClusterSnapshotResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetClusterSnapshotResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "snapshot")
      )

instance Prelude.Hashable GetClusterSnapshot where
  hashWithSalt _salt GetClusterSnapshot' {..} =
    _salt `Prelude.hashWithSalt` snapshotArn

instance Prelude.NFData GetClusterSnapshot where
  rnf GetClusterSnapshot' {..} = Prelude.rnf snapshotArn

instance Data.ToHeaders GetClusterSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetClusterSnapshot where
  toPath GetClusterSnapshot' {..} =
    Prelude.mconcat
      ["/cluster-snapshot/", Data.toBS snapshotArn]

instance Data.ToQuery GetClusterSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetClusterSnapshotResponse' smart constructor.
data GetClusterSnapshotResponse = GetClusterSnapshotResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns information about a specific Elastic DocumentDB snapshot.
    snapshot :: ClusterSnapshot
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClusterSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getClusterSnapshotResponse_httpStatus' - The response's http status code.
--
-- 'snapshot', 'getClusterSnapshotResponse_snapshot' - Returns information about a specific Elastic DocumentDB snapshot.
newGetClusterSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'snapshot'
  ClusterSnapshot ->
  GetClusterSnapshotResponse
newGetClusterSnapshotResponse pHttpStatus_ pSnapshot_ =
  GetClusterSnapshotResponse'
    { httpStatus =
        pHttpStatus_,
      snapshot = pSnapshot_
    }

-- | The response's http status code.
getClusterSnapshotResponse_httpStatus :: Lens.Lens' GetClusterSnapshotResponse Prelude.Int
getClusterSnapshotResponse_httpStatus = Lens.lens (\GetClusterSnapshotResponse' {httpStatus} -> httpStatus) (\s@GetClusterSnapshotResponse' {} a -> s {httpStatus = a} :: GetClusterSnapshotResponse)

-- | Returns information about a specific Elastic DocumentDB snapshot.
getClusterSnapshotResponse_snapshot :: Lens.Lens' GetClusterSnapshotResponse ClusterSnapshot
getClusterSnapshotResponse_snapshot = Lens.lens (\GetClusterSnapshotResponse' {snapshot} -> snapshot) (\s@GetClusterSnapshotResponse' {} a -> s {snapshot = a} :: GetClusterSnapshotResponse)

instance Prelude.NFData GetClusterSnapshotResponse where
  rnf GetClusterSnapshotResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf snapshot
