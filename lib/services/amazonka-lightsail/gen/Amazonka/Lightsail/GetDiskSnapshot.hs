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
-- Module      : Amazonka.Lightsail.GetDiskSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific block storage disk snapshot.
module Amazonka.Lightsail.GetDiskSnapshot
  ( -- * Creating a Request
    GetDiskSnapshot (..),
    newGetDiskSnapshot,

    -- * Request Lenses
    getDiskSnapshot_diskSnapshotName,

    -- * Destructuring the Response
    GetDiskSnapshotResponse (..),
    newGetDiskSnapshotResponse,

    -- * Response Lenses
    getDiskSnapshotResponse_diskSnapshot,
    getDiskSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDiskSnapshot' smart constructor.
data GetDiskSnapshot = GetDiskSnapshot'
  { -- | The name of the disk snapshot (e.g., @my-disk-snapshot@).
    diskSnapshotName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDiskSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'diskSnapshotName', 'getDiskSnapshot_diskSnapshotName' - The name of the disk snapshot (e.g., @my-disk-snapshot@).
newGetDiskSnapshot ::
  -- | 'diskSnapshotName'
  Prelude.Text ->
  GetDiskSnapshot
newGetDiskSnapshot pDiskSnapshotName_ =
  GetDiskSnapshot'
    { diskSnapshotName =
        pDiskSnapshotName_
    }

-- | The name of the disk snapshot (e.g., @my-disk-snapshot@).
getDiskSnapshot_diskSnapshotName :: Lens.Lens' GetDiskSnapshot Prelude.Text
getDiskSnapshot_diskSnapshotName = Lens.lens (\GetDiskSnapshot' {diskSnapshotName} -> diskSnapshotName) (\s@GetDiskSnapshot' {} a -> s {diskSnapshotName = a} :: GetDiskSnapshot)

instance Core.AWSRequest GetDiskSnapshot where
  type
    AWSResponse GetDiskSnapshot =
      GetDiskSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDiskSnapshotResponse'
            Prelude.<$> (x Data..?> "diskSnapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDiskSnapshot where
  hashWithSalt _salt GetDiskSnapshot' {..} =
    _salt `Prelude.hashWithSalt` diskSnapshotName

instance Prelude.NFData GetDiskSnapshot where
  rnf GetDiskSnapshot' {..} =
    Prelude.rnf diskSnapshotName

instance Data.ToHeaders GetDiskSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetDiskSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDiskSnapshot where
  toJSON GetDiskSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("diskSnapshotName" Data..= diskSnapshotName)
          ]
      )

instance Data.ToPath GetDiskSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDiskSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDiskSnapshotResponse' smart constructor.
data GetDiskSnapshotResponse = GetDiskSnapshotResponse'
  { -- | An object containing information about the disk snapshot.
    diskSnapshot :: Prelude.Maybe DiskSnapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDiskSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'diskSnapshot', 'getDiskSnapshotResponse_diskSnapshot' - An object containing information about the disk snapshot.
--
-- 'httpStatus', 'getDiskSnapshotResponse_httpStatus' - The response's http status code.
newGetDiskSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDiskSnapshotResponse
newGetDiskSnapshotResponse pHttpStatus_ =
  GetDiskSnapshotResponse'
    { diskSnapshot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object containing information about the disk snapshot.
getDiskSnapshotResponse_diskSnapshot :: Lens.Lens' GetDiskSnapshotResponse (Prelude.Maybe DiskSnapshot)
getDiskSnapshotResponse_diskSnapshot = Lens.lens (\GetDiskSnapshotResponse' {diskSnapshot} -> diskSnapshot) (\s@GetDiskSnapshotResponse' {} a -> s {diskSnapshot = a} :: GetDiskSnapshotResponse)

-- | The response's http status code.
getDiskSnapshotResponse_httpStatus :: Lens.Lens' GetDiskSnapshotResponse Prelude.Int
getDiskSnapshotResponse_httpStatus = Lens.lens (\GetDiskSnapshotResponse' {httpStatus} -> httpStatus) (\s@GetDiskSnapshotResponse' {} a -> s {httpStatus = a} :: GetDiskSnapshotResponse)

instance Prelude.NFData GetDiskSnapshotResponse where
  rnf GetDiskSnapshotResponse' {..} =
    Prelude.rnf diskSnapshot `Prelude.seq`
      Prelude.rnf httpStatus
