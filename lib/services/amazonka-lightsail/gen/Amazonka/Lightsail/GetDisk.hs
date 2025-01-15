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
-- Module      : Amazonka.Lightsail.GetDisk
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific block storage disk.
module Amazonka.Lightsail.GetDisk
  ( -- * Creating a Request
    GetDisk (..),
    newGetDisk,

    -- * Request Lenses
    getDisk_diskName,

    -- * Destructuring the Response
    GetDiskResponse (..),
    newGetDiskResponse,

    -- * Response Lenses
    getDiskResponse_disk,
    getDiskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDisk' smart constructor.
data GetDisk = GetDisk'
  { -- | The name of the disk (e.g., @my-disk@).
    diskName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDisk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'diskName', 'getDisk_diskName' - The name of the disk (e.g., @my-disk@).
newGetDisk ::
  -- | 'diskName'
  Prelude.Text ->
  GetDisk
newGetDisk pDiskName_ =
  GetDisk' {diskName = pDiskName_}

-- | The name of the disk (e.g., @my-disk@).
getDisk_diskName :: Lens.Lens' GetDisk Prelude.Text
getDisk_diskName = Lens.lens (\GetDisk' {diskName} -> diskName) (\s@GetDisk' {} a -> s {diskName = a} :: GetDisk)

instance Core.AWSRequest GetDisk where
  type AWSResponse GetDisk = GetDiskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDiskResponse'
            Prelude.<$> (x Data..?> "disk")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDisk where
  hashWithSalt _salt GetDisk' {..} =
    _salt `Prelude.hashWithSalt` diskName

instance Prelude.NFData GetDisk where
  rnf GetDisk' {..} = Prelude.rnf diskName

instance Data.ToHeaders GetDisk where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("Lightsail_20161128.GetDisk" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDisk where
  toJSON GetDisk' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("diskName" Data..= diskName)]
      )

instance Data.ToPath GetDisk where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDisk where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDiskResponse' smart constructor.
data GetDiskResponse = GetDiskResponse'
  { -- | An object containing information about the disk.
    disk :: Prelude.Maybe Disk,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDiskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disk', 'getDiskResponse_disk' - An object containing information about the disk.
--
-- 'httpStatus', 'getDiskResponse_httpStatus' - The response's http status code.
newGetDiskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDiskResponse
newGetDiskResponse pHttpStatus_ =
  GetDiskResponse'
    { disk = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object containing information about the disk.
getDiskResponse_disk :: Lens.Lens' GetDiskResponse (Prelude.Maybe Disk)
getDiskResponse_disk = Lens.lens (\GetDiskResponse' {disk} -> disk) (\s@GetDiskResponse' {} a -> s {disk = a} :: GetDiskResponse)

-- | The response's http status code.
getDiskResponse_httpStatus :: Lens.Lens' GetDiskResponse Prelude.Int
getDiskResponse_httpStatus = Lens.lens (\GetDiskResponse' {httpStatus} -> httpStatus) (\s@GetDiskResponse' {} a -> s {httpStatus = a} :: GetDiskResponse)

instance Prelude.NFData GetDiskResponse where
  rnf GetDiskResponse' {..} =
    Prelude.rnf disk `Prelude.seq`
      Prelude.rnf httpStatus
