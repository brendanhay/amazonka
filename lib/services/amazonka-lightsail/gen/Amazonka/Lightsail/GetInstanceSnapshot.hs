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
-- Module      : Amazonka.Lightsail.GetInstanceSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific instance snapshot.
module Amazonka.Lightsail.GetInstanceSnapshot
  ( -- * Creating a Request
    GetInstanceSnapshot (..),
    newGetInstanceSnapshot,

    -- * Request Lenses
    getInstanceSnapshot_instanceSnapshotName,

    -- * Destructuring the Response
    GetInstanceSnapshotResponse (..),
    newGetInstanceSnapshotResponse,

    -- * Response Lenses
    getInstanceSnapshotResponse_instanceSnapshot,
    getInstanceSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetInstanceSnapshot' smart constructor.
data GetInstanceSnapshot = GetInstanceSnapshot'
  { -- | The name of the snapshot for which you are requesting information.
    instanceSnapshotName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstanceSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceSnapshotName', 'getInstanceSnapshot_instanceSnapshotName' - The name of the snapshot for which you are requesting information.
newGetInstanceSnapshot ::
  -- | 'instanceSnapshotName'
  Prelude.Text ->
  GetInstanceSnapshot
newGetInstanceSnapshot pInstanceSnapshotName_ =
  GetInstanceSnapshot'
    { instanceSnapshotName =
        pInstanceSnapshotName_
    }

-- | The name of the snapshot for which you are requesting information.
getInstanceSnapshot_instanceSnapshotName :: Lens.Lens' GetInstanceSnapshot Prelude.Text
getInstanceSnapshot_instanceSnapshotName = Lens.lens (\GetInstanceSnapshot' {instanceSnapshotName} -> instanceSnapshotName) (\s@GetInstanceSnapshot' {} a -> s {instanceSnapshotName = a} :: GetInstanceSnapshot)

instance Core.AWSRequest GetInstanceSnapshot where
  type
    AWSResponse GetInstanceSnapshot =
      GetInstanceSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceSnapshotResponse'
            Prelude.<$> (x Data..?> "instanceSnapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInstanceSnapshot where
  hashWithSalt _salt GetInstanceSnapshot' {..} =
    _salt `Prelude.hashWithSalt` instanceSnapshotName

instance Prelude.NFData GetInstanceSnapshot where
  rnf GetInstanceSnapshot' {..} =
    Prelude.rnf instanceSnapshotName

instance Data.ToHeaders GetInstanceSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetInstanceSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetInstanceSnapshot where
  toJSON GetInstanceSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "instanceSnapshotName"
                  Data..= instanceSnapshotName
              )
          ]
      )

instance Data.ToPath GetInstanceSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery GetInstanceSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInstanceSnapshotResponse' smart constructor.
data GetInstanceSnapshotResponse = GetInstanceSnapshotResponse'
  { -- | An array of key-value pairs containing information about the results of
    -- your get instance snapshot request.
    instanceSnapshot :: Prelude.Maybe InstanceSnapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstanceSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceSnapshot', 'getInstanceSnapshotResponse_instanceSnapshot' - An array of key-value pairs containing information about the results of
-- your get instance snapshot request.
--
-- 'httpStatus', 'getInstanceSnapshotResponse_httpStatus' - The response's http status code.
newGetInstanceSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInstanceSnapshotResponse
newGetInstanceSnapshotResponse pHttpStatus_ =
  GetInstanceSnapshotResponse'
    { instanceSnapshot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about the results of
-- your get instance snapshot request.
getInstanceSnapshotResponse_instanceSnapshot :: Lens.Lens' GetInstanceSnapshotResponse (Prelude.Maybe InstanceSnapshot)
getInstanceSnapshotResponse_instanceSnapshot = Lens.lens (\GetInstanceSnapshotResponse' {instanceSnapshot} -> instanceSnapshot) (\s@GetInstanceSnapshotResponse' {} a -> s {instanceSnapshot = a} :: GetInstanceSnapshotResponse)

-- | The response's http status code.
getInstanceSnapshotResponse_httpStatus :: Lens.Lens' GetInstanceSnapshotResponse Prelude.Int
getInstanceSnapshotResponse_httpStatus = Lens.lens (\GetInstanceSnapshotResponse' {httpStatus} -> httpStatus) (\s@GetInstanceSnapshotResponse' {} a -> s {httpStatus = a} :: GetInstanceSnapshotResponse)

instance Prelude.NFData GetInstanceSnapshotResponse where
  rnf GetInstanceSnapshotResponse' {..} =
    Prelude.rnf instanceSnapshot
      `Prelude.seq` Prelude.rnf httpStatus
