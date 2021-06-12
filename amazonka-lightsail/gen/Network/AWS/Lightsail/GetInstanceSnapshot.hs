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
-- Module      : Network.AWS.Lightsail.GetInstanceSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific instance snapshot.
module Network.AWS.Lightsail.GetInstanceSnapshot
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetInstanceSnapshot' smart constructor.
data GetInstanceSnapshot = GetInstanceSnapshot'
  { -- | The name of the snapshot for which you are requesting information.
    instanceSnapshotName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetInstanceSnapshot
newGetInstanceSnapshot pInstanceSnapshotName_ =
  GetInstanceSnapshot'
    { instanceSnapshotName =
        pInstanceSnapshotName_
    }

-- | The name of the snapshot for which you are requesting information.
getInstanceSnapshot_instanceSnapshotName :: Lens.Lens' GetInstanceSnapshot Core.Text
getInstanceSnapshot_instanceSnapshotName = Lens.lens (\GetInstanceSnapshot' {instanceSnapshotName} -> instanceSnapshotName) (\s@GetInstanceSnapshot' {} a -> s {instanceSnapshotName = a} :: GetInstanceSnapshot)

instance Core.AWSRequest GetInstanceSnapshot where
  type
    AWSResponse GetInstanceSnapshot =
      GetInstanceSnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceSnapshotResponse'
            Core.<$> (x Core..?> "instanceSnapshot")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetInstanceSnapshot

instance Core.NFData GetInstanceSnapshot

instance Core.ToHeaders GetInstanceSnapshot where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetInstanceSnapshot" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetInstanceSnapshot where
  toJSON GetInstanceSnapshot' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "instanceSnapshotName"
                  Core..= instanceSnapshotName
              )
          ]
      )

instance Core.ToPath GetInstanceSnapshot where
  toPath = Core.const "/"

instance Core.ToQuery GetInstanceSnapshot where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetInstanceSnapshotResponse' smart constructor.
data GetInstanceSnapshotResponse = GetInstanceSnapshotResponse'
  { -- | An array of key-value pairs containing information about the results of
    -- your get instance snapshot request.
    instanceSnapshot :: Core.Maybe InstanceSnapshot,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetInstanceSnapshotResponse
newGetInstanceSnapshotResponse pHttpStatus_ =
  GetInstanceSnapshotResponse'
    { instanceSnapshot =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about the results of
-- your get instance snapshot request.
getInstanceSnapshotResponse_instanceSnapshot :: Lens.Lens' GetInstanceSnapshotResponse (Core.Maybe InstanceSnapshot)
getInstanceSnapshotResponse_instanceSnapshot = Lens.lens (\GetInstanceSnapshotResponse' {instanceSnapshot} -> instanceSnapshot) (\s@GetInstanceSnapshotResponse' {} a -> s {instanceSnapshot = a} :: GetInstanceSnapshotResponse)

-- | The response's http status code.
getInstanceSnapshotResponse_httpStatus :: Lens.Lens' GetInstanceSnapshotResponse Core.Int
getInstanceSnapshotResponse_httpStatus = Lens.lens (\GetInstanceSnapshotResponse' {httpStatus} -> httpStatus) (\s@GetInstanceSnapshotResponse' {} a -> s {httpStatus = a} :: GetInstanceSnapshotResponse)

instance Core.NFData GetInstanceSnapshotResponse
