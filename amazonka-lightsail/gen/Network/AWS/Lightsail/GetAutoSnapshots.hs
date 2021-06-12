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
-- Module      : Network.AWS.Lightsail.GetAutoSnapshots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the available automatic snapshots for an instance or disk. For
-- more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
module Network.AWS.Lightsail.GetAutoSnapshots
  ( -- * Creating a Request
    GetAutoSnapshots (..),
    newGetAutoSnapshots,

    -- * Request Lenses
    getAutoSnapshots_resourceName,

    -- * Destructuring the Response
    GetAutoSnapshotsResponse (..),
    newGetAutoSnapshotsResponse,

    -- * Response Lenses
    getAutoSnapshotsResponse_resourceType,
    getAutoSnapshotsResponse_autoSnapshots,
    getAutoSnapshotsResponse_resourceName,
    getAutoSnapshotsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAutoSnapshots' smart constructor.
data GetAutoSnapshots = GetAutoSnapshots'
  { -- | The name of the source instance or disk from which to get automatic
    -- snapshot information.
    resourceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAutoSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'getAutoSnapshots_resourceName' - The name of the source instance or disk from which to get automatic
-- snapshot information.
newGetAutoSnapshots ::
  -- | 'resourceName'
  Core.Text ->
  GetAutoSnapshots
newGetAutoSnapshots pResourceName_ =
  GetAutoSnapshots' {resourceName = pResourceName_}

-- | The name of the source instance or disk from which to get automatic
-- snapshot information.
getAutoSnapshots_resourceName :: Lens.Lens' GetAutoSnapshots Core.Text
getAutoSnapshots_resourceName = Lens.lens (\GetAutoSnapshots' {resourceName} -> resourceName) (\s@GetAutoSnapshots' {} a -> s {resourceName = a} :: GetAutoSnapshots)

instance Core.AWSRequest GetAutoSnapshots where
  type
    AWSResponse GetAutoSnapshots =
      GetAutoSnapshotsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAutoSnapshotsResponse'
            Core.<$> (x Core..?> "resourceType")
            Core.<*> (x Core..?> "autoSnapshots" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "resourceName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAutoSnapshots

instance Core.NFData GetAutoSnapshots

instance Core.ToHeaders GetAutoSnapshots where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetAutoSnapshots" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetAutoSnapshots where
  toJSON GetAutoSnapshots' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("resourceName" Core..= resourceName)]
      )

instance Core.ToPath GetAutoSnapshots where
  toPath = Core.const "/"

instance Core.ToQuery GetAutoSnapshots where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAutoSnapshotsResponse' smart constructor.
data GetAutoSnapshotsResponse = GetAutoSnapshotsResponse'
  { -- | The resource type (e.g., @Instance@ or @Disk@).
    resourceType :: Core.Maybe ResourceType,
    -- | An array of objects that describe the automatic snapshots that are
    -- available for the specified source instance or disk.
    autoSnapshots :: Core.Maybe [AutoSnapshotDetails],
    -- | The name of the source instance or disk for the automatic snapshots.
    resourceName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAutoSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'getAutoSnapshotsResponse_resourceType' - The resource type (e.g., @Instance@ or @Disk@).
--
-- 'autoSnapshots', 'getAutoSnapshotsResponse_autoSnapshots' - An array of objects that describe the automatic snapshots that are
-- available for the specified source instance or disk.
--
-- 'resourceName', 'getAutoSnapshotsResponse_resourceName' - The name of the source instance or disk for the automatic snapshots.
--
-- 'httpStatus', 'getAutoSnapshotsResponse_httpStatus' - The response's http status code.
newGetAutoSnapshotsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAutoSnapshotsResponse
newGetAutoSnapshotsResponse pHttpStatus_ =
  GetAutoSnapshotsResponse'
    { resourceType =
        Core.Nothing,
      autoSnapshots = Core.Nothing,
      resourceName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource type (e.g., @Instance@ or @Disk@).
getAutoSnapshotsResponse_resourceType :: Lens.Lens' GetAutoSnapshotsResponse (Core.Maybe ResourceType)
getAutoSnapshotsResponse_resourceType = Lens.lens (\GetAutoSnapshotsResponse' {resourceType} -> resourceType) (\s@GetAutoSnapshotsResponse' {} a -> s {resourceType = a} :: GetAutoSnapshotsResponse)

-- | An array of objects that describe the automatic snapshots that are
-- available for the specified source instance or disk.
getAutoSnapshotsResponse_autoSnapshots :: Lens.Lens' GetAutoSnapshotsResponse (Core.Maybe [AutoSnapshotDetails])
getAutoSnapshotsResponse_autoSnapshots = Lens.lens (\GetAutoSnapshotsResponse' {autoSnapshots} -> autoSnapshots) (\s@GetAutoSnapshotsResponse' {} a -> s {autoSnapshots = a} :: GetAutoSnapshotsResponse) Core.. Lens.mapping Lens._Coerce

-- | The name of the source instance or disk for the automatic snapshots.
getAutoSnapshotsResponse_resourceName :: Lens.Lens' GetAutoSnapshotsResponse (Core.Maybe Core.Text)
getAutoSnapshotsResponse_resourceName = Lens.lens (\GetAutoSnapshotsResponse' {resourceName} -> resourceName) (\s@GetAutoSnapshotsResponse' {} a -> s {resourceName = a} :: GetAutoSnapshotsResponse)

-- | The response's http status code.
getAutoSnapshotsResponse_httpStatus :: Lens.Lens' GetAutoSnapshotsResponse Core.Int
getAutoSnapshotsResponse_httpStatus = Lens.lens (\GetAutoSnapshotsResponse' {httpStatus} -> httpStatus) (\s@GetAutoSnapshotsResponse' {} a -> s {httpStatus = a} :: GetAutoSnapshotsResponse)

instance Core.NFData GetAutoSnapshotsResponse
