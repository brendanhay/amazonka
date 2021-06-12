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
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceSnapshots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the snapshots for the specified WorkSpace.
module Network.AWS.WorkSpaces.DescribeWorkspaceSnapshots
  ( -- * Creating a Request
    DescribeWorkspaceSnapshots (..),
    newDescribeWorkspaceSnapshots,

    -- * Request Lenses
    describeWorkspaceSnapshots_workspaceId,

    -- * Destructuring the Response
    DescribeWorkspaceSnapshotsResponse (..),
    newDescribeWorkspaceSnapshotsResponse,

    -- * Response Lenses
    describeWorkspaceSnapshotsResponse_rebuildSnapshots,
    describeWorkspaceSnapshotsResponse_restoreSnapshots,
    describeWorkspaceSnapshotsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeWorkspaceSnapshots' smart constructor.
data DescribeWorkspaceSnapshots = DescribeWorkspaceSnapshots'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeWorkspaceSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'describeWorkspaceSnapshots_workspaceId' - The identifier of the WorkSpace.
newDescribeWorkspaceSnapshots ::
  -- | 'workspaceId'
  Core.Text ->
  DescribeWorkspaceSnapshots
newDescribeWorkspaceSnapshots pWorkspaceId_ =
  DescribeWorkspaceSnapshots'
    { workspaceId =
        pWorkspaceId_
    }

-- | The identifier of the WorkSpace.
describeWorkspaceSnapshots_workspaceId :: Lens.Lens' DescribeWorkspaceSnapshots Core.Text
describeWorkspaceSnapshots_workspaceId = Lens.lens (\DescribeWorkspaceSnapshots' {workspaceId} -> workspaceId) (\s@DescribeWorkspaceSnapshots' {} a -> s {workspaceId = a} :: DescribeWorkspaceSnapshots)

instance Core.AWSRequest DescribeWorkspaceSnapshots where
  type
    AWSResponse DescribeWorkspaceSnapshots =
      DescribeWorkspaceSnapshotsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkspaceSnapshotsResponse'
            Core.<$> (x Core..?> "RebuildSnapshots" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "RestoreSnapshots" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeWorkspaceSnapshots

instance Core.NFData DescribeWorkspaceSnapshots

instance Core.ToHeaders DescribeWorkspaceSnapshots where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DescribeWorkspaceSnapshots" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeWorkspaceSnapshots where
  toJSON DescribeWorkspaceSnapshots' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("WorkspaceId" Core..= workspaceId)]
      )

instance Core.ToPath DescribeWorkspaceSnapshots where
  toPath = Core.const "/"

instance Core.ToQuery DescribeWorkspaceSnapshots where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeWorkspaceSnapshotsResponse' smart constructor.
data DescribeWorkspaceSnapshotsResponse = DescribeWorkspaceSnapshotsResponse'
  { -- | Information about the snapshots that can be used to rebuild a WorkSpace.
    -- These snapshots include the user volume.
    rebuildSnapshots :: Core.Maybe [Snapshot],
    -- | Information about the snapshots that can be used to restore a WorkSpace.
    -- These snapshots include both the root volume and the user volume.
    restoreSnapshots :: Core.Maybe [Snapshot],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeWorkspaceSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rebuildSnapshots', 'describeWorkspaceSnapshotsResponse_rebuildSnapshots' - Information about the snapshots that can be used to rebuild a WorkSpace.
-- These snapshots include the user volume.
--
-- 'restoreSnapshots', 'describeWorkspaceSnapshotsResponse_restoreSnapshots' - Information about the snapshots that can be used to restore a WorkSpace.
-- These snapshots include both the root volume and the user volume.
--
-- 'httpStatus', 'describeWorkspaceSnapshotsResponse_httpStatus' - The response's http status code.
newDescribeWorkspaceSnapshotsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeWorkspaceSnapshotsResponse
newDescribeWorkspaceSnapshotsResponse pHttpStatus_ =
  DescribeWorkspaceSnapshotsResponse'
    { rebuildSnapshots =
        Core.Nothing,
      restoreSnapshots = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the snapshots that can be used to rebuild a WorkSpace.
-- These snapshots include the user volume.
describeWorkspaceSnapshotsResponse_rebuildSnapshots :: Lens.Lens' DescribeWorkspaceSnapshotsResponse (Core.Maybe [Snapshot])
describeWorkspaceSnapshotsResponse_rebuildSnapshots = Lens.lens (\DescribeWorkspaceSnapshotsResponse' {rebuildSnapshots} -> rebuildSnapshots) (\s@DescribeWorkspaceSnapshotsResponse' {} a -> s {rebuildSnapshots = a} :: DescribeWorkspaceSnapshotsResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the snapshots that can be used to restore a WorkSpace.
-- These snapshots include both the root volume and the user volume.
describeWorkspaceSnapshotsResponse_restoreSnapshots :: Lens.Lens' DescribeWorkspaceSnapshotsResponse (Core.Maybe [Snapshot])
describeWorkspaceSnapshotsResponse_restoreSnapshots = Lens.lens (\DescribeWorkspaceSnapshotsResponse' {restoreSnapshots} -> restoreSnapshots) (\s@DescribeWorkspaceSnapshotsResponse' {} a -> s {restoreSnapshots = a} :: DescribeWorkspaceSnapshotsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeWorkspaceSnapshotsResponse_httpStatus :: Lens.Lens' DescribeWorkspaceSnapshotsResponse Core.Int
describeWorkspaceSnapshotsResponse_httpStatus = Lens.lens (\DescribeWorkspaceSnapshotsResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkspaceSnapshotsResponse' {} a -> s {httpStatus = a} :: DescribeWorkspaceSnapshotsResponse)

instance
  Core.NFData
    DescribeWorkspaceSnapshotsResponse
