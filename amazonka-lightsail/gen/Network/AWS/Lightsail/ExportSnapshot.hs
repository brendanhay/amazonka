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
-- Module      : Network.AWS.Lightsail.ExportSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports an Amazon Lightsail instance or block storage disk snapshot to
-- Amazon Elastic Compute Cloud (Amazon EC2). This operation results in an
-- export snapshot record that can be used with the
-- @create cloud formation stack@ operation to create new Amazon EC2
-- instances.
--
-- Exported instance snapshots appear in Amazon EC2 as Amazon Machine
-- Images (AMIs), and the instance system disk appears as an Amazon Elastic
-- Block Store (Amazon EBS) volume. Exported disk snapshots appear in
-- Amazon EC2 as Amazon EBS volumes. Snapshots are exported to the same
-- Amazon Web Services Region in Amazon EC2 as the source Lightsail
-- snapshot.
--
-- The @export snapshot@ operation supports tag-based access control via
-- resource tags applied to the resource identified by
-- @source snapshot name@. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
--
-- Use the @get instance snapshots@ or @get disk snapshots@ operations to
-- get a list of snapshots that you can export to Amazon EC2.
module Network.AWS.Lightsail.ExportSnapshot
  ( -- * Creating a Request
    ExportSnapshot (..),
    newExportSnapshot,

    -- * Request Lenses
    exportSnapshot_sourceSnapshotName,

    -- * Destructuring the Response
    ExportSnapshotResponse (..),
    newExportSnapshotResponse,

    -- * Response Lenses
    exportSnapshotResponse_operations,
    exportSnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newExportSnapshot' smart constructor.
data ExportSnapshot = ExportSnapshot'
  { -- | The name of the instance or disk snapshot to be exported to Amazon EC2.
    sourceSnapshotName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExportSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceSnapshotName', 'exportSnapshot_sourceSnapshotName' - The name of the instance or disk snapshot to be exported to Amazon EC2.
newExportSnapshot ::
  -- | 'sourceSnapshotName'
  Core.Text ->
  ExportSnapshot
newExportSnapshot pSourceSnapshotName_ =
  ExportSnapshot'
    { sourceSnapshotName =
        pSourceSnapshotName_
    }

-- | The name of the instance or disk snapshot to be exported to Amazon EC2.
exportSnapshot_sourceSnapshotName :: Lens.Lens' ExportSnapshot Core.Text
exportSnapshot_sourceSnapshotName = Lens.lens (\ExportSnapshot' {sourceSnapshotName} -> sourceSnapshotName) (\s@ExportSnapshot' {} a -> s {sourceSnapshotName = a} :: ExportSnapshot)

instance Core.AWSRequest ExportSnapshot where
  type
    AWSResponse ExportSnapshot =
      ExportSnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportSnapshotResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ExportSnapshot

instance Core.NFData ExportSnapshot

instance Core.ToHeaders ExportSnapshot where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.ExportSnapshot" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ExportSnapshot where
  toJSON ExportSnapshot' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("sourceSnapshotName" Core..= sourceSnapshotName)
          ]
      )

instance Core.ToPath ExportSnapshot where
  toPath = Core.const "/"

instance Core.ToQuery ExportSnapshot where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newExportSnapshotResponse' smart constructor.
data ExportSnapshotResponse = ExportSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExportSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'exportSnapshotResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'exportSnapshotResponse_httpStatus' - The response's http status code.
newExportSnapshotResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ExportSnapshotResponse
newExportSnapshotResponse pHttpStatus_ =
  ExportSnapshotResponse'
    { operations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
exportSnapshotResponse_operations :: Lens.Lens' ExportSnapshotResponse (Core.Maybe [Operation])
exportSnapshotResponse_operations = Lens.lens (\ExportSnapshotResponse' {operations} -> operations) (\s@ExportSnapshotResponse' {} a -> s {operations = a} :: ExportSnapshotResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
exportSnapshotResponse_httpStatus :: Lens.Lens' ExportSnapshotResponse Core.Int
exportSnapshotResponse_httpStatus = Lens.lens (\ExportSnapshotResponse' {httpStatus} -> httpStatus) (\s@ExportSnapshotResponse' {} a -> s {httpStatus = a} :: ExportSnapshotResponse)

instance Core.NFData ExportSnapshotResponse
