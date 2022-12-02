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
-- Module      : Amazonka.Lightsail.ExportSnapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
--
-- Use the @get instance snapshots@ or @get disk snapshots@ operations to
-- get a list of snapshots that you can export to Amazon EC2.
module Amazonka.Lightsail.ExportSnapshot
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExportSnapshot' smart constructor.
data ExportSnapshot = ExportSnapshot'
  { -- | The name of the instance or disk snapshot to be exported to Amazon EC2.
    sourceSnapshotName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ExportSnapshot
newExportSnapshot pSourceSnapshotName_ =
  ExportSnapshot'
    { sourceSnapshotName =
        pSourceSnapshotName_
    }

-- | The name of the instance or disk snapshot to be exported to Amazon EC2.
exportSnapshot_sourceSnapshotName :: Lens.Lens' ExportSnapshot Prelude.Text
exportSnapshot_sourceSnapshotName = Lens.lens (\ExportSnapshot' {sourceSnapshotName} -> sourceSnapshotName) (\s@ExportSnapshot' {} a -> s {sourceSnapshotName = a} :: ExportSnapshot)

instance Core.AWSRequest ExportSnapshot where
  type
    AWSResponse ExportSnapshot =
      ExportSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportSnapshotResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExportSnapshot where
  hashWithSalt _salt ExportSnapshot' {..} =
    _salt `Prelude.hashWithSalt` sourceSnapshotName

instance Prelude.NFData ExportSnapshot where
  rnf ExportSnapshot' {..} =
    Prelude.rnf sourceSnapshotName

instance Data.ToHeaders ExportSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.ExportSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExportSnapshot where
  toJSON ExportSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("sourceSnapshotName" Data..= sourceSnapshotName)
          ]
      )

instance Data.ToPath ExportSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery ExportSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExportSnapshotResponse' smart constructor.
data ExportSnapshotResponse = ExportSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ExportSnapshotResponse
newExportSnapshotResponse pHttpStatus_ =
  ExportSnapshotResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
exportSnapshotResponse_operations :: Lens.Lens' ExportSnapshotResponse (Prelude.Maybe [Operation])
exportSnapshotResponse_operations = Lens.lens (\ExportSnapshotResponse' {operations} -> operations) (\s@ExportSnapshotResponse' {} a -> s {operations = a} :: ExportSnapshotResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
exportSnapshotResponse_httpStatus :: Lens.Lens' ExportSnapshotResponse Prelude.Int
exportSnapshotResponse_httpStatus = Lens.lens (\ExportSnapshotResponse' {httpStatus} -> httpStatus) (\s@ExportSnapshotResponse' {} a -> s {httpStatus = a} :: ExportSnapshotResponse)

instance Prelude.NFData ExportSnapshotResponse where
  rnf ExportSnapshotResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
