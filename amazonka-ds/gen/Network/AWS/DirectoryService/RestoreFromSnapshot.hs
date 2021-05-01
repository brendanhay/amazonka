{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DirectoryService.RestoreFromSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a directory using an existing directory snapshot.
--
-- When you restore a directory from a snapshot, any changes made to the
-- directory after the snapshot date are overwritten.
--
-- This action returns as soon as the restore operation is initiated. You
-- can monitor the progress of the restore operation by calling the
-- DescribeDirectories operation with the directory identifier. When the
-- __DirectoryDescription.Stage__ value changes to @Active@, the restore
-- operation is complete.
module Network.AWS.DirectoryService.RestoreFromSnapshot
  ( -- * Creating a Request
    RestoreFromSnapshot (..),
    newRestoreFromSnapshot,

    -- * Request Lenses
    restoreFromSnapshot_snapshotId,

    -- * Destructuring the Response
    RestoreFromSnapshotResponse (..),
    newRestoreFromSnapshotResponse,

    -- * Response Lenses
    restoreFromSnapshotResponse_httpStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | An object representing the inputs for the RestoreFromSnapshot operation.
--
-- /See:/ 'newRestoreFromSnapshot' smart constructor.
data RestoreFromSnapshot = RestoreFromSnapshot'
  { -- | The identifier of the snapshot to restore from.
    snapshotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RestoreFromSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotId', 'restoreFromSnapshot_snapshotId' - The identifier of the snapshot to restore from.
newRestoreFromSnapshot ::
  -- | 'snapshotId'
  Prelude.Text ->
  RestoreFromSnapshot
newRestoreFromSnapshot pSnapshotId_ =
  RestoreFromSnapshot' {snapshotId = pSnapshotId_}

-- | The identifier of the snapshot to restore from.
restoreFromSnapshot_snapshotId :: Lens.Lens' RestoreFromSnapshot Prelude.Text
restoreFromSnapshot_snapshotId = Lens.lens (\RestoreFromSnapshot' {snapshotId} -> snapshotId) (\s@RestoreFromSnapshot' {} a -> s {snapshotId = a} :: RestoreFromSnapshot)

instance Prelude.AWSRequest RestoreFromSnapshot where
  type
    Rs RestoreFromSnapshot =
      RestoreFromSnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RestoreFromSnapshotResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreFromSnapshot

instance Prelude.NFData RestoreFromSnapshot

instance Prelude.ToHeaders RestoreFromSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.RestoreFromSnapshot" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RestoreFromSnapshot where
  toJSON RestoreFromSnapshot' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("SnapshotId" Prelude..= snapshotId)]
      )

instance Prelude.ToPath RestoreFromSnapshot where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RestoreFromSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results of the RestoreFromSnapshot operation.
--
-- /See:/ 'newRestoreFromSnapshotResponse' smart constructor.
data RestoreFromSnapshotResponse = RestoreFromSnapshotResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RestoreFromSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'restoreFromSnapshotResponse_httpStatus' - The response's http status code.
newRestoreFromSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreFromSnapshotResponse
newRestoreFromSnapshotResponse pHttpStatus_ =
  RestoreFromSnapshotResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
restoreFromSnapshotResponse_httpStatus :: Lens.Lens' RestoreFromSnapshotResponse Prelude.Int
restoreFromSnapshotResponse_httpStatus = Lens.lens (\RestoreFromSnapshotResponse' {httpStatus} -> httpStatus) (\s@RestoreFromSnapshotResponse' {} a -> s {httpStatus = a} :: RestoreFromSnapshotResponse)

instance Prelude.NFData RestoreFromSnapshotResponse
