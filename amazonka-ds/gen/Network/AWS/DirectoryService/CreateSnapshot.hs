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
-- Module      : Network.AWS.DirectoryService.CreateSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a Simple AD or Microsoft AD directory in the AWS
-- cloud.
--
-- You cannot take snapshots of AD Connector directories.
module Network.AWS.DirectoryService.CreateSnapshot
  ( -- * Creating a Request
    CreateSnapshot (..),
    newCreateSnapshot,

    -- * Request Lenses
    createSnapshot_name,
    createSnapshot_directoryId,

    -- * Destructuring the Response
    CreateSnapshotResponse (..),
    newCreateSnapshotResponse,

    -- * Response Lenses
    createSnapshotResponse_snapshotId,
    createSnapshotResponse_httpStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the CreateSnapshot operation.
--
-- /See:/ 'newCreateSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { -- | The descriptive name to apply to the snapshot.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the directory of which to take a snapshot.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createSnapshot_name' - The descriptive name to apply to the snapshot.
--
-- 'directoryId', 'createSnapshot_directoryId' - The identifier of the directory of which to take a snapshot.
newCreateSnapshot ::
  -- | 'directoryId'
  Prelude.Text ->
  CreateSnapshot
newCreateSnapshot pDirectoryId_ =
  CreateSnapshot'
    { name = Prelude.Nothing,
      directoryId = pDirectoryId_
    }

-- | The descriptive name to apply to the snapshot.
createSnapshot_name :: Lens.Lens' CreateSnapshot (Prelude.Maybe Prelude.Text)
createSnapshot_name = Lens.lens (\CreateSnapshot' {name} -> name) (\s@CreateSnapshot' {} a -> s {name = a} :: CreateSnapshot)

-- | The identifier of the directory of which to take a snapshot.
createSnapshot_directoryId :: Lens.Lens' CreateSnapshot Prelude.Text
createSnapshot_directoryId = Lens.lens (\CreateSnapshot' {directoryId} -> directoryId) (\s@CreateSnapshot' {} a -> s {directoryId = a} :: CreateSnapshot)

instance Prelude.AWSRequest CreateSnapshot where
  type Rs CreateSnapshot = CreateSnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSnapshotResponse'
            Prelude.<$> (x Prelude..?> "SnapshotId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSnapshot

instance Prelude.NFData CreateSnapshot

instance Prelude.ToHeaders CreateSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.CreateSnapshot" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateSnapshot where
  toJSON CreateSnapshot' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Name" Prelude..=) Prelude.<$> name,
            Prelude.Just ("DirectoryId" Prelude..= directoryId)
          ]
      )

instance Prelude.ToPath CreateSnapshot where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results of the CreateSnapshot operation.
--
-- /See:/ 'newCreateSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
  { -- | The identifier of the snapshot that was created.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotId', 'createSnapshotResponse_snapshotId' - The identifier of the snapshot that was created.
--
-- 'httpStatus', 'createSnapshotResponse_httpStatus' - The response's http status code.
newCreateSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSnapshotResponse
newCreateSnapshotResponse pHttpStatus_ =
  CreateSnapshotResponse'
    { snapshotId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the snapshot that was created.
createSnapshotResponse_snapshotId :: Lens.Lens' CreateSnapshotResponse (Prelude.Maybe Prelude.Text)
createSnapshotResponse_snapshotId = Lens.lens (\CreateSnapshotResponse' {snapshotId} -> snapshotId) (\s@CreateSnapshotResponse' {} a -> s {snapshotId = a} :: CreateSnapshotResponse)

-- | The response's http status code.
createSnapshotResponse_httpStatus :: Lens.Lens' CreateSnapshotResponse Prelude.Int
createSnapshotResponse_httpStatus = Lens.lens (\CreateSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateSnapshotResponse' {} a -> s {httpStatus = a} :: CreateSnapshotResponse)

instance Prelude.NFData CreateSnapshotResponse
