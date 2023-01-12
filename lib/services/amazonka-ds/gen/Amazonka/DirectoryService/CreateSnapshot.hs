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
-- Module      : Amazonka.DirectoryService.CreateSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a Simple AD or Microsoft AD directory in the
-- Amazon Web Services cloud.
--
-- You cannot take snapshots of AD Connector directories.
module Amazonka.DirectoryService.CreateSnapshot
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the CreateSnapshot operation.
--
-- /See:/ 'newCreateSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { -- | The descriptive name to apply to the snapshot.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the directory of which to take a snapshot.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest CreateSnapshot where
  type
    AWSResponse CreateSnapshot =
      CreateSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSnapshotResponse'
            Prelude.<$> (x Data..?> "SnapshotId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSnapshot where
  hashWithSalt _salt CreateSnapshot' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` directoryId

instance Prelude.NFData CreateSnapshot where
  rnf CreateSnapshot' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf directoryId

instance Data.ToHeaders CreateSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.CreateSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSnapshot where
  toJSON CreateSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            Prelude.Just ("DirectoryId" Data..= directoryId)
          ]
      )

instance Data.ToPath CreateSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSnapshot where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateSnapshotResponse where
  rnf CreateSnapshotResponse' {..} =
    Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf httpStatus
