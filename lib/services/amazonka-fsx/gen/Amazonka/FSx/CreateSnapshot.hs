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
-- Module      : Amazonka.FSx.CreateSnapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of an existing Amazon FSx for OpenZFS volume. With
-- snapshots, you can easily undo file changes and compare file versions by
-- restoring the volume to a previous version.
--
-- If a snapshot with the specified client request token exists, and the
-- parameters match, this operation returns the description of the existing
-- snapshot. If a snapshot with the specified client request token exists,
-- and the parameters don\'t match, this operation returns
-- @IncompatibleParameterError@. If a snapshot with the specified client
-- request token doesn\'t exist, @CreateSnapshot@ does the following:
--
-- -   Creates a new OpenZFS snapshot with an assigned ID, and an initial
--     lifecycle state of @CREATING@.
--
-- -   Returns the description of the snapshot.
--
-- By using the idempotent operation, you can retry a @CreateSnapshot@
-- operation without the risk of creating an extra snapshot. This approach
-- can be useful when an initial call fails in a way that makes it unclear
-- whether a snapshot was created. If you use the same client request token
-- and the initial call created a snapshot, the operation returns a
-- successful result because all the parameters are the same.
--
-- The @CreateSnapshot@ operation returns while the snapshot\'s lifecycle
-- state is still @CREATING@. You can check the snapshot creation status by
-- calling the
-- <https://docs.aws.amazon.com/fsx/latest/APIReference/API_DescribeSnapshots.html DescribeSnapshots>
-- operation, which returns the snapshot state along with other
-- information.
module Amazonka.FSx.CreateSnapshot
  ( -- * Creating a Request
    CreateSnapshot (..),
    newCreateSnapshot,

    -- * Request Lenses
    createSnapshot_tags,
    createSnapshot_clientRequestToken,
    createSnapshot_name,
    createSnapshot_volumeId,

    -- * Destructuring the Response
    CreateSnapshotResponse (..),
    newCreateSnapshotResponse,

    -- * Response Lenses
    createSnapshotResponse_snapshot,
    createSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the snapshot.
    name :: Prelude.Text,
    -- | The ID of the volume that you are taking a snapshot of.
    volumeId :: Prelude.Text
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
-- 'tags', 'createSnapshot_tags' - Undocumented member.
--
-- 'clientRequestToken', 'createSnapshot_clientRequestToken' - Undocumented member.
--
-- 'name', 'createSnapshot_name' - The name of the snapshot.
--
-- 'volumeId', 'createSnapshot_volumeId' - The ID of the volume that you are taking a snapshot of.
newCreateSnapshot ::
  -- | 'name'
  Prelude.Text ->
  -- | 'volumeId'
  Prelude.Text ->
  CreateSnapshot
newCreateSnapshot pName_ pVolumeId_ =
  CreateSnapshot'
    { tags = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      name = pName_,
      volumeId = pVolumeId_
    }

-- | Undocumented member.
createSnapshot_tags :: Lens.Lens' CreateSnapshot (Prelude.Maybe (Prelude.NonEmpty Tag))
createSnapshot_tags = Lens.lens (\CreateSnapshot' {tags} -> tags) (\s@CreateSnapshot' {} a -> s {tags = a} :: CreateSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createSnapshot_clientRequestToken :: Lens.Lens' CreateSnapshot (Prelude.Maybe Prelude.Text)
createSnapshot_clientRequestToken = Lens.lens (\CreateSnapshot' {clientRequestToken} -> clientRequestToken) (\s@CreateSnapshot' {} a -> s {clientRequestToken = a} :: CreateSnapshot)

-- | The name of the snapshot.
createSnapshot_name :: Lens.Lens' CreateSnapshot Prelude.Text
createSnapshot_name = Lens.lens (\CreateSnapshot' {name} -> name) (\s@CreateSnapshot' {} a -> s {name = a} :: CreateSnapshot)

-- | The ID of the volume that you are taking a snapshot of.
createSnapshot_volumeId :: Lens.Lens' CreateSnapshot Prelude.Text
createSnapshot_volumeId = Lens.lens (\CreateSnapshot' {volumeId} -> volumeId) (\s@CreateSnapshot' {} a -> s {volumeId = a} :: CreateSnapshot)

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
            Prelude.<$> (x Core..?> "Snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSnapshot where
  hashWithSalt _salt CreateSnapshot' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` volumeId

instance Prelude.NFData CreateSnapshot where
  rnf CreateSnapshot' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf volumeId

instance Core.ToHeaders CreateSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSSimbaAPIService_v20180301.CreateSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSnapshot where
  toJSON CreateSnapshot' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("VolumeId" Core..= volumeId)
          ]
      )

instance Core.ToPath CreateSnapshot where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
  { -- | A description of the snapshot.
    snapshot :: Prelude.Maybe Snapshot,
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
-- 'snapshot', 'createSnapshotResponse_snapshot' - A description of the snapshot.
--
-- 'httpStatus', 'createSnapshotResponse_httpStatus' - The response's http status code.
newCreateSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSnapshotResponse
newCreateSnapshotResponse pHttpStatus_ =
  CreateSnapshotResponse'
    { snapshot = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the snapshot.
createSnapshotResponse_snapshot :: Lens.Lens' CreateSnapshotResponse (Prelude.Maybe Snapshot)
createSnapshotResponse_snapshot = Lens.lens (\CreateSnapshotResponse' {snapshot} -> snapshot) (\s@CreateSnapshotResponse' {} a -> s {snapshot = a} :: CreateSnapshotResponse)

-- | The response's http status code.
createSnapshotResponse_httpStatus :: Lens.Lens' CreateSnapshotResponse Prelude.Int
createSnapshotResponse_httpStatus = Lens.lens (\CreateSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateSnapshotResponse' {} a -> s {httpStatus = a} :: CreateSnapshotResponse)

instance Prelude.NFData CreateSnapshotResponse where
  rnf CreateSnapshotResponse' {..} =
    Prelude.rnf snapshot
      `Prelude.seq` Prelude.rnf httpStatus
