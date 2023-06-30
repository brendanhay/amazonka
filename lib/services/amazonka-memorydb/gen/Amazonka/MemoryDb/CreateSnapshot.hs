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
-- Module      : Amazonka.MemoryDb.CreateSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a copy of an entire cluster at a specific moment in time.
module Amazonka.MemoryDb.CreateSnapshot
  ( -- * Creating a Request
    CreateSnapshot (..),
    newCreateSnapshot,

    -- * Request Lenses
    createSnapshot_kmsKeyId,
    createSnapshot_tags,
    createSnapshot_clusterName,
    createSnapshot_snapshotName,

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
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { -- | The ID of the KMS key used to encrypt the snapshot.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A list of tags to be added to this resource. A tag is a key-value pair.
    -- A tag key must be accompanied by a tag value, although null is accepted.
    tags :: Prelude.Maybe [Tag],
    -- | The snapshot is created from this cluster.
    clusterName :: Prelude.Text,
    -- | A name for the snapshot being created.
    snapshotName :: Prelude.Text
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
-- 'kmsKeyId', 'createSnapshot_kmsKeyId' - The ID of the KMS key used to encrypt the snapshot.
--
-- 'tags', 'createSnapshot_tags' - A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
--
-- 'clusterName', 'createSnapshot_clusterName' - The snapshot is created from this cluster.
--
-- 'snapshotName', 'createSnapshot_snapshotName' - A name for the snapshot being created.
newCreateSnapshot ::
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'snapshotName'
  Prelude.Text ->
  CreateSnapshot
newCreateSnapshot pClusterName_ pSnapshotName_ =
  CreateSnapshot'
    { kmsKeyId = Prelude.Nothing,
      tags = Prelude.Nothing,
      clusterName = pClusterName_,
      snapshotName = pSnapshotName_
    }

-- | The ID of the KMS key used to encrypt the snapshot.
createSnapshot_kmsKeyId :: Lens.Lens' CreateSnapshot (Prelude.Maybe Prelude.Text)
createSnapshot_kmsKeyId = Lens.lens (\CreateSnapshot' {kmsKeyId} -> kmsKeyId) (\s@CreateSnapshot' {} a -> s {kmsKeyId = a} :: CreateSnapshot)

-- | A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
createSnapshot_tags :: Lens.Lens' CreateSnapshot (Prelude.Maybe [Tag])
createSnapshot_tags = Lens.lens (\CreateSnapshot' {tags} -> tags) (\s@CreateSnapshot' {} a -> s {tags = a} :: CreateSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The snapshot is created from this cluster.
createSnapshot_clusterName :: Lens.Lens' CreateSnapshot Prelude.Text
createSnapshot_clusterName = Lens.lens (\CreateSnapshot' {clusterName} -> clusterName) (\s@CreateSnapshot' {} a -> s {clusterName = a} :: CreateSnapshot)

-- | A name for the snapshot being created.
createSnapshot_snapshotName :: Lens.Lens' CreateSnapshot Prelude.Text
createSnapshot_snapshotName = Lens.lens (\CreateSnapshot' {snapshotName} -> snapshotName) (\s@CreateSnapshot' {} a -> s {snapshotName = a} :: CreateSnapshot)

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
            Prelude.<$> (x Data..?> "Snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSnapshot where
  hashWithSalt _salt CreateSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` snapshotName

instance Prelude.NFData CreateSnapshot where
  rnf CreateSnapshot' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf snapshotName

instance Data.ToHeaders CreateSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonMemoryDB.CreateSnapshot" ::
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
          [ ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("ClusterName" Data..= clusterName),
            Prelude.Just ("SnapshotName" Data..= snapshotName)
          ]
      )

instance Data.ToPath CreateSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
  { -- | The newly-created snapshot.
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
-- 'snapshot', 'createSnapshotResponse_snapshot' - The newly-created snapshot.
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

-- | The newly-created snapshot.
createSnapshotResponse_snapshot :: Lens.Lens' CreateSnapshotResponse (Prelude.Maybe Snapshot)
createSnapshotResponse_snapshot = Lens.lens (\CreateSnapshotResponse' {snapshot} -> snapshot) (\s@CreateSnapshotResponse' {} a -> s {snapshot = a} :: CreateSnapshotResponse)

-- | The response's http status code.
createSnapshotResponse_httpStatus :: Lens.Lens' CreateSnapshotResponse Prelude.Int
createSnapshotResponse_httpStatus = Lens.lens (\CreateSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateSnapshotResponse' {} a -> s {httpStatus = a} :: CreateSnapshotResponse)

instance Prelude.NFData CreateSnapshotResponse where
  rnf CreateSnapshotResponse' {..} =
    Prelude.rnf snapshot
      `Prelude.seq` Prelude.rnf httpStatus
