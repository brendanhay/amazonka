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
-- Module      : Amazonka.DocDbElastic.CreateClusterSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a cluster.
module Amazonka.DocDbElastic.CreateClusterSnapshot
  ( -- * Creating a Request
    CreateClusterSnapshot (..),
    newCreateClusterSnapshot,

    -- * Request Lenses
    createClusterSnapshot_tags,
    createClusterSnapshot_clusterArn,
    createClusterSnapshot_snapshotName,

    -- * Destructuring the Response
    CreateClusterSnapshotResponse (..),
    newCreateClusterSnapshotResponse,

    -- * Response Lenses
    createClusterSnapshotResponse_httpStatus,
    createClusterSnapshotResponse_snapshot,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocDbElastic.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateClusterSnapshot' smart constructor.
data CreateClusterSnapshot = CreateClusterSnapshot'
  { -- | The tags to be assigned to the new Elastic DocumentDB snapshot.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The arn of the Elastic DocumentDB cluster that the snapshot will be
    -- taken from.
    clusterArn :: Prelude.Text,
    -- | The name of the Elastic DocumentDB snapshot.
    snapshotName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateClusterSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createClusterSnapshot_tags' - The tags to be assigned to the new Elastic DocumentDB snapshot.
--
-- 'clusterArn', 'createClusterSnapshot_clusterArn' - The arn of the Elastic DocumentDB cluster that the snapshot will be
-- taken from.
--
-- 'snapshotName', 'createClusterSnapshot_snapshotName' - The name of the Elastic DocumentDB snapshot.
newCreateClusterSnapshot ::
  -- | 'clusterArn'
  Prelude.Text ->
  -- | 'snapshotName'
  Prelude.Text ->
  CreateClusterSnapshot
newCreateClusterSnapshot pClusterArn_ pSnapshotName_ =
  CreateClusterSnapshot'
    { tags = Prelude.Nothing,
      clusterArn = pClusterArn_,
      snapshotName = pSnapshotName_
    }

-- | The tags to be assigned to the new Elastic DocumentDB snapshot.
createClusterSnapshot_tags :: Lens.Lens' CreateClusterSnapshot (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createClusterSnapshot_tags = Lens.lens (\CreateClusterSnapshot' {tags} -> tags) (\s@CreateClusterSnapshot' {} a -> s {tags = a} :: CreateClusterSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The arn of the Elastic DocumentDB cluster that the snapshot will be
-- taken from.
createClusterSnapshot_clusterArn :: Lens.Lens' CreateClusterSnapshot Prelude.Text
createClusterSnapshot_clusterArn = Lens.lens (\CreateClusterSnapshot' {clusterArn} -> clusterArn) (\s@CreateClusterSnapshot' {} a -> s {clusterArn = a} :: CreateClusterSnapshot)

-- | The name of the Elastic DocumentDB snapshot.
createClusterSnapshot_snapshotName :: Lens.Lens' CreateClusterSnapshot Prelude.Text
createClusterSnapshot_snapshotName = Lens.lens (\CreateClusterSnapshot' {snapshotName} -> snapshotName) (\s@CreateClusterSnapshot' {} a -> s {snapshotName = a} :: CreateClusterSnapshot)

instance Core.AWSRequest CreateClusterSnapshot where
  type
    AWSResponse CreateClusterSnapshot =
      CreateClusterSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateClusterSnapshotResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "snapshot")
      )

instance Prelude.Hashable CreateClusterSnapshot where
  hashWithSalt _salt CreateClusterSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` snapshotName

instance Prelude.NFData CreateClusterSnapshot where
  rnf CreateClusterSnapshot' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf snapshotName

instance Data.ToHeaders CreateClusterSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateClusterSnapshot where
  toJSON CreateClusterSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("clusterArn" Data..= clusterArn),
            Prelude.Just ("snapshotName" Data..= snapshotName)
          ]
      )

instance Data.ToPath CreateClusterSnapshot where
  toPath = Prelude.const "/cluster-snapshot"

instance Data.ToQuery CreateClusterSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateClusterSnapshotResponse' smart constructor.
data CreateClusterSnapshotResponse = CreateClusterSnapshotResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns information about the new Elastic DocumentDB snapshot.
    snapshot :: ClusterSnapshot
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateClusterSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createClusterSnapshotResponse_httpStatus' - The response's http status code.
--
-- 'snapshot', 'createClusterSnapshotResponse_snapshot' - Returns information about the new Elastic DocumentDB snapshot.
newCreateClusterSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'snapshot'
  ClusterSnapshot ->
  CreateClusterSnapshotResponse
newCreateClusterSnapshotResponse
  pHttpStatus_
  pSnapshot_ =
    CreateClusterSnapshotResponse'
      { httpStatus =
          pHttpStatus_,
        snapshot = pSnapshot_
      }

-- | The response's http status code.
createClusterSnapshotResponse_httpStatus :: Lens.Lens' CreateClusterSnapshotResponse Prelude.Int
createClusterSnapshotResponse_httpStatus = Lens.lens (\CreateClusterSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateClusterSnapshotResponse' {} a -> s {httpStatus = a} :: CreateClusterSnapshotResponse)

-- | Returns information about the new Elastic DocumentDB snapshot.
createClusterSnapshotResponse_snapshot :: Lens.Lens' CreateClusterSnapshotResponse ClusterSnapshot
createClusterSnapshotResponse_snapshot = Lens.lens (\CreateClusterSnapshotResponse' {snapshot} -> snapshot) (\s@CreateClusterSnapshotResponse' {} a -> s {snapshot = a} :: CreateClusterSnapshotResponse)

instance Prelude.NFData CreateClusterSnapshotResponse where
  rnf CreateClusterSnapshotResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf snapshot
