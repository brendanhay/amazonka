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
-- Module      : Amazonka.RedshiftServerLess.CreateSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of all databases in a namespace. For more information
-- about snapshots, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/serverless-snapshots-recovery.html Working with snapshots and recovery points>.
module Amazonka.RedshiftServerLess.CreateSnapshot
  ( -- * Creating a Request
    CreateSnapshot (..),
    newCreateSnapshot,

    -- * Request Lenses
    createSnapshot_retentionPeriod,
    createSnapshot_tags,
    createSnapshot_namespaceName,
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
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { -- | How long to retain the created snapshot.
    retentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | An array of
    -- <https://docs.aws.amazon.com/redshift-serverless/latest/APIReference/API_Tag.html Tag objects>
    -- to associate with the snapshot.
    tags :: Prelude.Maybe [Tag],
    -- | The namespace to create a snapshot for.
    namespaceName :: Prelude.Text,
    -- | The name of the snapshot.
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
-- 'retentionPeriod', 'createSnapshot_retentionPeriod' - How long to retain the created snapshot.
--
-- 'tags', 'createSnapshot_tags' - An array of
-- <https://docs.aws.amazon.com/redshift-serverless/latest/APIReference/API_Tag.html Tag objects>
-- to associate with the snapshot.
--
-- 'namespaceName', 'createSnapshot_namespaceName' - The namespace to create a snapshot for.
--
-- 'snapshotName', 'createSnapshot_snapshotName' - The name of the snapshot.
newCreateSnapshot ::
  -- | 'namespaceName'
  Prelude.Text ->
  -- | 'snapshotName'
  Prelude.Text ->
  CreateSnapshot
newCreateSnapshot pNamespaceName_ pSnapshotName_ =
  CreateSnapshot'
    { retentionPeriod = Prelude.Nothing,
      tags = Prelude.Nothing,
      namespaceName = pNamespaceName_,
      snapshotName = pSnapshotName_
    }

-- | How long to retain the created snapshot.
createSnapshot_retentionPeriod :: Lens.Lens' CreateSnapshot (Prelude.Maybe Prelude.Int)
createSnapshot_retentionPeriod = Lens.lens (\CreateSnapshot' {retentionPeriod} -> retentionPeriod) (\s@CreateSnapshot' {} a -> s {retentionPeriod = a} :: CreateSnapshot)

-- | An array of
-- <https://docs.aws.amazon.com/redshift-serverless/latest/APIReference/API_Tag.html Tag objects>
-- to associate with the snapshot.
createSnapshot_tags :: Lens.Lens' CreateSnapshot (Prelude.Maybe [Tag])
createSnapshot_tags = Lens.lens (\CreateSnapshot' {tags} -> tags) (\s@CreateSnapshot' {} a -> s {tags = a} :: CreateSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The namespace to create a snapshot for.
createSnapshot_namespaceName :: Lens.Lens' CreateSnapshot Prelude.Text
createSnapshot_namespaceName = Lens.lens (\CreateSnapshot' {namespaceName} -> namespaceName) (\s@CreateSnapshot' {} a -> s {namespaceName = a} :: CreateSnapshot)

-- | The name of the snapshot.
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
            Prelude.<$> (x Data..?> "snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSnapshot where
  hashWithSalt _salt CreateSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` namespaceName
      `Prelude.hashWithSalt` snapshotName

instance Prelude.NFData CreateSnapshot where
  rnf CreateSnapshot' {..} =
    Prelude.rnf retentionPeriod `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf namespaceName `Prelude.seq`
          Prelude.rnf snapshotName

instance Data.ToHeaders CreateSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.CreateSnapshot" ::
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
          [ ("retentionPeriod" Data..=)
              Prelude.<$> retentionPeriod,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("namespaceName" Data..= namespaceName),
            Prelude.Just ("snapshotName" Data..= snapshotName)
          ]
      )

instance Data.ToPath CreateSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
  { -- | The created snapshot object.
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
-- 'snapshot', 'createSnapshotResponse_snapshot' - The created snapshot object.
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

-- | The created snapshot object.
createSnapshotResponse_snapshot :: Lens.Lens' CreateSnapshotResponse (Prelude.Maybe Snapshot)
createSnapshotResponse_snapshot = Lens.lens (\CreateSnapshotResponse' {snapshot} -> snapshot) (\s@CreateSnapshotResponse' {} a -> s {snapshot = a} :: CreateSnapshotResponse)

-- | The response's http status code.
createSnapshotResponse_httpStatus :: Lens.Lens' CreateSnapshotResponse Prelude.Int
createSnapshotResponse_httpStatus = Lens.lens (\CreateSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateSnapshotResponse' {} a -> s {httpStatus = a} :: CreateSnapshotResponse)

instance Prelude.NFData CreateSnapshotResponse where
  rnf CreateSnapshotResponse' {..} =
    Prelude.rnf snapshot `Prelude.seq`
      Prelude.rnf httpStatus
