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
-- Module      : Amazonka.Neptune.CreateDBClusterSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a DB cluster.
module Amazonka.Neptune.CreateDBClusterSnapshot
  ( -- * Creating a Request
    CreateDBClusterSnapshot (..),
    newCreateDBClusterSnapshot,

    -- * Request Lenses
    createDBClusterSnapshot_tags,
    createDBClusterSnapshot_dbClusterSnapshotIdentifier,
    createDBClusterSnapshot_dbClusterIdentifier,

    -- * Destructuring the Response
    CreateDBClusterSnapshotResponse (..),
    newCreateDBClusterSnapshotResponse,

    -- * Response Lenses
    createDBClusterSnapshotResponse_dbClusterSnapshot,
    createDBClusterSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDBClusterSnapshot' smart constructor.
data CreateDBClusterSnapshot = CreateDBClusterSnapshot'
  { -- | The tags to be assigned to the DB cluster snapshot.
    tags :: Prelude.Maybe [Tag],
    -- | The identifier of the DB cluster snapshot. This parameter is stored as a
    -- lowercase string.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @my-cluster1-snapshot1@
    dbClusterSnapshotIdentifier :: Prelude.Text,
    -- | The identifier of the DB cluster to create a snapshot for. This
    -- parameter is not case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DBCluster.
    --
    -- Example: @my-cluster1@
    dbClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBClusterSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDBClusterSnapshot_tags' - The tags to be assigned to the DB cluster snapshot.
--
-- 'dbClusterSnapshotIdentifier', 'createDBClusterSnapshot_dbClusterSnapshotIdentifier' - The identifier of the DB cluster snapshot. This parameter is stored as a
-- lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster1-snapshot1@
--
-- 'dbClusterIdentifier', 'createDBClusterSnapshot_dbClusterIdentifier' - The identifier of the DB cluster to create a snapshot for. This
-- parameter is not case-sensitive.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBCluster.
--
-- Example: @my-cluster1@
newCreateDBClusterSnapshot ::
  -- | 'dbClusterSnapshotIdentifier'
  Prelude.Text ->
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  CreateDBClusterSnapshot
newCreateDBClusterSnapshot
  pDBClusterSnapshotIdentifier_
  pDBClusterIdentifier_ =
    CreateDBClusterSnapshot'
      { tags = Prelude.Nothing,
        dbClusterSnapshotIdentifier =
          pDBClusterSnapshotIdentifier_,
        dbClusterIdentifier = pDBClusterIdentifier_
      }

-- | The tags to be assigned to the DB cluster snapshot.
createDBClusterSnapshot_tags :: Lens.Lens' CreateDBClusterSnapshot (Prelude.Maybe [Tag])
createDBClusterSnapshot_tags = Lens.lens (\CreateDBClusterSnapshot' {tags} -> tags) (\s@CreateDBClusterSnapshot' {} a -> s {tags = a} :: CreateDBClusterSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the DB cluster snapshot. This parameter is stored as a
-- lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster1-snapshot1@
createDBClusterSnapshot_dbClusterSnapshotIdentifier :: Lens.Lens' CreateDBClusterSnapshot Prelude.Text
createDBClusterSnapshot_dbClusterSnapshotIdentifier = Lens.lens (\CreateDBClusterSnapshot' {dbClusterSnapshotIdentifier} -> dbClusterSnapshotIdentifier) (\s@CreateDBClusterSnapshot' {} a -> s {dbClusterSnapshotIdentifier = a} :: CreateDBClusterSnapshot)

-- | The identifier of the DB cluster to create a snapshot for. This
-- parameter is not case-sensitive.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBCluster.
--
-- Example: @my-cluster1@
createDBClusterSnapshot_dbClusterIdentifier :: Lens.Lens' CreateDBClusterSnapshot Prelude.Text
createDBClusterSnapshot_dbClusterIdentifier = Lens.lens (\CreateDBClusterSnapshot' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@CreateDBClusterSnapshot' {} a -> s {dbClusterIdentifier = a} :: CreateDBClusterSnapshot)

instance Core.AWSRequest CreateDBClusterSnapshot where
  type
    AWSResponse CreateDBClusterSnapshot =
      CreateDBClusterSnapshotResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateDBClusterSnapshotResult"
      ( \s h x ->
          CreateDBClusterSnapshotResponse'
            Prelude.<$> (x Data..@? "DBClusterSnapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDBClusterSnapshot where
  hashWithSalt _salt CreateDBClusterSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` dbClusterSnapshotIdentifier
      `Prelude.hashWithSalt` dbClusterIdentifier

instance Prelude.NFData CreateDBClusterSnapshot where
  rnf CreateDBClusterSnapshot' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf dbClusterSnapshotIdentifier
      `Prelude.seq` Prelude.rnf dbClusterIdentifier

instance Data.ToHeaders CreateDBClusterSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateDBClusterSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDBClusterSnapshot where
  toQuery CreateDBClusterSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateDBClusterSnapshot" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "DBClusterSnapshotIdentifier"
          Data.=: dbClusterSnapshotIdentifier,
        "DBClusterIdentifier" Data.=: dbClusterIdentifier
      ]

-- | /See:/ 'newCreateDBClusterSnapshotResponse' smart constructor.
data CreateDBClusterSnapshotResponse = CreateDBClusterSnapshotResponse'
  { dbClusterSnapshot :: Prelude.Maybe DBClusterSnapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBClusterSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterSnapshot', 'createDBClusterSnapshotResponse_dbClusterSnapshot' - Undocumented member.
--
-- 'httpStatus', 'createDBClusterSnapshotResponse_httpStatus' - The response's http status code.
newCreateDBClusterSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDBClusterSnapshotResponse
newCreateDBClusterSnapshotResponse pHttpStatus_ =
  CreateDBClusterSnapshotResponse'
    { dbClusterSnapshot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createDBClusterSnapshotResponse_dbClusterSnapshot :: Lens.Lens' CreateDBClusterSnapshotResponse (Prelude.Maybe DBClusterSnapshot)
createDBClusterSnapshotResponse_dbClusterSnapshot = Lens.lens (\CreateDBClusterSnapshotResponse' {dbClusterSnapshot} -> dbClusterSnapshot) (\s@CreateDBClusterSnapshotResponse' {} a -> s {dbClusterSnapshot = a} :: CreateDBClusterSnapshotResponse)

-- | The response's http status code.
createDBClusterSnapshotResponse_httpStatus :: Lens.Lens' CreateDBClusterSnapshotResponse Prelude.Int
createDBClusterSnapshotResponse_httpStatus = Lens.lens (\CreateDBClusterSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateDBClusterSnapshotResponse' {} a -> s {httpStatus = a} :: CreateDBClusterSnapshotResponse)

instance
  Prelude.NFData
    CreateDBClusterSnapshotResponse
  where
  rnf CreateDBClusterSnapshotResponse' {..} =
    Prelude.rnf dbClusterSnapshot
      `Prelude.seq` Prelude.rnf httpStatus
