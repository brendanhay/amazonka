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
-- Module      : Amazonka.RDS.CreateDBSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a DB instance. The source DB instance must be in
-- the @available@ or @storage-optimization@ state.
module Amazonka.RDS.CreateDBSnapshot
  ( -- * Creating a Request
    CreateDBSnapshot (..),
    newCreateDBSnapshot,

    -- * Request Lenses
    createDBSnapshot_tags,
    createDBSnapshot_dbSnapshotIdentifier,
    createDBSnapshot_dbInstanceIdentifier,

    -- * Destructuring the Response
    CreateDBSnapshotResponse (..),
    newCreateDBSnapshotResponse,

    -- * Response Lenses
    createDBSnapshotResponse_dbSnapshot,
    createDBSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateDBSnapshot' smart constructor.
data CreateDBSnapshot = CreateDBSnapshot'
  { tags :: Prelude.Maybe [Tag],
    -- | The identifier for the DB snapshot.
    --
    -- Constraints:
    --
    -- -   Can\'t be null, empty, or blank
    --
    -- -   Must contain from 1 to 255 letters, numbers, or hyphens
    --
    -- -   First character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    --
    -- Example: @my-snapshot-id@
    dbSnapshotIdentifier :: Prelude.Text,
    -- | The identifier of the DB instance that you want to create the snapshot
    -- of.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DBInstance.
    dbInstanceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDBSnapshot_tags' - Undocumented member.
--
-- 'dbSnapshotIdentifier', 'createDBSnapshot_dbSnapshotIdentifier' - The identifier for the DB snapshot.
--
-- Constraints:
--
-- -   Can\'t be null, empty, or blank
--
-- -   Must contain from 1 to 255 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-snapshot-id@
--
-- 'dbInstanceIdentifier', 'createDBSnapshot_dbInstanceIdentifier' - The identifier of the DB instance that you want to create the snapshot
-- of.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBInstance.
newCreateDBSnapshot ::
  -- | 'dbSnapshotIdentifier'
  Prelude.Text ->
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  CreateDBSnapshot
newCreateDBSnapshot
  pDBSnapshotIdentifier_
  pDBInstanceIdentifier_ =
    CreateDBSnapshot'
      { tags = Prelude.Nothing,
        dbSnapshotIdentifier = pDBSnapshotIdentifier_,
        dbInstanceIdentifier = pDBInstanceIdentifier_
      }

-- | Undocumented member.
createDBSnapshot_tags :: Lens.Lens' CreateDBSnapshot (Prelude.Maybe [Tag])
createDBSnapshot_tags = Lens.lens (\CreateDBSnapshot' {tags} -> tags) (\s@CreateDBSnapshot' {} a -> s {tags = a} :: CreateDBSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the DB snapshot.
--
-- Constraints:
--
-- -   Can\'t be null, empty, or blank
--
-- -   Must contain from 1 to 255 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-snapshot-id@
createDBSnapshot_dbSnapshotIdentifier :: Lens.Lens' CreateDBSnapshot Prelude.Text
createDBSnapshot_dbSnapshotIdentifier = Lens.lens (\CreateDBSnapshot' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@CreateDBSnapshot' {} a -> s {dbSnapshotIdentifier = a} :: CreateDBSnapshot)

-- | The identifier of the DB instance that you want to create the snapshot
-- of.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBInstance.
createDBSnapshot_dbInstanceIdentifier :: Lens.Lens' CreateDBSnapshot Prelude.Text
createDBSnapshot_dbInstanceIdentifier = Lens.lens (\CreateDBSnapshot' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@CreateDBSnapshot' {} a -> s {dbInstanceIdentifier = a} :: CreateDBSnapshot)

instance Core.AWSRequest CreateDBSnapshot where
  type
    AWSResponse CreateDBSnapshot =
      CreateDBSnapshotResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateDBSnapshotResult"
      ( \s h x ->
          CreateDBSnapshotResponse'
            Prelude.<$> (x Data..@? "DBSnapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDBSnapshot where
  hashWithSalt _salt CreateDBSnapshot' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` dbSnapshotIdentifier
      `Prelude.hashWithSalt` dbInstanceIdentifier

instance Prelude.NFData CreateDBSnapshot where
  rnf CreateDBSnapshot' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf dbSnapshotIdentifier
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier

instance Data.ToHeaders CreateDBSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateDBSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDBSnapshot where
  toQuery CreateDBSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateDBSnapshot" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "DBSnapshotIdentifier" Data.=: dbSnapshotIdentifier,
        "DBInstanceIdentifier" Data.=: dbInstanceIdentifier
      ]

-- | /See:/ 'newCreateDBSnapshotResponse' smart constructor.
data CreateDBSnapshotResponse = CreateDBSnapshotResponse'
  { dbSnapshot :: Prelude.Maybe DBSnapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSnapshot', 'createDBSnapshotResponse_dbSnapshot' - Undocumented member.
--
-- 'httpStatus', 'createDBSnapshotResponse_httpStatus' - The response's http status code.
newCreateDBSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDBSnapshotResponse
newCreateDBSnapshotResponse pHttpStatus_ =
  CreateDBSnapshotResponse'
    { dbSnapshot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createDBSnapshotResponse_dbSnapshot :: Lens.Lens' CreateDBSnapshotResponse (Prelude.Maybe DBSnapshot)
createDBSnapshotResponse_dbSnapshot = Lens.lens (\CreateDBSnapshotResponse' {dbSnapshot} -> dbSnapshot) (\s@CreateDBSnapshotResponse' {} a -> s {dbSnapshot = a} :: CreateDBSnapshotResponse)

-- | The response's http status code.
createDBSnapshotResponse_httpStatus :: Lens.Lens' CreateDBSnapshotResponse Prelude.Int
createDBSnapshotResponse_httpStatus = Lens.lens (\CreateDBSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateDBSnapshotResponse' {} a -> s {httpStatus = a} :: CreateDBSnapshotResponse)

instance Prelude.NFData CreateDBSnapshotResponse where
  rnf CreateDBSnapshotResponse' {..} =
    Prelude.rnf dbSnapshot
      `Prelude.seq` Prelude.rnf httpStatus
