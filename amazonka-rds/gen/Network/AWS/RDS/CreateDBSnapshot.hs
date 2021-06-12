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
-- Module      : Network.AWS.RDS.CreateDBSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a DB instance. The source DB instance must be in
-- the @available@ or @storage-optimization@ state.
module Network.AWS.RDS.CreateDBSnapshot
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newCreateDBSnapshot' smart constructor.
data CreateDBSnapshot = CreateDBSnapshot'
  { tags :: Core.Maybe [Tag],
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
    dbSnapshotIdentifier :: Core.Text,
    -- | The identifier of the DB instance that you want to create the snapshot
    -- of.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DBInstance.
    dbInstanceIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'dbInstanceIdentifier'
  Core.Text ->
  CreateDBSnapshot
newCreateDBSnapshot
  pDBSnapshotIdentifier_
  pDBInstanceIdentifier_ =
    CreateDBSnapshot'
      { tags = Core.Nothing,
        dbSnapshotIdentifier = pDBSnapshotIdentifier_,
        dbInstanceIdentifier = pDBInstanceIdentifier_
      }

-- | Undocumented member.
createDBSnapshot_tags :: Lens.Lens' CreateDBSnapshot (Core.Maybe [Tag])
createDBSnapshot_tags = Lens.lens (\CreateDBSnapshot' {tags} -> tags) (\s@CreateDBSnapshot' {} a -> s {tags = a} :: CreateDBSnapshot) Core.. Lens.mapping Lens._Coerce

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
createDBSnapshot_dbSnapshotIdentifier :: Lens.Lens' CreateDBSnapshot Core.Text
createDBSnapshot_dbSnapshotIdentifier = Lens.lens (\CreateDBSnapshot' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@CreateDBSnapshot' {} a -> s {dbSnapshotIdentifier = a} :: CreateDBSnapshot)

-- | The identifier of the DB instance that you want to create the snapshot
-- of.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBInstance.
createDBSnapshot_dbInstanceIdentifier :: Lens.Lens' CreateDBSnapshot Core.Text
createDBSnapshot_dbInstanceIdentifier = Lens.lens (\CreateDBSnapshot' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@CreateDBSnapshot' {} a -> s {dbInstanceIdentifier = a} :: CreateDBSnapshot)

instance Core.AWSRequest CreateDBSnapshot where
  type
    AWSResponse CreateDBSnapshot =
      CreateDBSnapshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateDBSnapshotResult"
      ( \s h x ->
          CreateDBSnapshotResponse'
            Core.<$> (x Core..@? "DBSnapshot")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDBSnapshot

instance Core.NFData CreateDBSnapshot

instance Core.ToHeaders CreateDBSnapshot where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateDBSnapshot where
  toPath = Core.const "/"

instance Core.ToQuery CreateDBSnapshot where
  toQuery CreateDBSnapshot' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateDBSnapshot" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "Tags"
          Core.=: Core.toQuery (Core.toQueryList "Tag" Core.<$> tags),
        "DBSnapshotIdentifier" Core.=: dbSnapshotIdentifier,
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier
      ]

-- | /See:/ 'newCreateDBSnapshotResponse' smart constructor.
data CreateDBSnapshotResponse = CreateDBSnapshotResponse'
  { dbSnapshot :: Core.Maybe DBSnapshot,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateDBSnapshotResponse
newCreateDBSnapshotResponse pHttpStatus_ =
  CreateDBSnapshotResponse'
    { dbSnapshot =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createDBSnapshotResponse_dbSnapshot :: Lens.Lens' CreateDBSnapshotResponse (Core.Maybe DBSnapshot)
createDBSnapshotResponse_dbSnapshot = Lens.lens (\CreateDBSnapshotResponse' {dbSnapshot} -> dbSnapshot) (\s@CreateDBSnapshotResponse' {} a -> s {dbSnapshot = a} :: CreateDBSnapshotResponse)

-- | The response's http status code.
createDBSnapshotResponse_httpStatus :: Lens.Lens' CreateDBSnapshotResponse Core.Int
createDBSnapshotResponse_httpStatus = Lens.lens (\CreateDBSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateDBSnapshotResponse' {} a -> s {httpStatus = a} :: CreateDBSnapshotResponse)

instance Core.NFData CreateDBSnapshotResponse
