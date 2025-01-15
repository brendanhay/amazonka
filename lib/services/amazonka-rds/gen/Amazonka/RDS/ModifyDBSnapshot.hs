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
-- Module      : Amazonka.RDS.ModifyDBSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a manual DB snapshot with a new engine version. The snapshot can
-- be encrypted or unencrypted, but not shared or public.
--
-- Amazon RDS supports upgrading DB snapshots for MySQL, PostgreSQL, and
-- Oracle. This command doesn\'t apply to RDS Custom.
module Amazonka.RDS.ModifyDBSnapshot
  ( -- * Creating a Request
    ModifyDBSnapshot (..),
    newModifyDBSnapshot,

    -- * Request Lenses
    modifyDBSnapshot_engineVersion,
    modifyDBSnapshot_optionGroupName,
    modifyDBSnapshot_dbSnapshotIdentifier,

    -- * Destructuring the Response
    ModifyDBSnapshotResponse (..),
    newModifyDBSnapshotResponse,

    -- * Response Lenses
    modifyDBSnapshotResponse_dbSnapshot,
    modifyDBSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyDBSnapshot' smart constructor.
data ModifyDBSnapshot = ModifyDBSnapshot'
  { -- | The engine version to upgrade the DB snapshot to.
    --
    -- The following are the database engines and engine versions that are
    -- available when you upgrade a DB snapshot.
    --
    -- __MySQL__
    --
    -- -   @5.5.46@ (supported for 5.1 DB snapshots)
    --
    -- __Oracle__
    --
    -- -   @12.1.0.2.v8@ (supported for 12.1.0.1 DB snapshots)
    --
    -- -   @11.2.0.4.v12@ (supported for 11.2.0.2 DB snapshots)
    --
    -- -   @11.2.0.4.v11@ (supported for 11.2.0.3 DB snapshots)
    --
    -- __PostgreSQL__
    --
    -- For the list of engine versions that are available for upgrading a DB
    -- snapshot, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.PostgreSQL.html#USER_UpgradeDBInstance.PostgreSQL.MajorVersion Upgrading the PostgreSQL DB Engine for Amazon RDS>.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The option group to identify with the upgraded DB snapshot.
    --
    -- You can specify this parameter when you upgrade an Oracle DB snapshot.
    -- The same option group considerations apply when upgrading a DB snapshot
    -- as when upgrading a DB instance. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Oracle.html#USER_UpgradeDBInstance.Oracle.OGPG.OG Option group considerations>
    -- in the /Amazon RDS User Guide./
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the DB snapshot to modify.
    dbSnapshotIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'modifyDBSnapshot_engineVersion' - The engine version to upgrade the DB snapshot to.
--
-- The following are the database engines and engine versions that are
-- available when you upgrade a DB snapshot.
--
-- __MySQL__
--
-- -   @5.5.46@ (supported for 5.1 DB snapshots)
--
-- __Oracle__
--
-- -   @12.1.0.2.v8@ (supported for 12.1.0.1 DB snapshots)
--
-- -   @11.2.0.4.v12@ (supported for 11.2.0.2 DB snapshots)
--
-- -   @11.2.0.4.v11@ (supported for 11.2.0.3 DB snapshots)
--
-- __PostgreSQL__
--
-- For the list of engine versions that are available for upgrading a DB
-- snapshot, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.PostgreSQL.html#USER_UpgradeDBInstance.PostgreSQL.MajorVersion Upgrading the PostgreSQL DB Engine for Amazon RDS>.
--
-- 'optionGroupName', 'modifyDBSnapshot_optionGroupName' - The option group to identify with the upgraded DB snapshot.
--
-- You can specify this parameter when you upgrade an Oracle DB snapshot.
-- The same option group considerations apply when upgrading a DB snapshot
-- as when upgrading a DB instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Oracle.html#USER_UpgradeDBInstance.Oracle.OGPG.OG Option group considerations>
-- in the /Amazon RDS User Guide./
--
-- 'dbSnapshotIdentifier', 'modifyDBSnapshot_dbSnapshotIdentifier' - The identifier of the DB snapshot to modify.
newModifyDBSnapshot ::
  -- | 'dbSnapshotIdentifier'
  Prelude.Text ->
  ModifyDBSnapshot
newModifyDBSnapshot pDBSnapshotIdentifier_ =
  ModifyDBSnapshot'
    { engineVersion = Prelude.Nothing,
      optionGroupName = Prelude.Nothing,
      dbSnapshotIdentifier = pDBSnapshotIdentifier_
    }

-- | The engine version to upgrade the DB snapshot to.
--
-- The following are the database engines and engine versions that are
-- available when you upgrade a DB snapshot.
--
-- __MySQL__
--
-- -   @5.5.46@ (supported for 5.1 DB snapshots)
--
-- __Oracle__
--
-- -   @12.1.0.2.v8@ (supported for 12.1.0.1 DB snapshots)
--
-- -   @11.2.0.4.v12@ (supported for 11.2.0.2 DB snapshots)
--
-- -   @11.2.0.4.v11@ (supported for 11.2.0.3 DB snapshots)
--
-- __PostgreSQL__
--
-- For the list of engine versions that are available for upgrading a DB
-- snapshot, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.PostgreSQL.html#USER_UpgradeDBInstance.PostgreSQL.MajorVersion Upgrading the PostgreSQL DB Engine for Amazon RDS>.
modifyDBSnapshot_engineVersion :: Lens.Lens' ModifyDBSnapshot (Prelude.Maybe Prelude.Text)
modifyDBSnapshot_engineVersion = Lens.lens (\ModifyDBSnapshot' {engineVersion} -> engineVersion) (\s@ModifyDBSnapshot' {} a -> s {engineVersion = a} :: ModifyDBSnapshot)

-- | The option group to identify with the upgraded DB snapshot.
--
-- You can specify this parameter when you upgrade an Oracle DB snapshot.
-- The same option group considerations apply when upgrading a DB snapshot
-- as when upgrading a DB instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Oracle.html#USER_UpgradeDBInstance.Oracle.OGPG.OG Option group considerations>
-- in the /Amazon RDS User Guide./
modifyDBSnapshot_optionGroupName :: Lens.Lens' ModifyDBSnapshot (Prelude.Maybe Prelude.Text)
modifyDBSnapshot_optionGroupName = Lens.lens (\ModifyDBSnapshot' {optionGroupName} -> optionGroupName) (\s@ModifyDBSnapshot' {} a -> s {optionGroupName = a} :: ModifyDBSnapshot)

-- | The identifier of the DB snapshot to modify.
modifyDBSnapshot_dbSnapshotIdentifier :: Lens.Lens' ModifyDBSnapshot Prelude.Text
modifyDBSnapshot_dbSnapshotIdentifier = Lens.lens (\ModifyDBSnapshot' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@ModifyDBSnapshot' {} a -> s {dbSnapshotIdentifier = a} :: ModifyDBSnapshot)

instance Core.AWSRequest ModifyDBSnapshot where
  type
    AWSResponse ModifyDBSnapshot =
      ModifyDBSnapshotResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyDBSnapshotResult"
      ( \s h x ->
          ModifyDBSnapshotResponse'
            Prelude.<$> (x Data..@? "DBSnapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyDBSnapshot where
  hashWithSalt _salt ModifyDBSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` dbSnapshotIdentifier

instance Prelude.NFData ModifyDBSnapshot where
  rnf ModifyDBSnapshot' {..} =
    Prelude.rnf engineVersion `Prelude.seq`
      Prelude.rnf optionGroupName `Prelude.seq`
        Prelude.rnf dbSnapshotIdentifier

instance Data.ToHeaders ModifyDBSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyDBSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyDBSnapshot where
  toQuery ModifyDBSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyDBSnapshot" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "EngineVersion" Data.=: engineVersion,
        "OptionGroupName" Data.=: optionGroupName,
        "DBSnapshotIdentifier" Data.=: dbSnapshotIdentifier
      ]

-- | /See:/ 'newModifyDBSnapshotResponse' smart constructor.
data ModifyDBSnapshotResponse = ModifyDBSnapshotResponse'
  { dbSnapshot :: Prelude.Maybe DBSnapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSnapshot', 'modifyDBSnapshotResponse_dbSnapshot' - Undocumented member.
--
-- 'httpStatus', 'modifyDBSnapshotResponse_httpStatus' - The response's http status code.
newModifyDBSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyDBSnapshotResponse
newModifyDBSnapshotResponse pHttpStatus_ =
  ModifyDBSnapshotResponse'
    { dbSnapshot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyDBSnapshotResponse_dbSnapshot :: Lens.Lens' ModifyDBSnapshotResponse (Prelude.Maybe DBSnapshot)
modifyDBSnapshotResponse_dbSnapshot = Lens.lens (\ModifyDBSnapshotResponse' {dbSnapshot} -> dbSnapshot) (\s@ModifyDBSnapshotResponse' {} a -> s {dbSnapshot = a} :: ModifyDBSnapshotResponse)

-- | The response's http status code.
modifyDBSnapshotResponse_httpStatus :: Lens.Lens' ModifyDBSnapshotResponse Prelude.Int
modifyDBSnapshotResponse_httpStatus = Lens.lens (\ModifyDBSnapshotResponse' {httpStatus} -> httpStatus) (\s@ModifyDBSnapshotResponse' {} a -> s {httpStatus = a} :: ModifyDBSnapshotResponse)

instance Prelude.NFData ModifyDBSnapshotResponse where
  rnf ModifyDBSnapshotResponse' {..} =
    Prelude.rnf dbSnapshot `Prelude.seq`
      Prelude.rnf httpStatus
