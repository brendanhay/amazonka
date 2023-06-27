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
-- Module      : Amazonka.RDS.PromoteReadReplica
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Promotes a read replica DB instance to a standalone DB instance.
--
-- -   Backup duration is a function of the amount of changes to the
--     database since the previous backup. If you plan to promote a read
--     replica to a standalone instance, we recommend that you enable
--     backups and complete at least one backup prior to promotion. In
--     addition, a read replica cannot be promoted to a standalone instance
--     when it is in the @backing-up@ status. If you have enabled backups
--     on your read replica, configure the automated backup window so that
--     daily backups do not interfere with read replica promotion.
--
-- -   This command doesn\'t apply to Aurora MySQL, Aurora PostgreSQL, or
--     RDS Custom.
module Amazonka.RDS.PromoteReadReplica
  ( -- * Creating a Request
    PromoteReadReplica (..),
    newPromoteReadReplica,

    -- * Request Lenses
    promoteReadReplica_backupRetentionPeriod,
    promoteReadReplica_preferredBackupWindow,
    promoteReadReplica_dbInstanceIdentifier,

    -- * Destructuring the Response
    PromoteReadReplicaResponse (..),
    newPromoteReadReplicaResponse,

    -- * Response Lenses
    promoteReadReplicaResponse_dbInstance,
    promoteReadReplicaResponse_httpStatus,
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
-- /See:/ 'newPromoteReadReplica' smart constructor.
data PromoteReadReplica = PromoteReadReplica'
  { -- | The number of days for which automated backups are retained. Setting
    -- this parameter to a positive number enables backups. Setting this
    -- parameter to 0 disables automated backups.
    --
    -- Default: 1
    --
    -- Constraints:
    --
    -- -   Must be a value from 0 to 35.
    --
    -- -   Can\'t be set to 0 if the DB instance is a source to read replicas.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The daily time range during which automated backups are created if
    -- automated backups are enabled, using the @BackupRetentionPeriod@
    -- parameter.
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Web Services Region. To see the time
    -- blocks available, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window>
    -- in the /Amazon RDS User Guide./
    --
    -- Constraints:
    --
    -- -   Must be in the format @hh24:mi-hh24:mi@.
    --
    -- -   Must be in Universal Coordinated Time (UTC).
    --
    -- -   Must not conflict with the preferred maintenance window.
    --
    -- -   Must be at least 30 minutes.
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | The DB instance identifier. This value is stored as a lowercase string.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing read replica DB instance.
    --
    -- Example: @mydbinstance@
    dbInstanceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PromoteReadReplica' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupRetentionPeriod', 'promoteReadReplica_backupRetentionPeriod' - The number of days for which automated backups are retained. Setting
-- this parameter to a positive number enables backups. Setting this
-- parameter to 0 disables automated backups.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 0 to 35.
--
-- -   Can\'t be set to 0 if the DB instance is a source to read replicas.
--
-- 'preferredBackupWindow', 'promoteReadReplica_preferredBackupWindow' - The daily time range during which automated backups are created if
-- automated backups are enabled, using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region. To see the time
-- blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window>
-- in the /Amazon RDS User Guide./
--
-- Constraints:
--
-- -   Must be in the format @hh24:mi-hh24:mi@.
--
-- -   Must be in Universal Coordinated Time (UTC).
--
-- -   Must not conflict with the preferred maintenance window.
--
-- -   Must be at least 30 minutes.
--
-- 'dbInstanceIdentifier', 'promoteReadReplica_dbInstanceIdentifier' - The DB instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must match the identifier of an existing read replica DB instance.
--
-- Example: @mydbinstance@
newPromoteReadReplica ::
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  PromoteReadReplica
newPromoteReadReplica pDBInstanceIdentifier_ =
  PromoteReadReplica'
    { backupRetentionPeriod =
        Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | The number of days for which automated backups are retained. Setting
-- this parameter to a positive number enables backups. Setting this
-- parameter to 0 disables automated backups.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 0 to 35.
--
-- -   Can\'t be set to 0 if the DB instance is a source to read replicas.
promoteReadReplica_backupRetentionPeriod :: Lens.Lens' PromoteReadReplica (Prelude.Maybe Prelude.Int)
promoteReadReplica_backupRetentionPeriod = Lens.lens (\PromoteReadReplica' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@PromoteReadReplica' {} a -> s {backupRetentionPeriod = a} :: PromoteReadReplica)

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region. To see the time
-- blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window>
-- in the /Amazon RDS User Guide./
--
-- Constraints:
--
-- -   Must be in the format @hh24:mi-hh24:mi@.
--
-- -   Must be in Universal Coordinated Time (UTC).
--
-- -   Must not conflict with the preferred maintenance window.
--
-- -   Must be at least 30 minutes.
promoteReadReplica_preferredBackupWindow :: Lens.Lens' PromoteReadReplica (Prelude.Maybe Prelude.Text)
promoteReadReplica_preferredBackupWindow = Lens.lens (\PromoteReadReplica' {preferredBackupWindow} -> preferredBackupWindow) (\s@PromoteReadReplica' {} a -> s {preferredBackupWindow = a} :: PromoteReadReplica)

-- | The DB instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must match the identifier of an existing read replica DB instance.
--
-- Example: @mydbinstance@
promoteReadReplica_dbInstanceIdentifier :: Lens.Lens' PromoteReadReplica Prelude.Text
promoteReadReplica_dbInstanceIdentifier = Lens.lens (\PromoteReadReplica' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@PromoteReadReplica' {} a -> s {dbInstanceIdentifier = a} :: PromoteReadReplica)

instance Core.AWSRequest PromoteReadReplica where
  type
    AWSResponse PromoteReadReplica =
      PromoteReadReplicaResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "PromoteReadReplicaResult"
      ( \s h x ->
          PromoteReadReplicaResponse'
            Prelude.<$> (x Data..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PromoteReadReplica where
  hashWithSalt _salt PromoteReadReplica' {..} =
    _salt
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` dbInstanceIdentifier

instance Prelude.NFData PromoteReadReplica where
  rnf PromoteReadReplica' {..} =
    Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf preferredBackupWindow
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier

instance Data.ToHeaders PromoteReadReplica where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath PromoteReadReplica where
  toPath = Prelude.const "/"

instance Data.ToQuery PromoteReadReplica where
  toQuery PromoteReadReplica' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("PromoteReadReplica" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "BackupRetentionPeriod"
          Data.=: backupRetentionPeriod,
        "PreferredBackupWindow"
          Data.=: preferredBackupWindow,
        "DBInstanceIdentifier" Data.=: dbInstanceIdentifier
      ]

-- | /See:/ 'newPromoteReadReplicaResponse' smart constructor.
data PromoteReadReplicaResponse = PromoteReadReplicaResponse'
  { dbInstance :: Prelude.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PromoteReadReplicaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstance', 'promoteReadReplicaResponse_dbInstance' - Undocumented member.
--
-- 'httpStatus', 'promoteReadReplicaResponse_httpStatus' - The response's http status code.
newPromoteReadReplicaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PromoteReadReplicaResponse
newPromoteReadReplicaResponse pHttpStatus_ =
  PromoteReadReplicaResponse'
    { dbInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
promoteReadReplicaResponse_dbInstance :: Lens.Lens' PromoteReadReplicaResponse (Prelude.Maybe DBInstance)
promoteReadReplicaResponse_dbInstance = Lens.lens (\PromoteReadReplicaResponse' {dbInstance} -> dbInstance) (\s@PromoteReadReplicaResponse' {} a -> s {dbInstance = a} :: PromoteReadReplicaResponse)

-- | The response's http status code.
promoteReadReplicaResponse_httpStatus :: Lens.Lens' PromoteReadReplicaResponse Prelude.Int
promoteReadReplicaResponse_httpStatus = Lens.lens (\PromoteReadReplicaResponse' {httpStatus} -> httpStatus) (\s@PromoteReadReplicaResponse' {} a -> s {httpStatus = a} :: PromoteReadReplicaResponse)

instance Prelude.NFData PromoteReadReplicaResponse where
  rnf PromoteReadReplicaResponse' {..} =
    Prelude.rnf dbInstance
      `Prelude.seq` Prelude.rnf httpStatus
