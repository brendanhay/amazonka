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
-- Module      : Amazonka.RDS.StopDBInstanceAutomatedBackupsReplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops automated backup replication for a DB instance.
--
-- This command doesn\'t apply to RDS Custom, Aurora MySQL, and Aurora
-- PostgreSQL.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_ReplicateBackups.html Replicating Automated Backups to Another Amazon Web Services Region>
-- in the /Amazon RDS User Guide./
module Amazonka.RDS.StopDBInstanceAutomatedBackupsReplication
  ( -- * Creating a Request
    StopDBInstanceAutomatedBackupsReplication (..),
    newStopDBInstanceAutomatedBackupsReplication,

    -- * Request Lenses
    stopDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn,

    -- * Destructuring the Response
    StopDBInstanceAutomatedBackupsReplicationResponse (..),
    newStopDBInstanceAutomatedBackupsReplicationResponse,

    -- * Response Lenses
    stopDBInstanceAutomatedBackupsReplicationResponse_dbInstanceAutomatedBackup,
    stopDBInstanceAutomatedBackupsReplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopDBInstanceAutomatedBackupsReplication' smart constructor.
data StopDBInstanceAutomatedBackupsReplication = StopDBInstanceAutomatedBackupsReplication'
  { -- | The Amazon Resource Name (ARN) of the source DB instance for which to
    -- stop replicating automated backups, for example,
    -- @arn:aws:rds:us-west-2:123456789012:db:mydatabase@.
    sourceDBInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopDBInstanceAutomatedBackupsReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceDBInstanceArn', 'stopDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn' - The Amazon Resource Name (ARN) of the source DB instance for which to
-- stop replicating automated backups, for example,
-- @arn:aws:rds:us-west-2:123456789012:db:mydatabase@.
newStopDBInstanceAutomatedBackupsReplication ::
  -- | 'sourceDBInstanceArn'
  Prelude.Text ->
  StopDBInstanceAutomatedBackupsReplication
newStopDBInstanceAutomatedBackupsReplication
  pSourceDBInstanceArn_ =
    StopDBInstanceAutomatedBackupsReplication'
      { sourceDBInstanceArn =
          pSourceDBInstanceArn_
      }

-- | The Amazon Resource Name (ARN) of the source DB instance for which to
-- stop replicating automated backups, for example,
-- @arn:aws:rds:us-west-2:123456789012:db:mydatabase@.
stopDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn :: Lens.Lens' StopDBInstanceAutomatedBackupsReplication Prelude.Text
stopDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn = Lens.lens (\StopDBInstanceAutomatedBackupsReplication' {sourceDBInstanceArn} -> sourceDBInstanceArn) (\s@StopDBInstanceAutomatedBackupsReplication' {} a -> s {sourceDBInstanceArn = a} :: StopDBInstanceAutomatedBackupsReplication)

instance
  Core.AWSRequest
    StopDBInstanceAutomatedBackupsReplication
  where
  type
    AWSResponse
      StopDBInstanceAutomatedBackupsReplication =
      StopDBInstanceAutomatedBackupsReplicationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "StopDBInstanceAutomatedBackupsReplicationResult"
      ( \s h x ->
          StopDBInstanceAutomatedBackupsReplicationResponse'
            Prelude.<$> (x Core..@? "DBInstanceAutomatedBackup")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StopDBInstanceAutomatedBackupsReplication
  where
  hashWithSalt
    _salt
    StopDBInstanceAutomatedBackupsReplication' {..} =
      _salt `Prelude.hashWithSalt` sourceDBInstanceArn

instance
  Prelude.NFData
    StopDBInstanceAutomatedBackupsReplication
  where
  rnf StopDBInstanceAutomatedBackupsReplication' {..} =
    Prelude.rnf sourceDBInstanceArn

instance
  Core.ToHeaders
    StopDBInstanceAutomatedBackupsReplication
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    StopDBInstanceAutomatedBackupsReplication
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    StopDBInstanceAutomatedBackupsReplication
  where
  toQuery
    StopDBInstanceAutomatedBackupsReplication' {..} =
      Prelude.mconcat
        [ "Action"
            Core.=: ( "StopDBInstanceAutomatedBackupsReplication" ::
                        Prelude.ByteString
                    ),
          "Version"
            Core.=: ("2014-10-31" :: Prelude.ByteString),
          "SourceDBInstanceArn" Core.=: sourceDBInstanceArn
        ]

-- | /See:/ 'newStopDBInstanceAutomatedBackupsReplicationResponse' smart constructor.
data StopDBInstanceAutomatedBackupsReplicationResponse = StopDBInstanceAutomatedBackupsReplicationResponse'
  { dbInstanceAutomatedBackup :: Prelude.Maybe DBInstanceAutomatedBackup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopDBInstanceAutomatedBackupsReplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstanceAutomatedBackup', 'stopDBInstanceAutomatedBackupsReplicationResponse_dbInstanceAutomatedBackup' - Undocumented member.
--
-- 'httpStatus', 'stopDBInstanceAutomatedBackupsReplicationResponse_httpStatus' - The response's http status code.
newStopDBInstanceAutomatedBackupsReplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopDBInstanceAutomatedBackupsReplicationResponse
newStopDBInstanceAutomatedBackupsReplicationResponse
  pHttpStatus_ =
    StopDBInstanceAutomatedBackupsReplicationResponse'
      { dbInstanceAutomatedBackup =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Undocumented member.
stopDBInstanceAutomatedBackupsReplicationResponse_dbInstanceAutomatedBackup :: Lens.Lens' StopDBInstanceAutomatedBackupsReplicationResponse (Prelude.Maybe DBInstanceAutomatedBackup)
stopDBInstanceAutomatedBackupsReplicationResponse_dbInstanceAutomatedBackup = Lens.lens (\StopDBInstanceAutomatedBackupsReplicationResponse' {dbInstanceAutomatedBackup} -> dbInstanceAutomatedBackup) (\s@StopDBInstanceAutomatedBackupsReplicationResponse' {} a -> s {dbInstanceAutomatedBackup = a} :: StopDBInstanceAutomatedBackupsReplicationResponse)

-- | The response's http status code.
stopDBInstanceAutomatedBackupsReplicationResponse_httpStatus :: Lens.Lens' StopDBInstanceAutomatedBackupsReplicationResponse Prelude.Int
stopDBInstanceAutomatedBackupsReplicationResponse_httpStatus = Lens.lens (\StopDBInstanceAutomatedBackupsReplicationResponse' {httpStatus} -> httpStatus) (\s@StopDBInstanceAutomatedBackupsReplicationResponse' {} a -> s {httpStatus = a} :: StopDBInstanceAutomatedBackupsReplicationResponse)

instance
  Prelude.NFData
    StopDBInstanceAutomatedBackupsReplicationResponse
  where
  rnf
    StopDBInstanceAutomatedBackupsReplicationResponse' {..} =
      Prelude.rnf dbInstanceAutomatedBackup
        `Prelude.seq` Prelude.rnf httpStatus
