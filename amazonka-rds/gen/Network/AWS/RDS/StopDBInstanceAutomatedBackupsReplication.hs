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
-- Module      : Network.AWS.RDS.StopDBInstanceAutomatedBackupsReplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops automated backup replication for a DB instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_ReplicateBackups.html Replicating Automated Backups to Another AWS Region>
-- in the /Amazon RDS User Guide./
module Network.AWS.RDS.StopDBInstanceAutomatedBackupsReplication
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopDBInstanceAutomatedBackupsReplication' smart constructor.
data StopDBInstanceAutomatedBackupsReplication = StopDBInstanceAutomatedBackupsReplication'
  { -- | The Amazon Resource Name (ARN) of the source DB instance for which to
    -- stop replicating automated backups, for example,
    -- @arn:aws:rds:us-west-2:123456789012:db:mydatabase@.
    sourceDBInstanceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
stopDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn :: Lens.Lens' StopDBInstanceAutomatedBackupsReplication Core.Text
stopDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn = Lens.lens (\StopDBInstanceAutomatedBackupsReplication' {sourceDBInstanceArn} -> sourceDBInstanceArn) (\s@StopDBInstanceAutomatedBackupsReplication' {} a -> s {sourceDBInstanceArn = a} :: StopDBInstanceAutomatedBackupsReplication)

instance
  Core.AWSRequest
    StopDBInstanceAutomatedBackupsReplication
  where
  type
    AWSResponse
      StopDBInstanceAutomatedBackupsReplication =
      StopDBInstanceAutomatedBackupsReplicationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "StopDBInstanceAutomatedBackupsReplicationResult"
      ( \s h x ->
          StopDBInstanceAutomatedBackupsReplicationResponse'
            Core.<$> (x Core..@? "DBInstanceAutomatedBackup")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    StopDBInstanceAutomatedBackupsReplication

instance
  Core.NFData
    StopDBInstanceAutomatedBackupsReplication

instance
  Core.ToHeaders
    StopDBInstanceAutomatedBackupsReplication
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    StopDBInstanceAutomatedBackupsReplication
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    StopDBInstanceAutomatedBackupsReplication
  where
  toQuery
    StopDBInstanceAutomatedBackupsReplication' {..} =
      Core.mconcat
        [ "Action"
            Core.=: ( "StopDBInstanceAutomatedBackupsReplication" ::
                        Core.ByteString
                    ),
          "Version" Core.=: ("2014-10-31" :: Core.ByteString),
          "SourceDBInstanceArn" Core.=: sourceDBInstanceArn
        ]

-- | /See:/ 'newStopDBInstanceAutomatedBackupsReplicationResponse' smart constructor.
data StopDBInstanceAutomatedBackupsReplicationResponse = StopDBInstanceAutomatedBackupsReplicationResponse'
  { dbInstanceAutomatedBackup :: Core.Maybe DBInstanceAutomatedBackup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  StopDBInstanceAutomatedBackupsReplicationResponse
newStopDBInstanceAutomatedBackupsReplicationResponse
  pHttpStatus_ =
    StopDBInstanceAutomatedBackupsReplicationResponse'
      { dbInstanceAutomatedBackup =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Undocumented member.
stopDBInstanceAutomatedBackupsReplicationResponse_dbInstanceAutomatedBackup :: Lens.Lens' StopDBInstanceAutomatedBackupsReplicationResponse (Core.Maybe DBInstanceAutomatedBackup)
stopDBInstanceAutomatedBackupsReplicationResponse_dbInstanceAutomatedBackup = Lens.lens (\StopDBInstanceAutomatedBackupsReplicationResponse' {dbInstanceAutomatedBackup} -> dbInstanceAutomatedBackup) (\s@StopDBInstanceAutomatedBackupsReplicationResponse' {} a -> s {dbInstanceAutomatedBackup = a} :: StopDBInstanceAutomatedBackupsReplicationResponse)

-- | The response's http status code.
stopDBInstanceAutomatedBackupsReplicationResponse_httpStatus :: Lens.Lens' StopDBInstanceAutomatedBackupsReplicationResponse Core.Int
stopDBInstanceAutomatedBackupsReplicationResponse_httpStatus = Lens.lens (\StopDBInstanceAutomatedBackupsReplicationResponse' {httpStatus} -> httpStatus) (\s@StopDBInstanceAutomatedBackupsReplicationResponse' {} a -> s {httpStatus = a} :: StopDBInstanceAutomatedBackupsReplicationResponse)

instance
  Core.NFData
    StopDBInstanceAutomatedBackupsReplicationResponse
