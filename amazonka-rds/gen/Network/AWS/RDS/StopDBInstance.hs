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
-- Module      : Network.AWS.RDS.StopDBInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an Amazon RDS DB instance. When you stop a DB instance, Amazon RDS
-- retains the DB instance\'s metadata, including its endpoint, DB
-- parameter group, and option group membership. Amazon RDS also retains
-- the transaction logs so you can do a point-in-time restore if necessary.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_StopInstance.html Stopping an Amazon RDS DB Instance Temporarily>
-- in the /Amazon RDS User Guide./
--
-- This command doesn\'t apply to Aurora MySQL and Aurora PostgreSQL. For
-- Aurora clusters, use @StopDBCluster@ instead.
module Network.AWS.RDS.StopDBInstance
  ( -- * Creating a Request
    StopDBInstance (..),
    newStopDBInstance,

    -- * Request Lenses
    stopDBInstance_dbSnapshotIdentifier,
    stopDBInstance_dbInstanceIdentifier,

    -- * Destructuring the Response
    StopDBInstanceResponse (..),
    newStopDBInstanceResponse,

    -- * Response Lenses
    stopDBInstanceResponse_dbInstance,
    stopDBInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopDBInstance' smart constructor.
data StopDBInstance = StopDBInstance'
  { -- | The user-supplied instance identifier of the DB Snapshot created
    -- immediately before the DB instance is stopped.
    dbSnapshotIdentifier :: Core.Maybe Core.Text,
    -- | The user-supplied instance identifier.
    dbInstanceIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopDBInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSnapshotIdentifier', 'stopDBInstance_dbSnapshotIdentifier' - The user-supplied instance identifier of the DB Snapshot created
-- immediately before the DB instance is stopped.
--
-- 'dbInstanceIdentifier', 'stopDBInstance_dbInstanceIdentifier' - The user-supplied instance identifier.
newStopDBInstance ::
  -- | 'dbInstanceIdentifier'
  Core.Text ->
  StopDBInstance
newStopDBInstance pDBInstanceIdentifier_ =
  StopDBInstance'
    { dbSnapshotIdentifier =
        Core.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | The user-supplied instance identifier of the DB Snapshot created
-- immediately before the DB instance is stopped.
stopDBInstance_dbSnapshotIdentifier :: Lens.Lens' StopDBInstance (Core.Maybe Core.Text)
stopDBInstance_dbSnapshotIdentifier = Lens.lens (\StopDBInstance' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@StopDBInstance' {} a -> s {dbSnapshotIdentifier = a} :: StopDBInstance)

-- | The user-supplied instance identifier.
stopDBInstance_dbInstanceIdentifier :: Lens.Lens' StopDBInstance Core.Text
stopDBInstance_dbInstanceIdentifier = Lens.lens (\StopDBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@StopDBInstance' {} a -> s {dbInstanceIdentifier = a} :: StopDBInstance)

instance Core.AWSRequest StopDBInstance where
  type
    AWSResponse StopDBInstance =
      StopDBInstanceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "StopDBInstanceResult"
      ( \s h x ->
          StopDBInstanceResponse'
            Core.<$> (x Core..@? "DBInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopDBInstance

instance Core.NFData StopDBInstance

instance Core.ToHeaders StopDBInstance where
  toHeaders = Core.const Core.mempty

instance Core.ToPath StopDBInstance where
  toPath = Core.const "/"

instance Core.ToQuery StopDBInstance where
  toQuery StopDBInstance' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("StopDBInstance" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBSnapshotIdentifier" Core.=: dbSnapshotIdentifier,
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier
      ]

-- | /See:/ 'newStopDBInstanceResponse' smart constructor.
data StopDBInstanceResponse = StopDBInstanceResponse'
  { dbInstance :: Core.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopDBInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstance', 'stopDBInstanceResponse_dbInstance' - Undocumented member.
--
-- 'httpStatus', 'stopDBInstanceResponse_httpStatus' - The response's http status code.
newStopDBInstanceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopDBInstanceResponse
newStopDBInstanceResponse pHttpStatus_ =
  StopDBInstanceResponse'
    { dbInstance = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
stopDBInstanceResponse_dbInstance :: Lens.Lens' StopDBInstanceResponse (Core.Maybe DBInstance)
stopDBInstanceResponse_dbInstance = Lens.lens (\StopDBInstanceResponse' {dbInstance} -> dbInstance) (\s@StopDBInstanceResponse' {} a -> s {dbInstance = a} :: StopDBInstanceResponse)

-- | The response's http status code.
stopDBInstanceResponse_httpStatus :: Lens.Lens' StopDBInstanceResponse Core.Int
stopDBInstanceResponse_httpStatus = Lens.lens (\StopDBInstanceResponse' {httpStatus} -> httpStatus) (\s@StopDBInstanceResponse' {} a -> s {httpStatus = a} :: StopDBInstanceResponse)

instance Core.NFData StopDBInstanceResponse
