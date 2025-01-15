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
-- Module      : Amazonka.RDS.StopDBInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- This command doesn\'t apply to RDS Custom, Aurora MySQL, and Aurora
-- PostgreSQL. For Aurora clusters, use @StopDBCluster@ instead.
module Amazonka.RDS.StopDBInstance
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopDBInstance' smart constructor.
data StopDBInstance = StopDBInstance'
  { -- | The user-supplied instance identifier of the DB Snapshot created
    -- immediately before the DB instance is stopped.
    dbSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The user-supplied instance identifier.
    dbInstanceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  StopDBInstance
newStopDBInstance pDBInstanceIdentifier_ =
  StopDBInstance'
    { dbSnapshotIdentifier =
        Prelude.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | The user-supplied instance identifier of the DB Snapshot created
-- immediately before the DB instance is stopped.
stopDBInstance_dbSnapshotIdentifier :: Lens.Lens' StopDBInstance (Prelude.Maybe Prelude.Text)
stopDBInstance_dbSnapshotIdentifier = Lens.lens (\StopDBInstance' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@StopDBInstance' {} a -> s {dbSnapshotIdentifier = a} :: StopDBInstance)

-- | The user-supplied instance identifier.
stopDBInstance_dbInstanceIdentifier :: Lens.Lens' StopDBInstance Prelude.Text
stopDBInstance_dbInstanceIdentifier = Lens.lens (\StopDBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@StopDBInstance' {} a -> s {dbInstanceIdentifier = a} :: StopDBInstance)

instance Core.AWSRequest StopDBInstance where
  type
    AWSResponse StopDBInstance =
      StopDBInstanceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "StopDBInstanceResult"
      ( \s h x ->
          StopDBInstanceResponse'
            Prelude.<$> (x Data..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopDBInstance where
  hashWithSalt _salt StopDBInstance' {..} =
    _salt
      `Prelude.hashWithSalt` dbSnapshotIdentifier
      `Prelude.hashWithSalt` dbInstanceIdentifier

instance Prelude.NFData StopDBInstance where
  rnf StopDBInstance' {..} =
    Prelude.rnf dbSnapshotIdentifier `Prelude.seq`
      Prelude.rnf dbInstanceIdentifier

instance Data.ToHeaders StopDBInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath StopDBInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery StopDBInstance where
  toQuery StopDBInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("StopDBInstance" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBSnapshotIdentifier" Data.=: dbSnapshotIdentifier,
        "DBInstanceIdentifier" Data.=: dbInstanceIdentifier
      ]

-- | /See:/ 'newStopDBInstanceResponse' smart constructor.
data StopDBInstanceResponse = StopDBInstanceResponse'
  { dbInstance :: Prelude.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StopDBInstanceResponse
newStopDBInstanceResponse pHttpStatus_ =
  StopDBInstanceResponse'
    { dbInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
stopDBInstanceResponse_dbInstance :: Lens.Lens' StopDBInstanceResponse (Prelude.Maybe DBInstance)
stopDBInstanceResponse_dbInstance = Lens.lens (\StopDBInstanceResponse' {dbInstance} -> dbInstance) (\s@StopDBInstanceResponse' {} a -> s {dbInstance = a} :: StopDBInstanceResponse)

-- | The response's http status code.
stopDBInstanceResponse_httpStatus :: Lens.Lens' StopDBInstanceResponse Prelude.Int
stopDBInstanceResponse_httpStatus = Lens.lens (\StopDBInstanceResponse' {httpStatus} -> httpStatus) (\s@StopDBInstanceResponse' {} a -> s {httpStatus = a} :: StopDBInstanceResponse)

instance Prelude.NFData StopDBInstanceResponse where
  rnf StopDBInstanceResponse' {..} =
    Prelude.rnf dbInstance `Prelude.seq`
      Prelude.rnf httpStatus
