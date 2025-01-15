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
-- Module      : Amazonka.RDS.RebootDBInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You might need to reboot your DB instance, usually for maintenance
-- reasons. For example, if you make certain modifications, or if you
-- change the DB parameter group associated with the DB instance, you must
-- reboot the instance for the changes to take effect.
--
-- Rebooting a DB instance restarts the database engine service. Rebooting
-- a DB instance results in a momentary outage, during which the DB
-- instance status is set to rebooting.
--
-- For more information about rebooting, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_RebootInstance.html Rebooting a DB Instance>
-- in the /Amazon RDS User Guide./
--
-- This command doesn\'t apply to RDS Custom.
--
-- If your DB instance is part of a Multi-AZ DB cluster, you can reboot the
-- DB cluster with the @RebootDBCluster@ operation.
module Amazonka.RDS.RebootDBInstance
  ( -- * Creating a Request
    RebootDBInstance (..),
    newRebootDBInstance,

    -- * Request Lenses
    rebootDBInstance_forceFailover,
    rebootDBInstance_dbInstanceIdentifier,

    -- * Destructuring the Response
    RebootDBInstanceResponse (..),
    newRebootDBInstanceResponse,

    -- * Response Lenses
    rebootDBInstanceResponse_dbInstance,
    rebootDBInstanceResponse_httpStatus,
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
-- /See:/ 'newRebootDBInstance' smart constructor.
data RebootDBInstance = RebootDBInstance'
  { -- | A value that indicates whether the reboot is conducted through a
    -- Multi-AZ failover.
    --
    -- Constraint: You can\'t enable force failover if the instance isn\'t
    -- configured for Multi-AZ.
    forceFailover :: Prelude.Maybe Prelude.Bool,
    -- | The DB instance identifier. This parameter is stored as a lowercase
    -- string.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DBInstance.
    dbInstanceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootDBInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceFailover', 'rebootDBInstance_forceFailover' - A value that indicates whether the reboot is conducted through a
-- Multi-AZ failover.
--
-- Constraint: You can\'t enable force failover if the instance isn\'t
-- configured for Multi-AZ.
--
-- 'dbInstanceIdentifier', 'rebootDBInstance_dbInstanceIdentifier' - The DB instance identifier. This parameter is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBInstance.
newRebootDBInstance ::
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  RebootDBInstance
newRebootDBInstance pDBInstanceIdentifier_ =
  RebootDBInstance'
    { forceFailover = Prelude.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | A value that indicates whether the reboot is conducted through a
-- Multi-AZ failover.
--
-- Constraint: You can\'t enable force failover if the instance isn\'t
-- configured for Multi-AZ.
rebootDBInstance_forceFailover :: Lens.Lens' RebootDBInstance (Prelude.Maybe Prelude.Bool)
rebootDBInstance_forceFailover = Lens.lens (\RebootDBInstance' {forceFailover} -> forceFailover) (\s@RebootDBInstance' {} a -> s {forceFailover = a} :: RebootDBInstance)

-- | The DB instance identifier. This parameter is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBInstance.
rebootDBInstance_dbInstanceIdentifier :: Lens.Lens' RebootDBInstance Prelude.Text
rebootDBInstance_dbInstanceIdentifier = Lens.lens (\RebootDBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@RebootDBInstance' {} a -> s {dbInstanceIdentifier = a} :: RebootDBInstance)

instance Core.AWSRequest RebootDBInstance where
  type
    AWSResponse RebootDBInstance =
      RebootDBInstanceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RebootDBInstanceResult"
      ( \s h x ->
          RebootDBInstanceResponse'
            Prelude.<$> (x Data..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RebootDBInstance where
  hashWithSalt _salt RebootDBInstance' {..} =
    _salt
      `Prelude.hashWithSalt` forceFailover
      `Prelude.hashWithSalt` dbInstanceIdentifier

instance Prelude.NFData RebootDBInstance where
  rnf RebootDBInstance' {..} =
    Prelude.rnf forceFailover `Prelude.seq`
      Prelude.rnf dbInstanceIdentifier

instance Data.ToHeaders RebootDBInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RebootDBInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery RebootDBInstance where
  toQuery RebootDBInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RebootDBInstance" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "ForceFailover" Data.=: forceFailover,
        "DBInstanceIdentifier" Data.=: dbInstanceIdentifier
      ]

-- | /See:/ 'newRebootDBInstanceResponse' smart constructor.
data RebootDBInstanceResponse = RebootDBInstanceResponse'
  { dbInstance :: Prelude.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootDBInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstance', 'rebootDBInstanceResponse_dbInstance' - Undocumented member.
--
-- 'httpStatus', 'rebootDBInstanceResponse_httpStatus' - The response's http status code.
newRebootDBInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RebootDBInstanceResponse
newRebootDBInstanceResponse pHttpStatus_ =
  RebootDBInstanceResponse'
    { dbInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
rebootDBInstanceResponse_dbInstance :: Lens.Lens' RebootDBInstanceResponse (Prelude.Maybe DBInstance)
rebootDBInstanceResponse_dbInstance = Lens.lens (\RebootDBInstanceResponse' {dbInstance} -> dbInstance) (\s@RebootDBInstanceResponse' {} a -> s {dbInstance = a} :: RebootDBInstanceResponse)

-- | The response's http status code.
rebootDBInstanceResponse_httpStatus :: Lens.Lens' RebootDBInstanceResponse Prelude.Int
rebootDBInstanceResponse_httpStatus = Lens.lens (\RebootDBInstanceResponse' {httpStatus} -> httpStatus) (\s@RebootDBInstanceResponse' {} a -> s {httpStatus = a} :: RebootDBInstanceResponse)

instance Prelude.NFData RebootDBInstanceResponse where
  rnf RebootDBInstanceResponse' {..} =
    Prelude.rnf dbInstance `Prelude.seq`
      Prelude.rnf httpStatus
