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
-- Module      : Network.AWS.RDS.RebootDBInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.RDS.RebootDBInstance
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newRebootDBInstance' smart constructor.
data RebootDBInstance = RebootDBInstance'
  { -- | A value that indicates whether the reboot is conducted through a
    -- Multi-AZ failover.
    --
    -- Constraint: You can\'t enable force failover if the instance isn\'t
    -- configured for Multi-AZ.
    forceFailover :: Core.Maybe Core.Bool,
    -- | The DB instance identifier. This parameter is stored as a lowercase
    -- string.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DBInstance.
    dbInstanceIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  RebootDBInstance
newRebootDBInstance pDBInstanceIdentifier_ =
  RebootDBInstance'
    { forceFailover = Core.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | A value that indicates whether the reboot is conducted through a
-- Multi-AZ failover.
--
-- Constraint: You can\'t enable force failover if the instance isn\'t
-- configured for Multi-AZ.
rebootDBInstance_forceFailover :: Lens.Lens' RebootDBInstance (Core.Maybe Core.Bool)
rebootDBInstance_forceFailover = Lens.lens (\RebootDBInstance' {forceFailover} -> forceFailover) (\s@RebootDBInstance' {} a -> s {forceFailover = a} :: RebootDBInstance)

-- | The DB instance identifier. This parameter is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBInstance.
rebootDBInstance_dbInstanceIdentifier :: Lens.Lens' RebootDBInstance Core.Text
rebootDBInstance_dbInstanceIdentifier = Lens.lens (\RebootDBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@RebootDBInstance' {} a -> s {dbInstanceIdentifier = a} :: RebootDBInstance)

instance Core.AWSRequest RebootDBInstance where
  type
    AWSResponse RebootDBInstance =
      RebootDBInstanceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RebootDBInstanceResult"
      ( \s h x ->
          RebootDBInstanceResponse'
            Core.<$> (x Core..@? "DBInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RebootDBInstance

instance Core.NFData RebootDBInstance

instance Core.ToHeaders RebootDBInstance where
  toHeaders = Core.const Core.mempty

instance Core.ToPath RebootDBInstance where
  toPath = Core.const "/"

instance Core.ToQuery RebootDBInstance where
  toQuery RebootDBInstance' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("RebootDBInstance" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "ForceFailover" Core.=: forceFailover,
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier
      ]

-- | /See:/ 'newRebootDBInstanceResponse' smart constructor.
data RebootDBInstanceResponse = RebootDBInstanceResponse'
  { dbInstance :: Core.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  RebootDBInstanceResponse
newRebootDBInstanceResponse pHttpStatus_ =
  RebootDBInstanceResponse'
    { dbInstance =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
rebootDBInstanceResponse_dbInstance :: Lens.Lens' RebootDBInstanceResponse (Core.Maybe DBInstance)
rebootDBInstanceResponse_dbInstance = Lens.lens (\RebootDBInstanceResponse' {dbInstance} -> dbInstance) (\s@RebootDBInstanceResponse' {} a -> s {dbInstance = a} :: RebootDBInstanceResponse)

-- | The response's http status code.
rebootDBInstanceResponse_httpStatus :: Lens.Lens' RebootDBInstanceResponse Core.Int
rebootDBInstanceResponse_httpStatus = Lens.lens (\RebootDBInstanceResponse' {httpStatus} -> httpStatus) (\s@RebootDBInstanceResponse' {} a -> s {httpStatus = a} :: RebootDBInstanceResponse)

instance Core.NFData RebootDBInstanceResponse
