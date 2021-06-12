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
-- Module      : Network.AWS.RDS.StartDBInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an Amazon RDS DB instance that was stopped using the AWS console,
-- the stop-db-instance AWS CLI command, or the StopDBInstance action.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_StartInstance.html Starting an Amazon RDS DB instance That Was Previously Stopped>
-- in the /Amazon RDS User Guide./
--
-- This command doesn\'t apply to Aurora MySQL and Aurora PostgreSQL. For
-- Aurora DB clusters, use @StartDBCluster@ instead.
module Network.AWS.RDS.StartDBInstance
  ( -- * Creating a Request
    StartDBInstance (..),
    newStartDBInstance,

    -- * Request Lenses
    startDBInstance_dbInstanceIdentifier,

    -- * Destructuring the Response
    StartDBInstanceResponse (..),
    newStartDBInstanceResponse,

    -- * Response Lenses
    startDBInstanceResponse_dbInstance,
    startDBInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartDBInstance' smart constructor.
data StartDBInstance = StartDBInstance'
  { -- | The user-supplied instance identifier.
    dbInstanceIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartDBInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstanceIdentifier', 'startDBInstance_dbInstanceIdentifier' - The user-supplied instance identifier.
newStartDBInstance ::
  -- | 'dbInstanceIdentifier'
  Core.Text ->
  StartDBInstance
newStartDBInstance pDBInstanceIdentifier_ =
  StartDBInstance'
    { dbInstanceIdentifier =
        pDBInstanceIdentifier_
    }

-- | The user-supplied instance identifier.
startDBInstance_dbInstanceIdentifier :: Lens.Lens' StartDBInstance Core.Text
startDBInstance_dbInstanceIdentifier = Lens.lens (\StartDBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@StartDBInstance' {} a -> s {dbInstanceIdentifier = a} :: StartDBInstance)

instance Core.AWSRequest StartDBInstance where
  type
    AWSResponse StartDBInstance =
      StartDBInstanceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "StartDBInstanceResult"
      ( \s h x ->
          StartDBInstanceResponse'
            Core.<$> (x Core..@? "DBInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartDBInstance

instance Core.NFData StartDBInstance

instance Core.ToHeaders StartDBInstance where
  toHeaders = Core.const Core.mempty

instance Core.ToPath StartDBInstance where
  toPath = Core.const "/"

instance Core.ToQuery StartDBInstance where
  toQuery StartDBInstance' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("StartDBInstance" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier
      ]

-- | /See:/ 'newStartDBInstanceResponse' smart constructor.
data StartDBInstanceResponse = StartDBInstanceResponse'
  { dbInstance :: Core.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartDBInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstance', 'startDBInstanceResponse_dbInstance' - Undocumented member.
--
-- 'httpStatus', 'startDBInstanceResponse_httpStatus' - The response's http status code.
newStartDBInstanceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartDBInstanceResponse
newStartDBInstanceResponse pHttpStatus_ =
  StartDBInstanceResponse'
    { dbInstance = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
startDBInstanceResponse_dbInstance :: Lens.Lens' StartDBInstanceResponse (Core.Maybe DBInstance)
startDBInstanceResponse_dbInstance = Lens.lens (\StartDBInstanceResponse' {dbInstance} -> dbInstance) (\s@StartDBInstanceResponse' {} a -> s {dbInstance = a} :: StartDBInstanceResponse)

-- | The response's http status code.
startDBInstanceResponse_httpStatus :: Lens.Lens' StartDBInstanceResponse Core.Int
startDBInstanceResponse_httpStatus = Lens.lens (\StartDBInstanceResponse' {httpStatus} -> httpStatus) (\s@StartDBInstanceResponse' {} a -> s {httpStatus = a} :: StartDBInstanceResponse)

instance Core.NFData StartDBInstanceResponse
