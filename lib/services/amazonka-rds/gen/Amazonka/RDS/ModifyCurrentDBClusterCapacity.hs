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
-- Module      : Amazonka.RDS.ModifyCurrentDBClusterCapacity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the capacity of an Aurora Serverless v1 DB cluster to a specific
-- value.
--
-- Aurora Serverless v1 scales seamlessly based on the workload on the DB
-- cluster. In some cases, the capacity might not scale fast enough to meet
-- a sudden change in workload, such as a large number of new transactions.
-- Call @ModifyCurrentDBClusterCapacity@ to set the capacity explicitly.
--
-- After this call sets the DB cluster capacity, Aurora Serverless v1 can
-- automatically scale the DB cluster based on the cooldown period for
-- scaling up and the cooldown period for scaling down.
--
-- For more information about Aurora Serverless v1, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless v1>
-- in the /Amazon Aurora User Guide/.
--
-- If you call @ModifyCurrentDBClusterCapacity@ with the default
-- @TimeoutAction@, connections that prevent Aurora Serverless v1 from
-- finding a scaling point might be dropped. For more information about
-- scaling points, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.how-it-works.html#aurora-serverless.how-it-works.auto-scaling Autoscaling for Aurora Serverless v1>
-- in the /Amazon Aurora User Guide/.
--
-- This action only applies to Aurora Serverless v1 DB clusters.
module Amazonka.RDS.ModifyCurrentDBClusterCapacity
  ( -- * Creating a Request
    ModifyCurrentDBClusterCapacity (..),
    newModifyCurrentDBClusterCapacity,

    -- * Request Lenses
    modifyCurrentDBClusterCapacity_timeoutAction,
    modifyCurrentDBClusterCapacity_secondsBeforeTimeout,
    modifyCurrentDBClusterCapacity_capacity,
    modifyCurrentDBClusterCapacity_dbClusterIdentifier,

    -- * Destructuring the Response
    ModifyCurrentDBClusterCapacityResponse (..),
    newModifyCurrentDBClusterCapacityResponse,

    -- * Response Lenses
    modifyCurrentDBClusterCapacityResponse_timeoutAction,
    modifyCurrentDBClusterCapacityResponse_dbClusterIdentifier,
    modifyCurrentDBClusterCapacityResponse_pendingCapacity,
    modifyCurrentDBClusterCapacityResponse_secondsBeforeTimeout,
    modifyCurrentDBClusterCapacityResponse_currentCapacity,
    modifyCurrentDBClusterCapacityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyCurrentDBClusterCapacity' smart constructor.
data ModifyCurrentDBClusterCapacity = ModifyCurrentDBClusterCapacity'
  { -- | The action to take when the timeout is reached, either
    -- @ForceApplyCapacityChange@ or @RollbackCapacityChange@.
    --
    -- @ForceApplyCapacityChange@, the default, sets the capacity to the
    -- specified value as soon as possible.
    --
    -- @RollbackCapacityChange@ ignores the capacity change if a scaling point
    -- isn\'t found in the timeout period.
    timeoutAction :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in seconds, that Aurora Serverless v1 tries to find
    -- a scaling point to perform seamless scaling before enforcing the timeout
    -- action. The default is 300.
    --
    -- Specify a value between 10 and 600 seconds.
    secondsBeforeTimeout :: Prelude.Maybe Prelude.Int,
    -- | The DB cluster capacity.
    --
    -- When you change the capacity of a paused Aurora Serverless v1 DB
    -- cluster, it automatically resumes.
    --
    -- Constraints:
    --
    -- -   For Aurora MySQL, valid capacity values are @1@, @2@, @4@, @8@,
    --     @16@, @32@, @64@, @128@, and @256@.
    --
    -- -   For Aurora PostgreSQL, valid capacity values are @2@, @4@, @8@,
    --     @16@, @32@, @64@, @192@, and @384@.
    capacity :: Prelude.Maybe Prelude.Int,
    -- | The DB cluster identifier for the cluster being modified. This parameter
    -- isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DB cluster.
    dbClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCurrentDBClusterCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeoutAction', 'modifyCurrentDBClusterCapacity_timeoutAction' - The action to take when the timeout is reached, either
-- @ForceApplyCapacityChange@ or @RollbackCapacityChange@.
--
-- @ForceApplyCapacityChange@, the default, sets the capacity to the
-- specified value as soon as possible.
--
-- @RollbackCapacityChange@ ignores the capacity change if a scaling point
-- isn\'t found in the timeout period.
--
-- 'secondsBeforeTimeout', 'modifyCurrentDBClusterCapacity_secondsBeforeTimeout' - The amount of time, in seconds, that Aurora Serverless v1 tries to find
-- a scaling point to perform seamless scaling before enforcing the timeout
-- action. The default is 300.
--
-- Specify a value between 10 and 600 seconds.
--
-- 'capacity', 'modifyCurrentDBClusterCapacity_capacity' - The DB cluster capacity.
--
-- When you change the capacity of a paused Aurora Serverless v1 DB
-- cluster, it automatically resumes.
--
-- Constraints:
--
-- -   For Aurora MySQL, valid capacity values are @1@, @2@, @4@, @8@,
--     @16@, @32@, @64@, @128@, and @256@.
--
-- -   For Aurora PostgreSQL, valid capacity values are @2@, @4@, @8@,
--     @16@, @32@, @64@, @192@, and @384@.
--
-- 'dbClusterIdentifier', 'modifyCurrentDBClusterCapacity_dbClusterIdentifier' - The DB cluster identifier for the cluster being modified. This parameter
-- isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DB cluster.
newModifyCurrentDBClusterCapacity ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  ModifyCurrentDBClusterCapacity
newModifyCurrentDBClusterCapacity
  pDBClusterIdentifier_ =
    ModifyCurrentDBClusterCapacity'
      { timeoutAction =
          Prelude.Nothing,
        secondsBeforeTimeout = Prelude.Nothing,
        capacity = Prelude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_
      }

-- | The action to take when the timeout is reached, either
-- @ForceApplyCapacityChange@ or @RollbackCapacityChange@.
--
-- @ForceApplyCapacityChange@, the default, sets the capacity to the
-- specified value as soon as possible.
--
-- @RollbackCapacityChange@ ignores the capacity change if a scaling point
-- isn\'t found in the timeout period.
modifyCurrentDBClusterCapacity_timeoutAction :: Lens.Lens' ModifyCurrentDBClusterCapacity (Prelude.Maybe Prelude.Text)
modifyCurrentDBClusterCapacity_timeoutAction = Lens.lens (\ModifyCurrentDBClusterCapacity' {timeoutAction} -> timeoutAction) (\s@ModifyCurrentDBClusterCapacity' {} a -> s {timeoutAction = a} :: ModifyCurrentDBClusterCapacity)

-- | The amount of time, in seconds, that Aurora Serverless v1 tries to find
-- a scaling point to perform seamless scaling before enforcing the timeout
-- action. The default is 300.
--
-- Specify a value between 10 and 600 seconds.
modifyCurrentDBClusterCapacity_secondsBeforeTimeout :: Lens.Lens' ModifyCurrentDBClusterCapacity (Prelude.Maybe Prelude.Int)
modifyCurrentDBClusterCapacity_secondsBeforeTimeout = Lens.lens (\ModifyCurrentDBClusterCapacity' {secondsBeforeTimeout} -> secondsBeforeTimeout) (\s@ModifyCurrentDBClusterCapacity' {} a -> s {secondsBeforeTimeout = a} :: ModifyCurrentDBClusterCapacity)

-- | The DB cluster capacity.
--
-- When you change the capacity of a paused Aurora Serverless v1 DB
-- cluster, it automatically resumes.
--
-- Constraints:
--
-- -   For Aurora MySQL, valid capacity values are @1@, @2@, @4@, @8@,
--     @16@, @32@, @64@, @128@, and @256@.
--
-- -   For Aurora PostgreSQL, valid capacity values are @2@, @4@, @8@,
--     @16@, @32@, @64@, @192@, and @384@.
modifyCurrentDBClusterCapacity_capacity :: Lens.Lens' ModifyCurrentDBClusterCapacity (Prelude.Maybe Prelude.Int)
modifyCurrentDBClusterCapacity_capacity = Lens.lens (\ModifyCurrentDBClusterCapacity' {capacity} -> capacity) (\s@ModifyCurrentDBClusterCapacity' {} a -> s {capacity = a} :: ModifyCurrentDBClusterCapacity)

-- | The DB cluster identifier for the cluster being modified. This parameter
-- isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DB cluster.
modifyCurrentDBClusterCapacity_dbClusterIdentifier :: Lens.Lens' ModifyCurrentDBClusterCapacity Prelude.Text
modifyCurrentDBClusterCapacity_dbClusterIdentifier = Lens.lens (\ModifyCurrentDBClusterCapacity' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@ModifyCurrentDBClusterCapacity' {} a -> s {dbClusterIdentifier = a} :: ModifyCurrentDBClusterCapacity)

instance
  Core.AWSRequest
    ModifyCurrentDBClusterCapacity
  where
  type
    AWSResponse ModifyCurrentDBClusterCapacity =
      ModifyCurrentDBClusterCapacityResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyCurrentDBClusterCapacityResult"
      ( \s h x ->
          ModifyCurrentDBClusterCapacityResponse'
            Prelude.<$> (x Core..@? "TimeoutAction")
            Prelude.<*> (x Core..@? "DBClusterIdentifier")
            Prelude.<*> (x Core..@? "PendingCapacity")
            Prelude.<*> (x Core..@? "SecondsBeforeTimeout")
            Prelude.<*> (x Core..@? "CurrentCapacity")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyCurrentDBClusterCapacity
  where
  hashWithSalt
    _salt
    ModifyCurrentDBClusterCapacity' {..} =
      _salt `Prelude.hashWithSalt` timeoutAction
        `Prelude.hashWithSalt` secondsBeforeTimeout
        `Prelude.hashWithSalt` capacity
        `Prelude.hashWithSalt` dbClusterIdentifier

instance
  Prelude.NFData
    ModifyCurrentDBClusterCapacity
  where
  rnf ModifyCurrentDBClusterCapacity' {..} =
    Prelude.rnf timeoutAction
      `Prelude.seq` Prelude.rnf secondsBeforeTimeout
      `Prelude.seq` Prelude.rnf capacity
      `Prelude.seq` Prelude.rnf dbClusterIdentifier

instance
  Core.ToHeaders
    ModifyCurrentDBClusterCapacity
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyCurrentDBClusterCapacity where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyCurrentDBClusterCapacity where
  toQuery ModifyCurrentDBClusterCapacity' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ModifyCurrentDBClusterCapacity" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "TimeoutAction" Core.=: timeoutAction,
        "SecondsBeforeTimeout" Core.=: secondsBeforeTimeout,
        "Capacity" Core.=: capacity,
        "DBClusterIdentifier" Core.=: dbClusterIdentifier
      ]

-- | /See:/ 'newModifyCurrentDBClusterCapacityResponse' smart constructor.
data ModifyCurrentDBClusterCapacityResponse = ModifyCurrentDBClusterCapacityResponse'
  { -- | The timeout action of a call to @ModifyCurrentDBClusterCapacity@, either
    -- @ForceApplyCapacityChange@ or @RollbackCapacityChange@.
    timeoutAction :: Prelude.Maybe Prelude.Text,
    -- | A user-supplied DB cluster identifier. This identifier is the unique key
    -- that identifies a DB cluster.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies the capacity that the DB cluster scales to next.
    pendingCapacity :: Prelude.Maybe Prelude.Int,
    -- | The number of seconds before a call to @ModifyCurrentDBClusterCapacity@
    -- times out.
    secondsBeforeTimeout :: Prelude.Maybe Prelude.Int,
    -- | The current capacity of the DB cluster.
    currentCapacity :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCurrentDBClusterCapacityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeoutAction', 'modifyCurrentDBClusterCapacityResponse_timeoutAction' - The timeout action of a call to @ModifyCurrentDBClusterCapacity@, either
-- @ForceApplyCapacityChange@ or @RollbackCapacityChange@.
--
-- 'dbClusterIdentifier', 'modifyCurrentDBClusterCapacityResponse_dbClusterIdentifier' - A user-supplied DB cluster identifier. This identifier is the unique key
-- that identifies a DB cluster.
--
-- 'pendingCapacity', 'modifyCurrentDBClusterCapacityResponse_pendingCapacity' - A value that specifies the capacity that the DB cluster scales to next.
--
-- 'secondsBeforeTimeout', 'modifyCurrentDBClusterCapacityResponse_secondsBeforeTimeout' - The number of seconds before a call to @ModifyCurrentDBClusterCapacity@
-- times out.
--
-- 'currentCapacity', 'modifyCurrentDBClusterCapacityResponse_currentCapacity' - The current capacity of the DB cluster.
--
-- 'httpStatus', 'modifyCurrentDBClusterCapacityResponse_httpStatus' - The response's http status code.
newModifyCurrentDBClusterCapacityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyCurrentDBClusterCapacityResponse
newModifyCurrentDBClusterCapacityResponse
  pHttpStatus_ =
    ModifyCurrentDBClusterCapacityResponse'
      { timeoutAction =
          Prelude.Nothing,
        dbClusterIdentifier =
          Prelude.Nothing,
        pendingCapacity = Prelude.Nothing,
        secondsBeforeTimeout =
          Prelude.Nothing,
        currentCapacity = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The timeout action of a call to @ModifyCurrentDBClusterCapacity@, either
-- @ForceApplyCapacityChange@ or @RollbackCapacityChange@.
modifyCurrentDBClusterCapacityResponse_timeoutAction :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Prelude.Maybe Prelude.Text)
modifyCurrentDBClusterCapacityResponse_timeoutAction = Lens.lens (\ModifyCurrentDBClusterCapacityResponse' {timeoutAction} -> timeoutAction) (\s@ModifyCurrentDBClusterCapacityResponse' {} a -> s {timeoutAction = a} :: ModifyCurrentDBClusterCapacityResponse)

-- | A user-supplied DB cluster identifier. This identifier is the unique key
-- that identifies a DB cluster.
modifyCurrentDBClusterCapacityResponse_dbClusterIdentifier :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Prelude.Maybe Prelude.Text)
modifyCurrentDBClusterCapacityResponse_dbClusterIdentifier = Lens.lens (\ModifyCurrentDBClusterCapacityResponse' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@ModifyCurrentDBClusterCapacityResponse' {} a -> s {dbClusterIdentifier = a} :: ModifyCurrentDBClusterCapacityResponse)

-- | A value that specifies the capacity that the DB cluster scales to next.
modifyCurrentDBClusterCapacityResponse_pendingCapacity :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Prelude.Maybe Prelude.Int)
modifyCurrentDBClusterCapacityResponse_pendingCapacity = Lens.lens (\ModifyCurrentDBClusterCapacityResponse' {pendingCapacity} -> pendingCapacity) (\s@ModifyCurrentDBClusterCapacityResponse' {} a -> s {pendingCapacity = a} :: ModifyCurrentDBClusterCapacityResponse)

-- | The number of seconds before a call to @ModifyCurrentDBClusterCapacity@
-- times out.
modifyCurrentDBClusterCapacityResponse_secondsBeforeTimeout :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Prelude.Maybe Prelude.Int)
modifyCurrentDBClusterCapacityResponse_secondsBeforeTimeout = Lens.lens (\ModifyCurrentDBClusterCapacityResponse' {secondsBeforeTimeout} -> secondsBeforeTimeout) (\s@ModifyCurrentDBClusterCapacityResponse' {} a -> s {secondsBeforeTimeout = a} :: ModifyCurrentDBClusterCapacityResponse)

-- | The current capacity of the DB cluster.
modifyCurrentDBClusterCapacityResponse_currentCapacity :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Prelude.Maybe Prelude.Int)
modifyCurrentDBClusterCapacityResponse_currentCapacity = Lens.lens (\ModifyCurrentDBClusterCapacityResponse' {currentCapacity} -> currentCapacity) (\s@ModifyCurrentDBClusterCapacityResponse' {} a -> s {currentCapacity = a} :: ModifyCurrentDBClusterCapacityResponse)

-- | The response's http status code.
modifyCurrentDBClusterCapacityResponse_httpStatus :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse Prelude.Int
modifyCurrentDBClusterCapacityResponse_httpStatus = Lens.lens (\ModifyCurrentDBClusterCapacityResponse' {httpStatus} -> httpStatus) (\s@ModifyCurrentDBClusterCapacityResponse' {} a -> s {httpStatus = a} :: ModifyCurrentDBClusterCapacityResponse)

instance
  Prelude.NFData
    ModifyCurrentDBClusterCapacityResponse
  where
  rnf ModifyCurrentDBClusterCapacityResponse' {..} =
    Prelude.rnf timeoutAction
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf pendingCapacity
      `Prelude.seq` Prelude.rnf secondsBeforeTimeout
      `Prelude.seq` Prelude.rnf currentCapacity
      `Prelude.seq` Prelude.rnf httpStatus
