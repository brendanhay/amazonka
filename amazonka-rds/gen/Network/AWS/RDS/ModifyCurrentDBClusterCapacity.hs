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
-- Module      : Network.AWS.RDS.ModifyCurrentDBClusterCapacity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the capacity of an Aurora Serverless DB cluster to a specific value.
--
-- Aurora Serverless scales seamlessly based on the workload on the DB
-- cluster. In some cases, the capacity might not scale fast enough to meet
-- a sudden change in workload, such as a large number of new transactions.
-- Call @ModifyCurrentDBClusterCapacity@ to set the capacity explicitly.
--
-- After this call sets the DB cluster capacity, Aurora Serverless can
-- automatically scale the DB cluster based on the cooldown period for
-- scaling up and the cooldown period for scaling down.
--
-- For more information about Aurora Serverless, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless>
-- in the /Amazon Aurora User Guide/.
--
-- If you call @ModifyCurrentDBClusterCapacity@ with the default
-- @TimeoutAction@, connections that prevent Aurora Serverless from finding
-- a scaling point might be dropped. For more information about scaling
-- points, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.how-it-works.html#aurora-serverless.how-it-works.auto-scaling Autoscaling for Aurora Serverless>
-- in the /Amazon Aurora User Guide/.
--
-- This action only applies to Aurora DB clusters.
module Network.AWS.RDS.ModifyCurrentDBClusterCapacity
  ( -- * Creating a Request
    ModifyCurrentDBClusterCapacity (..),
    newModifyCurrentDBClusterCapacity,

    -- * Request Lenses
    modifyCurrentDBClusterCapacity_timeoutAction,
    modifyCurrentDBClusterCapacity_capacity,
    modifyCurrentDBClusterCapacity_secondsBeforeTimeout,
    modifyCurrentDBClusterCapacity_dbClusterIdentifier,

    -- * Destructuring the Response
    ModifyCurrentDBClusterCapacityResponse (..),
    newModifyCurrentDBClusterCapacityResponse,

    -- * Response Lenses
    modifyCurrentDBClusterCapacityResponse_pendingCapacity,
    modifyCurrentDBClusterCapacityResponse_timeoutAction,
    modifyCurrentDBClusterCapacityResponse_dbClusterIdentifier,
    modifyCurrentDBClusterCapacityResponse_secondsBeforeTimeout,
    modifyCurrentDBClusterCapacityResponse_currentCapacity,
    modifyCurrentDBClusterCapacityResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    timeoutAction :: Core.Maybe Core.Text,
    -- | The DB cluster capacity.
    --
    -- When you change the capacity of a paused Aurora Serverless DB cluster,
    -- it automatically resumes.
    --
    -- Constraints:
    --
    -- -   For Aurora MySQL, valid capacity values are @1@, @2@, @4@, @8@,
    --     @16@, @32@, @64@, @128@, and @256@.
    --
    -- -   For Aurora PostgreSQL, valid capacity values are @2@, @4@, @8@,
    --     @16@, @32@, @64@, @192@, and @384@.
    capacity :: Core.Maybe Core.Int,
    -- | The amount of time, in seconds, that Aurora Serverless tries to find a
    -- scaling point to perform seamless scaling before enforcing the timeout
    -- action. The default is 300.
    --
    -- -   Value must be from 10 through 600.
    secondsBeforeTimeout :: Core.Maybe Core.Int,
    -- | The DB cluster identifier for the cluster being modified. This parameter
    -- isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DB cluster.
    dbClusterIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'capacity', 'modifyCurrentDBClusterCapacity_capacity' - The DB cluster capacity.
--
-- When you change the capacity of a paused Aurora Serverless DB cluster,
-- it automatically resumes.
--
-- Constraints:
--
-- -   For Aurora MySQL, valid capacity values are @1@, @2@, @4@, @8@,
--     @16@, @32@, @64@, @128@, and @256@.
--
-- -   For Aurora PostgreSQL, valid capacity values are @2@, @4@, @8@,
--     @16@, @32@, @64@, @192@, and @384@.
--
-- 'secondsBeforeTimeout', 'modifyCurrentDBClusterCapacity_secondsBeforeTimeout' - The amount of time, in seconds, that Aurora Serverless tries to find a
-- scaling point to perform seamless scaling before enforcing the timeout
-- action. The default is 300.
--
-- -   Value must be from 10 through 600.
--
-- 'dbClusterIdentifier', 'modifyCurrentDBClusterCapacity_dbClusterIdentifier' - The DB cluster identifier for the cluster being modified. This parameter
-- isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DB cluster.
newModifyCurrentDBClusterCapacity ::
  -- | 'dbClusterIdentifier'
  Core.Text ->
  ModifyCurrentDBClusterCapacity
newModifyCurrentDBClusterCapacity
  pDBClusterIdentifier_ =
    ModifyCurrentDBClusterCapacity'
      { timeoutAction =
          Core.Nothing,
        capacity = Core.Nothing,
        secondsBeforeTimeout = Core.Nothing,
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
modifyCurrentDBClusterCapacity_timeoutAction :: Lens.Lens' ModifyCurrentDBClusterCapacity (Core.Maybe Core.Text)
modifyCurrentDBClusterCapacity_timeoutAction = Lens.lens (\ModifyCurrentDBClusterCapacity' {timeoutAction} -> timeoutAction) (\s@ModifyCurrentDBClusterCapacity' {} a -> s {timeoutAction = a} :: ModifyCurrentDBClusterCapacity)

-- | The DB cluster capacity.
--
-- When you change the capacity of a paused Aurora Serverless DB cluster,
-- it automatically resumes.
--
-- Constraints:
--
-- -   For Aurora MySQL, valid capacity values are @1@, @2@, @4@, @8@,
--     @16@, @32@, @64@, @128@, and @256@.
--
-- -   For Aurora PostgreSQL, valid capacity values are @2@, @4@, @8@,
--     @16@, @32@, @64@, @192@, and @384@.
modifyCurrentDBClusterCapacity_capacity :: Lens.Lens' ModifyCurrentDBClusterCapacity (Core.Maybe Core.Int)
modifyCurrentDBClusterCapacity_capacity = Lens.lens (\ModifyCurrentDBClusterCapacity' {capacity} -> capacity) (\s@ModifyCurrentDBClusterCapacity' {} a -> s {capacity = a} :: ModifyCurrentDBClusterCapacity)

-- | The amount of time, in seconds, that Aurora Serverless tries to find a
-- scaling point to perform seamless scaling before enforcing the timeout
-- action. The default is 300.
--
-- -   Value must be from 10 through 600.
modifyCurrentDBClusterCapacity_secondsBeforeTimeout :: Lens.Lens' ModifyCurrentDBClusterCapacity (Core.Maybe Core.Int)
modifyCurrentDBClusterCapacity_secondsBeforeTimeout = Lens.lens (\ModifyCurrentDBClusterCapacity' {secondsBeforeTimeout} -> secondsBeforeTimeout) (\s@ModifyCurrentDBClusterCapacity' {} a -> s {secondsBeforeTimeout = a} :: ModifyCurrentDBClusterCapacity)

-- | The DB cluster identifier for the cluster being modified. This parameter
-- isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DB cluster.
modifyCurrentDBClusterCapacity_dbClusterIdentifier :: Lens.Lens' ModifyCurrentDBClusterCapacity Core.Text
modifyCurrentDBClusterCapacity_dbClusterIdentifier = Lens.lens (\ModifyCurrentDBClusterCapacity' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@ModifyCurrentDBClusterCapacity' {} a -> s {dbClusterIdentifier = a} :: ModifyCurrentDBClusterCapacity)

instance
  Core.AWSRequest
    ModifyCurrentDBClusterCapacity
  where
  type
    AWSResponse ModifyCurrentDBClusterCapacity =
      ModifyCurrentDBClusterCapacityResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyCurrentDBClusterCapacityResult"
      ( \s h x ->
          ModifyCurrentDBClusterCapacityResponse'
            Core.<$> (x Core..@? "PendingCapacity")
            Core.<*> (x Core..@? "TimeoutAction")
            Core.<*> (x Core..@? "DBClusterIdentifier")
            Core.<*> (x Core..@? "SecondsBeforeTimeout")
            Core.<*> (x Core..@? "CurrentCapacity")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyCurrentDBClusterCapacity

instance Core.NFData ModifyCurrentDBClusterCapacity

instance
  Core.ToHeaders
    ModifyCurrentDBClusterCapacity
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyCurrentDBClusterCapacity where
  toPath = Core.const "/"

instance Core.ToQuery ModifyCurrentDBClusterCapacity where
  toQuery ModifyCurrentDBClusterCapacity' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "ModifyCurrentDBClusterCapacity" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "TimeoutAction" Core.=: timeoutAction,
        "Capacity" Core.=: capacity,
        "SecondsBeforeTimeout" Core.=: secondsBeforeTimeout,
        "DBClusterIdentifier" Core.=: dbClusterIdentifier
      ]

-- | /See:/ 'newModifyCurrentDBClusterCapacityResponse' smart constructor.
data ModifyCurrentDBClusterCapacityResponse = ModifyCurrentDBClusterCapacityResponse'
  { -- | A value that specifies the capacity that the DB cluster scales to next.
    pendingCapacity :: Core.Maybe Core.Int,
    -- | The timeout action of a call to @ModifyCurrentDBClusterCapacity@, either
    -- @ForceApplyCapacityChange@ or @RollbackCapacityChange@.
    timeoutAction :: Core.Maybe Core.Text,
    -- | A user-supplied DB cluster identifier. This identifier is the unique key
    -- that identifies a DB cluster.
    dbClusterIdentifier :: Core.Maybe Core.Text,
    -- | The number of seconds before a call to @ModifyCurrentDBClusterCapacity@
    -- times out.
    secondsBeforeTimeout :: Core.Maybe Core.Int,
    -- | The current capacity of the DB cluster.
    currentCapacity :: Core.Maybe Core.Int,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyCurrentDBClusterCapacityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pendingCapacity', 'modifyCurrentDBClusterCapacityResponse_pendingCapacity' - A value that specifies the capacity that the DB cluster scales to next.
--
-- 'timeoutAction', 'modifyCurrentDBClusterCapacityResponse_timeoutAction' - The timeout action of a call to @ModifyCurrentDBClusterCapacity@, either
-- @ForceApplyCapacityChange@ or @RollbackCapacityChange@.
--
-- 'dbClusterIdentifier', 'modifyCurrentDBClusterCapacityResponse_dbClusterIdentifier' - A user-supplied DB cluster identifier. This identifier is the unique key
-- that identifies a DB cluster.
--
-- 'secondsBeforeTimeout', 'modifyCurrentDBClusterCapacityResponse_secondsBeforeTimeout' - The number of seconds before a call to @ModifyCurrentDBClusterCapacity@
-- times out.
--
-- 'currentCapacity', 'modifyCurrentDBClusterCapacityResponse_currentCapacity' - The current capacity of the DB cluster.
--
-- 'httpStatus', 'modifyCurrentDBClusterCapacityResponse_httpStatus' - The response's http status code.
newModifyCurrentDBClusterCapacityResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyCurrentDBClusterCapacityResponse
newModifyCurrentDBClusterCapacityResponse
  pHttpStatus_ =
    ModifyCurrentDBClusterCapacityResponse'
      { pendingCapacity =
          Core.Nothing,
        timeoutAction = Core.Nothing,
        dbClusterIdentifier = Core.Nothing,
        secondsBeforeTimeout = Core.Nothing,
        currentCapacity = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A value that specifies the capacity that the DB cluster scales to next.
modifyCurrentDBClusterCapacityResponse_pendingCapacity :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Core.Maybe Core.Int)
modifyCurrentDBClusterCapacityResponse_pendingCapacity = Lens.lens (\ModifyCurrentDBClusterCapacityResponse' {pendingCapacity} -> pendingCapacity) (\s@ModifyCurrentDBClusterCapacityResponse' {} a -> s {pendingCapacity = a} :: ModifyCurrentDBClusterCapacityResponse)

-- | The timeout action of a call to @ModifyCurrentDBClusterCapacity@, either
-- @ForceApplyCapacityChange@ or @RollbackCapacityChange@.
modifyCurrentDBClusterCapacityResponse_timeoutAction :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Core.Maybe Core.Text)
modifyCurrentDBClusterCapacityResponse_timeoutAction = Lens.lens (\ModifyCurrentDBClusterCapacityResponse' {timeoutAction} -> timeoutAction) (\s@ModifyCurrentDBClusterCapacityResponse' {} a -> s {timeoutAction = a} :: ModifyCurrentDBClusterCapacityResponse)

-- | A user-supplied DB cluster identifier. This identifier is the unique key
-- that identifies a DB cluster.
modifyCurrentDBClusterCapacityResponse_dbClusterIdentifier :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Core.Maybe Core.Text)
modifyCurrentDBClusterCapacityResponse_dbClusterIdentifier = Lens.lens (\ModifyCurrentDBClusterCapacityResponse' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@ModifyCurrentDBClusterCapacityResponse' {} a -> s {dbClusterIdentifier = a} :: ModifyCurrentDBClusterCapacityResponse)

-- | The number of seconds before a call to @ModifyCurrentDBClusterCapacity@
-- times out.
modifyCurrentDBClusterCapacityResponse_secondsBeforeTimeout :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Core.Maybe Core.Int)
modifyCurrentDBClusterCapacityResponse_secondsBeforeTimeout = Lens.lens (\ModifyCurrentDBClusterCapacityResponse' {secondsBeforeTimeout} -> secondsBeforeTimeout) (\s@ModifyCurrentDBClusterCapacityResponse' {} a -> s {secondsBeforeTimeout = a} :: ModifyCurrentDBClusterCapacityResponse)

-- | The current capacity of the DB cluster.
modifyCurrentDBClusterCapacityResponse_currentCapacity :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Core.Maybe Core.Int)
modifyCurrentDBClusterCapacityResponse_currentCapacity = Lens.lens (\ModifyCurrentDBClusterCapacityResponse' {currentCapacity} -> currentCapacity) (\s@ModifyCurrentDBClusterCapacityResponse' {} a -> s {currentCapacity = a} :: ModifyCurrentDBClusterCapacityResponse)

-- | The response's http status code.
modifyCurrentDBClusterCapacityResponse_httpStatus :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse Core.Int
modifyCurrentDBClusterCapacityResponse_httpStatus = Lens.lens (\ModifyCurrentDBClusterCapacityResponse' {httpStatus} -> httpStatus) (\s@ModifyCurrentDBClusterCapacityResponse' {} a -> s {httpStatus = a} :: ModifyCurrentDBClusterCapacityResponse)

instance
  Core.NFData
    ModifyCurrentDBClusterCapacityResponse
