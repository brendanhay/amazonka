{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyCurrentDBClusterCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the capacity of an Aurora Serverless DB cluster to a specific value.
--
-- Aurora Serverless scales seamlessly based on the workload on the DB cluster. In some cases, the capacity might not scale fast enough to meet a sudden change in workload, such as a large number of new transactions. Call @ModifyCurrentDBClusterCapacity@ to set the capacity explicitly.
-- After this call sets the DB cluster capacity, Aurora Serverless can automatically scale the DB cluster based on the cooldown period for scaling up and the cooldown period for scaling down.
-- For more information about Aurora Serverless, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless> in the /Amazon Aurora User Guide/ .
-- /Important:/ If you call @ModifyCurrentDBClusterCapacity@ with the default @TimeoutAction@ , connections that prevent Aurora Serverless from finding a scaling point might be dropped. For more information about scaling points, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.how-it-works.html#aurora-serverless.how-it-works.auto-scaling Autoscaling for Aurora Serverless> in the /Amazon Aurora User Guide/ .
module Network.AWS.RDS.ModifyCurrentDBClusterCapacity
  ( -- * Creating a request
    ModifyCurrentDBClusterCapacity (..),
    mkModifyCurrentDBClusterCapacity,

    -- ** Request lenses
    mcdbccDBClusterIdentifier,
    mcdbccCapacity,
    mcdbccSecondsBeforeTimeout,
    mcdbccTimeoutAction,

    -- * Destructuring the response
    ModifyCurrentDBClusterCapacityResponse (..),
    mkModifyCurrentDBClusterCapacityResponse,

    -- ** Response lenses
    mcdbccrrsCurrentCapacity,
    mcdbccrrsDBClusterIdentifier,
    mcdbccrrsPendingCapacity,
    mcdbccrrsSecondsBeforeTimeout,
    mcdbccrrsTimeoutAction,
    mcdbccrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyCurrentDBClusterCapacity' smart constructor.
data ModifyCurrentDBClusterCapacity = ModifyCurrentDBClusterCapacity'
  { -- | The DB cluster identifier for the cluster being modified. This parameter isn't case-sensitive.
    --
    -- Constraints:
    --
    --     * Must match the identifier of an existing DB cluster.
    dBClusterIdentifier :: Types.String,
    -- | The DB cluster capacity.
    --
    -- When you change the capacity of a paused Aurora Serverless DB cluster, it automatically resumes.
    -- Constraints:
    --
    --     * For Aurora MySQL, valid capacity values are @1@ , @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ .
    --
    --
    --     * For Aurora PostgreSQL, valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @192@ , and @384@ .
    capacity :: Core.Maybe Core.Int,
    -- | The amount of time, in seconds, that Aurora Serverless tries to find a scaling point to perform seamless scaling before enforcing the timeout action. The default is 300.
    --
    --
    --     * Value must be from 10 through 600.
    secondsBeforeTimeout :: Core.Maybe Core.Int,
    -- | The action to take when the timeout is reached, either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
    --
    -- @ForceApplyCapacityChange@ , the default, sets the capacity to the specified value as soon as possible.
    -- @RollbackCapacityChange@ ignores the capacity change if a scaling point isn't found in the timeout period.
    timeoutAction :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyCurrentDBClusterCapacity' value with any optional fields omitted.
mkModifyCurrentDBClusterCapacity ::
  -- | 'dBClusterIdentifier'
  Types.String ->
  ModifyCurrentDBClusterCapacity
mkModifyCurrentDBClusterCapacity dBClusterIdentifier =
  ModifyCurrentDBClusterCapacity'
    { dBClusterIdentifier,
      capacity = Core.Nothing,
      secondsBeforeTimeout = Core.Nothing,
      timeoutAction = Core.Nothing
    }

-- | The DB cluster identifier for the cluster being modified. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing DB cluster.
--
--
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdbccDBClusterIdentifier :: Lens.Lens' ModifyCurrentDBClusterCapacity Types.String
mcdbccDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# DEPRECATED mcdbccDBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead." #-}

-- | The DB cluster capacity.
--
-- When you change the capacity of a paused Aurora Serverless DB cluster, it automatically resumes.
-- Constraints:
--
--     * For Aurora MySQL, valid capacity values are @1@ , @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ .
--
--
--     * For Aurora PostgreSQL, valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @192@ , and @384@ .
--
--
--
-- /Note:/ Consider using 'capacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdbccCapacity :: Lens.Lens' ModifyCurrentDBClusterCapacity (Core.Maybe Core.Int)
mcdbccCapacity = Lens.field @"capacity"
{-# DEPRECATED mcdbccCapacity "Use generic-lens or generic-optics with 'capacity' instead." #-}

-- | The amount of time, in seconds, that Aurora Serverless tries to find a scaling point to perform seamless scaling before enforcing the timeout action. The default is 300.
--
--
--     * Value must be from 10 through 600.
--
--
--
-- /Note:/ Consider using 'secondsBeforeTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdbccSecondsBeforeTimeout :: Lens.Lens' ModifyCurrentDBClusterCapacity (Core.Maybe Core.Int)
mcdbccSecondsBeforeTimeout = Lens.field @"secondsBeforeTimeout"
{-# DEPRECATED mcdbccSecondsBeforeTimeout "Use generic-lens or generic-optics with 'secondsBeforeTimeout' instead." #-}

-- | The action to take when the timeout is reached, either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
--
-- @ForceApplyCapacityChange@ , the default, sets the capacity to the specified value as soon as possible.
-- @RollbackCapacityChange@ ignores the capacity change if a scaling point isn't found in the timeout period.
--
-- /Note:/ Consider using 'timeoutAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdbccTimeoutAction :: Lens.Lens' ModifyCurrentDBClusterCapacity (Core.Maybe Types.String)
mcdbccTimeoutAction = Lens.field @"timeoutAction"
{-# DEPRECATED mcdbccTimeoutAction "Use generic-lens or generic-optics with 'timeoutAction' instead." #-}

instance Core.AWSRequest ModifyCurrentDBClusterCapacity where
  type
    Rs ModifyCurrentDBClusterCapacity =
      ModifyCurrentDBClusterCapacityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ModifyCurrentDBClusterCapacity")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBClusterIdentifier" dBClusterIdentifier)
                Core.<> (Core.toQueryValue "Capacity" Core.<$> capacity)
                Core.<> ( Core.toQueryValue "SecondsBeforeTimeout"
                            Core.<$> secondsBeforeTimeout
                        )
                Core.<> (Core.toQueryValue "TimeoutAction" Core.<$> timeoutAction)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyCurrentDBClusterCapacityResult"
      ( \s h x ->
          ModifyCurrentDBClusterCapacityResponse'
            Core.<$> (x Core..@? "CurrentCapacity")
            Core.<*> (x Core..@? "DBClusterIdentifier")
            Core.<*> (x Core..@? "PendingCapacity")
            Core.<*> (x Core..@? "SecondsBeforeTimeout")
            Core.<*> (x Core..@? "TimeoutAction")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyCurrentDBClusterCapacityResponse' smart constructor.
data ModifyCurrentDBClusterCapacityResponse = ModifyCurrentDBClusterCapacityResponse'
  { -- | The current capacity of the DB cluster.
    currentCapacity :: Core.Maybe Core.Int,
    -- | A user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
    dBClusterIdentifier :: Core.Maybe Types.String,
    -- | A value that specifies the capacity that the DB cluster scales to next.
    pendingCapacity :: Core.Maybe Core.Int,
    -- | The number of seconds before a call to @ModifyCurrentDBClusterCapacity@ times out.
    secondsBeforeTimeout :: Core.Maybe Core.Int,
    -- | The timeout action of a call to @ModifyCurrentDBClusterCapacity@ , either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
    timeoutAction :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyCurrentDBClusterCapacityResponse' value with any optional fields omitted.
mkModifyCurrentDBClusterCapacityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyCurrentDBClusterCapacityResponse
mkModifyCurrentDBClusterCapacityResponse responseStatus =
  ModifyCurrentDBClusterCapacityResponse'
    { currentCapacity =
        Core.Nothing,
      dBClusterIdentifier = Core.Nothing,
      pendingCapacity = Core.Nothing,
      secondsBeforeTimeout = Core.Nothing,
      timeoutAction = Core.Nothing,
      responseStatus
    }

-- | The current capacity of the DB cluster.
--
-- /Note:/ Consider using 'currentCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdbccrrsCurrentCapacity :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Core.Maybe Core.Int)
mcdbccrrsCurrentCapacity = Lens.field @"currentCapacity"
{-# DEPRECATED mcdbccrrsCurrentCapacity "Use generic-lens or generic-optics with 'currentCapacity' instead." #-}

-- | A user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdbccrrsDBClusterIdentifier :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Core.Maybe Types.String)
mcdbccrrsDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# DEPRECATED mcdbccrrsDBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead." #-}

-- | A value that specifies the capacity that the DB cluster scales to next.
--
-- /Note:/ Consider using 'pendingCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdbccrrsPendingCapacity :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Core.Maybe Core.Int)
mcdbccrrsPendingCapacity = Lens.field @"pendingCapacity"
{-# DEPRECATED mcdbccrrsPendingCapacity "Use generic-lens or generic-optics with 'pendingCapacity' instead." #-}

-- | The number of seconds before a call to @ModifyCurrentDBClusterCapacity@ times out.
--
-- /Note:/ Consider using 'secondsBeforeTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdbccrrsSecondsBeforeTimeout :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Core.Maybe Core.Int)
mcdbccrrsSecondsBeforeTimeout = Lens.field @"secondsBeforeTimeout"
{-# DEPRECATED mcdbccrrsSecondsBeforeTimeout "Use generic-lens or generic-optics with 'secondsBeforeTimeout' instead." #-}

-- | The timeout action of a call to @ModifyCurrentDBClusterCapacity@ , either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
--
-- /Note:/ Consider using 'timeoutAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdbccrrsTimeoutAction :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Core.Maybe Types.String)
mcdbccrrsTimeoutAction = Lens.field @"timeoutAction"
{-# DEPRECATED mcdbccrrsTimeoutAction "Use generic-lens or generic-optics with 'timeoutAction' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdbccrrsResponseStatus :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse Core.Int
mcdbccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mcdbccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
