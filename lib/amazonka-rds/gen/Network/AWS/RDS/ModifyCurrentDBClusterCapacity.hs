{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyCurrentDBClusterCapacity (..)
    , mkModifyCurrentDBClusterCapacity
    -- ** Request lenses
    , mcdbccDBClusterIdentifier
    , mcdbccCapacity
    , mcdbccSecondsBeforeTimeout
    , mcdbccTimeoutAction

    -- * Destructuring the response
    , ModifyCurrentDBClusterCapacityResponse (..)
    , mkModifyCurrentDBClusterCapacityResponse
    -- ** Response lenses
    , mcdbccrrsCurrentCapacity
    , mcdbccrrsDBClusterIdentifier
    , mcdbccrrsPendingCapacity
    , mcdbccrrsSecondsBeforeTimeout
    , mcdbccrrsTimeoutAction
    , mcdbccrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyCurrentDBClusterCapacity' smart constructor.
data ModifyCurrentDBClusterCapacity = ModifyCurrentDBClusterCapacity'
  { dBClusterIdentifier :: Core.Text
    -- ^ The DB cluster identifier for the cluster being modified. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing DB cluster.
--
--
  , capacity :: Core.Maybe Core.Int
    -- ^ The DB cluster capacity.
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
  , secondsBeforeTimeout :: Core.Maybe Core.Int
    -- ^ The amount of time, in seconds, that Aurora Serverless tries to find a scaling point to perform seamless scaling before enforcing the timeout action. The default is 300.
--
--
--     * Value must be from 10 through 600.
--
--
  , timeoutAction :: Core.Maybe Core.Text
    -- ^ The action to take when the timeout is reached, either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
--
-- @ForceApplyCapacityChange@ , the default, sets the capacity to the specified value as soon as possible.
-- @RollbackCapacityChange@ ignores the capacity change if a scaling point isn't found in the timeout period.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyCurrentDBClusterCapacity' value with any optional fields omitted.
mkModifyCurrentDBClusterCapacity
    :: Core.Text -- ^ 'dBClusterIdentifier'
    -> ModifyCurrentDBClusterCapacity
mkModifyCurrentDBClusterCapacity dBClusterIdentifier
  = ModifyCurrentDBClusterCapacity'{dBClusterIdentifier,
                                    capacity = Core.Nothing, secondsBeforeTimeout = Core.Nothing,
                                    timeoutAction = Core.Nothing}

-- | The DB cluster identifier for the cluster being modified. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing DB cluster.
--
--
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdbccDBClusterIdentifier :: Lens.Lens' ModifyCurrentDBClusterCapacity Core.Text
mcdbccDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# INLINEABLE mcdbccDBClusterIdentifier #-}
{-# DEPRECATED dBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead"  #-}

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
{-# INLINEABLE mcdbccCapacity #-}
{-# DEPRECATED capacity "Use generic-lens or generic-optics with 'capacity' instead"  #-}

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
{-# INLINEABLE mcdbccSecondsBeforeTimeout #-}
{-# DEPRECATED secondsBeforeTimeout "Use generic-lens or generic-optics with 'secondsBeforeTimeout' instead"  #-}

-- | The action to take when the timeout is reached, either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
--
-- @ForceApplyCapacityChange@ , the default, sets the capacity to the specified value as soon as possible.
-- @RollbackCapacityChange@ ignores the capacity change if a scaling point isn't found in the timeout period.
--
-- /Note:/ Consider using 'timeoutAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdbccTimeoutAction :: Lens.Lens' ModifyCurrentDBClusterCapacity (Core.Maybe Core.Text)
mcdbccTimeoutAction = Lens.field @"timeoutAction"
{-# INLINEABLE mcdbccTimeoutAction #-}
{-# DEPRECATED timeoutAction "Use generic-lens or generic-optics with 'timeoutAction' instead"  #-}

instance Core.ToQuery ModifyCurrentDBClusterCapacity where
        toQuery ModifyCurrentDBClusterCapacity{..}
          = Core.toQueryPair "Action"
              ("ModifyCurrentDBClusterCapacity" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBClusterIdentifier" dBClusterIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Capacity") capacity
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SecondsBeforeTimeout")
                secondsBeforeTimeout
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TimeoutAction")
                timeoutAction

instance Core.ToHeaders ModifyCurrentDBClusterCapacity where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyCurrentDBClusterCapacity where
        type Rs ModifyCurrentDBClusterCapacity =
             ModifyCurrentDBClusterCapacityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ModifyCurrentDBClusterCapacityResult"
              (\ s h x ->
                 ModifyCurrentDBClusterCapacityResponse' Core.<$>
                   (x Core..@? "CurrentCapacity") Core.<*>
                     x Core..@? "DBClusterIdentifier"
                     Core.<*> x Core..@? "PendingCapacity"
                     Core.<*> x Core..@? "SecondsBeforeTimeout"
                     Core.<*> x Core..@? "TimeoutAction"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyCurrentDBClusterCapacityResponse' smart constructor.
data ModifyCurrentDBClusterCapacityResponse = ModifyCurrentDBClusterCapacityResponse'
  { currentCapacity :: Core.Maybe Core.Int
    -- ^ The current capacity of the DB cluster.
  , dBClusterIdentifier :: Core.Maybe Core.Text
    -- ^ A user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster. 
  , pendingCapacity :: Core.Maybe Core.Int
    -- ^ A value that specifies the capacity that the DB cluster scales to next.
  , secondsBeforeTimeout :: Core.Maybe Core.Int
    -- ^ The number of seconds before a call to @ModifyCurrentDBClusterCapacity@ times out.
  , timeoutAction :: Core.Maybe Core.Text
    -- ^ The timeout action of a call to @ModifyCurrentDBClusterCapacity@ , either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyCurrentDBClusterCapacityResponse' value with any optional fields omitted.
mkModifyCurrentDBClusterCapacityResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyCurrentDBClusterCapacityResponse
mkModifyCurrentDBClusterCapacityResponse responseStatus
  = ModifyCurrentDBClusterCapacityResponse'{currentCapacity =
                                              Core.Nothing,
                                            dBClusterIdentifier = Core.Nothing,
                                            pendingCapacity = Core.Nothing,
                                            secondsBeforeTimeout = Core.Nothing,
                                            timeoutAction = Core.Nothing, responseStatus}

-- | The current capacity of the DB cluster.
--
-- /Note:/ Consider using 'currentCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdbccrrsCurrentCapacity :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Core.Maybe Core.Int)
mcdbccrrsCurrentCapacity = Lens.field @"currentCapacity"
{-# INLINEABLE mcdbccrrsCurrentCapacity #-}
{-# DEPRECATED currentCapacity "Use generic-lens or generic-optics with 'currentCapacity' instead"  #-}

-- | A user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster. 
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdbccrrsDBClusterIdentifier :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Core.Maybe Core.Text)
mcdbccrrsDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# INLINEABLE mcdbccrrsDBClusterIdentifier #-}
{-# DEPRECATED dBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead"  #-}

-- | A value that specifies the capacity that the DB cluster scales to next.
--
-- /Note:/ Consider using 'pendingCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdbccrrsPendingCapacity :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Core.Maybe Core.Int)
mcdbccrrsPendingCapacity = Lens.field @"pendingCapacity"
{-# INLINEABLE mcdbccrrsPendingCapacity #-}
{-# DEPRECATED pendingCapacity "Use generic-lens or generic-optics with 'pendingCapacity' instead"  #-}

-- | The number of seconds before a call to @ModifyCurrentDBClusterCapacity@ times out.
--
-- /Note:/ Consider using 'secondsBeforeTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdbccrrsSecondsBeforeTimeout :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Core.Maybe Core.Int)
mcdbccrrsSecondsBeforeTimeout = Lens.field @"secondsBeforeTimeout"
{-# INLINEABLE mcdbccrrsSecondsBeforeTimeout #-}
{-# DEPRECATED secondsBeforeTimeout "Use generic-lens or generic-optics with 'secondsBeforeTimeout' instead"  #-}

-- | The timeout action of a call to @ModifyCurrentDBClusterCapacity@ , either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
--
-- /Note:/ Consider using 'timeoutAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdbccrrsTimeoutAction :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Core.Maybe Core.Text)
mcdbccrrsTimeoutAction = Lens.field @"timeoutAction"
{-# INLINEABLE mcdbccrrsTimeoutAction #-}
{-# DEPRECATED timeoutAction "Use generic-lens or generic-optics with 'timeoutAction' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdbccrrsResponseStatus :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse Core.Int
mcdbccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mcdbccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
