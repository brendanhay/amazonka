{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RegisterDBProxyTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate one or more @DBProxyTarget@ data structures with a @DBProxyTargetGroup@ .
module Network.AWS.RDS.RegisterDBProxyTargets
    (
    -- * Creating a request
      RegisterDBProxyTargets (..)
    , mkRegisterDBProxyTargets
    -- ** Request lenses
    , rdbptDBProxyName
    , rdbptDBClusterIdentifiers
    , rdbptDBInstanceIdentifiers
    , rdbptTargetGroupName

    -- * Destructuring the response
    , RegisterDBProxyTargetsResponse (..)
    , mkRegisterDBProxyTargetsResponse
    -- ** Response lenses
    , rdbptrrsDBProxyTargets
    , rdbptrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterDBProxyTargets' smart constructor.
data RegisterDBProxyTargets = RegisterDBProxyTargets'
  { dBProxyName :: Core.Text
    -- ^ The identifier of the @DBProxy@ that is associated with the @DBProxyTargetGroup@ .
  , dBClusterIdentifiers :: Core.Maybe [Core.Text]
    -- ^ One or more DB cluster identifiers.
  , dBInstanceIdentifiers :: Core.Maybe [Core.Text]
    -- ^ One or more DB instance identifiers.
  , targetGroupName :: Core.Maybe Core.Text
    -- ^ The identifier of the @DBProxyTargetGroup@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterDBProxyTargets' value with any optional fields omitted.
mkRegisterDBProxyTargets
    :: Core.Text -- ^ 'dBProxyName'
    -> RegisterDBProxyTargets
mkRegisterDBProxyTargets dBProxyName
  = RegisterDBProxyTargets'{dBProxyName,
                            dBClusterIdentifiers = Core.Nothing,
                            dBInstanceIdentifiers = Core.Nothing,
                            targetGroupName = Core.Nothing}

-- | The identifier of the @DBProxy@ that is associated with the @DBProxyTargetGroup@ .
--
-- /Note:/ Consider using 'dBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbptDBProxyName :: Lens.Lens' RegisterDBProxyTargets Core.Text
rdbptDBProxyName = Lens.field @"dBProxyName"
{-# INLINEABLE rdbptDBProxyName #-}
{-# DEPRECATED dBProxyName "Use generic-lens or generic-optics with 'dBProxyName' instead"  #-}

-- | One or more DB cluster identifiers.
--
-- /Note:/ Consider using 'dBClusterIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbptDBClusterIdentifiers :: Lens.Lens' RegisterDBProxyTargets (Core.Maybe [Core.Text])
rdbptDBClusterIdentifiers = Lens.field @"dBClusterIdentifiers"
{-# INLINEABLE rdbptDBClusterIdentifiers #-}
{-# DEPRECATED dBClusterIdentifiers "Use generic-lens or generic-optics with 'dBClusterIdentifiers' instead"  #-}

-- | One or more DB instance identifiers.
--
-- /Note:/ Consider using 'dBInstanceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbptDBInstanceIdentifiers :: Lens.Lens' RegisterDBProxyTargets (Core.Maybe [Core.Text])
rdbptDBInstanceIdentifiers = Lens.field @"dBInstanceIdentifiers"
{-# INLINEABLE rdbptDBInstanceIdentifiers #-}
{-# DEPRECATED dBInstanceIdentifiers "Use generic-lens or generic-optics with 'dBInstanceIdentifiers' instead"  #-}

-- | The identifier of the @DBProxyTargetGroup@ .
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbptTargetGroupName :: Lens.Lens' RegisterDBProxyTargets (Core.Maybe Core.Text)
rdbptTargetGroupName = Lens.field @"targetGroupName"
{-# INLINEABLE rdbptTargetGroupName #-}
{-# DEPRECATED targetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead"  #-}

instance Core.ToQuery RegisterDBProxyTargets where
        toQuery RegisterDBProxyTargets{..}
          = Core.toQueryPair "Action" ("RegisterDBProxyTargets" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBProxyName" dBProxyName
              Core.<>
              Core.toQueryPair "DBClusterIdentifiers"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   dBClusterIdentifiers)
              Core.<>
              Core.toQueryPair "DBInstanceIdentifiers"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   dBInstanceIdentifiers)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TargetGroupName")
                targetGroupName

instance Core.ToHeaders RegisterDBProxyTargets where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RegisterDBProxyTargets where
        type Rs RegisterDBProxyTargets = RegisterDBProxyTargetsResponse
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
          = Response.receiveXMLWrapper "RegisterDBProxyTargetsResult"
              (\ s h x ->
                 RegisterDBProxyTargetsResponse' Core.<$>
                   (x Core..@? "DBProxyTargets" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterDBProxyTargetsResponse' smart constructor.
data RegisterDBProxyTargetsResponse = RegisterDBProxyTargetsResponse'
  { dBProxyTargets :: Core.Maybe [Types.DBProxyTarget]
    -- ^ One or more @DBProxyTarget@ objects that are created when you register targets with a target group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterDBProxyTargetsResponse' value with any optional fields omitted.
mkRegisterDBProxyTargetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RegisterDBProxyTargetsResponse
mkRegisterDBProxyTargetsResponse responseStatus
  = RegisterDBProxyTargetsResponse'{dBProxyTargets = Core.Nothing,
                                    responseStatus}

-- | One or more @DBProxyTarget@ objects that are created when you register targets with a target group.
--
-- /Note:/ Consider using 'dBProxyTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbptrrsDBProxyTargets :: Lens.Lens' RegisterDBProxyTargetsResponse (Core.Maybe [Types.DBProxyTarget])
rdbptrrsDBProxyTargets = Lens.field @"dBProxyTargets"
{-# INLINEABLE rdbptrrsDBProxyTargets #-}
{-# DEPRECATED dBProxyTargets "Use generic-lens or generic-optics with 'dBProxyTargets' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbptrrsResponseStatus :: Lens.Lens' RegisterDBProxyTargetsResponse Core.Int
rdbptrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rdbptrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
