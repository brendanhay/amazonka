{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeregisterDBProxyTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove the association between one or more @DBProxyTarget@ data structures and a @DBProxyTargetGroup@ .
module Network.AWS.RDS.DeregisterDBProxyTargets
    (
    -- * Creating a request
      DeregisterDBProxyTargets (..)
    , mkDeregisterDBProxyTargets
    -- ** Request lenses
    , dDBProxyName
    , dDBClusterIdentifiers
    , dDBInstanceIdentifiers
    , dTargetGroupName

    -- * Destructuring the response
    , DeregisterDBProxyTargetsResponse (..)
    , mkDeregisterDBProxyTargetsResponse
    -- ** Response lenses
    , ddbptrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterDBProxyTargets' smart constructor.
data DeregisterDBProxyTargets = DeregisterDBProxyTargets'
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

-- | Creates a 'DeregisterDBProxyTargets' value with any optional fields omitted.
mkDeregisterDBProxyTargets
    :: Core.Text -- ^ 'dBProxyName'
    -> DeregisterDBProxyTargets
mkDeregisterDBProxyTargets dBProxyName
  = DeregisterDBProxyTargets'{dBProxyName,
                              dBClusterIdentifiers = Core.Nothing,
                              dBInstanceIdentifiers = Core.Nothing,
                              targetGroupName = Core.Nothing}

-- | The identifier of the @DBProxy@ that is associated with the @DBProxyTargetGroup@ .
--
-- /Note:/ Consider using 'dBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDBProxyName :: Lens.Lens' DeregisterDBProxyTargets Core.Text
dDBProxyName = Lens.field @"dBProxyName"
{-# INLINEABLE dDBProxyName #-}
{-# DEPRECATED dBProxyName "Use generic-lens or generic-optics with 'dBProxyName' instead"  #-}

-- | One or more DB cluster identifiers.
--
-- /Note:/ Consider using 'dBClusterIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDBClusterIdentifiers :: Lens.Lens' DeregisterDBProxyTargets (Core.Maybe [Core.Text])
dDBClusterIdentifiers = Lens.field @"dBClusterIdentifiers"
{-# INLINEABLE dDBClusterIdentifiers #-}
{-# DEPRECATED dBClusterIdentifiers "Use generic-lens or generic-optics with 'dBClusterIdentifiers' instead"  #-}

-- | One or more DB instance identifiers.
--
-- /Note:/ Consider using 'dBInstanceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDBInstanceIdentifiers :: Lens.Lens' DeregisterDBProxyTargets (Core.Maybe [Core.Text])
dDBInstanceIdentifiers = Lens.field @"dBInstanceIdentifiers"
{-# INLINEABLE dDBInstanceIdentifiers #-}
{-# DEPRECATED dBInstanceIdentifiers "Use generic-lens or generic-optics with 'dBInstanceIdentifiers' instead"  #-}

-- | The identifier of the @DBProxyTargetGroup@ .
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTargetGroupName :: Lens.Lens' DeregisterDBProxyTargets (Core.Maybe Core.Text)
dTargetGroupName = Lens.field @"targetGroupName"
{-# INLINEABLE dTargetGroupName #-}
{-# DEPRECATED targetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead"  #-}

instance Core.ToQuery DeregisterDBProxyTargets where
        toQuery DeregisterDBProxyTargets{..}
          = Core.toQueryPair "Action"
              ("DeregisterDBProxyTargets" :: Core.Text)
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

instance Core.ToHeaders DeregisterDBProxyTargets where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeregisterDBProxyTargets where
        type Rs DeregisterDBProxyTargets = DeregisterDBProxyTargetsResponse
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
          = Response.receiveXMLWrapper "DeregisterDBProxyTargetsResult"
              (\ s h x ->
                 DeregisterDBProxyTargetsResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeregisterDBProxyTargetsResponse' smart constructor.
newtype DeregisterDBProxyTargetsResponse = DeregisterDBProxyTargetsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterDBProxyTargetsResponse' value with any optional fields omitted.
mkDeregisterDBProxyTargetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeregisterDBProxyTargetsResponse
mkDeregisterDBProxyTargetsResponse responseStatus
  = DeregisterDBProxyTargetsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptrrsResponseStatus :: Lens.Lens' DeregisterDBProxyTargetsResponse Core.Int
ddbptrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbptrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
