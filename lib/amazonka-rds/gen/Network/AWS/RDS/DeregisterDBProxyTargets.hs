{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeregisterDBProxyTargets (..),
    mkDeregisterDBProxyTargets,

    -- ** Request lenses
    dDBProxyName,
    dDBClusterIdentifiers,
    dDBInstanceIdentifiers,
    dTargetGroupName,

    -- * Destructuring the response
    DeregisterDBProxyTargetsResponse (..),
    mkDeregisterDBProxyTargetsResponse,

    -- ** Response lenses
    ddbptrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterDBProxyTargets' smart constructor.
data DeregisterDBProxyTargets = DeregisterDBProxyTargets'
  { -- | The identifier of the @DBProxy@ that is associated with the @DBProxyTargetGroup@ .
    dBProxyName :: Types.String,
    -- | One or more DB cluster identifiers.
    dBClusterIdentifiers :: Core.Maybe [Types.String],
    -- | One or more DB instance identifiers.
    dBInstanceIdentifiers :: Core.Maybe [Types.String],
    -- | The identifier of the @DBProxyTargetGroup@ .
    targetGroupName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterDBProxyTargets' value with any optional fields omitted.
mkDeregisterDBProxyTargets ::
  -- | 'dBProxyName'
  Types.String ->
  DeregisterDBProxyTargets
mkDeregisterDBProxyTargets dBProxyName =
  DeregisterDBProxyTargets'
    { dBProxyName,
      dBClusterIdentifiers = Core.Nothing,
      dBInstanceIdentifiers = Core.Nothing,
      targetGroupName = Core.Nothing
    }

-- | The identifier of the @DBProxy@ that is associated with the @DBProxyTargetGroup@ .
--
-- /Note:/ Consider using 'dBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDBProxyName :: Lens.Lens' DeregisterDBProxyTargets Types.String
dDBProxyName = Lens.field @"dBProxyName"
{-# DEPRECATED dDBProxyName "Use generic-lens or generic-optics with 'dBProxyName' instead." #-}

-- | One or more DB cluster identifiers.
--
-- /Note:/ Consider using 'dBClusterIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDBClusterIdentifiers :: Lens.Lens' DeregisterDBProxyTargets (Core.Maybe [Types.String])
dDBClusterIdentifiers = Lens.field @"dBClusterIdentifiers"
{-# DEPRECATED dDBClusterIdentifiers "Use generic-lens or generic-optics with 'dBClusterIdentifiers' instead." #-}

-- | One or more DB instance identifiers.
--
-- /Note:/ Consider using 'dBInstanceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDBInstanceIdentifiers :: Lens.Lens' DeregisterDBProxyTargets (Core.Maybe [Types.String])
dDBInstanceIdentifiers = Lens.field @"dBInstanceIdentifiers"
{-# DEPRECATED dDBInstanceIdentifiers "Use generic-lens or generic-optics with 'dBInstanceIdentifiers' instead." #-}

-- | The identifier of the @DBProxyTargetGroup@ .
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTargetGroupName :: Lens.Lens' DeregisterDBProxyTargets (Core.Maybe Types.String)
dTargetGroupName = Lens.field @"targetGroupName"
{-# DEPRECATED dTargetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead." #-}

instance Core.AWSRequest DeregisterDBProxyTargets where
  type Rs DeregisterDBProxyTargets = DeregisterDBProxyTargetsResponse
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
            ( Core.pure ("Action", "DeregisterDBProxyTargets")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBProxyName" dBProxyName)
                Core.<> ( Core.toQueryValue
                            "DBClusterIdentifiers"
                            (Core.toQueryList "member" Core.<$> dBClusterIdentifiers)
                        )
                Core.<> ( Core.toQueryValue
                            "DBInstanceIdentifiers"
                            (Core.toQueryList "member" Core.<$> dBInstanceIdentifiers)
                        )
                Core.<> (Core.toQueryValue "TargetGroupName" Core.<$> targetGroupName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeregisterDBProxyTargetsResult"
      ( \s h x ->
          DeregisterDBProxyTargetsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeregisterDBProxyTargetsResponse' smart constructor.
newtype DeregisterDBProxyTargetsResponse = DeregisterDBProxyTargetsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterDBProxyTargetsResponse' value with any optional fields omitted.
mkDeregisterDBProxyTargetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeregisterDBProxyTargetsResponse
mkDeregisterDBProxyTargetsResponse responseStatus =
  DeregisterDBProxyTargetsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptrrsResponseStatus :: Lens.Lens' DeregisterDBProxyTargetsResponse Core.Int
ddbptrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddbptrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
