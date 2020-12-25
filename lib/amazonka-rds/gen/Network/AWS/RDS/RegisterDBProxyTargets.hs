{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    RegisterDBProxyTargets (..),
    mkRegisterDBProxyTargets,

    -- ** Request lenses
    rdbptDBProxyName,
    rdbptDBClusterIdentifiers,
    rdbptDBInstanceIdentifiers,
    rdbptTargetGroupName,

    -- * Destructuring the response
    RegisterDBProxyTargetsResponse (..),
    mkRegisterDBProxyTargetsResponse,

    -- ** Response lenses
    rdbptrrsDBProxyTargets,
    rdbptrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterDBProxyTargets' smart constructor.
data RegisterDBProxyTargets = RegisterDBProxyTargets'
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

-- | Creates a 'RegisterDBProxyTargets' value with any optional fields omitted.
mkRegisterDBProxyTargets ::
  -- | 'dBProxyName'
  Types.String ->
  RegisterDBProxyTargets
mkRegisterDBProxyTargets dBProxyName =
  RegisterDBProxyTargets'
    { dBProxyName,
      dBClusterIdentifiers = Core.Nothing,
      dBInstanceIdentifiers = Core.Nothing,
      targetGroupName = Core.Nothing
    }

-- | The identifier of the @DBProxy@ that is associated with the @DBProxyTargetGroup@ .
--
-- /Note:/ Consider using 'dBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbptDBProxyName :: Lens.Lens' RegisterDBProxyTargets Types.String
rdbptDBProxyName = Lens.field @"dBProxyName"
{-# DEPRECATED rdbptDBProxyName "Use generic-lens or generic-optics with 'dBProxyName' instead." #-}

-- | One or more DB cluster identifiers.
--
-- /Note:/ Consider using 'dBClusterIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbptDBClusterIdentifiers :: Lens.Lens' RegisterDBProxyTargets (Core.Maybe [Types.String])
rdbptDBClusterIdentifiers = Lens.field @"dBClusterIdentifiers"
{-# DEPRECATED rdbptDBClusterIdentifiers "Use generic-lens or generic-optics with 'dBClusterIdentifiers' instead." #-}

-- | One or more DB instance identifiers.
--
-- /Note:/ Consider using 'dBInstanceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbptDBInstanceIdentifiers :: Lens.Lens' RegisterDBProxyTargets (Core.Maybe [Types.String])
rdbptDBInstanceIdentifiers = Lens.field @"dBInstanceIdentifiers"
{-# DEPRECATED rdbptDBInstanceIdentifiers "Use generic-lens or generic-optics with 'dBInstanceIdentifiers' instead." #-}

-- | The identifier of the @DBProxyTargetGroup@ .
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbptTargetGroupName :: Lens.Lens' RegisterDBProxyTargets (Core.Maybe Types.String)
rdbptTargetGroupName = Lens.field @"targetGroupName"
{-# DEPRECATED rdbptTargetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead." #-}

instance Core.AWSRequest RegisterDBProxyTargets where
  type Rs RegisterDBProxyTargets = RegisterDBProxyTargetsResponse
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
            ( Core.pure ("Action", "RegisterDBProxyTargets")
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
      "RegisterDBProxyTargetsResult"
      ( \s h x ->
          RegisterDBProxyTargetsResponse'
            Core.<$> (x Core..@? "DBProxyTargets" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRegisterDBProxyTargetsResponse' smart constructor.
data RegisterDBProxyTargetsResponse = RegisterDBProxyTargetsResponse'
  { -- | One or more @DBProxyTarget@ objects that are created when you register targets with a target group.
    dBProxyTargets :: Core.Maybe [Types.DBProxyTarget],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterDBProxyTargetsResponse' value with any optional fields omitted.
mkRegisterDBProxyTargetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterDBProxyTargetsResponse
mkRegisterDBProxyTargetsResponse responseStatus =
  RegisterDBProxyTargetsResponse'
    { dBProxyTargets = Core.Nothing,
      responseStatus
    }

-- | One or more @DBProxyTarget@ objects that are created when you register targets with a target group.
--
-- /Note:/ Consider using 'dBProxyTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbptrrsDBProxyTargets :: Lens.Lens' RegisterDBProxyTargetsResponse (Core.Maybe [Types.DBProxyTarget])
rdbptrrsDBProxyTargets = Lens.field @"dBProxyTargets"
{-# DEPRECATED rdbptrrsDBProxyTargets "Use generic-lens or generic-optics with 'dBProxyTargets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbptrrsResponseStatus :: Lens.Lens' RegisterDBProxyTargetsResponse Core.Int
rdbptrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rdbptrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
