{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.StartMaintenance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Manually starts server maintenance. This command can be useful if an earlier maintenance attempt failed, and the underlying cause of maintenance failure has been resolved. The server is in an @UNDER_MAINTENANCE@ state while maintenance is in progress.
--
-- Maintenance can only be started on servers in @HEALTHY@ and @UNHEALTHY@ states. Otherwise, an @InvalidStateException@ is thrown. A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
module Network.AWS.OpsWorksCM.StartMaintenance
  ( -- * Creating a request
    StartMaintenance (..),
    mkStartMaintenance,

    -- ** Request lenses
    smServerName,
    smEngineAttributes,

    -- * Destructuring the response
    StartMaintenanceResponse (..),
    mkStartMaintenanceResponse,

    -- ** Response lenses
    smrrsServer,
    smrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorksCM.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartMaintenance' smart constructor.
data StartMaintenance = StartMaintenance'
  { -- | The name of the server on which to run maintenance.
    serverName :: Types.ServerName,
    -- | Engine attributes that are specific to the server on which you want to run maintenance.
    --
    -- __Attributes accepted in a StartMaintenance request for Chef__
    --
    --     * @CHEF_MAJOR_UPGRADE@ : If a Chef Automate server is eligible for upgrade to Chef Automate 2, add this engine attribute to a @StartMaintenance@ request and set the value to @true@ to upgrade the server to Chef Automate 2. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opscm-a2upgrade.html Upgrade an AWS OpsWorks for Chef Automate Server to Chef Automate 2> .
    engineAttributes :: Core.Maybe [Types.EngineAttribute]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartMaintenance' value with any optional fields omitted.
mkStartMaintenance ::
  -- | 'serverName'
  Types.ServerName ->
  StartMaintenance
mkStartMaintenance serverName =
  StartMaintenance' {serverName, engineAttributes = Core.Nothing}

-- | The name of the server on which to run maintenance.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smServerName :: Lens.Lens' StartMaintenance Types.ServerName
smServerName = Lens.field @"serverName"
{-# DEPRECATED smServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | Engine attributes that are specific to the server on which you want to run maintenance.
--
-- __Attributes accepted in a StartMaintenance request for Chef__
--
--     * @CHEF_MAJOR_UPGRADE@ : If a Chef Automate server is eligible for upgrade to Chef Automate 2, add this engine attribute to a @StartMaintenance@ request and set the value to @true@ to upgrade the server to Chef Automate 2. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opscm-a2upgrade.html Upgrade an AWS OpsWorks for Chef Automate Server to Chef Automate 2> .
--
--
--
-- /Note:/ Consider using 'engineAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smEngineAttributes :: Lens.Lens' StartMaintenance (Core.Maybe [Types.EngineAttribute])
smEngineAttributes = Lens.field @"engineAttributes"
{-# DEPRECATED smEngineAttributes "Use generic-lens or generic-optics with 'engineAttributes' instead." #-}

instance Core.FromJSON StartMaintenance where
  toJSON StartMaintenance {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ServerName" Core..= serverName),
            ("EngineAttributes" Core..=) Core.<$> engineAttributes
          ]
      )

instance Core.AWSRequest StartMaintenance where
  type Rs StartMaintenance = StartMaintenanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OpsWorksCM_V2016_11_01.StartMaintenance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartMaintenanceResponse'
            Core.<$> (x Core..:? "Server") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartMaintenanceResponse' smart constructor.
data StartMaintenanceResponse = StartMaintenanceResponse'
  { -- | Contains the response to a @StartMaintenance@ request.
    server :: Core.Maybe Types.Server,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartMaintenanceResponse' value with any optional fields omitted.
mkStartMaintenanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartMaintenanceResponse
mkStartMaintenanceResponse responseStatus =
  StartMaintenanceResponse' {server = Core.Nothing, responseStatus}

-- | Contains the response to a @StartMaintenance@ request.
--
-- /Note:/ Consider using 'server' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsServer :: Lens.Lens' StartMaintenanceResponse (Core.Maybe Types.Server)
smrrsServer = Lens.field @"server"
{-# DEPRECATED smrrsServer "Use generic-lens or generic-optics with 'server' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsResponseStatus :: Lens.Lens' StartMaintenanceResponse Core.Int
smrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED smrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
