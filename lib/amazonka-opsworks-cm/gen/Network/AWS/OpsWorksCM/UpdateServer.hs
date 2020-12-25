{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.UpdateServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates settings for a server.
--
-- This operation is synchronous.
module Network.AWS.OpsWorksCM.UpdateServer
  ( -- * Creating a request
    UpdateServer (..),
    mkUpdateServer,

    -- ** Request lenses
    usServerName,
    usBackupRetentionCount,
    usDisableAutomatedBackup,
    usPreferredBackupWindow,
    usPreferredMaintenanceWindow,

    -- * Destructuring the response
    UpdateServerResponse (..),
    mkUpdateServerResponse,

    -- ** Response lenses
    usrrsServer,
    usrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorksCM.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateServer' smart constructor.
data UpdateServer = UpdateServer'
  { -- | The name of the server to update.
    serverName :: Types.ServerName,
    -- | Sets the number of automated backups that you want to keep.
    backupRetentionCount :: Core.Maybe Core.Int,
    -- | Setting DisableAutomatedBackup to @true@ disables automated or scheduled backups. Automated backups are enabled by default.
    disableAutomatedBackup :: Core.Maybe Core.Bool,
    preferredBackupWindow :: Core.Maybe Types.TimeWindowDefinition,
    preferredMaintenanceWindow :: Core.Maybe Types.TimeWindowDefinition
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateServer' value with any optional fields omitted.
mkUpdateServer ::
  -- | 'serverName'
  Types.ServerName ->
  UpdateServer
mkUpdateServer serverName =
  UpdateServer'
    { serverName,
      backupRetentionCount = Core.Nothing,
      disableAutomatedBackup = Core.Nothing,
      preferredBackupWindow = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing
    }

-- | The name of the server to update.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usServerName :: Lens.Lens' UpdateServer Types.ServerName
usServerName = Lens.field @"serverName"
{-# DEPRECATED usServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | Sets the number of automated backups that you want to keep.
--
-- /Note:/ Consider using 'backupRetentionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usBackupRetentionCount :: Lens.Lens' UpdateServer (Core.Maybe Core.Int)
usBackupRetentionCount = Lens.field @"backupRetentionCount"
{-# DEPRECATED usBackupRetentionCount "Use generic-lens or generic-optics with 'backupRetentionCount' instead." #-}

-- | Setting DisableAutomatedBackup to @true@ disables automated or scheduled backups. Automated backups are enabled by default.
--
-- /Note:/ Consider using 'disableAutomatedBackup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDisableAutomatedBackup :: Lens.Lens' UpdateServer (Core.Maybe Core.Bool)
usDisableAutomatedBackup = Lens.field @"disableAutomatedBackup"
{-# DEPRECATED usDisableAutomatedBackup "Use generic-lens or generic-optics with 'disableAutomatedBackup' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'preferredBackupWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usPreferredBackupWindow :: Lens.Lens' UpdateServer (Core.Maybe Types.TimeWindowDefinition)
usPreferredBackupWindow = Lens.field @"preferredBackupWindow"
{-# DEPRECATED usPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usPreferredMaintenanceWindow :: Lens.Lens' UpdateServer (Core.Maybe Types.TimeWindowDefinition)
usPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED usPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

instance Core.FromJSON UpdateServer where
  toJSON UpdateServer {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ServerName" Core..= serverName),
            ("BackupRetentionCount" Core..=) Core.<$> backupRetentionCount,
            ("DisableAutomatedBackup" Core..=) Core.<$> disableAutomatedBackup,
            ("PreferredBackupWindow" Core..=) Core.<$> preferredBackupWindow,
            ("PreferredMaintenanceWindow" Core..=)
              Core.<$> preferredMaintenanceWindow
          ]
      )

instance Core.AWSRequest UpdateServer where
  type Rs UpdateServer = UpdateServerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorksCM_V2016_11_01.UpdateServer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServerResponse'
            Core.<$> (x Core..:? "Server") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateServerResponse' smart constructor.
data UpdateServerResponse = UpdateServerResponse'
  { -- | Contains the response to a @UpdateServer@ request.
    server :: Core.Maybe Types.Server,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateServerResponse' value with any optional fields omitted.
mkUpdateServerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateServerResponse
mkUpdateServerResponse responseStatus =
  UpdateServerResponse' {server = Core.Nothing, responseStatus}

-- | Contains the response to a @UpdateServer@ request.
--
-- /Note:/ Consider using 'server' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsServer :: Lens.Lens' UpdateServerResponse (Core.Maybe Types.Server)
usrrsServer = Lens.field @"server"
{-# DEPRECATED usrrsServer "Use generic-lens or generic-optics with 'server' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UpdateServerResponse Core.Int
usrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
