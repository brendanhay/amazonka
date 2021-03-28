{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateServer (..)
    , mkUpdateServer
    -- ** Request lenses
    , usServerName
    , usBackupRetentionCount
    , usDisableAutomatedBackup
    , usPreferredBackupWindow
    , usPreferredMaintenanceWindow

    -- * Destructuring the response
    , UpdateServerResponse (..)
    , mkUpdateServerResponse
    -- ** Response lenses
    , usrrsServer
    , usrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorksCM.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateServer' smart constructor.
data UpdateServer = UpdateServer'
  { serverName :: Types.ServerName
    -- ^ The name of the server to update. 
  , backupRetentionCount :: Core.Maybe Core.Int
    -- ^ Sets the number of automated backups that you want to keep. 
  , disableAutomatedBackup :: Core.Maybe Core.Bool
    -- ^ Setting DisableAutomatedBackup to @true@ disables automated or scheduled backups. Automated backups are enabled by default. 
  , preferredBackupWindow :: Core.Maybe Types.TimeWindowDefinition
  , preferredMaintenanceWindow :: Core.Maybe Types.TimeWindowDefinition
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateServer' value with any optional fields omitted.
mkUpdateServer
    :: Types.ServerName -- ^ 'serverName'
    -> UpdateServer
mkUpdateServer serverName
  = UpdateServer'{serverName, backupRetentionCount = Core.Nothing,
                  disableAutomatedBackup = Core.Nothing,
                  preferredBackupWindow = Core.Nothing,
                  preferredMaintenanceWindow = Core.Nothing}

-- | The name of the server to update. 
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usServerName :: Lens.Lens' UpdateServer Types.ServerName
usServerName = Lens.field @"serverName"
{-# INLINEABLE usServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

-- | Sets the number of automated backups that you want to keep. 
--
-- /Note:/ Consider using 'backupRetentionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usBackupRetentionCount :: Lens.Lens' UpdateServer (Core.Maybe Core.Int)
usBackupRetentionCount = Lens.field @"backupRetentionCount"
{-# INLINEABLE usBackupRetentionCount #-}
{-# DEPRECATED backupRetentionCount "Use generic-lens or generic-optics with 'backupRetentionCount' instead"  #-}

-- | Setting DisableAutomatedBackup to @true@ disables automated or scheduled backups. Automated backups are enabled by default. 
--
-- /Note:/ Consider using 'disableAutomatedBackup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDisableAutomatedBackup :: Lens.Lens' UpdateServer (Core.Maybe Core.Bool)
usDisableAutomatedBackup = Lens.field @"disableAutomatedBackup"
{-# INLINEABLE usDisableAutomatedBackup #-}
{-# DEPRECATED disableAutomatedBackup "Use generic-lens or generic-optics with 'disableAutomatedBackup' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'preferredBackupWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usPreferredBackupWindow :: Lens.Lens' UpdateServer (Core.Maybe Types.TimeWindowDefinition)
usPreferredBackupWindow = Lens.field @"preferredBackupWindow"
{-# INLINEABLE usPreferredBackupWindow #-}
{-# DEPRECATED preferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usPreferredMaintenanceWindow :: Lens.Lens' UpdateServer (Core.Maybe Types.TimeWindowDefinition)
usPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# INLINEABLE usPreferredMaintenanceWindow #-}
{-# DEPRECATED preferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead"  #-}

instance Core.ToQuery UpdateServer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateServer where
        toHeaders UpdateServer{..}
          = Core.pure ("X-Amz-Target", "OpsWorksCM_V2016_11_01.UpdateServer")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateServer where
        toJSON UpdateServer{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ServerName" Core..= serverName),
                  ("BackupRetentionCount" Core..=) Core.<$> backupRetentionCount,
                  ("DisableAutomatedBackup" Core..=) Core.<$> disableAutomatedBackup,
                  ("PreferredBackupWindow" Core..=) Core.<$> preferredBackupWindow,
                  ("PreferredMaintenanceWindow" Core..=) Core.<$>
                    preferredMaintenanceWindow])

instance Core.AWSRequest UpdateServer where
        type Rs UpdateServer = UpdateServerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateServerResponse' Core.<$>
                   (x Core..:? "Server") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateServerResponse' smart constructor.
data UpdateServerResponse = UpdateServerResponse'
  { server :: Core.Maybe Types.Server
    -- ^ Contains the response to a @UpdateServer@ request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateServerResponse' value with any optional fields omitted.
mkUpdateServerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateServerResponse
mkUpdateServerResponse responseStatus
  = UpdateServerResponse'{server = Core.Nothing, responseStatus}

-- | Contains the response to a @UpdateServer@ request. 
--
-- /Note:/ Consider using 'server' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsServer :: Lens.Lens' UpdateServerResponse (Core.Maybe Types.Server)
usrrsServer = Lens.field @"server"
{-# INLINEABLE usrrsServer #-}
{-# DEPRECATED server "Use generic-lens or generic-optics with 'server' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UpdateServerResponse Core.Int
usrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
