{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.ReloadTables
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reloads the target database table with the source data. 
module Network.AWS.DMS.ReloadTables
    (
    -- * Creating a request
      ReloadTables (..)
    , mkReloadTables
    -- ** Request lenses
    , rtsReplicationTaskArn
    , rtsTablesToReload
    , rtsReloadOption

    -- * Destructuring the response
    , ReloadTablesResponse (..)
    , mkReloadTablesResponse
    -- ** Response lenses
    , rtrrsReplicationTaskArn
    , rtrrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkReloadTables' smart constructor.
data ReloadTables = ReloadTables'
  { replicationTaskArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the replication task. 
  , tablesToReload :: [Types.TableToReload]
    -- ^ The name and schema of the table to be reloaded. 
  , reloadOption :: Core.Maybe Types.ReloadOptionValue
    -- ^ Options for reload. Specify @data-reload@ to reload the data and re-validate it if validation is enabled. Specify @validate-only@ to re-validate the table. This option applies only when validation is enabled for the task. 
--
-- Valid values: data-reload, validate-only
-- Default value is data-reload.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReloadTables' value with any optional fields omitted.
mkReloadTables
    :: Core.Text -- ^ 'replicationTaskArn'
    -> ReloadTables
mkReloadTables replicationTaskArn
  = ReloadTables'{replicationTaskArn, tablesToReload = Core.mempty,
                  reloadOption = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the replication task. 
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsReplicationTaskArn :: Lens.Lens' ReloadTables Core.Text
rtsReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# INLINEABLE rtsReplicationTaskArn #-}
{-# DEPRECATED replicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead"  #-}

-- | The name and schema of the table to be reloaded. 
--
-- /Note:/ Consider using 'tablesToReload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsTablesToReload :: Lens.Lens' ReloadTables [Types.TableToReload]
rtsTablesToReload = Lens.field @"tablesToReload"
{-# INLINEABLE rtsTablesToReload #-}
{-# DEPRECATED tablesToReload "Use generic-lens or generic-optics with 'tablesToReload' instead"  #-}

-- | Options for reload. Specify @data-reload@ to reload the data and re-validate it if validation is enabled. Specify @validate-only@ to re-validate the table. This option applies only when validation is enabled for the task. 
--
-- Valid values: data-reload, validate-only
-- Default value is data-reload.
--
-- /Note:/ Consider using 'reloadOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsReloadOption :: Lens.Lens' ReloadTables (Core.Maybe Types.ReloadOptionValue)
rtsReloadOption = Lens.field @"reloadOption"
{-# INLINEABLE rtsReloadOption #-}
{-# DEPRECATED reloadOption "Use generic-lens or generic-optics with 'reloadOption' instead"  #-}

instance Core.ToQuery ReloadTables where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ReloadTables where
        toHeaders ReloadTables{..}
          = Core.pure ("X-Amz-Target", "AmazonDMSv20160101.ReloadTables")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ReloadTables where
        toJSON ReloadTables{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ReplicationTaskArn" Core..= replicationTaskArn),
                  Core.Just ("TablesToReload" Core..= tablesToReload),
                  ("ReloadOption" Core..=) Core.<$> reloadOption])

instance Core.AWSRequest ReloadTables where
        type Rs ReloadTables = ReloadTablesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ReloadTablesResponse' Core.<$>
                   (x Core..:? "ReplicationTaskArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkReloadTablesResponse' smart constructor.
data ReloadTablesResponse = ReloadTablesResponse'
  { replicationTaskArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the replication task. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReloadTablesResponse' value with any optional fields omitted.
mkReloadTablesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ReloadTablesResponse
mkReloadTablesResponse responseStatus
  = ReloadTablesResponse'{replicationTaskArn = Core.Nothing,
                          responseStatus}

-- | The Amazon Resource Name (ARN) of the replication task. 
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsReplicationTaskArn :: Lens.Lens' ReloadTablesResponse (Core.Maybe Core.Text)
rtrrsReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# INLINEABLE rtrrsReplicationTaskArn #-}
{-# DEPRECATED replicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsResponseStatus :: Lens.Lens' ReloadTablesResponse Core.Int
rtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
