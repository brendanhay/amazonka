{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.ModifyCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies AWS CloudHSM cluster.
module Network.AWS.CloudHSMv2.ModifyCluster
    (
    -- * Creating a request
      ModifyCluster (..)
    , mkModifyCluster
    -- ** Request lenses
    , mcBackupRetentionPolicy
    , mcClusterId

    -- * Destructuring the response
    , ModifyClusterResponse (..)
    , mkModifyClusterResponse
    -- ** Response lenses
    , mcrrsCluster
    , mcrrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSMv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyCluster' smart constructor.
data ModifyCluster = ModifyCluster'
  { backupRetentionPolicy :: Types.BackupRetentionPolicy
    -- ^ A policy that defines how the service retains backups.
  , clusterId :: Types.ClusterId
    -- ^ The identifier (ID) of the cluster that you want to modify. To find the cluster ID, use 'DescribeClusters' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyCluster' value with any optional fields omitted.
mkModifyCluster
    :: Types.BackupRetentionPolicy -- ^ 'backupRetentionPolicy'
    -> Types.ClusterId -- ^ 'clusterId'
    -> ModifyCluster
mkModifyCluster backupRetentionPolicy clusterId
  = ModifyCluster'{backupRetentionPolicy, clusterId}

-- | A policy that defines how the service retains backups.
--
-- /Note:/ Consider using 'backupRetentionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcBackupRetentionPolicy :: Lens.Lens' ModifyCluster Types.BackupRetentionPolicy
mcBackupRetentionPolicy = Lens.field @"backupRetentionPolicy"
{-# INLINEABLE mcBackupRetentionPolicy #-}
{-# DEPRECATED backupRetentionPolicy "Use generic-lens or generic-optics with 'backupRetentionPolicy' instead"  #-}

-- | The identifier (ID) of the cluster that you want to modify. To find the cluster ID, use 'DescribeClusters' .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcClusterId :: Lens.Lens' ModifyCluster Types.ClusterId
mcClusterId = Lens.field @"clusterId"
{-# INLINEABLE mcClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

instance Core.ToQuery ModifyCluster where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ModifyCluster where
        toHeaders ModifyCluster{..}
          = Core.pure ("X-Amz-Target", "BaldrApiService.ModifyCluster")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ModifyCluster where
        toJSON ModifyCluster{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("BackupRetentionPolicy" Core..= backupRetentionPolicy),
                  Core.Just ("ClusterId" Core..= clusterId)])

instance Core.AWSRequest ModifyCluster where
        type Rs ModifyCluster = ModifyClusterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ModifyClusterResponse' Core.<$>
                   (x Core..:? "Cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyClusterResponse' smart constructor.
data ModifyClusterResponse = ModifyClusterResponse'
  { cluster :: Core.Maybe Types.Cluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyClusterResponse' value with any optional fields omitted.
mkModifyClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyClusterResponse
mkModifyClusterResponse responseStatus
  = ModifyClusterResponse'{cluster = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrrsCluster :: Lens.Lens' ModifyClusterResponse (Core.Maybe Types.Cluster)
mcrrsCluster = Lens.field @"cluster"
{-# INLINEABLE mcrrsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrrsResponseStatus :: Lens.Lens' ModifyClusterResponse Core.Int
mcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
