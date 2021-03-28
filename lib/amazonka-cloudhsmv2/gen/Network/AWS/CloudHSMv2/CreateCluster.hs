{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.CreateCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new AWS CloudHSM cluster.
module Network.AWS.CloudHSMv2.CreateCluster
    (
    -- * Creating a request
      CreateCluster (..)
    , mkCreateCluster
    -- ** Request lenses
    , ccHsmType
    , ccSubnetIds
    , ccBackupRetentionPolicy
    , ccSourceBackupId
    , ccTagList

    -- * Destructuring the response
    , CreateClusterResponse (..)
    , mkCreateClusterResponse
    -- ** Response lenses
    , ccrrsCluster
    , ccrrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSMv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { hsmType :: Types.HsmType
    -- ^ The type of HSM to use in the cluster. Currently the only allowed value is @hsm1.medium@ .
  , subnetIds :: Core.NonEmpty Types.SubnetId
    -- ^ The identifiers (IDs) of the subnets where you are creating the cluster. You must specify at least one subnet. If you specify multiple subnets, they must meet the following criteria:
--
--
--     * All subnets must be in the same virtual private cloud (VPC).
--
--
--     * You can specify only one subnet per Availability Zone.
--
--
  , backupRetentionPolicy :: Core.Maybe Types.BackupRetentionPolicy
    -- ^ A policy that defines how the service retains backups.
  , sourceBackupId :: Core.Maybe Types.SourceBackupId
    -- ^ The identifier (ID) of the cluster backup to restore. Use this value to restore the cluster from a backup instead of creating a new cluster. To find the backup ID, use 'DescribeBackups' .
  , tagList :: Core.Maybe [Types.Tag]
    -- ^ Tags to apply to the CloudHSM cluster during creation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCluster' value with any optional fields omitted.
mkCreateCluster
    :: Types.HsmType -- ^ 'hsmType'
    -> Core.NonEmpty Types.SubnetId -- ^ 'subnetIds'
    -> CreateCluster
mkCreateCluster hsmType subnetIds
  = CreateCluster'{hsmType, subnetIds,
                   backupRetentionPolicy = Core.Nothing,
                   sourceBackupId = Core.Nothing, tagList = Core.Nothing}

-- | The type of HSM to use in the cluster. Currently the only allowed value is @hsm1.medium@ .
--
-- /Note:/ Consider using 'hsmType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccHsmType :: Lens.Lens' CreateCluster Types.HsmType
ccHsmType = Lens.field @"hsmType"
{-# INLINEABLE ccHsmType #-}
{-# DEPRECATED hsmType "Use generic-lens or generic-optics with 'hsmType' instead"  #-}

-- | The identifiers (IDs) of the subnets where you are creating the cluster. You must specify at least one subnet. If you specify multiple subnets, they must meet the following criteria:
--
--
--     * All subnets must be in the same virtual private cloud (VPC).
--
--
--     * You can specify only one subnet per Availability Zone.
--
--
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSubnetIds :: Lens.Lens' CreateCluster (Core.NonEmpty Types.SubnetId)
ccSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE ccSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | A policy that defines how the service retains backups.
--
-- /Note:/ Consider using 'backupRetentionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccBackupRetentionPolicy :: Lens.Lens' CreateCluster (Core.Maybe Types.BackupRetentionPolicy)
ccBackupRetentionPolicy = Lens.field @"backupRetentionPolicy"
{-# INLINEABLE ccBackupRetentionPolicy #-}
{-# DEPRECATED backupRetentionPolicy "Use generic-lens or generic-optics with 'backupRetentionPolicy' instead"  #-}

-- | The identifier (ID) of the cluster backup to restore. Use this value to restore the cluster from a backup instead of creating a new cluster. To find the backup ID, use 'DescribeBackups' .
--
-- /Note:/ Consider using 'sourceBackupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSourceBackupId :: Lens.Lens' CreateCluster (Core.Maybe Types.SourceBackupId)
ccSourceBackupId = Lens.field @"sourceBackupId"
{-# INLINEABLE ccSourceBackupId #-}
{-# DEPRECATED sourceBackupId "Use generic-lens or generic-optics with 'sourceBackupId' instead"  #-}

-- | Tags to apply to the CloudHSM cluster during creation.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTagList :: Lens.Lens' CreateCluster (Core.Maybe [Types.Tag])
ccTagList = Lens.field @"tagList"
{-# INLINEABLE ccTagList #-}
{-# DEPRECATED tagList "Use generic-lens or generic-optics with 'tagList' instead"  #-}

instance Core.ToQuery CreateCluster where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCluster where
        toHeaders CreateCluster{..}
          = Core.pure ("X-Amz-Target", "BaldrApiService.CreateCluster")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCluster where
        toJSON CreateCluster{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("HsmType" Core..= hsmType),
                  Core.Just ("SubnetIds" Core..= subnetIds),
                  ("BackupRetentionPolicy" Core..=) Core.<$> backupRetentionPolicy,
                  ("SourceBackupId" Core..=) Core.<$> sourceBackupId,
                  ("TagList" Core..=) Core.<$> tagList])

instance Core.AWSRequest CreateCluster where
        type Rs CreateCluster = CreateClusterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateClusterResponse' Core.<$>
                   (x Core..:? "Cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { cluster :: Core.Maybe Types.Cluster
    -- ^ Information about the cluster that was created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateClusterResponse' value with any optional fields omitted.
mkCreateClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateClusterResponse
mkCreateClusterResponse responseStatus
  = CreateClusterResponse'{cluster = Core.Nothing, responseStatus}

-- | Information about the cluster that was created.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsCluster :: Lens.Lens' CreateClusterResponse (Core.Maybe Types.Cluster)
ccrrsCluster = Lens.field @"cluster"
{-# INLINEABLE ccrrsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateClusterResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
