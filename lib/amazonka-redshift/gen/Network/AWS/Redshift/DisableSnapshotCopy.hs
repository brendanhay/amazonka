{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DisableSnapshotCopy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the automatic copying of snapshots from one region to another region for a specified cluster.
--
-- If your cluster and its snapshots are encrypted using a customer master key (CMK) from AWS KMS, use 'DeleteSnapshotCopyGrant' to delete the grant that grants Amazon Redshift permission to the CMK in the destination region. 
module Network.AWS.Redshift.DisableSnapshotCopy
    (
    -- * Creating a request
      DisableSnapshotCopy (..)
    , mkDisableSnapshotCopy
    -- ** Request lenses
    , dscClusterIdentifier

    -- * Destructuring the response
    , DisableSnapshotCopyResponse (..)
    , mkDisableSnapshotCopyResponse
    -- ** Response lenses
    , dscrrsCluster
    , dscrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDisableSnapshotCopy' smart constructor.
newtype DisableSnapshotCopy = DisableSnapshotCopy'
  { clusterIdentifier :: Core.Text
    -- ^ The unique identifier of the source cluster that you want to disable copying of snapshots to a destination region.
--
-- Constraints: Must be the valid name of an existing cluster that has cross-region snapshot copy enabled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableSnapshotCopy' value with any optional fields omitted.
mkDisableSnapshotCopy
    :: Core.Text -- ^ 'clusterIdentifier'
    -> DisableSnapshotCopy
mkDisableSnapshotCopy clusterIdentifier
  = DisableSnapshotCopy'{clusterIdentifier}

-- | The unique identifier of the source cluster that you want to disable copying of snapshots to a destination region.
--
-- Constraints: Must be the valid name of an existing cluster that has cross-region snapshot copy enabled.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscClusterIdentifier :: Lens.Lens' DisableSnapshotCopy Core.Text
dscClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE dscClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

instance Core.ToQuery DisableSnapshotCopy where
        toQuery DisableSnapshotCopy{..}
          = Core.toQueryPair "Action" ("DisableSnapshotCopy" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier

instance Core.ToHeaders DisableSnapshotCopy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisableSnapshotCopy where
        type Rs DisableSnapshotCopy = DisableSnapshotCopyResponse
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
          = Response.receiveXMLWrapper "DisableSnapshotCopyResult"
              (\ s h x ->
                 DisableSnapshotCopyResponse' Core.<$>
                   (x Core..@? "Cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisableSnapshotCopyResponse' smart constructor.
data DisableSnapshotCopyResponse = DisableSnapshotCopyResponse'
  { cluster :: Core.Maybe Types.Cluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DisableSnapshotCopyResponse' value with any optional fields omitted.
mkDisableSnapshotCopyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisableSnapshotCopyResponse
mkDisableSnapshotCopyResponse responseStatus
  = DisableSnapshotCopyResponse'{cluster = Core.Nothing,
                                 responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrrsCluster :: Lens.Lens' DisableSnapshotCopyResponse (Core.Maybe Types.Cluster)
dscrrsCluster = Lens.field @"cluster"
{-# INLINEABLE dscrrsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrrsResponseStatus :: Lens.Lens' DisableSnapshotCopyResponse Core.Int
dscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
