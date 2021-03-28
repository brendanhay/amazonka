{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.RotateEncryptionKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rotates the encryption keys for a cluster.
module Network.AWS.Redshift.RotateEncryptionKey
    (
    -- * Creating a request
      RotateEncryptionKey (..)
    , mkRotateEncryptionKey
    -- ** Request lenses
    , rekClusterIdentifier

    -- * Destructuring the response
    , RotateEncryptionKeyResponse (..)
    , mkRotateEncryptionKeyResponse
    -- ** Response lenses
    , rekrrsCluster
    , rekrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkRotateEncryptionKey' smart constructor.
newtype RotateEncryptionKey = RotateEncryptionKey'
  { clusterIdentifier :: Core.Text
    -- ^ The unique identifier of the cluster that you want to rotate the encryption keys for.
--
-- Constraints: Must be the name of valid cluster that has encryption enabled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RotateEncryptionKey' value with any optional fields omitted.
mkRotateEncryptionKey
    :: Core.Text -- ^ 'clusterIdentifier'
    -> RotateEncryptionKey
mkRotateEncryptionKey clusterIdentifier
  = RotateEncryptionKey'{clusterIdentifier}

-- | The unique identifier of the cluster that you want to rotate the encryption keys for.
--
-- Constraints: Must be the name of valid cluster that has encryption enabled.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rekClusterIdentifier :: Lens.Lens' RotateEncryptionKey Core.Text
rekClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE rekClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

instance Core.ToQuery RotateEncryptionKey where
        toQuery RotateEncryptionKey{..}
          = Core.toQueryPair "Action" ("RotateEncryptionKey" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier

instance Core.ToHeaders RotateEncryptionKey where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RotateEncryptionKey where
        type Rs RotateEncryptionKey = RotateEncryptionKeyResponse
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
          = Response.receiveXMLWrapper "RotateEncryptionKeyResult"
              (\ s h x ->
                 RotateEncryptionKeyResponse' Core.<$>
                   (x Core..@? "Cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRotateEncryptionKeyResponse' smart constructor.
data RotateEncryptionKeyResponse = RotateEncryptionKeyResponse'
  { cluster :: Core.Maybe Types.Cluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RotateEncryptionKeyResponse' value with any optional fields omitted.
mkRotateEncryptionKeyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RotateEncryptionKeyResponse
mkRotateEncryptionKeyResponse responseStatus
  = RotateEncryptionKeyResponse'{cluster = Core.Nothing,
                                 responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rekrrsCluster :: Lens.Lens' RotateEncryptionKeyResponse (Core.Maybe Types.Cluster)
rekrrsCluster = Lens.field @"cluster"
{-# INLINEABLE rekrrsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rekrrsResponseStatus :: Lens.Lens' RotateEncryptionKeyResponse Core.Int
rekrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rekrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
