{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.CancelCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a cluster job. You can only cancel a cluster job while it's in the @AwaitingQuorum@ status. You'll have at least an hour after creating a cluster job to cancel it.
module Network.AWS.Snowball.CancelCluster
    (
    -- * Creating a request
      CancelCluster (..)
    , mkCancelCluster
    -- ** Request lenses
    , ccClusterId

    -- * Destructuring the response
    , CancelClusterResponse (..)
    , mkCancelClusterResponse
    -- ** Response lenses
    , ccrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkCancelCluster' smart constructor.
newtype CancelCluster = CancelCluster'
  { clusterId :: Types.ClusterId
    -- ^ The 39-character ID for the cluster that you want to cancel, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelCluster' value with any optional fields omitted.
mkCancelCluster
    :: Types.ClusterId -- ^ 'clusterId'
    -> CancelCluster
mkCancelCluster clusterId = CancelCluster'{clusterId}

-- | The 39-character ID for the cluster that you want to cancel, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClusterId :: Lens.Lens' CancelCluster Types.ClusterId
ccClusterId = Lens.field @"clusterId"
{-# INLINEABLE ccClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

instance Core.ToQuery CancelCluster where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CancelCluster where
        toHeaders CancelCluster{..}
          = Core.pure
              ("X-Amz-Target", "AWSIESnowballJobManagementService.CancelCluster")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CancelCluster where
        toJSON CancelCluster{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ClusterId" Core..= clusterId)])

instance Core.AWSRequest CancelCluster where
        type Rs CancelCluster = CancelClusterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CancelClusterResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCancelClusterResponse' smart constructor.
newtype CancelClusterResponse = CancelClusterResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelClusterResponse' value with any optional fields omitted.
mkCancelClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelClusterResponse
mkCancelClusterResponse responseStatus
  = CancelClusterResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CancelClusterResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
