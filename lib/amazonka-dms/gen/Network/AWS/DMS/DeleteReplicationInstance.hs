{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DeleteReplicationInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified replication instance.
--
--
module Network.AWS.DMS.DeleteReplicationInstance
    (
    -- * Creating a request
      DeleteReplicationInstance (..)
    , mkDeleteReplicationInstance
    -- ** Request lenses
    , driReplicationInstanceArn

    -- * Destructuring the response
    , DeleteReplicationInstanceResponse (..)
    , mkDeleteReplicationInstanceResponse
    -- ** Response lenses
    , drirrsReplicationInstance
    , drirrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteReplicationInstance' smart constructor.
newtype DeleteReplicationInstance = DeleteReplicationInstance'
  { replicationInstanceArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the replication instance to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReplicationInstance' value with any optional fields omitted.
mkDeleteReplicationInstance
    :: Core.Text -- ^ 'replicationInstanceArn'
    -> DeleteReplicationInstance
mkDeleteReplicationInstance replicationInstanceArn
  = DeleteReplicationInstance'{replicationInstanceArn}

-- | The Amazon Resource Name (ARN) of the replication instance to be deleted.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driReplicationInstanceArn :: Lens.Lens' DeleteReplicationInstance Core.Text
driReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# INLINEABLE driReplicationInstanceArn #-}
{-# DEPRECATED replicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead"  #-}

instance Core.ToQuery DeleteReplicationInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteReplicationInstance where
        toHeaders DeleteReplicationInstance{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.DeleteReplicationInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteReplicationInstance where
        toJSON DeleteReplicationInstance{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ReplicationInstanceArn" Core..= replicationInstanceArn)])

instance Core.AWSRequest DeleteReplicationInstance where
        type Rs DeleteReplicationInstance =
             DeleteReplicationInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteReplicationInstanceResponse' Core.<$>
                   (x Core..:? "ReplicationInstance") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkDeleteReplicationInstanceResponse' smart constructor.
data DeleteReplicationInstanceResponse = DeleteReplicationInstanceResponse'
  { replicationInstance :: Core.Maybe Types.ReplicationInstance
    -- ^ The replication instance that was deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteReplicationInstanceResponse' value with any optional fields omitted.
mkDeleteReplicationInstanceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteReplicationInstanceResponse
mkDeleteReplicationInstanceResponse responseStatus
  = DeleteReplicationInstanceResponse'{replicationInstance =
                                         Core.Nothing,
                                       responseStatus}

-- | The replication instance that was deleted.
--
-- /Note:/ Consider using 'replicationInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drirrsReplicationInstance :: Lens.Lens' DeleteReplicationInstanceResponse (Core.Maybe Types.ReplicationInstance)
drirrsReplicationInstance = Lens.field @"replicationInstance"
{-# INLINEABLE drirrsReplicationInstance #-}
{-# DEPRECATED replicationInstance "Use generic-lens or generic-optics with 'replicationInstance' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drirrsResponseStatus :: Lens.Lens' DeleteReplicationInstanceResponse Core.Int
drirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
