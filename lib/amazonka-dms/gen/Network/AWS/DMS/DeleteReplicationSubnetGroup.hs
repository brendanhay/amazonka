{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DeleteReplicationSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subnet group.
module Network.AWS.DMS.DeleteReplicationSubnetGroup
    (
    -- * Creating a request
      DeleteReplicationSubnetGroup (..)
    , mkDeleteReplicationSubnetGroup
    -- ** Request lenses
    , drsgReplicationSubnetGroupIdentifier

    -- * Destructuring the response
    , DeleteReplicationSubnetGroupResponse (..)
    , mkDeleteReplicationSubnetGroupResponse
    -- ** Response lenses
    , drsgrrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteReplicationSubnetGroup' smart constructor.
newtype DeleteReplicationSubnetGroup = DeleteReplicationSubnetGroup'
  { replicationSubnetGroupIdentifier :: Core.Text
    -- ^ The subnet group name of the replication instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReplicationSubnetGroup' value with any optional fields omitted.
mkDeleteReplicationSubnetGroup
    :: Core.Text -- ^ 'replicationSubnetGroupIdentifier'
    -> DeleteReplicationSubnetGroup
mkDeleteReplicationSubnetGroup replicationSubnetGroupIdentifier
  = DeleteReplicationSubnetGroup'{replicationSubnetGroupIdentifier}

-- | The subnet group name of the replication instance.
--
-- /Note:/ Consider using 'replicationSubnetGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsgReplicationSubnetGroupIdentifier :: Lens.Lens' DeleteReplicationSubnetGroup Core.Text
drsgReplicationSubnetGroupIdentifier = Lens.field @"replicationSubnetGroupIdentifier"
{-# INLINEABLE drsgReplicationSubnetGroupIdentifier #-}
{-# DEPRECATED replicationSubnetGroupIdentifier "Use generic-lens or generic-optics with 'replicationSubnetGroupIdentifier' instead"  #-}

instance Core.ToQuery DeleteReplicationSubnetGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteReplicationSubnetGroup where
        toHeaders DeleteReplicationSubnetGroup{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.DeleteReplicationSubnetGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteReplicationSubnetGroup where
        toJSON DeleteReplicationSubnetGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ReplicationSubnetGroupIdentifier" Core..=
                       replicationSubnetGroupIdentifier)])

instance Core.AWSRequest DeleteReplicationSubnetGroup where
        type Rs DeleteReplicationSubnetGroup =
             DeleteReplicationSubnetGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteReplicationSubnetGroupResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkDeleteReplicationSubnetGroupResponse' smart constructor.
newtype DeleteReplicationSubnetGroupResponse = DeleteReplicationSubnetGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReplicationSubnetGroupResponse' value with any optional fields omitted.
mkDeleteReplicationSubnetGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteReplicationSubnetGroupResponse
mkDeleteReplicationSubnetGroupResponse responseStatus
  = DeleteReplicationSubnetGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsgrrsResponseStatus :: Lens.Lens' DeleteReplicationSubnetGroupResponse Core.Int
drsgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
