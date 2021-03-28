{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.ModifyReplicationSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for the specified replication subnet group.
module Network.AWS.DMS.ModifyReplicationSubnetGroup
    (
    -- * Creating a request
      ModifyReplicationSubnetGroup (..)
    , mkModifyReplicationSubnetGroup
    -- ** Request lenses
    , mrsgReplicationSubnetGroupIdentifier
    , mrsgSubnetIds
    , mrsgReplicationSubnetGroupDescription

    -- * Destructuring the response
    , ModifyReplicationSubnetGroupResponse (..)
    , mkModifyReplicationSubnetGroupResponse
    -- ** Response lenses
    , mrsgrrsReplicationSubnetGroup
    , mrsgrrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkModifyReplicationSubnetGroup' smart constructor.
data ModifyReplicationSubnetGroup = ModifyReplicationSubnetGroup'
  { replicationSubnetGroupIdentifier :: Core.Text
    -- ^ The name of the replication instance subnet group.
  , subnetIds :: [Core.Text]
    -- ^ A list of subnet IDs.
  , replicationSubnetGroupDescription :: Core.Maybe Core.Text
    -- ^ A description for the replication instance subnet group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyReplicationSubnetGroup' value with any optional fields omitted.
mkModifyReplicationSubnetGroup
    :: Core.Text -- ^ 'replicationSubnetGroupIdentifier'
    -> ModifyReplicationSubnetGroup
mkModifyReplicationSubnetGroup replicationSubnetGroupIdentifier
  = ModifyReplicationSubnetGroup'{replicationSubnetGroupIdentifier,
                                  subnetIds = Core.mempty,
                                  replicationSubnetGroupDescription = Core.Nothing}

-- | The name of the replication instance subnet group.
--
-- /Note:/ Consider using 'replicationSubnetGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsgReplicationSubnetGroupIdentifier :: Lens.Lens' ModifyReplicationSubnetGroup Core.Text
mrsgReplicationSubnetGroupIdentifier = Lens.field @"replicationSubnetGroupIdentifier"
{-# INLINEABLE mrsgReplicationSubnetGroupIdentifier #-}
{-# DEPRECATED replicationSubnetGroupIdentifier "Use generic-lens or generic-optics with 'replicationSubnetGroupIdentifier' instead"  #-}

-- | A list of subnet IDs.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsgSubnetIds :: Lens.Lens' ModifyReplicationSubnetGroup [Core.Text]
mrsgSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE mrsgSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | A description for the replication instance subnet group.
--
-- /Note:/ Consider using 'replicationSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsgReplicationSubnetGroupDescription :: Lens.Lens' ModifyReplicationSubnetGroup (Core.Maybe Core.Text)
mrsgReplicationSubnetGroupDescription = Lens.field @"replicationSubnetGroupDescription"
{-# INLINEABLE mrsgReplicationSubnetGroupDescription #-}
{-# DEPRECATED replicationSubnetGroupDescription "Use generic-lens or generic-optics with 'replicationSubnetGroupDescription' instead"  #-}

instance Core.ToQuery ModifyReplicationSubnetGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ModifyReplicationSubnetGroup where
        toHeaders ModifyReplicationSubnetGroup{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.ModifyReplicationSubnetGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ModifyReplicationSubnetGroup where
        toJSON ModifyReplicationSubnetGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ReplicationSubnetGroupIdentifier" Core..=
                       replicationSubnetGroupIdentifier),
                  Core.Just ("SubnetIds" Core..= subnetIds),
                  ("ReplicationSubnetGroupDescription" Core..=) Core.<$>
                    replicationSubnetGroupDescription])

instance Core.AWSRequest ModifyReplicationSubnetGroup where
        type Rs ModifyReplicationSubnetGroup =
             ModifyReplicationSubnetGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ModifyReplicationSubnetGroupResponse' Core.<$>
                   (x Core..:? "ReplicationSubnetGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkModifyReplicationSubnetGroupResponse' smart constructor.
data ModifyReplicationSubnetGroupResponse = ModifyReplicationSubnetGroupResponse'
  { replicationSubnetGroup :: Core.Maybe Types.ReplicationSubnetGroup
    -- ^ The modified replication subnet group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyReplicationSubnetGroupResponse' value with any optional fields omitted.
mkModifyReplicationSubnetGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyReplicationSubnetGroupResponse
mkModifyReplicationSubnetGroupResponse responseStatus
  = ModifyReplicationSubnetGroupResponse'{replicationSubnetGroup =
                                            Core.Nothing,
                                          responseStatus}

-- | The modified replication subnet group.
--
-- /Note:/ Consider using 'replicationSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsgrrsReplicationSubnetGroup :: Lens.Lens' ModifyReplicationSubnetGroupResponse (Core.Maybe Types.ReplicationSubnetGroup)
mrsgrrsReplicationSubnetGroup = Lens.field @"replicationSubnetGroup"
{-# INLINEABLE mrsgrrsReplicationSubnetGroup #-}
{-# DEPRECATED replicationSubnetGroup "Use generic-lens or generic-optics with 'replicationSubnetGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsgrrsResponseStatus :: Lens.Lens' ModifyReplicationSubnetGroupResponse Core.Int
mrsgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mrsgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
