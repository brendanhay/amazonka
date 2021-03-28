{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.CreateReplicationSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a replication subnet group given a list of the subnet IDs in a VPC.
module Network.AWS.DMS.CreateReplicationSubnetGroup
    (
    -- * Creating a request
      CreateReplicationSubnetGroup (..)
    , mkCreateReplicationSubnetGroup
    -- ** Request lenses
    , crsgReplicationSubnetGroupIdentifier
    , crsgReplicationSubnetGroupDescription
    , crsgSubnetIds
    , crsgTags

    -- * Destructuring the response
    , CreateReplicationSubnetGroupResponse (..)
    , mkCreateReplicationSubnetGroupResponse
    -- ** Response lenses
    , crsgrrsReplicationSubnetGroup
    , crsgrrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCreateReplicationSubnetGroup' smart constructor.
data CreateReplicationSubnetGroup = CreateReplicationSubnetGroup'
  { replicationSubnetGroupIdentifier :: Core.Text
    -- ^ The name for the replication subnet group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters, periods, spaces, underscores, or hyphens. Must not be "default".
-- Example: @mySubnetgroup@ 
  , replicationSubnetGroupDescription :: Core.Text
    -- ^ The description for the subnet group.
  , subnetIds :: [Core.Text]
    -- ^ One or more subnet IDs to be assigned to the subnet group.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ One or more tags to be assigned to the subnet group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReplicationSubnetGroup' value with any optional fields omitted.
mkCreateReplicationSubnetGroup
    :: Core.Text -- ^ 'replicationSubnetGroupIdentifier'
    -> Core.Text -- ^ 'replicationSubnetGroupDescription'
    -> CreateReplicationSubnetGroup
mkCreateReplicationSubnetGroup replicationSubnetGroupIdentifier
  replicationSubnetGroupDescription
  = CreateReplicationSubnetGroup'{replicationSubnetGroupIdentifier,
                                  replicationSubnetGroupDescription, subnetIds = Core.mempty,
                                  tags = Core.Nothing}

-- | The name for the replication subnet group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters, periods, spaces, underscores, or hyphens. Must not be "default".
-- Example: @mySubnetgroup@ 
--
-- /Note:/ Consider using 'replicationSubnetGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsgReplicationSubnetGroupIdentifier :: Lens.Lens' CreateReplicationSubnetGroup Core.Text
crsgReplicationSubnetGroupIdentifier = Lens.field @"replicationSubnetGroupIdentifier"
{-# INLINEABLE crsgReplicationSubnetGroupIdentifier #-}
{-# DEPRECATED replicationSubnetGroupIdentifier "Use generic-lens or generic-optics with 'replicationSubnetGroupIdentifier' instead"  #-}

-- | The description for the subnet group.
--
-- /Note:/ Consider using 'replicationSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsgReplicationSubnetGroupDescription :: Lens.Lens' CreateReplicationSubnetGroup Core.Text
crsgReplicationSubnetGroupDescription = Lens.field @"replicationSubnetGroupDescription"
{-# INLINEABLE crsgReplicationSubnetGroupDescription #-}
{-# DEPRECATED replicationSubnetGroupDescription "Use generic-lens or generic-optics with 'replicationSubnetGroupDescription' instead"  #-}

-- | One or more subnet IDs to be assigned to the subnet group.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsgSubnetIds :: Lens.Lens' CreateReplicationSubnetGroup [Core.Text]
crsgSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE crsgSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | One or more tags to be assigned to the subnet group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsgTags :: Lens.Lens' CreateReplicationSubnetGroup (Core.Maybe [Types.Tag])
crsgTags = Lens.field @"tags"
{-# INLINEABLE crsgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateReplicationSubnetGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateReplicationSubnetGroup where
        toHeaders CreateReplicationSubnetGroup{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.CreateReplicationSubnetGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateReplicationSubnetGroup where
        toJSON CreateReplicationSubnetGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ReplicationSubnetGroupIdentifier" Core..=
                       replicationSubnetGroupIdentifier),
                  Core.Just
                    ("ReplicationSubnetGroupDescription" Core..=
                       replicationSubnetGroupDescription),
                  Core.Just ("SubnetIds" Core..= subnetIds),
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateReplicationSubnetGroup where
        type Rs CreateReplicationSubnetGroup =
             CreateReplicationSubnetGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateReplicationSubnetGroupResponse' Core.<$>
                   (x Core..:? "ReplicationSubnetGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkCreateReplicationSubnetGroupResponse' smart constructor.
data CreateReplicationSubnetGroupResponse = CreateReplicationSubnetGroupResponse'
  { replicationSubnetGroup :: Core.Maybe Types.ReplicationSubnetGroup
    -- ^ The replication subnet group that was created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReplicationSubnetGroupResponse' value with any optional fields omitted.
mkCreateReplicationSubnetGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateReplicationSubnetGroupResponse
mkCreateReplicationSubnetGroupResponse responseStatus
  = CreateReplicationSubnetGroupResponse'{replicationSubnetGroup =
                                            Core.Nothing,
                                          responseStatus}

-- | The replication subnet group that was created.
--
-- /Note:/ Consider using 'replicationSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsgrrsReplicationSubnetGroup :: Lens.Lens' CreateReplicationSubnetGroupResponse (Core.Maybe Types.ReplicationSubnetGroup)
crsgrrsReplicationSubnetGroup = Lens.field @"replicationSubnetGroup"
{-# INLINEABLE crsgrrsReplicationSubnetGroup #-}
{-# DEPRECATED replicationSubnetGroup "Use generic-lens or generic-optics with 'replicationSubnetGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsgrrsResponseStatus :: Lens.Lens' CreateReplicationSubnetGroupResponse Core.Int
crsgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crsgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
