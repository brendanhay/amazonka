{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateReplicationSubnetGroup (..),
    mkCreateReplicationSubnetGroup,

    -- ** Request lenses
    crsgReplicationSubnetGroupIdentifier,
    crsgReplicationSubnetGroupDescription,
    crsgSubnetIds,
    crsgTags,

    -- * Destructuring the response
    CreateReplicationSubnetGroupResponse (..),
    mkCreateReplicationSubnetGroupResponse,

    -- ** Response lenses
    crsgrrsReplicationSubnetGroup,
    crsgrrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCreateReplicationSubnetGroup' smart constructor.
data CreateReplicationSubnetGroup = CreateReplicationSubnetGroup'
  { -- | The name for the replication subnet group. This value is stored as a lowercase string.
    --
    -- Constraints: Must contain no more than 255 alphanumeric characters, periods, spaces, underscores, or hyphens. Must not be "default".
    -- Example: @mySubnetgroup@
    replicationSubnetGroupIdentifier :: Types.ReplicationSubnetGroupIdentifier,
    -- | The description for the subnet group.
    replicationSubnetGroupDescription :: Types.ReplicationSubnetGroupDescription,
    -- | One or more subnet IDs to be assigned to the subnet group.
    subnetIds :: [Types.String],
    -- | One or more tags to be assigned to the subnet group.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReplicationSubnetGroup' value with any optional fields omitted.
mkCreateReplicationSubnetGroup ::
  -- | 'replicationSubnetGroupIdentifier'
  Types.ReplicationSubnetGroupIdentifier ->
  -- | 'replicationSubnetGroupDescription'
  Types.ReplicationSubnetGroupDescription ->
  CreateReplicationSubnetGroup
mkCreateReplicationSubnetGroup
  replicationSubnetGroupIdentifier
  replicationSubnetGroupDescription =
    CreateReplicationSubnetGroup'
      { replicationSubnetGroupIdentifier,
        replicationSubnetGroupDescription,
        subnetIds = Core.mempty,
        tags = Core.Nothing
      }

-- | The name for the replication subnet group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters, periods, spaces, underscores, or hyphens. Must not be "default".
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'replicationSubnetGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsgReplicationSubnetGroupIdentifier :: Lens.Lens' CreateReplicationSubnetGroup Types.ReplicationSubnetGroupIdentifier
crsgReplicationSubnetGroupIdentifier = Lens.field @"replicationSubnetGroupIdentifier"
{-# DEPRECATED crsgReplicationSubnetGroupIdentifier "Use generic-lens or generic-optics with 'replicationSubnetGroupIdentifier' instead." #-}

-- | The description for the subnet group.
--
-- /Note:/ Consider using 'replicationSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsgReplicationSubnetGroupDescription :: Lens.Lens' CreateReplicationSubnetGroup Types.ReplicationSubnetGroupDescription
crsgReplicationSubnetGroupDescription = Lens.field @"replicationSubnetGroupDescription"
{-# DEPRECATED crsgReplicationSubnetGroupDescription "Use generic-lens or generic-optics with 'replicationSubnetGroupDescription' instead." #-}

-- | One or more subnet IDs to be assigned to the subnet group.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsgSubnetIds :: Lens.Lens' CreateReplicationSubnetGroup [Types.String]
crsgSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED crsgSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | One or more tags to be assigned to the subnet group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsgTags :: Lens.Lens' CreateReplicationSubnetGroup (Core.Maybe [Types.Tag])
crsgTags = Lens.field @"tags"
{-# DEPRECATED crsgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateReplicationSubnetGroup where
  toJSON CreateReplicationSubnetGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ReplicationSubnetGroupIdentifier"
                  Core..= replicationSubnetGroupIdentifier
              ),
            Core.Just
              ( "ReplicationSubnetGroupDescription"
                  Core..= replicationSubnetGroupDescription
              ),
            Core.Just ("SubnetIds" Core..= subnetIds),
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateReplicationSubnetGroup where
  type
    Rs CreateReplicationSubnetGroup =
      CreateReplicationSubnetGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonDMSv20160101.CreateReplicationSubnetGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateReplicationSubnetGroupResponse'
            Core.<$> (x Core..:? "ReplicationSubnetGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkCreateReplicationSubnetGroupResponse' smart constructor.
data CreateReplicationSubnetGroupResponse = CreateReplicationSubnetGroupResponse'
  { -- | The replication subnet group that was created.
    replicationSubnetGroup :: Core.Maybe Types.ReplicationSubnetGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReplicationSubnetGroupResponse' value with any optional fields omitted.
mkCreateReplicationSubnetGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateReplicationSubnetGroupResponse
mkCreateReplicationSubnetGroupResponse responseStatus =
  CreateReplicationSubnetGroupResponse'
    { replicationSubnetGroup =
        Core.Nothing,
      responseStatus
    }

-- | The replication subnet group that was created.
--
-- /Note:/ Consider using 'replicationSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsgrrsReplicationSubnetGroup :: Lens.Lens' CreateReplicationSubnetGroupResponse (Core.Maybe Types.ReplicationSubnetGroup)
crsgrrsReplicationSubnetGroup = Lens.field @"replicationSubnetGroup"
{-# DEPRECATED crsgrrsReplicationSubnetGroup "Use generic-lens or generic-optics with 'replicationSubnetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsgrrsResponseStatus :: Lens.Lens' CreateReplicationSubnetGroupResponse Core.Int
crsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
