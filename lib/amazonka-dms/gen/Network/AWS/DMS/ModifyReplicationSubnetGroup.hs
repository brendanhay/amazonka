{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ModifyReplicationSubnetGroup (..),
    mkModifyReplicationSubnetGroup,

    -- ** Request lenses
    mrsgReplicationSubnetGroupIdentifier,
    mrsgSubnetIds,
    mrsgReplicationSubnetGroupDescription,

    -- * Destructuring the response
    ModifyReplicationSubnetGroupResponse (..),
    mkModifyReplicationSubnetGroupResponse,

    -- ** Response lenses
    mrsgrrsReplicationSubnetGroup,
    mrsgrrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkModifyReplicationSubnetGroup' smart constructor.
data ModifyReplicationSubnetGroup = ModifyReplicationSubnetGroup'
  { -- | The name of the replication instance subnet group.
    replicationSubnetGroupIdentifier :: Types.String,
    -- | A list of subnet IDs.
    subnetIds :: [Types.String],
    -- | A description for the replication instance subnet group.
    replicationSubnetGroupDescription :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyReplicationSubnetGroup' value with any optional fields omitted.
mkModifyReplicationSubnetGroup ::
  -- | 'replicationSubnetGroupIdentifier'
  Types.String ->
  ModifyReplicationSubnetGroup
mkModifyReplicationSubnetGroup replicationSubnetGroupIdentifier =
  ModifyReplicationSubnetGroup'
    { replicationSubnetGroupIdentifier,
      subnetIds = Core.mempty,
      replicationSubnetGroupDescription = Core.Nothing
    }

-- | The name of the replication instance subnet group.
--
-- /Note:/ Consider using 'replicationSubnetGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsgReplicationSubnetGroupIdentifier :: Lens.Lens' ModifyReplicationSubnetGroup Types.String
mrsgReplicationSubnetGroupIdentifier = Lens.field @"replicationSubnetGroupIdentifier"
{-# DEPRECATED mrsgReplicationSubnetGroupIdentifier "Use generic-lens or generic-optics with 'replicationSubnetGroupIdentifier' instead." #-}

-- | A list of subnet IDs.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsgSubnetIds :: Lens.Lens' ModifyReplicationSubnetGroup [Types.String]
mrsgSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED mrsgSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | A description for the replication instance subnet group.
--
-- /Note:/ Consider using 'replicationSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsgReplicationSubnetGroupDescription :: Lens.Lens' ModifyReplicationSubnetGroup (Core.Maybe Types.String)
mrsgReplicationSubnetGroupDescription = Lens.field @"replicationSubnetGroupDescription"
{-# DEPRECATED mrsgReplicationSubnetGroupDescription "Use generic-lens or generic-optics with 'replicationSubnetGroupDescription' instead." #-}

instance Core.FromJSON ModifyReplicationSubnetGroup where
  toJSON ModifyReplicationSubnetGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ReplicationSubnetGroupIdentifier"
                  Core..= replicationSubnetGroupIdentifier
              ),
            Core.Just ("SubnetIds" Core..= subnetIds),
            ("ReplicationSubnetGroupDescription" Core..=)
              Core.<$> replicationSubnetGroupDescription
          ]
      )

instance Core.AWSRequest ModifyReplicationSubnetGroup where
  type
    Rs ModifyReplicationSubnetGroup =
      ModifyReplicationSubnetGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonDMSv20160101.ModifyReplicationSubnetGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyReplicationSubnetGroupResponse'
            Core.<$> (x Core..:? "ReplicationSubnetGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkModifyReplicationSubnetGroupResponse' smart constructor.
data ModifyReplicationSubnetGroupResponse = ModifyReplicationSubnetGroupResponse'
  { -- | The modified replication subnet group.
    replicationSubnetGroup :: Core.Maybe Types.ReplicationSubnetGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyReplicationSubnetGroupResponse' value with any optional fields omitted.
mkModifyReplicationSubnetGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyReplicationSubnetGroupResponse
mkModifyReplicationSubnetGroupResponse responseStatus =
  ModifyReplicationSubnetGroupResponse'
    { replicationSubnetGroup =
        Core.Nothing,
      responseStatus
    }

-- | The modified replication subnet group.
--
-- /Note:/ Consider using 'replicationSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsgrrsReplicationSubnetGroup :: Lens.Lens' ModifyReplicationSubnetGroupResponse (Core.Maybe Types.ReplicationSubnetGroup)
mrsgrrsReplicationSubnetGroup = Lens.field @"replicationSubnetGroup"
{-# DEPRECATED mrsgrrsReplicationSubnetGroup "Use generic-lens or generic-optics with 'replicationSubnetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsgrrsResponseStatus :: Lens.Lens' ModifyReplicationSubnetGroupResponse Core.Int
mrsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mrsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
