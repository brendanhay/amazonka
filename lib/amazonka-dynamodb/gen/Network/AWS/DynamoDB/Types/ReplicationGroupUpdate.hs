{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicationGroupUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicationGroupUpdate
  ( ReplicationGroupUpdate (..),

    -- * Smart constructor
    mkReplicationGroupUpdate,

    -- * Lenses
    rguCreate,
    rguDelete,
    rguUpdate,
  )
where

import qualified Network.AWS.DynamoDB.Types.CreateReplicationGroupMemberAction as Types
import qualified Network.AWS.DynamoDB.Types.DeleteReplicationGroupMemberAction as Types
import qualified Network.AWS.DynamoDB.Types.UpdateReplicationGroupMemberAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents one of the following:
--
--
--     * A new replica to be added to an existing regional table or global table. This request invokes the @CreateTableReplica@ action in the destination Region.
--
--
--     * New parameters for an existing replica. This request invokes the @UpdateTable@ action in the destination Region.
--
--
--     * An existing replica to be deleted. The request invokes the @DeleteTableReplica@ action in the destination Region, deleting the replica and all if its items in the destination Region.
--
--
--
-- /See:/ 'mkReplicationGroupUpdate' smart constructor.
data ReplicationGroupUpdate = ReplicationGroupUpdate'
  { -- | The parameters required for creating a replica for the table.
    create :: Core.Maybe Types.CreateReplicationGroupMemberAction,
    -- | The parameters required for deleting a replica for the table.
    delete :: Core.Maybe Types.DeleteReplicationGroupMemberAction,
    -- | The parameters required for updating a replica for the table.
    update :: Core.Maybe Types.UpdateReplicationGroupMemberAction
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicationGroupUpdate' value with any optional fields omitted.
mkReplicationGroupUpdate ::
  ReplicationGroupUpdate
mkReplicationGroupUpdate =
  ReplicationGroupUpdate'
    { create = Core.Nothing,
      delete = Core.Nothing,
      update = Core.Nothing
    }

-- | The parameters required for creating a replica for the table.
--
-- /Note:/ Consider using 'create' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rguCreate :: Lens.Lens' ReplicationGroupUpdate (Core.Maybe Types.CreateReplicationGroupMemberAction)
rguCreate = Lens.field @"create"
{-# DEPRECATED rguCreate "Use generic-lens or generic-optics with 'create' instead." #-}

-- | The parameters required for deleting a replica for the table.
--
-- /Note:/ Consider using 'delete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rguDelete :: Lens.Lens' ReplicationGroupUpdate (Core.Maybe Types.DeleteReplicationGroupMemberAction)
rguDelete = Lens.field @"delete"
{-# DEPRECATED rguDelete "Use generic-lens or generic-optics with 'delete' instead." #-}

-- | The parameters required for updating a replica for the table.
--
-- /Note:/ Consider using 'update' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rguUpdate :: Lens.Lens' ReplicationGroupUpdate (Core.Maybe Types.UpdateReplicationGroupMemberAction)
rguUpdate = Lens.field @"update"
{-# DEPRECATED rguUpdate "Use generic-lens or generic-optics with 'update' instead." #-}

instance Core.FromJSON ReplicationGroupUpdate where
  toJSON ReplicationGroupUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ ("Create" Core..=) Core.<$> create,
            ("Delete" Core..=) Core.<$> delete,
            ("Update" Core..=) Core.<$> update
          ]
      )
