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

import Network.AWS.DynamoDB.Types.CreateReplicationGroupMemberAction
import Network.AWS.DynamoDB.Types.DeleteReplicationGroupMemberAction
import Network.AWS.DynamoDB.Types.UpdateReplicationGroupMemberAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { create ::
      Lude.Maybe CreateReplicationGroupMemberAction,
    delete ::
      Lude.Maybe DeleteReplicationGroupMemberAction,
    update ::
      Lude.Maybe UpdateReplicationGroupMemberAction
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationGroupUpdate' with the minimum fields required to make a request.
--
-- * 'create' - The parameters required for creating a replica for the table.
-- * 'delete' - The parameters required for deleting a replica for the table.
-- * 'update' - The parameters required for updating a replica for the table.
mkReplicationGroupUpdate ::
  ReplicationGroupUpdate
mkReplicationGroupUpdate =
  ReplicationGroupUpdate'
    { create = Lude.Nothing,
      delete = Lude.Nothing,
      update = Lude.Nothing
    }

-- | The parameters required for creating a replica for the table.
--
-- /Note:/ Consider using 'create' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rguCreate :: Lens.Lens' ReplicationGroupUpdate (Lude.Maybe CreateReplicationGroupMemberAction)
rguCreate = Lens.lens (create :: ReplicationGroupUpdate -> Lude.Maybe CreateReplicationGroupMemberAction) (\s a -> s {create = a} :: ReplicationGroupUpdate)
{-# DEPRECATED rguCreate "Use generic-lens or generic-optics with 'create' instead." #-}

-- | The parameters required for deleting a replica for the table.
--
-- /Note:/ Consider using 'delete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rguDelete :: Lens.Lens' ReplicationGroupUpdate (Lude.Maybe DeleteReplicationGroupMemberAction)
rguDelete = Lens.lens (delete :: ReplicationGroupUpdate -> Lude.Maybe DeleteReplicationGroupMemberAction) (\s a -> s {delete = a} :: ReplicationGroupUpdate)
{-# DEPRECATED rguDelete "Use generic-lens or generic-optics with 'delete' instead." #-}

-- | The parameters required for updating a replica for the table.
--
-- /Note:/ Consider using 'update' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rguUpdate :: Lens.Lens' ReplicationGroupUpdate (Lude.Maybe UpdateReplicationGroupMemberAction)
rguUpdate = Lens.lens (update :: ReplicationGroupUpdate -> Lude.Maybe UpdateReplicationGroupMemberAction) (\s a -> s {update = a} :: ReplicationGroupUpdate)
{-# DEPRECATED rguUpdate "Use generic-lens or generic-optics with 'update' instead." #-}

instance Lude.ToJSON ReplicationGroupUpdate where
  toJSON ReplicationGroupUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Create" Lude..=) Lude.<$> create,
            ("Delete" Lude..=) Lude.<$> delete,
            ("Update" Lude..=) Lude.<$> update
          ]
      )
