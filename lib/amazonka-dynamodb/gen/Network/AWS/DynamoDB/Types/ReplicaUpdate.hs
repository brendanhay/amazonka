-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaUpdate
  ( ReplicaUpdate (..),

    -- * Smart constructor
    mkReplicaUpdate,

    -- * Lenses
    ruCreate,
    ruDelete,
  )
where

import Network.AWS.DynamoDB.Types.CreateReplicaAction
import Network.AWS.DynamoDB.Types.DeleteReplicaAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents one of the following:
--
--
--     * A new replica to be added to an existing global table.
--
--
--     * New parameters for an existing replica.
--
--
--     * An existing replica to be removed from an existing global table.
--
--
--
-- /See:/ 'mkReplicaUpdate' smart constructor.
data ReplicaUpdate = ReplicaUpdate'
  { create ::
      Lude.Maybe CreateReplicaAction,
    delete :: Lude.Maybe DeleteReplicaAction
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicaUpdate' with the minimum fields required to make a request.
--
-- * 'create' - The parameters required for creating a replica on an existing global table.
-- * 'delete' - The name of the existing replica to be removed.
mkReplicaUpdate ::
  ReplicaUpdate
mkReplicaUpdate =
  ReplicaUpdate' {create = Lude.Nothing, delete = Lude.Nothing}

-- | The parameters required for creating a replica on an existing global table.
--
-- /Note:/ Consider using 'create' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruCreate :: Lens.Lens' ReplicaUpdate (Lude.Maybe CreateReplicaAction)
ruCreate = Lens.lens (create :: ReplicaUpdate -> Lude.Maybe CreateReplicaAction) (\s a -> s {create = a} :: ReplicaUpdate)
{-# DEPRECATED ruCreate "Use generic-lens or generic-optics with 'create' instead." #-}

-- | The name of the existing replica to be removed.
--
-- /Note:/ Consider using 'delete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruDelete :: Lens.Lens' ReplicaUpdate (Lude.Maybe DeleteReplicaAction)
ruDelete = Lens.lens (delete :: ReplicaUpdate -> Lude.Maybe DeleteReplicaAction) (\s a -> s {delete = a} :: ReplicaUpdate)
{-# DEPRECATED ruDelete "Use generic-lens or generic-optics with 'delete' instead." #-}

instance Lude.ToJSON ReplicaUpdate where
  toJSON ReplicaUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Create" Lude..=) Lude.<$> create,
            ("Delete" Lude..=) Lude.<$> delete
          ]
      )
