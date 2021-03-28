{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ReplicaUpdate
  ( ReplicaUpdate (..)
  -- * Smart constructor
  , mkReplicaUpdate
  -- * Lenses
  , ruCreate
  , ruDelete
  ) where

import qualified Network.AWS.DynamoDB.Types.CreateReplicaAction as Types
import qualified Network.AWS.DynamoDB.Types.DeleteReplicaAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { create :: Core.Maybe Types.CreateReplicaAction
    -- ^ The parameters required for creating a replica on an existing global table.
  , delete :: Core.Maybe Types.DeleteReplicaAction
    -- ^ The name of the existing replica to be removed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicaUpdate' value with any optional fields omitted.
mkReplicaUpdate
    :: ReplicaUpdate
mkReplicaUpdate
  = ReplicaUpdate'{create = Core.Nothing, delete = Core.Nothing}

-- | The parameters required for creating a replica on an existing global table.
--
-- /Note:/ Consider using 'create' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruCreate :: Lens.Lens' ReplicaUpdate (Core.Maybe Types.CreateReplicaAction)
ruCreate = Lens.field @"create"
{-# INLINEABLE ruCreate #-}
{-# DEPRECATED create "Use generic-lens or generic-optics with 'create' instead"  #-}

-- | The name of the existing replica to be removed.
--
-- /Note:/ Consider using 'delete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruDelete :: Lens.Lens' ReplicaUpdate (Core.Maybe Types.DeleteReplicaAction)
ruDelete = Lens.field @"delete"
{-# INLINEABLE ruDelete #-}
{-# DEPRECATED delete "Use generic-lens or generic-optics with 'delete' instead"  #-}

instance Core.FromJSON ReplicaUpdate where
        toJSON ReplicaUpdate{..}
          = Core.object
              (Core.catMaybes
                 [("Create" Core..=) Core.<$> create,
                  ("Delete" Core..=) Core.<$> delete])
