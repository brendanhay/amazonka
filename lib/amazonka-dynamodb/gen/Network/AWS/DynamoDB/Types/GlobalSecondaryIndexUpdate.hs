{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalSecondaryIndexUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalSecondaryIndexUpdate
  ( GlobalSecondaryIndexUpdate (..),

    -- * Smart constructor
    mkGlobalSecondaryIndexUpdate,

    -- * Lenses
    gsiuCreate,
    gsiuDelete,
    gsiuUpdate,
  )
where

import qualified Network.AWS.DynamoDB.Types.CreateGlobalSecondaryIndexAction as Types
import qualified Network.AWS.DynamoDB.Types.DeleteGlobalSecondaryIndexAction as Types
import qualified Network.AWS.DynamoDB.Types.UpdateGlobalSecondaryIndexAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents one of the following:
--
--
--     * A new global secondary index to be added to an existing table.
--
--
--     * New provisioned throughput parameters for an existing global secondary index.
--
--
--     * An existing global secondary index to be removed from an existing table.
--
--
--
-- /See:/ 'mkGlobalSecondaryIndexUpdate' smart constructor.
data GlobalSecondaryIndexUpdate = GlobalSecondaryIndexUpdate'
  { -- | The parameters required for creating a global secondary index on an existing table:
    --
    --
    --     * @IndexName @
    --
    --
    --     * @KeySchema @
    --
    --
    --     * @AttributeDefinitions @
    --
    --
    --     * @Projection @
    --
    --
    --     * @ProvisionedThroughput @
    create :: Core.Maybe Types.CreateGlobalSecondaryIndexAction,
    -- | The name of an existing global secondary index to be removed.
    delete :: Core.Maybe Types.DeleteGlobalSecondaryIndexAction,
    -- | The name of an existing global secondary index, along with new provisioned throughput settings to be applied to that index.
    update :: Core.Maybe Types.UpdateGlobalSecondaryIndexAction
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlobalSecondaryIndexUpdate' value with any optional fields omitted.
mkGlobalSecondaryIndexUpdate ::
  GlobalSecondaryIndexUpdate
mkGlobalSecondaryIndexUpdate =
  GlobalSecondaryIndexUpdate'
    { create = Core.Nothing,
      delete = Core.Nothing,
      update = Core.Nothing
    }

-- | The parameters required for creating a global secondary index on an existing table:
--
--
--     * @IndexName @
--
--
--     * @KeySchema @
--
--
--     * @AttributeDefinitions @
--
--
--     * @Projection @
--
--
--     * @ProvisionedThroughput @
--
--
--
-- /Note:/ Consider using 'create' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiuCreate :: Lens.Lens' GlobalSecondaryIndexUpdate (Core.Maybe Types.CreateGlobalSecondaryIndexAction)
gsiuCreate = Lens.field @"create"
{-# DEPRECATED gsiuCreate "Use generic-lens or generic-optics with 'create' instead." #-}

-- | The name of an existing global secondary index to be removed.
--
-- /Note:/ Consider using 'delete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiuDelete :: Lens.Lens' GlobalSecondaryIndexUpdate (Core.Maybe Types.DeleteGlobalSecondaryIndexAction)
gsiuDelete = Lens.field @"delete"
{-# DEPRECATED gsiuDelete "Use generic-lens or generic-optics with 'delete' instead." #-}

-- | The name of an existing global secondary index, along with new provisioned throughput settings to be applied to that index.
--
-- /Note:/ Consider using 'update' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiuUpdate :: Lens.Lens' GlobalSecondaryIndexUpdate (Core.Maybe Types.UpdateGlobalSecondaryIndexAction)
gsiuUpdate = Lens.field @"update"
{-# DEPRECATED gsiuUpdate "Use generic-lens or generic-optics with 'update' instead." #-}

instance Core.FromJSON GlobalSecondaryIndexUpdate where
  toJSON GlobalSecondaryIndexUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ ("Create" Core..=) Core.<$> create,
            ("Delete" Core..=) Core.<$> delete,
            ("Update" Core..=) Core.<$> update
          ]
      )
