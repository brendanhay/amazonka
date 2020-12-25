{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaChangePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaChangePolicy
  ( SchemaChangePolicy (..),

    -- * Smart constructor
    mkSchemaChangePolicy,

    -- * Lenses
    scpDeleteBehavior,
    scpUpdateBehavior,
  )
where

import qualified Network.AWS.Glue.Types.DeleteBehavior as Types
import qualified Network.AWS.Glue.Types.UpdateBehavior as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A policy that specifies update and deletion behaviors for the crawler.
--
-- /See:/ 'mkSchemaChangePolicy' smart constructor.
data SchemaChangePolicy = SchemaChangePolicy'
  { -- | The deletion behavior when the crawler finds a deleted object.
    deleteBehavior :: Core.Maybe Types.DeleteBehavior,
    -- | The update behavior when the crawler finds a changed schema.
    updateBehavior :: Core.Maybe Types.UpdateBehavior
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SchemaChangePolicy' value with any optional fields omitted.
mkSchemaChangePolicy ::
  SchemaChangePolicy
mkSchemaChangePolicy =
  SchemaChangePolicy'
    { deleteBehavior = Core.Nothing,
      updateBehavior = Core.Nothing
    }

-- | The deletion behavior when the crawler finds a deleted object.
--
-- /Note:/ Consider using 'deleteBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpDeleteBehavior :: Lens.Lens' SchemaChangePolicy (Core.Maybe Types.DeleteBehavior)
scpDeleteBehavior = Lens.field @"deleteBehavior"
{-# DEPRECATED scpDeleteBehavior "Use generic-lens or generic-optics with 'deleteBehavior' instead." #-}

-- | The update behavior when the crawler finds a changed schema.
--
-- /Note:/ Consider using 'updateBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpUpdateBehavior :: Lens.Lens' SchemaChangePolicy (Core.Maybe Types.UpdateBehavior)
scpUpdateBehavior = Lens.field @"updateBehavior"
{-# DEPRECATED scpUpdateBehavior "Use generic-lens or generic-optics with 'updateBehavior' instead." #-}

instance Core.FromJSON SchemaChangePolicy where
  toJSON SchemaChangePolicy {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeleteBehavior" Core..=) Core.<$> deleteBehavior,
            ("UpdateBehavior" Core..=) Core.<$> updateBehavior
          ]
      )

instance Core.FromJSON SchemaChangePolicy where
  parseJSON =
    Core.withObject "SchemaChangePolicy" Core.$
      \x ->
        SchemaChangePolicy'
          Core.<$> (x Core..:? "DeleteBehavior")
          Core.<*> (x Core..:? "UpdateBehavior")
