{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.ResourceGroupTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.ResourceGroupTag
  ( ResourceGroupTag (..),

    -- * Smart constructor
    mkResourceGroupTag,

    -- * Lenses
    rgtKey,
    rgtValue,
  )
where

import qualified Network.AWS.Inspector.Types.Key as Types
import qualified Network.AWS.Inspector.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This data type is used as one of the elements of the 'ResourceGroup' data type.
--
-- /See:/ 'mkResourceGroupTag' smart constructor.
data ResourceGroupTag = ResourceGroupTag'
  { -- | A tag key.
    key :: Types.Key,
    -- | The value assigned to a tag key.
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceGroupTag' value with any optional fields omitted.
mkResourceGroupTag ::
  -- | 'key'
  Types.Key ->
  ResourceGroupTag
mkResourceGroupTag key =
  ResourceGroupTag' {key, value = Core.Nothing}

-- | A tag key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgtKey :: Lens.Lens' ResourceGroupTag Types.Key
rgtKey = Lens.field @"key"
{-# DEPRECATED rgtKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The value assigned to a tag key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgtValue :: Lens.Lens' ResourceGroupTag (Core.Maybe Types.Value)
rgtValue = Lens.field @"value"
{-# DEPRECATED rgtValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON ResourceGroupTag where
  toJSON ResourceGroupTag {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("key" Core..= key), ("value" Core..=) Core.<$> value]
      )

instance Core.FromJSON ResourceGroupTag where
  parseJSON =
    Core.withObject "ResourceGroupTag" Core.$
      \x ->
        ResourceGroupTag'
          Core.<$> (x Core..: "key") Core.<*> (x Core..:? "value")
