{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ResourceTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ResourceTag
  ( ResourceTag (..),

    -- * Smart constructor
    mkResourceTag,

    -- * Lenses
    rtKey,
    rtValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.ResourceTagKey as Types
import qualified Network.AWS.SWF.Types.Value as Types

-- | Tags are key-value pairs that can be associated with Amazon SWF state machines and activities.
--
-- Tags may only contain unicode letters, digits, whitespace, or these symbols: @_ . : / = + - @@ .
--
-- /See:/ 'mkResourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { -- | The key of a tag.
    key :: Types.ResourceTagKey,
    -- | The value of a tag.
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceTag' value with any optional fields omitted.
mkResourceTag ::
  -- | 'key'
  Types.ResourceTagKey ->
  ResourceTag
mkResourceTag key = ResourceTag' {key, value = Core.Nothing}

-- | The key of a tag.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtKey :: Lens.Lens' ResourceTag Types.ResourceTagKey
rtKey = Lens.field @"key"
{-# DEPRECATED rtKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The value of a tag.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtValue :: Lens.Lens' ResourceTag (Core.Maybe Types.Value)
rtValue = Lens.field @"value"
{-# DEPRECATED rtValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON ResourceTag where
  toJSON ResourceTag {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("key" Core..= key), ("value" Core..=) Core.<$> value]
      )

instance Core.FromJSON ResourceTag where
  parseJSON =
    Core.withObject "ResourceTag" Core.$
      \x ->
        ResourceTag'
          Core.<$> (x Core..: "key") Core.<*> (x Core..:? "value")
