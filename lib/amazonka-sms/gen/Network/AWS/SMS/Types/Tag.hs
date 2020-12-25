{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.Tag
  ( Tag (..),

    -- * Smart constructor
    mkTag,

    -- * Lenses
    tKey,
    tValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.Key as Types
import qualified Network.AWS.SMS.Types.Value as Types

-- | Key/value pair that can be assigned to an application.
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { -- | The tag key.
    key :: Core.Maybe Types.Key,
    -- | The tag value.
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Tag' value with any optional fields omitted.
mkTag ::
  Tag
mkTag = Tag' {key = Core.Nothing, value = Core.Nothing}

-- | The tag key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag (Core.Maybe Types.Key)
tKey = Lens.field @"key"
{-# DEPRECATED tKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The tag value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag (Core.Maybe Types.Value)
tValue = Lens.field @"value"
{-# DEPRECATED tValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Tag where
  toJSON Tag {..} =
    Core.object
      ( Core.catMaybes
          [("key" Core..=) Core.<$> key, ("value" Core..=) Core.<$> value]
      )

instance Core.FromJSON Tag where
  parseJSON =
    Core.withObject "Tag" Core.$
      \x ->
        Tag' Core.<$> (x Core..:? "key") Core.<*> (x Core..:? "value")
