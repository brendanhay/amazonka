{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.TagFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScalingPlans.Types.TagFilter
  ( TagFilter (..)
  -- * Smart constructor
  , mkTagFilter
  -- * Lenses
  , tfKey
  , tfValues
  ) where

import qualified Network.AWS.AutoScalingPlans.Types.XmlStringMaxLen128 as Types
import qualified Network.AWS.AutoScalingPlans.Types.XmlStringMaxLen256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a tag.
--
-- /See:/ 'mkTagFilter' smart constructor.
data TagFilter = TagFilter'
  { key :: Core.Maybe Types.XmlStringMaxLen128
    -- ^ The tag key.
  , values :: Core.Maybe [Types.XmlStringMaxLen256]
    -- ^ The tag values (0 to 20).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagFilter' value with any optional fields omitted.
mkTagFilter
    :: TagFilter
mkTagFilter = TagFilter'{key = Core.Nothing, values = Core.Nothing}

-- | The tag key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfKey :: Lens.Lens' TagFilter (Core.Maybe Types.XmlStringMaxLen128)
tfKey = Lens.field @"key"
{-# INLINEABLE tfKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The tag values (0 to 20).
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfValues :: Lens.Lens' TagFilter (Core.Maybe [Types.XmlStringMaxLen256])
tfValues = Lens.field @"values"
{-# INLINEABLE tfValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.FromJSON TagFilter where
        toJSON TagFilter{..}
          = Core.object
              (Core.catMaybes
                 [("Key" Core..=) Core.<$> key, ("Values" Core..=) Core.<$> values])

instance Core.FromJSON TagFilter where
        parseJSON
          = Core.withObject "TagFilter" Core.$
              \ x ->
                TagFilter' Core.<$> (x Core..:? "Key") Core.<*> x Core..:? "Values"
