{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.TagFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroupsTagging.Types.TagFilter
  ( TagFilter (..)
  -- * Smart constructor
  , mkTagFilter
  -- * Lenses
  , tfKey
  , tfValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ResourceGroupsTagging.Types.Key as Types
import qualified Network.AWS.ResourceGroupsTagging.Types.TagValue as Types

-- | A list of tags (keys and values) that are used to specify the associated resources.
--
-- /See:/ 'mkTagFilter' smart constructor.
data TagFilter = TagFilter'
  { key :: Core.Maybe Types.Key
    -- ^ One part of a key-value pair that makes up a tag. A key is a general label that acts like a category for more specific tag values.
  , values :: Core.Maybe [Types.TagValue]
    -- ^ One part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key). The value can be empty or null.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagFilter' value with any optional fields omitted.
mkTagFilter
    :: TagFilter
mkTagFilter = TagFilter'{key = Core.Nothing, values = Core.Nothing}

-- | One part of a key-value pair that makes up a tag. A key is a general label that acts like a category for more specific tag values.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfKey :: Lens.Lens' TagFilter (Core.Maybe Types.Key)
tfKey = Lens.field @"key"
{-# INLINEABLE tfKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | One part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key). The value can be empty or null.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfValues :: Lens.Lens' TagFilter (Core.Maybe [Types.TagValue])
tfValues = Lens.field @"values"
{-# INLINEABLE tfValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.FromJSON TagFilter where
        toJSON TagFilter{..}
          = Core.object
              (Core.catMaybes
                 [("Key" Core..=) Core.<$> key, ("Values" Core..=) Core.<$> values])
