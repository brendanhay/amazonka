{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TagFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.TagFilter
  ( TagFilter (..)
  -- * Smart constructor
  , mkTagFilter
  -- * Lenses
  , tfKey
  , tfType
  , tfValue
  ) where

import qualified Network.AWS.CodeDeploy.Types.Key as Types
import qualified Network.AWS.CodeDeploy.Types.TagFilterType as Types
import qualified Network.AWS.CodeDeploy.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an on-premises instance tag filter.
--
-- /See:/ 'mkTagFilter' smart constructor.
data TagFilter = TagFilter'
  { key :: Core.Maybe Types.Key
    -- ^ The on-premises instance tag filter key.
  , type' :: Core.Maybe Types.TagFilterType
    -- ^ The on-premises instance tag filter type:
--
--
--     * KEY_ONLY: Key only.
--
--
--     * VALUE_ONLY: Value only.
--
--
--     * KEY_AND_VALUE: Key and value.
--
--
  , value :: Core.Maybe Types.Value
    -- ^ The on-premises instance tag filter value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagFilter' value with any optional fields omitted.
mkTagFilter
    :: TagFilter
mkTagFilter
  = TagFilter'{key = Core.Nothing, type' = Core.Nothing,
               value = Core.Nothing}

-- | The on-premises instance tag filter key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfKey :: Lens.Lens' TagFilter (Core.Maybe Types.Key)
tfKey = Lens.field @"key"
{-# INLINEABLE tfKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The on-premises instance tag filter type:
--
--
--     * KEY_ONLY: Key only.
--
--
--     * VALUE_ONLY: Value only.
--
--
--     * KEY_AND_VALUE: Key and value.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfType :: Lens.Lens' TagFilter (Core.Maybe Types.TagFilterType)
tfType = Lens.field @"type'"
{-# INLINEABLE tfType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The on-premises instance tag filter value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfValue :: Lens.Lens' TagFilter (Core.Maybe Types.Value)
tfValue = Lens.field @"value"
{-# INLINEABLE tfValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON TagFilter where
        toJSON TagFilter{..}
          = Core.object
              (Core.catMaybes
                 [("Key" Core..=) Core.<$> key, ("Type" Core..=) Core.<$> type',
                  ("Value" Core..=) Core.<$> value])

instance Core.FromJSON TagFilter where
        parseJSON
          = Core.withObject "TagFilter" Core.$
              \ x ->
                TagFilter' Core.<$>
                  (x Core..:? "Key") Core.<*> x Core..:? "Type" Core.<*>
                    x Core..:? "Value"
