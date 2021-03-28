{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.Tag
  ( Tag (..)
  -- * Smart constructor
  , mkTag
  -- * Lenses
  , tKey
  , tValue
  ) where

import qualified Network.AWS.DirectConnect.Types.Key as Types
import qualified Network.AWS.DirectConnect.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a tag.
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { key :: Types.Key
    -- ^ The key.
  , value :: Core.Maybe Types.Value
    -- ^ The value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Tag' value with any optional fields omitted.
mkTag
    :: Types.Key -- ^ 'key'
    -> Tag
mkTag key = Tag'{key, value = Core.Nothing}

-- | The key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag Types.Key
tKey = Lens.field @"key"
{-# INLINEABLE tKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag (Core.Maybe Types.Value)
tValue = Lens.field @"value"
{-# INLINEABLE tValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON Tag where
        toJSON Tag{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("key" Core..= key), ("value" Core..=) Core.<$> value])

instance Core.FromJSON Tag where
        parseJSON
          = Core.withObject "Tag" Core.$
              \ x -> Tag' Core.<$> (x Core..: "key") Core.<*> x Core..:? "value"
