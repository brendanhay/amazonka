{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MarketplaceMetering.Types.Tag
  ( Tag (..)
  -- * Smart constructor
  , mkTag
  -- * Lenses
  , tKey
  , tValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MarketplaceMetering.Types.Key as Types
import qualified Network.AWS.MarketplaceMetering.Types.Value as Types
import qualified Network.AWS.Prelude as Core

-- | Metadata assigned to an allocation. Each tag is made up of a key and a value.
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { key :: Types.Key
    -- ^ One part of a key-value pair that makes up a tag. A key is a label that acts like a category for the specific tag values.
  , value :: Types.Value
    -- ^ One part of a key-value pair that makes up a tag. A value acts as a descriptor within a tag category (key). The value can be empty or null.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Tag' value with any optional fields omitted.
mkTag
    :: Types.Key -- ^ 'key'
    -> Types.Value -- ^ 'value'
    -> Tag
mkTag key value = Tag'{key, value}

-- | One part of a key-value pair that makes up a tag. A key is a label that acts like a category for the specific tag values.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag Types.Key
tKey = Lens.field @"key"
{-# INLINEABLE tKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | One part of a key-value pair that makes up a tag. A value acts as a descriptor within a tag category (key). The value can be empty or null.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag Types.Value
tValue = Lens.field @"value"
{-# INLINEABLE tValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON Tag where
        toJSON Tag{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Key" Core..= key), Core.Just ("Value" Core..= value)])

instance Core.FromJSON Tag where
        parseJSON
          = Core.withObject "Tag" Core.$
              \ x -> Tag' Core.<$> (x Core..: "Key") Core.<*> x Core..: "Value"
