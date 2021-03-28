{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.Tag
  ( Tag (..)
  -- * Smart constructor
  , mkTag
  -- * Lenses
  , tKey
  , tValue
  ) where

import qualified Network.AWS.Firehose.Types.Key as Types
import qualified Network.AWS.Firehose.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Metadata that you can assign to a delivery stream, consisting of a key-value pair.
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { key :: Types.Key
    -- ^ A unique identifier for the tag. Maximum length: 128 characters. Valid characters: Unicode letters, digits, white space, _ . / = + - % @
  , value :: Core.Maybe Types.Value
    -- ^ An optional string, which you can use to describe or define the tag. Maximum length: 256 characters. Valid characters: Unicode letters, digits, white space, _ . / = + - % @
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Tag' value with any optional fields omitted.
mkTag
    :: Types.Key -- ^ 'key'
    -> Tag
mkTag key = Tag'{key, value = Core.Nothing}

-- | A unique identifier for the tag. Maximum length: 128 characters. Valid characters: Unicode letters, digits, white space, _ . / = + - % @
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag Types.Key
tKey = Lens.field @"key"
{-# INLINEABLE tKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | An optional string, which you can use to describe or define the tag. Maximum length: 256 characters. Valid characters: Unicode letters, digits, white space, _ . / = + - % @
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
                 [Core.Just ("Key" Core..= key), ("Value" Core..=) Core.<$> value])

instance Core.FromJSON Tag where
        parseJSON
          = Core.withObject "Tag" Core.$
              \ x -> Tag' Core.<$> (x Core..: "Key") Core.<*> x Core..:? "Value"
