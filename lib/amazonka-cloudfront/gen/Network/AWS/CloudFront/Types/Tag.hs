{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Tag
  ( Tag (..),

    -- * Smart constructor
    mkTag,

    -- * Lenses
    tKey,
    tValue,
  )
where

import qualified Network.AWS.CloudFront.Types.Key as Types
import qualified Network.AWS.CloudFront.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that contains @Tag@ key and @Tag@ value.
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { -- | A string that contains @Tag@ key.
    --
    -- The string length should be between 1 and 128 characters. Valid characters include @a-z@ , @A-Z@ , @0-9@ , space, and the special characters @_ - . : / = + @@ .
    key :: Types.Key,
    -- | A string that contains an optional @Tag@ value.
    --
    -- The string length should be between 0 and 256 characters. Valid characters include @a-z@ , @A-Z@ , @0-9@ , space, and the special characters @_ - . : / = + @@ .
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Tag' value with any optional fields omitted.
mkTag ::
  -- | 'key'
  Types.Key ->
  Tag
mkTag key = Tag' {key, value = Core.Nothing}

-- | A string that contains @Tag@ key.
--
-- The string length should be between 1 and 128 characters. Valid characters include @a-z@ , @A-Z@ , @0-9@ , space, and the special characters @_ - . : / = + @@ .
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag Types.Key
tKey = Lens.field @"key"
{-# DEPRECATED tKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | A string that contains an optional @Tag@ value.
--
-- The string length should be between 0 and 256 characters. Valid characters include @a-z@ , @A-Z@ , @0-9@ , space, and the special characters @_ - . : / = + @@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag (Core.Maybe Types.Value)
tValue = Lens.field @"value"
{-# DEPRECATED tValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.ToXML Tag where
  toXML Tag {..} =
    Core.toXMLNode "Key" key
      Core.<> Core.toXMLNode "Value" Core.<$> value

instance Core.FromXML Tag where
  parseXML x =
    Tag' Core.<$> (x Core..@ "Key") Core.<*> (x Core..@? "Value")
