{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Tag
  ( Tag (..)
  -- * Smart constructor
  , mkTag
  -- * Lenses
  , tKey
  , tValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a tag.
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { key :: Core.Text
    -- ^ The key of the tag.
--
-- Constraints: Tag keys are case-sensitive and accept a maximum of 127 Unicode characters. May not begin with @aws:@ .
  , value :: Core.Text
    -- ^ The value of the tag.
--
-- Constraints: Tag values are case-sensitive and accept a maximum of 255 Unicode characters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Tag' value with any optional fields omitted.
mkTag
    :: Core.Text -- ^ 'key'
    -> Core.Text -- ^ 'value'
    -> Tag
mkTag key value = Tag'{key, value}

-- | The key of the tag.
--
-- Constraints: Tag keys are case-sensitive and accept a maximum of 127 Unicode characters. May not begin with @aws:@ .
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag Core.Text
tKey = Lens.field @"key"
{-# INLINEABLE tKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The value of the tag.
--
-- Constraints: Tag values are case-sensitive and accept a maximum of 255 Unicode characters.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag Core.Text
tValue = Lens.field @"value"
{-# INLINEABLE tValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.ToQuery Tag where
        toQuery Tag{..}
          = Core.toQueryPair "Key" key Core.<> Core.toQueryPair "Value" value

instance Core.FromXML Tag where
        parseXML x
          = Tag' Core.<$> (x Core..@ "key") Core.<*> x Core..@ "value"
