{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Tags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Tags
  ( Tags (..),

    -- * Smart constructor
    mkTags,

    -- * Lenses
    tItems,
  )
where

import qualified Network.AWS.CloudFront.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that contains zero or more @Tag@ elements.
--
-- /See:/ 'mkTags' smart constructor.
newtype Tags = Tags'
  { -- | A complex type that contains @Tag@ elements.
    items :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Tags' value with any optional fields omitted.
mkTags ::
  Tags
mkTags = Tags' {items = Core.Nothing}

-- | A complex type that contains @Tag@ elements.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tItems :: Lens.Lens' Tags (Core.Maybe [Types.Tag])
tItems = Lens.field @"items"
{-# DEPRECATED tItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.ToXML Tags where
  toXML Tags {..} =
    Core.toXMLNode "Items" (Core.toXMLList "Tag" Core.<$> items)

instance Core.FromXML Tags where
  parseXML x =
    Tags'
      Core.<$> (x Core..@? "Items" Core..<@> Core.parseXMLList "Tag")
