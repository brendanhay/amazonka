{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.TagKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.TagKeys
  ( TagKeys (..),

    -- * Smart constructor
    mkTagKeys,

    -- * Lenses
    tkItems,
  )
where

import qualified Network.AWS.CloudFront.Types.TagKey as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that contains zero or more @Tag@ elements.
--
-- /See:/ 'mkTagKeys' smart constructor.
newtype TagKeys = TagKeys'
  { -- | A complex type that contains @Tag@ key elements.
    items :: Core.Maybe [Types.TagKey]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TagKeys' value with any optional fields omitted.
mkTagKeys ::
  TagKeys
mkTagKeys = TagKeys' {items = Core.Nothing}

-- | A complex type that contains @Tag@ key elements.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tkItems :: Lens.Lens' TagKeys (Core.Maybe [Types.TagKey])
tkItems = Lens.field @"items"
{-# DEPRECATED tkItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.ToXML TagKeys where
  toXML TagKeys {..} =
    Core.toXMLNode "Items" (Core.toXMLList "Key" Core.<$> items)
