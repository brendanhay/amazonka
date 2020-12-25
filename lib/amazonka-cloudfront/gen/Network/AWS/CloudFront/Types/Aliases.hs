{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Aliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Aliases
  ( Aliases (..),

    -- * Smart constructor
    mkAliases,

    -- * Lenses
    aQuantity,
    aItems,
  )
where

import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
--
-- /See:/ 'mkAliases' smart constructor.
data Aliases = Aliases'
  { -- | The number of CNAME aliases, if any, that you want to associate with this distribution.
    quantity :: Core.Int,
    -- | A complex type that contains the CNAME aliases, if any, that you want to associate with this distribution.
    items :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Aliases' value with any optional fields omitted.
mkAliases ::
  -- | 'quantity'
  Core.Int ->
  Aliases
mkAliases quantity = Aliases' {quantity, items = Core.Nothing}

-- | The number of CNAME aliases, if any, that you want to associate with this distribution.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aQuantity :: Lens.Lens' Aliases Core.Int
aQuantity = Lens.field @"quantity"
{-# DEPRECATED aQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A complex type that contains the CNAME aliases, if any, that you want to associate with this distribution.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aItems :: Lens.Lens' Aliases (Core.Maybe [Types.String])
aItems = Lens.field @"items"
{-# DEPRECATED aItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.ToXML Aliases where
  toXML Aliases {..} =
    Core.toXMLNode "Quantity" quantity
      Core.<> Core.toXMLNode "Items" (Core.toXMLList "CNAME" Core.<$> items)

instance Core.FromXML Aliases where
  parseXML x =
    Aliases'
      Core.<$> (x Core..@ "Quantity")
      Core.<*> (x Core..@? "Items" Core..<@> Core.parseXMLList "CNAME")
