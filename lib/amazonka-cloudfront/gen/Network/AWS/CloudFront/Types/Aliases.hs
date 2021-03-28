{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Aliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.Aliases
  ( Aliases (..)
  -- * Smart constructor
  , mkAliases
  -- * Lenses
  , aQuantity
  , aItems
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution. 
--
-- /See:/ 'mkAliases' smart constructor.
data Aliases = Aliases'
  { quantity :: Core.Int
    -- ^ The number of CNAME aliases, if any, that you want to associate with this distribution.
  , items :: Core.Maybe [Core.Text]
    -- ^ A complex type that contains the CNAME aliases, if any, that you want to associate with this distribution.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Aliases' value with any optional fields omitted.
mkAliases
    :: Core.Int -- ^ 'quantity'
    -> Aliases
mkAliases quantity = Aliases'{quantity, items = Core.Nothing}

-- | The number of CNAME aliases, if any, that you want to associate with this distribution.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aQuantity :: Lens.Lens' Aliases Core.Int
aQuantity = Lens.field @"quantity"
{-# INLINEABLE aQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | A complex type that contains the CNAME aliases, if any, that you want to associate with this distribution.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aItems :: Lens.Lens' Aliases (Core.Maybe [Core.Text])
aItems = Lens.field @"items"
{-# INLINEABLE aItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

instance Core.ToXML Aliases where
        toXML Aliases{..}
          = Core.toXMLElement "Quantity" quantity Core.<>
              Core.toXMLElement "Items"
                (Core.maybe Core.mempty (Core.toXMLList "CNAME") items)

instance Core.FromXML Aliases where
        parseXML x
          = Aliases' Core.<$>
              (x Core..@ "Quantity") Core.<*>
                x Core..@? "Items" Core..<@> Core.parseXMLList "CNAME"
