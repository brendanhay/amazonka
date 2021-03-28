{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CustomHeaders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.CustomHeaders
  ( CustomHeaders (..)
  -- * Smart constructor
  , mkCustomHeaders
  -- * Lenses
  , chQuantity
  , chItems
  ) where

import qualified Network.AWS.CloudFront.Types.OriginCustomHeader as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that contains the list of Custom Headers for each origin. 
--
-- /See:/ 'mkCustomHeaders' smart constructor.
data CustomHeaders = CustomHeaders'
  { quantity :: Core.Int
    -- ^ The number of custom headers, if any, for this distribution.
  , items :: Core.Maybe [Types.OriginCustomHeader]
    -- ^ __Optional__ : A list that contains one @OriginCustomHeader@ element for each custom header that you want CloudFront to forward to the origin. If Quantity is @0@ , omit @Items@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomHeaders' value with any optional fields omitted.
mkCustomHeaders
    :: Core.Int -- ^ 'quantity'
    -> CustomHeaders
mkCustomHeaders quantity
  = CustomHeaders'{quantity, items = Core.Nothing}

-- | The number of custom headers, if any, for this distribution.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chQuantity :: Lens.Lens' CustomHeaders Core.Int
chQuantity = Lens.field @"quantity"
{-# INLINEABLE chQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | __Optional__ : A list that contains one @OriginCustomHeader@ element for each custom header that you want CloudFront to forward to the origin. If Quantity is @0@ , omit @Items@ .
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chItems :: Lens.Lens' CustomHeaders (Core.Maybe [Types.OriginCustomHeader])
chItems = Lens.field @"items"
{-# INLINEABLE chItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

instance Core.ToXML CustomHeaders where
        toXML CustomHeaders{..}
          = Core.toXMLElement "Quantity" quantity Core.<>
              Core.toXMLElement "Items"
                (Core.maybe Core.mempty (Core.toXMLList "OriginCustomHeader")
                   items)

instance Core.FromXML CustomHeaders where
        parseXML x
          = CustomHeaders' Core.<$>
              (x Core..@ "Quantity") Core.<*>
                x Core..@? "Items" Core..<@> Core.parseXMLList "OriginCustomHeader"
