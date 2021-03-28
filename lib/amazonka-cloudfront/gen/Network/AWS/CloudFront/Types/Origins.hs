{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Origins
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.Origins
  ( Origins (..)
  -- * Smart constructor
  , mkOrigins
  -- * Lenses
  , oQuantity
  , oItems
  ) where

import qualified Network.AWS.CloudFront.Types.Origin as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the origins for this distribution.
--
-- /See:/ 'mkOrigins' smart constructor.
data Origins = Origins'
  { quantity :: Core.Int
    -- ^ The number of origins for this distribution.
  , items :: Core.NonEmpty Types.Origin
    -- ^ A list of origins.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Origins' value with any optional fields omitted.
mkOrigins
    :: Core.Int -- ^ 'quantity'
    -> Core.NonEmpty Types.Origin -- ^ 'items'
    -> Origins
mkOrigins quantity items = Origins'{quantity, items}

-- | The number of origins for this distribution.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oQuantity :: Lens.Lens' Origins Core.Int
oQuantity = Lens.field @"quantity"
{-# INLINEABLE oQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | A list of origins.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oItems :: Lens.Lens' Origins (Core.NonEmpty Types.Origin)
oItems = Lens.field @"items"
{-# INLINEABLE oItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

instance Core.ToXML Origins where
        toXML Origins{..}
          = Core.toXMLElement "Quantity" quantity Core.<>
              Core.toXMLElement "Items" (Core.toXMLList "Origin" items)

instance Core.FromXML Origins where
        parseXML x
          = Origins' Core.<$>
              (x Core..@ "Quantity") Core.<*>
                x Core..@ "Items" Core..<@> Core.parseXMLNonEmpty "Origin"
