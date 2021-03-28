{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KeyPairIds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.KeyPairIds
  ( KeyPairIds (..)
  -- * Smart constructor
  , mkKeyPairIds
  -- * Lenses
  , kpiQuantity
  , kpiItems
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of CloudFront key pair identifiers.
--
-- /See:/ 'mkKeyPairIds' smart constructor.
data KeyPairIds = KeyPairIds'
  { quantity :: Core.Int
    -- ^ The number of key pair identifiers in the list.
  , items :: Core.Maybe [Core.Text]
    -- ^ A list of CloudFront key pair identifiers.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KeyPairIds' value with any optional fields omitted.
mkKeyPairIds
    :: Core.Int -- ^ 'quantity'
    -> KeyPairIds
mkKeyPairIds quantity = KeyPairIds'{quantity, items = Core.Nothing}

-- | The number of key pair identifiers in the list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpiQuantity :: Lens.Lens' KeyPairIds Core.Int
kpiQuantity = Lens.field @"quantity"
{-# INLINEABLE kpiQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | A list of CloudFront key pair identifiers.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpiItems :: Lens.Lens' KeyPairIds (Core.Maybe [Core.Text])
kpiItems = Lens.field @"items"
{-# INLINEABLE kpiItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

instance Core.FromXML KeyPairIds where
        parseXML x
          = KeyPairIds' Core.<$>
              (x Core..@ "Quantity") Core.<*>
                x Core..@? "Items" Core..<@> Core.parseXMLList "KeyPairId"
