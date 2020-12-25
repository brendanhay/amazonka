{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Paths
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Paths
  ( Paths (..),

    -- * Smart constructor
    mkPaths,

    -- * Lenses
    pQuantity,
    pItems,
  )
where

import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that contains information about the objects that you want to invalidate. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects Specifying the Objects to Invalidate> in the /Amazon CloudFront Developer Guide/ .
--
-- /See:/ 'mkPaths' smart constructor.
data Paths = Paths'
  { -- | The number of invalidation paths specified for the objects that you want to invalidate.
    quantity :: Core.Int,
    -- | A complex type that contains a list of the paths that you want to invalidate.
    items :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Paths' value with any optional fields omitted.
mkPaths ::
  -- | 'quantity'
  Core.Int ->
  Paths
mkPaths quantity = Paths' {quantity, items = Core.Nothing}

-- | The number of invalidation paths specified for the objects that you want to invalidate.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pQuantity :: Lens.Lens' Paths Core.Int
pQuantity = Lens.field @"quantity"
{-# DEPRECATED pQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A complex type that contains a list of the paths that you want to invalidate.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pItems :: Lens.Lens' Paths (Core.Maybe [Types.String])
pItems = Lens.field @"items"
{-# DEPRECATED pItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.ToXML Paths where
  toXML Paths {..} =
    Core.toXMLNode "Quantity" quantity
      Core.<> Core.toXMLNode "Items" (Core.toXMLList "Path" Core.<$> items)

instance Core.FromXML Paths where
  parseXML x =
    Paths'
      Core.<$> (x Core..@ "Quantity")
      Core.<*> (x Core..@? "Items" Core..<@> Core.parseXMLList "Path")
