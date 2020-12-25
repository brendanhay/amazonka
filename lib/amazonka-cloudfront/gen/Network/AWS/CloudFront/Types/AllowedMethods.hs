{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.AllowedMethods
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.AllowedMethods
  ( AllowedMethods (..),

    -- * Smart constructor
    mkAllowedMethods,

    -- * Lenses
    amQuantity,
    amItems,
    amCachedMethods,
  )
where

import qualified Network.AWS.CloudFront.Types.CachedMethods as Types
import qualified Network.AWS.CloudFront.Types.Method as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that controls which HTTP methods CloudFront processes and forwards to your Amazon S3 bucket or your custom origin. There are three choices:
--
--
--     * CloudFront forwards only @GET@ and @HEAD@ requests.
--
--
--     * CloudFront forwards only @GET@ , @HEAD@ , and @OPTIONS@ requests.
--
--
--     * CloudFront forwards @GET, HEAD, OPTIONS, PUT, PATCH, POST@ , and @DELETE@ requests.
--
--
-- If you pick the third choice, you may need to restrict access to your Amazon S3 bucket or to your custom origin so users can't perform operations that you don't want them to. For example, you might not want users to have permissions to delete objects from your origin.
--
-- /See:/ 'mkAllowedMethods' smart constructor.
data AllowedMethods = AllowedMethods'
  { -- | The number of HTTP methods that you want CloudFront to forward to your origin. Valid values are 2 (for @GET@ and @HEAD@ requests), 3 (for @GET@ , @HEAD@ , and @OPTIONS@ requests) and 7 (for @GET, HEAD, OPTIONS, PUT, PATCH, POST@ , and @DELETE@ requests).
    quantity :: Core.Int,
    -- | A complex type that contains the HTTP methods that you want CloudFront to process and forward to your origin.
    items :: [Types.Method],
    cachedMethods :: Core.Maybe Types.CachedMethods
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AllowedMethods' value with any optional fields omitted.
mkAllowedMethods ::
  -- | 'quantity'
  Core.Int ->
  AllowedMethods
mkAllowedMethods quantity =
  AllowedMethods'
    { quantity,
      items = Core.mempty,
      cachedMethods = Core.Nothing
    }

-- | The number of HTTP methods that you want CloudFront to forward to your origin. Valid values are 2 (for @GET@ and @HEAD@ requests), 3 (for @GET@ , @HEAD@ , and @OPTIONS@ requests) and 7 (for @GET, HEAD, OPTIONS, PUT, PATCH, POST@ , and @DELETE@ requests).
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amQuantity :: Lens.Lens' AllowedMethods Core.Int
amQuantity = Lens.field @"quantity"
{-# DEPRECATED amQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A complex type that contains the HTTP methods that you want CloudFront to process and forward to your origin.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amItems :: Lens.Lens' AllowedMethods [Types.Method]
amItems = Lens.field @"items"
{-# DEPRECATED amItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cachedMethods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amCachedMethods :: Lens.Lens' AllowedMethods (Core.Maybe Types.CachedMethods)
amCachedMethods = Lens.field @"cachedMethods"
{-# DEPRECATED amCachedMethods "Use generic-lens or generic-optics with 'cachedMethods' instead." #-}

instance Core.ToXML AllowedMethods where
  toXML AllowedMethods {..} =
    Core.toXMLNode "Quantity" quantity
      Core.<> Core.toXMLNode "Items" (Core.toXMLList "Method" items)
      Core.<> Core.toXMLNode "CachedMethods" Core.<$> cachedMethods

instance Core.FromXML AllowedMethods where
  parseXML x =
    AllowedMethods'
      Core.<$> (x Core..@ "Quantity")
      Core.<*> ( x Core..@? "Items" Core..@! Core.mempty
                   Core..<@> Core.parseXMLList "Method"
               )
      Core.<*> (x Core..@? "CachedMethods")
