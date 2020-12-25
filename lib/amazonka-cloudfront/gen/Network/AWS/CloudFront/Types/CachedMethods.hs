{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachedMethods
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachedMethods
  ( CachedMethods (..),

    -- * Smart constructor
    mkCachedMethods,

    -- * Lenses
    cmQuantity,
    cmItems,
  )
where

import qualified Network.AWS.CloudFront.Types.Method as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that controls whether CloudFront caches the response to requests using the specified HTTP methods. There are two choices:
--
--
--     * CloudFront caches responses to @GET@ and @HEAD@ requests.
--
--
--     * CloudFront caches responses to @GET@ , @HEAD@ , and @OPTIONS@ requests.
--
--
-- If you pick the second choice for your Amazon S3 Origin, you may need to forward Access-Control-Request-Method, Access-Control-Request-Headers, and Origin headers for the responses to be cached correctly.
--
-- /See:/ 'mkCachedMethods' smart constructor.
data CachedMethods = CachedMethods'
  { -- | The number of HTTP methods for which you want CloudFront to cache responses. Valid values are @2@ (for caching responses to @GET@ and @HEAD@ requests) and @3@ (for caching responses to @GET@ , @HEAD@ , and @OPTIONS@ requests).
    quantity :: Core.Int,
    -- | A complex type that contains the HTTP methods that you want CloudFront to cache responses to.
    items :: [Types.Method]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CachedMethods' value with any optional fields omitted.
mkCachedMethods ::
  -- | 'quantity'
  Core.Int ->
  CachedMethods
mkCachedMethods quantity =
  CachedMethods' {quantity, items = Core.mempty}

-- | The number of HTTP methods for which you want CloudFront to cache responses. Valid values are @2@ (for caching responses to @GET@ and @HEAD@ requests) and @3@ (for caching responses to @GET@ , @HEAD@ , and @OPTIONS@ requests).
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmQuantity :: Lens.Lens' CachedMethods Core.Int
cmQuantity = Lens.field @"quantity"
{-# DEPRECATED cmQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A complex type that contains the HTTP methods that you want CloudFront to cache responses to.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmItems :: Lens.Lens' CachedMethods [Types.Method]
cmItems = Lens.field @"items"
{-# DEPRECATED cmItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.ToXML CachedMethods where
  toXML CachedMethods {..} =
    Core.toXMLNode "Quantity" quantity
      Core.<> Core.toXMLNode "Items" (Core.toXMLList "Method" items)

instance Core.FromXML CachedMethods where
  parseXML x =
    CachedMethods'
      Core.<$> (x Core..@ "Quantity")
      Core.<*> ( x Core..@? "Items" Core..@! Core.mempty
                   Core..<@> Core.parseXMLList "Method"
               )
