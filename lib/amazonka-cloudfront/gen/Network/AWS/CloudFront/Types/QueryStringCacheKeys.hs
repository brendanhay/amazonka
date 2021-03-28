{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.QueryStringCacheKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.QueryStringCacheKeys
  ( QueryStringCacheKeys (..)
  -- * Smart constructor
  , mkQueryStringCacheKeys
  -- * Lenses
  , qsckQuantity
  , qsckItems
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field.
--
-- If you want to include query strings in the cache key, use @QueryStringsConfig@ in a cache policy. See @CachePolicy@ .
-- If you want to send query strings to the origin but not include them in the cache key, use @QueryStringsConfig@ in an origin request policy. See @OriginRequestPolicy@ .
-- A complex type that contains information about the query string parameters that you want CloudFront to use for caching for a cache behavior. 
--
-- /See:/ 'mkQueryStringCacheKeys' smart constructor.
data QueryStringCacheKeys = QueryStringCacheKeys'
  { quantity :: Core.Int
    -- ^ The number of @whitelisted@ query string parameters for a cache behavior.
  , items :: Core.Maybe [Core.Text]
    -- ^ A list that contains the query string parameters that you want CloudFront to use as a basis for caching for a cache behavior. If @Quantity@ is 0, you can omit @Items@ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryStringCacheKeys' value with any optional fields omitted.
mkQueryStringCacheKeys
    :: Core.Int -- ^ 'quantity'
    -> QueryStringCacheKeys
mkQueryStringCacheKeys quantity
  = QueryStringCacheKeys'{quantity, items = Core.Nothing}

-- | The number of @whitelisted@ query string parameters for a cache behavior.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsckQuantity :: Lens.Lens' QueryStringCacheKeys Core.Int
qsckQuantity = Lens.field @"quantity"
{-# INLINEABLE qsckQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | A list that contains the query string parameters that you want CloudFront to use as a basis for caching for a cache behavior. If @Quantity@ is 0, you can omit @Items@ . 
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsckItems :: Lens.Lens' QueryStringCacheKeys (Core.Maybe [Core.Text])
qsckItems = Lens.field @"items"
{-# INLINEABLE qsckItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

instance Core.ToXML QueryStringCacheKeys where
        toXML QueryStringCacheKeys{..}
          = Core.toXMLElement "Quantity" quantity Core.<>
              Core.toXMLElement "Items"
                (Core.maybe Core.mempty (Core.toXMLList "Name") items)

instance Core.FromXML QueryStringCacheKeys where
        parseXML x
          = QueryStringCacheKeys' Core.<$>
              (x Core..@ "Quantity") Core.<*>
                x Core..@? "Items" Core..<@> Core.parseXMLList "Name"
