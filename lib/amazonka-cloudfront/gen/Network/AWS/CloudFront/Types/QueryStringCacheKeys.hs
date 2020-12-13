{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.QueryStringCacheKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.QueryStringCacheKeys
  ( QueryStringCacheKeys (..),

    -- * Smart constructor
    mkQueryStringCacheKeys,

    -- * Lenses
    qsckQuantity,
    qsckItems,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field.
--
-- If you want to include query strings in the cache key, use @QueryStringsConfig@ in a cache policy. See @CachePolicy@ .
-- If you want to send query strings to the origin but not include them in the cache key, use @QueryStringsConfig@ in an origin request policy. See @OriginRequestPolicy@ .
-- A complex type that contains information about the query string parameters that you want CloudFront to use for caching for a cache behavior.
--
-- /See:/ 'mkQueryStringCacheKeys' smart constructor.
data QueryStringCacheKeys = QueryStringCacheKeys'
  { -- | The number of @whitelisted@ query string parameters for a cache behavior.
    quantity :: Lude.Int,
    -- | A list that contains the query string parameters that you want CloudFront to use as a basis for caching for a cache behavior. If @Quantity@ is 0, you can omit @Items@ .
    items :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueryStringCacheKeys' with the minimum fields required to make a request.
--
-- * 'quantity' - The number of @whitelisted@ query string parameters for a cache behavior.
-- * 'items' - A list that contains the query string parameters that you want CloudFront to use as a basis for caching for a cache behavior. If @Quantity@ is 0, you can omit @Items@ .
mkQueryStringCacheKeys ::
  -- | 'quantity'
  Lude.Int ->
  QueryStringCacheKeys
mkQueryStringCacheKeys pQuantity_ =
  QueryStringCacheKeys'
    { quantity = pQuantity_,
      items = Lude.Nothing
    }

-- | The number of @whitelisted@ query string parameters for a cache behavior.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsckQuantity :: Lens.Lens' QueryStringCacheKeys Lude.Int
qsckQuantity = Lens.lens (quantity :: QueryStringCacheKeys -> Lude.Int) (\s a -> s {quantity = a} :: QueryStringCacheKeys)
{-# DEPRECATED qsckQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A list that contains the query string parameters that you want CloudFront to use as a basis for caching for a cache behavior. If @Quantity@ is 0, you can omit @Items@ .
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsckItems :: Lens.Lens' QueryStringCacheKeys (Lude.Maybe [Lude.Text])
qsckItems = Lens.lens (items :: QueryStringCacheKeys -> Lude.Maybe [Lude.Text]) (\s a -> s {items = a} :: QueryStringCacheKeys)
{-# DEPRECATED qsckItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.FromXML QueryStringCacheKeys where
  parseXML x =
    QueryStringCacheKeys'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Name")
               )

instance Lude.ToXML QueryStringCacheKeys where
  toXML QueryStringCacheKeys' {..} =
    Lude.mconcat
      [ "Quantity" Lude.@= quantity,
        "Items" Lude.@= Lude.toXML (Lude.toXMLList "Name" Lude.<$> items)
      ]
