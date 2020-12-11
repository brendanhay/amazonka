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

import Network.AWS.CloudFront.Types.Method
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { quantity :: Lude.Int,
    items :: [Method]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CachedMethods' with the minimum fields required to make a request.
--
-- * 'items' - A complex type that contains the HTTP methods that you want CloudFront to cache responses to.
-- * 'quantity' - The number of HTTP methods for which you want CloudFront to cache responses. Valid values are @2@ (for caching responses to @GET@ and @HEAD@ requests) and @3@ (for caching responses to @GET@ , @HEAD@ , and @OPTIONS@ requests).
mkCachedMethods ::
  -- | 'quantity'
  Lude.Int ->
  CachedMethods
mkCachedMethods pQuantity_ =
  CachedMethods' {quantity = pQuantity_, items = Lude.mempty}

-- | The number of HTTP methods for which you want CloudFront to cache responses. Valid values are @2@ (for caching responses to @GET@ and @HEAD@ requests) and @3@ (for caching responses to @GET@ , @HEAD@ , and @OPTIONS@ requests).
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmQuantity :: Lens.Lens' CachedMethods Lude.Int
cmQuantity = Lens.lens (quantity :: CachedMethods -> Lude.Int) (\s a -> s {quantity = a} :: CachedMethods)
{-# DEPRECATED cmQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A complex type that contains the HTTP methods that you want CloudFront to cache responses to.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmItems :: Lens.Lens' CachedMethods [Method]
cmItems = Lens.lens (items :: CachedMethods -> [Method]) (\s a -> s {items = a} :: CachedMethods)
{-# DEPRECATED cmItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.FromXML CachedMethods where
  parseXML x =
    CachedMethods'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.parseXMLList "Method"
               )

instance Lude.ToXML CachedMethods where
  toXML CachedMethods' {..} =
    Lude.mconcat
      [ "Quantity" Lude.@= quantity,
        "Items" Lude.@= Lude.toXMLList "Method" items
      ]
