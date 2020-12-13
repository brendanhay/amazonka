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

import Network.AWS.CloudFront.Types.CachedMethods
import Network.AWS.CloudFront.Types.Method
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
    quantity :: Lude.Int,
    -- | A complex type that contains the HTTP methods that you want CloudFront to process and forward to your origin.
    items :: [Method],
    cachedMethods :: Lude.Maybe CachedMethods
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AllowedMethods' with the minimum fields required to make a request.
--
-- * 'quantity' - The number of HTTP methods that you want CloudFront to forward to your origin. Valid values are 2 (for @GET@ and @HEAD@ requests), 3 (for @GET@ , @HEAD@ , and @OPTIONS@ requests) and 7 (for @GET, HEAD, OPTIONS, PUT, PATCH, POST@ , and @DELETE@ requests).
-- * 'items' - A complex type that contains the HTTP methods that you want CloudFront to process and forward to your origin.
-- * 'cachedMethods' -
mkAllowedMethods ::
  -- | 'quantity'
  Lude.Int ->
  AllowedMethods
mkAllowedMethods pQuantity_ =
  AllowedMethods'
    { quantity = pQuantity_,
      items = Lude.mempty,
      cachedMethods = Lude.Nothing
    }

-- | The number of HTTP methods that you want CloudFront to forward to your origin. Valid values are 2 (for @GET@ and @HEAD@ requests), 3 (for @GET@ , @HEAD@ , and @OPTIONS@ requests) and 7 (for @GET, HEAD, OPTIONS, PUT, PATCH, POST@ , and @DELETE@ requests).
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amQuantity :: Lens.Lens' AllowedMethods Lude.Int
amQuantity = Lens.lens (quantity :: AllowedMethods -> Lude.Int) (\s a -> s {quantity = a} :: AllowedMethods)
{-# DEPRECATED amQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A complex type that contains the HTTP methods that you want CloudFront to process and forward to your origin.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amItems :: Lens.Lens' AllowedMethods [Method]
amItems = Lens.lens (items :: AllowedMethods -> [Method]) (\s a -> s {items = a} :: AllowedMethods)
{-# DEPRECATED amItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cachedMethods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amCachedMethods :: Lens.Lens' AllowedMethods (Lude.Maybe CachedMethods)
amCachedMethods = Lens.lens (cachedMethods :: AllowedMethods -> Lude.Maybe CachedMethods) (\s a -> s {cachedMethods = a} :: AllowedMethods)
{-# DEPRECATED amCachedMethods "Use generic-lens or generic-optics with 'cachedMethods' instead." #-}

instance Lude.FromXML AllowedMethods where
  parseXML x =
    AllowedMethods'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.parseXMLList "Method"
               )
      Lude.<*> (x Lude..@? "CachedMethods")

instance Lude.ToXML AllowedMethods where
  toXML AllowedMethods' {..} =
    Lude.mconcat
      [ "Quantity" Lude.@= quantity,
        "Items" Lude.@= Lude.toXMLList "Method" items,
        "CachedMethods" Lude.@= cachedMethods
      ]
