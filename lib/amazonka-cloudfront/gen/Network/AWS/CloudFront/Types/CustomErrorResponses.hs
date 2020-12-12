{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CustomErrorResponses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CustomErrorResponses
  ( CustomErrorResponses (..),

    -- * Smart constructor
    mkCustomErrorResponses,

    -- * Lenses
    cerItems,
    cerQuantity,
  )
where

import Network.AWS.CloudFront.Types.CustomErrorResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that controls:
--
--
--     * Whether CloudFront replaces HTTP status codes in the 4xx and 5xx range with custom error messages before returning the response to the viewer.
--
--
--     * How long CloudFront caches HTTP status codes in the 4xx and 5xx range.
--
--
-- For more information about custom error pages, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
--
-- /See:/ 'mkCustomErrorResponses' smart constructor.
data CustomErrorResponses = CustomErrorResponses'
  { items ::
      Lude.Maybe [CustomErrorResponse],
    quantity :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomErrorResponses' with the minimum fields required to make a request.
--
-- * 'items' - A complex type that contains a @CustomErrorResponse@ element for each HTTP status code for which you want to specify a custom error page and/or a caching duration.
-- * 'quantity' - The number of HTTP status codes for which you want to specify a custom error page and/or a caching duration. If @Quantity@ is @0@ , you can omit @Items@ .
mkCustomErrorResponses ::
  -- | 'quantity'
  Lude.Int ->
  CustomErrorResponses
mkCustomErrorResponses pQuantity_ =
  CustomErrorResponses'
    { items = Lude.Nothing,
      quantity = pQuantity_
    }

-- | A complex type that contains a @CustomErrorResponse@ element for each HTTP status code for which you want to specify a custom error page and/or a caching duration.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerItems :: Lens.Lens' CustomErrorResponses (Lude.Maybe [CustomErrorResponse])
cerItems = Lens.lens (items :: CustomErrorResponses -> Lude.Maybe [CustomErrorResponse]) (\s a -> s {items = a} :: CustomErrorResponses)
{-# DEPRECATED cerItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | The number of HTTP status codes for which you want to specify a custom error page and/or a caching duration. If @Quantity@ is @0@ , you can omit @Items@ .
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerQuantity :: Lens.Lens' CustomErrorResponses Lude.Int
cerQuantity = Lens.lens (quantity :: CustomErrorResponses -> Lude.Int) (\s a -> s {quantity = a} :: CustomErrorResponses)
{-# DEPRECATED cerQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

instance Lude.FromXML CustomErrorResponses where
  parseXML x =
    CustomErrorResponses'
      Lude.<$> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "CustomErrorResponse")
               )
      Lude.<*> (x Lude..@ "Quantity")

instance Lude.ToXML CustomErrorResponses where
  toXML CustomErrorResponses' {..} =
    Lude.mconcat
      [ "Items"
          Lude.@= Lude.toXML (Lude.toXMLList "CustomErrorResponse" Lude.<$> items),
        "Quantity" Lude.@= quantity
      ]
