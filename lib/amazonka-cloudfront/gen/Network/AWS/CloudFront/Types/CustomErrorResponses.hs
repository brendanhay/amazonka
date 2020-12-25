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
    cerQuantity,
    cerItems,
  )
where

import qualified Network.AWS.CloudFront.Types.CustomErrorResponse as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { -- | The number of HTTP status codes for which you want to specify a custom error page and/or a caching duration. If @Quantity@ is @0@ , you can omit @Items@ .
    quantity :: Core.Int,
    -- | A complex type that contains a @CustomErrorResponse@ element for each HTTP status code for which you want to specify a custom error page and/or a caching duration.
    items :: Core.Maybe [Types.CustomErrorResponse]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomErrorResponses' value with any optional fields omitted.
mkCustomErrorResponses ::
  -- | 'quantity'
  Core.Int ->
  CustomErrorResponses
mkCustomErrorResponses quantity =
  CustomErrorResponses' {quantity, items = Core.Nothing}

-- | The number of HTTP status codes for which you want to specify a custom error page and/or a caching duration. If @Quantity@ is @0@ , you can omit @Items@ .
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerQuantity :: Lens.Lens' CustomErrorResponses Core.Int
cerQuantity = Lens.field @"quantity"
{-# DEPRECATED cerQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A complex type that contains a @CustomErrorResponse@ element for each HTTP status code for which you want to specify a custom error page and/or a caching duration.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerItems :: Lens.Lens' CustomErrorResponses (Core.Maybe [Types.CustomErrorResponse])
cerItems = Lens.field @"items"
{-# DEPRECATED cerItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.ToXML CustomErrorResponses where
  toXML CustomErrorResponses {..} =
    Core.toXMLNode "Quantity" quantity
      Core.<> Core.toXMLNode
        "Items"
        (Core.toXMLList "CustomErrorResponse" Core.<$> items)

instance Core.FromXML CustomErrorResponses where
  parseXML x =
    CustomErrorResponses'
      Core.<$> (x Core..@ "Quantity")
      Core.<*> ( x Core..@? "Items"
                   Core..<@> Core.parseXMLList "CustomErrorResponse"
               )
