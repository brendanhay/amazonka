{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.LambdaFunctionAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.LambdaFunctionAssociations
  ( LambdaFunctionAssociations (..),

    -- * Smart constructor
    mkLambdaFunctionAssociations,

    -- * Lenses
    lfaQuantity,
    lfaItems,
  )
where

import qualified Network.AWS.CloudFront.Types.LambdaFunctionAssociation as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that specifies a list of Lambda functions associations for a cache behavior.
--
-- If you want to invoke one or more Lambda functions triggered by requests that match the @PathPattern@ of the cache behavior, specify the applicable values for @Quantity@ and @Items@ . Note that there can be up to 4 @LambdaFunctionAssociation@ items in this list (one for each possible value of @EventType@ ) and each @EventType@ can be associated with the Lambda function only once.
-- If you don't want to invoke any Lambda functions for the requests that match @PathPattern@ , specify @0@ for @Quantity@ and omit @Items@ .
--
-- /See:/ 'mkLambdaFunctionAssociations' smart constructor.
data LambdaFunctionAssociations = LambdaFunctionAssociations'
  { -- | The number of Lambda function associations for this cache behavior.
    quantity :: Core.Int,
    -- | __Optional__ : A complex type that contains @LambdaFunctionAssociation@ items for this cache behavior. If @Quantity@ is @0@ , you can omit @Items@ .
    items :: Core.Maybe [Types.LambdaFunctionAssociation]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaFunctionAssociations' value with any optional fields omitted.
mkLambdaFunctionAssociations ::
  -- | 'quantity'
  Core.Int ->
  LambdaFunctionAssociations
mkLambdaFunctionAssociations quantity =
  LambdaFunctionAssociations' {quantity, items = Core.Nothing}

-- | The number of Lambda function associations for this cache behavior.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfaQuantity :: Lens.Lens' LambdaFunctionAssociations Core.Int
lfaQuantity = Lens.field @"quantity"
{-# DEPRECATED lfaQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | __Optional__ : A complex type that contains @LambdaFunctionAssociation@ items for this cache behavior. If @Quantity@ is @0@ , you can omit @Items@ .
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfaItems :: Lens.Lens' LambdaFunctionAssociations (Core.Maybe [Types.LambdaFunctionAssociation])
lfaItems = Lens.field @"items"
{-# DEPRECATED lfaItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.ToXML LambdaFunctionAssociations where
  toXML LambdaFunctionAssociations {..} =
    Core.toXMLNode "Quantity" quantity
      Core.<> Core.toXMLNode
        "Items"
        (Core.toXMLList "LambdaFunctionAssociation" Core.<$> items)

instance Core.FromXML LambdaFunctionAssociations where
  parseXML x =
    LambdaFunctionAssociations'
      Core.<$> (x Core..@ "Quantity")
      Core.<*> ( x Core..@? "Items"
                   Core..<@> Core.parseXMLList "LambdaFunctionAssociation"
               )
