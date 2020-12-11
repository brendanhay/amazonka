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
    lfaItems,
    lfaQuantity,
  )
where

import Network.AWS.CloudFront.Types.LambdaFunctionAssociation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that specifies a list of Lambda functions associations for a cache behavior.
--
-- If you want to invoke one or more Lambda functions triggered by requests that match the @PathPattern@ of the cache behavior, specify the applicable values for @Quantity@ and @Items@ . Note that there can be up to 4 @LambdaFunctionAssociation@ items in this list (one for each possible value of @EventType@ ) and each @EventType@ can be associated with the Lambda function only once.
-- If you don't want to invoke any Lambda functions for the requests that match @PathPattern@ , specify @0@ for @Quantity@ and omit @Items@ .
--
-- /See:/ 'mkLambdaFunctionAssociations' smart constructor.
data LambdaFunctionAssociations = LambdaFunctionAssociations'
  { items ::
      Lude.Maybe
        [LambdaFunctionAssociation],
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

-- | Creates a value of 'LambdaFunctionAssociations' with the minimum fields required to make a request.
--
-- * 'items' - __Optional__ : A complex type that contains @LambdaFunctionAssociation@ items for this cache behavior. If @Quantity@ is @0@ , you can omit @Items@ .
-- * 'quantity' - The number of Lambda function associations for this cache behavior.
mkLambdaFunctionAssociations ::
  -- | 'quantity'
  Lude.Int ->
  LambdaFunctionAssociations
mkLambdaFunctionAssociations pQuantity_ =
  LambdaFunctionAssociations'
    { items = Lude.Nothing,
      quantity = pQuantity_
    }

-- | __Optional__ : A complex type that contains @LambdaFunctionAssociation@ items for this cache behavior. If @Quantity@ is @0@ , you can omit @Items@ .
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfaItems :: Lens.Lens' LambdaFunctionAssociations (Lude.Maybe [LambdaFunctionAssociation])
lfaItems = Lens.lens (items :: LambdaFunctionAssociations -> Lude.Maybe [LambdaFunctionAssociation]) (\s a -> s {items = a} :: LambdaFunctionAssociations)
{-# DEPRECATED lfaItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | The number of Lambda function associations for this cache behavior.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfaQuantity :: Lens.Lens' LambdaFunctionAssociations Lude.Int
lfaQuantity = Lens.lens (quantity :: LambdaFunctionAssociations -> Lude.Int) (\s a -> s {quantity = a} :: LambdaFunctionAssociations)
{-# DEPRECATED lfaQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

instance Lude.FromXML LambdaFunctionAssociations where
  parseXML x =
    LambdaFunctionAssociations'
      Lude.<$> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "LambdaFunctionAssociation")
               )
      Lude.<*> (x Lude..@ "Quantity")

instance Lude.ToXML LambdaFunctionAssociations where
  toXML LambdaFunctionAssociations' {..} =
    Lude.mconcat
      [ "Items"
          Lude.@= Lude.toXML
            (Lude.toXMLList "LambdaFunctionAssociation" Lude.<$> items),
        "Quantity" Lude.@= quantity
      ]
