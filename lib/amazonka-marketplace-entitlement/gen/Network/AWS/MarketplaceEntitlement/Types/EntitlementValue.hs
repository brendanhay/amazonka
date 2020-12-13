{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceEntitlement.Types.EntitlementValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceEntitlement.Types.EntitlementValue
  ( EntitlementValue (..),

    -- * Smart constructor
    mkEntitlementValue,

    -- * Lenses
    evIntegerValue,
    evDoubleValue,
    evStringValue,
    evBooleanValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The EntitlementValue represents the amount of capacity that the customer is entitled to for the product.
--
-- /See:/ 'mkEntitlementValue' smart constructor.
data EntitlementValue = EntitlementValue'
  { -- | The IntegerValue field will be populated with an integer value when the entitlement is an integer type. Otherwise, the field will not be set.
    integerValue :: Lude.Maybe Lude.Int,
    -- | The DoubleValue field will be populated with a double value when the entitlement is a double type. Otherwise, the field will not be set.
    doubleValue :: Lude.Maybe Lude.Double,
    -- | The StringValue field will be populated with a string value when the entitlement is a string type. Otherwise, the field will not be set.
    stringValue :: Lude.Maybe Lude.Text,
    -- | The BooleanValue field will be populated with a boolean value when the entitlement is a boolean type. Otherwise, the field will not be set.
    booleanValue :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EntitlementValue' with the minimum fields required to make a request.
--
-- * 'integerValue' - The IntegerValue field will be populated with an integer value when the entitlement is an integer type. Otherwise, the field will not be set.
-- * 'doubleValue' - The DoubleValue field will be populated with a double value when the entitlement is a double type. Otherwise, the field will not be set.
-- * 'stringValue' - The StringValue field will be populated with a string value when the entitlement is a string type. Otherwise, the field will not be set.
-- * 'booleanValue' - The BooleanValue field will be populated with a boolean value when the entitlement is a boolean type. Otherwise, the field will not be set.
mkEntitlementValue ::
  EntitlementValue
mkEntitlementValue =
  EntitlementValue'
    { integerValue = Lude.Nothing,
      doubleValue = Lude.Nothing,
      stringValue = Lude.Nothing,
      booleanValue = Lude.Nothing
    }

-- | The IntegerValue field will be populated with an integer value when the entitlement is an integer type. Otherwise, the field will not be set.
--
-- /Note:/ Consider using 'integerValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evIntegerValue :: Lens.Lens' EntitlementValue (Lude.Maybe Lude.Int)
evIntegerValue = Lens.lens (integerValue :: EntitlementValue -> Lude.Maybe Lude.Int) (\s a -> s {integerValue = a} :: EntitlementValue)
{-# DEPRECATED evIntegerValue "Use generic-lens or generic-optics with 'integerValue' instead." #-}

-- | The DoubleValue field will be populated with a double value when the entitlement is a double type. Otherwise, the field will not be set.
--
-- /Note:/ Consider using 'doubleValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evDoubleValue :: Lens.Lens' EntitlementValue (Lude.Maybe Lude.Double)
evDoubleValue = Lens.lens (doubleValue :: EntitlementValue -> Lude.Maybe Lude.Double) (\s a -> s {doubleValue = a} :: EntitlementValue)
{-# DEPRECATED evDoubleValue "Use generic-lens or generic-optics with 'doubleValue' instead." #-}

-- | The StringValue field will be populated with a string value when the entitlement is a string type. Otherwise, the field will not be set.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evStringValue :: Lens.Lens' EntitlementValue (Lude.Maybe Lude.Text)
evStringValue = Lens.lens (stringValue :: EntitlementValue -> Lude.Maybe Lude.Text) (\s a -> s {stringValue = a} :: EntitlementValue)
{-# DEPRECATED evStringValue "Use generic-lens or generic-optics with 'stringValue' instead." #-}

-- | The BooleanValue field will be populated with a boolean value when the entitlement is a boolean type. Otherwise, the field will not be set.
--
-- /Note:/ Consider using 'booleanValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evBooleanValue :: Lens.Lens' EntitlementValue (Lude.Maybe Lude.Bool)
evBooleanValue = Lens.lens (booleanValue :: EntitlementValue -> Lude.Maybe Lude.Bool) (\s a -> s {booleanValue = a} :: EntitlementValue)
{-# DEPRECATED evBooleanValue "Use generic-lens or generic-optics with 'booleanValue' instead." #-}

instance Lude.FromJSON EntitlementValue where
  parseJSON =
    Lude.withObject
      "EntitlementValue"
      ( \x ->
          EntitlementValue'
            Lude.<$> (x Lude..:? "IntegerValue")
            Lude.<*> (x Lude..:? "DoubleValue")
            Lude.<*> (x Lude..:? "StringValue")
            Lude.<*> (x Lude..:? "BooleanValue")
      )
