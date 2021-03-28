{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceEntitlement.Types.EntitlementValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MarketplaceEntitlement.Types.EntitlementValue
  ( EntitlementValue (..)
  -- * Smart constructor
  , mkEntitlementValue
  -- * Lenses
  , evBooleanValue
  , evDoubleValue
  , evIntegerValue
  , evStringValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The EntitlementValue represents the amount of capacity that the customer is entitled to for the product.
--
-- /See:/ 'mkEntitlementValue' smart constructor.
data EntitlementValue = EntitlementValue'
  { booleanValue :: Core.Maybe Core.Bool
    -- ^ The BooleanValue field will be populated with a boolean value when the entitlement is a boolean type. Otherwise, the field will not be set.
  , doubleValue :: Core.Maybe Core.Double
    -- ^ The DoubleValue field will be populated with a double value when the entitlement is a double type. Otherwise, the field will not be set.
  , integerValue :: Core.Maybe Core.Int
    -- ^ The IntegerValue field will be populated with an integer value when the entitlement is an integer type. Otherwise, the field will not be set.
  , stringValue :: Core.Maybe Core.Text
    -- ^ The StringValue field will be populated with a string value when the entitlement is a string type. Otherwise, the field will not be set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EntitlementValue' value with any optional fields omitted.
mkEntitlementValue
    :: EntitlementValue
mkEntitlementValue
  = EntitlementValue'{booleanValue = Core.Nothing,
                      doubleValue = Core.Nothing, integerValue = Core.Nothing,
                      stringValue = Core.Nothing}

-- | The BooleanValue field will be populated with a boolean value when the entitlement is a boolean type. Otherwise, the field will not be set.
--
-- /Note:/ Consider using 'booleanValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evBooleanValue :: Lens.Lens' EntitlementValue (Core.Maybe Core.Bool)
evBooleanValue = Lens.field @"booleanValue"
{-# INLINEABLE evBooleanValue #-}
{-# DEPRECATED booleanValue "Use generic-lens or generic-optics with 'booleanValue' instead"  #-}

-- | The DoubleValue field will be populated with a double value when the entitlement is a double type. Otherwise, the field will not be set.
--
-- /Note:/ Consider using 'doubleValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evDoubleValue :: Lens.Lens' EntitlementValue (Core.Maybe Core.Double)
evDoubleValue = Lens.field @"doubleValue"
{-# INLINEABLE evDoubleValue #-}
{-# DEPRECATED doubleValue "Use generic-lens or generic-optics with 'doubleValue' instead"  #-}

-- | The IntegerValue field will be populated with an integer value when the entitlement is an integer type. Otherwise, the field will not be set.
--
-- /Note:/ Consider using 'integerValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evIntegerValue :: Lens.Lens' EntitlementValue (Core.Maybe Core.Int)
evIntegerValue = Lens.field @"integerValue"
{-# INLINEABLE evIntegerValue #-}
{-# DEPRECATED integerValue "Use generic-lens or generic-optics with 'integerValue' instead"  #-}

-- | The StringValue field will be populated with a string value when the entitlement is a string type. Otherwise, the field will not be set.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evStringValue :: Lens.Lens' EntitlementValue (Core.Maybe Core.Text)
evStringValue = Lens.field @"stringValue"
{-# INLINEABLE evStringValue #-}
{-# DEPRECATED stringValue "Use generic-lens or generic-optics with 'stringValue' instead"  #-}

instance Core.FromJSON EntitlementValue where
        parseJSON
          = Core.withObject "EntitlementValue" Core.$
              \ x ->
                EntitlementValue' Core.<$>
                  (x Core..:? "BooleanValue") Core.<*> x Core..:? "DoubleValue"
                    Core.<*> x Core..:? "IntegerValue"
                    Core.<*> x Core..:? "StringValue"
