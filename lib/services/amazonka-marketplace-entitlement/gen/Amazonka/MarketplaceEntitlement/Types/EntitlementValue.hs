{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MarketplaceEntitlement.Types.EntitlementValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceEntitlement.Types.EntitlementValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The EntitlementValue represents the amount of capacity that the customer
-- is entitled to for the product.
--
-- /See:/ 'newEntitlementValue' smart constructor.
data EntitlementValue = EntitlementValue'
  { -- | The BooleanValue field will be populated with a boolean value when the
    -- entitlement is a boolean type. Otherwise, the field will not be set.
    booleanValue :: Prelude.Maybe Prelude.Bool,
    -- | The DoubleValue field will be populated with a double value when the
    -- entitlement is a double type. Otherwise, the field will not be set.
    doubleValue :: Prelude.Maybe Prelude.Double,
    -- | The IntegerValue field will be populated with an integer value when the
    -- entitlement is an integer type. Otherwise, the field will not be set.
    integerValue :: Prelude.Maybe Prelude.Int,
    -- | The StringValue field will be populated with a string value when the
    -- entitlement is a string type. Otherwise, the field will not be set.
    stringValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntitlementValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'booleanValue', 'entitlementValue_booleanValue' - The BooleanValue field will be populated with a boolean value when the
-- entitlement is a boolean type. Otherwise, the field will not be set.
--
-- 'doubleValue', 'entitlementValue_doubleValue' - The DoubleValue field will be populated with a double value when the
-- entitlement is a double type. Otherwise, the field will not be set.
--
-- 'integerValue', 'entitlementValue_integerValue' - The IntegerValue field will be populated with an integer value when the
-- entitlement is an integer type. Otherwise, the field will not be set.
--
-- 'stringValue', 'entitlementValue_stringValue' - The StringValue field will be populated with a string value when the
-- entitlement is a string type. Otherwise, the field will not be set.
newEntitlementValue ::
  EntitlementValue
newEntitlementValue =
  EntitlementValue'
    { booleanValue = Prelude.Nothing,
      doubleValue = Prelude.Nothing,
      integerValue = Prelude.Nothing,
      stringValue = Prelude.Nothing
    }

-- | The BooleanValue field will be populated with a boolean value when the
-- entitlement is a boolean type. Otherwise, the field will not be set.
entitlementValue_booleanValue :: Lens.Lens' EntitlementValue (Prelude.Maybe Prelude.Bool)
entitlementValue_booleanValue = Lens.lens (\EntitlementValue' {booleanValue} -> booleanValue) (\s@EntitlementValue' {} a -> s {booleanValue = a} :: EntitlementValue)

-- | The DoubleValue field will be populated with a double value when the
-- entitlement is a double type. Otherwise, the field will not be set.
entitlementValue_doubleValue :: Lens.Lens' EntitlementValue (Prelude.Maybe Prelude.Double)
entitlementValue_doubleValue = Lens.lens (\EntitlementValue' {doubleValue} -> doubleValue) (\s@EntitlementValue' {} a -> s {doubleValue = a} :: EntitlementValue)

-- | The IntegerValue field will be populated with an integer value when the
-- entitlement is an integer type. Otherwise, the field will not be set.
entitlementValue_integerValue :: Lens.Lens' EntitlementValue (Prelude.Maybe Prelude.Int)
entitlementValue_integerValue = Lens.lens (\EntitlementValue' {integerValue} -> integerValue) (\s@EntitlementValue' {} a -> s {integerValue = a} :: EntitlementValue)

-- | The StringValue field will be populated with a string value when the
-- entitlement is a string type. Otherwise, the field will not be set.
entitlementValue_stringValue :: Lens.Lens' EntitlementValue (Prelude.Maybe Prelude.Text)
entitlementValue_stringValue = Lens.lens (\EntitlementValue' {stringValue} -> stringValue) (\s@EntitlementValue' {} a -> s {stringValue = a} :: EntitlementValue)

instance Data.FromJSON EntitlementValue where
  parseJSON =
    Data.withObject
      "EntitlementValue"
      ( \x ->
          EntitlementValue'
            Prelude.<$> (x Data..:? "BooleanValue")
            Prelude.<*> (x Data..:? "DoubleValue")
            Prelude.<*> (x Data..:? "IntegerValue")
            Prelude.<*> (x Data..:? "StringValue")
      )

instance Prelude.Hashable EntitlementValue where
  hashWithSalt _salt EntitlementValue' {..} =
    _salt
      `Prelude.hashWithSalt` booleanValue
      `Prelude.hashWithSalt` doubleValue
      `Prelude.hashWithSalt` integerValue
      `Prelude.hashWithSalt` stringValue

instance Prelude.NFData EntitlementValue where
  rnf EntitlementValue' {..} =
    Prelude.rnf booleanValue
      `Prelude.seq` Prelude.rnf doubleValue
      `Prelude.seq` Prelude.rnf integerValue
      `Prelude.seq` Prelude.rnf stringValue
