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
-- Module      : Amazonka.IoTSiteWise.Types.Variant
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.Variant where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains an asset property value (of a single type only).
--
-- /See:/ 'newVariant' smart constructor.
data Variant = Variant'
  { -- | Asset property data of type integer (whole number).
    integerValue :: Prelude.Maybe Prelude.Int,
    -- | Asset property data of type double (floating point number).
    doubleValue :: Prelude.Maybe Prelude.Double,
    -- | Asset property data of type Boolean (true or false).
    booleanValue :: Prelude.Maybe Prelude.Bool,
    -- | Asset property data of type string (sequence of characters).
    stringValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Variant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'integerValue', 'variant_integerValue' - Asset property data of type integer (whole number).
--
-- 'doubleValue', 'variant_doubleValue' - Asset property data of type double (floating point number).
--
-- 'booleanValue', 'variant_booleanValue' - Asset property data of type Boolean (true or false).
--
-- 'stringValue', 'variant_stringValue' - Asset property data of type string (sequence of characters).
newVariant ::
  Variant
newVariant =
  Variant'
    { integerValue = Prelude.Nothing,
      doubleValue = Prelude.Nothing,
      booleanValue = Prelude.Nothing,
      stringValue = Prelude.Nothing
    }

-- | Asset property data of type integer (whole number).
variant_integerValue :: Lens.Lens' Variant (Prelude.Maybe Prelude.Int)
variant_integerValue = Lens.lens (\Variant' {integerValue} -> integerValue) (\s@Variant' {} a -> s {integerValue = a} :: Variant)

-- | Asset property data of type double (floating point number).
variant_doubleValue :: Lens.Lens' Variant (Prelude.Maybe Prelude.Double)
variant_doubleValue = Lens.lens (\Variant' {doubleValue} -> doubleValue) (\s@Variant' {} a -> s {doubleValue = a} :: Variant)

-- | Asset property data of type Boolean (true or false).
variant_booleanValue :: Lens.Lens' Variant (Prelude.Maybe Prelude.Bool)
variant_booleanValue = Lens.lens (\Variant' {booleanValue} -> booleanValue) (\s@Variant' {} a -> s {booleanValue = a} :: Variant)

-- | Asset property data of type string (sequence of characters).
variant_stringValue :: Lens.Lens' Variant (Prelude.Maybe Prelude.Text)
variant_stringValue = Lens.lens (\Variant' {stringValue} -> stringValue) (\s@Variant' {} a -> s {stringValue = a} :: Variant)

instance Data.FromJSON Variant where
  parseJSON =
    Data.withObject
      "Variant"
      ( \x ->
          Variant'
            Prelude.<$> (x Data..:? "integerValue")
            Prelude.<*> (x Data..:? "doubleValue")
            Prelude.<*> (x Data..:? "booleanValue")
            Prelude.<*> (x Data..:? "stringValue")
      )

instance Prelude.Hashable Variant where
  hashWithSalt _salt Variant' {..} =
    _salt `Prelude.hashWithSalt` integerValue
      `Prelude.hashWithSalt` doubleValue
      `Prelude.hashWithSalt` booleanValue
      `Prelude.hashWithSalt` stringValue

instance Prelude.NFData Variant where
  rnf Variant' {..} =
    Prelude.rnf integerValue
      `Prelude.seq` Prelude.rnf doubleValue
      `Prelude.seq` Prelude.rnf booleanValue
      `Prelude.seq` Prelude.rnf stringValue

instance Data.ToJSON Variant where
  toJSON Variant' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("integerValue" Data..=) Prelude.<$> integerValue,
            ("doubleValue" Data..=) Prelude.<$> doubleValue,
            ("booleanValue" Data..=) Prelude.<$> booleanValue,
            ("stringValue" Data..=) Prelude.<$> stringValue
          ]
      )
