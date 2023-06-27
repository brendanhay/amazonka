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
-- Module      : Amazonka.QuickSight.Types.DecimalDefaultValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DecimalDefaultValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DynamicDefaultValue

-- | The default values of the @DecimalParameterDeclaration@.
--
-- /See:/ 'newDecimalDefaultValues' smart constructor.
data DecimalDefaultValues = DecimalDefaultValues'
  { -- | The dynamic value of the @DecimalDefaultValues@. Different defaults are
    -- displayed according to users, groups, and values mapping.
    dynamicValue :: Prelude.Maybe DynamicDefaultValue,
    -- | The static values of the @DecimalDefaultValues@.
    staticValues :: Prelude.Maybe [Data.Sensitive Prelude.Double]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecimalDefaultValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dynamicValue', 'decimalDefaultValues_dynamicValue' - The dynamic value of the @DecimalDefaultValues@. Different defaults are
-- displayed according to users, groups, and values mapping.
--
-- 'staticValues', 'decimalDefaultValues_staticValues' - The static values of the @DecimalDefaultValues@.
newDecimalDefaultValues ::
  DecimalDefaultValues
newDecimalDefaultValues =
  DecimalDefaultValues'
    { dynamicValue =
        Prelude.Nothing,
      staticValues = Prelude.Nothing
    }

-- | The dynamic value of the @DecimalDefaultValues@. Different defaults are
-- displayed according to users, groups, and values mapping.
decimalDefaultValues_dynamicValue :: Lens.Lens' DecimalDefaultValues (Prelude.Maybe DynamicDefaultValue)
decimalDefaultValues_dynamicValue = Lens.lens (\DecimalDefaultValues' {dynamicValue} -> dynamicValue) (\s@DecimalDefaultValues' {} a -> s {dynamicValue = a} :: DecimalDefaultValues)

-- | The static values of the @DecimalDefaultValues@.
decimalDefaultValues_staticValues :: Lens.Lens' DecimalDefaultValues (Prelude.Maybe [Prelude.Double])
decimalDefaultValues_staticValues = Lens.lens (\DecimalDefaultValues' {staticValues} -> staticValues) (\s@DecimalDefaultValues' {} a -> s {staticValues = a} :: DecimalDefaultValues) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DecimalDefaultValues where
  parseJSON =
    Data.withObject
      "DecimalDefaultValues"
      ( \x ->
          DecimalDefaultValues'
            Prelude.<$> (x Data..:? "DynamicValue")
            Prelude.<*> (x Data..:? "StaticValues" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DecimalDefaultValues where
  hashWithSalt _salt DecimalDefaultValues' {..} =
    _salt
      `Prelude.hashWithSalt` dynamicValue
      `Prelude.hashWithSalt` staticValues

instance Prelude.NFData DecimalDefaultValues where
  rnf DecimalDefaultValues' {..} =
    Prelude.rnf dynamicValue
      `Prelude.seq` Prelude.rnf staticValues

instance Data.ToJSON DecimalDefaultValues where
  toJSON DecimalDefaultValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DynamicValue" Data..=) Prelude.<$> dynamicValue,
            ("StaticValues" Data..=) Prelude.<$> staticValues
          ]
      )
