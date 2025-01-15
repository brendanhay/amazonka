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
-- Module      : Amazonka.QuickSight.Types.IntegerDefaultValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.IntegerDefaultValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DynamicDefaultValue

-- | The default values of the @IntegerParameterDeclaration@.
--
-- /See:/ 'newIntegerDefaultValues' smart constructor.
data IntegerDefaultValues = IntegerDefaultValues'
  { -- | The dynamic value of the @IntegerDefaultValues@. Different defaults are
    -- displayed according to users, groups, and values mapping.
    dynamicValue :: Prelude.Maybe DynamicDefaultValue,
    -- | The static values of the @IntegerDefaultValues@.
    staticValues :: Prelude.Maybe [Data.Sensitive Prelude.Integer]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntegerDefaultValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dynamicValue', 'integerDefaultValues_dynamicValue' - The dynamic value of the @IntegerDefaultValues@. Different defaults are
-- displayed according to users, groups, and values mapping.
--
-- 'staticValues', 'integerDefaultValues_staticValues' - The static values of the @IntegerDefaultValues@.
newIntegerDefaultValues ::
  IntegerDefaultValues
newIntegerDefaultValues =
  IntegerDefaultValues'
    { dynamicValue =
        Prelude.Nothing,
      staticValues = Prelude.Nothing
    }

-- | The dynamic value of the @IntegerDefaultValues@. Different defaults are
-- displayed according to users, groups, and values mapping.
integerDefaultValues_dynamicValue :: Lens.Lens' IntegerDefaultValues (Prelude.Maybe DynamicDefaultValue)
integerDefaultValues_dynamicValue = Lens.lens (\IntegerDefaultValues' {dynamicValue} -> dynamicValue) (\s@IntegerDefaultValues' {} a -> s {dynamicValue = a} :: IntegerDefaultValues)

-- | The static values of the @IntegerDefaultValues@.
integerDefaultValues_staticValues :: Lens.Lens' IntegerDefaultValues (Prelude.Maybe [Prelude.Integer])
integerDefaultValues_staticValues = Lens.lens (\IntegerDefaultValues' {staticValues} -> staticValues) (\s@IntegerDefaultValues' {} a -> s {staticValues = a} :: IntegerDefaultValues) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON IntegerDefaultValues where
  parseJSON =
    Data.withObject
      "IntegerDefaultValues"
      ( \x ->
          IntegerDefaultValues'
            Prelude.<$> (x Data..:? "DynamicValue")
            Prelude.<*> (x Data..:? "StaticValues" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable IntegerDefaultValues where
  hashWithSalt _salt IntegerDefaultValues' {..} =
    _salt
      `Prelude.hashWithSalt` dynamicValue
      `Prelude.hashWithSalt` staticValues

instance Prelude.NFData IntegerDefaultValues where
  rnf IntegerDefaultValues' {..} =
    Prelude.rnf dynamicValue `Prelude.seq`
      Prelude.rnf staticValues

instance Data.ToJSON IntegerDefaultValues where
  toJSON IntegerDefaultValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DynamicValue" Data..=) Prelude.<$> dynamicValue,
            ("StaticValues" Data..=) Prelude.<$> staticValues
          ]
      )
