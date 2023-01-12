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
-- Module      : Amazonka.QuickSight.Types.StringDefaultValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.StringDefaultValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DynamicDefaultValue

-- | The default values of the @StringParameterDeclaration@.
--
-- /See:/ 'newStringDefaultValues' smart constructor.
data StringDefaultValues = StringDefaultValues'
  { -- | The dynamic value of the @StringDefaultValues@. Different defaults
    -- displayed according to users, groups, and values mapping.
    dynamicValue :: Prelude.Maybe DynamicDefaultValue,
    -- | The static values of the @DecimalDefaultValues@.
    staticValues :: Prelude.Maybe [Data.Sensitive Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StringDefaultValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dynamicValue', 'stringDefaultValues_dynamicValue' - The dynamic value of the @StringDefaultValues@. Different defaults
-- displayed according to users, groups, and values mapping.
--
-- 'staticValues', 'stringDefaultValues_staticValues' - The static values of the @DecimalDefaultValues@.
newStringDefaultValues ::
  StringDefaultValues
newStringDefaultValues =
  StringDefaultValues'
    { dynamicValue =
        Prelude.Nothing,
      staticValues = Prelude.Nothing
    }

-- | The dynamic value of the @StringDefaultValues@. Different defaults
-- displayed according to users, groups, and values mapping.
stringDefaultValues_dynamicValue :: Lens.Lens' StringDefaultValues (Prelude.Maybe DynamicDefaultValue)
stringDefaultValues_dynamicValue = Lens.lens (\StringDefaultValues' {dynamicValue} -> dynamicValue) (\s@StringDefaultValues' {} a -> s {dynamicValue = a} :: StringDefaultValues)

-- | The static values of the @DecimalDefaultValues@.
stringDefaultValues_staticValues :: Lens.Lens' StringDefaultValues (Prelude.Maybe [Prelude.Text])
stringDefaultValues_staticValues = Lens.lens (\StringDefaultValues' {staticValues} -> staticValues) (\s@StringDefaultValues' {} a -> s {staticValues = a} :: StringDefaultValues) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON StringDefaultValues where
  parseJSON =
    Data.withObject
      "StringDefaultValues"
      ( \x ->
          StringDefaultValues'
            Prelude.<$> (x Data..:? "DynamicValue")
            Prelude.<*> (x Data..:? "StaticValues" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable StringDefaultValues where
  hashWithSalt _salt StringDefaultValues' {..} =
    _salt `Prelude.hashWithSalt` dynamicValue
      `Prelude.hashWithSalt` staticValues

instance Prelude.NFData StringDefaultValues where
  rnf StringDefaultValues' {..} =
    Prelude.rnf dynamicValue
      `Prelude.seq` Prelude.rnf staticValues

instance Data.ToJSON StringDefaultValues where
  toJSON StringDefaultValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DynamicValue" Data..=) Prelude.<$> dynamicValue,
            ("StaticValues" Data..=) Prelude.<$> staticValues
          ]
      )
