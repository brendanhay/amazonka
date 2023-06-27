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
-- Module      : Amazonka.QuickSight.Types.DecimalDatasetParameterDefaultValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DecimalDatasetParameterDefaultValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The default values of a decimal parameter.
--
-- /See:/ 'newDecimalDatasetParameterDefaultValues' smart constructor.
data DecimalDatasetParameterDefaultValues = DecimalDatasetParameterDefaultValues'
  { -- | A list of static default values for a given decimal parameter.
    staticValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Double)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecimalDatasetParameterDefaultValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'staticValues', 'decimalDatasetParameterDefaultValues_staticValues' - A list of static default values for a given decimal parameter.
newDecimalDatasetParameterDefaultValues ::
  DecimalDatasetParameterDefaultValues
newDecimalDatasetParameterDefaultValues =
  DecimalDatasetParameterDefaultValues'
    { staticValues =
        Prelude.Nothing
    }

-- | A list of static default values for a given decimal parameter.
decimalDatasetParameterDefaultValues_staticValues :: Lens.Lens' DecimalDatasetParameterDefaultValues (Prelude.Maybe (Prelude.NonEmpty Prelude.Double))
decimalDatasetParameterDefaultValues_staticValues = Lens.lens (\DecimalDatasetParameterDefaultValues' {staticValues} -> staticValues) (\s@DecimalDatasetParameterDefaultValues' {} a -> s {staticValues = a} :: DecimalDatasetParameterDefaultValues) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    DecimalDatasetParameterDefaultValues
  where
  parseJSON =
    Data.withObject
      "DecimalDatasetParameterDefaultValues"
      ( \x ->
          DecimalDatasetParameterDefaultValues'
            Prelude.<$> (x Data..:? "StaticValues")
      )

instance
  Prelude.Hashable
    DecimalDatasetParameterDefaultValues
  where
  hashWithSalt
    _salt
    DecimalDatasetParameterDefaultValues' {..} =
      _salt `Prelude.hashWithSalt` staticValues

instance
  Prelude.NFData
    DecimalDatasetParameterDefaultValues
  where
  rnf DecimalDatasetParameterDefaultValues' {..} =
    Prelude.rnf staticValues

instance
  Data.ToJSON
    DecimalDatasetParameterDefaultValues
  where
  toJSON DecimalDatasetParameterDefaultValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [("StaticValues" Data..=) Prelude.<$> staticValues]
      )
