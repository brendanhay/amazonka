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
-- Module      : Amazonka.QuickSight.Types.IntegerDatasetParameterDefaultValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.IntegerDatasetParameterDefaultValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The default values of an integer parameter.
--
-- /See:/ 'newIntegerDatasetParameterDefaultValues' smart constructor.
data IntegerDatasetParameterDefaultValues = IntegerDatasetParameterDefaultValues'
  { -- | A list of static default values for a given integer parameter.
    staticValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Integer)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntegerDatasetParameterDefaultValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'staticValues', 'integerDatasetParameterDefaultValues_staticValues' - A list of static default values for a given integer parameter.
newIntegerDatasetParameterDefaultValues ::
  IntegerDatasetParameterDefaultValues
newIntegerDatasetParameterDefaultValues =
  IntegerDatasetParameterDefaultValues'
    { staticValues =
        Prelude.Nothing
    }

-- | A list of static default values for a given integer parameter.
integerDatasetParameterDefaultValues_staticValues :: Lens.Lens' IntegerDatasetParameterDefaultValues (Prelude.Maybe (Prelude.NonEmpty Prelude.Integer))
integerDatasetParameterDefaultValues_staticValues = Lens.lens (\IntegerDatasetParameterDefaultValues' {staticValues} -> staticValues) (\s@IntegerDatasetParameterDefaultValues' {} a -> s {staticValues = a} :: IntegerDatasetParameterDefaultValues) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    IntegerDatasetParameterDefaultValues
  where
  parseJSON =
    Data.withObject
      "IntegerDatasetParameterDefaultValues"
      ( \x ->
          IntegerDatasetParameterDefaultValues'
            Prelude.<$> (x Data..:? "StaticValues")
      )

instance
  Prelude.Hashable
    IntegerDatasetParameterDefaultValues
  where
  hashWithSalt
    _salt
    IntegerDatasetParameterDefaultValues' {..} =
      _salt `Prelude.hashWithSalt` staticValues

instance
  Prelude.NFData
    IntegerDatasetParameterDefaultValues
  where
  rnf IntegerDatasetParameterDefaultValues' {..} =
    Prelude.rnf staticValues

instance
  Data.ToJSON
    IntegerDatasetParameterDefaultValues
  where
  toJSON IntegerDatasetParameterDefaultValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [("StaticValues" Data..=) Prelude.<$> staticValues]
      )
