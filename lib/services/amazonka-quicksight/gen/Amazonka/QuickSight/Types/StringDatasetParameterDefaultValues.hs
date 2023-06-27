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
-- Module      : Amazonka.QuickSight.Types.StringDatasetParameterDefaultValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.StringDatasetParameterDefaultValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The default values of a string parameter.
--
-- /See:/ 'newStringDatasetParameterDefaultValues' smart constructor.
data StringDatasetParameterDefaultValues = StringDatasetParameterDefaultValues'
  { -- | A list of static default values for a given string parameter.
    staticValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StringDatasetParameterDefaultValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'staticValues', 'stringDatasetParameterDefaultValues_staticValues' - A list of static default values for a given string parameter.
newStringDatasetParameterDefaultValues ::
  StringDatasetParameterDefaultValues
newStringDatasetParameterDefaultValues =
  StringDatasetParameterDefaultValues'
    { staticValues =
        Prelude.Nothing
    }

-- | A list of static default values for a given string parameter.
stringDatasetParameterDefaultValues_staticValues :: Lens.Lens' StringDatasetParameterDefaultValues (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
stringDatasetParameterDefaultValues_staticValues = Lens.lens (\StringDatasetParameterDefaultValues' {staticValues} -> staticValues) (\s@StringDatasetParameterDefaultValues' {} a -> s {staticValues = a} :: StringDatasetParameterDefaultValues) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    StringDatasetParameterDefaultValues
  where
  parseJSON =
    Data.withObject
      "StringDatasetParameterDefaultValues"
      ( \x ->
          StringDatasetParameterDefaultValues'
            Prelude.<$> (x Data..:? "StaticValues")
      )

instance
  Prelude.Hashable
    StringDatasetParameterDefaultValues
  where
  hashWithSalt
    _salt
    StringDatasetParameterDefaultValues' {..} =
      _salt `Prelude.hashWithSalt` staticValues

instance
  Prelude.NFData
    StringDatasetParameterDefaultValues
  where
  rnf StringDatasetParameterDefaultValues' {..} =
    Prelude.rnf staticValues

instance
  Data.ToJSON
    StringDatasetParameterDefaultValues
  where
  toJSON StringDatasetParameterDefaultValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [("StaticValues" Data..=) Prelude.<$> staticValues]
      )
