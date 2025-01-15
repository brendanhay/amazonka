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
-- Module      : Amazonka.QuickSight.Types.CustomValuesConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CustomValuesConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CustomParameterValues

-- | The configuration of custom values for the destination parameter in
-- @DestinationParameterValueConfiguration@.
--
-- /See:/ 'newCustomValuesConfiguration' smart constructor.
data CustomValuesConfiguration = CustomValuesConfiguration'
  { -- | Includes the null value in custom action parameter values.
    includeNullValue :: Prelude.Maybe Prelude.Bool,
    customValues :: CustomParameterValues
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomValuesConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeNullValue', 'customValuesConfiguration_includeNullValue' - Includes the null value in custom action parameter values.
--
-- 'customValues', 'customValuesConfiguration_customValues' - Undocumented member.
newCustomValuesConfiguration ::
  -- | 'customValues'
  CustomParameterValues ->
  CustomValuesConfiguration
newCustomValuesConfiguration pCustomValues_ =
  CustomValuesConfiguration'
    { includeNullValue =
        Prelude.Nothing,
      customValues = pCustomValues_
    }

-- | Includes the null value in custom action parameter values.
customValuesConfiguration_includeNullValue :: Lens.Lens' CustomValuesConfiguration (Prelude.Maybe Prelude.Bool)
customValuesConfiguration_includeNullValue = Lens.lens (\CustomValuesConfiguration' {includeNullValue} -> includeNullValue) (\s@CustomValuesConfiguration' {} a -> s {includeNullValue = a} :: CustomValuesConfiguration)

-- | Undocumented member.
customValuesConfiguration_customValues :: Lens.Lens' CustomValuesConfiguration CustomParameterValues
customValuesConfiguration_customValues = Lens.lens (\CustomValuesConfiguration' {customValues} -> customValues) (\s@CustomValuesConfiguration' {} a -> s {customValues = a} :: CustomValuesConfiguration)

instance Data.FromJSON CustomValuesConfiguration where
  parseJSON =
    Data.withObject
      "CustomValuesConfiguration"
      ( \x ->
          CustomValuesConfiguration'
            Prelude.<$> (x Data..:? "IncludeNullValue")
            Prelude.<*> (x Data..: "CustomValues")
      )

instance Prelude.Hashable CustomValuesConfiguration where
  hashWithSalt _salt CustomValuesConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` includeNullValue
      `Prelude.hashWithSalt` customValues

instance Prelude.NFData CustomValuesConfiguration where
  rnf CustomValuesConfiguration' {..} =
    Prelude.rnf includeNullValue `Prelude.seq`
      Prelude.rnf customValues

instance Data.ToJSON CustomValuesConfiguration where
  toJSON CustomValuesConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IncludeNullValue" Data..=)
              Prelude.<$> includeNullValue,
            Prelude.Just ("CustomValues" Data..= customValues)
          ]
      )
