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
-- Module      : Amazonka.SSM.Types.ParametersFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ParametersFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.ParametersFilterKey

-- | This data type is deprecated. Instead, use ParameterStringFilter.
--
-- /See:/ 'newParametersFilter' smart constructor.
data ParametersFilter = ParametersFilter'
  { -- | The name of the filter.
    key :: ParametersFilterKey,
    -- | The filter values.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParametersFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'parametersFilter_key' - The name of the filter.
--
-- 'values', 'parametersFilter_values' - The filter values.
newParametersFilter ::
  -- | 'key'
  ParametersFilterKey ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  ParametersFilter
newParametersFilter pKey_ pValues_ =
  ParametersFilter'
    { key = pKey_,
      values = Lens.coerced Lens.# pValues_
    }

-- | The name of the filter.
parametersFilter_key :: Lens.Lens' ParametersFilter ParametersFilterKey
parametersFilter_key = Lens.lens (\ParametersFilter' {key} -> key) (\s@ParametersFilter' {} a -> s {key = a} :: ParametersFilter)

-- | The filter values.
parametersFilter_values :: Lens.Lens' ParametersFilter (Prelude.NonEmpty Prelude.Text)
parametersFilter_values = Lens.lens (\ParametersFilter' {values} -> values) (\s@ParametersFilter' {} a -> s {values = a} :: ParametersFilter) Prelude.. Lens.coerced

instance Prelude.Hashable ParametersFilter where
  hashWithSalt _salt ParametersFilter' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData ParametersFilter where
  rnf ParametersFilter' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf values

instance Data.ToJSON ParametersFilter where
  toJSON ParametersFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Data..= key),
            Prelude.Just ("Values" Data..= values)
          ]
      )
