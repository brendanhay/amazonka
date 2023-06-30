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
-- Module      : Amazonka.SavingsPlans.Types.SavingsPlanFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlanFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SavingsPlans.Types.SavingsPlansFilterName

-- | Information about a filter.
--
-- /See:/ 'newSavingsPlanFilter' smart constructor.
data SavingsPlanFilter = SavingsPlanFilter'
  { -- | The filter name.
    name :: Prelude.Maybe SavingsPlansFilterName,
    -- | The filter value.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlanFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'savingsPlanFilter_name' - The filter name.
--
-- 'values', 'savingsPlanFilter_values' - The filter value.
newSavingsPlanFilter ::
  SavingsPlanFilter
newSavingsPlanFilter =
  SavingsPlanFilter'
    { name = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The filter name.
savingsPlanFilter_name :: Lens.Lens' SavingsPlanFilter (Prelude.Maybe SavingsPlansFilterName)
savingsPlanFilter_name = Lens.lens (\SavingsPlanFilter' {name} -> name) (\s@SavingsPlanFilter' {} a -> s {name = a} :: SavingsPlanFilter)

-- | The filter value.
savingsPlanFilter_values :: Lens.Lens' SavingsPlanFilter (Prelude.Maybe [Prelude.Text])
savingsPlanFilter_values = Lens.lens (\SavingsPlanFilter' {values} -> values) (\s@SavingsPlanFilter' {} a -> s {values = a} :: SavingsPlanFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable SavingsPlanFilter where
  hashWithSalt _salt SavingsPlanFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData SavingsPlanFilter where
  rnf SavingsPlanFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON SavingsPlanFilter where
  toJSON SavingsPlanFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("values" Data..=) Prelude.<$> values
          ]
      )
