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
-- Module      : Amazonka.SavingsPlans.Types.SavingsPlanRateFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlanRateFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SavingsPlans.Types.SavingsPlanRateFilterName

-- | Information about a filter.
--
-- /See:/ 'newSavingsPlanRateFilter' smart constructor.
data SavingsPlanRateFilter = SavingsPlanRateFilter'
  { -- | The filter name.
    name :: Prelude.Maybe SavingsPlanRateFilterName,
    -- | The filter values.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlanRateFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'savingsPlanRateFilter_name' - The filter name.
--
-- 'values', 'savingsPlanRateFilter_values' - The filter values.
newSavingsPlanRateFilter ::
  SavingsPlanRateFilter
newSavingsPlanRateFilter =
  SavingsPlanRateFilter'
    { name = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The filter name.
savingsPlanRateFilter_name :: Lens.Lens' SavingsPlanRateFilter (Prelude.Maybe SavingsPlanRateFilterName)
savingsPlanRateFilter_name = Lens.lens (\SavingsPlanRateFilter' {name} -> name) (\s@SavingsPlanRateFilter' {} a -> s {name = a} :: SavingsPlanRateFilter)

-- | The filter values.
savingsPlanRateFilter_values :: Lens.Lens' SavingsPlanRateFilter (Prelude.Maybe [Prelude.Text])
savingsPlanRateFilter_values = Lens.lens (\SavingsPlanRateFilter' {values} -> values) (\s@SavingsPlanRateFilter' {} a -> s {values = a} :: SavingsPlanRateFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable SavingsPlanRateFilter where
  hashWithSalt _salt SavingsPlanRateFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData SavingsPlanRateFilter where
  rnf SavingsPlanRateFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON SavingsPlanRateFilter where
  toJSON SavingsPlanRateFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("values" Data..=) Prelude.<$> values
          ]
      )
