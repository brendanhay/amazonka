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
-- Module      : Network.AWS.SavingsPlans.Types.SavingsPlanFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SavingsPlans.Types.SavingsPlanFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SavingsPlans.Types.SavingsPlansFilterName

-- | Information about a filter.
--
-- /See:/ 'newSavingsPlanFilter' smart constructor.
data SavingsPlanFilter = SavingsPlanFilter'
  { -- | The filter value.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The filter name.
    name :: Prelude.Maybe SavingsPlansFilterName
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
-- 'values', 'savingsPlanFilter_values' - The filter value.
--
-- 'name', 'savingsPlanFilter_name' - The filter name.
newSavingsPlanFilter ::
  SavingsPlanFilter
newSavingsPlanFilter =
  SavingsPlanFilter'
    { values = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The filter value.
savingsPlanFilter_values :: Lens.Lens' SavingsPlanFilter (Prelude.Maybe [Prelude.Text])
savingsPlanFilter_values = Lens.lens (\SavingsPlanFilter' {values} -> values) (\s@SavingsPlanFilter' {} a -> s {values = a} :: SavingsPlanFilter) Prelude.. Lens.mapping Lens.coerced

-- | The filter name.
savingsPlanFilter_name :: Lens.Lens' SavingsPlanFilter (Prelude.Maybe SavingsPlansFilterName)
savingsPlanFilter_name = Lens.lens (\SavingsPlanFilter' {name} -> name) (\s@SavingsPlanFilter' {} a -> s {name = a} :: SavingsPlanFilter)

instance Prelude.Hashable SavingsPlanFilter

instance Prelude.NFData SavingsPlanFilter

instance Core.ToJSON SavingsPlanFilter where
  toJSON SavingsPlanFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("values" Core..=) Prelude.<$> values,
            ("name" Core..=) Prelude.<$> name
          ]
      )
