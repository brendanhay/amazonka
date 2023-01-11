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
-- Module      : Amazonka.SavingsPlans.Types.SavingsPlanOfferingRateFilterElement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlanOfferingRateFilterElement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SavingsPlans.Types.SavingsPlanRateFilterAttribute

-- | Information about a filter.
--
-- /See:/ 'newSavingsPlanOfferingRateFilterElement' smart constructor.
data SavingsPlanOfferingRateFilterElement = SavingsPlanOfferingRateFilterElement'
  { -- | The filter name.
    name :: Prelude.Maybe SavingsPlanRateFilterAttribute,
    -- | The filter values.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlanOfferingRateFilterElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'savingsPlanOfferingRateFilterElement_name' - The filter name.
--
-- 'values', 'savingsPlanOfferingRateFilterElement_values' - The filter values.
newSavingsPlanOfferingRateFilterElement ::
  SavingsPlanOfferingRateFilterElement
newSavingsPlanOfferingRateFilterElement =
  SavingsPlanOfferingRateFilterElement'
    { name =
        Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The filter name.
savingsPlanOfferingRateFilterElement_name :: Lens.Lens' SavingsPlanOfferingRateFilterElement (Prelude.Maybe SavingsPlanRateFilterAttribute)
savingsPlanOfferingRateFilterElement_name = Lens.lens (\SavingsPlanOfferingRateFilterElement' {name} -> name) (\s@SavingsPlanOfferingRateFilterElement' {} a -> s {name = a} :: SavingsPlanOfferingRateFilterElement)

-- | The filter values.
savingsPlanOfferingRateFilterElement_values :: Lens.Lens' SavingsPlanOfferingRateFilterElement (Prelude.Maybe [Prelude.Text])
savingsPlanOfferingRateFilterElement_values = Lens.lens (\SavingsPlanOfferingRateFilterElement' {values} -> values) (\s@SavingsPlanOfferingRateFilterElement' {} a -> s {values = a} :: SavingsPlanOfferingRateFilterElement) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    SavingsPlanOfferingRateFilterElement
  where
  hashWithSalt
    _salt
    SavingsPlanOfferingRateFilterElement' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    SavingsPlanOfferingRateFilterElement
  where
  rnf SavingsPlanOfferingRateFilterElement' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance
  Data.ToJSON
    SavingsPlanOfferingRateFilterElement
  where
  toJSON SavingsPlanOfferingRateFilterElement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("values" Data..=) Prelude.<$> values
          ]
      )
