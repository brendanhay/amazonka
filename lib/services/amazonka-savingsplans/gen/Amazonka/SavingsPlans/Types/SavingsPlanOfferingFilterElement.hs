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
-- Module      : Amazonka.SavingsPlans.Types.SavingsPlanOfferingFilterElement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlanOfferingFilterElement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SavingsPlans.Types.SavingsPlanOfferingFilterAttribute

-- | Information about a filter.
--
-- /See:/ 'newSavingsPlanOfferingFilterElement' smart constructor.
data SavingsPlanOfferingFilterElement = SavingsPlanOfferingFilterElement'
  { -- | The filter name.
    name :: Prelude.Maybe SavingsPlanOfferingFilterAttribute,
    -- | The filter values.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlanOfferingFilterElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'savingsPlanOfferingFilterElement_name' - The filter name.
--
-- 'values', 'savingsPlanOfferingFilterElement_values' - The filter values.
newSavingsPlanOfferingFilterElement ::
  SavingsPlanOfferingFilterElement
newSavingsPlanOfferingFilterElement =
  SavingsPlanOfferingFilterElement'
    { name =
        Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The filter name.
savingsPlanOfferingFilterElement_name :: Lens.Lens' SavingsPlanOfferingFilterElement (Prelude.Maybe SavingsPlanOfferingFilterAttribute)
savingsPlanOfferingFilterElement_name = Lens.lens (\SavingsPlanOfferingFilterElement' {name} -> name) (\s@SavingsPlanOfferingFilterElement' {} a -> s {name = a} :: SavingsPlanOfferingFilterElement)

-- | The filter values.
savingsPlanOfferingFilterElement_values :: Lens.Lens' SavingsPlanOfferingFilterElement (Prelude.Maybe [Prelude.Text])
savingsPlanOfferingFilterElement_values = Lens.lens (\SavingsPlanOfferingFilterElement' {values} -> values) (\s@SavingsPlanOfferingFilterElement' {} a -> s {values = a} :: SavingsPlanOfferingFilterElement) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    SavingsPlanOfferingFilterElement
  where
  hashWithSalt
    _salt
    SavingsPlanOfferingFilterElement' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    SavingsPlanOfferingFilterElement
  where
  rnf SavingsPlanOfferingFilterElement' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Core.ToJSON SavingsPlanOfferingFilterElement where
  toJSON SavingsPlanOfferingFilterElement' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("values" Core..=) Prelude.<$> values
          ]
      )
