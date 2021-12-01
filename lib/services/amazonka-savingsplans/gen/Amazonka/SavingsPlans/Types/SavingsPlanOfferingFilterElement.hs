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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlanOfferingFilterElement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SavingsPlans.Types.SavingsPlanOfferingFilterAttribute

-- | Information about a filter.
--
-- /See:/ 'newSavingsPlanOfferingFilterElement' smart constructor.
data SavingsPlanOfferingFilterElement = SavingsPlanOfferingFilterElement'
  { -- | The filter values.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The filter name.
    name :: Prelude.Maybe SavingsPlanOfferingFilterAttribute
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
-- 'values', 'savingsPlanOfferingFilterElement_values' - The filter values.
--
-- 'name', 'savingsPlanOfferingFilterElement_name' - The filter name.
newSavingsPlanOfferingFilterElement ::
  SavingsPlanOfferingFilterElement
newSavingsPlanOfferingFilterElement =
  SavingsPlanOfferingFilterElement'
    { values =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The filter values.
savingsPlanOfferingFilterElement_values :: Lens.Lens' SavingsPlanOfferingFilterElement (Prelude.Maybe [Prelude.Text])
savingsPlanOfferingFilterElement_values = Lens.lens (\SavingsPlanOfferingFilterElement' {values} -> values) (\s@SavingsPlanOfferingFilterElement' {} a -> s {values = a} :: SavingsPlanOfferingFilterElement) Prelude.. Lens.mapping Lens.coerced

-- | The filter name.
savingsPlanOfferingFilterElement_name :: Lens.Lens' SavingsPlanOfferingFilterElement (Prelude.Maybe SavingsPlanOfferingFilterAttribute)
savingsPlanOfferingFilterElement_name = Lens.lens (\SavingsPlanOfferingFilterElement' {name} -> name) (\s@SavingsPlanOfferingFilterElement' {} a -> s {name = a} :: SavingsPlanOfferingFilterElement)

instance
  Prelude.Hashable
    SavingsPlanOfferingFilterElement
  where
  hashWithSalt
    salt'
    SavingsPlanOfferingFilterElement' {..} =
      salt' `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    SavingsPlanOfferingFilterElement
  where
  rnf SavingsPlanOfferingFilterElement' {..} =
    Prelude.rnf values `Prelude.seq` Prelude.rnf name

instance Core.ToJSON SavingsPlanOfferingFilterElement where
  toJSON SavingsPlanOfferingFilterElement' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("values" Core..=) Prelude.<$> values,
            ("name" Core..=) Prelude.<$> name
          ]
      )
