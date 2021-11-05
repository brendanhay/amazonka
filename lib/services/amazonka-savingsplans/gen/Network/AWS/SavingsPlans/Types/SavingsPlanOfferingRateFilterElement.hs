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
-- Module      : Network.AWS.SavingsPlans.Types.SavingsPlanOfferingRateFilterElement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SavingsPlans.Types.SavingsPlanOfferingRateFilterElement where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SavingsPlans.Types.SavingsPlanRateFilterAttribute

-- | Information about a filter.
--
-- /See:/ 'newSavingsPlanOfferingRateFilterElement' smart constructor.
data SavingsPlanOfferingRateFilterElement = SavingsPlanOfferingRateFilterElement'
  { -- | The filter values.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The filter name.
    name :: Prelude.Maybe SavingsPlanRateFilterAttribute
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
-- 'values', 'savingsPlanOfferingRateFilterElement_values' - The filter values.
--
-- 'name', 'savingsPlanOfferingRateFilterElement_name' - The filter name.
newSavingsPlanOfferingRateFilterElement ::
  SavingsPlanOfferingRateFilterElement
newSavingsPlanOfferingRateFilterElement =
  SavingsPlanOfferingRateFilterElement'
    { values =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The filter values.
savingsPlanOfferingRateFilterElement_values :: Lens.Lens' SavingsPlanOfferingRateFilterElement (Prelude.Maybe [Prelude.Text])
savingsPlanOfferingRateFilterElement_values = Lens.lens (\SavingsPlanOfferingRateFilterElement' {values} -> values) (\s@SavingsPlanOfferingRateFilterElement' {} a -> s {values = a} :: SavingsPlanOfferingRateFilterElement) Prelude.. Lens.mapping Lens.coerced

-- | The filter name.
savingsPlanOfferingRateFilterElement_name :: Lens.Lens' SavingsPlanOfferingRateFilterElement (Prelude.Maybe SavingsPlanRateFilterAttribute)
savingsPlanOfferingRateFilterElement_name = Lens.lens (\SavingsPlanOfferingRateFilterElement' {name} -> name) (\s@SavingsPlanOfferingRateFilterElement' {} a -> s {name = a} :: SavingsPlanOfferingRateFilterElement)

instance
  Prelude.Hashable
    SavingsPlanOfferingRateFilterElement

instance
  Prelude.NFData
    SavingsPlanOfferingRateFilterElement

instance
  Core.ToJSON
    SavingsPlanOfferingRateFilterElement
  where
  toJSON SavingsPlanOfferingRateFilterElement' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("values" Core..=) Prelude.<$> values,
            ("name" Core..=) Prelude.<$> name
          ]
      )
