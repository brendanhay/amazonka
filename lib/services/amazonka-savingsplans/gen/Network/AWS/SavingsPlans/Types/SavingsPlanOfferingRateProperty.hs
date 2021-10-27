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
-- Module      : Network.AWS.SavingsPlans.Types.SavingsPlanOfferingRateProperty
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SavingsPlans.Types.SavingsPlanOfferingRateProperty where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a property.
--
-- /See:/ 'newSavingsPlanOfferingRateProperty' smart constructor.
data SavingsPlanOfferingRateProperty = SavingsPlanOfferingRateProperty'
  { -- | The property value.
    value :: Prelude.Maybe Prelude.Text,
    -- | The property name.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlanOfferingRateProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'savingsPlanOfferingRateProperty_value' - The property value.
--
-- 'name', 'savingsPlanOfferingRateProperty_name' - The property name.
newSavingsPlanOfferingRateProperty ::
  SavingsPlanOfferingRateProperty
newSavingsPlanOfferingRateProperty =
  SavingsPlanOfferingRateProperty'
    { value =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The property value.
savingsPlanOfferingRateProperty_value :: Lens.Lens' SavingsPlanOfferingRateProperty (Prelude.Maybe Prelude.Text)
savingsPlanOfferingRateProperty_value = Lens.lens (\SavingsPlanOfferingRateProperty' {value} -> value) (\s@SavingsPlanOfferingRateProperty' {} a -> s {value = a} :: SavingsPlanOfferingRateProperty)

-- | The property name.
savingsPlanOfferingRateProperty_name :: Lens.Lens' SavingsPlanOfferingRateProperty (Prelude.Maybe Prelude.Text)
savingsPlanOfferingRateProperty_name = Lens.lens (\SavingsPlanOfferingRateProperty' {name} -> name) (\s@SavingsPlanOfferingRateProperty' {} a -> s {name = a} :: SavingsPlanOfferingRateProperty)

instance
  Core.FromJSON
    SavingsPlanOfferingRateProperty
  where
  parseJSON =
    Core.withObject
      "SavingsPlanOfferingRateProperty"
      ( \x ->
          SavingsPlanOfferingRateProperty'
            Prelude.<$> (x Core..:? "value") Prelude.<*> (x Core..:? "name")
      )

instance
  Prelude.Hashable
    SavingsPlanOfferingRateProperty

instance
  Prelude.NFData
    SavingsPlanOfferingRateProperty
