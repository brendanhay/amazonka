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
-- Module      : Amazonka.SavingsPlans.Types.SavingsPlanOfferingRateProperty
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlanOfferingRateProperty where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a property.
--
-- /See:/ 'newSavingsPlanOfferingRateProperty' smart constructor.
data SavingsPlanOfferingRateProperty = SavingsPlanOfferingRateProperty'
  { -- | The property name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The property value.
    value :: Prelude.Maybe Prelude.Text
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
-- 'name', 'savingsPlanOfferingRateProperty_name' - The property name.
--
-- 'value', 'savingsPlanOfferingRateProperty_value' - The property value.
newSavingsPlanOfferingRateProperty ::
  SavingsPlanOfferingRateProperty
newSavingsPlanOfferingRateProperty =
  SavingsPlanOfferingRateProperty'
    { name =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The property name.
savingsPlanOfferingRateProperty_name :: Lens.Lens' SavingsPlanOfferingRateProperty (Prelude.Maybe Prelude.Text)
savingsPlanOfferingRateProperty_name = Lens.lens (\SavingsPlanOfferingRateProperty' {name} -> name) (\s@SavingsPlanOfferingRateProperty' {} a -> s {name = a} :: SavingsPlanOfferingRateProperty)

-- | The property value.
savingsPlanOfferingRateProperty_value :: Lens.Lens' SavingsPlanOfferingRateProperty (Prelude.Maybe Prelude.Text)
savingsPlanOfferingRateProperty_value = Lens.lens (\SavingsPlanOfferingRateProperty' {value} -> value) (\s@SavingsPlanOfferingRateProperty' {} a -> s {value = a} :: SavingsPlanOfferingRateProperty)

instance
  Data.FromJSON
    SavingsPlanOfferingRateProperty
  where
  parseJSON =
    Data.withObject
      "SavingsPlanOfferingRateProperty"
      ( \x ->
          SavingsPlanOfferingRateProperty'
            Prelude.<$> (x Data..:? "name") Prelude.<*> (x Data..:? "value")
      )

instance
  Prelude.Hashable
    SavingsPlanOfferingRateProperty
  where
  hashWithSalt
    _salt
    SavingsPlanOfferingRateProperty' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    SavingsPlanOfferingRateProperty
  where
  rnf SavingsPlanOfferingRateProperty' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value
