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
-- Module      : Amazonka.SavingsPlans.Types.SavingsPlanOfferingProperty
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlanOfferingProperty where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SavingsPlans.Types.SavingsPlanOfferingPropertyKey

-- | Information about a property.
--
-- /See:/ 'newSavingsPlanOfferingProperty' smart constructor.
data SavingsPlanOfferingProperty = SavingsPlanOfferingProperty'
  { -- | The property name.
    name :: Prelude.Maybe SavingsPlanOfferingPropertyKey,
    -- | The property value.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlanOfferingProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'savingsPlanOfferingProperty_name' - The property name.
--
-- 'value', 'savingsPlanOfferingProperty_value' - The property value.
newSavingsPlanOfferingProperty ::
  SavingsPlanOfferingProperty
newSavingsPlanOfferingProperty =
  SavingsPlanOfferingProperty'
    { name =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The property name.
savingsPlanOfferingProperty_name :: Lens.Lens' SavingsPlanOfferingProperty (Prelude.Maybe SavingsPlanOfferingPropertyKey)
savingsPlanOfferingProperty_name = Lens.lens (\SavingsPlanOfferingProperty' {name} -> name) (\s@SavingsPlanOfferingProperty' {} a -> s {name = a} :: SavingsPlanOfferingProperty)

-- | The property value.
savingsPlanOfferingProperty_value :: Lens.Lens' SavingsPlanOfferingProperty (Prelude.Maybe Prelude.Text)
savingsPlanOfferingProperty_value = Lens.lens (\SavingsPlanOfferingProperty' {value} -> value) (\s@SavingsPlanOfferingProperty' {} a -> s {value = a} :: SavingsPlanOfferingProperty)

instance Data.FromJSON SavingsPlanOfferingProperty where
  parseJSON =
    Data.withObject
      "SavingsPlanOfferingProperty"
      ( \x ->
          SavingsPlanOfferingProperty'
            Prelude.<$> (x Data..:? "name") Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable SavingsPlanOfferingProperty where
  hashWithSalt _salt SavingsPlanOfferingProperty' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData SavingsPlanOfferingProperty where
  rnf SavingsPlanOfferingProperty' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value
