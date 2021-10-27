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
-- Module      : Network.AWS.SavingsPlans.Types.SavingsPlanRateProperty
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SavingsPlans.Types.SavingsPlanRateProperty where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SavingsPlans.Types.SavingsPlanRatePropertyKey

-- | Information about a property.
--
-- /See:/ 'newSavingsPlanRateProperty' smart constructor.
data SavingsPlanRateProperty = SavingsPlanRateProperty'
  { -- | The property value.
    value :: Prelude.Maybe Prelude.Text,
    -- | The property name.
    name :: Prelude.Maybe SavingsPlanRatePropertyKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlanRateProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'savingsPlanRateProperty_value' - The property value.
--
-- 'name', 'savingsPlanRateProperty_name' - The property name.
newSavingsPlanRateProperty ::
  SavingsPlanRateProperty
newSavingsPlanRateProperty =
  SavingsPlanRateProperty'
    { value = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The property value.
savingsPlanRateProperty_value :: Lens.Lens' SavingsPlanRateProperty (Prelude.Maybe Prelude.Text)
savingsPlanRateProperty_value = Lens.lens (\SavingsPlanRateProperty' {value} -> value) (\s@SavingsPlanRateProperty' {} a -> s {value = a} :: SavingsPlanRateProperty)

-- | The property name.
savingsPlanRateProperty_name :: Lens.Lens' SavingsPlanRateProperty (Prelude.Maybe SavingsPlanRatePropertyKey)
savingsPlanRateProperty_name = Lens.lens (\SavingsPlanRateProperty' {name} -> name) (\s@SavingsPlanRateProperty' {} a -> s {name = a} :: SavingsPlanRateProperty)

instance Core.FromJSON SavingsPlanRateProperty where
  parseJSON =
    Core.withObject
      "SavingsPlanRateProperty"
      ( \x ->
          SavingsPlanRateProperty'
            Prelude.<$> (x Core..:? "value") Prelude.<*> (x Core..:? "name")
      )

instance Prelude.Hashable SavingsPlanRateProperty

instance Prelude.NFData SavingsPlanRateProperty
