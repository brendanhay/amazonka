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
-- Module      : Amazonka.GuardDuty.Types.FreeTrialFeatureConfigurationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.FreeTrialFeatureConfigurationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.FreeTrialFeatureResult
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the free trial period for a feature.
--
-- /See:/ 'newFreeTrialFeatureConfigurationResult' smart constructor.
data FreeTrialFeatureConfigurationResult = FreeTrialFeatureConfigurationResult'
  { -- | The number of the remaining free trial days for the feature.
    freeTrialDaysRemaining :: Prelude.Maybe Prelude.Int,
    -- | The name of the feature for which the free trial is configured.
    name :: Prelude.Maybe FreeTrialFeatureResult
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FreeTrialFeatureConfigurationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'freeTrialDaysRemaining', 'freeTrialFeatureConfigurationResult_freeTrialDaysRemaining' - The number of the remaining free trial days for the feature.
--
-- 'name', 'freeTrialFeatureConfigurationResult_name' - The name of the feature for which the free trial is configured.
newFreeTrialFeatureConfigurationResult ::
  FreeTrialFeatureConfigurationResult
newFreeTrialFeatureConfigurationResult =
  FreeTrialFeatureConfigurationResult'
    { freeTrialDaysRemaining =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The number of the remaining free trial days for the feature.
freeTrialFeatureConfigurationResult_freeTrialDaysRemaining :: Lens.Lens' FreeTrialFeatureConfigurationResult (Prelude.Maybe Prelude.Int)
freeTrialFeatureConfigurationResult_freeTrialDaysRemaining = Lens.lens (\FreeTrialFeatureConfigurationResult' {freeTrialDaysRemaining} -> freeTrialDaysRemaining) (\s@FreeTrialFeatureConfigurationResult' {} a -> s {freeTrialDaysRemaining = a} :: FreeTrialFeatureConfigurationResult)

-- | The name of the feature for which the free trial is configured.
freeTrialFeatureConfigurationResult_name :: Lens.Lens' FreeTrialFeatureConfigurationResult (Prelude.Maybe FreeTrialFeatureResult)
freeTrialFeatureConfigurationResult_name = Lens.lens (\FreeTrialFeatureConfigurationResult' {name} -> name) (\s@FreeTrialFeatureConfigurationResult' {} a -> s {name = a} :: FreeTrialFeatureConfigurationResult)

instance
  Data.FromJSON
    FreeTrialFeatureConfigurationResult
  where
  parseJSON =
    Data.withObject
      "FreeTrialFeatureConfigurationResult"
      ( \x ->
          FreeTrialFeatureConfigurationResult'
            Prelude.<$> (x Data..:? "freeTrialDaysRemaining")
            Prelude.<*> (x Data..:? "name")
      )

instance
  Prelude.Hashable
    FreeTrialFeatureConfigurationResult
  where
  hashWithSalt
    _salt
    FreeTrialFeatureConfigurationResult' {..} =
      _salt
        `Prelude.hashWithSalt` freeTrialDaysRemaining
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    FreeTrialFeatureConfigurationResult
  where
  rnf FreeTrialFeatureConfigurationResult' {..} =
    Prelude.rnf freeTrialDaysRemaining
      `Prelude.seq` Prelude.rnf name
