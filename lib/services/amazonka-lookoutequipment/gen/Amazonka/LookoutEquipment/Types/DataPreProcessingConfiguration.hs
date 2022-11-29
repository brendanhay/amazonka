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
-- Module      : Amazonka.LookoutEquipment.Types.DataPreProcessingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.DataPreProcessingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutEquipment.Types.TargetSamplingRate
import qualified Amazonka.Prelude as Prelude

-- | The configuration is the @TargetSamplingRate@, which is the sampling
-- rate of the data after post processing by Amazon Lookout for Equipment.
-- For example, if you provide data that has been collected at a 1 second
-- level and you want the system to resample the data at a 1 minute rate
-- before training, the @TargetSamplingRate@ is 1 minute.
--
-- When providing a value for the @TargetSamplingRate@, you must attach the
-- prefix \"PT\" to the rate you want. The value for a 1 second rate is
-- therefore /PT1S/, the value for a 15 minute rate is /PT15M/, and the
-- value for a 1 hour rate is /PT1H/
--
-- /See:/ 'newDataPreProcessingConfiguration' smart constructor.
data DataPreProcessingConfiguration = DataPreProcessingConfiguration'
  { -- | The sampling rate of the data after post processing by Amazon Lookout
    -- for Equipment. For example, if you provide data that has been collected
    -- at a 1 second level and you want the system to resample the data at a 1
    -- minute rate before training, the @TargetSamplingRate@ is 1 minute.
    --
    -- When providing a value for the @TargetSamplingRate@, you must attach the
    -- prefix \"PT\" to the rate you want. The value for a 1 second rate is
    -- therefore /PT1S/, the value for a 15 minute rate is /PT15M/, and the
    -- value for a 1 hour rate is /PT1H/
    targetSamplingRate :: Prelude.Maybe TargetSamplingRate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataPreProcessingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetSamplingRate', 'dataPreProcessingConfiguration_targetSamplingRate' - The sampling rate of the data after post processing by Amazon Lookout
-- for Equipment. For example, if you provide data that has been collected
-- at a 1 second level and you want the system to resample the data at a 1
-- minute rate before training, the @TargetSamplingRate@ is 1 minute.
--
-- When providing a value for the @TargetSamplingRate@, you must attach the
-- prefix \"PT\" to the rate you want. The value for a 1 second rate is
-- therefore /PT1S/, the value for a 15 minute rate is /PT15M/, and the
-- value for a 1 hour rate is /PT1H/
newDataPreProcessingConfiguration ::
  DataPreProcessingConfiguration
newDataPreProcessingConfiguration =
  DataPreProcessingConfiguration'
    { targetSamplingRate =
        Prelude.Nothing
    }

-- | The sampling rate of the data after post processing by Amazon Lookout
-- for Equipment. For example, if you provide data that has been collected
-- at a 1 second level and you want the system to resample the data at a 1
-- minute rate before training, the @TargetSamplingRate@ is 1 minute.
--
-- When providing a value for the @TargetSamplingRate@, you must attach the
-- prefix \"PT\" to the rate you want. The value for a 1 second rate is
-- therefore /PT1S/, the value for a 15 minute rate is /PT15M/, and the
-- value for a 1 hour rate is /PT1H/
dataPreProcessingConfiguration_targetSamplingRate :: Lens.Lens' DataPreProcessingConfiguration (Prelude.Maybe TargetSamplingRate)
dataPreProcessingConfiguration_targetSamplingRate = Lens.lens (\DataPreProcessingConfiguration' {targetSamplingRate} -> targetSamplingRate) (\s@DataPreProcessingConfiguration' {} a -> s {targetSamplingRate = a} :: DataPreProcessingConfiguration)

instance Core.FromJSON DataPreProcessingConfiguration where
  parseJSON =
    Core.withObject
      "DataPreProcessingConfiguration"
      ( \x ->
          DataPreProcessingConfiguration'
            Prelude.<$> (x Core..:? "TargetSamplingRate")
      )

instance
  Prelude.Hashable
    DataPreProcessingConfiguration
  where
  hashWithSalt
    _salt
    DataPreProcessingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` targetSamplingRate

instance
  Prelude.NFData
    DataPreProcessingConfiguration
  where
  rnf DataPreProcessingConfiguration' {..} =
    Prelude.rnf targetSamplingRate

instance Core.ToJSON DataPreProcessingConfiguration where
  toJSON DataPreProcessingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TargetSamplingRate" Core..=)
              Prelude.<$> targetSamplingRate
          ]
      )
