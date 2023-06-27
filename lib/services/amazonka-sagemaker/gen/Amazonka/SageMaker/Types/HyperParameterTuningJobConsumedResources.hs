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
-- Module      : Amazonka.SageMaker.Types.HyperParameterTuningJobConsumedResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperParameterTuningJobConsumedResources where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The total resources consumed by your hyperparameter tuning job.
--
-- /See:/ 'newHyperParameterTuningJobConsumedResources' smart constructor.
data HyperParameterTuningJobConsumedResources = HyperParameterTuningJobConsumedResources'
  { -- | The wall clock runtime in seconds used by your hyperparameter tuning
    -- job.
    runtimeInSeconds :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HyperParameterTuningJobConsumedResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runtimeInSeconds', 'hyperParameterTuningJobConsumedResources_runtimeInSeconds' - The wall clock runtime in seconds used by your hyperparameter tuning
-- job.
newHyperParameterTuningJobConsumedResources ::
  HyperParameterTuningJobConsumedResources
newHyperParameterTuningJobConsumedResources =
  HyperParameterTuningJobConsumedResources'
    { runtimeInSeconds =
        Prelude.Nothing
    }

-- | The wall clock runtime in seconds used by your hyperparameter tuning
-- job.
hyperParameterTuningJobConsumedResources_runtimeInSeconds :: Lens.Lens' HyperParameterTuningJobConsumedResources (Prelude.Maybe Prelude.Int)
hyperParameterTuningJobConsumedResources_runtimeInSeconds = Lens.lens (\HyperParameterTuningJobConsumedResources' {runtimeInSeconds} -> runtimeInSeconds) (\s@HyperParameterTuningJobConsumedResources' {} a -> s {runtimeInSeconds = a} :: HyperParameterTuningJobConsumedResources)

instance
  Data.FromJSON
    HyperParameterTuningJobConsumedResources
  where
  parseJSON =
    Data.withObject
      "HyperParameterTuningJobConsumedResources"
      ( \x ->
          HyperParameterTuningJobConsumedResources'
            Prelude.<$> (x Data..:? "RuntimeInSeconds")
      )

instance
  Prelude.Hashable
    HyperParameterTuningJobConsumedResources
  where
  hashWithSalt
    _salt
    HyperParameterTuningJobConsumedResources' {..} =
      _salt `Prelude.hashWithSalt` runtimeInSeconds

instance
  Prelude.NFData
    HyperParameterTuningJobConsumedResources
  where
  rnf HyperParameterTuningJobConsumedResources' {..} =
    Prelude.rnf runtimeInSeconds
