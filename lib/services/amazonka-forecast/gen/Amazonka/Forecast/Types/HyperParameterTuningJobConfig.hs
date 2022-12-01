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
-- Module      : Amazonka.Forecast.Types.HyperParameterTuningJobConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.HyperParameterTuningJobConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types.ParameterRanges
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for a hyperparameter tuning job. You specify
-- this object in the CreatePredictor request.
--
-- A /hyperparameter/ is a parameter that governs the model training
-- process. You set hyperparameters before training starts, unlike model
-- parameters, which are determined during training. The values of the
-- hyperparameters effect which values are chosen for the model parameters.
--
-- In a /hyperparameter tuning job/, Amazon Forecast chooses the set of
-- hyperparameter values that optimize a specified metric. Forecast
-- accomplishes this by running many training jobs over a range of
-- hyperparameter values. The optimum set of values depends on the
-- algorithm, the training data, and the specified metric objective.
--
-- /See:/ 'newHyperParameterTuningJobConfig' smart constructor.
data HyperParameterTuningJobConfig = HyperParameterTuningJobConfig'
  { -- | Specifies the ranges of valid values for the hyperparameters.
    parameterRanges :: Prelude.Maybe ParameterRanges
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HyperParameterTuningJobConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterRanges', 'hyperParameterTuningJobConfig_parameterRanges' - Specifies the ranges of valid values for the hyperparameters.
newHyperParameterTuningJobConfig ::
  HyperParameterTuningJobConfig
newHyperParameterTuningJobConfig =
  HyperParameterTuningJobConfig'
    { parameterRanges =
        Prelude.Nothing
    }

-- | Specifies the ranges of valid values for the hyperparameters.
hyperParameterTuningJobConfig_parameterRanges :: Lens.Lens' HyperParameterTuningJobConfig (Prelude.Maybe ParameterRanges)
hyperParameterTuningJobConfig_parameterRanges = Lens.lens (\HyperParameterTuningJobConfig' {parameterRanges} -> parameterRanges) (\s@HyperParameterTuningJobConfig' {} a -> s {parameterRanges = a} :: HyperParameterTuningJobConfig)

instance Core.FromJSON HyperParameterTuningJobConfig where
  parseJSON =
    Core.withObject
      "HyperParameterTuningJobConfig"
      ( \x ->
          HyperParameterTuningJobConfig'
            Prelude.<$> (x Core..:? "ParameterRanges")
      )

instance
  Prelude.Hashable
    HyperParameterTuningJobConfig
  where
  hashWithSalt _salt HyperParameterTuningJobConfig' {..} =
    _salt `Prelude.hashWithSalt` parameterRanges

instance Prelude.NFData HyperParameterTuningJobConfig where
  rnf HyperParameterTuningJobConfig' {..} =
    Prelude.rnf parameterRanges

instance Core.ToJSON HyperParameterTuningJobConfig where
  toJSON HyperParameterTuningJobConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ParameterRanges" Core..=)
              Prelude.<$> parameterRanges
          ]
      )
