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
-- Module      : Amazonka.SageMaker.Types.HyperParameterTuningJobStrategyConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperParameterTuningJobStrategyConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.HyperbandStrategyConfig

-- | The configuration for a training job launched by a hyperparameter tuning
-- job. Choose @Bayesian@ for Bayesian optimization, and @Random@ for
-- random search optimization. For more advanced use cases, use
-- @Hyperband@, which evaluates objective metrics for training jobs after
-- every epoch. For more information about strategies, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works>.
--
-- /See:/ 'newHyperParameterTuningJobStrategyConfig' smart constructor.
data HyperParameterTuningJobStrategyConfig = HyperParameterTuningJobStrategyConfig'
  { -- | The configuration for the object that specifies the @Hyperband@
    -- strategy. This parameter is only supported for the @Hyperband@ selection
    -- for @Strategy@ within the @HyperParameterTuningJobConfig@ API.
    hyperbandStrategyConfig :: Prelude.Maybe HyperbandStrategyConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HyperParameterTuningJobStrategyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hyperbandStrategyConfig', 'hyperParameterTuningJobStrategyConfig_hyperbandStrategyConfig' - The configuration for the object that specifies the @Hyperband@
-- strategy. This parameter is only supported for the @Hyperband@ selection
-- for @Strategy@ within the @HyperParameterTuningJobConfig@ API.
newHyperParameterTuningJobStrategyConfig ::
  HyperParameterTuningJobStrategyConfig
newHyperParameterTuningJobStrategyConfig =
  HyperParameterTuningJobStrategyConfig'
    { hyperbandStrategyConfig =
        Prelude.Nothing
    }

-- | The configuration for the object that specifies the @Hyperband@
-- strategy. This parameter is only supported for the @Hyperband@ selection
-- for @Strategy@ within the @HyperParameterTuningJobConfig@ API.
hyperParameterTuningJobStrategyConfig_hyperbandStrategyConfig :: Lens.Lens' HyperParameterTuningJobStrategyConfig (Prelude.Maybe HyperbandStrategyConfig)
hyperParameterTuningJobStrategyConfig_hyperbandStrategyConfig = Lens.lens (\HyperParameterTuningJobStrategyConfig' {hyperbandStrategyConfig} -> hyperbandStrategyConfig) (\s@HyperParameterTuningJobStrategyConfig' {} a -> s {hyperbandStrategyConfig = a} :: HyperParameterTuningJobStrategyConfig)

instance
  Core.FromJSON
    HyperParameterTuningJobStrategyConfig
  where
  parseJSON =
    Core.withObject
      "HyperParameterTuningJobStrategyConfig"
      ( \x ->
          HyperParameterTuningJobStrategyConfig'
            Prelude.<$> (x Core..:? "HyperbandStrategyConfig")
      )

instance
  Prelude.Hashable
    HyperParameterTuningJobStrategyConfig
  where
  hashWithSalt
    _salt
    HyperParameterTuningJobStrategyConfig' {..} =
      _salt
        `Prelude.hashWithSalt` hyperbandStrategyConfig

instance
  Prelude.NFData
    HyperParameterTuningJobStrategyConfig
  where
  rnf HyperParameterTuningJobStrategyConfig' {..} =
    Prelude.rnf hyperbandStrategyConfig

instance
  Core.ToJSON
    HyperParameterTuningJobStrategyConfig
  where
  toJSON HyperParameterTuningJobStrategyConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("HyperbandStrategyConfig" Core..=)
              Prelude.<$> hyperbandStrategyConfig
          ]
      )
