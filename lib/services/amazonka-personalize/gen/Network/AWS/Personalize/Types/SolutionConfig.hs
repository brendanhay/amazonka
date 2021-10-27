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
-- Module      : Network.AWS.Personalize.Types.SolutionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Personalize.Types.SolutionConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Personalize.Types.AutoMLConfig
import Network.AWS.Personalize.Types.HPOConfig
import Network.AWS.Personalize.Types.OptimizationObjective
import qualified Network.AWS.Prelude as Prelude

-- | Describes the configuration properties for the solution.
--
-- /See:/ 'newSolutionConfig' smart constructor.
data SolutionConfig = SolutionConfig'
  { -- | Lists the feature transformation parameters.
    featureTransformationParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Describes the properties for hyperparameter optimization (HPO).
    hpoConfig :: Prelude.Maybe HPOConfig,
    -- | Only events with a value greater than or equal to this threshold are
    -- used for training a model.
    eventValueThreshold :: Prelude.Maybe Prelude.Text,
    -- | The AutoMLConfig object containing a list of recipes to search when
    -- AutoML is performed.
    autoMLConfig :: Prelude.Maybe AutoMLConfig,
    -- | Lists the hyperparameter names and ranges.
    algorithmHyperParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Describes the additional objective for the solution, such as maximizing
    -- streaming minutes or increasing revenue. For more information see
    -- <https://docs.aws.amazon.com/personalize/latest/dg/optimizing-solution-for-objective.html Optimizing a solution>.
    optimizationObjective :: Prelude.Maybe OptimizationObjective
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SolutionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureTransformationParameters', 'solutionConfig_featureTransformationParameters' - Lists the feature transformation parameters.
--
-- 'hpoConfig', 'solutionConfig_hpoConfig' - Describes the properties for hyperparameter optimization (HPO).
--
-- 'eventValueThreshold', 'solutionConfig_eventValueThreshold' - Only events with a value greater than or equal to this threshold are
-- used for training a model.
--
-- 'autoMLConfig', 'solutionConfig_autoMLConfig' - The AutoMLConfig object containing a list of recipes to search when
-- AutoML is performed.
--
-- 'algorithmHyperParameters', 'solutionConfig_algorithmHyperParameters' - Lists the hyperparameter names and ranges.
--
-- 'optimizationObjective', 'solutionConfig_optimizationObjective' - Describes the additional objective for the solution, such as maximizing
-- streaming minutes or increasing revenue. For more information see
-- <https://docs.aws.amazon.com/personalize/latest/dg/optimizing-solution-for-objective.html Optimizing a solution>.
newSolutionConfig ::
  SolutionConfig
newSolutionConfig =
  SolutionConfig'
    { featureTransformationParameters =
        Prelude.Nothing,
      hpoConfig = Prelude.Nothing,
      eventValueThreshold = Prelude.Nothing,
      autoMLConfig = Prelude.Nothing,
      algorithmHyperParameters = Prelude.Nothing,
      optimizationObjective = Prelude.Nothing
    }

-- | Lists the feature transformation parameters.
solutionConfig_featureTransformationParameters :: Lens.Lens' SolutionConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
solutionConfig_featureTransformationParameters = Lens.lens (\SolutionConfig' {featureTransformationParameters} -> featureTransformationParameters) (\s@SolutionConfig' {} a -> s {featureTransformationParameters = a} :: SolutionConfig) Prelude.. Lens.mapping Lens.coerced

-- | Describes the properties for hyperparameter optimization (HPO).
solutionConfig_hpoConfig :: Lens.Lens' SolutionConfig (Prelude.Maybe HPOConfig)
solutionConfig_hpoConfig = Lens.lens (\SolutionConfig' {hpoConfig} -> hpoConfig) (\s@SolutionConfig' {} a -> s {hpoConfig = a} :: SolutionConfig)

-- | Only events with a value greater than or equal to this threshold are
-- used for training a model.
solutionConfig_eventValueThreshold :: Lens.Lens' SolutionConfig (Prelude.Maybe Prelude.Text)
solutionConfig_eventValueThreshold = Lens.lens (\SolutionConfig' {eventValueThreshold} -> eventValueThreshold) (\s@SolutionConfig' {} a -> s {eventValueThreshold = a} :: SolutionConfig)

-- | The AutoMLConfig object containing a list of recipes to search when
-- AutoML is performed.
solutionConfig_autoMLConfig :: Lens.Lens' SolutionConfig (Prelude.Maybe AutoMLConfig)
solutionConfig_autoMLConfig = Lens.lens (\SolutionConfig' {autoMLConfig} -> autoMLConfig) (\s@SolutionConfig' {} a -> s {autoMLConfig = a} :: SolutionConfig)

-- | Lists the hyperparameter names and ranges.
solutionConfig_algorithmHyperParameters :: Lens.Lens' SolutionConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
solutionConfig_algorithmHyperParameters = Lens.lens (\SolutionConfig' {algorithmHyperParameters} -> algorithmHyperParameters) (\s@SolutionConfig' {} a -> s {algorithmHyperParameters = a} :: SolutionConfig) Prelude.. Lens.mapping Lens.coerced

-- | Describes the additional objective for the solution, such as maximizing
-- streaming minutes or increasing revenue. For more information see
-- <https://docs.aws.amazon.com/personalize/latest/dg/optimizing-solution-for-objective.html Optimizing a solution>.
solutionConfig_optimizationObjective :: Lens.Lens' SolutionConfig (Prelude.Maybe OptimizationObjective)
solutionConfig_optimizationObjective = Lens.lens (\SolutionConfig' {optimizationObjective} -> optimizationObjective) (\s@SolutionConfig' {} a -> s {optimizationObjective = a} :: SolutionConfig)

instance Core.FromJSON SolutionConfig where
  parseJSON =
    Core.withObject
      "SolutionConfig"
      ( \x ->
          SolutionConfig'
            Prelude.<$> ( x Core..:? "featureTransformationParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "hpoConfig")
            Prelude.<*> (x Core..:? "eventValueThreshold")
            Prelude.<*> (x Core..:? "autoMLConfig")
            Prelude.<*> ( x Core..:? "algorithmHyperParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "optimizationObjective")
      )

instance Prelude.Hashable SolutionConfig

instance Prelude.NFData SolutionConfig

instance Core.ToJSON SolutionConfig where
  toJSON SolutionConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("featureTransformationParameters" Core..=)
              Prelude.<$> featureTransformationParameters,
            ("hpoConfig" Core..=) Prelude.<$> hpoConfig,
            ("eventValueThreshold" Core..=)
              Prelude.<$> eventValueThreshold,
            ("autoMLConfig" Core..=) Prelude.<$> autoMLConfig,
            ("algorithmHyperParameters" Core..=)
              Prelude.<$> algorithmHyperParameters,
            ("optimizationObjective" Core..=)
              Prelude.<$> optimizationObjective
          ]
      )
