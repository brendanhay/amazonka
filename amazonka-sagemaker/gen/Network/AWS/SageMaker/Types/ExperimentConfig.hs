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
-- Module      : Network.AWS.SageMaker.Types.ExperimentConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ExperimentConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Associates a SageMaker job as a trial component with an experiment and
-- trial. Specified when you call the following APIs:
--
-- -   CreateProcessingJob
--
-- -   CreateTrainingJob
--
-- -   CreateTransformJob
--
-- /See:/ 'newExperimentConfig' smart constructor.
data ExperimentConfig = ExperimentConfig'
  { -- | The name of an existing experiment to associate the trial component
    -- with.
    experimentName :: Core.Maybe Core.Text,
    -- | The display name for the trial component. If this key isn\'t specified,
    -- the display name is the trial component name.
    trialComponentDisplayName :: Core.Maybe Core.Text,
    -- | The name of an existing trial to associate the trial component with. If
    -- not specified, a new trial is created.
    trialName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExperimentConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experimentName', 'experimentConfig_experimentName' - The name of an existing experiment to associate the trial component
-- with.
--
-- 'trialComponentDisplayName', 'experimentConfig_trialComponentDisplayName' - The display name for the trial component. If this key isn\'t specified,
-- the display name is the trial component name.
--
-- 'trialName', 'experimentConfig_trialName' - The name of an existing trial to associate the trial component with. If
-- not specified, a new trial is created.
newExperimentConfig ::
  ExperimentConfig
newExperimentConfig =
  ExperimentConfig'
    { experimentName = Core.Nothing,
      trialComponentDisplayName = Core.Nothing,
      trialName = Core.Nothing
    }

-- | The name of an existing experiment to associate the trial component
-- with.
experimentConfig_experimentName :: Lens.Lens' ExperimentConfig (Core.Maybe Core.Text)
experimentConfig_experimentName = Lens.lens (\ExperimentConfig' {experimentName} -> experimentName) (\s@ExperimentConfig' {} a -> s {experimentName = a} :: ExperimentConfig)

-- | The display name for the trial component. If this key isn\'t specified,
-- the display name is the trial component name.
experimentConfig_trialComponentDisplayName :: Lens.Lens' ExperimentConfig (Core.Maybe Core.Text)
experimentConfig_trialComponentDisplayName = Lens.lens (\ExperimentConfig' {trialComponentDisplayName} -> trialComponentDisplayName) (\s@ExperimentConfig' {} a -> s {trialComponentDisplayName = a} :: ExperimentConfig)

-- | The name of an existing trial to associate the trial component with. If
-- not specified, a new trial is created.
experimentConfig_trialName :: Lens.Lens' ExperimentConfig (Core.Maybe Core.Text)
experimentConfig_trialName = Lens.lens (\ExperimentConfig' {trialName} -> trialName) (\s@ExperimentConfig' {} a -> s {trialName = a} :: ExperimentConfig)

instance Core.FromJSON ExperimentConfig where
  parseJSON =
    Core.withObject
      "ExperimentConfig"
      ( \x ->
          ExperimentConfig'
            Core.<$> (x Core..:? "ExperimentName")
            Core.<*> (x Core..:? "TrialComponentDisplayName")
            Core.<*> (x Core..:? "TrialName")
      )

instance Core.Hashable ExperimentConfig

instance Core.NFData ExperimentConfig

instance Core.ToJSON ExperimentConfig where
  toJSON ExperimentConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExperimentName" Core..=) Core.<$> experimentName,
            ("TrialComponentDisplayName" Core..=)
              Core.<$> trialComponentDisplayName,
            ("TrialName" Core..=) Core.<$> trialName
          ]
      )
