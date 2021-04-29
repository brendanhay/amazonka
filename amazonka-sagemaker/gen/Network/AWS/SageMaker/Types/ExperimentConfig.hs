{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    experimentName :: Prelude.Maybe Prelude.Text,
    -- | The display name for the trial component. If this key isn\'t specified,
    -- the display name is the trial component name.
    trialComponentDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The name of an existing trial to associate the trial component with. If
    -- not specified, a new trial is created.
    trialName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { experimentName = Prelude.Nothing,
      trialComponentDisplayName = Prelude.Nothing,
      trialName = Prelude.Nothing
    }

-- | The name of an existing experiment to associate the trial component
-- with.
experimentConfig_experimentName :: Lens.Lens' ExperimentConfig (Prelude.Maybe Prelude.Text)
experimentConfig_experimentName = Lens.lens (\ExperimentConfig' {experimentName} -> experimentName) (\s@ExperimentConfig' {} a -> s {experimentName = a} :: ExperimentConfig)

-- | The display name for the trial component. If this key isn\'t specified,
-- the display name is the trial component name.
experimentConfig_trialComponentDisplayName :: Lens.Lens' ExperimentConfig (Prelude.Maybe Prelude.Text)
experimentConfig_trialComponentDisplayName = Lens.lens (\ExperimentConfig' {trialComponentDisplayName} -> trialComponentDisplayName) (\s@ExperimentConfig' {} a -> s {trialComponentDisplayName = a} :: ExperimentConfig)

-- | The name of an existing trial to associate the trial component with. If
-- not specified, a new trial is created.
experimentConfig_trialName :: Lens.Lens' ExperimentConfig (Prelude.Maybe Prelude.Text)
experimentConfig_trialName = Lens.lens (\ExperimentConfig' {trialName} -> trialName) (\s@ExperimentConfig' {} a -> s {trialName = a} :: ExperimentConfig)

instance Prelude.FromJSON ExperimentConfig where
  parseJSON =
    Prelude.withObject
      "ExperimentConfig"
      ( \x ->
          ExperimentConfig'
            Prelude.<$> (x Prelude..:? "ExperimentName")
            Prelude.<*> (x Prelude..:? "TrialComponentDisplayName")
            Prelude.<*> (x Prelude..:? "TrialName")
      )

instance Prelude.Hashable ExperimentConfig

instance Prelude.NFData ExperimentConfig

instance Prelude.ToJSON ExperimentConfig where
  toJSON ExperimentConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ExperimentName" Prelude..=)
              Prelude.<$> experimentName,
            ("TrialComponentDisplayName" Prelude..=)
              Prelude.<$> trialComponentDisplayName,
            ("TrialName" Prelude..=) Prelude.<$> trialName
          ]
      )
