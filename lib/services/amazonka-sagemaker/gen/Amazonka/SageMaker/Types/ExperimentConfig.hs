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
-- Module      : Amazonka.SageMaker.Types.ExperimentConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ExperimentConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  { -- | The name of an existing trial to associate the trial component with. If
    -- not specified, a new trial is created.
    trialName :: Prelude.Maybe Prelude.Text,
    -- | The display name for the trial component. If this key isn\'t specified,
    -- the display name is the trial component name.
    trialComponentDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The name of an existing experiment to associate the trial component
    -- with.
    experimentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialName', 'experimentConfig_trialName' - The name of an existing trial to associate the trial component with. If
-- not specified, a new trial is created.
--
-- 'trialComponentDisplayName', 'experimentConfig_trialComponentDisplayName' - The display name for the trial component. If this key isn\'t specified,
-- the display name is the trial component name.
--
-- 'experimentName', 'experimentConfig_experimentName' - The name of an existing experiment to associate the trial component
-- with.
newExperimentConfig ::
  ExperimentConfig
newExperimentConfig =
  ExperimentConfig'
    { trialName = Prelude.Nothing,
      trialComponentDisplayName = Prelude.Nothing,
      experimentName = Prelude.Nothing
    }

-- | The name of an existing trial to associate the trial component with. If
-- not specified, a new trial is created.
experimentConfig_trialName :: Lens.Lens' ExperimentConfig (Prelude.Maybe Prelude.Text)
experimentConfig_trialName = Lens.lens (\ExperimentConfig' {trialName} -> trialName) (\s@ExperimentConfig' {} a -> s {trialName = a} :: ExperimentConfig)

-- | The display name for the trial component. If this key isn\'t specified,
-- the display name is the trial component name.
experimentConfig_trialComponentDisplayName :: Lens.Lens' ExperimentConfig (Prelude.Maybe Prelude.Text)
experimentConfig_trialComponentDisplayName = Lens.lens (\ExperimentConfig' {trialComponentDisplayName} -> trialComponentDisplayName) (\s@ExperimentConfig' {} a -> s {trialComponentDisplayName = a} :: ExperimentConfig)

-- | The name of an existing experiment to associate the trial component
-- with.
experimentConfig_experimentName :: Lens.Lens' ExperimentConfig (Prelude.Maybe Prelude.Text)
experimentConfig_experimentName = Lens.lens (\ExperimentConfig' {experimentName} -> experimentName) (\s@ExperimentConfig' {} a -> s {experimentName = a} :: ExperimentConfig)

instance Data.FromJSON ExperimentConfig where
  parseJSON =
    Data.withObject
      "ExperimentConfig"
      ( \x ->
          ExperimentConfig'
            Prelude.<$> (x Data..:? "TrialName")
            Prelude.<*> (x Data..:? "TrialComponentDisplayName")
            Prelude.<*> (x Data..:? "ExperimentName")
      )

instance Prelude.Hashable ExperimentConfig where
  hashWithSalt _salt ExperimentConfig' {..} =
    _salt `Prelude.hashWithSalt` trialName
      `Prelude.hashWithSalt` trialComponentDisplayName
      `Prelude.hashWithSalt` experimentName

instance Prelude.NFData ExperimentConfig where
  rnf ExperimentConfig' {..} =
    Prelude.rnf trialName
      `Prelude.seq` Prelude.rnf trialComponentDisplayName
      `Prelude.seq` Prelude.rnf experimentName

instance Data.ToJSON ExperimentConfig where
  toJSON ExperimentConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TrialName" Data..=) Prelude.<$> trialName,
            ("TrialComponentDisplayName" Data..=)
              Prelude.<$> trialComponentDisplayName,
            ("ExperimentName" Data..=)
              Prelude.<$> experimentName
          ]
      )
