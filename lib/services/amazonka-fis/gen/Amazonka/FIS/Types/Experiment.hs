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
-- Module      : Amazonka.FIS.Types.Experiment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.Experiment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types.ExperimentAction
import Amazonka.FIS.Types.ExperimentLogConfiguration
import Amazonka.FIS.Types.ExperimentState
import Amazonka.FIS.Types.ExperimentStopCondition
import Amazonka.FIS.Types.ExperimentTarget
import qualified Amazonka.Prelude as Prelude

-- | Describes an experiment.
--
-- /See:/ 'newExperiment' smart constructor.
data Experiment = Experiment'
  { -- | The actions for the experiment.
    actions :: Prelude.Maybe (Prelude.HashMap Prelude.Text ExperimentAction),
    -- | The time that the experiment was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The time that the experiment ended.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the experiment template.
    experimentTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the experiment.
    id :: Prelude.Maybe Prelude.Text,
    -- | The configuration for experiment logging.
    logConfiguration :: Prelude.Maybe ExperimentLogConfiguration,
    -- | The Amazon Resource Name (ARN) of an IAM role that grants the FIS
    -- service permission to perform service actions on your behalf.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The time that the experiment started.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The state of the experiment.
    state :: Prelude.Maybe ExperimentState,
    -- | The stop conditions for the experiment.
    stopConditions :: Prelude.Maybe [ExperimentStopCondition],
    -- | The tags for the experiment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The targets for the experiment.
    targets :: Prelude.Maybe (Prelude.HashMap Prelude.Text ExperimentTarget)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Experiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'experiment_actions' - The actions for the experiment.
--
-- 'creationTime', 'experiment_creationTime' - The time that the experiment was created.
--
-- 'endTime', 'experiment_endTime' - The time that the experiment ended.
--
-- 'experimentTemplateId', 'experiment_experimentTemplateId' - The ID of the experiment template.
--
-- 'id', 'experiment_id' - The ID of the experiment.
--
-- 'logConfiguration', 'experiment_logConfiguration' - The configuration for experiment logging.
--
-- 'roleArn', 'experiment_roleArn' - The Amazon Resource Name (ARN) of an IAM role that grants the FIS
-- service permission to perform service actions on your behalf.
--
-- 'startTime', 'experiment_startTime' - The time that the experiment started.
--
-- 'state', 'experiment_state' - The state of the experiment.
--
-- 'stopConditions', 'experiment_stopConditions' - The stop conditions for the experiment.
--
-- 'tags', 'experiment_tags' - The tags for the experiment.
--
-- 'targets', 'experiment_targets' - The targets for the experiment.
newExperiment ::
  Experiment
newExperiment =
  Experiment'
    { actions = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      experimentTemplateId = Prelude.Nothing,
      id = Prelude.Nothing,
      logConfiguration = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      startTime = Prelude.Nothing,
      state = Prelude.Nothing,
      stopConditions = Prelude.Nothing,
      tags = Prelude.Nothing,
      targets = Prelude.Nothing
    }

-- | The actions for the experiment.
experiment_actions :: Lens.Lens' Experiment (Prelude.Maybe (Prelude.HashMap Prelude.Text ExperimentAction))
experiment_actions = Lens.lens (\Experiment' {actions} -> actions) (\s@Experiment' {} a -> s {actions = a} :: Experiment) Prelude.. Lens.mapping Lens.coerced

-- | The time that the experiment was created.
experiment_creationTime :: Lens.Lens' Experiment (Prelude.Maybe Prelude.UTCTime)
experiment_creationTime = Lens.lens (\Experiment' {creationTime} -> creationTime) (\s@Experiment' {} a -> s {creationTime = a} :: Experiment) Prelude.. Lens.mapping Data._Time

-- | The time that the experiment ended.
experiment_endTime :: Lens.Lens' Experiment (Prelude.Maybe Prelude.UTCTime)
experiment_endTime = Lens.lens (\Experiment' {endTime} -> endTime) (\s@Experiment' {} a -> s {endTime = a} :: Experiment) Prelude.. Lens.mapping Data._Time

-- | The ID of the experiment template.
experiment_experimentTemplateId :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_experimentTemplateId = Lens.lens (\Experiment' {experimentTemplateId} -> experimentTemplateId) (\s@Experiment' {} a -> s {experimentTemplateId = a} :: Experiment)

-- | The ID of the experiment.
experiment_id :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_id = Lens.lens (\Experiment' {id} -> id) (\s@Experiment' {} a -> s {id = a} :: Experiment)

-- | The configuration for experiment logging.
experiment_logConfiguration :: Lens.Lens' Experiment (Prelude.Maybe ExperimentLogConfiguration)
experiment_logConfiguration = Lens.lens (\Experiment' {logConfiguration} -> logConfiguration) (\s@Experiment' {} a -> s {logConfiguration = a} :: Experiment)

-- | The Amazon Resource Name (ARN) of an IAM role that grants the FIS
-- service permission to perform service actions on your behalf.
experiment_roleArn :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_roleArn = Lens.lens (\Experiment' {roleArn} -> roleArn) (\s@Experiment' {} a -> s {roleArn = a} :: Experiment)

-- | The time that the experiment started.
experiment_startTime :: Lens.Lens' Experiment (Prelude.Maybe Prelude.UTCTime)
experiment_startTime = Lens.lens (\Experiment' {startTime} -> startTime) (\s@Experiment' {} a -> s {startTime = a} :: Experiment) Prelude.. Lens.mapping Data._Time

-- | The state of the experiment.
experiment_state :: Lens.Lens' Experiment (Prelude.Maybe ExperimentState)
experiment_state = Lens.lens (\Experiment' {state} -> state) (\s@Experiment' {} a -> s {state = a} :: Experiment)

-- | The stop conditions for the experiment.
experiment_stopConditions :: Lens.Lens' Experiment (Prelude.Maybe [ExperimentStopCondition])
experiment_stopConditions = Lens.lens (\Experiment' {stopConditions} -> stopConditions) (\s@Experiment' {} a -> s {stopConditions = a} :: Experiment) Prelude.. Lens.mapping Lens.coerced

-- | The tags for the experiment.
experiment_tags :: Lens.Lens' Experiment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
experiment_tags = Lens.lens (\Experiment' {tags} -> tags) (\s@Experiment' {} a -> s {tags = a} :: Experiment) Prelude.. Lens.mapping Lens.coerced

-- | The targets for the experiment.
experiment_targets :: Lens.Lens' Experiment (Prelude.Maybe (Prelude.HashMap Prelude.Text ExperimentTarget))
experiment_targets = Lens.lens (\Experiment' {targets} -> targets) (\s@Experiment' {} a -> s {targets = a} :: Experiment) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Experiment where
  parseJSON =
    Data.withObject
      "Experiment"
      ( \x ->
          Experiment'
            Prelude.<$> (x Data..:? "actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "experimentTemplateId")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "logConfiguration")
            Prelude.<*> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "stopConditions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "targets" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Experiment where
  hashWithSalt _salt Experiment' {..} =
    _salt
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` experimentTemplateId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` logConfiguration
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stopConditions
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targets

instance Prelude.NFData Experiment where
  rnf Experiment' {..} =
    Prelude.rnf actions `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf endTime `Prelude.seq`
          Prelude.rnf experimentTemplateId `Prelude.seq`
            Prelude.rnf id `Prelude.seq`
              Prelude.rnf logConfiguration `Prelude.seq`
                Prelude.rnf roleArn `Prelude.seq`
                  Prelude.rnf startTime `Prelude.seq`
                    Prelude.rnf state `Prelude.seq`
                      Prelude.rnf stopConditions `Prelude.seq`
                        Prelude.rnf tags `Prelude.seq`
                          Prelude.rnf targets
