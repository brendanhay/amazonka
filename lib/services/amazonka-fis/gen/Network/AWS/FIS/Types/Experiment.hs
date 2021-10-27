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
-- Module      : Network.AWS.FIS.Types.Experiment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FIS.Types.Experiment where

import qualified Network.AWS.Core as Core
import Network.AWS.FIS.Types.ExperimentAction
import Network.AWS.FIS.Types.ExperimentState
import Network.AWS.FIS.Types.ExperimentStopCondition
import Network.AWS.FIS.Types.ExperimentTarget
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an experiment.
--
-- /See:/ 'newExperiment' smart constructor.
data Experiment = Experiment'
  { -- | The time the experiment was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the experiment template.
    experimentTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The state of the experiment.
    state :: Prelude.Maybe ExperimentState,
    -- | The actions for the experiment.
    actions :: Prelude.Maybe (Prelude.HashMap Prelude.Text ExperimentAction),
    -- | The time that the experiment was started.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The stop conditions for the experiment.
    stopConditions :: Prelude.Maybe [ExperimentStopCondition],
    -- | The time that the experiment ended.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The targets for the experiment.
    targets :: Prelude.Maybe (Prelude.HashMap Prelude.Text ExperimentTarget),
    -- | The ID of the experiment.
    id :: Prelude.Maybe Prelude.Text,
    -- | The tags for the experiment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of an IAM role that grants the AWS FIS
    -- service permission to perform service actions on your behalf.
    roleArn :: Prelude.Maybe Prelude.Text
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
-- 'creationTime', 'experiment_creationTime' - The time the experiment was created.
--
-- 'experimentTemplateId', 'experiment_experimentTemplateId' - The ID of the experiment template.
--
-- 'state', 'experiment_state' - The state of the experiment.
--
-- 'actions', 'experiment_actions' - The actions for the experiment.
--
-- 'startTime', 'experiment_startTime' - The time that the experiment was started.
--
-- 'stopConditions', 'experiment_stopConditions' - The stop conditions for the experiment.
--
-- 'endTime', 'experiment_endTime' - The time that the experiment ended.
--
-- 'targets', 'experiment_targets' - The targets for the experiment.
--
-- 'id', 'experiment_id' - The ID of the experiment.
--
-- 'tags', 'experiment_tags' - The tags for the experiment.
--
-- 'roleArn', 'experiment_roleArn' - The Amazon Resource Name (ARN) of an IAM role that grants the AWS FIS
-- service permission to perform service actions on your behalf.
newExperiment ::
  Experiment
newExperiment =
  Experiment'
    { creationTime = Prelude.Nothing,
      experimentTemplateId = Prelude.Nothing,
      state = Prelude.Nothing,
      actions = Prelude.Nothing,
      startTime = Prelude.Nothing,
      stopConditions = Prelude.Nothing,
      endTime = Prelude.Nothing,
      targets = Prelude.Nothing,
      id = Prelude.Nothing,
      tags = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The time the experiment was created.
experiment_creationTime :: Lens.Lens' Experiment (Prelude.Maybe Prelude.UTCTime)
experiment_creationTime = Lens.lens (\Experiment' {creationTime} -> creationTime) (\s@Experiment' {} a -> s {creationTime = a} :: Experiment) Prelude.. Lens.mapping Core._Time

-- | The ID of the experiment template.
experiment_experimentTemplateId :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_experimentTemplateId = Lens.lens (\Experiment' {experimentTemplateId} -> experimentTemplateId) (\s@Experiment' {} a -> s {experimentTemplateId = a} :: Experiment)

-- | The state of the experiment.
experiment_state :: Lens.Lens' Experiment (Prelude.Maybe ExperimentState)
experiment_state = Lens.lens (\Experiment' {state} -> state) (\s@Experiment' {} a -> s {state = a} :: Experiment)

-- | The actions for the experiment.
experiment_actions :: Lens.Lens' Experiment (Prelude.Maybe (Prelude.HashMap Prelude.Text ExperimentAction))
experiment_actions = Lens.lens (\Experiment' {actions} -> actions) (\s@Experiment' {} a -> s {actions = a} :: Experiment) Prelude.. Lens.mapping Lens.coerced

-- | The time that the experiment was started.
experiment_startTime :: Lens.Lens' Experiment (Prelude.Maybe Prelude.UTCTime)
experiment_startTime = Lens.lens (\Experiment' {startTime} -> startTime) (\s@Experiment' {} a -> s {startTime = a} :: Experiment) Prelude.. Lens.mapping Core._Time

-- | The stop conditions for the experiment.
experiment_stopConditions :: Lens.Lens' Experiment (Prelude.Maybe [ExperimentStopCondition])
experiment_stopConditions = Lens.lens (\Experiment' {stopConditions} -> stopConditions) (\s@Experiment' {} a -> s {stopConditions = a} :: Experiment) Prelude.. Lens.mapping Lens.coerced

-- | The time that the experiment ended.
experiment_endTime :: Lens.Lens' Experiment (Prelude.Maybe Prelude.UTCTime)
experiment_endTime = Lens.lens (\Experiment' {endTime} -> endTime) (\s@Experiment' {} a -> s {endTime = a} :: Experiment) Prelude.. Lens.mapping Core._Time

-- | The targets for the experiment.
experiment_targets :: Lens.Lens' Experiment (Prelude.Maybe (Prelude.HashMap Prelude.Text ExperimentTarget))
experiment_targets = Lens.lens (\Experiment' {targets} -> targets) (\s@Experiment' {} a -> s {targets = a} :: Experiment) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the experiment.
experiment_id :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_id = Lens.lens (\Experiment' {id} -> id) (\s@Experiment' {} a -> s {id = a} :: Experiment)

-- | The tags for the experiment.
experiment_tags :: Lens.Lens' Experiment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
experiment_tags = Lens.lens (\Experiment' {tags} -> tags) (\s@Experiment' {} a -> s {tags = a} :: Experiment) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of an IAM role that grants the AWS FIS
-- service permission to perform service actions on your behalf.
experiment_roleArn :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_roleArn = Lens.lens (\Experiment' {roleArn} -> roleArn) (\s@Experiment' {} a -> s {roleArn = a} :: Experiment)

instance Core.FromJSON Experiment where
  parseJSON =
    Core.withObject
      "Experiment"
      ( \x ->
          Experiment'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "experimentTemplateId")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "actions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "startTime")
            Prelude.<*> (x Core..:? "stopConditions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "endTime")
            Prelude.<*> (x Core..:? "targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "roleArn")
      )

instance Prelude.Hashable Experiment

instance Prelude.NFData Experiment
