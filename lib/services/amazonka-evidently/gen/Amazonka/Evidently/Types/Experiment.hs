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
-- Module      : Amazonka.Evidently.Types.Experiment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.Experiment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Evidently.Types.ExperimentExecution
import Amazonka.Evidently.Types.ExperimentSchedule
import Amazonka.Evidently.Types.ExperimentStatus
import Amazonka.Evidently.Types.ExperimentType
import Amazonka.Evidently.Types.MetricGoal
import Amazonka.Evidently.Types.OnlineAbDefinition
import Amazonka.Evidently.Types.Treatment
import qualified Amazonka.Prelude as Prelude

-- | A structure containing the configuration details of an experiment.
--
-- /See:/ 'newExperiment' smart constructor.
data Experiment = Experiment'
  { -- | A structure that contains the time and date that Evidently completed the
    -- analysis of the experiment.
    schedule :: Prelude.Maybe ExperimentSchedule,
    -- | The list of tag keys and values associated with this experiment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A structure that contains the configuration of which variation to use as
    -- the \"control\" version. The \"control\" version is used for comparison
    -- with other variations. This structure also specifies how much experiment
    -- traffic is allocated to each variation.
    onlineAbDefinition :: Prelude.Maybe OnlineAbDefinition,
    -- | If the experiment was stopped, this is the string that was entered by
    -- the person who stopped the experiment, to explain why it was stopped.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | A description of the experiment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the project that contains this experiment.
    project :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains the date and time that the experiment started
    -- and ended.
    execution :: Prelude.Maybe ExperimentExecution,
    -- | An array of structures that describe the configuration of each feature
    -- variation used in the experiment.
    treatments :: Prelude.Maybe (Prelude.NonEmpty Treatment),
    -- | In thousandths of a percent, the amount of the available audience that
    -- is allocated to this experiment. The available audience is the total
    -- audience minus the audience that you have allocated to overrides or
    -- current launches of this feature.
    --
    -- This is represented in thousandths of a percent, so a value of 10,000 is
    -- 10% of the available audience.
    samplingRate :: Prelude.Maybe Prelude.Natural,
    -- | The audience segment being used for the experiment, if a segment is
    -- being used.
    segment :: Prelude.Maybe Prelude.Text,
    -- | An array of structures that defines the metrics used for the experiment,
    -- and whether a higher or lower value for each metric is the goal.
    metricGoals :: Prelude.Maybe (Prelude.NonEmpty MetricGoal),
    -- | This value is used when Evidently assigns a particular user session to
    -- the experiment. It helps create a randomization ID to determine which
    -- variation the user session is served. This randomization ID is a
    -- combination of the entity ID and @randomizationSalt@.
    randomizationSalt :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the experiment.
    arn :: Prelude.Text,
    -- | The date and time that the experiment is first created.
    createdTime :: Core.POSIX,
    -- | The date and time that the experiment was most recently updated.
    lastUpdatedTime :: Core.POSIX,
    -- | The name of the experiment.
    name :: Prelude.Text,
    -- | The current state of the experiment.
    status :: ExperimentStatus,
    -- | The type of this experiment. Currently, this value must be
    -- @aws.experiment.onlineab@.
    type' :: ExperimentType
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
-- 'schedule', 'experiment_schedule' - A structure that contains the time and date that Evidently completed the
-- analysis of the experiment.
--
-- 'tags', 'experiment_tags' - The list of tag keys and values associated with this experiment.
--
-- 'onlineAbDefinition', 'experiment_onlineAbDefinition' - A structure that contains the configuration of which variation to use as
-- the \"control\" version. The \"control\" version is used for comparison
-- with other variations. This structure also specifies how much experiment
-- traffic is allocated to each variation.
--
-- 'statusReason', 'experiment_statusReason' - If the experiment was stopped, this is the string that was entered by
-- the person who stopped the experiment, to explain why it was stopped.
--
-- 'description', 'experiment_description' - A description of the experiment.
--
-- 'project', 'experiment_project' - The name or ARN of the project that contains this experiment.
--
-- 'execution', 'experiment_execution' - A structure that contains the date and time that the experiment started
-- and ended.
--
-- 'treatments', 'experiment_treatments' - An array of structures that describe the configuration of each feature
-- variation used in the experiment.
--
-- 'samplingRate', 'experiment_samplingRate' - In thousandths of a percent, the amount of the available audience that
-- is allocated to this experiment. The available audience is the total
-- audience minus the audience that you have allocated to overrides or
-- current launches of this feature.
--
-- This is represented in thousandths of a percent, so a value of 10,000 is
-- 10% of the available audience.
--
-- 'segment', 'experiment_segment' - The audience segment being used for the experiment, if a segment is
-- being used.
--
-- 'metricGoals', 'experiment_metricGoals' - An array of structures that defines the metrics used for the experiment,
-- and whether a higher or lower value for each metric is the goal.
--
-- 'randomizationSalt', 'experiment_randomizationSalt' - This value is used when Evidently assigns a particular user session to
-- the experiment. It helps create a randomization ID to determine which
-- variation the user session is served. This randomization ID is a
-- combination of the entity ID and @randomizationSalt@.
--
-- 'arn', 'experiment_arn' - The ARN of the experiment.
--
-- 'createdTime', 'experiment_createdTime' - The date and time that the experiment is first created.
--
-- 'lastUpdatedTime', 'experiment_lastUpdatedTime' - The date and time that the experiment was most recently updated.
--
-- 'name', 'experiment_name' - The name of the experiment.
--
-- 'status', 'experiment_status' - The current state of the experiment.
--
-- 'type'', 'experiment_type' - The type of this experiment. Currently, this value must be
-- @aws.experiment.onlineab@.
newExperiment ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  -- | 'lastUpdatedTime'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  ExperimentStatus ->
  -- | 'type''
  ExperimentType ->
  Experiment
newExperiment
  pArn_
  pCreatedTime_
  pLastUpdatedTime_
  pName_
  pStatus_
  pType_ =
    Experiment'
      { schedule = Prelude.Nothing,
        tags = Prelude.Nothing,
        onlineAbDefinition = Prelude.Nothing,
        statusReason = Prelude.Nothing,
        description = Prelude.Nothing,
        project = Prelude.Nothing,
        execution = Prelude.Nothing,
        treatments = Prelude.Nothing,
        samplingRate = Prelude.Nothing,
        segment = Prelude.Nothing,
        metricGoals = Prelude.Nothing,
        randomizationSalt = Prelude.Nothing,
        arn = pArn_,
        createdTime = Core._Time Lens.# pCreatedTime_,
        lastUpdatedTime =
          Core._Time Lens.# pLastUpdatedTime_,
        name = pName_,
        status = pStatus_,
        type' = pType_
      }

-- | A structure that contains the time and date that Evidently completed the
-- analysis of the experiment.
experiment_schedule :: Lens.Lens' Experiment (Prelude.Maybe ExperimentSchedule)
experiment_schedule = Lens.lens (\Experiment' {schedule} -> schedule) (\s@Experiment' {} a -> s {schedule = a} :: Experiment)

-- | The list of tag keys and values associated with this experiment.
experiment_tags :: Lens.Lens' Experiment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
experiment_tags = Lens.lens (\Experiment' {tags} -> tags) (\s@Experiment' {} a -> s {tags = a} :: Experiment) Prelude.. Lens.mapping Lens.coerced

-- | A structure that contains the configuration of which variation to use as
-- the \"control\" version. The \"control\" version is used for comparison
-- with other variations. This structure also specifies how much experiment
-- traffic is allocated to each variation.
experiment_onlineAbDefinition :: Lens.Lens' Experiment (Prelude.Maybe OnlineAbDefinition)
experiment_onlineAbDefinition = Lens.lens (\Experiment' {onlineAbDefinition} -> onlineAbDefinition) (\s@Experiment' {} a -> s {onlineAbDefinition = a} :: Experiment)

-- | If the experiment was stopped, this is the string that was entered by
-- the person who stopped the experiment, to explain why it was stopped.
experiment_statusReason :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_statusReason = Lens.lens (\Experiment' {statusReason} -> statusReason) (\s@Experiment' {} a -> s {statusReason = a} :: Experiment)

-- | A description of the experiment.
experiment_description :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_description = Lens.lens (\Experiment' {description} -> description) (\s@Experiment' {} a -> s {description = a} :: Experiment)

-- | The name or ARN of the project that contains this experiment.
experiment_project :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_project = Lens.lens (\Experiment' {project} -> project) (\s@Experiment' {} a -> s {project = a} :: Experiment)

-- | A structure that contains the date and time that the experiment started
-- and ended.
experiment_execution :: Lens.Lens' Experiment (Prelude.Maybe ExperimentExecution)
experiment_execution = Lens.lens (\Experiment' {execution} -> execution) (\s@Experiment' {} a -> s {execution = a} :: Experiment)

-- | An array of structures that describe the configuration of each feature
-- variation used in the experiment.
experiment_treatments :: Lens.Lens' Experiment (Prelude.Maybe (Prelude.NonEmpty Treatment))
experiment_treatments = Lens.lens (\Experiment' {treatments} -> treatments) (\s@Experiment' {} a -> s {treatments = a} :: Experiment) Prelude.. Lens.mapping Lens.coerced

-- | In thousandths of a percent, the amount of the available audience that
-- is allocated to this experiment. The available audience is the total
-- audience minus the audience that you have allocated to overrides or
-- current launches of this feature.
--
-- This is represented in thousandths of a percent, so a value of 10,000 is
-- 10% of the available audience.
experiment_samplingRate :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Natural)
experiment_samplingRate = Lens.lens (\Experiment' {samplingRate} -> samplingRate) (\s@Experiment' {} a -> s {samplingRate = a} :: Experiment)

-- | The audience segment being used for the experiment, if a segment is
-- being used.
experiment_segment :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_segment = Lens.lens (\Experiment' {segment} -> segment) (\s@Experiment' {} a -> s {segment = a} :: Experiment)

-- | An array of structures that defines the metrics used for the experiment,
-- and whether a higher or lower value for each metric is the goal.
experiment_metricGoals :: Lens.Lens' Experiment (Prelude.Maybe (Prelude.NonEmpty MetricGoal))
experiment_metricGoals = Lens.lens (\Experiment' {metricGoals} -> metricGoals) (\s@Experiment' {} a -> s {metricGoals = a} :: Experiment) Prelude.. Lens.mapping Lens.coerced

-- | This value is used when Evidently assigns a particular user session to
-- the experiment. It helps create a randomization ID to determine which
-- variation the user session is served. This randomization ID is a
-- combination of the entity ID and @randomizationSalt@.
experiment_randomizationSalt :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_randomizationSalt = Lens.lens (\Experiment' {randomizationSalt} -> randomizationSalt) (\s@Experiment' {} a -> s {randomizationSalt = a} :: Experiment)

-- | The ARN of the experiment.
experiment_arn :: Lens.Lens' Experiment Prelude.Text
experiment_arn = Lens.lens (\Experiment' {arn} -> arn) (\s@Experiment' {} a -> s {arn = a} :: Experiment)

-- | The date and time that the experiment is first created.
experiment_createdTime :: Lens.Lens' Experiment Prelude.UTCTime
experiment_createdTime = Lens.lens (\Experiment' {createdTime} -> createdTime) (\s@Experiment' {} a -> s {createdTime = a} :: Experiment) Prelude.. Core._Time

-- | The date and time that the experiment was most recently updated.
experiment_lastUpdatedTime :: Lens.Lens' Experiment Prelude.UTCTime
experiment_lastUpdatedTime = Lens.lens (\Experiment' {lastUpdatedTime} -> lastUpdatedTime) (\s@Experiment' {} a -> s {lastUpdatedTime = a} :: Experiment) Prelude.. Core._Time

-- | The name of the experiment.
experiment_name :: Lens.Lens' Experiment Prelude.Text
experiment_name = Lens.lens (\Experiment' {name} -> name) (\s@Experiment' {} a -> s {name = a} :: Experiment)

-- | The current state of the experiment.
experiment_status :: Lens.Lens' Experiment ExperimentStatus
experiment_status = Lens.lens (\Experiment' {status} -> status) (\s@Experiment' {} a -> s {status = a} :: Experiment)

-- | The type of this experiment. Currently, this value must be
-- @aws.experiment.onlineab@.
experiment_type :: Lens.Lens' Experiment ExperimentType
experiment_type = Lens.lens (\Experiment' {type'} -> type') (\s@Experiment' {} a -> s {type' = a} :: Experiment)

instance Core.FromJSON Experiment where
  parseJSON =
    Core.withObject
      "Experiment"
      ( \x ->
          Experiment'
            Prelude.<$> (x Core..:? "schedule")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "onlineAbDefinition")
            Prelude.<*> (x Core..:? "statusReason")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "project")
            Prelude.<*> (x Core..:? "execution")
            Prelude.<*> (x Core..:? "treatments")
            Prelude.<*> (x Core..:? "samplingRate")
            Prelude.<*> (x Core..:? "segment")
            Prelude.<*> (x Core..:? "metricGoals")
            Prelude.<*> (x Core..:? "randomizationSalt")
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> (x Core..: "createdTime")
            Prelude.<*> (x Core..: "lastUpdatedTime")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "status")
            Prelude.<*> (x Core..: "type")
      )

instance Prelude.Hashable Experiment where
  hashWithSalt _salt Experiment' {..} =
    _salt `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` onlineAbDefinition
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` project
      `Prelude.hashWithSalt` execution
      `Prelude.hashWithSalt` treatments
      `Prelude.hashWithSalt` samplingRate
      `Prelude.hashWithSalt` segment
      `Prelude.hashWithSalt` metricGoals
      `Prelude.hashWithSalt` randomizationSalt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Experiment where
  rnf Experiment' {..} =
    Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf onlineAbDefinition
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf project
      `Prelude.seq` Prelude.rnf execution
      `Prelude.seq` Prelude.rnf treatments
      `Prelude.seq` Prelude.rnf samplingRate
      `Prelude.seq` Prelude.rnf segment
      `Prelude.seq` Prelude.rnf metricGoals
      `Prelude.seq` Prelude.rnf randomizationSalt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
