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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.Experiment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | A description of the experiment.
    description :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains the date and time that the experiment started
    -- and ended.
    execution :: Prelude.Maybe ExperimentExecution,
    -- | An array of structures that defines the metrics used for the experiment,
    -- and whether a higher or lower value for each metric is the goal.
    metricGoals :: Prelude.Maybe (Prelude.NonEmpty MetricGoal),
    -- | A structure that contains the configuration of which variation to use as
    -- the \"control\" version. The \"control\" version is used for comparison
    -- with other variations. This structure also specifies how much experiment
    -- traffic is allocated to each variation.
    onlineAbDefinition :: Prelude.Maybe OnlineAbDefinition,
    -- | The name or ARN of the project that contains this experiment.
    project :: Prelude.Maybe Prelude.Text,
    -- | This value is used when Evidently assigns a particular user session to
    -- the experiment. It helps create a randomization ID to determine which
    -- variation the user session is served. This randomization ID is a
    -- combination of the entity ID and @randomizationSalt@.
    randomizationSalt :: Prelude.Maybe Prelude.Text,
    -- | In thousandths of a percent, the amount of the available audience that
    -- is allocated to this experiment. The available audience is the total
    -- audience minus the audience that you have allocated to overrides or
    -- current launches of this feature.
    --
    -- This is represented in thousandths of a percent, so a value of 10,000 is
    -- 10% of the available audience.
    samplingRate :: Prelude.Maybe Prelude.Natural,
    -- | A structure that contains the time and date that Evidently completed the
    -- analysis of the experiment.
    schedule :: Prelude.Maybe ExperimentSchedule,
    -- | The audience segment being used for the experiment, if a segment is
    -- being used.
    segment :: Prelude.Maybe Prelude.Text,
    -- | If the experiment was stopped, this is the string that was entered by
    -- the person who stopped the experiment, to explain why it was stopped.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The list of tag keys and values associated with this experiment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An array of structures that describe the configuration of each feature
    -- variation used in the experiment.
    treatments :: Prelude.Maybe (Prelude.NonEmpty Treatment),
    -- | The ARN of the experiment.
    arn :: Prelude.Text,
    -- | The date and time that the experiment is first created.
    createdTime :: Data.POSIX,
    -- | The date and time that the experiment was most recently updated.
    lastUpdatedTime :: Data.POSIX,
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
-- 'description', 'experiment_description' - A description of the experiment.
--
-- 'execution', 'experiment_execution' - A structure that contains the date and time that the experiment started
-- and ended.
--
-- 'metricGoals', 'experiment_metricGoals' - An array of structures that defines the metrics used for the experiment,
-- and whether a higher or lower value for each metric is the goal.
--
-- 'onlineAbDefinition', 'experiment_onlineAbDefinition' - A structure that contains the configuration of which variation to use as
-- the \"control\" version. The \"control\" version is used for comparison
-- with other variations. This structure also specifies how much experiment
-- traffic is allocated to each variation.
--
-- 'project', 'experiment_project' - The name or ARN of the project that contains this experiment.
--
-- 'randomizationSalt', 'experiment_randomizationSalt' - This value is used when Evidently assigns a particular user session to
-- the experiment. It helps create a randomization ID to determine which
-- variation the user session is served. This randomization ID is a
-- combination of the entity ID and @randomizationSalt@.
--
-- 'samplingRate', 'experiment_samplingRate' - In thousandths of a percent, the amount of the available audience that
-- is allocated to this experiment. The available audience is the total
-- audience minus the audience that you have allocated to overrides or
-- current launches of this feature.
--
-- This is represented in thousandths of a percent, so a value of 10,000 is
-- 10% of the available audience.
--
-- 'schedule', 'experiment_schedule' - A structure that contains the time and date that Evidently completed the
-- analysis of the experiment.
--
-- 'segment', 'experiment_segment' - The audience segment being used for the experiment, if a segment is
-- being used.
--
-- 'statusReason', 'experiment_statusReason' - If the experiment was stopped, this is the string that was entered by
-- the person who stopped the experiment, to explain why it was stopped.
--
-- 'tags', 'experiment_tags' - The list of tag keys and values associated with this experiment.
--
-- 'treatments', 'experiment_treatments' - An array of structures that describe the configuration of each feature
-- variation used in the experiment.
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
      { description = Prelude.Nothing,
        execution = Prelude.Nothing,
        metricGoals = Prelude.Nothing,
        onlineAbDefinition = Prelude.Nothing,
        project = Prelude.Nothing,
        randomizationSalt = Prelude.Nothing,
        samplingRate = Prelude.Nothing,
        schedule = Prelude.Nothing,
        segment = Prelude.Nothing,
        statusReason = Prelude.Nothing,
        tags = Prelude.Nothing,
        treatments = Prelude.Nothing,
        arn = pArn_,
        createdTime = Data._Time Lens.# pCreatedTime_,
        lastUpdatedTime =
          Data._Time Lens.# pLastUpdatedTime_,
        name = pName_,
        status = pStatus_,
        type' = pType_
      }

-- | A description of the experiment.
experiment_description :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_description = Lens.lens (\Experiment' {description} -> description) (\s@Experiment' {} a -> s {description = a} :: Experiment)

-- | A structure that contains the date and time that the experiment started
-- and ended.
experiment_execution :: Lens.Lens' Experiment (Prelude.Maybe ExperimentExecution)
experiment_execution = Lens.lens (\Experiment' {execution} -> execution) (\s@Experiment' {} a -> s {execution = a} :: Experiment)

-- | An array of structures that defines the metrics used for the experiment,
-- and whether a higher or lower value for each metric is the goal.
experiment_metricGoals :: Lens.Lens' Experiment (Prelude.Maybe (Prelude.NonEmpty MetricGoal))
experiment_metricGoals = Lens.lens (\Experiment' {metricGoals} -> metricGoals) (\s@Experiment' {} a -> s {metricGoals = a} :: Experiment) Prelude.. Lens.mapping Lens.coerced

-- | A structure that contains the configuration of which variation to use as
-- the \"control\" version. The \"control\" version is used for comparison
-- with other variations. This structure also specifies how much experiment
-- traffic is allocated to each variation.
experiment_onlineAbDefinition :: Lens.Lens' Experiment (Prelude.Maybe OnlineAbDefinition)
experiment_onlineAbDefinition = Lens.lens (\Experiment' {onlineAbDefinition} -> onlineAbDefinition) (\s@Experiment' {} a -> s {onlineAbDefinition = a} :: Experiment)

-- | The name or ARN of the project that contains this experiment.
experiment_project :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_project = Lens.lens (\Experiment' {project} -> project) (\s@Experiment' {} a -> s {project = a} :: Experiment)

-- | This value is used when Evidently assigns a particular user session to
-- the experiment. It helps create a randomization ID to determine which
-- variation the user session is served. This randomization ID is a
-- combination of the entity ID and @randomizationSalt@.
experiment_randomizationSalt :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_randomizationSalt = Lens.lens (\Experiment' {randomizationSalt} -> randomizationSalt) (\s@Experiment' {} a -> s {randomizationSalt = a} :: Experiment)

-- | In thousandths of a percent, the amount of the available audience that
-- is allocated to this experiment. The available audience is the total
-- audience minus the audience that you have allocated to overrides or
-- current launches of this feature.
--
-- This is represented in thousandths of a percent, so a value of 10,000 is
-- 10% of the available audience.
experiment_samplingRate :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Natural)
experiment_samplingRate = Lens.lens (\Experiment' {samplingRate} -> samplingRate) (\s@Experiment' {} a -> s {samplingRate = a} :: Experiment)

-- | A structure that contains the time and date that Evidently completed the
-- analysis of the experiment.
experiment_schedule :: Lens.Lens' Experiment (Prelude.Maybe ExperimentSchedule)
experiment_schedule = Lens.lens (\Experiment' {schedule} -> schedule) (\s@Experiment' {} a -> s {schedule = a} :: Experiment)

-- | The audience segment being used for the experiment, if a segment is
-- being used.
experiment_segment :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_segment = Lens.lens (\Experiment' {segment} -> segment) (\s@Experiment' {} a -> s {segment = a} :: Experiment)

-- | If the experiment was stopped, this is the string that was entered by
-- the person who stopped the experiment, to explain why it was stopped.
experiment_statusReason :: Lens.Lens' Experiment (Prelude.Maybe Prelude.Text)
experiment_statusReason = Lens.lens (\Experiment' {statusReason} -> statusReason) (\s@Experiment' {} a -> s {statusReason = a} :: Experiment)

-- | The list of tag keys and values associated with this experiment.
experiment_tags :: Lens.Lens' Experiment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
experiment_tags = Lens.lens (\Experiment' {tags} -> tags) (\s@Experiment' {} a -> s {tags = a} :: Experiment) Prelude.. Lens.mapping Lens.coerced

-- | An array of structures that describe the configuration of each feature
-- variation used in the experiment.
experiment_treatments :: Lens.Lens' Experiment (Prelude.Maybe (Prelude.NonEmpty Treatment))
experiment_treatments = Lens.lens (\Experiment' {treatments} -> treatments) (\s@Experiment' {} a -> s {treatments = a} :: Experiment) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the experiment.
experiment_arn :: Lens.Lens' Experiment Prelude.Text
experiment_arn = Lens.lens (\Experiment' {arn} -> arn) (\s@Experiment' {} a -> s {arn = a} :: Experiment)

-- | The date and time that the experiment is first created.
experiment_createdTime :: Lens.Lens' Experiment Prelude.UTCTime
experiment_createdTime = Lens.lens (\Experiment' {createdTime} -> createdTime) (\s@Experiment' {} a -> s {createdTime = a} :: Experiment) Prelude.. Data._Time

-- | The date and time that the experiment was most recently updated.
experiment_lastUpdatedTime :: Lens.Lens' Experiment Prelude.UTCTime
experiment_lastUpdatedTime = Lens.lens (\Experiment' {lastUpdatedTime} -> lastUpdatedTime) (\s@Experiment' {} a -> s {lastUpdatedTime = a} :: Experiment) Prelude.. Data._Time

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

instance Data.FromJSON Experiment where
  parseJSON =
    Data.withObject
      "Experiment"
      ( \x ->
          Experiment'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "execution")
            Prelude.<*> (x Data..:? "metricGoals")
            Prelude.<*> (x Data..:? "onlineAbDefinition")
            Prelude.<*> (x Data..:? "project")
            Prelude.<*> (x Data..:? "randomizationSalt")
            Prelude.<*> (x Data..:? "samplingRate")
            Prelude.<*> (x Data..:? "schedule")
            Prelude.<*> (x Data..:? "segment")
            Prelude.<*> (x Data..:? "statusReason")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "treatments")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdTime")
            Prelude.<*> (x Data..: "lastUpdatedTime")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable Experiment where
  hashWithSalt _salt Experiment' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` execution
      `Prelude.hashWithSalt` metricGoals
      `Prelude.hashWithSalt` onlineAbDefinition
      `Prelude.hashWithSalt` project
      `Prelude.hashWithSalt` randomizationSalt
      `Prelude.hashWithSalt` samplingRate
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` segment
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` treatments
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Experiment where
  rnf Experiment' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf execution
      `Prelude.seq` Prelude.rnf metricGoals
      `Prelude.seq` Prelude.rnf onlineAbDefinition
      `Prelude.seq` Prelude.rnf project
      `Prelude.seq` Prelude.rnf randomizationSalt
      `Prelude.seq` Prelude.rnf samplingRate
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf segment
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf treatments
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
