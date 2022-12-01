{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Evidently.UpdateExperiment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Evidently experiment.
--
-- Don\'t use this operation to update an experiment\'s tag. Instead, use
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_TagResource.html TagResource>.
module Amazonka.Evidently.UpdateExperiment
  ( -- * Creating a Request
    UpdateExperiment (..),
    newUpdateExperiment,

    -- * Request Lenses
    updateExperiment_removeSegment,
    updateExperiment_onlineAbConfig,
    updateExperiment_description,
    updateExperiment_treatments,
    updateExperiment_samplingRate,
    updateExperiment_segment,
    updateExperiment_metricGoals,
    updateExperiment_randomizationSalt,
    updateExperiment_experiment,
    updateExperiment_project,

    -- * Destructuring the Response
    UpdateExperimentResponse (..),
    newUpdateExperimentResponse,

    -- * Response Lenses
    updateExperimentResponse_httpStatus,
    updateExperimentResponse_experiment,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateExperiment' smart constructor.
data UpdateExperiment = UpdateExperiment'
  { -- | Removes a segment from being used in an experiment. You can\'t use this
    -- parameter if the experiment is currently running.
    removeSegment :: Prelude.Maybe Prelude.Bool,
    -- | A structure that contains the configuration of which variation o use as
    -- the \"control\" version. The \"control\" version is used for comparison
    -- with other variations. This structure also specifies how much experiment
    -- traffic is allocated to each variation.
    onlineAbConfig :: Prelude.Maybe OnlineAbConfig,
    -- | An optional description of the experiment.
    description :: Prelude.Maybe Prelude.Text,
    -- | An array of structures that define the variations being tested in the
    -- experiment.
    treatments :: Prelude.Maybe [TreatmentConfig],
    -- | The portion of the available audience that you want to allocate to this
    -- experiment, in thousandths of a percent. The available audience is the
    -- total audience minus the audience that you have allocated to overrides
    -- or current launches of this feature.
    --
    -- This is represented in thousandths of a percent. For example, specify
    -- 20,000 to allocate 20% of the available audience.
    samplingRate :: Prelude.Maybe Prelude.Natural,
    -- | Adds an audience /segment/ to an experiment. When a segment is used in
    -- an experiment, only user sessions that match the segment pattern are
    -- used in the experiment. You can\'t use this parameter if the experiment
    -- is currently running.
    segment :: Prelude.Maybe Prelude.Text,
    -- | An array of structures that defines the metrics used for the experiment,
    -- and whether a higher or lower value for each metric is the goal.
    metricGoals :: Prelude.Maybe (Prelude.NonEmpty MetricGoalConfig),
    -- | When Evidently assigns a particular user session to an experiment, it
    -- must use a randomization ID to determine which variation the user
    -- session is served. This randomization ID is a combination of the entity
    -- ID and @randomizationSalt@. If you omit @randomizationSalt@, Evidently
    -- uses the experiment name as the @randomizationSalt@.
    randomizationSalt :: Prelude.Maybe Prelude.Text,
    -- | The name of the experiment to update.
    experiment :: Prelude.Text,
    -- | The name or ARN of the project that contains the experiment that you
    -- want to update.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'removeSegment', 'updateExperiment_removeSegment' - Removes a segment from being used in an experiment. You can\'t use this
-- parameter if the experiment is currently running.
--
-- 'onlineAbConfig', 'updateExperiment_onlineAbConfig' - A structure that contains the configuration of which variation o use as
-- the \"control\" version. The \"control\" version is used for comparison
-- with other variations. This structure also specifies how much experiment
-- traffic is allocated to each variation.
--
-- 'description', 'updateExperiment_description' - An optional description of the experiment.
--
-- 'treatments', 'updateExperiment_treatments' - An array of structures that define the variations being tested in the
-- experiment.
--
-- 'samplingRate', 'updateExperiment_samplingRate' - The portion of the available audience that you want to allocate to this
-- experiment, in thousandths of a percent. The available audience is the
-- total audience minus the audience that you have allocated to overrides
-- or current launches of this feature.
--
-- This is represented in thousandths of a percent. For example, specify
-- 20,000 to allocate 20% of the available audience.
--
-- 'segment', 'updateExperiment_segment' - Adds an audience /segment/ to an experiment. When a segment is used in
-- an experiment, only user sessions that match the segment pattern are
-- used in the experiment. You can\'t use this parameter if the experiment
-- is currently running.
--
-- 'metricGoals', 'updateExperiment_metricGoals' - An array of structures that defines the metrics used for the experiment,
-- and whether a higher or lower value for each metric is the goal.
--
-- 'randomizationSalt', 'updateExperiment_randomizationSalt' - When Evidently assigns a particular user session to an experiment, it
-- must use a randomization ID to determine which variation the user
-- session is served. This randomization ID is a combination of the entity
-- ID and @randomizationSalt@. If you omit @randomizationSalt@, Evidently
-- uses the experiment name as the @randomizationSalt@.
--
-- 'experiment', 'updateExperiment_experiment' - The name of the experiment to update.
--
-- 'project', 'updateExperiment_project' - The name or ARN of the project that contains the experiment that you
-- want to update.
newUpdateExperiment ::
  -- | 'experiment'
  Prelude.Text ->
  -- | 'project'
  Prelude.Text ->
  UpdateExperiment
newUpdateExperiment pExperiment_ pProject_ =
  UpdateExperiment'
    { removeSegment = Prelude.Nothing,
      onlineAbConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      treatments = Prelude.Nothing,
      samplingRate = Prelude.Nothing,
      segment = Prelude.Nothing,
      metricGoals = Prelude.Nothing,
      randomizationSalt = Prelude.Nothing,
      experiment = pExperiment_,
      project = pProject_
    }

-- | Removes a segment from being used in an experiment. You can\'t use this
-- parameter if the experiment is currently running.
updateExperiment_removeSegment :: Lens.Lens' UpdateExperiment (Prelude.Maybe Prelude.Bool)
updateExperiment_removeSegment = Lens.lens (\UpdateExperiment' {removeSegment} -> removeSegment) (\s@UpdateExperiment' {} a -> s {removeSegment = a} :: UpdateExperiment)

-- | A structure that contains the configuration of which variation o use as
-- the \"control\" version. The \"control\" version is used for comparison
-- with other variations. This structure also specifies how much experiment
-- traffic is allocated to each variation.
updateExperiment_onlineAbConfig :: Lens.Lens' UpdateExperiment (Prelude.Maybe OnlineAbConfig)
updateExperiment_onlineAbConfig = Lens.lens (\UpdateExperiment' {onlineAbConfig} -> onlineAbConfig) (\s@UpdateExperiment' {} a -> s {onlineAbConfig = a} :: UpdateExperiment)

-- | An optional description of the experiment.
updateExperiment_description :: Lens.Lens' UpdateExperiment (Prelude.Maybe Prelude.Text)
updateExperiment_description = Lens.lens (\UpdateExperiment' {description} -> description) (\s@UpdateExperiment' {} a -> s {description = a} :: UpdateExperiment)

-- | An array of structures that define the variations being tested in the
-- experiment.
updateExperiment_treatments :: Lens.Lens' UpdateExperiment (Prelude.Maybe [TreatmentConfig])
updateExperiment_treatments = Lens.lens (\UpdateExperiment' {treatments} -> treatments) (\s@UpdateExperiment' {} a -> s {treatments = a} :: UpdateExperiment) Prelude.. Lens.mapping Lens.coerced

-- | The portion of the available audience that you want to allocate to this
-- experiment, in thousandths of a percent. The available audience is the
-- total audience minus the audience that you have allocated to overrides
-- or current launches of this feature.
--
-- This is represented in thousandths of a percent. For example, specify
-- 20,000 to allocate 20% of the available audience.
updateExperiment_samplingRate :: Lens.Lens' UpdateExperiment (Prelude.Maybe Prelude.Natural)
updateExperiment_samplingRate = Lens.lens (\UpdateExperiment' {samplingRate} -> samplingRate) (\s@UpdateExperiment' {} a -> s {samplingRate = a} :: UpdateExperiment)

-- | Adds an audience /segment/ to an experiment. When a segment is used in
-- an experiment, only user sessions that match the segment pattern are
-- used in the experiment. You can\'t use this parameter if the experiment
-- is currently running.
updateExperiment_segment :: Lens.Lens' UpdateExperiment (Prelude.Maybe Prelude.Text)
updateExperiment_segment = Lens.lens (\UpdateExperiment' {segment} -> segment) (\s@UpdateExperiment' {} a -> s {segment = a} :: UpdateExperiment)

-- | An array of structures that defines the metrics used for the experiment,
-- and whether a higher or lower value for each metric is the goal.
updateExperiment_metricGoals :: Lens.Lens' UpdateExperiment (Prelude.Maybe (Prelude.NonEmpty MetricGoalConfig))
updateExperiment_metricGoals = Lens.lens (\UpdateExperiment' {metricGoals} -> metricGoals) (\s@UpdateExperiment' {} a -> s {metricGoals = a} :: UpdateExperiment) Prelude.. Lens.mapping Lens.coerced

-- | When Evidently assigns a particular user session to an experiment, it
-- must use a randomization ID to determine which variation the user
-- session is served. This randomization ID is a combination of the entity
-- ID and @randomizationSalt@. If you omit @randomizationSalt@, Evidently
-- uses the experiment name as the @randomizationSalt@.
updateExperiment_randomizationSalt :: Lens.Lens' UpdateExperiment (Prelude.Maybe Prelude.Text)
updateExperiment_randomizationSalt = Lens.lens (\UpdateExperiment' {randomizationSalt} -> randomizationSalt) (\s@UpdateExperiment' {} a -> s {randomizationSalt = a} :: UpdateExperiment)

-- | The name of the experiment to update.
updateExperiment_experiment :: Lens.Lens' UpdateExperiment Prelude.Text
updateExperiment_experiment = Lens.lens (\UpdateExperiment' {experiment} -> experiment) (\s@UpdateExperiment' {} a -> s {experiment = a} :: UpdateExperiment)

-- | The name or ARN of the project that contains the experiment that you
-- want to update.
updateExperiment_project :: Lens.Lens' UpdateExperiment Prelude.Text
updateExperiment_project = Lens.lens (\UpdateExperiment' {project} -> project) (\s@UpdateExperiment' {} a -> s {project = a} :: UpdateExperiment)

instance Core.AWSRequest UpdateExperiment where
  type
    AWSResponse UpdateExperiment =
      UpdateExperimentResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateExperimentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "experiment")
      )

instance Prelude.Hashable UpdateExperiment where
  hashWithSalt _salt UpdateExperiment' {..} =
    _salt `Prelude.hashWithSalt` removeSegment
      `Prelude.hashWithSalt` onlineAbConfig
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` treatments
      `Prelude.hashWithSalt` samplingRate
      `Prelude.hashWithSalt` segment
      `Prelude.hashWithSalt` metricGoals
      `Prelude.hashWithSalt` randomizationSalt
      `Prelude.hashWithSalt` experiment
      `Prelude.hashWithSalt` project

instance Prelude.NFData UpdateExperiment where
  rnf UpdateExperiment' {..} =
    Prelude.rnf removeSegment
      `Prelude.seq` Prelude.rnf onlineAbConfig
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf treatments
      `Prelude.seq` Prelude.rnf samplingRate
      `Prelude.seq` Prelude.rnf segment
      `Prelude.seq` Prelude.rnf metricGoals
      `Prelude.seq` Prelude.rnf randomizationSalt
      `Prelude.seq` Prelude.rnf experiment
      `Prelude.seq` Prelude.rnf project

instance Core.ToHeaders UpdateExperiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateExperiment where
  toJSON UpdateExperiment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("removeSegment" Core..=) Prelude.<$> removeSegment,
            ("onlineAbConfig" Core..=)
              Prelude.<$> onlineAbConfig,
            ("description" Core..=) Prelude.<$> description,
            ("treatments" Core..=) Prelude.<$> treatments,
            ("samplingRate" Core..=) Prelude.<$> samplingRate,
            ("segment" Core..=) Prelude.<$> segment,
            ("metricGoals" Core..=) Prelude.<$> metricGoals,
            ("randomizationSalt" Core..=)
              Prelude.<$> randomizationSalt
          ]
      )

instance Core.ToPath UpdateExperiment where
  toPath UpdateExperiment' {..} =
    Prelude.mconcat
      [ "/projects/",
        Core.toBS project,
        "/experiments/",
        Core.toBS experiment
      ]

instance Core.ToQuery UpdateExperiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateExperimentResponse' smart constructor.
data UpdateExperimentResponse = UpdateExperimentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing the configuration details of the experiment that
    -- was updated.
    experiment :: Experiment
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateExperimentResponse_httpStatus' - The response's http status code.
--
-- 'experiment', 'updateExperimentResponse_experiment' - A structure containing the configuration details of the experiment that
-- was updated.
newUpdateExperimentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'experiment'
  Experiment ->
  UpdateExperimentResponse
newUpdateExperimentResponse pHttpStatus_ pExperiment_ =
  UpdateExperimentResponse'
    { httpStatus =
        pHttpStatus_,
      experiment = pExperiment_
    }

-- | The response's http status code.
updateExperimentResponse_httpStatus :: Lens.Lens' UpdateExperimentResponse Prelude.Int
updateExperimentResponse_httpStatus = Lens.lens (\UpdateExperimentResponse' {httpStatus} -> httpStatus) (\s@UpdateExperimentResponse' {} a -> s {httpStatus = a} :: UpdateExperimentResponse)

-- | A structure containing the configuration details of the experiment that
-- was updated.
updateExperimentResponse_experiment :: Lens.Lens' UpdateExperimentResponse Experiment
updateExperimentResponse_experiment = Lens.lens (\UpdateExperimentResponse' {experiment} -> experiment) (\s@UpdateExperimentResponse' {} a -> s {experiment = a} :: UpdateExperimentResponse)

instance Prelude.NFData UpdateExperimentResponse where
  rnf UpdateExperimentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf experiment
