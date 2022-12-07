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
-- Module      : Amazonka.Evidently.CreateExperiment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Evidently /experiment/. Before you create an experiment, you
-- must create the feature to use for the experiment.
--
-- An experiment helps you make feature design decisions based on evidence
-- and data. An experiment can test as many as five variations at once.
-- Evidently collects experiment data and analyzes it by statistical
-- methods, and provides clear recommendations about which variations
-- perform better.
--
-- You can optionally specify a @segment@ to have the experiment consider
-- only certain audience types in the experiment, such as using only user
-- sessions from a certain location or who use a certain internet browser.
--
-- Don\'t use this operation to update an existing experiment. Instead, use
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_UpdateExperiment.html UpdateExperiment>.
module Amazonka.Evidently.CreateExperiment
  ( -- * Creating a Request
    CreateExperiment (..),
    newCreateExperiment,

    -- * Request Lenses
    createExperiment_tags,
    createExperiment_onlineAbConfig,
    createExperiment_description,
    createExperiment_samplingRate,
    createExperiment_segment,
    createExperiment_randomizationSalt,
    createExperiment_metricGoals,
    createExperiment_name,
    createExperiment_project,
    createExperiment_treatments,

    -- * Destructuring the Response
    CreateExperimentResponse (..),
    newCreateExperimentResponse,

    -- * Response Lenses
    createExperimentResponse_httpStatus,
    createExperimentResponse_experiment,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateExperiment' smart constructor.
data CreateExperiment = CreateExperiment'
  { -- | Assigns one or more tags (key-value pairs) to the experiment.
    --
    -- Tags can help you organize and categorize your resources. You can also
    -- use them to scope user permissions by granting a user permission to
    -- access or change only resources with certain tag values.
    --
    -- Tags don\'t have any semantic meaning to Amazon Web Services and are
    -- interpreted strictly as strings of characters.
    --
    -- >  <p>You can associate as many as 50 tags with an experiment.</p> <p>For more information, see <a href="https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html">Tagging Amazon Web Services resources</a>.</p>
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A structure that contains the configuration of which variation to use as
    -- the \"control\" version. tThe \"control\" version is used for comparison
    -- with other variations. This structure also specifies how much experiment
    -- traffic is allocated to each variation.
    onlineAbConfig :: Prelude.Maybe OnlineAbConfig,
    -- | An optional description of the experiment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The portion of the available audience that you want to allocate to this
    -- experiment, in thousandths of a percent. The available audience is the
    -- total audience minus the audience that you have allocated to overrides
    -- or current launches of this feature.
    --
    -- This is represented in thousandths of a percent. For example, specify
    -- 10,000 to allocate 10% of the available audience.
    samplingRate :: Prelude.Maybe Prelude.Natural,
    -- | Specifies an audience /segment/ to use in the experiment. When a segment
    -- is used in an experiment, only user sessions that match the segment
    -- pattern are used in the experiment.
    segment :: Prelude.Maybe Prelude.Text,
    -- | When Evidently assigns a particular user session to an experiment, it
    -- must use a randomization ID to determine which variation the user
    -- session is served. This randomization ID is a combination of the entity
    -- ID and @randomizationSalt@. If you omit @randomizationSalt@, Evidently
    -- uses the experiment name as the @randomizationSalt@.
    randomizationSalt :: Prelude.Maybe Prelude.Text,
    -- | An array of structures that defines the metrics used for the experiment,
    -- and whether a higher or lower value for each metric is the goal.
    metricGoals :: Prelude.NonEmpty MetricGoalConfig,
    -- | A name for the new experiment.
    name :: Prelude.Text,
    -- | The name or ARN of the project that you want to create the new
    -- experiment in.
    project :: Prelude.Text,
    -- | An array of structures that describe the configuration of each feature
    -- variation used in the experiment.
    treatments :: [TreatmentConfig]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createExperiment_tags' - Assigns one or more tags (key-value pairs) to the experiment.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- Tags don\'t have any semantic meaning to Amazon Web Services and are
-- interpreted strictly as strings of characters.
--
-- >  <p>You can associate as many as 50 tags with an experiment.</p> <p>For more information, see <a href="https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html">Tagging Amazon Web Services resources</a>.</p>
--
-- 'onlineAbConfig', 'createExperiment_onlineAbConfig' - A structure that contains the configuration of which variation to use as
-- the \"control\" version. tThe \"control\" version is used for comparison
-- with other variations. This structure also specifies how much experiment
-- traffic is allocated to each variation.
--
-- 'description', 'createExperiment_description' - An optional description of the experiment.
--
-- 'samplingRate', 'createExperiment_samplingRate' - The portion of the available audience that you want to allocate to this
-- experiment, in thousandths of a percent. The available audience is the
-- total audience minus the audience that you have allocated to overrides
-- or current launches of this feature.
--
-- This is represented in thousandths of a percent. For example, specify
-- 10,000 to allocate 10% of the available audience.
--
-- 'segment', 'createExperiment_segment' - Specifies an audience /segment/ to use in the experiment. When a segment
-- is used in an experiment, only user sessions that match the segment
-- pattern are used in the experiment.
--
-- 'randomizationSalt', 'createExperiment_randomizationSalt' - When Evidently assigns a particular user session to an experiment, it
-- must use a randomization ID to determine which variation the user
-- session is served. This randomization ID is a combination of the entity
-- ID and @randomizationSalt@. If you omit @randomizationSalt@, Evidently
-- uses the experiment name as the @randomizationSalt@.
--
-- 'metricGoals', 'createExperiment_metricGoals' - An array of structures that defines the metrics used for the experiment,
-- and whether a higher or lower value for each metric is the goal.
--
-- 'name', 'createExperiment_name' - A name for the new experiment.
--
-- 'project', 'createExperiment_project' - The name or ARN of the project that you want to create the new
-- experiment in.
--
-- 'treatments', 'createExperiment_treatments' - An array of structures that describe the configuration of each feature
-- variation used in the experiment.
newCreateExperiment ::
  -- | 'metricGoals'
  Prelude.NonEmpty MetricGoalConfig ->
  -- | 'name'
  Prelude.Text ->
  -- | 'project'
  Prelude.Text ->
  CreateExperiment
newCreateExperiment pMetricGoals_ pName_ pProject_ =
  CreateExperiment'
    { tags = Prelude.Nothing,
      onlineAbConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      samplingRate = Prelude.Nothing,
      segment = Prelude.Nothing,
      randomizationSalt = Prelude.Nothing,
      metricGoals = Lens.coerced Lens.# pMetricGoals_,
      name = pName_,
      project = pProject_,
      treatments = Prelude.mempty
    }

-- | Assigns one or more tags (key-value pairs) to the experiment.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- Tags don\'t have any semantic meaning to Amazon Web Services and are
-- interpreted strictly as strings of characters.
--
-- >  <p>You can associate as many as 50 tags with an experiment.</p> <p>For more information, see <a href="https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html">Tagging Amazon Web Services resources</a>.</p>
createExperiment_tags :: Lens.Lens' CreateExperiment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createExperiment_tags = Lens.lens (\CreateExperiment' {tags} -> tags) (\s@CreateExperiment' {} a -> s {tags = a} :: CreateExperiment) Prelude.. Lens.mapping Lens.coerced

-- | A structure that contains the configuration of which variation to use as
-- the \"control\" version. tThe \"control\" version is used for comparison
-- with other variations. This structure also specifies how much experiment
-- traffic is allocated to each variation.
createExperiment_onlineAbConfig :: Lens.Lens' CreateExperiment (Prelude.Maybe OnlineAbConfig)
createExperiment_onlineAbConfig = Lens.lens (\CreateExperiment' {onlineAbConfig} -> onlineAbConfig) (\s@CreateExperiment' {} a -> s {onlineAbConfig = a} :: CreateExperiment)

-- | An optional description of the experiment.
createExperiment_description :: Lens.Lens' CreateExperiment (Prelude.Maybe Prelude.Text)
createExperiment_description = Lens.lens (\CreateExperiment' {description} -> description) (\s@CreateExperiment' {} a -> s {description = a} :: CreateExperiment)

-- | The portion of the available audience that you want to allocate to this
-- experiment, in thousandths of a percent. The available audience is the
-- total audience minus the audience that you have allocated to overrides
-- or current launches of this feature.
--
-- This is represented in thousandths of a percent. For example, specify
-- 10,000 to allocate 10% of the available audience.
createExperiment_samplingRate :: Lens.Lens' CreateExperiment (Prelude.Maybe Prelude.Natural)
createExperiment_samplingRate = Lens.lens (\CreateExperiment' {samplingRate} -> samplingRate) (\s@CreateExperiment' {} a -> s {samplingRate = a} :: CreateExperiment)

-- | Specifies an audience /segment/ to use in the experiment. When a segment
-- is used in an experiment, only user sessions that match the segment
-- pattern are used in the experiment.
createExperiment_segment :: Lens.Lens' CreateExperiment (Prelude.Maybe Prelude.Text)
createExperiment_segment = Lens.lens (\CreateExperiment' {segment} -> segment) (\s@CreateExperiment' {} a -> s {segment = a} :: CreateExperiment)

-- | When Evidently assigns a particular user session to an experiment, it
-- must use a randomization ID to determine which variation the user
-- session is served. This randomization ID is a combination of the entity
-- ID and @randomizationSalt@. If you omit @randomizationSalt@, Evidently
-- uses the experiment name as the @randomizationSalt@.
createExperiment_randomizationSalt :: Lens.Lens' CreateExperiment (Prelude.Maybe Prelude.Text)
createExperiment_randomizationSalt = Lens.lens (\CreateExperiment' {randomizationSalt} -> randomizationSalt) (\s@CreateExperiment' {} a -> s {randomizationSalt = a} :: CreateExperiment)

-- | An array of structures that defines the metrics used for the experiment,
-- and whether a higher or lower value for each metric is the goal.
createExperiment_metricGoals :: Lens.Lens' CreateExperiment (Prelude.NonEmpty MetricGoalConfig)
createExperiment_metricGoals = Lens.lens (\CreateExperiment' {metricGoals} -> metricGoals) (\s@CreateExperiment' {} a -> s {metricGoals = a} :: CreateExperiment) Prelude.. Lens.coerced

-- | A name for the new experiment.
createExperiment_name :: Lens.Lens' CreateExperiment Prelude.Text
createExperiment_name = Lens.lens (\CreateExperiment' {name} -> name) (\s@CreateExperiment' {} a -> s {name = a} :: CreateExperiment)

-- | The name or ARN of the project that you want to create the new
-- experiment in.
createExperiment_project :: Lens.Lens' CreateExperiment Prelude.Text
createExperiment_project = Lens.lens (\CreateExperiment' {project} -> project) (\s@CreateExperiment' {} a -> s {project = a} :: CreateExperiment)

-- | An array of structures that describe the configuration of each feature
-- variation used in the experiment.
createExperiment_treatments :: Lens.Lens' CreateExperiment [TreatmentConfig]
createExperiment_treatments = Lens.lens (\CreateExperiment' {treatments} -> treatments) (\s@CreateExperiment' {} a -> s {treatments = a} :: CreateExperiment) Prelude.. Lens.coerced

instance Core.AWSRequest CreateExperiment where
  type
    AWSResponse CreateExperiment =
      CreateExperimentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateExperimentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "experiment")
      )

instance Prelude.Hashable CreateExperiment where
  hashWithSalt _salt CreateExperiment' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` onlineAbConfig
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` samplingRate
      `Prelude.hashWithSalt` segment
      `Prelude.hashWithSalt` randomizationSalt
      `Prelude.hashWithSalt` metricGoals
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` project
      `Prelude.hashWithSalt` treatments

instance Prelude.NFData CreateExperiment where
  rnf CreateExperiment' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf onlineAbConfig
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf samplingRate
      `Prelude.seq` Prelude.rnf segment
      `Prelude.seq` Prelude.rnf randomizationSalt
      `Prelude.seq` Prelude.rnf metricGoals
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf project
      `Prelude.seq` Prelude.rnf treatments

instance Data.ToHeaders CreateExperiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateExperiment where
  toJSON CreateExperiment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("onlineAbConfig" Data..=)
              Prelude.<$> onlineAbConfig,
            ("description" Data..=) Prelude.<$> description,
            ("samplingRate" Data..=) Prelude.<$> samplingRate,
            ("segment" Data..=) Prelude.<$> segment,
            ("randomizationSalt" Data..=)
              Prelude.<$> randomizationSalt,
            Prelude.Just ("metricGoals" Data..= metricGoals),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("treatments" Data..= treatments)
          ]
      )

instance Data.ToPath CreateExperiment where
  toPath CreateExperiment' {..} =
    Prelude.mconcat
      ["/projects/", Data.toBS project, "/experiments"]

instance Data.ToQuery CreateExperiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateExperimentResponse' smart constructor.
data CreateExperimentResponse = CreateExperimentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing the configuration details of the experiment that
    -- you created.
    experiment :: Experiment
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createExperimentResponse_httpStatus' - The response's http status code.
--
-- 'experiment', 'createExperimentResponse_experiment' - A structure containing the configuration details of the experiment that
-- you created.
newCreateExperimentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'experiment'
  Experiment ->
  CreateExperimentResponse
newCreateExperimentResponse pHttpStatus_ pExperiment_ =
  CreateExperimentResponse'
    { httpStatus =
        pHttpStatus_,
      experiment = pExperiment_
    }

-- | The response's http status code.
createExperimentResponse_httpStatus :: Lens.Lens' CreateExperimentResponse Prelude.Int
createExperimentResponse_httpStatus = Lens.lens (\CreateExperimentResponse' {httpStatus} -> httpStatus) (\s@CreateExperimentResponse' {} a -> s {httpStatus = a} :: CreateExperimentResponse)

-- | A structure containing the configuration details of the experiment that
-- you created.
createExperimentResponse_experiment :: Lens.Lens' CreateExperimentResponse Experiment
createExperimentResponse_experiment = Lens.lens (\CreateExperimentResponse' {experiment} -> experiment) (\s@CreateExperimentResponse' {} a -> s {experiment = a} :: CreateExperimentResponse)

instance Prelude.NFData CreateExperimentResponse where
  rnf CreateExperimentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf experiment
