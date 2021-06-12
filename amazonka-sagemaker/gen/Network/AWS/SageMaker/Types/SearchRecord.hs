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
-- Module      : Network.AWS.SageMaker.Types.SearchRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SearchRecord where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.Endpoint
import Network.AWS.SageMaker.Types.Experiment
import Network.AWS.SageMaker.Types.FeatureGroup
import Network.AWS.SageMaker.Types.ModelPackage
import Network.AWS.SageMaker.Types.ModelPackageGroup
import Network.AWS.SageMaker.Types.Pipeline
import Network.AWS.SageMaker.Types.PipelineExecution
import Network.AWS.SageMaker.Types.TrainingJob
import Network.AWS.SageMaker.Types.Trial
import Network.AWS.SageMaker.Types.TrialComponent

-- | A single resource returned as part of the Search API response.
--
-- /See:/ 'newSearchRecord' smart constructor.
data SearchRecord = SearchRecord'
  { -- | The properties of an experiment.
    experiment :: Core.Maybe Experiment,
    featureGroup :: Core.Maybe FeatureGroup,
    modelPackage :: Core.Maybe ModelPackage,
    -- | The properties of a training job.
    trainingJob :: Core.Maybe TrainingJob,
    endpoint :: Core.Maybe Endpoint,
    pipelineExecution :: Core.Maybe PipelineExecution,
    -- | The properties of a trial component.
    trialComponent :: Core.Maybe TrialComponent,
    modelPackageGroup :: Core.Maybe ModelPackageGroup,
    pipeline :: Core.Maybe Pipeline,
    -- | The properties of a trial.
    trial :: Core.Maybe Trial
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experiment', 'searchRecord_experiment' - The properties of an experiment.
--
-- 'featureGroup', 'searchRecord_featureGroup' - Undocumented member.
--
-- 'modelPackage', 'searchRecord_modelPackage' - Undocumented member.
--
-- 'trainingJob', 'searchRecord_trainingJob' - The properties of a training job.
--
-- 'endpoint', 'searchRecord_endpoint' - Undocumented member.
--
-- 'pipelineExecution', 'searchRecord_pipelineExecution' - Undocumented member.
--
-- 'trialComponent', 'searchRecord_trialComponent' - The properties of a trial component.
--
-- 'modelPackageGroup', 'searchRecord_modelPackageGroup' - Undocumented member.
--
-- 'pipeline', 'searchRecord_pipeline' - Undocumented member.
--
-- 'trial', 'searchRecord_trial' - The properties of a trial.
newSearchRecord ::
  SearchRecord
newSearchRecord =
  SearchRecord'
    { experiment = Core.Nothing,
      featureGroup = Core.Nothing,
      modelPackage = Core.Nothing,
      trainingJob = Core.Nothing,
      endpoint = Core.Nothing,
      pipelineExecution = Core.Nothing,
      trialComponent = Core.Nothing,
      modelPackageGroup = Core.Nothing,
      pipeline = Core.Nothing,
      trial = Core.Nothing
    }

-- | The properties of an experiment.
searchRecord_experiment :: Lens.Lens' SearchRecord (Core.Maybe Experiment)
searchRecord_experiment = Lens.lens (\SearchRecord' {experiment} -> experiment) (\s@SearchRecord' {} a -> s {experiment = a} :: SearchRecord)

-- | Undocumented member.
searchRecord_featureGroup :: Lens.Lens' SearchRecord (Core.Maybe FeatureGroup)
searchRecord_featureGroup = Lens.lens (\SearchRecord' {featureGroup} -> featureGroup) (\s@SearchRecord' {} a -> s {featureGroup = a} :: SearchRecord)

-- | Undocumented member.
searchRecord_modelPackage :: Lens.Lens' SearchRecord (Core.Maybe ModelPackage)
searchRecord_modelPackage = Lens.lens (\SearchRecord' {modelPackage} -> modelPackage) (\s@SearchRecord' {} a -> s {modelPackage = a} :: SearchRecord)

-- | The properties of a training job.
searchRecord_trainingJob :: Lens.Lens' SearchRecord (Core.Maybe TrainingJob)
searchRecord_trainingJob = Lens.lens (\SearchRecord' {trainingJob} -> trainingJob) (\s@SearchRecord' {} a -> s {trainingJob = a} :: SearchRecord)

-- | Undocumented member.
searchRecord_endpoint :: Lens.Lens' SearchRecord (Core.Maybe Endpoint)
searchRecord_endpoint = Lens.lens (\SearchRecord' {endpoint} -> endpoint) (\s@SearchRecord' {} a -> s {endpoint = a} :: SearchRecord)

-- | Undocumented member.
searchRecord_pipelineExecution :: Lens.Lens' SearchRecord (Core.Maybe PipelineExecution)
searchRecord_pipelineExecution = Lens.lens (\SearchRecord' {pipelineExecution} -> pipelineExecution) (\s@SearchRecord' {} a -> s {pipelineExecution = a} :: SearchRecord)

-- | The properties of a trial component.
searchRecord_trialComponent :: Lens.Lens' SearchRecord (Core.Maybe TrialComponent)
searchRecord_trialComponent = Lens.lens (\SearchRecord' {trialComponent} -> trialComponent) (\s@SearchRecord' {} a -> s {trialComponent = a} :: SearchRecord)

-- | Undocumented member.
searchRecord_modelPackageGroup :: Lens.Lens' SearchRecord (Core.Maybe ModelPackageGroup)
searchRecord_modelPackageGroup = Lens.lens (\SearchRecord' {modelPackageGroup} -> modelPackageGroup) (\s@SearchRecord' {} a -> s {modelPackageGroup = a} :: SearchRecord)

-- | Undocumented member.
searchRecord_pipeline :: Lens.Lens' SearchRecord (Core.Maybe Pipeline)
searchRecord_pipeline = Lens.lens (\SearchRecord' {pipeline} -> pipeline) (\s@SearchRecord' {} a -> s {pipeline = a} :: SearchRecord)

-- | The properties of a trial.
searchRecord_trial :: Lens.Lens' SearchRecord (Core.Maybe Trial)
searchRecord_trial = Lens.lens (\SearchRecord' {trial} -> trial) (\s@SearchRecord' {} a -> s {trial = a} :: SearchRecord)

instance Core.FromJSON SearchRecord where
  parseJSON =
    Core.withObject
      "SearchRecord"
      ( \x ->
          SearchRecord'
            Core.<$> (x Core..:? "Experiment")
            Core.<*> (x Core..:? "FeatureGroup")
            Core.<*> (x Core..:? "ModelPackage")
            Core.<*> (x Core..:? "TrainingJob")
            Core.<*> (x Core..:? "Endpoint")
            Core.<*> (x Core..:? "PipelineExecution")
            Core.<*> (x Core..:? "TrialComponent")
            Core.<*> (x Core..:? "ModelPackageGroup")
            Core.<*> (x Core..:? "Pipeline")
            Core.<*> (x Core..:? "Trial")
      )

instance Core.Hashable SearchRecord

instance Core.NFData SearchRecord
