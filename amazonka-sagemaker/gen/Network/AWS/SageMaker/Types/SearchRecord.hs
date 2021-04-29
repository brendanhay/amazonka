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
-- Module      : Network.AWS.SageMaker.Types.SearchRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SearchRecord where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    experiment :: Prelude.Maybe Experiment,
    featureGroup :: Prelude.Maybe FeatureGroup,
    modelPackage :: Prelude.Maybe ModelPackage,
    -- | The properties of a training job.
    trainingJob :: Prelude.Maybe TrainingJob,
    endpoint :: Prelude.Maybe Endpoint,
    pipelineExecution :: Prelude.Maybe PipelineExecution,
    -- | The properties of a trial component.
    trialComponent :: Prelude.Maybe TrialComponent,
    modelPackageGroup :: Prelude.Maybe ModelPackageGroup,
    pipeline :: Prelude.Maybe Pipeline,
    -- | The properties of a trial.
    trial :: Prelude.Maybe Trial
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { experiment = Prelude.Nothing,
      featureGroup = Prelude.Nothing,
      modelPackage = Prelude.Nothing,
      trainingJob = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      pipelineExecution = Prelude.Nothing,
      trialComponent = Prelude.Nothing,
      modelPackageGroup = Prelude.Nothing,
      pipeline = Prelude.Nothing,
      trial = Prelude.Nothing
    }

-- | The properties of an experiment.
searchRecord_experiment :: Lens.Lens' SearchRecord (Prelude.Maybe Experiment)
searchRecord_experiment = Lens.lens (\SearchRecord' {experiment} -> experiment) (\s@SearchRecord' {} a -> s {experiment = a} :: SearchRecord)

-- | Undocumented member.
searchRecord_featureGroup :: Lens.Lens' SearchRecord (Prelude.Maybe FeatureGroup)
searchRecord_featureGroup = Lens.lens (\SearchRecord' {featureGroup} -> featureGroup) (\s@SearchRecord' {} a -> s {featureGroup = a} :: SearchRecord)

-- | Undocumented member.
searchRecord_modelPackage :: Lens.Lens' SearchRecord (Prelude.Maybe ModelPackage)
searchRecord_modelPackage = Lens.lens (\SearchRecord' {modelPackage} -> modelPackage) (\s@SearchRecord' {} a -> s {modelPackage = a} :: SearchRecord)

-- | The properties of a training job.
searchRecord_trainingJob :: Lens.Lens' SearchRecord (Prelude.Maybe TrainingJob)
searchRecord_trainingJob = Lens.lens (\SearchRecord' {trainingJob} -> trainingJob) (\s@SearchRecord' {} a -> s {trainingJob = a} :: SearchRecord)

-- | Undocumented member.
searchRecord_endpoint :: Lens.Lens' SearchRecord (Prelude.Maybe Endpoint)
searchRecord_endpoint = Lens.lens (\SearchRecord' {endpoint} -> endpoint) (\s@SearchRecord' {} a -> s {endpoint = a} :: SearchRecord)

-- | Undocumented member.
searchRecord_pipelineExecution :: Lens.Lens' SearchRecord (Prelude.Maybe PipelineExecution)
searchRecord_pipelineExecution = Lens.lens (\SearchRecord' {pipelineExecution} -> pipelineExecution) (\s@SearchRecord' {} a -> s {pipelineExecution = a} :: SearchRecord)

-- | The properties of a trial component.
searchRecord_trialComponent :: Lens.Lens' SearchRecord (Prelude.Maybe TrialComponent)
searchRecord_trialComponent = Lens.lens (\SearchRecord' {trialComponent} -> trialComponent) (\s@SearchRecord' {} a -> s {trialComponent = a} :: SearchRecord)

-- | Undocumented member.
searchRecord_modelPackageGroup :: Lens.Lens' SearchRecord (Prelude.Maybe ModelPackageGroup)
searchRecord_modelPackageGroup = Lens.lens (\SearchRecord' {modelPackageGroup} -> modelPackageGroup) (\s@SearchRecord' {} a -> s {modelPackageGroup = a} :: SearchRecord)

-- | Undocumented member.
searchRecord_pipeline :: Lens.Lens' SearchRecord (Prelude.Maybe Pipeline)
searchRecord_pipeline = Lens.lens (\SearchRecord' {pipeline} -> pipeline) (\s@SearchRecord' {} a -> s {pipeline = a} :: SearchRecord)

-- | The properties of a trial.
searchRecord_trial :: Lens.Lens' SearchRecord (Prelude.Maybe Trial)
searchRecord_trial = Lens.lens (\SearchRecord' {trial} -> trial) (\s@SearchRecord' {} a -> s {trial = a} :: SearchRecord)

instance Prelude.FromJSON SearchRecord where
  parseJSON =
    Prelude.withObject
      "SearchRecord"
      ( \x ->
          SearchRecord'
            Prelude.<$> (x Prelude..:? "Experiment")
            Prelude.<*> (x Prelude..:? "FeatureGroup")
            Prelude.<*> (x Prelude..:? "ModelPackage")
            Prelude.<*> (x Prelude..:? "TrainingJob")
            Prelude.<*> (x Prelude..:? "Endpoint")
            Prelude.<*> (x Prelude..:? "PipelineExecution")
            Prelude.<*> (x Prelude..:? "TrialComponent")
            Prelude.<*> (x Prelude..:? "ModelPackageGroup")
            Prelude.<*> (x Prelude..:? "Pipeline")
            Prelude.<*> (x Prelude..:? "Trial")
      )

instance Prelude.Hashable SearchRecord

instance Prelude.NFData SearchRecord
