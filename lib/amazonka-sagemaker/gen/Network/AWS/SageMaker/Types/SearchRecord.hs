{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SearchRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SearchRecord where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.Experiment
import Network.AWS.SageMaker.Types.TrainingJob
import Network.AWS.SageMaker.Types.Trial
import Network.AWS.SageMaker.Types.TrialComponent

-- | A single resource returned as part of the 'Search' API response.
--
--
--
-- /See:/ 'searchRecord' smart constructor.
data SearchRecord = SearchRecord'
  { _srTrainingJob ::
      !(Maybe TrainingJob),
    _srTrial :: !(Maybe Trial),
    _srTrialComponent :: !(Maybe TrialComponent),
    _srExperiment :: !(Maybe Experiment)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SearchRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srTrainingJob' - The properties of a training job.
--
-- * 'srTrial' - The properties of a trial.
--
-- * 'srTrialComponent' - The properties of a trial component.
--
-- * 'srExperiment' - The properties of an experiment.
searchRecord ::
  SearchRecord
searchRecord =
  SearchRecord'
    { _srTrainingJob = Nothing,
      _srTrial = Nothing,
      _srTrialComponent = Nothing,
      _srExperiment = Nothing
    }

-- | The properties of a training job.
srTrainingJob :: Lens' SearchRecord (Maybe TrainingJob)
srTrainingJob = lens _srTrainingJob (\s a -> s {_srTrainingJob = a})

-- | The properties of a trial.
srTrial :: Lens' SearchRecord (Maybe Trial)
srTrial = lens _srTrial (\s a -> s {_srTrial = a})

-- | The properties of a trial component.
srTrialComponent :: Lens' SearchRecord (Maybe TrialComponent)
srTrialComponent = lens _srTrialComponent (\s a -> s {_srTrialComponent = a})

-- | The properties of an experiment.
srExperiment :: Lens' SearchRecord (Maybe Experiment)
srExperiment = lens _srExperiment (\s a -> s {_srExperiment = a})

instance FromJSON SearchRecord where
  parseJSON =
    withObject
      "SearchRecord"
      ( \x ->
          SearchRecord'
            <$> (x .:? "TrainingJob")
            <*> (x .:? "Trial")
            <*> (x .:? "TrialComponent")
            <*> (x .:? "Experiment")
      )

instance Hashable SearchRecord

instance NFData SearchRecord
