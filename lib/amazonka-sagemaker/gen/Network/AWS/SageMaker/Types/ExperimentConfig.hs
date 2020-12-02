{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ExperimentConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ExperimentConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Associates a SageMaker job as a trial component with an experiment and trial. Specified when you call the following APIs:
--
--
--     * 'CreateProcessingJob'
--
--     * 'CreateTrainingJob'
--
--     * 'CreateTransformJob'
--
--
--
--
-- /See:/ 'experimentConfig' smart constructor.
data ExperimentConfig = ExperimentConfig'
  { _ecTrialComponentDisplayName ::
      !(Maybe Text),
    _ecExperimentName :: !(Maybe Text),
    _ecTrialName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExperimentConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecTrialComponentDisplayName' - The display name for the trial component. If this key isn't specified, the display name is the trial component name.
--
-- * 'ecExperimentName' - The name of an existing experiment to associate the trial component with.
--
-- * 'ecTrialName' - The name of an existing trial to associate the trial component with. If not specified, a new trial is created.
experimentConfig ::
  ExperimentConfig
experimentConfig =
  ExperimentConfig'
    { _ecTrialComponentDisplayName = Nothing,
      _ecExperimentName = Nothing,
      _ecTrialName = Nothing
    }

-- | The display name for the trial component. If this key isn't specified, the display name is the trial component name.
ecTrialComponentDisplayName :: Lens' ExperimentConfig (Maybe Text)
ecTrialComponentDisplayName = lens _ecTrialComponentDisplayName (\s a -> s {_ecTrialComponentDisplayName = a})

-- | The name of an existing experiment to associate the trial component with.
ecExperimentName :: Lens' ExperimentConfig (Maybe Text)
ecExperimentName = lens _ecExperimentName (\s a -> s {_ecExperimentName = a})

-- | The name of an existing trial to associate the trial component with. If not specified, a new trial is created.
ecTrialName :: Lens' ExperimentConfig (Maybe Text)
ecTrialName = lens _ecTrialName (\s a -> s {_ecTrialName = a})

instance FromJSON ExperimentConfig where
  parseJSON =
    withObject
      "ExperimentConfig"
      ( \x ->
          ExperimentConfig'
            <$> (x .:? "TrialComponentDisplayName")
            <*> (x .:? "ExperimentName")
            <*> (x .:? "TrialName")
      )

instance Hashable ExperimentConfig

instance NFData ExperimentConfig

instance ToJSON ExperimentConfig where
  toJSON ExperimentConfig' {..} =
    object
      ( catMaybes
          [ ("TrialComponentDisplayName" .=) <$> _ecTrialComponentDisplayName,
            ("ExperimentName" .=) <$> _ecExperimentName,
            ("TrialName" .=) <$> _ecTrialName
          ]
      )
