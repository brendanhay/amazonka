{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobCompletionCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobCompletionCriteria where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | How long a job is allowed to run, or how many candidates a job is allowed to generate.
--
--
--
-- /See:/ 'autoMLJobCompletionCriteria' smart constructor.
data AutoMLJobCompletionCriteria = AutoMLJobCompletionCriteria'
  { _amljccMaxCandidates ::
      !(Maybe Nat),
    _amljccMaxRuntimePerTrainingJobInSeconds ::
      !(Maybe Nat),
    _amljccMaxAutoMLJobRuntimeInSeconds ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoMLJobCompletionCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amljccMaxCandidates' - The maximum number of times a training job is allowed to run.
--
-- * 'amljccMaxRuntimePerTrainingJobInSeconds' - The maximum time, in seconds, a job is allowed to run.
--
-- * 'amljccMaxAutoMLJobRuntimeInSeconds' - The maximum time, in seconds, an AutoML job is allowed to wait for a trial to complete. It must be equal to or greater than MaxRuntimePerTrainingJobInSeconds.
autoMLJobCompletionCriteria ::
  AutoMLJobCompletionCriteria
autoMLJobCompletionCriteria =
  AutoMLJobCompletionCriteria'
    { _amljccMaxCandidates = Nothing,
      _amljccMaxRuntimePerTrainingJobInSeconds = Nothing,
      _amljccMaxAutoMLJobRuntimeInSeconds = Nothing
    }

-- | The maximum number of times a training job is allowed to run.
amljccMaxCandidates :: Lens' AutoMLJobCompletionCriteria (Maybe Natural)
amljccMaxCandidates = lens _amljccMaxCandidates (\s a -> s {_amljccMaxCandidates = a}) . mapping _Nat

-- | The maximum time, in seconds, a job is allowed to run.
amljccMaxRuntimePerTrainingJobInSeconds :: Lens' AutoMLJobCompletionCriteria (Maybe Natural)
amljccMaxRuntimePerTrainingJobInSeconds = lens _amljccMaxRuntimePerTrainingJobInSeconds (\s a -> s {_amljccMaxRuntimePerTrainingJobInSeconds = a}) . mapping _Nat

-- | The maximum time, in seconds, an AutoML job is allowed to wait for a trial to complete. It must be equal to or greater than MaxRuntimePerTrainingJobInSeconds.
amljccMaxAutoMLJobRuntimeInSeconds :: Lens' AutoMLJobCompletionCriteria (Maybe Natural)
amljccMaxAutoMLJobRuntimeInSeconds = lens _amljccMaxAutoMLJobRuntimeInSeconds (\s a -> s {_amljccMaxAutoMLJobRuntimeInSeconds = a}) . mapping _Nat

instance FromJSON AutoMLJobCompletionCriteria where
  parseJSON =
    withObject
      "AutoMLJobCompletionCriteria"
      ( \x ->
          AutoMLJobCompletionCriteria'
            <$> (x .:? "MaxCandidates")
            <*> (x .:? "MaxRuntimePerTrainingJobInSeconds")
            <*> (x .:? "MaxAutoMLJobRuntimeInSeconds")
      )

instance Hashable AutoMLJobCompletionCriteria

instance NFData AutoMLJobCompletionCriteria

instance ToJSON AutoMLJobCompletionCriteria where
  toJSON AutoMLJobCompletionCriteria' {..} =
    object
      ( catMaybes
          [ ("MaxCandidates" .=) <$> _amljccMaxCandidates,
            ("MaxRuntimePerTrainingJobInSeconds" .=)
              <$> _amljccMaxRuntimePerTrainingJobInSeconds,
            ("MaxAutoMLJobRuntimeInSeconds" .=)
              <$> _amljccMaxAutoMLJobRuntimeInSeconds
          ]
      )
