{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ResourceLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ResourceLimits where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the maximum number of training jobs and parallel training jobs that a hyperparameter tuning job can launch.
--
--
--
-- /See:/ 'resourceLimits' smart constructor.
data ResourceLimits = ResourceLimits'
  { _rlMaxNumberOfTrainingJobs ::
      !Nat,
    _rlMaxParallelTrainingJobs :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlMaxNumberOfTrainingJobs' - The maximum number of training jobs that a hyperparameter tuning job can launch.
--
-- * 'rlMaxParallelTrainingJobs' - The maximum number of concurrent training jobs that a hyperparameter tuning job can launch.
resourceLimits ::
  -- | 'rlMaxNumberOfTrainingJobs'
  Natural ->
  -- | 'rlMaxParallelTrainingJobs'
  Natural ->
  ResourceLimits
resourceLimits pMaxNumberOfTrainingJobs_ pMaxParallelTrainingJobs_ =
  ResourceLimits'
    { _rlMaxNumberOfTrainingJobs =
        _Nat # pMaxNumberOfTrainingJobs_,
      _rlMaxParallelTrainingJobs = _Nat # pMaxParallelTrainingJobs_
    }

-- | The maximum number of training jobs that a hyperparameter tuning job can launch.
rlMaxNumberOfTrainingJobs :: Lens' ResourceLimits Natural
rlMaxNumberOfTrainingJobs = lens _rlMaxNumberOfTrainingJobs (\s a -> s {_rlMaxNumberOfTrainingJobs = a}) . _Nat

-- | The maximum number of concurrent training jobs that a hyperparameter tuning job can launch.
rlMaxParallelTrainingJobs :: Lens' ResourceLimits Natural
rlMaxParallelTrainingJobs = lens _rlMaxParallelTrainingJobs (\s a -> s {_rlMaxParallelTrainingJobs = a}) . _Nat

instance FromJSON ResourceLimits where
  parseJSON =
    withObject
      "ResourceLimits"
      ( \x ->
          ResourceLimits'
            <$> (x .: "MaxNumberOfTrainingJobs")
            <*> (x .: "MaxParallelTrainingJobs")
      )

instance Hashable ResourceLimits

instance NFData ResourceLimits

instance ToJSON ResourceLimits where
  toJSON ResourceLimits' {..} =
    object
      ( catMaybes
          [ Just ("MaxNumberOfTrainingJobs" .= _rlMaxNumberOfTrainingJobs),
            Just ("MaxParallelTrainingJobs" .= _rlMaxParallelTrainingJobs)
          ]
      )
