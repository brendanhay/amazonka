{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.StoppingCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.StoppingCondition where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a limit to how long a model training or compilation job can run. It also specifies how long you are willing to wait for a managed spot training job to complete. When the job reaches the time limit, Amazon SageMaker ends the training or compilation job. Use this API to cap model training costs.
--
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
--
-- The training algorithms provided by Amazon SageMaker automatically save the intermediate results of a model training job when possible. This attempt to save artifacts is only a best effort case as model might not be in a state from which it can be saved. For example, if training has just started, the model might not be ready to save. When saved, this intermediate data is a valid model artifact. You can use it to create a model with @CreateModel@ .
--
--
-- /See:/ 'stoppingCondition' smart constructor.
data StoppingCondition = StoppingCondition'
  { _scMaxWaitTimeInSeconds ::
      !(Maybe Nat),
    _scMaxRuntimeInSeconds :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StoppingCondition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scMaxWaitTimeInSeconds' - The maximum length of time, in seconds, how long you are willing to wait for a managed spot training job to complete. It is the amount of time spent waiting for Spot capacity plus the amount of time the training job runs. It must be equal to or greater than @MaxRuntimeInSeconds@ .
--
-- * 'scMaxRuntimeInSeconds' - The maximum length of time, in seconds, that the training or compilation job can run. If job does not complete during this time, Amazon SageMaker ends the job. If value is not specified, default value is 1 day. The maximum value is 28 days.
stoppingCondition ::
  StoppingCondition
stoppingCondition =
  StoppingCondition'
    { _scMaxWaitTimeInSeconds = Nothing,
      _scMaxRuntimeInSeconds = Nothing
    }

-- | The maximum length of time, in seconds, how long you are willing to wait for a managed spot training job to complete. It is the amount of time spent waiting for Spot capacity plus the amount of time the training job runs. It must be equal to or greater than @MaxRuntimeInSeconds@ .
scMaxWaitTimeInSeconds :: Lens' StoppingCondition (Maybe Natural)
scMaxWaitTimeInSeconds = lens _scMaxWaitTimeInSeconds (\s a -> s {_scMaxWaitTimeInSeconds = a}) . mapping _Nat

-- | The maximum length of time, in seconds, that the training or compilation job can run. If job does not complete during this time, Amazon SageMaker ends the job. If value is not specified, default value is 1 day. The maximum value is 28 days.
scMaxRuntimeInSeconds :: Lens' StoppingCondition (Maybe Natural)
scMaxRuntimeInSeconds = lens _scMaxRuntimeInSeconds (\s a -> s {_scMaxRuntimeInSeconds = a}) . mapping _Nat

instance FromJSON StoppingCondition where
  parseJSON =
    withObject
      "StoppingCondition"
      ( \x ->
          StoppingCondition'
            <$> (x .:? "MaxWaitTimeInSeconds") <*> (x .:? "MaxRuntimeInSeconds")
      )

instance Hashable StoppingCondition

instance NFData StoppingCondition

instance ToJSON StoppingCondition where
  toJSON StoppingCondition' {..} =
    object
      ( catMaybes
          [ ("MaxWaitTimeInSeconds" .=) <$> _scMaxWaitTimeInSeconds,
            ("MaxRuntimeInSeconds" .=) <$> _scMaxRuntimeInSeconds
          ]
      )
