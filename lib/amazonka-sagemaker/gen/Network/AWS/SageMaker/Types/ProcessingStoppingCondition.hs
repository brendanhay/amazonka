{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingStoppingCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingStoppingCondition where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a time limit for how long the processing job is allowed to run.
--
--
--
-- /See:/ 'processingStoppingCondition' smart constructor.
newtype ProcessingStoppingCondition = ProcessingStoppingCondition'
  { _pscMaxRuntimeInSeconds ::
      Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProcessingStoppingCondition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pscMaxRuntimeInSeconds' - Specifies the maximum runtime in seconds.
processingStoppingCondition ::
  -- | 'pscMaxRuntimeInSeconds'
  Natural ->
  ProcessingStoppingCondition
processingStoppingCondition pMaxRuntimeInSeconds_ =
  ProcessingStoppingCondition'
    { _pscMaxRuntimeInSeconds =
        _Nat # pMaxRuntimeInSeconds_
    }

-- | Specifies the maximum runtime in seconds.
pscMaxRuntimeInSeconds :: Lens' ProcessingStoppingCondition Natural
pscMaxRuntimeInSeconds = lens _pscMaxRuntimeInSeconds (\s a -> s {_pscMaxRuntimeInSeconds = a}) . _Nat

instance FromJSON ProcessingStoppingCondition where
  parseJSON =
    withObject
      "ProcessingStoppingCondition"
      ( \x ->
          ProcessingStoppingCondition' <$> (x .: "MaxRuntimeInSeconds")
      )

instance Hashable ProcessingStoppingCondition

instance NFData ProcessingStoppingCondition

instance ToJSON ProcessingStoppingCondition where
  toJSON ProcessingStoppingCondition' {..} =
    object
      ( catMaybes
          [Just ("MaxRuntimeInSeconds" .= _pscMaxRuntimeInSeconds)]
      )
