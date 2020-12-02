{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringStoppingCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringStoppingCondition where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A time limit for how long the monitoring job is allowed to run before stopping.
--
--
--
-- /See:/ 'monitoringStoppingCondition' smart constructor.
newtype MonitoringStoppingCondition = MonitoringStoppingCondition'
  { _mscMaxRuntimeInSeconds ::
      Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MonitoringStoppingCondition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mscMaxRuntimeInSeconds' - The maximum runtime allowed in seconds.
monitoringStoppingCondition ::
  -- | 'mscMaxRuntimeInSeconds'
  Natural ->
  MonitoringStoppingCondition
monitoringStoppingCondition pMaxRuntimeInSeconds_ =
  MonitoringStoppingCondition'
    { _mscMaxRuntimeInSeconds =
        _Nat # pMaxRuntimeInSeconds_
    }

-- | The maximum runtime allowed in seconds.
mscMaxRuntimeInSeconds :: Lens' MonitoringStoppingCondition Natural
mscMaxRuntimeInSeconds = lens _mscMaxRuntimeInSeconds (\s a -> s {_mscMaxRuntimeInSeconds = a}) . _Nat

instance FromJSON MonitoringStoppingCondition where
  parseJSON =
    withObject
      "MonitoringStoppingCondition"
      ( \x ->
          MonitoringStoppingCondition' <$> (x .: "MaxRuntimeInSeconds")
      )

instance Hashable MonitoringStoppingCondition

instance NFData MonitoringStoppingCondition

instance ToJSON MonitoringStoppingCondition where
  toJSON MonitoringStoppingCondition' {..} =
    object
      ( catMaybes
          [Just ("MaxRuntimeInSeconds" .= _mscMaxRuntimeInSeconds)]
      )
