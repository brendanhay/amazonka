{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepStatus where

import Network.AWS.EMR.Types.FailureDetails
import Network.AWS.EMR.Types.StepState
import Network.AWS.EMR.Types.StepStateChangeReason
import Network.AWS.EMR.Types.StepTimeline
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The execution status details of the cluster step.
--
--
--
-- /See:/ 'stepStatus' smart constructor.
data StepStatus = StepStatus'
  { _ssState :: !(Maybe StepState),
    _ssFailureDetails :: !(Maybe FailureDetails),
    _ssStateChangeReason :: !(Maybe StepStateChangeReason),
    _ssTimeline :: !(Maybe StepTimeline)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StepStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssState' - The execution state of the cluster step.
--
-- * 'ssFailureDetails' - The details for the step failure including reason, message, and log file path where the root cause was identified.
--
-- * 'ssStateChangeReason' - The reason for the step execution status change.
--
-- * 'ssTimeline' - The timeline of the cluster step status over time.
stepStatus ::
  StepStatus
stepStatus =
  StepStatus'
    { _ssState = Nothing,
      _ssFailureDetails = Nothing,
      _ssStateChangeReason = Nothing,
      _ssTimeline = Nothing
    }

-- | The execution state of the cluster step.
ssState :: Lens' StepStatus (Maybe StepState)
ssState = lens _ssState (\s a -> s {_ssState = a})

-- | The details for the step failure including reason, message, and log file path where the root cause was identified.
ssFailureDetails :: Lens' StepStatus (Maybe FailureDetails)
ssFailureDetails = lens _ssFailureDetails (\s a -> s {_ssFailureDetails = a})

-- | The reason for the step execution status change.
ssStateChangeReason :: Lens' StepStatus (Maybe StepStateChangeReason)
ssStateChangeReason = lens _ssStateChangeReason (\s a -> s {_ssStateChangeReason = a})

-- | The timeline of the cluster step status over time.
ssTimeline :: Lens' StepStatus (Maybe StepTimeline)
ssTimeline = lens _ssTimeline (\s a -> s {_ssTimeline = a})

instance FromJSON StepStatus where
  parseJSON =
    withObject
      "StepStatus"
      ( \x ->
          StepStatus'
            <$> (x .:? "State")
            <*> (x .:? "FailureDetails")
            <*> (x .:? "StateChangeReason")
            <*> (x .:? "Timeline")
      )

instance Hashable StepStatus

instance NFData StepStatus
