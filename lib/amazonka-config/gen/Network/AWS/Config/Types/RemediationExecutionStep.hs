{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationExecutionStep
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExecutionStep where

import Network.AWS.Config.Types.RemediationExecutionStepState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Name of the step from the SSM document.
--
--
--
-- /See:/ 'remediationExecutionStep' smart constructor.
data RemediationExecutionStep = RemediationExecutionStep'
  { _resState ::
      !(Maybe RemediationExecutionStepState),
    _resStartTime :: !(Maybe POSIX),
    _resName :: !(Maybe Text),
    _resStopTime :: !(Maybe POSIX),
    _resErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemediationExecutionStep' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'resState' - The valid status of the step.
--
-- * 'resStartTime' - The time when the step started.
--
-- * 'resName' - The details of the step.
--
-- * 'resStopTime' - The time when the step stopped.
--
-- * 'resErrorMessage' - An error message if the step was interrupted during execution.
remediationExecutionStep ::
  RemediationExecutionStep
remediationExecutionStep =
  RemediationExecutionStep'
    { _resState = Nothing,
      _resStartTime = Nothing,
      _resName = Nothing,
      _resStopTime = Nothing,
      _resErrorMessage = Nothing
    }

-- | The valid status of the step.
resState :: Lens' RemediationExecutionStep (Maybe RemediationExecutionStepState)
resState = lens _resState (\s a -> s {_resState = a})

-- | The time when the step started.
resStartTime :: Lens' RemediationExecutionStep (Maybe UTCTime)
resStartTime = lens _resStartTime (\s a -> s {_resStartTime = a}) . mapping _Time

-- | The details of the step.
resName :: Lens' RemediationExecutionStep (Maybe Text)
resName = lens _resName (\s a -> s {_resName = a})

-- | The time when the step stopped.
resStopTime :: Lens' RemediationExecutionStep (Maybe UTCTime)
resStopTime = lens _resStopTime (\s a -> s {_resStopTime = a}) . mapping _Time

-- | An error message if the step was interrupted during execution.
resErrorMessage :: Lens' RemediationExecutionStep (Maybe Text)
resErrorMessage = lens _resErrorMessage (\s a -> s {_resErrorMessage = a})

instance FromJSON RemediationExecutionStep where
  parseJSON =
    withObject
      "RemediationExecutionStep"
      ( \x ->
          RemediationExecutionStep'
            <$> (x .:? "State")
            <*> (x .:? "StartTime")
            <*> (x .:? "Name")
            <*> (x .:? "StopTime")
            <*> (x .:? "ErrorMessage")
      )

instance Hashable RemediationExecutionStep

instance NFData RemediationExecutionStep
